---
title: "AVX10.2の新機能"
emoji: "📚"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [x86, avx, avx10]
published: true
---

将来のIntel製CPUに搭載される命令セット拡張、AVX10.2の情報が先日出ました。

* [Intel® Advanced Vector Extensions 10.2 (Intel® AVX10.2) Architecture Specification](https://www.intel.com/content/www/us/en/content-details/828965/intel-advanced-vector-extensions-10-2-intel-avx10-2-architecture-specification.html?wapkw=advanced%20vector%20extensions%2010)

この記事では、AVX10.2の新機能をざっくりと紹介します。

## 静的な丸め指定

浮動小数点演算において演算結果を浮動小数点形式で正確に表現できない場合は丸めが発生します。この丸めのやり方はIEEE 754でいくつか定義されており、x86系では4つが利用可能です。丸めのやり方は通常は浮動小数点演算器の制御レジスターで選択されますが、AVX-512では、命令の機械語で丸め方を指定できるようになりました。

（宣伝：丸め方を静的に指定する話は「Binary Hacks Rebooted」にも書いたので是非読んでください。）

ただ、機械語のエンコードの都合上、AVX-512では「スカラー演算」と「512ビット幅のベクトル演算」にのみ静的な丸め指定が可能で、「128ビット/256ビット幅のベクトル演算」には静的な丸め指定ができませんでした。「機械語のエンコードの都合」というのは前に書いた記事

* [x86-64機械語入門 AVX/AVX-512編](https://zenn.dev/mod_poppo/articles/x86-64-vex-evex)

の「EVEX.L'LとEVEX.RCは実体が同一」の部分です。

AVX10.2ではEVEXプリフィックスが拡張され、3バイト目の「固定」だった部分を活用して256ビットの静的丸めが利用可能になります。詳しいエンコード方法は仕様書を見てください。

【2025年10月10日 追記】2025年春頃の計画変更でAVX10も512ビットを必須とすることとなり、256ビットの静的丸めはキャンセルされたようです。【追記終わり】

## AI向けの浮動小数点演算の強化：FP8のサポート、BF16の強化など

### FP8

ディープラーニングでは低精度の数値型が多用されます。浮動小数点数のFP16、BF16、整数のINT8、INT4など。浮動小数点数の方にももっと幅の小さいデータ型があると便利だと思われたのか、Open Compute ProjectというところがFP8の仕様を出しているようです：

* [OCP 8-bit Floating Point Specification (OFP8)](https://www.opencompute.org/documents/ocp-8-bit-floating-point-specification-ofp8-revision-1-0-2023-12-01-pdf-1)

概要は以下のようになります：

E4M3（またはHF8）

* 符号ビット：1ビット
* 指数部：4ビット
    * バイアスは7
* 仮数部下位：3ビット
* 指数部を少しでも広く取るため、無限大はない。NaNはある

E5M2（またはBF8）

* 符号ビット：1ビット
* 指数部：5ビット
    * バイアスは15
* 仮数部下位：2ビット
* 無限大とNaNがある

AVX10.2では、FP8のドット積、幅の広い型への変換、幅の広い型からの変換がサポートされます。

```
VCVTBIASPH2BF8
VCVTBIASPH2BF8S
VCVTBIASPH2HF8
VCVTBIASPH2HF8S
```

`VCVTBIAS` 系の命令は、入力のFP16の値にバイアスと呼ばれる整数（仮数部の下に続くように配置される）を加えて切り捨てによってFP8に変換します。末尾にSがつくものはオーバーフロー時に有限の最大値を返します。

`VCVTHF82PH` 命令は、E4M3（HF8）の浮動小数点数をFP16に変換します。

E5M2の浮動小数点数をFP16等に変換する命令はなさそうです。

```
VCVTNEPH2BF8
VCVTNEPH2BF8S
VCVTNEPH2HF8
VCVTNEPH2HF8S
VCVTNE2PH2BF8
VCVTNE2PH2BF8S
VCVTNE2PH2HF8
VCVTNE2PH2HF8S
```

`VCVTNE` 系の命令はFP16の入力をE5M2またはE4M3のFP8に変換します。常に最近接丸め（距離が等しい場合は偶数へ）が使用されます。末尾にSがつくものはオーバーフロー時に有限の最大値を返します。

### BF16サポートの強化

IntelのCPUはこれまでもBF16 (bfloat16, brain float16)をサポートしてきましたが、従来のサポートは単精度との変換とドット積（AVX512_BF16）と行列演算（AMX-BF16）に限られてきました。

実際のところ、AVX512_BF16の命令はこれだけです：

```
VCVTNE2PS2BF16
VCVTNEPS2BF16
VDPBF16PS
```

AVX10.2では、BF16を直に演算対象とする四則演算などの命令が追加されるようです。

【2025年10月10日 追記】BF16については「[bfloat16 (brain float16) のCPUによる対応とC言語からの利用](brain-float16)」という記事も書いたので見てください。【追記終わり】

## IEEE 754-2019のmin/max演算のサポート

浮動小数点数のmin/max演算に色々あるという話は

* [浮動小数点数の min / max](https://qiita.com/mod_poppo/items/41a09bd40acfceec6ec8)

に書きました。AVX-512DQではIEEE 754-2008のminNum/maxNum演算をサポートしたという話も書きました。

今回追加されるのは、IEEE 754-2019のmin/max演算に対応する命令です。命令としては

```
VMINMAXNEPBF16
VMINMAX[PH,PS,PD]
VMINMAX[SH,SS,SD]
```

で、半精度（binary16）、単精度（binary32）、倍精度（binary64）のそれぞれスカラーとベクトル、それにBF16のベクトルに対応しているようです。

`VMINMAXNEPBF16` のどこが「NE」なのかはよく分かりません。

【2025年10月10日 追記】2024年11月の版でBF16の命令のニーモニックから「NE」と「P」が消え、`VMINMAXBF16` となりました。【追記終わり】

WebAssemblyでは「入力のNaNを伝播させるmin/max演算」が採用されているため、Intelはこの機能の売り文句として「making it compatible for WebAssembly application development」としているようです。

## 飽和変換

浮動小数点数を固定長整数型に変換するとき、結果の型で表現できない値はどうすればいいでしょうか？従来のx86では、全ビット1で埋めるとか、2の補数表現の最小値を返していました。しかし、プログラミング言語によってはそれ以外の方法を規定しているというのは

* [ArmにあるというJavaScript専用命令とは何か、あるいは浮動小数点数を整数に変換する方法について](https://qiita.com/mod_poppo/items/66663d9a05790579b5e4)

に書いた通りです。この辺の話は「Binary Hacks Rebooted」にも書きました（宣伝）。

今回追加される命令は、以下の方法で整数型に変換します：

* 小数点以下は切り捨て
* NaNには0を
* 無限大や範囲外の値には、表現可能な最大値・最小値を

切り捨て以外の方法を使いたい場合は他の命令と組み合わせます。

Intelによると、WebAssemblyやRustで必要になるので追加したらしいです。WebAssemblyはいいとして、私はRustには詳しくないのでこの言説の妥当性を判断できません。

一昔前なら「Javaで必要になるので」と言ってそうなものですが、WebAssemblyやRustが挙がるのは時代の流れですねえ。

## Zero-extending Partial Vector Copies

SIMDレジスターに格納した32ビット値や16ビット値を他のSIMDレジスターにコピーする際、従来は下位128ビットごとコピーするか、32ビットや16ビットをコピーして上位は元の値を残す（あるいは他のレジスターからコピーする）、という選択肢しかありませんでした。今回、上位ビットをゼロクリアする選択肢が追加されます。

`VMOVD` は32ビットで、`VMOVW` は16ビットです。

`VMOVD` や `VMOVW` は命令の名前としては以前からありましたが、SIMDレジスター同士でなく、オペランドの片方は汎用レジスター（またはメモリー）でした。

64ビット値に関しては、以前から `(V)MOVQ` があります。

## FP Scalar Comparison

浮動小数点数の比較については、以前に記事を書きました：

* [浮動小数点数の比較について](https://qiita.com/mod_poppo/items/d30b71eb3eb957332145)

今回、浮動小数点数の比較命令が強化されました。

従来は、浮動小数点数の比較には `(v)(u)comis[h,s,d]` 命令を使っていました。`u` の意味ですが、`(v)ucomisd` は入力にquiet NaNが含まれていても例外を発生させない比較命令です。いずれも、比較結果はEFLAGSレジスターのZF, PF, CFに格納します。疑似コードで書けば次のようになります：

```
(v)(u)comisd(x, y) {
    match compare(x, y) {
        UNORDERED: ZF,PF,CF := 0b111;
        GREATER_THAN: ZF,PF,CF := 0b000;
        LESS_THAN: ZF,PF,CF := 0b001;
        EQUAL: ZF,PF,CF := 0b100;
    }
    OF,AF,SF := 0b000;
}
```

フラグが立つ条件は次のようになります：

* ZF: EQUALまたはUNORDEREDの場合に立つ。
* PF: UNORDEREDの場合に立つ。
* CF: LESS_THANまたはUNORDEREDの場合に立つ。

条件ジャンプ命令との関係は次のようになります：

| ニーモニック | 条件 | 浮動小数点数の比較結果との対応 |
|-|-|-|
| JA (above), JNBE (not below or equal) | CF=0 and ZF=0 | GREATER_THAN |
| JAE (above or equal), JNB (not below), JNC (not carry) | CF=0 | EQUAL or GREATER_THAN |
| JB (below), JC (carry), JNAE (not above or equal) | CF=1 | LESS_THAN or UNORDERED |
| JBE (below or equal), JNA (not above) | CF=1 or ZF=1 | EQUAL, LESS_THAN or UNORDERED |
| JE (equal), JZ (zero) | ZF=1 | EQUAL or UNORDERED |
| JG (greater), JNLE (not less or equal) | ZF=0 and SF=OF | LESS_THAN or GREATER_THAN |
| JGE (greater or equal), JNL (not less) | SF=OF | 全て |
| JL (less), JNGE (not greater or equal) | SF≠OF | なし |
| JLE (less or equal), JNG (not greater) | ZF=1 or SF≠OF | EQUAL or UNORDERED |
| JNE (not equal) | ZF=0 | LESS_THAN or GREATER_THAN |
| JO (overflow) | OF=1 | なし |
| JP (parity), JPE (parity even) | PF=1 | UNORDERED |
| JPO (parity odd) | PF=0 | EQUAL, LESS_THAN or GREATER_THAN |
| JS (sign) | SF=1 | なし |

| 浮動小数点数の比較結果の集合 | 使える命令 | 代用方法 |
|-|-|-|
| EQUAL | | |
| LESS_THAN | | JAやJNBEをオペランドを反転させて使えば良い |
| GREATER_THAN | JA, JNBE | |
| UNORDERED | JP, JPE | |
| {EQUAL, LESS_THAN} | | JAE, JNB, JNCをオペランドを反転させて使えば良い |
| {EQUAL, GREATER_THAN} | JAE, JNB, JNC | |
| {EQUAL, UNORDERED} | JE, JZ, JLE, JNG | |
| {LESS_THAN, GREATER_THAN} | JG, JNLE | |
| {LESS_THAN, UNORDERED} | JB, JC, JNAE | |
| {GREATER_THAN, UNORDERED} | | JB, JC, JNAEをオペランドを反転させて使えば良い |
| {EQUAL, LESS_THAN, GREATER_THAN} | JPO | |
| {EQUAL, LESS_THAN, UNORDERED} | JBE, JNA | |
| {EQUAL, GREATER_THAN, UNORDERED} | | JBE, JNAをオペランドを反転させて使えば良い |
| {LESS_THAN, GREATER_THAN, UNORDERED} | | |

これを見ると、「等価性（EQUAL）に基づいた条件ジャンプを一発で実行できる命令がない」ことが分かります。

AVX10.2では、浮動小数点数の比較を行う別の命令群が追加されます。これらは、立てるフラグの組み合わせが異なります。

```
VCOMXSD
VCOMXSH
VCOMXSS
VUCOMXSD
VUCOMXSH
VUCOMXSS
```

命令にUが含まれるやつはquiet NaNに対して例外を発生させないやつです。疑似コードで書けば

```
v(u)comxsd(x, y) {
    match compare(x, y) {
        UNORDERED: OF,SF,ZF,PF,CF := 0b11011;
        GREATER_THAN: OF,SF,ZF,PF,CF := 0b00000;
        LESS_THAN: OF,SF,ZF,PF,CF := 0b10001;
        EQUAL: OF,SF,ZF,PF,CF := 0b11100;
    }
    AF := 0;
}
```

となります。

フラグが立つ条件は次のようになります：

* OF: EQUALまたはLESS_THANまたはUNORDEREDの場合に立つ。
* SF: EQUALまたはUNORDEREDの場合に立つ。
* ZF: EQUALの場合に立つ。
* PF: UNORDEREDの場合に立つ。
* CF: LESS_THANまたはUNORDEREDの場合に立つ。

条件ジャンプ命令との関係は次のようになります：

| ニーモニック | 条件 | 浮動小数点数の比較結果との対応 |
|-|-|-|
| JA (above), JNBE (not below or equal) | CF=0 and ZF=0 | GREATER_THAN |
| JAE (above or equal), JNB (not below), JNC (not carry) | CF=0 | EQUAL or GREATER_THAN |
| JB (below), JC (carry), JNAE (not above or equal) | CF=1 | LESS_THAN or UNORDERED |
| JBE (below or equal), JNA (not above) | CF=1 or ZF=1 | EQUAL, LESS_THAN or UNORDERED |
| JE (equal), JZ (zero) | ZF=1 | EQUAL |
| JG (greater), JNLE (not less or equal) | ZF=0 and SF=OF | UNORDERED |
| JGE (greater or equal), JNL (not less) | SF=OF | EQUAL or UNORDERED |
| JL (less), JNGE (not greater or equal) | SF≠OF | LESS_THAN |
| JLE (less or equal), JNG (not greater) | ZF=1 or SF≠OF | EQUAL or LESS_THAN |
| JNE (not equal) | ZF=0 | LESS_THAN, GREATER_THAN or UNORDERED |
| JO (overflow) | OF=1 | EQUAL, LESS_THAN or UNORDERED |
| JP (parity), JPE (parity even) | PF=1 | UNORDERED |
| JPO (parity odd) | PF=0 | EQUAL, LESS_THAN or GREATER_THAN |
| JS (sign) | SF=1 | EQUAL or UNORDERED |

| 浮動小数点数の比較結果の集合 | 使える命令 | 代用方法 |
|-|-|-|
| EQUAL | JE, JZ | |
| LESS_THAN | JL, JNGE | |
| GREATER_THAN | JA, JNBE | |
| UNORDERED | JG, JNLE, JP, JPE | |
| {EQUAL, LESS_THAN} | JLE, JNG | |
| {EQUAL, GREATER_THAN} | JAE, JNB, JNC | |
| {EQUAL, UNORDERED} | JGE, JNL, JS | |
| {LESS_THAN, GREATER_THAN} | | |
| {LESS_THAN, UNORDERED} | JB, JC, JNAE | |
| {GREATER_THAN, UNORDERED} | | JB, JC, JNAEをオペランドを反転させて使えば良い |
| {EQUAL, LESS_THAN, GREATER_THAN} | JPO | |
| {EQUAL, LESS_THAN, UNORDERED} | JBE, JNA, JO | |
| {EQUAL, GREATER_THAN, UNORDERED} | | JBE, JNA, JOをオペランドを反転させて使えば良い |
| {LESS_THAN, GREATER_THAN, UNORDERED} | | |

というわけで、AVX10.2を使うと「浮動小数点数が等しいか」を簡潔に判定できるようになります。

## Media Acceleration

整数やFP16の内積（VNNI系の命令）や、EVEX版のsum of absolute difference命令が追加されるようです：

```
VDPPHPS
VMPSADBW
VPDPB[SU,UU,SS]D[,S]
VPDPW[SU,US,SS]D[,S]
```

## まとめと宣伝

AVX10.2の新機能をざっくり紹介しました。

記事中にちょいちょい宣伝を挟みましたが、今度（2024年の8月末）Binary Hacks Rebootedという本が出ます（私は著者の一人です）。流石にAVX10.2の情報は載っていませんが、浮動小数点数のマニアックな話題や、SIMD命令の話題も載っているので、この記事に興味のある人は持っておいて損はないでしょう。

* [O'Reilly Japan - Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)
