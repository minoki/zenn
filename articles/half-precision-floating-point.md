---
title: "半精度（16ビット）浮動小数点数をC言語から使う"
emoji: "📚"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c", "float", "浮動小数点数"]
published: true
---

通常我々が使う浮動小数点数の形式（フォーマット）は倍精度（64ビット）や単精度（32ビット）が多いですが、世の中には他にも浮動小数点形式があります。

精度が高い方に目を向けると、80ビットの拡張倍精度や128ビットの四倍精度などがあります。これらについては以前記事を書きました。

* [C言語で四倍精度浮動小数点数 (binary128) を使う](https://qiita.com/mod_poppo/items/8a61bdcc44d8afb5caed)
* [x87 FPUの呪い](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)

浮動小数点数というと二進を思い浮かべる人が多いと思いますが、IEEE 754では十進形式も定められています。これも記事を書きました。

* [IEEE 754の十進浮動小数点数の基本](https://zenn.dev/mod_poppo/articles/ieee754-decimal)

一方この記事では、二進で精度が低い形式、具体的にはIEEE 754で言うところのbinary16を取り扱います。半精度 (half precision) と呼ばれることもあります。

## binary16形式と用途

IEEE 754-2008以降では、複数のシステムで値をやり取りするための二進交換形式 (binary interchange format) の一つとしてbinary16形式が定められています。具体的なパラメーターは以下の通りです：

* 幅：16ビット
* 精度：11ビット（二進11桁）
* 指数部の最大値：15
    * 仮数部を$[0,2)$の区間に取った時の指数部です。
* 指数部の最小値：-14
* 指数部をビット列表現する時のバイアス：15
* 指数部の幅：5ビット
* 仮数部下位の幅：10ビット（ケチ表現を使う）

正の最小値は$2^{-24}\approx 5.96\times 10^{-8}$で、正の正規化数の最小値は$2^{-14}\approx 6.10\times 10^{-5}$です。有限の最大値は$(2-2^{-10})\times2^{15}=65504$です。

計算結果の真の値の絶対値が$(2-2^{-10}+2^{-11})\times2^{15}=65520$以上になると、最近接丸めでオーバーフローが発生します。

binary16で表現できない正の整数で最小のものは$2^{11}+1=2049$です。絶対値が$2048$以下の整数はbinary16で正確に表現できます。

用途としては、画像の画素を表現するのに使われたりすることがあるようです。最近は機械学習にも使われるのでしょうか（機械学習向けには指数部の幅が広いbfloat16もありますが）。

## C言語での扱い

2024年に発行予定のC標準、C23では、Annex HでIEEE 754の交換形式のサポートが盛り込まれました（C23以前にTechnical Specificationがありましたが、ここでは紹介しません）。もちろん、処理系がそれを実装するかは任意（オプショナル）です。

C言語的には、binary16に対応する型は `_Float16` となります。リテラルのサフィックスは `f16` あるいは `F16` となります。

処理系が `_Float16` をサポートしているか問い合わせる直接的な方法はなさそうです。`<float.h>` で `FLT16_MIN` みたいなマクロが定義されているか確かめれば良いのでしょうか。

C言語では、浮動小数点演算は名目上の型ではなく、より高精度な型（形式）で行われる場合があります。`FLT_EVAL_METHOD` とか `float_t` とか `double_t` とかそういう話ですね。`_FloatN` 型をサポートするために `FLT_EVAL_METHOD` の仕様にも拡張が加えられています。

可変長引数に `float` を渡すと暗黙に `double` に変換されますが、`_Float16` にはそういう暗黙の変換 (default argument promotion) は起こりません（[N2844](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2844.pdf); `_Float32` や `_Float64` にもそういう変換は起こりません）。

ライブラリー関数は、ユーザーが `__STDC_WANT_IEC_60559_TYPE_EXT__` マクロを事前に定義した場合のみ定義されます。関数名は、典型的には `f16` がつきます。

```c
_Float16 sqrtf16(_Float16 x);
```

詳しいことはC23のAnnex Hを見てください。

各コンパイラーでの状況は、それぞれのマニュアルを見てください。

* [Half-Precision (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Half-Precision.html)
* [Clang Language Extensions — Clang 19.0.0git documentation](https://clang.llvm.org/docs/LanguageExtensions.html#half-precision-floating-point)

## C++での扱い

C++ではC++23以降でサポートが追加され、 `<stdfloat>` で定義される `std::float16_t` という風になるようです。参考：

* [Fixed width floating-point types (since C++23) - cppreference.com](https://en.cppreference.com/w/cpp/types/floating-point)

ここではこれ以上詳しくは取り扱いません。

## ハードウェア実装について

GPUは以前から半精度浮動小数点数をサポートしているものがそれなりにあったと思います（ちゃんと確かめていませんが）。

最近はCPUも半精度浮動小数点数に対応していることがあるので、この記事ではそれを紹介します。

### ハードウェア実装：Arm編

Armv8.0には半精度形式と他の形式の変換ができる命令が搭載されています。具体的には、半精度から単精度／倍精度の変換、倍精度／単精度から半精度への変換ができます。

Armv8.2では、オプショナルな機能FEAT_FP16として半精度浮動小数点数に対する演算のサポートが追加されています。実際にAppleのCPUで使えたりします。Macであれば `sysctl` コマンドで確かめられます：

```
$ sysctl hw.optional.arm.FEAT_FP16
hw.optional.arm.FEAT_FP16: 1
```

Linuxだと `/proc/cpuinfo` とかを見れば良いでしょう。`fphp` が入っていれば対応しています（多分）。

```
$ grep fphp /proc/cpuinfo
Features	: fp asimd evtstrm aes pmull sha1 sha2 crc32 atomics fphp asimdhp cpuid asimdrdm jscvt fcma lrcpc dcpop sha3 asimddp sha512 asimdfhm dit uscat ilrcpc flagm sb paca pacg dcpodp flagm2 frint
```

ArmのC拡張では `__fp16` という型を定義していますが、ここでは詳しく取り上げません。

試しに、次のコードをコンパイルしてみましょう。

```c
#include <stddef.h>

void add_fp16(size_t n, const _Float16 a[n], const _Float16 b[n], _Float16 result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

Macの場合、特にオプションをつけなくてもFEAT\_FP16の命令が使われます。Appleの作るCPUは常にFEAT\_FP16に対応しているだろう、ということですね。

```
$ clang -O3 -S add_fp16.c
$ grep fadd add_fp16.s
	fadd.8h	v0, v0, v4
	fadd.8h	v1, v1, v5
	fadd.8h	v2, v2, v6
	fadd.8h	v3, v3, v7
	fadd	h0, h0, h1
```

Linuxの場合は `-march=armv8-a+fp16` みたいなオプションをつけるとFEAT\_FP16の命令が使われます。世の中にはFEAT\_FP16に対応していないArm CPUもあり、Linuxはそういう環境も想定しないといけませんからね。

```
$ gcc -O3 -S -march=armv8-a+fp16 fp16.c
$ grep fadd fp16.s
	fadd	v0.8h, v0.8h, v1.8h
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
	fadd	h0, h0, h1
```

LinuxでFEAT\_FP16を有効化しなかった場合は、`fcvt` 命令で一旦単精度に変換してから単精度で演算し、また半精度に戻すようなコードが出力されます。

### ハードウェア実装：x86編

x86の半精度対応を見てみます。

#### F16C

x86で半精度浮動小数点数に対応した命令は、F16C拡張が最初のはずです。IntelだとIvy Bridge以降で対応しているようです。

F16Cは半精度と単精度のベクトルを相互変換する命令 `VCVTPH2PS`, `VCVTPS2PH` を追加します。倍精度との相互変換はサポートされていないので注意してください。

対応する組み込み関数は以下のようになります：

```c
#include <immintrin.h>
__m128 _mm_cvtph_ps(__m128i a); // 半精度の値4個を単精度の値4個に変換する
__m256 _mm256_cvtph_ps(__m128i a); // 半精度の値8個を単精度の値8個に変換する
__m128i _mm_cvtps_ph(__m128 a, int imm8); // 単精度の値4個を半精度の値4個に変換する。丸めモードを指定できる
__m128i _mm256_cvtps_ph(__m256 a, int imm8); // 単精度の値8個を半精度の値8個に変換する。丸めモードを指定できる
```

先ほどの関数をF16Cを有効にした状態でコンパイルすると、一旦半精度から単精度に変換して演算を行い、また半精度に戻すような命令列が出力されます。

```
$ gcc -O3 -mf16c -S add_fp16.c
$ grep -2 vcvt add_fp16.s
        vpinsrw $0, (%rsi,%rax,2), %xmm2, %xmm0
        vpinsrw $0, (%rdx,%rax,2), %xmm2, %xmm1
        vcvtph2ps       %xmm0, %xmm0
        vcvtph2ps       %xmm1, %xmm1
        vaddss  %xmm1, %xmm0, %xmm0
        vinsertps       $0xe, %xmm0, %xmm0, %xmm0
        vcvtps2ph       $4, %xmm0, %xmm0
        vpextrw $0, %xmm0, (%rcx,%rax,2)
        addq    $1, %rax
```

さて、倍精度から半精度への変換を行いたい時はどうすればいいでしょうか？一旦倍精度から単精度に変換し、改めてF16C拡張で半精度に変換するのではダメでしょうか？ダメです。

十六進表記で `0x1.005fff8p0` となる倍精度浮動小数点数を最近接丸め（偶数丸め）で半精度に変換することを考えましょう。元の数の二進表記は

```
0b1.0000 0000 0101 1111 1111 1111 1000 0000 0000 0000 0000 0000 0000
```

となり、これに最も近い半精度浮動小数点数は

```
0b1.0000 0000 01 = 0x1.004p0
```

となります。しかし、単精度を経由して変換すると

```
0b1.0000 0000 0101 1111 1111 1111 1000 0000 0000 0000 0000 0000 0000
↓
0b1.0000 0000 0110 0000 0000 000
↓
0b1.0000 0000 10 = 0x1.008p0
```

と、正しい（元の数に最も近い）値とは異なる値が得られました。一般に二段階で最近接丸めを行うとこのような問題が起きることがあり、私の同人誌「浮動小数点数小話」ではその辺（二段階丸め (double rounding)）に触れています。

* [だめぽラボ 「浮動小数点数小話」](https://lab.miz-ar.info/floating-point/)

こういう問題があるので、GCCで `double` から `_Float16` へのキャストを行うと、F16Cを有効にしていてもF16Cの命令は使用されず、GCCの組み込み関数が使用されます。

```
$ cat double_to_half.c
_Float16 convert(double x) { return (_Float16)x; }
$ gcc -S -mf16c -O2 double_to_half.c
$ cat double_to_half.c
...
        call    __truncdfhf2@PLT
...
```

#### AVX-512 FP16

AVX-512 FP16拡張では、半精度浮動小数点数に関する各種演算の命令が追加されました。皆さんは対応CPUを持っていますか？私は持っていません。Sapphire Rapidsってやつを買えばいいんですかね。

最近のコンパイラー（GCCだと12以降）はAVX-512 FP16対応のコードを出力できます。先ほどの足し算関数をAVX-512 FP16を有効にした状態でコンパイルすると、半精度のまま演算を行なっているのが見て取れます。

```
$ gcc -O3 -mavx512fp16 -S add_fp16.c
$ grep -2 vaddph add_fp16.s
.L4:
        vmovdqu16       (%rdi,%rax), %zmm1
        vaddph  (%rsi,%rax), %zmm1, %zmm0
        vmovdqu16       %zmm0, (%rcx,%rax)
        addq    $64, %rax
```

AVX-512 FP16には `vcvtsd2sh` という、倍精度から半精度に直接変換する命令があるので、`double` から `_Float16` へのキャストにはこれを使用できます。

#### AVX10.1

AVX10.1にはAVX-512 FP16に相当する命令が含まれるようなので、AVX-512が苦痛にまみれた死を遂げても大丈夫です。安心した！
