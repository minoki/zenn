---
title: "x86-64機械語入門 AVX/AVX-512編"
emoji: "📘"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["x86", "asm", "機械語", "avx", "avx512"]
published: true
---

この記事は[x86-64機械語入門](x86-64-machine-code)の続きです。SSE/AVX/AVX-512の紹介と、それに伴うVEXプリフィックス、EVEXプリフィックスの紹介を行います。

## SSE

x86-64で単精度・倍精度の浮動小数点数を扱うには、SSE (Streaming SIMD Extensions) と呼ばれる機能（命令セットとレジスター）を使います。元々は命令セット拡張でしたが、x86-64ではSSE2が常に利用可能なので、標準で使える機能だと思って構いません。

:::message
伝統的には、x86系で浮動小数点数を扱うにはx87 FPUという機能がありました。しかしx87 FPUはIEEE 754準拠の観点（参考：[x87 FPUの呪い](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)）やレジスターの使い方が独特などの問題があり、別系統の浮動小数点演算器としてSSEが導入されることになりました。

x86-64ではSSE2が必ず利用可能なため、x86-64でわざわざx87 FPUを使う機会は「80ビットの拡張倍精度を使いたい」場合に限られるでしょう。
:::

SSEでは128ビット幅のレジスターを16本（32ビットモードでは8本）利用できます。データ型は無印SSEでは単精度浮動小数点数（32ビット幅）が、SSE2以降では倍精度浮動小数点数（64ビット幅）や整数が使えます。

SSEの128ビット幅のレジスターはXMMレジスターと呼ばれ、xmm0, xmm1, ..., xmm15のように番号をつけて呼ばれます。

SSEはSIMD演算だけでなくスカラーにも使われます（x87 FPUの代替）。

## SIMDプリフィックス

SSEの代表的な命令である浮動小数点数の加算 `ADD{S,P}{S,D}` について、オペコードを確認してみましょう。ちなみに最後の1文字がSのものは単精度 (sigle precision)、Dのものは倍精度 (double precision)です。最後から2番目の文字がSのものはスカラー（1要素）、Pのものはベクトル (packed) です。

* ADDSS: F3 0F 58
* ADDSD: F2 0F 58
* ADDPS: NP 0F 58
* ADDPD: 66 0F 58

まずわかるのは、最後の2バイトはいずれも0F 58であることです。このうち0Fは「2バイトからなるオペコードの1バイト目」を意味します（後述）。

最初の1バイトは66だったりF2だったりF3だったりします。これらはプリフィックスです。66はoperand-size prefixとして前回の記事でも言及しました。SIMD命令に使われる場合は66/F2/F3は特にSIMDプリフィックスと呼ばれるようです。

ADDPSは最初にNPと書かれています。これは「No Prefix」の意味です。つまり、オペコード0F 58にプリフィックスをつけない場合にADDPSと解釈され、プリフィックスをつけると精度やスカラー／ベクトルが切り替わるということです。

## 2バイト・3バイトオペコードと、オペコードの空間

x86の機械語は1命令が8ビット（1バイト）の整数倍で、最初の1バイトか数バイトはプリフィックスやオペコードに該当します。機械語の解釈が曖昧にならないようにするためには、プリフィックスやオペコードの最初の1バイトは被らないようにしなければなりません。1バイト、すなわち256通りです。

「最初の1バイト」のどれがどの命令を表すかは、Intel SDM Vol. 2のAppendix A, One-byte Opcode Mapに載っています。

さて、x86の命令の種類は256よりも多いので、いくつかの命令のオペコードは2バイト以上使って表されることになります。そういうオペコードの最初の1バイトは0Fとなっています。オペコード0Fは2-byte escapeと呼ばれています。オペコードが3バイトの命令は、0Fの次に38または3Aを続け、その後にその命令固有のオペコードが続きます。

つまり、オペコードは大まかには

* 〈1バイト〉
* 0F 〈1バイト〉
* 0F 38 〈1バイト〉
* 0F 3A 〈1バイト〉

のいずれかの形をしており、4つの表（あるいはマップ）で列挙できます。（実際にはオペコードが8ビット未満だったり、ModR/Mバイトも使うケースがあるのは、前回の記事で説明した通りです。）

Intel APXのドキュメントでは、それぞれ

* legacy map 0: エスケープを使わず、1バイトで表現できるもの
* legacy map 1: 0Fに続く1バイトで表現できるもの
* legacy map 2: 0F 38に続く1バイトで表現できるもの
* legacy map 3: 0F 3Aに続く1バイトで表現できるもの

という呼び方がされています。

EVEXプリフィックスを使うと、4番目以降のマップにもアクセスできます。これらはAVX512-FP16やAPXなどの新しい命令セット拡張で利用されます：

* map 4: APXで利用
* map 5: AVX512-FP16で利用
* map 6: AVX512-FP16で利用
* map 7: APXで利用

## AVX

AVX (Advanced Vector Extensions) ではSSEで導入されたベクトルレジスターの幅が256ビットに拡張されました。ベクトルレジスターを256ビット幅として参照する場合はYMMと呼ばれます。XMMはYMMの下位128ビットということになりました。

命令的には、3オペランド形式が使えるようになりました。これまではC言語風に書けば `x += y` という風に入力レジスターのいずれかを破壊しなければならなかったのが、`x = y + z` という風に出力レジスターに入力とは異なるものを指定できるようになったのです。

機械語的に重要なのは、AVXでVEXプリフィックス (vector extension prefix) という新たなプリフィックスが導入されたことです。

### VEXプリフィックス

前回の記事で説明したREXプリフィックスは、命令に対して16個のレジスターにアクセスする能力を与えたり、オペランドの幅を指定したりしました。

VEXプリフィックスもいくつかの機能を持ちます。具体的には以下です：

* ベクトルの幅を選択する（256ビットまたは128ビット）
* （出力とは別の）入力レジスターの指定
* REXプリフィックスの機能
* SIMDプリフィックスの機能
* オペコードのエスケープの部分（0F / 0F 38 / 0F 3A）の圧縮

VEXプリフィックスを使う命令の機械語は以下の部分から構成されます：

1. レガシーなプリフィックス（あれば）
    * ただしLOCK, 66, F2, F3, REXは使えない
2. VEXプリフィックス（2バイトまたは3バイト）
3. オペコード（1バイト）
4. ModR/M（1バイト）
5. SIBバイト（あれば。0〜1バイト）
6. 相対位置（あれば。0, 1, 2, 4バイト）
7. 即値（あれば。0〜1バイト）

VEXプリフィックスは2バイトの形式と3バイトの形式があります。2バイトの形式の最初の1バイトはC5、3バイトの形式の最初の1バイトはC4です。

2バイト形式：

```
1バイト目 (0xC5):
1100 0101

2バイト目：
Rvvv vLpp
^\___/^\/
|  |  | +- SIMDプリフィックスの代用
|  |  +--- ベクトル長
|  +------ レジスター番号（ビット反転）
+--------- REX.R（ビット反転）
```

3バイト形式：

```
1バイト目 (0xC4):
1100 0100

2バイト目：
RXBm mmmm
^^^\____/
|||   +--- オペコードマップの選択
||+------- REX.B（ビット反転）
|+-------- REX.X（ビット反転）
+--------- REX.R（ビット反転）

3バイト目：
Wvvv vLpp
^\___/^\/
|  |  | +- SIMDプリフィックスの代替
|  |  +--- ベクトル長
|  +------ レジスター番号（ビット反転）
+--------- REX.Wの類似
```

3バイト形式のフィールドは2バイト形式のフィールドを全て含むので、2バイト形式でエンコードできる命令は3バイト形式でもエンコードできます。

`vvvv` の部分はレジスター番号（0から15）をビット反転した形で指定します。使用しない命令の場合は0b1111をセットします。命令のオペランドの説明で参照する際は `VEX.vvvv` と呼ばれます。

REXプリフィックスにあったR, X, Bフィールドの相当物は、VEXプリフィックスの中ではビット反転した形で指定します。

`L` はベクトル長で、0なら128ビット、1なら256ビットです。オペコードの説明のところではVEX.128またはVEX.256と書かれます。128/256の代わりにLIGと書かれていれば `L` の値は無関係 (ignore) であることを意味します。LZであれば、`L` は0でなければなりません。

`pp` はSIMDプリフィックスの代用で、0b00ならNo Prefix、0b01は66、0b10はF3、0b11はF2に対応します。

`mmmmm` はオペコードのマップを切り替えます。つまり、0F, 0F 38, 0F 3Aのいずれかを表現できます。0b00001は0Fを、0b00010は0F 38を、0b00011は0F 3Aを表します。他の値はreservedです。2バイト形式の場合は常に0Fが補われます。

`pp` と `mmmmm` が他のプリフィックスやエスケープの役割を果たすので、VEXプリフィックスの採用による命令長の増加は緩和されます。

VEXプリフィックスを使用してエンコードされた128ビット命令は、出力レジスターの上位ビット（幅が256ビットの場合は上位128ビット）をクリアします。

Intel SDMにおける個々の命令の説明では、VEXプリフィックスは `VEX.{128,256,LIG,LZ}.{NP,66,F2,F3}.{0F,0F3A,0F38}.{W0,W1,WIG}` の形で書かれます。例えば128ビットの `VADDPS` 命令であれば `VEX.128.0F.WIG` という感じです。これは `L=0`, `pp=0b01`, `mmmmm=0b00001`（2バイト形式も可）, `W` は任意であることを意味します。詳しくはIntel SDM Volume 2 3.1.1.2を参照してください。

## AVX-512

AVX-512ではベクトルレジスターの幅が512ビットに拡張されました。ベクトルレジスターを512ビット幅として参照する場合はZMMと呼ばれます。YMMはZMMの下位256ビットということになりました。レジスターの個数も16個から32個に倍増しています（64ビットモード）。

機械語的に重要なのは、EVEX (enhanced VEX) プリフィックスという新たなプリフィックスが導入されたことです。これによって、追加された各種機能にアクセスできるようになります。

特徴をまとめると次のようになります：

* VEXプリフィックスの機能
* ベクトルレジスターの幅を512ビットに拡張
* SIMDレジスターの個数を32個に拡張
* マスク
* 命令の種類に応じた機能：埋め込みブロードキャスト、静的な丸めモード指定、浮動小数点例外の状態フラグ操作の抑制

### EVEXプリフィックス

EVEXプリフィックスは4バイトです。最初の1バイトは固定で、残りの3バイトに情報が詰め込まれます。

```
1バイト目 (0x62):
0110 0010

2バイト目（P0=P[7:0]）：
RXBR' 0mmm
^^^^  ^\_/
||||  | +-- オペコードマップの選択
||||  +---- 予約済み（APXではB4 (ModR/M.r/m, SIB.baseの4ビット目）
|||+------- ModR/M.regの4ビット目（ビット反転）
||+-------- REX.Bと同等（ModR/M.r/m, SIB.baseの3ビット目）（ビット反転）
|+--------- REX.Xと同等（SIB.indexの3ビット目）（ビット反転）
+---------- REX.Rと同等（ModR/M.regの3ビット目）（ビット反転）

3バイト目（P1=P[15:8]）：
Wvvv v1pp
^\___/^\/
|  |  | +- SIMDプリフィックスの代替（VEX.ppと同じ）
|  |  +--- 固定（APXではSIB.indexの4ビット目のビット反転、AVX10では256ビットの静的丸め指定に活用）
|  +------ レジスター番号（VEX.vvvvと同じ）（ビット反転）
+--------- Operand size promotion/Opcode extension

4バイト目（P2=P[23:16]）：
zL'Lb V'aaa
^\_/^ ^ \_/
| | | |  +-- opmask register specifier
| | | +----- vvvvと組み合わせる（ビット反転）
| | +------- broadcast/RC/SAE context
| +--------- Vector length/RC
+----------- Zeroing/Merging
```

オペコードマップは先述の通りで、

* 1 (0b001): 0Fに対応
* 2 (0b010): 0F38に対応
* 3 (0b011): 0F3Aに対応
* 4 (0b100): APXで利用
* 5 (0b101): AVX512-FP16で利用
* 6 (0b110): AVX512-FP16で利用
* 7 (0b111): APXで利用

となっています。

レジスターの指定は

* REG: EVEX.R':EVEX.R:modrm.regの合計5ビット
* VVVV: EVEX.V':EVEX.vvvvの合計5ビット
    * なんでV'とvvvvの位置が離れてるんですかね？
* SIBがない場合：
    * RM: EVEX.X:EVEX.B:modrm.r/mの合計5ビット
* SIBがある場合：
    * BASE: EVEX.B:modrm.r/mの合計4ビット（APXでは1ビット追加されて5ビットになる）
    * INDEX: EVEX.X:sib.indexの合計4ビット（APXでは1ビット追加されて5ビットになる）
    * VIDX: EVEX.V':EVEX.X:sib.indexの合計5ビット

となります。

EVEX.bの役割について。丸めを伴う浮動小数点命令でオペランドがレジスターのみの場合、EVEX.b=1ならば静的な丸め指定とSAEが有効になります。丸めは

* EVEX.RC=0b00: to nearest
* EVEX.RC=0b01: downward
* EVEX.RC=0b10: upward
* EVEX.RC=0b11: toward zero

となります。EVEX.L'LとEVEX.RCは実体が同一なので、ベクトル長は指定できません（スカラーと512ビットのみ）。メモリーから読み込んで演算する命令の場合は、EVEX.bはブロードキャストを制御します。他の命令ではEVEX.bは0でなくてはなりません。

EVEX.zについて。EVEX.z=0がmerging-maskingに、EVEX.z=1がzeroing-maskingに対応します。

Intel SDMにおける個々の命令の説明では、EVEXプリフィックスは `EVEX.{128,256,512,LLIG,LLZ}.{NP,66,F2,F3}.{0F,0F3A,0F38,MAP4,MAP5,MAP6,MAP7}.{W0,W1,WIG}` の形で書かれます。詳しくは次のとおりです：

* `{128,256,512,LLIG,LLZ}`: ベクトル長を表します。EVEX.L'Lでエンコードされます（EVEX.RCを使わない場合）。LLIGはベクトル長が無視されることを意味します（スカラー命令など）。LLZはEVEX.L'Lが0でなければならないことを表します。
    * EVEX.L'L=0b00: 128ビット
    * EVEX.L'L=0b01: 256ビット
    * EVEX.L'L=0b10: 512ビット
    * EVEX.L'L=0b11: 予約済み
* `{NP,66,F2,F3}`: SIMDプリフィックス相当のやつです。EVEX.ppでエンコードされます。
    * EVEX.pp=0b00: No Prefix
    * EVEX.pp=0b01: 66
    * EVEX.pp=0b10: F3
    * EVEX.pp=0b11: F2
* `{0F,0F3A,0F38,MAP4,MAP5,MAP6,MAP7}`: オペコードマップを表します。EVEX.mmmでエンコードされます。
    * EVEX.mmm=0b001: 0F
    * EVEX.mmm=0b010: 0F38
    * EVEX.mmm=0b011: 0F3A
    * EVEX.mmm=0b100: MAP4
    * EVEX.mmm=0b101: MAP5
    * EVEX.mmm=0b110: MAP6
    * EVEX.mmm=0b111: MAP7
* `{W0,W1,WIG}`: EVEX.Wの値を表します。W0はEVEX.W=0、W1はEVEX.W=1、WIGはEVEX.Wが無視されることを表します。

例えば512ビットの `VADDPS` 命令であれば `EVEX.512.0F.W0` という感じです。これは `L'L=0b10`, `pp=0b00`, `mmm=0b001`, `W=0` であることを意味します。詳しくはIntel SDM Volume 2 3.1.1.2を参照してください。

## 命令の例

### ADDPS命令

単精度浮動小数点数からなるベクトルの加算を行うADDPS/VADDPS命令を見てみましょう。

> ADDPS -- Add Packed Single-Precision Floating-Point Values
> | Opcode/Instruction | Op/En | 64/32 bit Mode Support | CPUID Feature Flag | Description |
> |-|-|-|-|-|
> | NP 0F 58 /r<br>ADDPS xmm1, xmm2/m128 | A | V/V | SSE | Add packed single-precision floating-point values from xmm2/m128 to xmm1 and store result in xmm1 |
> | VEX.128.0F.WIG 58 /r<br>VADDPS xmm1, xmm2, xmm3/m128 | B | V/V | AVX | Add packed single-precision floating-point values from xmm3/m128 to xmm2 and store result in xmm1. |
> | VEX.256.0F.WIG 58 /r<br>VADDPS ymm1, ymm2, ymm3/m256 | B | V/V | AVX | Add packed single-precision floating-point values from ymm3/m128 to ymm2 and store result in ymm1. |
> | EVEX.128.0F.W0 58 /r<br>VADDPS xmm1 {k1}{z}, xmm2, xmm3/m128/m32bcst | C | V/V | AVX512VL<br>AVX512F | Add packed single-precision floating-point values from xmm3/m128/m32bcst to xmm2 and store result in xmm1 with writemask k1. |
> | EVEX.256.0F.W0 58 /r<br>VADDPS ymm1 {k1}{z}, ymm2, ymm3/m256/m32bcst | C | V/V | AVX512VL<br>AVX512F | Add packed single-precision floating-point values from ymm3/m256/m32bcst to ymm2 and store result in ymm1 with writemask k1. |
> | EVEX.512.0F.W0 58 /r<br>VADDPS zmm1 {k1}{z}, zmm2, zmm3/m512/m32bcst {er} | C | V/V | AVX512F | Add packed single-precision floating-point values from zmm3/m512/m32bcst to zmm2 and store result in zmm1 with writemask k1. |
>
> Instruction Operand Encoding
> | Op/En | Tuple Type | Operand 1 | Operand 2 | Operand 3 | Operand 4 |
> |-|-|-|-|-|-|
> | A | NA | ModRM:reg (r, w) | ModRM:r/m (r) | NA | NA |
> | B | NA | ModRM:reg (w) | VEX.vvvv (r) | ModRM:r/m (r) | NA |
> | C | Full | ModRM:reg (w) | EVEX.vvvv (r) | ModRM:r/m (r) | NA |

まずは、SSEの基本形の機械語を見てみます。NPはNo Prefixで、0F 58は2バイトオペコード、/rはModR/Mのreg/opcodeにレジスター番号の下位3ビットを格納するという意味です。

例えば、`addps xmm3, xmm5` は

```
opcode:
0x0F
0x58

ModR/M:
0b11 011 101 = 0xDD
     \_/ \_/
      |   +-- r/m (operand 2)
      +------ reg (opreand 1)
```

により、`0F 58 DD` の3バイトでエンコードできます。

別の例として、`addps xmm3, xmm10` は

```
REX.B:
0b0100 0001 = 0x41
       ^^^^
       |||+- B (operand 2の拡張)
       ||+-- X (indexの拡張)
       |+--- R (operand 1の拡張)
       +---- W

opcode:
0x0F
0x58

ModR/M:
0b11 011 010 = 0xDA
     \_/ \_/
      |   +-- r/m (operand 2)
      +------ reg (operand 1)
```

により、`41 0F 58 DA` の4バイトでエンコードできます。

次に、VEX.128の版を見てみます。アセンブリー言語でのニーモニックは `vaddps` になります。これとSSE版との違いは、オペランドの数と、上位ビットをクリアするか否かです。

例えば、`vaddps xmm3, xmm5, xmm10` は

```
VEX (3-byte):
0xC4

0b110 00001 = 0xC1
  ^^^ \___/
  |||   +---- mmmmm: 0b00001=0F
  ||+-------- B（operand 3の拡張）（ビット反転）
  |+--------- X（ビット反転）
  +---------- R（operand 1の拡張）（ビット反転）

0b0 1010 0 00 = 0x50
  ^ \__/ ^ \/
  |   |  | +-- pp: 00=No Prefix
  |   |  +---- L: 0=128-bit
  |   +------- vvvv（operand 2）（ビット反転）
  +----------- W

opcode:
0x58

ModR/M:
0b11 011 010 = 0xDA
     \_/ \_/
      |   +-- r/m (operand 3)
      +------ reg (operand 1)
```

により、`C4 C1 50 58 DA` とエンコードされます。VEX.Bを指定する必要があるため、2バイト形式のVEXではなく3バイト形式を使用しました。

VEX.256のものは、アセンブリー言語では `ymm` をオペランドに取ります。例えば、`vaddps ymm15, ymm3, ymm0` は

```
VEX (2-byte):
0xC5

0b0 1100 1 00 = 0x64
  ^ \__/ ^ \/
  |   |  | +-- pp: 00=No Prefix
  |   |  +---- L: 1=256-bit
  |   +------- vvvv（operand 2）（ビット反転）
  +----------- R（operand 1の拡張）（ビット反転）

opcode:
0x58

ModR/M:
0b11 111 000 = 0xF8
     \_/ \_/
      |   +-- r/m (operand 3)
      +------ reg (operand 1)
```

により、`C5 64 58 F8` とエンコードできます。

EVEX.512のものは、アセンブリー言語では `zmm` をオペランドに取ります。例えば、`vaddps zmm15, zmm24, zmm3` は

```
EVEX:
0x62

0b0111 0 001 = 0x71
  ^^^^ ^ \_/
  |||| |  +-- オペコードマップの選択：0b001=0F
  |||| +----- 予約済み
  |||+------- R': operand 1の4ビット目（ビット反転）
  ||+-------- B: operand 3の3ビット目（ビット反転）
  |+--------- X: operand 3の4ビット目（ビット反転）
  +---------- R: operand 1の3ビット目（ビット反転）

0b0 0111 1 00 = 0x3C
  ^ \__/ ^ \/
  |   |  |  +- pp: 00=No Prefix
  |   |  +---- 固定
  |   +------- vvvv（operand 2）（ビット反転）
  +----------- W: W0

0b0 10 0 0 000 = 0x40
  ^ \/ ^ ^ \_/
  |  | | |  +-- opmask register specifier
  |  | | +----- V': operand 2の4ビット目（ビット反転）
  |  | +------- b: broadcast/RC/SAE context
  |  +--------- L'L/RC: Vector length=512-bit
  +------------ Zeroing/Merging

opcode:
0x58

ModR/M:
0b11 111 011 = 0xFB
     \_/ \_/
      |   +-- r/m (operand 3)
      +------ reg (operand 1)
```

により、`62 71 3C 40 58 FB` とエンコードできます。

broadcastを使う例も見てみましょう。`vaddps ymm13, ymm30, [r12] {1to8}` は

```
EVEX:
0x62

0b0101 0 001 = 0x51
  ^^^^ ^ \_/
  |||| |  +-- オペコードマップの選択：0b001=0F
  |||| +----- 予約済み
  |||+------- R': operand 1の4ビット目（ビット反転）
  ||+-------- B: baseの3ビット目（ビット反転）
  |+--------- X: indexの4ビット目（ビット反転）
  +---------- R: operand 1の3ビット目（ビット反転）

0b0 0001 1 00 = 0x0C
  ^ \__/ ^ \/
  |   |  |  +- pp: 00=No Prefix
  |   |  +---- 固定
  |   +------- vvvv（operand 2）（ビット反転）
  +----------- W: W0

0b0 01 1 0 000 = 0x30
  ^ \/ ^ ^ \_/
  |  | | |  +-- opmask register specifier
  |  | | +----- V': operand 2の4ビット目（ビット反転）
  |  | +------- b: broadcast
  |  +--------- L'L/RC: Vector length=256-bit
  +------------ Zeroing/Merging

opcode:
0x58

ModR/M:
0b00 101 100 = 0x2C
     \_/ \_/
      |   +-- r/m (operand 3)
      +------ reg (operand 1)

SIB:
0b00 100 100 = 0x24
  \/ \_/ \_/
   |  |   +-- base
   |  +------ index
   +--------- scale: 任意
```

により、`62 51 0C 30 58 2C 24` とエンコードできます。

たとえxmmしか使わなくても、xmm16以降にアクセスするにはEVEXを使ったエンコーディングが必要です。したがって、`addps xmm16, xmm24` は不正な命令であり、`vaddps` を使う必要があります。
