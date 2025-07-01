---
title: "bfloat16 (brain float16) のCPUによる対応とC言語からの利用"
emoji: "🧠"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["float", "浮動小数点数"]
published: true
---

## bfloat16とは

bfloat16 (brain float16) は深層学習で使われる浮動小数点形式で、元々Googleが考案したらしいです。

* [Tearing Apart Google’s TPU 3.0 AI Coprocessor](https://www.nextplatform.com/2018/05/10/tearing-apart-googles-tpu-3-0-ai-coprocessor/) (2018-05-10)
* [BFloat16: The secret to high performance on Cloud TPUs | Google Cloud Blog](https://cloud.google.com/blog/products/ai-machine-learning/bfloat16-the-secret-to-high-performance-on-cloud-tpus?hl=en) (2019-08-24)

IEEE 754で標準化された16ビット浮動小数点形式としてbinary16がありますが、bfloat16はbinary16と比べて指数部を広く（binary32と同じ）取っているのが特徴です。深層学習では精度よりもダイナミックレンジの方が重要だとか云々。

```
binary32:
s eeeeeeee mmmm mmmm mmmm mmmm mmmm mmm
^    ^       ^
符号  指数部  仮数部下位 

binary16:
s eeeee mmmm mmmm mm
^    ^       ^
符号  指数部  仮数部下位 

bfloat16:
s eeeeeeee mmmm mmm
^    ^       ^
符号  指数部  仮数部下位 
```

| 形式 | 符号 | 指数部 | 仮数部下位 |
|:-:|:-:|:-:|:-:|
| binary32 | 1ビット | 8ビット | 23ビット |
| binary16 | 1ビット | 5ビット | 10ビット |
| bfloat16 | 1ビット | 8ビット | 7ビット |

## binary32との変換

指数部の幅が同じなので、binary32からbfloat16に変換するのは上位16ビットを取り出すだけで可能となります。ただ、この方法だと端数が単なる切り捨てになってしまいます。

上位16ビットを取り出す前にbinary32のビット列表現に `0x8000` を加えておけば、四捨五入の二進法版、零捨一入（IEEE 754用語で言えばroundTiesToAway）ができそうです（できるよね？）。元の浮動小数点数がNaNの場合はこれだとまずいかもしれませんが、入力がx86やArmなどの標準的なNaNであると仮定できるならチェックを省いても問題ないでしょう。

もうちょっとビット演算をガチャガチャやれば偶数丸め（roundTiesToEven）もできるでしょう。

bfloat16からbinary32への変換は、単に上位16ビットに設定すれば大丈夫です。

C言語で変換関数を書くなら、次のようになるでしょう：

```c
typedef uint16_t bf16;

bf16 binary32_to_bfloat16_trunc(float x)
{
    uint32_t u;
    memcpy(&u, &x, 4);
    return u >> 16;
}

bf16 binary32_to_bfloat16_round(float x)
{
    uint32_t u;
    memcpy(&u, &x, 4);
    return (u + 0x8000) >> 16;
}

float bfloat16_to_binary32(bf16 x)
{
    uint32_t u = (uint32_t)x << 16;
    float x;
    memcpy(&x, &u, 4);
    return x;
}
```

というわけで、bfloat16とbinary32の変換は、丸め方法にこだわらなければCPU側に特別な命令がなくてもできそうですね。

## C/C++での扱い

C++23では `<stdfloat>` で `std::bfloat16_t` が使えるようになるようです。

* [Fixed width floating-point types (since C++23) - cppreference.com](https://en.cppreference.com/w/cpp/types/floating-point.html)

x86のAVX-512 BF16拡張ではスカラーの `__bfloat16` 型とベクトルの `__m128bh`, `__m256bh`, `__m512bh` 型が導入されるようです。

ArmのC拡張 (ACLE) では、 `<arm_neon.h>` でスカラーの `bfloat16_t` 型（`__bf16` 型のエイリアス）と、ベクトルの `bfloat16x4_t`, `bfloat16x8_t` 型が導入されるようです。

C言語でも標準化されてほしい……。

## ハードウェア実装というかCPU実装について

最近のCPUやGPUにはbfloat16向けの変換命令や演算命令が搭載されていることがあります。この記事では、CPUでの取り扱いを見てみます。

### x86編

[Intel Intrinsics Guide](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html)によると、bfloat16のスカラーは `__bfloat16` 型で、ベクトルは `__m128bh`, `__m256bh`, `__m512bh` 型で表現されるようです。

#### AVX-512 BF16

* VCVTNE2PS2BF16 (EVEX): binary32→bfloat16の変換。2本のベクトルを1本のベクトルに変換する。
    * roundTiesToEvenにより丸める。
    * 出力が非正規化数になる場合は代わりに0が出力され、入力の非正規化数は0として扱われる。
    * MXCSRは参照も更新もされない。例外は抑制される。
* VCVTNEPS2BF16 (EVEX): binary32→bfloat16の変換。1本のベクトルを1本のベクトルに変換する。
    * 詳細はVCVTNE2PS2BF16と同様。
* VDPBF16PS: bfloat16ベクトル2本のドット積を計算し、binary32のアキュムレーターに加える。
    * 演算の際の詳細はVCVTNE2PS2BF16と同様。

AVX-512 BF16はAVX10.1の一部となります。

#### AVX-NE-CONVERT

* VCVTNEEBF162PS: メモリ上のbfloat16ベクトルの偶数番目の要素をbinary32に変換する。
* VCVTNEOBF162PS: メモリ上のbfloat16ベクトルの奇数番目の要素をbinary32に変換する。
* VCVTNEPS2BF16 (VEX): AVX-512 BF16の同名の命令と同じことをする。VEXでエンコードされる。

#### AMX-BF16

* TDPBF16PS: bfloat16×bfloat16→binary32のドット積を行う。roundTiesToEvenで丸められる。入出力の非正規化数は0扱いされる。MXCSRは参照も更新もされない。

#### AVX10.2

AVX10.2ではbfloat16に対する四則演算等の命令が追加される見込みです。

[Intel® Advanced Vector Extensions 10.2 (Intel® AVX10.2) Architecture Specification](https://www.intel.com/content/www/us/en/content-details/856721/intel-advanced-vector-extensions-10-2-intel-avx10-2-architecture-specification.html)

* VADDBF16
* VCMPBF16
* VCOMISBF16
* VDIVBF16
* VF[,N]M[ADD,SUB][132,213,231]BF16
* VFPCLASSBF16
* VGETEXPBF16
* VGETMANTBF16
* VMAXBF16
* VMINBF16
* VMULBF16
* VRCPBF16
* VREDUCEBF16
* VRNDSCALEBF16
* VRSQRTBF16
* VSCALEFBF16
* VSQRTBF16
* VSUBBF16
* VMINMAXBF16: IEEE 759-2019準拠のminimum/maximum系演算
* VCVTBF162I[,U]BS: bfloat16→整数の変換

### Arm編

[Clang Language Extensions — Clang documentation](https://clang.llvm.org/docs/LanguageExtensions.html#half-precision-floating-point)

Clang的には `__bf16` が組み込みの型として提供されていて、`<arm_neon.h>` によって `bfloat16_t` がエイリアスとして提供されるようです。

Armのbfloat16対応はFEAT_BF16とFEAT_EBF16の2つの拡張で提供されます。

FEAT_BF16: Armv8.2以降のオプショナルな機能で、Armv8.6以降で必須です。

* BFCVT: スカラーのbinary32→bfloat16変換を行う。
    * `FPCR.AH == 1` の場合はroundTiesToEvenで丸め、入出力の非正規化数は0となる。そうでない場合はFPCRに従う。
* BFCVTN, BFCVTN2: ベクトルのbinary32→bfloat16変換を行う。
    * `FPCR.AH == 1` の場合はroundTiesToEvenで丸め、入出力の非正規化数は0となる。そうでない場合はFPCRに従う。
* BFCVTNT (SVE): ベクトルのbinary32→bfloat16変換を行う。
* BFDOT: bfloat16のドット積を計算する。
    * FEAT_EBF16が実装されていて `FPCR.EBF == 1` の場合は挙動が少し変わる。
* BFMLALB, BFMLALT: 積和bfloat16×bfloat16+binary32→binary32を計算する。
* BFMMLA: bfloat16を要素とする2×2行列の乗算を行う。

FEAT_EBF16: Armv8.2以降のオプショナルな機能で、拡張された動作を可能にする。具体的には、丸めモードやflush to zeroの制御が可能になる。

binary64等の高い精度の値をbinary32を経由してもっと低い精度の値に変換する場合、普通にやると「二段階丸め」(double rounding) の問題が発生します（詳しくは私の同人誌「[浮動小数点数小話](https://lab.miz-ar.info/floating-point/)」を参照してください）。この問題は、奇数丸め (round to odd) という丸め方法を使うことで解消できます。Armには、round to oddでbinary64→binary32の変換を行う命令がいくつか用意されています：

* FCVTXN, FCVTXN2, FCVTX (SVE2), FCVTXNT (SVE2)

### RISC-V編

RISC-Vにもbfloat16をサポートする拡張がいくつか規定されています。

RISC-Vのbfloat16の対応では、非正規化数が完全にサポートされます。

#### Zfbfmin

Zfbfmin拡張はbfloat16に対する最低限のサポート、つまりbinary32との変換と、ロード・ストアを提供します。

* FCVT.BF16.S: Convert FP32 to BF16
* FCVT.S.BF16: Convert BF16 to FP32
* FLH
* FSH
* FMV.H.X
* FMV.X.H

ロード・ストアの命令はbinary16に対するもの（Zfhmin拡張で提供されるもの）と共通です。

#### Zvfbfmin

Zvfbfmin拡張は、bfloat16のベクトルに対する最低限のサポート、つまりbinary32との変換を提供します。

* VFNCVTBF16.F.F.W: Vector convert FP32 to BF16
* VFWCVTBF16.F.F.V: Vector convert BF16 to FP32

#### Zvfbfwma

Zvfbfwma拡張は、bfloat16同士を掛けてbinary32に加える、つまりbfloat16×bfloat16+binary32→binary32を行う命令を追加します。

* VFWMACCBF16: Vector BF16 widening multiply-accumulate

## ArmのFEAT_BF16を試す

AppleのMシリーズでは、M2以降でFEAT_BF16に対応しているようです（参考：[Apple Silicon M2はM1シリーズと比べて命令セットが拡張されている](https://qiita.com/zacky1972/items/5deec6b139a76246aeee)）。一方、Apple M4の時点でもFEAT_EBF16には対応していません。

```
$ # Apple M4での実行結果
$ sysctl hw | grep BF16
hw.optional.arm.FEAT_BF16: 1
hw.optional.arm.FEAT_EBF16: 0
```

FEAT_BF16を試すコードは「[浮動小数点数オタクがM1 Macを触ってみた](https://qiita.com/mod_poppo/items/fb18f2a1441e74af29a3#bfloat16-%E9%9D%9E%E5%AF%BE%E5%BF%9C)」にも書きました。ここでは同じコードをApple M4で実行してみます。

```c
#include <stdio.h>
#include <arm_acle.h>
#include <arm_neon.h>

int main(void)
{
#if defined(__ARM_FEATURE_BF16)
    puts("__ARM_FEATURE_BF16 is defined");
#else
    puts("__ARM_FEATURE_BF16 is not defined");
#endif
#if defined(__ARM_BF16_FORMAT_ALTERNATIVE)
    puts("__ARM_BF16_FORMAT_ALTERNATIVE is defined");
#else
    puts("__ARM_BF16_FORMAT_ALTERNATIVE is not defined");
#endif
#if defined(__ARM_BF16_FORMAT_ALTERNATIVE)
    float32_t x = 3.14f;
    bfloat16_t y = vcvth_bf16_f32(x);
    float32_t z = vcvtah_f32_bf16(y);
    printf("%a -> %a\n", x, z);
#endif
}
```

```
$ clang -march=armv8.6-a bf16.c
$ ./a.out
__ARM_FEATURE_BF16 is defined
__ARM_BF16_FORMAT_ALTERNATIVE is defined
0x1.91eb86p+1 -> 0x1.92p+1
```

奇数丸めの例も載せておきます。

```c
#include <stdio.h>
#include <arm_acle.h>
#include <arm_neon.h>

int main(void)
{
    float a = vcvtxd_f32_f64(0x1.70000001p10);
    float b = vcvtxd_f32_f64(0x1.70000001p300);
    printf("%a, %a\n", a, b);
}
```

```
$ clang cvtx.c
$ ./a.out
0x1.700002p+10, 0x1.fffffep+127
```

Armの奇数丸め命令は、絶対値が大きくてbinary32で表現できない有限値を、「binary32の無限大」ではなく「binary32の有限の最大値」に変換するようです。マジで？って感じですが、この後bfloat16に変換する時に無限大になるから気にしない、ってことですかね。
