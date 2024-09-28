---
title: "浮動小数点演算の結果が環境依存なのはどんなときか"
emoji: "✨"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c", "c言語", "浮動小数点数"]
published: true
---

浮動小数点数の演算結果は環境によって変わる場合があります。どういう場合に変わることがあり、それはどういう原因によるのでしょうか。この記事では、演算結果が変わる原因として考えられるものをいくつか紹介します。

対象とする言語はC言語とします。基本的にC17の前提で書きますが、一部にC23への言及を含みます。

C言語では浮動小数点演算の精度は処理系定義です（C17 5.2.4.2.2.7）。C言語的に浮動小数点演算についてまともなことが言えるのは処理系がAnnex Fに準拠している場合です。C17のAnnex FはIEEE 754-1985を参照し、C23のAnnex FではIEEE 754-2019を参照します。

ただ、この記事では「どういう場合に変わるか」を考え、「どういう場合に結果が変わらないことが保証されるか」はあまり考えないので、Annex F準拠であるかはあまり重要ではないかもしれません。

# fast math

`-ffast-math` 系のオプションが有効だとコンパイラーが（計算結果を変えうる）アグレッシブな最適化をするので、環境によって結果が変わる要因になります。

以下では、基本的に `-ffast-math` を使わない前提で話を進めます。

# データ型とNaN

データ型が表す浮動小数点形式が違えば、表現できる数の集合も変わってくるので、当然演算結果も変わります。

大抵の環境では

* `float` 型はIEEE binary32
* `double` 型はIEEE binary64

です。マイコン等の組み込み向けだと `double` 型もbinary32の場合があるらしいです。

`long double` 型の精度は環境依存が激しいのでポータブルな結果を得たい場合は避けましょう。この辺は前に書きました：

* [long doubleの話](https://qiita.com/mod_poppo/items/8860505f38e2997cd021)

数ではないもの、NaNのビットパターンは環境に依存すると思った方が良いです。最近書いた記事です：

* [NaNのビットパターンを使ってWebAssemblyからCPUの命令セットを推測する](https://zenn.dev/mod_poppo/articles/detect-isa-via-nan)

# 四則演算

`+`、`-`、`*`、`/` などのプリミティブな演算はどうでしょうか。

まず、x87 FPUを使う環境（32ビットx86）では、これらの演算結果が他と変わる場合があります。事情については

* [x87 FPUの呪い](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)

を参照してください。SSE2を使えばこの呪いから解放され、他の環境と同一の結果を得ることができます。x86_64であれば自動でSSE2が使われます。32ビットx86向けのGCCであれば `-mfpmath=sse -msse2` を指定します。

環境によっては、`a * b + c` の形の式がFMAを使うようにコンパイルされる場合があります。C言語的には `#pragma STDC FP_CONTRACT OFF` でこれを抑制できることになっていますが、実際のコンパイラーがこれに対応しているかは確認する必要があります。関連記事：

* [FMA (fused multiply-add) の話](https://qiita.com/mod_poppo/items/e6577df362f44a3ef8dd)

FMAの使用可否によって結果が変わる例を見てみましょう。以下のコードをGCCでコンパイルします：

```c
#include <stdio.h>

__attribute__((noinline))
double eval_poly(size_t n, const double poly[n], double x)
{
    double y = 0.0;
    for (size_t i = n; i > 0; --i) {
        double coeff = poly[i - 1];
        y = x * y + coeff;
    }
    return y;
}

int main(void)
{
    double a[] = {1.1, -2.2};
    double r = eval_poly(2, a, 0.1);
    printf("%a\n", r);
}
```

x86のデフォルトではFMAが使われないので、結果は以下のようになります:

```
$ uname -m
x86_64
$ gcc -O2 poly.c && ./a.out
0x1.c28f5c28f5c2ap-1
```

AArch64には最初からFMAがあるので、GCCはFMAを使い、以下の結果となります:

```
$ uname -m
aarch64
$ gcc -O2 poly.c && ./a.out
0x1.c28f5c28f5c29p-1
```

現状のClangはGCCほどアグレッシブにFMAを使わないので、Clangでは環境による差は出ないかもしれません。

あとは、flush to zeroが有効な環境では非正規化数が0になってしまうので、演算結果が他と変わります。flush to zeroが何かというのは「Binary Hacks Rebooted」の浮動小数点例外のところに書きました。Intelのコンパイラーだとflush to zeroが自動で有効になるようなので注意してください。他のコンパイラーでも `-ffast-math` 系のオプションを使うとflush to zeroが有効になったりします。

# 型キャスト

浮動小数点数から固定長整数へのキャストの場合、入力が範囲外やNaNだったりすると環境依存の結果（6.3.1.4ではundefined behavior、Annex F.4.1ではunspecified）が返ってきます。これも「Binary Hacks Rebooted」に書きました（浮動小数点数の環境依存性みたいな話）。

それ以外の場合（有限の値で、結果を変換後の型で表現できる場合）は、コンパイラーがバグっていない限りポータブルな結果が得られるはずです。

# 数学関数

GCCやClangなどのコンパイラーはかなりクオリティーが高いと思われるので、上記の事項に注意すればコンパイラーが提供する演算、つまり四則演算とキャストについてはポータブルな結果を得やすいと思います。

一方で、`sqrt` などの数学関数はどうでしょうか。これらはシステムのlibcが提供するものであり、個々のlibcのクオリティーは必ずしもコンパイラーほど高くはありません。また、`sin` や `exp` などの複雑な演算はC標準のレベルでは精度が保証されていないので、一般にポータブルではないと思った方が良いです。

C17のAnnex Fでは以下の関数がIEEE 754-1985との関係が明示され、これらは（NaNの符号ビットを除いて）ポータブルであることがそれなりに期待できます：

* `sqrt`, `remainder`
* `rint`, `lrint`, `llrint`
* `strtod`, `strtof`, `strtold`, `fprintf`, `fscanf` など
* `isgreater`, `isgreaterequal`, `isless`, `islessequal`, `islessgreater`, `isunordered`
* `copysign`, `fabs`, `scalbn`, `scalbln`, `logb`, `nextafter`, `nexttoward`
* `isfinite`, `isnan`, `signbit`, `fpclassify`

C23ではさらに以下の演算と関数もIEEE 754-2019との関係が明示されます：

* `roundeven`, `round`, `trunc`, `ceil`, `floor`, `rint`
* `nextup`, `nextdown`
* `getpayload`, `setpayload`, `setpayloadsig`
* `quantize`, `samequantum`, `quantum`
* `encodedec`, `decodedec`, `encodebin`, `decodebin`
* `remainder`, `remquo`
* `fmaximum`, `fminimum`, `fmaximum_mag`, `fminimum_mag`
* `fmaximum_num`, `fminimum_num`, `fmaximum_mag_num`, `fminimum_mag_num`
* `scalbn`, `scalbln`
* `logb`, `ilogb`, `llogb`
* `+`, `fadd`, `faddl`, `daddl`
* `-`, `fsub`, `fsubl`, `dsubl`
* `*`, `fmul`, `fmull`, `dmull`
* `/`, `fdiv`, `fdivl`, `ddivl`
* `sqrt`, `fsqrt`, `fsqrtl`, `dsqrtl`
* `fma`, `ffma`, `ffmal`, `dfmal`
* キャスト
* `fromfp`, `ufromfp`, `lround`, `llround`, `fromfpx`, `ufromfpx`
* `canonicalize`
* `strtod`, `wcstod`, `scanf`, `wscanf`
* `printf`, `wprintf`, `strfromd`
* 単項 `-`, `fabs`, `copysign`
* `iseqsig`, `isgreater`, `isgreaterequal`, `isless`, `islessequal`, `isunordered`
* `fpclassify`, `signbit`, `issignaling`
* `isnormal`, `isfinite`, `iszero`, `issubnormal`, `isinf`, `isnan`, `issignaling`, `iscanonical`
* `totalorder`, `totalordermag`, 
* `fenv.h` の連中

ただ、NaNのビットパターンに触れるもの（符号ビット、ペイロード）や十進のcanonical性に関するものは実装依存が認められています。十進の「末尾の0の個数」とmin/max系の演算の関係も実装依存です。

また、libcの実装によっては「`sqrt` に `fesetround` が効かない」というような状況があるようです。筆者が耳にしたことがある（実際に筆者自身が試したわけではない）例では、Cygwinで使われているnewlibがあります。

`fma` も実装によってはバグっているというのはFMAの記事に書いた通りです。

十進小数から二進浮動小数点数への変換は、桁数が多すぎると危ないです。

上のリストに載っていないやつは、C標準では精度が保証されないということなので、環境によって結果が変わる可能性が大きいです。例を挙げます：

- `acos`
- `asin`
- `atan`
- `atan2`
- `cos`
- `sin`
- `tan`
- `acosh`
- `asinh`
- `atanh`
- `cosh`
- `sinh`
- `tanh`
- `exp`
- `exp2`
- `expm1`
- `log`
- `log10`
- `log1p`
- `log2`
- `logb`
- `cbrt`
- `hypot`
- `pow`
- `erf`
- `erfc`
- `lgamma`
- `tgamma`
- `nan`
- `fmax`
- `fmin`

これらの関数についてポータビリティーが必要なら、自前で実装を持っておくのが良いでしょう。

libcによる `sin` の値の違いを前に調べた時のブツが <https://github.com/minoki/math-func-test> にあります。興味のある方は見てみてください。

GCCの数学関数の定数畳み込みではMPFRという高精度なライブラリーを使うため、コンパイル時計算と実行時計算で値が変わる可能性があります。例を載せます：

```c
#include <math.h>
#include <stdio.h>

__attribute__((noinline))
void foo(double x)
{
    printf("[runtime] sin(%g) = %a\n", x, sin(x));
}

int main(void)
{
    double x = 0x1p25;
    printf("[compile-time] sin(%g) = %a\n", x, sin(x));
    foo(x);
}
```

このコードをUbuntu 22.04で実行した結果が以下になります：

```
$ gcc -O0 test.c -lm && ./a.out
[compile-time] sin(3.35544e+07) = -0x1.f3fa130939bbp-1
[runtime] sin(3.35544e+07) = -0x1.f3fa130939bbp-1
$ gcc -O2 test.c -lm && ./a.out
[compile-time] sin(3.35544e+07) = -0x1.f3fa130939bafp-1
[runtime] sin(3.35544e+07) = -0x1.f3fa130939bbp-1
$ /lib/ld-linux.so.2 --version
ld.so (Ubuntu GLIBC 2.35-0ubuntu3.8) stable release version 2.35.
Copyright (C) 2022 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.
```

最適化が有効で、`sin` に定数を与えた場合のみ、`sin` の結果が変わっているのがわかります。

# 命令セット的な話

今度はコンパイル後の話をします。特定のコンパイラーを使って特定のソースコードをコンパイルしてできたバイナリーは、どのCPUでも同じ結果を返すでしょうか？

## 同じバイナリーを実行した時の結果は同じか？

まず、「同じバイナリーを実行した時の結果は同じか？」という問題ですが、これは一般には否です。

例えば、CPUのサポートする拡張命令によって実行する命令列を切り替える関数があれば、そこで結果が変わる可能性があります。あるいは、CPUのコア数によって並列数が変わり、浮動小数点数の足し算の結合が変わって異なる結果が得られた、というのもいかにもありそうな話です。

## 同じ命令列を実行した時の結果は同じか？

次に、拡張命令やコア数に依存しないように注意深く書いたプログラムは異なるCPUでも同じ結果を返すでしょうか？これも一般には否です。

x86の命令には、数学関数などの近似値を返す命令があります。そういう命令を使ってしまうと、結果に影響が出る場合があります。

近似命令の例はx87の `FSIN` 命令です。試してみましょう：

```c
#include <stdio.h>

int main(void)
{
    double x = 23863.0;
    double y;
    asm volatile("fsin" : "=t"(y) : "0"(x));
    printf("fsin(%g) = %a\n", x, y);
}
```

AMD Ryzen 9 7940HSでの実行結果：

```
$ gcc fsin-23863.c && ./a.out
fsin(23863) = -0x1.0644f79cfc2f5p-1
```

MacBook Pro (2020, Intel Core i5) での実行結果：

```
$ clang fsin-23863.c && ./a.out
fsin(23863) = -0x1.0644f79cfc2f6p-1
```

同じ命令、同じ入力値であってもマシンによって結果が異なることが確かめられました（本当はOSとバイナリーも揃えるべきですが）。

# まとめ

浮動小数点演算の結果が変わる要因がいろいろあることがわかったと思います。

最初にも書きましたが、この記事に書いたことに気をつけたからと言ってポータブルになることを保証するものではありません。ゲーム等で再現性が必要な人は頑張ってください。
