---
title: "C言語で複素数値を構築する：CMPLXマクロの話"
emoji: "🍣"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

この記事は [C言語 Advent Calendar 2023](https://qiita.com/advent-calendar/2023/c_prog_lang) の 2 日目の記事です。

---

C 標準には C99 で複素数型 `_Complex` が導入されました。実部が `x`, 虚部が `y` の複素数の値を構築するには、 `x + y * I` と書きます。

一方で、C99 の次の改定 C11 では複素数を構築する別の方法として `CMPLX` マクロが導入されました。この記事では、なぜ C99 の方法に問題があって `CMPLX` マクロが導入されたのかを説明します。また、コンパイラー間の違いによって起こる問題も説明します。

## 虚数単位

`I` は `<complex.h>` で定義されるマクロです。`I` は `_Complex_I` または `_Imaginary_I` に展開されます。気に食わないユーザーは `#undef I` することが特例で認められています。

`_Complex_I` は `const float _Complex` 型の定数式に展開されます。値はもちろん虚数単位です。実部のゼロの符号は規定されていなさそうです。

`_Imaginary_I` は実装が純虚数型 `_Imaginary` をサポートしている場合に定義され、 `const float _Imaginary` 型の定数式に展開されます。値はもちろん虚数単位です。

本題とは関係ありませんが、`_Complex_I` と `_Imaginary_I` の型の `const` は C23 では外れます：

* N2726: [_Imaginary_I and _Complex_I Qualifiers | r0](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2726.htm)

## 複素数の乗算と加算を使うことによる問題

処理系が純虚数型をサポートしていて `I` の定義が `_Imaginary_I` の場合は何も問題はありません。`x + y * I` は期待通りに動きます。たとえ `y` が NaN や無限大であっても。

処理系が純虚数型をサポートしておらず、`I` の定義が `_Complex_I` の場合に `x + y * I` がどうなるか考えてみましょう。

まず、 `x` がゼロではなく、`y` が有限の浮動小数点数の場合は問題なさそうです。

`x` がゼロの時はどうなるでしょうか。浮動小数点数のゼロには符号があります。`x = -0.0` の場合でも、`y * I` という複素数の実部が正のゼロであれば、結果の実部は正のゼロとなってしまいます（通常の丸め方法の場合）。C 言語における一部の複素関数は枝 (branch) の選択にゼロの符号を考慮するので、これは看過できない問題です。詳しくは私が昔書いた記事

* [逆双曲線関数と逆三角関数の branch cut | 雑記帳](https://blog.miz-ar.info/2016/08/branch-cuts-of-inv-hyp-functions-and-inv-trig-functions/)

を参照してください。

`y` が無限大の場合はどうなるでしょうか。複素数の実部と虚部をペアで書くと `I` は `(0.0, 1.0)` なので、`y * I` は `(inf * 0.0, inf * 1.0) = (NaN, inf)` となります。したがって、`x + y * I` は `(NaN, inf)` となります。実部が `x` に関わらず NaN となってしまいました。

`y` が NaN の場合も考えてみます。`y * I` は `(NaN * 0.0, NaN * 1.0) = (NaN, NaN)` となります。したがって、`x + y * I` は `(NaN, NaN)` となります。これも、実部が `x` に関わらず NaN となってしまいました。

`x + y * I` の実部は `y` に関わらず `x` になってほしいところですが、そうはならない場合があることがわかりました。

## `CMPLX` マクロによる解決

この問題を解決するために、C11 では `CMPLX`, `CMPLXF`, `CMPLXL` というマクロが導入されました。型を関数っぽく書くとこうなります：

```c
#include <complex.h>
double complex CMPLX(double x, double y);
float complex CMPLXF(float x, float y);
long double complex CMPLXL(long double x, long double y);
```

これらのマクロによって、実部が負のゼロでも、虚部が無限大や NaN であっても、実部を指定して複素数を構築することができます。

関数ではなくマクロとなっているのは、static 変数の初期化に使えるようにするためのようです。つまり、compiler magic によって実装されることが期待されます。

* N1464: [Creation of complex value](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1464.htm)

`x + y * _Complex_I` と `CMPLX` の違いを感じられるコードを提示しておきます：

```c
#include <complex.h>
#include <stdio.h>

int main(void) {
    double complex x = -0.0 + 2.0 * _Complex_I;
    double complex y = CMPLX(-0.0, 2.0);
    double complex z = casinh(x);
    double complex w = casinh(y);
    printf("asinh(%g + %gi) = %g + %gi\n", creal(x), cimag(x), creal(z), cimag(z));
    printf("asinh(%g + %gi) = %g + %gi\n", creal(y), cimag(y), creal(w), cimag(w));
}
```

実行結果：

```
asinh(0 + 2i) = 1.31696 + 1.5708i
asinh(-0 + 2i) = -1.31696 + 1.5708i
```

## 実装：GCC vs Clang

`CMPLX` マクロの実装には、何らかの compiler magic が必要です。GCC 4.7（2012 年リリース）以降はそのために `__builtin_complex` という組み込み関数を提供しています。

* [Complex (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Complex.html)

これを使った `CMPLX` マクロの定義は次のようになるでしょう：

```c
#define CMPLX(x,y) __builtin_complex((double)(x), (double)y)
#define CMPLXF(x,y) __builtin_complex((float)(x), (float)y)
#define CMPLXL(x,y) __builtin_complex((long double)(x), (long double)y)
```

一方、Clang は初期化子を使って複素数を初期化できる拡張を実装しました：

* [Clang Language Extensions — Clang documentation](https://clang.llvm.org/docs/LanguageExtensions.html#initializer-lists-for-complex-numbers-in-c)

これを使った `CMPLX` マクロの定義は次のようになるでしょう：

```c
#define CMPLX(x,y) (double _Complex){x, y}
#define CMPLXF(x,y) (float _Complex){x, y}
#define CMPLXL(x,y) (long double _Complex){x, y}
```

Clang 12（2021 年リリース）以降には GCC 互換の `__builtin_complex` も提供されていますが、昔はそうではありませんでした。そうすると何が困るでしょうか？

Unix 系では、C 言語の標準ライブラリー (libc) は、処理系と別に提供されることが多いです。`complex.h` もコンパイラーとは別に開発されて、システムに付属するでしょう。もしも libc の開発者が GCC しか考えていなかったらどうなるでしょうか？あるいは Clang しか考えていなかったら？

これは「もしも」ではなく、現実に起こっている問題です。GNU の glibc は GCC を想定しているため、`CMPLX` マクロは Clang 向けには提供されません。Clang が `__builtin_complex` を提供していても、`CMPLX` マクロを定義する条件が「GCC 4.7 以上」となっているので、そこを変えないとダメそうです。

逆に、macOS の libc は Clang を想定しているため、`CMPLX` マクロは GCC 向けには提供されません。

もちろん、musl や FreeBSD の libc のように、両方に対応している libc もあります。

利用者側でできる対処は、`<complex.h>` を `#include` して `CMPLX` マクロが定義されていなかったら独自に定義する、という風になるでしょう。つまり、こういうことです：

```c
#include <complex.h>

#if !defined(CMPLX)
# if defined(__clang__)
#  define CMPLX(x,y) (double _Complex){x, y}
#  define CMPLXF(x,y) (float _Complex){x, y}
#  define CMPLXL(x,y) (long double _Complex){x, y}
# elif defined(__GCC__)
#  define CMPLX(x,y) __builtin_complex((double)(x), (double)y)
#  define CMPLXF(x,y) __builtin_complex((float)(x), (float)y)
#  define CMPLXL(x,y) __builtin_complex((long double)(x), (long double)y)
# endif
#endif
```

C 言語は……つらい！（とはいえ、`CMPLX` マクロは利用者側で容易に対処できるのでまだマシかもしれませんね）

---

書き終わってから改めてネットの海を検索したら、昔の自分のブログがヒットして、この件について簡単ですが触れていました。Clang 12 が `__builtin_complex` に対応したのが当時に対する更新点ですかね。

* [浮動小数点数による複素数の演算に関する注意点](https://blog.miz-ar.info/2016/07/complex-arithmetic-with-floating-points/)（2016年7月）
