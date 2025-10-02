---
title: "浮動小数点数に1を掛ける操作は最適化できるか／浮動小数点数のビット列表現のカノニカル性について"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["浮動小数点数"]
published: true
---

浮動小数点数に1を掛ける関数を考えます：

```c
void multiply_one(double *x)
{
    static const volatile double one = 1.0;
    *x *= one;
}
```

数学的には、この関数は入力をそのまま返す関数（恒等関数）です。浮動小数点数の場合はどうでしょうか？特に、この関数は、入力として与えられた浮動小数点数のビット列表現を変えるでしょうか？

多くの人は「変えないでしょ」「変わったとしても違いは重要じゃない」と思われるかもしれませんが、これはコンパイラーの最適化を考える上では重要な問題になります。もちろん、「浮動小数点数の細かいことは気にしない」モードなら最適化して良いでしょうが、ここでは浮動小数点数の重箱の隅まで気にするコンパイラーを考えます。

簡単なプログラムを作って、いくつかの値について試してみます：

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

void multiply_one(double *x)
{
    static const volatile double one = 1.0;
    *x *= one;
}

void make_f64(double *x, uint64_t data)
{
    memcpy(x, &data, 8);
}

void print_f64(const char *s, double *x)
{
    uint64_t data;
    memcpy(&data, x, 8);
    printf("%s: %g, 0x%016" PRIx64 "\n", s, *x, data);
}

int main(void)
{
    double x = 3.14;
    print_f64("3.14 (before)", &x);
    multiply_one(&x);
    print_f64("3.14 (after)", &x);
    x = -0.0;
    print_f64("-0.0 (before)", &x);
    multiply_one(&x);
    print_f64("-0.0 (after)", &x);
    make_f64(&x, 0x7ff8000000c0ffee);
    print_f64("NaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f64("NaN (c0ffee) (after)", &x);
}
```

```console
$ uname -m
x86_64
$ cc test.c
$ ./a.out
3.14 (before): 3.14, 0x40091eb851eb851f
3.14 (after): 3.14, 0x40091eb851eb851f
-0.0 (before): -0, 0x8000000000000000
-0.0 (after): -0, 0x8000000000000000
NaN (c0ffee) (before): nan, 0x7ff8000000c0ffee
NaN (c0ffee) (after): nan, 0x7ff8000000c0ffee
```

x86_64のマシンで実行したところ、これら3つの例についてはビット列表現が変わっていなさそうだということがわかります。

この記事では、「1をかける関数」について、

* 他のデータ型についてはどうか？
* もっと特殊な浮動小数点データについてはどうか？
* 浮動小数点演算器の設定を変えるとどうか？
* 他のアーキテクチャではどうか？

ということを探っていきます。そして、関連する概念である「浮動小数点数のビット列表現のカノニカル性」について触れます。

## 十進浮動小数点数の場合

IEEE 754では、十進浮動小数点数に対するビット列表現を定めています。「[IEEE 754の十進浮動小数点数の基本](ieee754-decimal)」という記事では十進浮動小数点数の概要に触れているので確認してください。

GCCを使って、十進浮動小数点数でも先ほどと同じようなことをしてみましょう。

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

void multiply_one(_Decimal32 *x)
{
    static const volatile _Decimal32 one = 1.0df;
    *x *= one;
}

void make_d32(_Decimal32 *x, uint32_t data)
{
    memcpy(x, &data, 4);
}

void print_d32(const char *s, _Decimal32 *x)
{
    uint32_t data;
    memcpy(&data, x, 4);
    printf("%s: %g, 0x%08" PRIx32 "\n", s, (double)*x, data);
}

int main(void)
{
    _Decimal32 x = 7.df;
    print_d32("7 (before)", &x);
    multiply_one(&x);
    print_d32("7 (after)", &x);
    x = -0.0df;
    print_d32("-0.0 (before)", &x);
    multiply_one(&x);
    print_d32("-0.0 (after)", &x);
    make_d32(&x, 0x62ffffff); // 0b0'110 0010 1'111 ...
    print_d32("0x62ffffff (before)", &x);
    multiply_one(&x);
    print_d32("0x62ffffff (after)", &x);
    make_d32(&x, 0x7c0ffee0);
    print_d32("NaN (0x7c0ffee0) (before)", &x);
    multiply_one(&x);
    print_d32("NaN (0x7c0ffee0) (after)", &x);
}
```

x86_64 Linux GCC 13.3での実行結果：

```console
$ gcc decimal32.c
$ ./a.out
7 (before): 7, 0x32800007
7 (after): 7, 0x32000046
-0.0 (before): -0, 0xb2000000
-0.0 (after): -0, 0xb1800000
0x62ffffff (before): 0, 0x62ffffff
0x62ffffff (after): 0, 0x0b000000
NaN (0x7c0ffee0) (before): nan, 0x7c0ffee0
NaN (0x7c0ffee0) (after): nan, 0x7c000000
```

なんと、1.0を掛けただけなのに、ビット列表現が変わりました！

7と-0.0のビット列表現が変わったのは、1.0という「末尾に0が付いた数」を掛けたのが原因で、仮数部の0が増える代わりに指数部が調整されたんですね。同じcohortに属する別の表現に変わったのです。

というわけで、IEEE 754の十進浮動小数点数では「1.0を掛ける操作」と「1を掛ける操作」は区別する必要があります。

`1.0df` の代わりに `1.df` を使うと、7と-0.0のビット列表現は変わらなくなります：

```c
// ...

void multiply_one(_Decimal32 *x)
{
    static const volatile _Decimal32 one = 1.df;
    *x *= one;
}

// ...
```

```console
$ gcc decimal32-2.c
$ ./a.out
7 (before): 7, 0x32800007
7 (after): 7, 0x32800007
-0.0 (before): -0, 0xb2000000
-0.0 (after): -0, 0xb2000000
0x62ffffff (before): 0, 0x62ffffff
0x62ffffff (after): 0, 0x0b800000
NaN (0x7c0ffee0) (before): nan, 0x7c0ffee0
NaN (0x7c0ffee0) (after): nan, 0x7c000000
```

`1.df` という浮動小数点数は、浮動小数点データの表現に対しては「掛けても何も起こらない数」として振る舞うはずで、実際に7や-0.0に対してはビット列表現が変わっていませんが、0x62ffffffや0x7c0ffee0というビット列表現で表されるものに対してはビット列表現を変化させています。

0x62ffffffというビット列表現は、`double` にキャストした結果を見ると値としては0になるようです。また、0x7c0ffee0というビット列表現は、同様にキャストした結果を見るとNaNを表すようです。これらのビット列表現は、0やNaNに対する**非カノニカルな表現**であり、浮動小数点演算を適用することによって（たとえそれが浮動小数点データを変えないものだとしても）、**カノニカルなビット列表現に変換（正則化）された**ということになります。

これがIEEE 754で定められた「浮動小数点データのビット列表現のカノニカル性」です。decimal32形式のビット列表現は $2^{32}$ 個あるのに対し、データの表現（通常の数に対しては符号、指数部、仮数部の組、NaNに対しては符号とペイロード）の個数はそれよりも小さいため、エンコード・デコードの際に重複が発生するのです。IEEE 754では、複数ありうるビット列表現のうち一つを「カノニカルなエンコーディング」と定め、浮動小数点演算は原則としてカノニカルなビット列表現を返すこと、としています（符号ビットだけを操作する系の演算を除く）。

そういうわけで、「1を掛ける」という数値的には何もしない浮動小数点演算であっても、非カノニカルなビット列表現をカノニカルなものに変換するという挙動を示します。

コンパイラーの観点からすると、「何もしない浮動小数点演算」だからと言って最適化で消してしまうと、ビット列表現を観測した時の挙動を変えてしまう可能性がある、ということになります。

## signaling NaN

signaling NaNについては「[Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)」の「Hack #74 NaNを深掘りする」に書いたのでそちらを参照してください。

signaling NaNは浮動小数点演算に与えると浮動小数点例外を引き起こし、（デフォルトでは）quiet NaNを返す、というのが原則です（符号ビットだけを操作する系の演算を除く）。なので、signaling NaNに1を掛けるとビット列表現が変わります。

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

void multiply_one(double *x)
{
    static const volatile double one = 1.0;
    *x *= one;
}

void make_f64(double *x, uint64_t data)
{
    memcpy(x, &data, 8);
}

void print_f64(const char *s, double *x)
{
    uint64_t data;
    memcpy(&data, x, 8);
    printf("%s: %g, 0x%016" PRIx64 "\n", s, *x, data);
}

int main(void)
{
    double x = 3.14;
    print_f64("3.14 (before)", &x);
    multiply_one(&x);
    print_f64("3.14 (after)", &x);
    x = -0.0;
    print_f64("-0.0 (before)", &x);
    multiply_one(&x);
    print_f64("-0.0 (after)", &x);
    make_f64(&x, 0x7ff0000000c0ffee);
    print_f64("sNaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f64("sNaN (c0ffee) (after)", &x);
}
```

```console
$ gcc snan.c
$ ./a.out
3.14 (before): 3.14, 0x40091eb851eb851f
3.14 (after): 3.14, 0x40091eb851eb851f
-0.0 (before): -0, 0x8000000000000000
-0.0 (after): -0, 0x8000000000000000
sNaN (c0ffee) (before): nan, 0x7ff0000000c0ffee
sNaN (c0ffee) (after): nan, 0x7ff8000000c0ffee
```

signaling NaNのビット列表現（0x7ff0...）がquiet NaN（0x7ff8...）に変わったのがわかります。

## x87 FPU 80ビットの拡張倍精度

x87 FPUの内部的な浮動小数点形式は80ビットあり、一部の処理系ではそれを `long double` 型として利用できます。x87 FPUについては「[x87 FPUの呪い](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)」「[long doubleの話](https://qiita.com/mod_poppo/items/8860505f38e2997cd021)」も見てください。

この形式のビット列表現はIEEE 754では定められておらず、いわゆる「ケチ表現」を採用するIEEE 754の交換形式とは異なり、仮数部の先頭の1をビット列表現に格納します。

では、仮数部の先頭が1じゃなかったらx87 FPUはその値をどう扱うのでしょうか？試してみましょう。

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

void multiply_one(long double *x)
{
    static const volatile long double one = 1.0L;
    *x *= one;
}

void make_f80(long double *x, uint16_t e, uint64_t m)
{
    char buf[10];
    memcpy(buf, &m, 8);
    memcpy(buf + 8, &e, 2);
    memcpy(x, buf, 10);
}

void print_f80(const char *s, long double *x)
{
    uint16_t e;
    uint64_t m;
    char buf[10];
    memcpy(buf, x, 10);
    memcpy(&m, buf, 8);
    memcpy(&e, buf + 8, 2);
    printf("%s: %Lg, 0x%04" PRIx16 "%016" PRIx64 "\n", s, *x, e, m);
}

int main(void)
{
    long double x = 3.14L;
    print_f80("3.14 (before)", &x);
    multiply_one(&x);
    print_f80("3.14 (after)", &x);
    x = -0.0L;
    print_f80("-0.0 (before)", &x);
    multiply_one(&x);
    print_f80("-0.0 (after)", &x);
    x = 0x1p-16400L;
    print_f80("0x1p-16400 (before)", &x);
    multiply_one(&x);
    print_f80("0x1p-16400 (after)", &x);
    x = 1.0L / 0.0L;
    print_f80("infinity (before)", &x);
    multiply_one(&x);
    print_f80("infinity (after)", &x);
    make_f80(&x, 0x7fff, 0xc000000000c0ffee);
    print_f80("qNaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f80("qNaN (c0ffee) (after)", &x);
    make_f80(&x, 0x7fff, 0x8000000000c0ffee);
    print_f80("sNaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f80("sNaN (c0ffee) (after)", &x);
    //
    // ここからが本番
    //
    make_f80(&x, 0x7fff, 0x0000000000c0ffee);
    print_f80("pseudo sNaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f80("pseudo sNaN (c0ffee) (after)", &x);
    make_f80(&x, 0x7fff, 0x4000000000c0ffee);
    print_f80("pseudo qNaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f80("pseudo qNaN (c0ffee) (after)", &x);
    make_f80(&x, 0x7fff, 0x0000000000000000);
    print_f80("pseudo infinity (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f80("pseudo infinity (c0ffee) (after)", &x);
    make_f80(&x, 0x3fff, 0x0000000000000003);
    print_f80("unnormal (before)", &x);
    multiply_one(&x);
    print_f80("unnormal (after)", &x);
    make_f80(&x, 0x0000, 0x9000000000000003);
    print_f80("pseudo denormal (before)", &x);
    multiply_one(&x);
    print_f80("pseudo denormal (after)", &x);
}
```

```console
$ gcc x87.c 
$ ./a.out
3.14 (before): 3.14, 0x4000c8f5c28f5c28f5c3
3.14 (after): 3.14, 0x4000c8f5c28f5c28f5c3
-0.0 (before): -0, 0x80000000000000000000
-0.0 (after): -0, 0x80000000000000000000
0x1p-16400 (before): 1.28254e-4937, 0x00000000200000000000
0x1p-16400 (after): 1.28254e-4937, 0x00000000200000000000
infinity (before): inf, 0x7fff8000000000000000
infinity (after): inf, 0x7fff8000000000000000
qNaN (c0ffee) (before): nan, 0x7fffc000000000c0ffee
qNaN (c0ffee) (after): nan, 0x7fffc000000000c0ffee
sNaN (c0ffee) (before): nan, 0x7fff8000000000c0ffee
sNaN (c0ffee) (after): nan, 0x7fffc000000000c0ffee
pseudo sNaN (c0ffee) (before): nan, 0x7fff0000000000c0ffee
pseudo sNaN (c0ffee) (after): -nan, 0xffffc000000000000000
pseudo qNaN (c0ffee) (before): nan, 0x7fff4000000000c0ffee
pseudo qNaN (c0ffee) (after): -nan, 0xffffc000000000000000
pseudo infinity (c0ffee) (before): nan, 0x7fff0000000000000000
pseudo infinity (c0ffee) (after): -nan, 0xffffc000000000000000
unnormal (before): nan, 0x3fff0000000000000003
unnormal (after): -nan, 0xffffc000000000000000
pseudo denormal (before): 4.20263e-4933, 0x00009000000000000003
pseudo denormal (after): 3.78237e-4932, 0x00019000000000000003
```

Intel SDMでは、「仮数部の先頭が0であるようなNaN」をpseudo-NaN、「仮数部の先頭が0であるような無限大」をpseudo-infinity、「仮数部の先頭が0であって指数部の範囲が正規化数なもの」をunnormal、「仮数部の先頭が1であって指数部が非正規化数と同じなもの」をpseudo-denomalと呼んでいます。

x87 FPUは、pseudo NaN、pseudo infinity、unnormalに遭遇するとinvalid例外を発生させます。x86では、invalid例外のデフォルト処理では符号ビットが立ったquiet NaNが返ってくるので、そのような実行結果になっています。pseudo denormalは正規化数に変換されます。

とにかく、x87の80ビット拡張倍精度にも、「1を掛けると変化するビット列表現」がたくさんあることがわかりました。

## flush to zeroと非正規化数

x86やArm、それからGPUなど、一部の環境は非正規化数を0扱いするモードを備えています。ここではflush to zeroと呼びます。「Binary Hacks Rebooted」の「Hack #71 浮動小数点例外」で説明しています。

flush to zeroが有効な状態で、非正規化数に1を掛けてみましょう。

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#if defined(__SSE2__)
#include <immintrin.h>
#elif defined(__aarch64__)
#include <arm_acle.h>
#endif

void multiply_one(double *x)
{
    static const volatile double one = 1.0;
    *x *= one;
}

void print_f64(const char *s, double *x)
{
    uint64_t data;
    memcpy(&data, x, 8);
    printf("%s: %g, 0x%016" PRIx64 "\n", s, *x, data);
}

int main(void)
{
    double x = 0x1.cafep-1050;
    print_f64("0x1.cafep-1050 (before)", &x);
#if defined(__SSE2__)
    unsigned int csr = _mm_getcsr();
    _mm_setcsr(csr | (1u << 15)); // Set FTZ (Flush to Zero)
#elif defined(__aarch64__)
    // Set FZ (Flushing denormalized numbers to zero)
    uint64_t fpcr = __arm_rsr64("fpcr");
    __arm_wsr64("fpcr", fpcr | (1u << 24));
#endif
    multiply_one(&x);
    print_f64("0x1.cafep-1050 (after)", &x);
}
```

```
$ cc fz.c
$ ./a.out
0x1.cafep-1050 (before): 1.48617e-316, 0x0000000001cafe00
0x1.cafep-1050 (after): 0, 0x0000000000000000
```

0になりました。

厳密に言うと、x86では「アンダーフローが起こった場合に非正規化数の代わりに0を返す」設定をFlush-To-Zero (FTZ) と呼び、「入力に非正規化数が含まれていたら0扱いする」設定をDenormals-Are-Zero (DAZ) と呼んでいます。いずれにせよこの場合は0が返ります。

というわけで、x86やArmでflush to zeroが有効になっていた場合は、非正規化数も「1を掛けるとビット列表現（どころか、値）が変化するもの」の仲間ということになります。

## RISC-V Canonical NaNおよびArm Default NaN

冒頭のx86の実行例では、NaNのペイロードは1を掛けてもそのままでした。

```
NaN (c0ffee) (before): nan, 0x7ff8000000c0ffee
NaN (c0ffee) (after): nan, 0x7ff8000000c0ffee
```

RISC-Vではこれが変化します。Linux上のQEMUで試してみます：

```console
$ riscv64-linux-gnu-gcc test.c
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./a.out
3.14 (before): 3.14, 0x40091eb851eb851f
3.14 (after): 3.14, 0x40091eb851eb851f
-0.0 (before): -0, 0x8000000000000000
-0.0 (after): -0, 0x8000000000000000
NaN (c0ffee) (before): nan, 0x7ff8000000c0ffee
NaN (c0ffee) (after): nan, 0x7ff8000000000000
```

RISC-Vの浮動小数点数に関する拡張では、符号ビットが0でペイロードも0のNaNが「canonical NaN」と呼び、浮動小数点演算で生成されるNaNはそうなるように定めています。

ArmにもDefault NaNという同様のモードがあります。

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>
#if defined(__aarch64__)
#include <arm_acle.h>
#endif

void multiply_one(double *x)
{
    static const volatile double one = 1.0;
    *x *= one;
}

void make_f64(double *x, uint64_t data)
{
    memcpy(x, &data, 8);
}

void print_f64(const char *s, double *x)
{
    uint64_t data;
    memcpy(&data, x, 8);
    printf("%s: %g, 0x%016" PRIx64 "\n", s, *x, data);
}

int main(void)
{
#if defined(__aarch64__)
    // Set DN (Default NaN)
    uint64_t fpcr = __arm_rsr64("fpcr");
    __arm_wsr64("fpcr", fpcr | (1u << 25));
#endif
    double x;
    make_f64(&x, 0x7ff8000000c0ffee);
    print_f64("NaN (c0ffee) (before)", &x);
    multiply_one(&x);
    print_f64("NaN (c0ffee) (after)", &x);
    make_f64(&x, 0xfff8000000c0ffee);
    print_f64("NaN (negative, c0ffee) (before)", &x);
    multiply_one(&x);
    print_f64("NaN (negative, c0ffee) (after)", &x);
}
```

```
$ clang default-nan.c 
$ ./a.out
NaN (c0ffee) (before): nan, 0x7ff8000000c0ffee
NaN (c0ffee) (after): nan, 0x7ff8000000000000
NaN (negative, c0ffee) (before): nan, 0xfff8000000c0ffee
NaN (negative, c0ffee) (after): nan, 0x7ff8000000000000
```

## カノニカル性

この記事では、以下の場合に「1を掛ける」という操作がビット列表現を変えうることを見ました：

* 形式が十進の場合
* 入力がsignaling NaNの場合
* x87 FPUの80ビット形式で入力が特殊なビットパターンだった場合
* flush to zeroが有効な状態で入力が非正規化数の場合
* RISC-VやArm Default NaNが有効な状態でペイロードを持つNaNを与えた場合

IEEE 754の規格書で言及されている「非カノニカルなエンコーディング」は十進交換形式に関するものだけですが（二進交換形式はすべての数とNaNのエンコーディングがカノニカルだとされている）、他の例も「演算器を通すとビットパターンが変化する」という意味では似たようなものだと言えるかもしれません。RISC-Vの場合は「canonical」という単語まで使っていますしね。

IEEE 754のisCanonical演算と、C23で追加されたマクロ `iscanonical` では、与えられた値がカノニカルかどうか判定できることになっています。十進の場合はいいでしょう。signaling NaNはおそらく「カノニカルである」という判定になりそうです。IEEE 754-2019には

> Implementations should extend isCanonical(x) to formats that are not interchange formats in ways appropriate to those formats, which might, or might not, have finite numbers, infinities, or NaNs that are non-canonical.

という文言があるので、x87 FPUのpseudo系は「カノニカルではない」という判断になるでしょう。flush to zero環境での非正規化数やRISC-VでのNaNはどうでしょうか。私的には微妙なところだと思います。

```c
#include <math.c>
int iscanonical(real-floating x); // macro
```

## コンパイラーの最適化とカノニカル化

浮動小数点型のビット列表現を観測することや、signaling NaNの存在を考えると、コンパイラーは安易に `1 * x` や `x - 0`（丸め方向属性がroundTowardNegative以外の時）を `x` に最適化できないということになります。しかし、非カノニカルなビット列表現やsignaling NaNの存在下でも、条件が整えば最適化はできそうです。

まず、`1 * x` や `x - 0` のように「数値的には変化しない浮動小数点演算」を**カノニカル化**と呼び、canonicalizeという関数で表すことにします。

```
// 最適化の規則
1 * x → canonicalize(x)
x - 0 → canonicalize(x)
```

そして、式の中でcanonicalizeの結果が他の浮動小数点演算への入力として利用される場合は、canonicalizeを消去できます。

```
// 最適化の規則
canonicalize(x) ★ y → x ★ y
x ★ canonicalize(y) → x ★ y
```

canonicalizeを他の浮動小数点演算の結果に適用している場合も、canonicalizeを消去できます。

```
// 最適化の規則
canonicalize(x ★ y) → x ★ y
```

そういう感じで、浮動小数点数を真面目に扱いたいコンパイラーの中間表現ではcanonicalizeという演算を持っておくと良さそうです。

C23には、そのままの名前の `canonicalize` という関数が定義されています。

```c
#include <math.h>
int canonicalize(double *cx, const double *x);
int canonicalizef(float *cx, const float *x);
int canonicalizel(long double *cx, const long double *x);
#ifdef __STDC_IEC_60559_DFP__
int canonicalized32(_Decimal32 *cx, const _Decimal32 *x);
int canonicalized64(_Decimal64 *cx, const _Decimal64 *x);
int canonicalized128(_Decimal128 *cx, const _Decimal128 *x);
#endif
```

この関数の入力は `*x` で与え、カノニカル化したものは `*cx` に格納されます。成功すれば0を、失敗したら非0を返します。この関数は、非カノニカルなビット列表現をカノニカルなものに変えるほか、signaling NaNをquietなものに変換することを明記しています。つまり、「1を掛ける」というような操作で実装されることを意図しているのでしょう。

C23の `canonicalize` は、IEEE 754的には「同じ形式に対してconvertFormatを適用する」操作に相当します。わざわざポインターを介しているのは、環境によっては「引数や返り値で渡すだけで浮動小数点演算器が介入してカノニカルな表現に変換されてしまう」可能性があるからでしょう。

---

そんな感じで、浮動小数点数関連の超マイナーな概念であるカノニカル性の話でした。浮動小数点数の話というよりはむしろコンパイラーの話と言うべきかもしれません。
