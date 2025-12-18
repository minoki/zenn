---
title: "標準ライブラリーが提供する数学関数はどのくらい正確か"
emoji: "🦔"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [浮動小数点数]
published: false
---

大抵のプログラミング言語では、数の四則演算だけではなく、`exp` や `sin` や `pow` などの数学関数が提供されています。この記事では、これらの数学関数について

* どのくらいの精度が期待できるのか？
* どのくらい移植性があるのか？

という話題を扱います。

## 正確な計算は難しい

無限大やNaNを除く有限の浮動小数点数は、数学的な実数を表していると考えられます。例えば、`1.5` という浮動小数点数なら1.5という実数に対応します。

そこで、四則演算や数学関数の仕様としては「与えられた浮動小数点数を正確な実数とみなし、演算を適用し、得られた実数に最も近い（あるいは、適切に丸めた）浮動小数点数」というものが考えられます。このようにして得られた結果を、「**正しく丸められている**」(correctly rounded) と言います。

正しく丸められた結果には、以下の良い性質があります：

* 誤差が最小限になる
* 移植性がある
    * 仕様が数学で決まっているので、浮動小数点形式が同一でさえあれば、x86-64 Windowsで計算してもAArch64 macOSで計算してもRISC-V Linuxで計算しても、スパコンで計算してもマイコンで計算しても、今日の環境で計算しても100年後の環境で計算しても同じ結果になります。

IEEE 754-2019では、正しく丸める指数関数や三角関数が「推奨される追加の演算」（9. Recommended operations / 9.2 Additional mathematical operations）として規定されています。

ここまでの話を聞くと良さそうですね。

しかし、残念なことに、四則演算や平方根などの一部を除いて、**その辺のプログラミング言語で提供される数学関数の精度は実装依存であり、一般には正しく丸められていません**。その辺に転がっている `exp` や `sin` などの関数はIEEE 754準拠ではないのです。

なぜそうなっているかというと、数学関数で正しい丸めを行うためには「最終的な精度よりも高い精度で途中計算を行う」必要があり、しかも「高い精度」を事前に見積もるのが難しい、という事情があるからです。この問題は、W. Kahan先生によって**The Table Maker's Dilemma**（**数表作成者のジレンマ**）と名付けられています[^tmd]。

[^tmd]: W. Kahan, [A Logarithm Too Clever by Half](https://people.eecs.berkeley.edu/~wkahan/LOG10HAF.TXT)

用途によっては「時間的・空間的コストをかけてでも正しい丸めで計算したい」と思うこともあるかもしれません。しかし、普通の浮動小数点数を使った計算はそもそも誤差が避けられない場面が多いこと（ので数学関数ばっかりこだわっても仕方がない）、数学関数はゲームなどのリアルタイム性が必要な状況でも使われる可能性があることを考えると、標準ライブラリーの数学関数は「そこそこのコストで、精度もそこそこ」なものを目指すのがバランスが良いと言えるでしょう。

つまり、その辺に転がっている `exp` や `sin` や `pow` の実装は

* 誤差が最小ではないし
* 環境が変われば異なる値を返す可能性がある

ということになります。

希望のある話もしておきます。研究の進展で、よく使う関数のよく使う精度についてはTable Maker's Dilemmaが解かれたというような話も聞きます。つまり、「正しい丸めを行う数学関数」が実装しやすくなっているということです。また、最近のC言語では（correctly roundedを意図する）`cr_` で始まる関数名を「将来 `<math.h>` に追加されるかもしれない」という扱い（potentially reserved identifiers）にしていますし、将来的には正しい丸めを行う数学関数が利用しやすくなるかもしれません。

## 数学関数は誰が実装するか

数学関数の実装というのはある程度専門性が必要で、プログラミング言語に依存するようなものでもないので、**自前で数学関数の実装を持っているプログラミング言語実装はそこまで多くはない**と思われます。じゃあ誰が実装した数学関数を使うのかというと、C言語の標準ライブラリー、**libc**（数学関数に関してはlibmと呼ぶべきかもしれませんが）を実装する人が用意したものを使うケースが多いと思われます。

多くのプログラミング言語では処理系を書く人が標準ライブラリーも提供するかと思いますが、Unix系ではlibcはOSへのインターフェースも兼ねている都合か、**GCCやClangなどのコンパイラーの開発主体とlibcの開発主体は別であることが多い**です。

libcの実装の例としては、Linuxでよく使われるglibcやmusl、Windowsで使われるMSVCRT/UCRT（WindowsではlibcはCRT（Cランタイム）と呼ばれます）、BSD libc、macOSのlibcなどがあります。

そういうわけで、**コンパイラーが同じでも環境が違えば数学関数の精度は違う**ということになります。例えば、「GCCの数学関数の精度」という言い方はおかしくて、「glibcの数学関数の精度」「macOSのlibcの数学関数の精度」という言い方をしなくてはなりません。

## 実験：C言語の場合

C言語で、数学関数の結果が「正しい丸め」とは異なること、そして環境によって異なることを検証してみます。

検証のためには「正しい丸め」で計算する方法が必要ですが、**GNU MPFR**というライブラリーを使うと（相応のコストをかけて）正しい丸めで計算してくれます。

今回はsin, exp, powについて幾つかの入力を与えて検証してみます。

検証に使うプログラムは以下です。記載のマジックナンバーを計算するのに使ったソースコードは[minoki/math-func-test](https://github.com/minoki/math-func-test)に置きました。

```c
#include <math.h>
#include <stdio.h>

int main(void)
{
    {
        volatile double x = 0x1p2;
        double y = sin(x);
        double cr_result = -0x1.837b9dddc1eaep-1;
        printf("sin(%.16g [%a]) = %.16g [%a]", x, x, y, y);
        if (y == cr_result) {
            puts("");
        } else {
            // AArch64 macOS
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = -0x1.40a3d70a3d70ap+2;
        double y = exp(x);
        double cr_result = 0x1.b52f2f88ff3ddp-8;
        printf("exp(%.16g [%a]) = %.16g [%a]", x, x, y, y);
        if (y == cr_result) {
            puts("");
        } else {
            // x86-64 macOS
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = 0x1.2cccccccccccdp+2;
        volatile double y = -0x1.599999999999ap+1;
        double cr_result = 0x1.f618446e34cf5p-7;
        double z = pow(x, y);
        printf("pow(%.16g [%a], %.16g [%a]) = %.16g [%a]", x, x, y, y, z, z);
        if (z == cr_result) {
            puts("");
        } else {
            // x86-64 UCRT
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = 0x1.3333333333333p-1;
        volatile double y = -0x1.999999999999ap-4;
        double cr_result = 0x1.0d6aba2f6f6a6p+0;
        double z = pow(x, y);
        printf("pow(%.16g [%a], %.16g [%a]) = %.16g [%a]", x, x, y, y, z, z);
        if (z == cr_result) {
            puts("");
        } else {
            // x86-64/AArch64 glibc
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = 0x1.135c28f5c28f6p+4;
        double cr_result = 0x1.282f212d7731ap+8;
        double y = pow(x, 2.0);
        printf("pow(%.16g [%a], 2.0) = %.16g [%a]", x, x, y, y);
        if (y == cr_result) {
            puts("");
        } else {
            // x86-64/AArch64 macOS
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = 0x1.7fe147ae147aep+6;
        double cr_result = 0x1.1fd1ed5cfaacdp+13;
        double y = pow(x, 2.0);
        printf("pow(%.16g [%a], 2.0) = %.16g [%a]", x, x, y, y);
        if (y == cr_result) {
            puts("");
        } else {
            // x86-64/AArch64 glibc
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
    {
        volatile double x = 0x1.2eccccccccccdp+6;
        double cr_result = 0x1.166b1b9eb5f04p+3;
        double y = pow(x, 0.5);
        printf("pow(%.16g [%a], 0.5) = %.16g [%a]", x, x, y, y);
        if (y == cr_result) {
            puts("");
        } else {
            // x86-64/AArch64 musl
            printf("* (cr: %.16g [%a])\n", cr_result, cr_result);
        }
    }
}
```

出力の見方ですが、その環境での数学関数の出力が正しい丸めで計算した結果と異なる場合に後ろに `*` をつけ、その後ろに正しい丸めでの結果を `cr:` として載せています。また、十進表記と十六進表記を併記しています。

macOS 15.7.2 (x86_64) での実行結果は以下となります：

```
sin(4 [0x1p+2]) = -0.7568024953079282 [-0x1.837b9dddc1eaep-1]
exp(-5.01 [-0x1.40a3d70a3d70ap+2]) = 0.006670903306255273 [0x1.b52f2f88ff3dcp-8]* (cr: 0.006670903306255274 [0x1.b52f2f88ff3ddp-8])
pow(4.7 [0x1.2cccccccccccdp+2], -2.7 [-0x1.599999999999ap+1]) = 0.01532271710713083 [0x1.f618446e34cf5p-7]
pow(0.6 [0x1.3333333333333p-1], -0.1 [-0x1.999999999999ap-4]) = 1.052409779148925 [0x1.0d6aba2f6f6a6p+0]
pow(17.21 [0x1.135c28f5c28f6p+4], 2.0) = 296.1841 [0x1.282f212d77319p+8]* (cr: 296.1841000000001 [0x1.282f212d7731ap+8])
pow(95.97 [0x1.7fe147ae147aep+6], 2.0) = 9210.240899999999 [0x1.1fd1ed5cfaacdp+13]
pow(75.7 [0x1.2eccccccccccdp+6], 0.5) = 8.700574693662482 [0x1.166b1b9eb5f04p+3]
```

同じmacOS (15.7.2) でも、アーキテクチャーがAArch64の場合の実行結果は以下となります：

```
sin(4 [0x1p+2]) = -0.7568024953079283 [-0x1.837b9dddc1eafp-1]* (cr: -0.7568024953079282 [-0x1.837b9dddc1eaep-1])
exp(-5.01 [-0x1.40a3d70a3d70ap+2]) = 0.006670903306255274 [0x1.b52f2f88ff3ddp-8]
pow(4.7 [0x1.2cccccccccccdp+2], -2.7 [-0x1.599999999999ap+1]) = 0.01532271710713083 [0x1.f618446e34cf5p-7]
pow(0.6 [0x1.3333333333333p-1], -0.1 [-0x1.999999999999ap-4]) = 1.052409779148925 [0x1.0d6aba2f6f6a6p+0]
pow(17.21 [0x1.135c28f5c28f6p+4], 2.0) = 296.1841 [0x1.282f212d77319p+8]* (cr: 296.1841000000001 [0x1.282f212d7731ap+8])
pow(95.97 [0x1.7fe147ae147aep+6], 2.0) = 9210.240899999999 [0x1.1fd1ed5cfaacdp+13]
pow(75.7 [0x1.2eccccccccccdp+6], 0.5) = 8.700574693662482 [0x1.166b1b9eb5f04p+3]
```

Ubuntu 24.04でx86-64 glibc 2.39を使った実行結果は次のようになります：

```
sin(4 [0x1p+2]) = -0.7568024953079282 [-0x1.837b9dddc1eaep-1]
exp(-5.01 [-0x1.40a3d70a3d70ap+2]) = 0.006670903306255274 [0x1.b52f2f88ff3ddp-8]
pow(4.7 [0x1.2cccccccccccdp+2], -2.7 [-0x1.599999999999ap+1]) = 0.01532271710713083 [0x1.f618446e34cf6p-7]* (cr: 0.01532271710713083 [0x1.f618446e34cf5p-7])
pow(0.6 [0x1.3333333333333p-1], -0.1 [-0x1.999999999999ap-4]) = 1.052409779148926 [0x1.0d6aba2f6f6a7p+0]* (cr: 1.052409779148925 [0x1.0d6aba2f6f6a6p+0])
pow(17.21 [0x1.135c28f5c28f6p+4], 2.0) = 296.1841000000001 [0x1.282f212d7731ap+8]
pow(95.97 [0x1.7fe147ae147aep+6], 2.0) = 9210.240900000001 [0x1.1fd1ed5cfaacep+13]* (cr: 9210.240899999999 [0x1.1fd1ed5cfaacdp+13])
pow(75.7 [0x1.2eccccccccccdp+6], 0.5) = 8.700574693662483 [0x1.166b1b9eb5f05p+3]* (cr: 8.700574693662482 [0x1.166b1b9eb5f04p+3])
```

他の環境でも動作確認できますが、**環境ごとに微妙に出力が違う**ことが確認できました。

ここに載せた実行例は、正しい丸めの結果と異なる場合でも、差は仮数部の最後の1ビットが1違う程度にとどまっています。libcの実装者は結構頑張っていると言えるのではないでしょうか。「真の値に最も近い浮動小数点数ではないかもしれないが、真の値との間に他の浮動小数点数がないようなもの」を選ぶ丸め方を**忠実な丸め** (faithful rounding) と呼んだりします。

## 帰結：繰り返し乗算による冪乗とpow関数は一般には一致しない

一般論に含まれる話ですが、`pow` に関しては強調する価値があると思うので個別に取り上げます。

C/C++で、数の2乗や3乗を計算したい場合はどう書いているでしょうか？`x * x` と書くか、あるいは `pow(x, 2)` と書く人もいるかもしれません。

しかし、実行結果から分かる通り、 **`pow` 関数の結果は環境依存**となります。一方で、`x * x` と書いた方は、モダンな環境であれば正しい丸めで計算されることが期待できます。

実行速度の観点からも違いがありそうです。`x * x` は乗算命令一発で計算できるのに対し、`pow` 関数は複雑な処理をしていると思われます。

したがって、**C/C++で数の2乗を計算するために安易に `pow` 関数を使うべきではない**、ということになります。

言語によっては、繰り返し乗算による冪乗演算子を提供していることがあります。例えば、Haskellには自然数乗の `^` 演算子、整数乗の `^^` 演算子、実数・複素数乗の `**` 演算子があり、前者二つは繰り返し乗算によって実装されているので `x^2` と `x * x` が等価となります。

JavaScriptの状況は興味深いので後述します。

## コンパイル時の計算結果と実行時の計算結果は同じか

実用的なコンパイラーは**定数畳み込み** (constant folding) という最適化を実装しています。例えば、ソース中に `1 + 2` という式があったら加算命令の代わりに `3` を埋め込むのです。

コンパイラーは数学関数についても定数畳み込みするべきでしょうか？つまり、`exp(1.0)` のような式を `2.718...` という定数に置き換えるような最適化をするべきでしょうか？最適化する場合、どの実装を使うべきでしょうか？最適化に使う数学関数の実装と実行時に使う実装が異なる場合、**コンパイル時に計算させた数学関数の値と実行時に計算した値が異なる**という事態が起こります。

これは現実に発生する問題です。次のコードをGCCでコンパイルしてみましょう（Macの人は `gcc` コマンドがClangになっているので、HomebrewやMacPortsで本物のGCCを入れてください）：

```c
#include <math.h>
#include <stdio.h>

int main()
{
    constexpr double a = sin(0x1p938);
    const volatile double x = 0x1p938;
    double b = sin(x);
    printf("[compile-time] sin(0x1p938)=%.17g [%a]\n", a, a);
    printf("[runtime] sin(0x1p938)=%.17g [%a]\n", b, b);
}
```

`constexpr` を使った方は、コンパイル時に `sin` が計算されます。一方で、`volatile` をつけた変数を引数にした方は実行時に `sin` が計算されます。引数はどちらも `0x1p938`（2の938乗）です。

GCCでコンパイル・実行した例を以下に載せます：

```
$ gcc -std=c2x -o const-sin const-sin.c -lm
$ ./const-sin                                    
[compile-time] sin(0x1p938)=0.70858464086739137 [0x1.6acb9b25f25b1p-1]
[runtime] sin(0x1p938)=0.70858464086739148 [0x1.6acb9b25f25b2p-1]
```

コンパイル時と実行時で結果が異なることがわかりました。

実は、**GCCは数学関数の値の定数畳み込みにMPFRを使い、コンパイル時の計算を正しい丸めで行う**のです。そのため、libcの数学関数が正しい丸めではない結果を返す場合、コンパイル時の計算結果と実行時の計算結果が異なる、という事態が発生します。

GCCがMPFRを使うようになったのはGCC 4.3以降のようです：[GCC 4.3 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-4.3/changes.html)

一方、LLVMは数学関数の値の定数畳み込みにホスト環境の実装を使うようです。クロスコンパイル時に同様の問題が発生しそうです：[Incorrect constant folding behavior of floating-point operations · Issue #62479 · llvm/llvm-project](https://github.com/llvm/llvm-project/issues/62479)

## 正しい丸めを行う関数はどれか

`exp`, `sin`, `pow` などの関数の精度が環境依存ということがわかりました。では、C言語が提供する数学関数はみんな環境依存なのでしょうか？

実は、IEEE 754に準拠したC処理系（正確には、C規格のAnnex Fに準拠した、という意味）であれば、四則演算、`sqrt`、`fma` などの一部の演算は正しい丸めであることが保証されます。詳しくは「[浮動小数点演算の結果が環境依存なのはどんなときか](https://zenn.dev/mod_poppo/articles/floating-point-portability)」を読んでください。

まあ、Annex F準拠を謳うC処理系がどれだけあるのか、という話もありますが……。`fma` も実装によってはバグっているというのは「[FMA (fused multiply-add) の話](https://qiita.com/mod_poppo/items/e6577df362f44a3ef8dd)」に書いた通りです。

## 他の言語の場合

C以外の言語でもlibcの数学関数を使っている場合がある、というのは上の方で述べました。ここでは、人気のプログラミング言語をいくつか選んで、「その環境におけるlibcの実装と同じ（正しい丸めとは限らない）値を出すか」を実験してみます。

### Python

```python
import math

def print_float(msg: str, x: float) -> None:
    print(f"{msg}{x} [{x.hex()}]")

print_float("sin(4)=", math.sin(4))
print_float("exp(-5.01)=", math.exp(-5.01))
print_float("pow(4.7, -2.7)=", math.pow(4.7, -2.7))
print_float("pow(0.6, -0.1)=", math.pow(0.6, -0.1))
print_float("pow(17.21, 2.0)=", math.pow(17.21, 2.0))
print_float("pow(95.97, 2.0)=", math.pow(95.97, 2.0))
print_float("pow(75.7, 0.5)=", math.pow(75.7, 0.5))
```

### JavaScript

```js
console.log("sin(4)=", Math.sin(4))
console.log("exp(-5.01)=", Math.exp(-5.01))
console.log("pow(4.7, -2.7)=", 4.7 ** (-2.7))
console.log("pow(0.6, -0.1)=", 0.6 ** (-0.1))
console.log("pow(17.21, 2.0)=", 17.21 ** 2.0)
console.log("pow(95.97, 2.0)=", 95.97 ** 2.0)
console.log("pow(75.7, 0.5)=", 75.7 ** 0.5)
```

### Java

### C#

### Go
