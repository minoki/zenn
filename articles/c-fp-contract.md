---
title: "C言語における浮動小数点演算の短縮 (contract) とそれに対する防衛術"
emoji: "😺"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c", "c言語", "浮動小数点数"]
published: true
---

## 標準

C言語では、複数の浮動小数点演算を一つの演算にまとめることを許容しています。これは式の**短縮** (contract) と呼ばれています（C17 6.5の段落8）。

（JIS X3010では「contract」の訳語に「短縮」を使っているようなので、この記事でもそれに従います。）

この規定により、FMA命令のある環境では `a * b + c` の形の式をFMAへコンパイルすることが可能になります。というか、この規定は実質的にはFMAのためにあると言って良いでしょう。しかし、C標準は式の形には言及していないので、例えば `a + b + c` をまとめて計算できる命令セットがあればそれを利用することも許容されると思われます。

重要なのは、式の短縮によって演算結果が変わるケースがあるということです。実際のコード例は過去の記事にも書きました：

* [浮動小数点演算の結果が環境依存なのはどんなときか](./floating-point-portability)
* [C言語において浮動小数点数の足し算は可換か：FMAがある場合](./floating-point-add-commutativity)

演算結果が変わることが望ましくない場合は、C標準では `#pragma STDC FP_CONTRACT OFF` という記述で式の短縮を阻止できることになっています。

## 実験

式の短縮と `FP_CONTRACT` による阻止を試してみましょう。noinline属性を使いたいのでGCCまたはClangを前提にします。

```c
// contract0.c
#include <math.h>
#include <stdio.h>

__attribute__((noinline))
double multiply_add_0(double a, double b, double c)
{
    return a * b + c;
}

__attribute__((noinline))
double multiply_add_1(double a, double b, double c)
{
    #pragma STDC FP_CONTRACT ON
    return a * b + c;
}

__attribute__((noinline))
double multiply_add_2(double a, double b, double c)
{
    #pragma STDC FP_CONTRACT OFF
    return a * b + c;
}

int main(void)
{
    printf("%.17g\n", multiply_add_0(2.2, -0.1, 1.1));
    printf("%.17g\n", multiply_add_1(2.2, -0.1, 1.1));
    printf("%.17g\n", multiply_add_2(2.2, -0.1, 1.1));
}
```

式の短縮なしで計算した場合は、このプログラムは `0.88` よりほんの少し大きい値を出力します。PythonとかNode.jsのREPL（式の短縮が起こらない）で確かめてみてください：

```python
>>> 2.2 * (-0.1) + 1.1
0.8800000000000001
```

### GCCの場合

では実験です。まず、GCCを使ってみます。x86の場合は `-mfma` オプションを使って、FMA命令が使えることを教えてやります。

```
$ gcc-13 -O2 -mfma -Wall contract0.c && ./a.out
contract0.c: In function ‘multiply_add_1’:
contract0.c:13: warning: ignoring ‘#pragma STDC FP_CONTRACT’ [-Wunknown-pragmas]
   13 |     #pragma STDC FP_CONTRACT ON
      |
contract0.c: In function ‘multiply_add_2’:
contract0.c:20: warning: ignoring ‘#pragma STDC FP_CONTRACT’ [-Wunknown-pragmas]
   20 |     #pragma STDC FP_CONTRACT OFF
      |
0.88
0.88
0.88
```

GCC 13は `FP_CONTRACT` pragmaに対応していないようです。結果として、全てのパターンで式の短縮が行われています。

`-std=c17` オプションを使うと、式の短縮が行われなくなります。

```
$ gcc-13 -O2 -mfma -std=c17 contract0.c && ./a.out
0.88000000000000012
0.88000000000000012
0.88000000000000012
```

GCCでは、`-ffp-contract` オプションで式の短縮を制御できます。GCC 14のマニュアルによると、`-std=c17` オプションにより `-ffp-contract` オプションのデフォルト値が変わるようで、GCC 13もそういう挙動なのだと思われます。

<https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gcc/Optimize-Options.html#index-ffp-contract>

では、`-ffp-contract` オプションを指定してみましょう。これは `fast`, `on`, `off` の3択です。

```
$ gcc-13 -O2 -mfma -ffp-contract=fast contract0.c && ./a.out
0.88
0.88
0.88
$ gcc-13 -O2 -mfma -ffp-contract=on contract0.c && ./a.out
0.88000000000000012
0.88000000000000012
0.88000000000000012
$ gcc-13 -O2 -mfma -ffp-contract=off contract0.c && ./a.out
0.88000000000000012
0.88000000000000012
0.88000000000000012
```

```
$ gcc-14 -O2 -mfma -ffp-contract=fast contract0.c && ./a.out
0.88
0.88
0.88
$ gcc-14 -O2 -mfma -ffp-contract=on contract0.c && ./a.out
0.88
0.88
0.88
$ gcc-14 -O2 -mfma -ffp-contract=off contract0.c && ./a.out
0.88000000000000012
0.88000000000000012
0.88000000000000012
```

`-ffp-contract=fast` は `-std=gnu*` の場合のデフォルトで、短縮が行われました。また、`-ffp-contract=off` の場合は短縮が行われなくなりました。

不思議なのは `-ffp-contract=on` （C標準で認められている場合に短縮する）の挙動がGCC 13とGCC 14で変わっていることです。

### Clangの場合

Clangでも試してみましょう。Clangにも `-ffp-contract` オプションがあります。

<https://clang.llvm.org/docs/UsersManual.html#cmdoption-ffp-contract>

```
$ clang-13 -O2 -mfma contract0.c && ./a.out
0.88000000000000012
0.88
0.88000000000000012
$ clang-13 -O2 -mfma -std=c17 contract0.c && ./a.out
0.88000000000000012
0.88
0.88000000000000012
$ clang-13 -O2 -mfma -ffp-contract=fast contract0.c && ./a.out
0.88
0.88
0.88
$ clang-13 -O2 -mfma -ffp-contract=on contract0.c && ./a.out
0.88
0.88
0.88000000000000012
$ clang-13 -O2 -mfma -ffp-contract=off contract0.c && ./a.out
0.88000000000000012
0.88
0.88000000000000012
```

```
$ clang-14 -O2 -mfma contract0.c && ./a.out
0.88
0.88
0.88000000000000012
$ clang-14 -O2 -mfma -std=c17 contract0.c && ./a.out
0.88
0.88
0.88000000000000012
$ clang-14 -O2 -mfma -ffp-contract=fast contract0.c && ./a.out
0.88
0.88
0.88
$ clang-14 -O2 -mfma -ffp-contract=on contract0.c && ./a.out
0.88
0.88
0.88000000000000012
$ clang-14 -O2 -mfma -ffp-contract=off contract0.c && ./a.out
0.88000000000000012
0.88
0.88000000000000012
```

（Clang 18でもClang 14と同様の実行結果だったので割愛）

Clangでは

* `-ffp-contract=fast`: `#pragma` を無視して常に短縮する
* `-ffp-contract=on`: デフォルトで短縮するが `#pragma` があった場合はそれに従う
* `-ffp-contract=off`: デフォルトで短縮しないが、`#pragma` があればそれに従う

となり、`-ffp-contract` を指定しなかった場合のデフォルトは

* Clang 13までは `off`
* Clang 14以降は `on`

のようです（参考：[Clang 14のリリースノート](https://releases.llvm.org/14.0.0/tools/clang/docs/ReleaseNotes.html#floating-point-support-in-clang)）。

## excess precisionとの関係

C標準にはexcess precisionという規定もあり、これは「演算の途中の値を名目上の型よりも高い精度で保持することを許容する」ものです。excess precisionが有用な状況は例えばx87 FPUで、「`float` や `double` などの演算途中の値を `long double` として保持する」ことを許容します。詳しくは次の記事を参照してください：

* [x87 FPUの呪い](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)

最近ではexcess precisionの概念は `_Float16` の計算に有益かもしれません。

さて、式の短縮は「演算の途中の値を無限精度で扱う」と考えることもできるので、excess precisionとの関係も気になります。

C標準では、excess precisionは「キャスト」あるいは「変数の代入」で削ぎ落とされることが規定されています。そこで、`a * b + c` の `a * b` をキャストあるいは変数に代入するコードを書いてみましょう：

```c
// contract1.c
#include <math.h>
#include <stdio.h>

__attribute__((noinline))
double multiply_add_0(double a, double b, double c)
{
    return a * b + c;
}

__attribute__((noinline))
double multiply_add_1(double a, double b, double c)
{
    return (double)(a * b) + c;
}

__attribute__((noinline))
double multiply_add_2(double a, double b, double c)
{
    double ab = a * b;
    return ab + c;
}

int main(void)
{
    printf("%.17g\n", multiply_add_0(2.2, -0.1, 1.1));
    printf("%.17g\n", multiply_add_1(2.2, -0.1, 1.1));
    printf("%.17g\n", multiply_add_2(2.2, -0.1, 1.1));
}
```

GCC/Clangでexcess precisionを制御するオプションは `-fexcess-precision` です。コンパイル・実行結果を以下に載せます:

```
$ gcc-14 -O2 -mfma -fexcess-precision=standard contract1.c && ./a.out
0.88
0.88
0.88
$ gcc-14 -O2 -mfma -fexcess-precision=fast contract1.c && ./a.out
0.88
0.88
0.88
$ clang-18 -O2 -mfma -fexcess-precision=standard contract1.c && ./a.out
0.88
0.88
0.88000000000000012
$ clang-18 -O2 -mfma -fexcess-precision=fast contract1.c && ./a.out
0.88
0.88
0.88000000000000012
```

GCCはキャストや代入を行った版も関係なく短縮していることがわかります。Clangでは、キャストは短縮を抑止せず、変数への代入は短縮を抑止するようです。

この結果から、**excess precisionと式の短縮は関係なさそう**（少なくともGCCやClangはそう思っている）ということがわかります。

## 式の短縮に対する防衛術

式の短縮が望ましくない場合は、どうやったら阻止できるでしょうか。

まず、`#pragma STDC FP_CONTRACT` に対応している環境ならそれを使えば良いです（Clangなど）。一方、GCCは `#pragma STDC FP_CONTRACT` に対応していないので、別の方法が必要です。

（これから書く内容は「Binary Hacks Rebooted」の「Hack #73 浮動小数点環境を触るコードに対するコンパイラの最適化と戦う」の焼き直しです。本を持っている方はそちらも見てみてください。）

まず考えられる方法は、`volatile` の使用です。コード例を載せます：

```c
#include <math.h>
#include <stdio.h>

__attribute__((noinline))
double multiply_add(double a, double b, double c)
{
    return a * b + c;
}

__attribute__((noinline))
double non_fusing_multiply_add(double a, double b, double c)
{
    volatile double ab = a * b;
    return ab + c;
}

int main(void)
{
    printf("%.17g\n", multiply_add(2.2, -0.1, 1.1));
    printf("%.17g\n", non_fusing_multiply_add(2.2, -0.1, 1.1));
}
```

実行例:

```
$ gcc-14 -O2 -mfma no-contract-volatile.c && ./a.out
0.88
0.88000000000000012
```

うまくいきました。

この方法の欠点は、不必要なメモリアクセスが発生することです。実際、生成されたアセンブリコードを読むと、不必要な `vmovsd` が出力されていることがわかります:

```
non_fusing_multiply_add:
	vmulsd	%xmm1, %xmm0, %xmm0
	vmovsd	%xmm0, -8(%rsp)
	vmovsd	-8(%rsp), %xmm0
	vaddsd	%xmm2, %xmm0, %xmm0
	ret
```

まあCPUの最適化とかキャッシュはすごそうなのでこれで観測可能な違いが発生するかは怪しいですが、余分な命令が出力されているとそわそわする人もいるかもしれません。

GCCでは、インラインアセンブリを使うことで、変数をレジスタに置くことを妨げずに最適化を抑制することができます。例を載せます：

```c
#include <math.h>
#include <stdio.h>

#if defined(__SSE2__)
#define GUARD(x) __asm__ __volatile__("" : "+x"(x))
#define FORCE_EVAL(x) __asm__ __volatile__("" : : "x"(x))
#elif defined(__aarch64__)
#define GUARD(x) __asm__ __volatile__("" : "+w"(x))
#define FORCE_EVAL(x) __asm__ __volatile__("" : : "w"(x))
#else
#error unsupported architecture
#endif

__attribute__((noinline))
double multiply_add(double a, double b, double c)
{
    return a * b + c;
}

__attribute__((noinline))
double non_fusing_multiply_add(double a, double b, double c)
{
    double ab = a * b;
    GUARD(ab);
    return ab + c;
}

int main(void)
{
    printf("%.17g\n", multiply_add(2.2, -0.1, 1.1));
    printf("%.17g\n", non_fusing_multiply_add(2.2, -0.1, 1.1));
}
```

実行例:

```
$ gcc-14 -O2 -mfma no-contract-asm.c && ./a.out
0.88
0.88000000000000012
```

うまくいきました。アセンブリコードは次のようになります:

```
non_fusing_multiply_add:
	vmulsd	%xmm1, %xmm0, %xmm0
	vaddsd	%xmm2, %xmm0, %xmm0
	ret
```

不要な `vmovsd` が存在しません。いいですね。

一応解説しておくと、`GUARD` は「変数の値が利用され、変化したかもしれない」とGCCに思わせるマクロです。疑似コードで書けば、`GUARD(x)` はコンパイラーから見ると次のように見えるでしょう：

```c
x = f(x); // f は不透明な関数
```

`FORCE_EVAL` は「変数の値が利用されるかもしれない」とGCCに思わせるマクロです。疑似コードで書けば、`FORCE_EVAL(x)` はコンパイラーから見ると次のように見えるでしょう：

```c
f(x); // f は不透明な関数
```

これを応用すると、「絶対に短縮を起こさない掛け算・足し算」を行う関数は次のように書けます：

```c
__attribute__((always_inline)) inline
double non_fusing_multiply(double x, double y)
{
    GUARD(x);
    GUARD(y);
    double z = x * y;
    GUARD(z);
    return z;
}

__attribute__((always_inline)) inline
double non_fusing_add(double x, double y)
{
    GUARD(x);
    GUARD(y);
    double z = x + y;
    GUARD(z);
    return z;
}
```

---

これであなたも今日から式の短縮マスターです。ここに書いたテクニックを使ってコンパイラーに望み通りの動作をさせましょう。

あとよかったら「[Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)」を買ってください（宣伝）。
