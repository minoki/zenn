---
title: "CPUの機能を実行時に検出する：実践編"
emoji: "🗂"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [simd]
published: true
---

シリーズ：

* [CPUの機能を実行時に検出する：x86編](detect-processor-features-x86)
* [CPUの機能を実行時に検出する：Arm編](detect-processor-features-arm)
* CPUの機能を実行時に検出する：実践編（この記事）

## CPUに依存する機能を使う流れ：組み込み関数を使う場合

CPUの特殊な命令はC/C++からは組み込み関数として利用できることが多いです。「CPUの機能が使えれば組み込み関数を使い、使えなければ別の（従来の）方法を使う」という書き方をしたい場合を考えます。

大まかな流れとしては、「`cpuid` 命令等でCPUの機能が使えるか判定し、使えるかどうかで場合分け（ディスパッチ）」となるでしょう。擬似コードで書けば次のようになります：

```
bool is_feature_available = ...; // 何らかの方法で判定する
if (is_feature_available) {
    // 組み込み関数を使って処理する
} else {
    // 別のやり方で処理する
}
```

例えば、FMA命令が使えればそれを使い、使えなければ通常の積和を使う関数は次のように書けそうです：

```c
bool is_fma_available(void);

double multiply_add_fma(double a, double b, double c)
{
    __m128d aa = _mm_set_sd(a);
    __m128d bb = _mm_set_sd(b);
    __m128d cc = _mm_set_sd(c);
    __m128d result_v = _mm_fmadd_sd(aa, bb, cc);
    double result;
    _mm_store_sd(&result, result_v);
    return result;
}

double multiply_add(double a, double b, double c)
{
    if (is_fma_available()) {
        return multiply_add_fma(a, b, c);
    } else {
        return a * b + c;
    }
}
```

完全なコードは次のようになります：

```c
#if defined(__GNUC__)
#include <cpuid.h>
#endif
#include <immintrin.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

bool is_fma_available(void)
{
#if defined(__GNUC__)
    unsigned int eax, ebx, ecx, edx;
    int ok = __get_cpuid(1, &eax, &ebx, &ecx, &edx);
    if (ok == 0) {
        return false;
    }
#elif defined(_MSC_VER)
    int cpuInfo[4];
    __cpuid(cpuInfo, 1);
    unsigned int ecx = cpuInfo[2];
#else
#error unsupported compiler
#endif
    const unsigned int OSXSAVE = 1u << 27, AVX = 1u << 28, FMA = 1u << 12;
    if ((ecx & (OSXSAVE | AVX | FMA)) != (OSXSAVE | AVX | FMA)) {
        return false;
    }
    uint64_t xcr0 = _xgetbv(0);
    return (xcr0 & 6) == 6; // XMM state (XCR0[1]) and YMM state (XCR0[2])
}

double multiply_add_fma(double a, double b, double c)
{
    __m128d aa = _mm_set_sd(a);
    __m128d bb = _mm_set_sd(b);
    __m128d cc = _mm_set_sd(c);
    __m128d result_v = _mm_fmadd_sd(aa, bb, cc);
    double result;
    _mm_store_sd(&result, result_v);
    return result;
}

double multiply_add(double a, double b, double c)
{
    if (is_fma_available()) {
        return multiply_add_fma(a, b, c);
    } else {
        return a * b + c;
    }
}

int main(void)
{
    printf("FMA is %savailable.\n", is_fma_available() ? "" : "not ");
    double result = multiply_add(2.0, 3.0, 4.0);
    printf("%g\n", result);
}
```

しかし、このコードはMSVCではコンパイルが通りますが、GCCやClangではコンパイルが通りません。GCCやClangでは、`_xgetbv` や `_mm_fmadd_sd` などの（CPUの特殊な機能に対応する）組み込み関数を使うには、その機能を使ってコンパイルして良いことを `-m` オプション等でコンパイラーに通知しなければならないのです。つまり、`-mxsave -mfma` オプションをつければGCCやClangでもコンパイルできます。

その一方で、`-mfma` オプションをつけてしまうと、ファイル全体でFMA命令が有効化されてしまい、「FMA命令が使えなかった場合の処理」の `a * b + c` がFMA命令にコンパイルされてしまう可能性があります。FMA命令を一括で有効化するのではなく、FMAを使っても良い部分と使ってはいけない部分があることをコンパイラーに教える必要があります。

方法の一つは、「FMA命令を使う関数」を別のファイルに切り出し、ファイルごとのコンパイルオプションで切り替える方法です。つまり、`multiply_add_fma` 関数を別のファイルに切り出し、そのファイルは `-mfma` 付きでコンパイルするのです。

別の方法は、GCC/Clangで使える `target` attributeを使う方法です。関数に `__attribute__((target(...)))` 属性を指定することで、その関数内でだけ特定の命令セット拡張を有効にできるのです。`target` attributeを使ったコード例を以下に載せます。

```c
#if defined(__GNUC__)
#include <cpuid.h>
#endif
#include <immintrin.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#if defined(__GNUC__)
__attribute__((target("xsave"))) // この関数内でXSAVE命令セット拡張を有効にする
#endif
bool is_fma_available(void)
{
#if defined(__GNUC__)
    unsigned int eax, ebx, ecx, edx;
    int ok = __get_cpuid(1, &eax, &ebx, &ecx, &edx);
    if (ok == 0) {
        return false;
    }
#elif defined(_MSC_VER)
    int cpuInfo[4];
    __cpuid(cpuInfo, 1);
    unsigned int ecx = cpuInfo[2];
#else
#error unsupported compiler
#endif
    const unsigned int OSXSAVE = 1u << 27, AVX = 1u << 28, FMA = 1u << 12;
    if ((ecx & (OSXSAVE | AVX | FMA)) != (OSXSAVE | AVX | FMA)) {
        return false;
    }
    uint64_t xcr0 = _xgetbv(0);
    return (xcr0 & 6) == 6; // XMM state (XCR0[1]) and YMM state (XCR0[2])
}

#if defined(__GNUC__)
__attribute__((target("fma"))) // この関数内でFMA命令セット拡張を有効にする
#endif
double multiply_add_fma(double a, double b, double c)
{
    __m128d aa = _mm_set_sd(a);
    __m128d bb = _mm_set_sd(b);
    __m128d cc = _mm_set_sd(c);
    __m128d result_v = _mm_fmadd_sd(aa, bb, cc);
    double result;
    _mm_store_sd(&result, result_v);
    return result;
}

double multiply_add(double a, double b, double c)
{
    if (is_fma_available()) {
        return multiply_add_fma(a, b, c);
    } else {
        return a * b + c;
    }
}

int main(void)
{
    printf("FMA is %savailable.\n", is_fma_available() ? "" : "not ");
    double result = multiply_add(2.0, 3.0, 4.0);
    printf("%g\n", result);
}
```

このコードはGCCやClangでもコンパイルが通ります。

## CPUに依存する機能を使う流れ：自動ベクトル化を使う場合

時には、組み込み関数を陽に使うのではなく、コンパイラーの自動ベクトル化のような仕組みで拡張命令を使って欲しい場合もあるでしょう。

例えば、単純な足し算のループはAVXを有効にすればAVXが、AVX-512を有効にすればAVX-512が使用されることが期待できます：

```c
void add_float_array(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

あるいは、`fma` 関数の呼び出しはFMA命令セットを有効にすればFMA命令にコンパイルされることが期待できるでしょう：

```c
double multiply_add_fma(double a, double b, double c)
{
    return fma(a, b, c);
}
```

これも「関数単位で使える命令セットを変えたい」という状況になりますが、その場合のやり方も先ほどと同様に、

* 関数を別のファイルに分けて、そのファイルをコンパイルするときのコンパイルオプションを変更する
* GCC/Clangの場合、`target` attributeを使う

となります。つまり、後者ならこんな感じです：

```c
__attribute__((target("avx")))
void add_float_array_avx(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}

__attribute__((target("avx512f")))
void add_float_array_avx512(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}

__attribute__((target("fma")))
double multiply_add_fma(double a, double b, double c)
{
    return fma(a, b, c);
}
```

しかし、自動ベクトル化の場合に同じループを何回も書くのはだるいですね。これの対処法は後で紹介します。

## `cpuid` を何回も呼び出したくない

上記の `multiply_add` 関数は毎回 `is_fma_available()` を呼び出しています。これは性能を出す上では決して得策ではありません。毎回 `is_fma_available()` を呼ぶことによるコストがどのくらいになるか、計測してみましょう。

```c
#if defined(__GNUC__)
#include <cpuid.h>
#endif
#include <immintrin.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

#if defined(__GNUC__)
__attribute__((target("xsave"))) // この関数内でXSAVE命令セットを有効にする
#endif
bool is_fma_available(void)
{
    // 省略
}

#if defined(__GNUC__)
__attribute__((target("fma"))) // この関数内でFMA命令セットを有効にする
#endif
double multiply_add_fma(double a, double b, double c)
{
    // 省略
}

#if defined(__GNUC__)
__attribute__((noinline))
#endif
double multiply_add_naive(double a, double b, double c)
{
    return a * b + c;
}

#if defined(__GNUC__)
__attribute__((noinline))
#endif
double multiply_add(double a, double b, double c)
{
    if (is_fma_available()) {
        return multiply_add_fma(a, b, c);
    } else {
        return a * b + c;
    }
}

int main(void)
{
    double a = 2.0, b = 3.0, c = 4.0;
    double r = 0.0;
    const int N = 100000;
    clock_t t0 = clock();
    for (int i = 0; i < N; ++i) {
        r += multiply_add_naive(a, b, c);
    }
    printf("[naive] result: %g, time: %.0f us\n", r, (double)(clock() - t0) / CLOCKS_PER_SEC * 1e6);
    if (is_fma_available()) {
        r = 0.0;
        clock_t t1 = clock();
        for (int i = 0; i < N; ++i) {
            r += multiply_add_fma(a, b, c);
        }
        printf("[FMA] result: %g, time: %.0f us\n", r, (double)(clock() - t1) / CLOCKS_PER_SEC * 1e6);
    }
    r = 0.0;
    clock_t t2 = clock();
    for (int i = 0; i < N; ++i) {
        r += multiply_add(a, b, c);
    }
    printf("[dispatch] result: %g, time: %.0f us\n", r, (double)(clock() - t2) / CLOCKS_PER_SEC * 1e6);
}
```

実行例：

```
$ gcc -O3 fma-time.c
$ ./a.out
[naive] result: 1e+06, time: 108 us
[FMA] result: 1e+06, time: 107 us
[dispatch] result: 1e+06, time: 111941 us
```

`clock` 関数による簡易的な計測ではありますが、`cpuid` を呼び出して判定するコードがループの内側にいると、判定しない版の1000倍も時間を食うことがわかりました。`is_fma_available()` のコストは1回あたり1マイクロ秒ということですかね。

`cpuid` の呼び出しコストを削減する方法としては、`cpuid` を使った判定は初回呼び出し時だけ行い、以降の呼び出しでは最初の判定結果を使う、というものがあります。以下にコード例を載せます。

```c
#if defined(__GNUC__)
#include <cpuid.h>
#endif
#include <immintrin.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

#if defined(__GNUC__)
__attribute__((target("xsave"))) // この関数内でXSAVE命令セットを有効にする
#endif
bool is_fma_available(void)
{
    // 省略
}

#if defined(__GNUC__)
__attribute__((target("fma"))) // この関数内でFMA命令セットを有効にする
#endif
double multiply_add_fma(double a, double b, double c)
{
    // 省略
}

#if defined(__GNUC__)
__attribute__((noinline))
#endif
double multiply_add_naive(double a, double b, double c)
{
    return a * b + c;
}

// やり方1：一個の関数で完結するやり方
#if defined(__GNUC__)
__attribute__((noinline))
#endif
double multiply_add(double a, double b, double c)
{
    static _Atomic(double (*)(double, double, double)) s_impl = ATOMIC_VAR_INIT(NULL);
    double (*impl)(double, double, double) = atomic_load_explicit(&s_impl, memory_order_relaxed);
    if (!impl) {
        if (is_fma_available()) {
            impl = multiply_add_fma;
        } else {
            impl = multiply_add_naive;
        }
        atomic_store_explicit(&s_impl, impl, memory_order_relaxed);
    }
    return impl(a, b, c);
}

// やり方2：初期化関数を別に用意するやり方
double multiply_add_init(double, double, double);
static _Atomic(double (*)(double, double, double)) multiply_add_impl = ATOMIC_VAR_INIT(&multiply_add_init);
double multiply_add_init(double a, double b, double c)
{
    double (*impl)(double, double, double);
    if (is_fma_available()) {
        impl = multiply_add_fma;
    } else {
        impl = multiply_add_naive;
    }
    atomic_store_explicit(&multiply_add_impl, impl, memory_order_relaxed);
    return impl(a, b, c);
}
#if defined(__GNUC__)
__attribute__((noinline))
#endif
double multiply_add_2(double a, double b, double c)
{
    double (*impl)(double, double, double) = atomic_load_explicit(&multiply_add_impl, memory_order_relaxed);
    return impl(a, b, c);
}

int main(void)
{
    double a = 2.0, b = 3.0, c = 4.0;
    double r = 0.0;
    const int N = 100000;
    clock_t t0 = clock();
    for (int i = 0; i < N; ++i) {
        r += multiply_add_naive(a, b, c);
    }
    printf("[naive] result: %g, time: %.0f us\n", r, (double)(clock() - t0) / CLOCKS_PER_SEC * 1e6);
    if (is_fma_available()) {
        r = 0.0;
        clock_t t1 = clock();
        for (int i = 0; i < N; ++i) {
            r += multiply_add_fma(a, b, c);
        }
        printf("[FMA] result: %g, time: %.0f us\n", r, (double)(clock() - t1) / CLOCKS_PER_SEC * 1e6);
    }
    r = 0.0;
    clock_t t2 = clock();
    for (int i = 0; i < N; ++i) {
        r += multiply_add(a, b, c);
    }
    printf("[dispatch] result: %g, time: %.0f us\n", r, (double)(clock() - t2) / CLOCKS_PER_SEC * 1e6);
    r = 0.0;
    clock_t t3 = clock();
    for (int i = 0; i < N; ++i) {
        r += multiply_add_2(a, b, c);
    }
    printf("[dispatch2] result: %g, time: %.0f us\n", r, (double)(clock() - t3) / CLOCKS_PER_SEC * 1e6);
}
```

```
$ gcc -O3 fma-time-2.c
$ ./a.out
[naive] result: 1e+06, time: 109 us
[FMA] result: 1e+06, time: 106 us
[dispatch] result: 1e+06, time: 303 us
[dispatch2] result: 1e+06, time: 286 us
```

実行時にディスパッチするコストは相変わらずかかっていますが、それでも毎回 `cpuid` を呼び出すよりは遥かにマシである（1000倍だったものが3倍になった）ことがわかります。

ここではマルチスレッドを考慮してアトミック変数を使いました。メモリオーダリングについては筆者は正直自信がないのですが、この使い方であればrelaxedで十分だと思います。C++であれば、`static` 変数の初期化はスレッドセーフらしいのでそれを使うのも良いでしょう。参考：[ブロックスコープを持つstatic変数初期化のスレッドセーフ化 \[N2660\] - cpprefjp C++日本語リファレンス](https://cpprefjp.github.io/lang/cpp11/static_initialization_thread_safely.html)

## ディスパッチ部分をコンパイラーに任せる：function multiversioning

GCCやClangにはfunction multiversioningというC++の拡張があって、ディスパッチする部分を自分で書かなくても関数オーバーロード感覚で複数のCPU向けのコードを書き分けることができます。

* [Function Multiversioning (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Function-Multiversioning.html)
* [FunctionMultiVersioning - GCC Wiki](https://gcc.gnu.org/wiki/FunctionMultiVersioning)
* [Attributes in Clang — Clang 19.0.0git documentation](https://clang.llvm.org/docs/AttributeReference.html#target)

コード例は以下のようになります。

```cpp
#include <immintrin.h>
#include <stdio.h>

__attribute__((target("default")))
double multiply_add(double a, double b, double c)
{
    puts("Using default");
    return a * b + c;
}

__attribute__((target("fma")))
double multiply_add(double a, double b, double c)
{
    puts("Using FMA");
    __m128d aa = _mm_set_sd(a);
    __m128d bb = _mm_set_sd(b);
    __m128d cc = _mm_set_sd(c);
    __m128d result_v = _mm_fmadd_sd(aa, bb, cc);
    double result;
    _mm_store_sd(&result, result_v);
    return result;
}

int main()
{
    double result = multiply_add(2.0, 3.0, 4.0);
    printf("result=%g\n", result);
}
```

実行例：

```
$ g++ -O2 fmv.cpp
$ ./a.out
Using FMA
result=10
```

注意点として、GCCの出力するコードがELFのGNU IFUNCという機能（拡張？）に依存するため、コンパイラーだけではなくOSやオブジェクトフォーマットにも依存することです。具体的には、WindowsやmacOSとGCCの組み合わせでは使えません。Clangだとコンパイラーが頑張ってくれるのか、WindowsやmacOSでも使えますが……。

## 同じ関数を何個も書きたくない場合

コンパイラーによる自動ベクトル化は、同じコードをオプション次第で異なるコードにコンパイルすることを可能にします。しかし、複数のCPUに向けた複数のコードを得たい場合は、素朴に考えると元のコードも複数用意しないといけないということになります。

重複を避ける方法の一つは、インライン関数を使うことです。例を載せます。

```c
#include <stddef.h>

__attribute__((always_inline))
static void add_float_array_impl(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}

__attribute__((target("avx")))
void add_float_array_avx(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    add_float_array_impl(n, a, b, result);
    // インライン化されたコードが自動ベクトル化されてAVXを使うことが期待できる
}

__attribute__((target("avx512f")))
void add_float_array_avx512(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    add_float_array_impl(n, a, b, result);
    // インライン化されたコードが自動ベクトル化されてAVX-512を使うことが期待できる
}
```

別の方法として、環境依存の度合いが上がりますが、GCCやClangが用意している `target_clones` attributeを使うことです。

* [Common Function Attributes (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-target_005fclones-function-attribute)
* [Attributes in Clang — Clang 19.0.0git documentation](https://clang.llvm.org/docs/AttributeReference.html#target-clones)

このattributeを使うと、コンパイラーが関数を複製してそれぞれの命令セット拡張向けのコードを生成してくれるようになります。CPUの機能を検出してディスパッチするコードもコンパイラーが生成してくれて、呼び出す側は普通の関数として呼び出せます。function multiversioningと同様ですが、こちらはC言語でも使えます。

```c
#include <stddef.h>
#include <stdio.h>

__attribute__((target_clones("default,avx,avx512f")))
void add_float_array(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}

int main(void)
{
    float a[100];
    float b[100];
    float c[100];
    for (int i = 0; i < 100; ++i) {
        a[i] = i * i;
        b[i] = i;
    }
    add_float_array(100, a, b, c);
    for (int i = 0; i < 100; ++i) {
        printf("c[%d] = %g\n", i, c[i]);
    }
}
```

これもGCCはGNU IFUNCを使うため、WindowsやmacOSとGCCの組み合わせでは動きません。

## ライブラリーの使用

前の記事で見たように、CPUの機能の検出方法はコンパイラーやOSに依存したり、AVXの検出がフラグ一発で判定できなくて面倒なので、その辺をいい感じにやってくれるライブラリーを使うというのも手です。いくつか見つけたものを紹介しておきます。

* [google/cpu_features: A cross platform C99 library to get cpu features at runtime.](https://github.com/google/cpu_features)
* [anrieff/libcpuid: a small C library for x86 CPU detection and feature extraction](https://github.com/anrieff/libcpuid)

各種SIMD命令をいい感じにラップしてくれるポータブルなライブラリーもあります。

* [google/highway: Performance-portable, length-agnostic SIMD with runtime dispatch](https://github.com/google/highway)

ただ、ライブラリーがいい感じにラップしてくれていても、裏で動いているのはこの記事で紹介したような書き方です。書き方がだるいな〜〜〜と思ってもポータブルに色々やるための代償だったりするので、仕方ないと割り切りましょう。
