---
title: "CPUの機能を実行時に検出する：x86編"
emoji: "🦔"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [x86]
published: true
---

その辺で売っているCPUは世代を経るにつれて性能が向上するだけではなく、使える命令が増えていきます。x86系であればSSEが実装され、AVXが実装され、AVX-512が実装され（そして削除され）、という感じです。Armもアーキテクチャーのマイナーバージョンが上がると命令が増えたりします。

新しく追加された命令をプログラムから利用したいとき、直接呼び出してしまうとそのプログラムは古いCPUでは動かなくなってしまいます。コンパイルオプションで切り替えるというのも一つの方法ですが、同じバイナリーを複数のCPUで動かし、最適な性能を得たいという場合に困ります。そこで、実行時にCPUの機能を検出して、プログラム内で分岐するという方法が考えられます。擬似コードで書けば次のようになります：

```c
bool has_AVX2 = ...; // 何らかの方法でAVX2の使用可否を検出する
if (has_AVX2) {
    // AVX2を使った処理を行う
} else {
    // AVX2を使わずに処理を行う
}
```

x86系の場合、実行時にCPUの機能を検出するには `cpuid` 命令を使います。しかし、`cpuid` 命令を使うには特定のレジスターを読み書きする必要があり、C言語のような高級言語からアクセスするにはコンパイラー依存のテクニックが必要です。ここでは、コンパイラーごとに `cpuid` 命令を使う方法を紹介します。

`cpuid` は問い合わせる機能のジャンルを表す2つの整数（leafとかfunctionとか呼ばれる）を受け取り、4つの整数を返す関数と解釈することができます。擬似コードで書けばこんな感じです：

```
function cpuid(leaf: u32, subleaf: u32) -> (eax: u32, ebx: u32, ecx: u32, edx: u32)
```

1番目の引数次第で、2番目の引数は無視されます。そのため、引数を1つしか取らない組み込み関数が用意されている場合もあります。

例えば、AVXの有無は次のように確認できます：

```
(_, _, c, _) = cpuid(0x01, 0); // subleafは無視される
avx: bool = (c & (1 << 28)) != 0;
(_, b, _, _) = cpuid(0x07, 0);
avx2: bool = (b & (1 << 5)) != 0;
avx512f: bool = (b & (1 << 16)) != 0;
```

【追記】CPUがAVXに対応していても、OSがYMMレジスター等に対応していない可能性があるので、本当にAVXを使いたい場合はこの擬似コードでは不十分です。ちゃんとしたやり方はIntel SDM等を参照するか、あるいはいい感じにラップしてくれるライブラリー（後述する「GCC/Clangの組み込み関数」を含む）を使ってください。【/追記】

`cpuid` 命令の詳細はIntel SDMやAMD APM（AMD特有の命令については当然Intel SDMには載っていないのでAMDのマニュアルを見る必要があります）を見てもらうことにして、ここでは各コンパイラーからの `cpuid` の使い方を見ます。

* [Intel® 64 and IA-32 Architectures Software Developer Manuals](https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html)
* [Documentation for AMD Processors, Accelerators, and Graphics](https://www.amd.com/en/search/documentation/hub.html#q=amd64%20architecture%20programmer's%20manual&sortCriteria=%40amd_release_date%20descending&f-amd_document_type=Programmer%20References)

## `<cpuid.h>` を使う

GCCやClangは `<cpuid.h>` を用意しており、`cpuid` 命令のラッパーである `__cpuid` マクロ、`__cpuid_count` マクロ、`__get_cpuid_max` 関数、`__get_cpuid` 関数、`__get_cpuid_count` 関数、そして各種機能に対応する定数が利用できます。

* [gcc/gcc/config/i386/cpuid.h at master · gcc-mirror/gcc](https://github.com/gcc-mirror/gcc/blob/master/gcc/config/i386/cpuid.h)
* [clang: lib/Headers/cpuid.h Source File](https://clang.llvm.org/doxygen/cpuid_8h_source.html)

```c
#include <cpuid.h>
void __cpuid(unsigned int leaf, [out] unsigned int eax, [out] unsigned int ebx, [out] unsigned int ecx, [out] unsigned int edx); // マクロ
void __cpuid_count(unsigned int leaf, unsigned int count, [out] unsigned int eax, [out] unsigned int ebx, [out] unsigned int ecx, [out] unsigned int edx); // マクロ
unsigned int __get_cpuid_max(unsigned int leaf, unsigned int *sig);
int __get_cpuid(unsigned int leaf, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx);
int __get_cpuid_count(unsigned int leaf, unsigned int subleaf, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx);
```

`__get_` から始まる方は `cpuid` 命令を呼び出す際にleafが範囲内かチェックしてくれるようです。

使用例は次のようになります。

```c
#include <cpuid.h>
#include <stdio.h>

int main(void)
{
    unsigned int eax, ebx, ecx, edx;
    __cpuid(0x01, eax, ebx, ecx, edx);
    printf("AVX: %d\n", (ecx & bit_AVX) != 0);
    __cpuid_count(0x07, 0, eax, ebx, ecx, edx);
    printf("AVX2: %d\n", (ebx & bit_AVX2) != 0);
    printf("AVX-512F: %d\n", (ebx & bit_AVX512F) != 0);
}
```

【追記】本当にAVXを使えるか確かめたい場合はこのコードでは不十分です。先述の注意を参照してください。【/追記】

実行例：

```
AVX: 1
AVX2: 1
AVX-512F: 1
```

## GCC/Clangの組み込み関数を使う

GCC/Clangは `cpuid` をラップした組み込み関数を提供しています。

* [x86 Built-in Functions (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/x86-Built-in-Functions.html#index-_005f_005fbuiltin_005fcpu_005fsupports-1)
* [Clang Language Extensions — Clang 19.0.0git documentation](https://clang.llvm.org/docs/LanguageExtensions.html#builtin-cpu-supports)

```c
void __builtin_cpu_init(void);
int __builtin_cpu_is(const char *cpuname);
int __builtin_cpu_supports(const char *feature);
```

`__builtin_cpu_supports` に機能の名前を文字列で渡すと判定できます。

`__builtin_cpu_init` は `main` 関数の前に `__builtin_cpu_supports` を使う場合に呼び出す必要があります。`main` 関数以降ならわざわざ呼び出す必要はありません。

使用例：

```c
#include <stdio.h>

int main(void)
{
    printf("AVX: %d\n", !!__builtin_cpu_supports("avx"));
    printf("AVX2: %d\n", !!__builtin_cpu_supports("avx2"));
    printf("AVX-512F: %d\n", !!__builtin_cpu_supports("avx512f"));
}
```

## MSVCの組み込み関数を使う

MSVCも組み込み関数を提供しています。`<cpuid.h>` のマクロと名前が同じですが引数が違うので注意してください。

* [__cpuid, __cpuidex | Microsoft Learn](https://learn.microsoft.com/en-us/cpp/intrinsics/cpuid-cpuidex?view=msvc-170)

```c
void __cpuid(int cpuInfo[4], int function_id);
void __cpuidex(int cpuInfo[4], int function_id, int subfunction_id);
```

使用例：

```c
#include <stdio.h>

int main(void)
{
    int result[4]; // {eax, ebx, ecx, edx}
    __cpuid(result, 0x01);
    printf("AVX: %d\n", (result[2] & (1 << 28)) != 0);
    __cpuidex(result, 0x07, 0);
    printf("AVX2: %d\n", (result[1] & (1 << 5)) != 0);
    printf("AVX-512F: %d\n", (result[1] & (1 << 16)) != 0);
}
```

【追記】本当にAVXを使えるか確かめたい場合はこのコードでは不十分です。先述の注意を参照してください。【/追記】

## Intelの組み込み関数を使う

みんな大好きIntel Intrinsics Guideに `cpuid` 用の組み込み関数は載っているでしょうか？実はあります。

* [Intel® Intrinsics Guide](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html#)

```c
#include <immintrin.h>
int _may_i_use_cpu_feature(unsigned __int64 a);
int _may_i_use_cpu_feature_ext(unsigned __int64 a, unsigned page);
int _may_i_use_cpu_feature_str(string literal feature, ...);
```

まあ、私が今試した感じではGCC/Clang/MSVCはこれに対応していないようなので、IntelのCコンパイラー専用という感じでしょうか。

使用例：

```c
#include <immintrin.h>
#include <stdio.h>

int main(void)
{
    printf("AVX: %d\n", !!_may_i_use_cpu_feature(_FEATURE_AVX));
    printf("AVX2: %d\n", !!_may_i_use_cpu_feature(_FEATURE_AVX2));
    printf("AVX-512F: %d\n", !!_may_i_use_cpu_feature(_FEATURE_AVX512F));
}
```

```c
#include <immintrin.h>
#include <stdio.h>

int main(void)
{
    printf("AVX: %d\n", !!_may_i_use_cpu_feature_str("avx"));
    printf("AVX2: %d\n", !!_may_i_use_cpu_feature_str("avx2"));
    printf("AVX-512F: %d\n", !!_may_i_use_cpu_feature_str("avx512f"));
}
```

【追記】Intelの組み込み関数が「OSがYMM等に対応しているか」を考慮してくれるかは調べていないのでよくわかりません。本気でこれでAVXの使用可否をテストしたい場合はその辺も確認すると良いでしょう。【/追記】

## インラインアセンブリを使う

GCCやClangではインラインアセンブリで `cpuid` 命令を呼び出すこともできます。

```c
#include <stdio.h>

int main(void) {
    unsigned int eax, ebx, ecx, edx;
    asm volatile("cpuid" : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx) : "0"(0x01), "2"(0));
    printf("AVX: %d\n", (ecx & (1 << 28)) != 0);
    asm volatile("cpuid" : "=a"(eax), "=b"(ebx), "=c"(ecx), "=d"(edx) : "0"(0x07), "2"(0));
    printf("AVX2: %d\n", (ebx & (1 << 5)) != 0);
    printf("AVX-512F: %d\n", (ebx & (1 << 16)) != 0);
}
```

【追記】本当にAVXを使えるか確かめたい場合はこのコードでは不十分です。先述の注意を参照してください。【/追記】

なお、x86_64では `rbx` がcallee-savedなレジスターであるため、Clangの `<cpuid.h>` では `cpuid` の呼び出し前後に `rbx` の退避を行なっているようですが、私が試した感じではGCC/Clangは自前で退避しなくても関数のプロローグとエピローグで `rbx` を退避するコードを出力するようでした。私の知らない事情が絡んでいる可能性はありますが……。
