---
title: "CPUの機能を実行時に検出する：Arm編"
emoji: "🌊"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [arm, aarch64]
published: true
---

シリーズ：

* [CPUの機能を実行時に検出する：x86編](detect-processor-features-x86)
* CPUの機能を実行時に検出する：Arm編（この記事）
* [CPUの機能を実行時に検出する：実践編](using-processor-features)
* [CPUの機能を実行時に検出する：RISC-V Linux編](detect-processor-features-riscv-linux)

---

[CPUの機能を実行時に検出する：x86編](detect-processor-features-x86)の続きです。ArmでCPUの機能を検出したい時にどうするか説明します。

Armで検出したいようなオプショナルな機能で、私が過去に記事にしたものをいくつか挙げておきます：

* FEAT_FP16: [半精度（16ビット）浮動小数点数をC言語から使う](half-precision-floating-point)
* FEAT_JSCVT: [ArmにあるというJavaScript専用命令とは何か、あるいは浮動小数点数を整数に変換する方法について](https://qiita.com/mod_poppo/items/66663d9a05790579b5e4)
* FEAT_SME: [ArmのScalable Matrix Extension (SME)を試す](arm-scalable-matrix-extension)

他にもいくつか[浮動小数点数オタクがM1 Macを触ってみた](https://qiita.com/mod_poppo/items/fb18f2a1441e74af29a3)で紹介しました。

## `cpuid` に相当する命令はないのか

x86では `cpuid` 命令を使うとCPUの情報を取ってこれました。Armに相当するものがないか探すと、`mrs` 命令でシステムレジスターを読み取れば似たようなことができそうです。試してみましょう。

次のコードを書いてみます。`__arm_rsr64` は `mrs` 命令に相当する組み込み関数です（GCCが対応したのは最新の14なので注意してください）。`ID_AA64PFR0_EL1` レジスターからは半精度浮動小数点数の対応状況が読み取れるはず（詳しくはArchitecture Reference Manualを読んでください）なので、これでFEAT_FP16の対応状況を読み取れるはずです。

```c
#include <arm_acle.h>
#include <stdint.h>
#include <stdio.h>

int main(void)
{
    uint64_t r = __arm_rsr64("ID_AA64PFR0_EL1");
    printf("FEAT_FP16: %d\n", (r & 0x000f0000) == 0x00010000);
}
```

ClangあるいはGCC 14でコンパイルすると、期待通り `mrs` 命令にコンパイルされることが分かります：

```
$ clang -S arm-rsr.c 
$ grep mrs arm-rsr.s
	mrs	x8, ID_AA64PFR0_EL1
```

では実行してみましょう。

Linuxでの実行結果：

```
$ clang arm-rsr.c 
$ ./a.out
FEAT_FP16: 1
```

macOSでの実行結果：

```
$ clang arm-rsr.c
$ ./a.out
zsh: illegal hardware instruction  ./a.out
```

Linuxだと期待通り読み取れましたが、macOSではSIGILLが発生してしまいました。

実は、このシステムレジスターを読み取るには特権が必要なのです。ArmだとException Levelというやつでしょうか。ユーザーモードがEL0で、カーネルがEL1らしいです。レジスター名の最後にEL1とあるので、EL1以上じゃないとアクセスできないってことですね。

Linuxで読み取れているのは、発生する例外をカーネルが処理して代わりに値をセットしてくれるということのようです。詳しくは

* [ARM64 CPU Feature Registers — The Linux Kernel documentation](https://www.kernel.org/doc/html/latest/arch/arm64/cpu-feature-registers.html)

を読んでください。

というわけで、`mrs` 命令はポータブルに使うことはできず、**ArmでCPUの使える機能を取得する方法はOSに依存する**ということになります。

## Linuxの場合

Linuxでは、CPUの機能は補助ベクトル (auxilliary vector) と呼ばれる仕組みでアプリケーションに伝えられます。補助ベクトルの情報はlibcが提供する `getauxval` 関数で取得できます。補助ベクトルの種類はいろいろありますが、CPUの機能に関するものは `AT_HWCAP` と `AT_HWCAP2` です。

```c
#include <sys/auxv.h>
unsigned long getauxval(unsigned long type);
#define AT_HWCAP ...
#define AT_HWCAP2 ...
```

つまり、`getauxval(AT_HWCAP)` とか `getauxval(AT_HWCAP2)` を呼び出せばCPUの機能の情報が詰まったビット列が得られるわけです。個々の機能に対応するフラグは `HWCAP_` あるいは `HWCAP2_` で始まる定数で参照できます。

* [ARM64 ELF hwcaps — The Linux Kernel documentation](https://www.kernel.org/doc/html/latest/arch/arm64/elf_hwcaps.html)

利用例：

```c
#include <stdio.h>
#include <sys/auxv.h>

#if !defined(HWCAP2_SME)
#define HWCAP2_SME (1 << 23)
#endif

int main(void)
{
    unsigned long hwcap = getauxval(AT_HWCAP);
    unsigned long hwcap2 = getauxval(AT_HWCAP2);
    printf("FEAT_FP16: %d\n", (hwcap & HWCAP_FPHP) != 0);
    printf("FEAT_JSCVT: %d\n", (hwcap & HWCAP_JSCVT) != 0);
    printf("FEAT_SME: %d\n", (hwcap2 & HWCAP2_SME) != 0);
}
```

実行例：

```
$ gcc cpu-feature-linux.c
$ ./a.out
FEAT_FP16: 1
FEAT_JSCVT: 1
FEAT_SME: 0
```

なお、`HWCAP_CPUID` をテストすることで、前述の `mrs` によるシステムレジスターへのアクセスがユーザーモード（EL0）でできるか確認できそうです。

`getauxval` 以外の方法としては、`/proc/cpuinfo` を見るという手もあるでしょう。

## macOSの場合

macOSの場合は、`sysctl` 系の関数でCPUの機能を問い合わせることができます。iOSの場合も似たような方法が使えるかもしれません（試してない）。

* [Determining Instruction Set Characteristics | Apple Developer Documentation](https://developer.apple.com/documentation/kernel/1387446-sysctlbyname/determining_instruction_set_characteristics)

```c
#include <sys/sysctl.h>
int sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp, void *newp, size_t newlen);
int sysctlbyname(const char *name, void *oldp, size_t *oldlenp, void *newp, size_t newlen);
```

問い合わせ内容を整数で渡す `sysctl` 関数もありますが、Arm CPUの機能に対応する整数定数は定義されてなさそうなので、この用途ではもっぱら `sysctlbyname` 関数を使うことになるでしょう。また、`newp` と `newlen` 引数は値を設定する場合に使うもので、値を取得する用途では必要ありません。なので、`NULL` と 0 を設定しておけば良いでしょう。詳しい使い方は `man 3 sysctl` を参照してください。

機能に対応する文字列は、最近のmacOSでは `hw.optional.arm.FEAT_***` の形をしているようです（macOS 13で確認）。ターミナルで `sysctl hw` を実行すると見れます。2021年に書いた[浮動小数点数オタクがM1 Macを触ってみた](https://qiita.com/mod_poppo/items/fb18f2a1441e74af29a3)の記事ではこの形の文字列は出てこないので、その頃はなかったのでしょう。

```c
#include <stdbool.h>
#include <stdio.h>
#include <sys/sysctl.h>

bool query_cpu_feature(const char *name)
{
    int result = 0;
    size_t len = sizeof(result);
    int ok = sysctlbyname(name, &result, &len, NULL, 0);
    // 成功時には 0 が返る
    return ok == 0 && result != 0;
}

int main(void)
{
    printf("FEAT_FP16: %d\n", (int)query_cpu_feature("hw.optional.arm.FEAT_FP16"));
    printf("FEAT_JSCVT: %d\n", (int)query_cpu_feature("hw.optional.arm.FEAT_JSCVT"));
    printf("FEAT_SME: %d\n", (int)query_cpu_feature("hw.optional.arm.FEAT_SME"));
}
```

実行例（Apple M1, macOS 13）：

```
$ clang cpu-feature-mac.c    
$ ./a.out                    
FEAT_FP16: 1
FEAT_JSCVT: 1
FEAT_SME: 0
```

参考までに、Apple M1でのmacOS 13の `sysctl hw.optional.arm` の実行結果を貼っておきます。macOSのバージョンが上がると表示される項目も増えるかもしれません。

```
$ sysctl hw.optional.arm
hw.optional.arm.FEAT_FlagM: 1
hw.optional.arm.FEAT_FlagM2: 1
hw.optional.arm.FEAT_FHM: 1
hw.optional.arm.FEAT_DotProd: 1
hw.optional.arm.FEAT_SHA3: 1
hw.optional.arm.FEAT_RDM: 1
hw.optional.arm.FEAT_LSE: 1
hw.optional.arm.FEAT_SHA256: 1
hw.optional.arm.FEAT_SHA512: 1
hw.optional.arm.FEAT_SHA1: 1
hw.optional.arm.FEAT_AES: 1
hw.optional.arm.FEAT_PMULL: 1
hw.optional.arm.FEAT_SPECRES: 0
hw.optional.arm.FEAT_SB: 1
hw.optional.arm.FEAT_FRINTTS: 1
hw.optional.arm.FEAT_LRCPC: 1
hw.optional.arm.FEAT_LRCPC2: 1
hw.optional.arm.FEAT_FCMA: 1
hw.optional.arm.FEAT_JSCVT: 1
hw.optional.arm.FEAT_PAuth: 1
hw.optional.arm.FEAT_PAuth2: 0
hw.optional.arm.FEAT_FPAC: 0
hw.optional.arm.FEAT_DPB: 1
hw.optional.arm.FEAT_DPB2: 1
hw.optional.arm.FEAT_BF16: 0
hw.optional.arm.FEAT_I8MM: 0
hw.optional.arm.FEAT_ECV: 1
hw.optional.arm.FEAT_LSE2: 1
hw.optional.arm.FEAT_CSV2: 1
hw.optional.arm.FEAT_CSV3: 1
hw.optional.arm.FEAT_DIT: 1
hw.optional.arm.FEAT_FP16: 1
hw.optional.arm.FEAT_SSBS: 1
hw.optional.arm.FEAT_BTI: 0
hw.optional.arm.FP_SyncExceptions: 1
```

## Windowsの場合

`IsProcessorFeaturePresent` というWindows APIがあるのでそれを使えば良さそうです。

* [IsProcessorFeaturePresent function (processthreadsapi.h) - Win32 apps | Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-isprocessorfeaturepresent)

```c
BOOL IsProcessorFeaturePresent(DWORD ProcessorFeature);
```

Armv8以降に対応する定数は以下でしょうか：

```
PF_ARM_V8_INSTRUCTIONS_AVAILABLE
PF_ARM_V8_CRYPTO_INSTRUCTIONS_AVAILABLE
PF_ARM_V8_CRC32_INSTRUCTIONS_AVAILABLE
PF_ARM_V81_ATOMIC_INSTRUCTIONS_AVAILABLE
PF_ARM_V82_DP_INSTRUCTIONS_AVAILABLE
PF_ARM_V83_JSCVT_INSTRUCTIONS_AVAILABLE
PF_ARM_V83_LRCPC_INSTRUCTIONS_AVAILABLE
```

少ない！

まあ、Windows on Armを持っている人は試してみてください。私はまだ持っていません。

## Clangの組み込み関数を使う（？）

Clangのマニュアル <https://clang.llvm.org/docs/LanguageExtensions.html#builtin-cpu-supports> を読むとAArch64でも `__builtin_cpu_supports` が使えるように読めますが、Clang 18は対応していませんでした。Clang 19の新機能ということになるのでしょう。

試して動かなかったコードを置いておきます：

```c
#include <stdio.h>

int main(void)
{
    printf("FEAT_FP16: %d\n", !!__builtin_cpu_supports("fp16"));
    printf("FEAT_JSCVT: %d\n", !!__builtin_cpu_supports("jscvt"));
    printf("FEAT_SME: %d\n", !!__builtin_cpu_supports("sme"));
}
```
