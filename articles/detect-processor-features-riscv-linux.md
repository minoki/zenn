---
title: "CPUの機能を実行時に検出する：RISC-V Linux編"
emoji: "🐕"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [riscv]
published: true
---

シリーズ：

* [CPUの機能を実行時に検出する：x86編](detect-processor-features-x86)
* [CPUの機能を実行時に検出する：Arm編](detect-processor-features-arm)
* [CPUの機能を実行時に検出する：実践編](using-processor-features)
* CPUの機能を実行時に検出する：RISC-V Linux編（この記事）

## RISC-Vの拡張について

RISC-Vには多数の拡張があります。コンパイル時にCPUの機能を決め打ちするのではなく、実行時にCPUの機能を検出して実行するコードを切り替えられると便利そうです。この記事では、Linux向けのプログラムでRISC-Vの機能を検出する方法を説明します。

RISC-Vの拡張については「[RISC-V Ratified Specifications](https://riscv.org/specifications/ratified/)」を参照するのが良さそうです。

RISC-Vの拡張（あるいは標準機能）は、いくつかには1文字の名前がついています：

* RV32I/RV64I/RV32E/RV64E: Base Integer Instruction Set
* M: Extension for Integer Multiplication and Division
* A: Extension for Atomic Instructions
* F: Extension for Single-Precision Floating-Point
* D: Extension for Double-Precision Floating-Point
* Q: Extension for Quad-Precision Floating-Point
* C: Extension for Compressed Instructions
* B: Extension for Bit Manipulation
    * Zba, Zbb, Zbsの集合体
* V: Standard Extension for Vector Operations
* G: IMAFD, Zicsr, Zifencei

他の拡張は、ZfaのようにZから始まる複数文字の名前がついています。

素人目にはZから始まる拡張が大量にあって乱雑だなあと思うのですが、その辺はProfileというやつにまとめることで解決されるのでしょう。

* [riscv/riscv-profiles: RISC-V Architecture Profiles](https://github.com/riscv/riscv-profiles)

さて、RISC-Vの拡張機能をユーザーモードで取得する方法は、Armと同様に、OS依存となります。ここでは、Linuxでの取得方法を説明します。

## Linuxの場合

Armと同様に、`getauxval(AT_HWCAP)` で拡張機能を取得できます。一番下のビット（0番目のビット）が立っていたらA拡張が使えて、次のビット（1番目のビット）が立っていたらB拡張が使えて、25番目のビットが立っていたらZ拡張が使えます。……そう、これは1文字の拡張しか取得できません！

```c
#include <sys/auxv.h>
unsigned long getauxval(unsigned long type);
#define AT_HWCAP ...
```

ともかく、試してみましょう。

利用例：

```c
#include <stdio.h>
#include <sys/auxv.h>

int main(void)
{
    unsigned long hwcap = getauxval(AT_HWCAP);
    printf("F: %d\n", (hwcap & (1 << ('F' - 'A'))) != 0);
    printf("D: %d\n", (hwcap & (1 << ('D' - 'A'))) != 0);
    printf("Q: %d\n", (hwcap & (1 << ('Q' - 'A'))) != 0);
    printf("V: %d\n", (hwcap & (1 << ('V' - 'A'))) != 0);
}
```

QEMUでの実行例：

```
$ riscv64-linux-gnu-gcc test_hwcap.c
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./a.out
F: 1
D: 1
Q: 0
V: 1
```

それっぽい結果が得られました。

複数文字からなる拡張を検出するには、hwprobeという機構を使います。

* [RISC-V Hardware Probing Interface — The Linux Kernel documentation](https://www.kernel.org/doc/html//next/riscv/hwprobe.html)
* <https://github.com/torvalds/linux/blob/master/arch/riscv/include/uapi/asm/hwprobe.h>

例えば、Zfa、Zfh、Zvfhminを検出する場合は次のようにします：

```c
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <sched.h> // cpu_set_t
#include <asm/hwprobe.h>
#include <sys/syscall.h> // SYS_riscv_hwprobe

#if RISCV_HWPROBE_EXT_ZVFHMIN == (1ULL << 31)
#undef RISCV_HWPROBE_EXT_ZVFHMIN
#define RISCV_HWPROBE_EXT_ZVFHMIN (1ULL << 31)
#endif

int main(void)
{
    struct riscv_hwprobe probe[1] = {
        {.key = RISCV_HWPROBE_KEY_IMA_EXT_0, .value = 0},
    };
    // long result = sys_riscv_hwprobe(probe, 1, 0, NULL, 0);
    long result = syscall(SYS_riscv_hwprobe, probe, (size_t)1, (size_t)0, (cpu_set_t *)NULL, 0u);
    uint64_t value = result == 0 ? probe[0].value : 0;
    printf("Zfa: %d\n", (value & RISCV_HWPROBE_EXT_ZFA) != 0);
    printf("Zfh: %d\n", (value & RISCV_HWPROBE_EXT_ZFH) != 0);
    printf("Zvfhmin: %d\n", (value & RISCV_HWPROBE_EXT_ZVFHMIN) != 0);
}
```

いくつか注意があります。新しめのglibcかLinuxのヘッダーか何かがあれば `sys_riscv_hwprobe` が関数として定義されると思うのですが、筆者が試した環境にはなかったので、`syscall` 関数を使ってシステムコールを呼んでいます。

それから、古いヘッダーだと `RISCV_HWPROBE_EXT_ZVFHMIN` が

```c
#define RISCV_HWPROBE_EXT_ZVFHMIN (1 << 31)
```

と定義されていて、32ビット整数のオーバーフローを踏んでしまい、意図しない値になってしまいます。そこで、上記のコード例ではその辺を修正しています。

`getauxval` やhwprobe以外の方法としては、`/proc/cpuinfo` を見るという手もあるでしょう。

---

この記事に書いたテクニックは、筆者が作ったHaskell向けのCPU機能検出ライブラリーで使っています。私はまだLinuxが動くRISC-Vの実機を持っていませんが……。

* [minoki/haskell-cpu-features: A Haskell library to detect CPU features](https://github.com/minoki/haskell-cpu-features)

