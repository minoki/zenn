---
title: "Arm64 (AArch64) のFPCRにC言語からアクセスする"
emoji: "🔖"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["arm64", "aarch64"]
published: true
---

Arm64 (AArch64) で浮動小数点数に関する挙動を変えるにはFPCR (Floating-Point Control Register) を触ります。FPCRで制御できる代表的な項目は丸めモードですが、他にもトラップの制御や非正規化数のflush to zeroの有効化、NaNのペイロードの伝播方法の変更など、色々あります。

32ビットArm (AArch32) の頃はFPSCR (Floating-Point Status and Control Register) と言って浮動小数点数例外の状態フラグも同じレジスターで管理されていましたが、64ビット化にあたって状態フラグはFPSRという別のレジスターに分離されました。

Arm64のFPCRは64ビットの幅がありますが、執筆時点では使用されている部分は下位32ビットに収まっており、上位32ビットはreservedとなっています。

# ACLE (Arm C Language Extensions)

ArmのC拡張、ACLEに対応した処理系では `__arm_rsr64`, `__arm_wsr64` などの組み込み関数を使ってFPCRを含む特殊なレジスターにアクセスできます。引数に文字列としてレジスター名を与えます。

* [Arm C Language Extensions > Special register intrinsics](https://arm-software.github.io/acle/main/acle.html#special-register-intrinsics)

執筆時点ではClangがこれらの組み込み関数に対応しています。GCCとMSVCは未対応です。

使用例：

```c
#include <arm_acle.h>

__attribute__((always_inline))
uint64_t get_fpcr(void)
{
    return __arm_rsr64("fpcr");
}
__attribute__((always_inline))
void set_fpcr(uint64_t x)
{
    __arm_wsr64("fpcr", x);
}
```

FPSRにも同様の方法でアクセスできます。

# GCC

現在のGCCはACLEの対応が部分的で、特に `__arm_rsr64`, `__arm_wsr64` には対応していません。代わりに、FPCRやFPSRにアクセスするための組み込み関数を提供しています。

* [AArch64 Built-in Functions (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/AArch64-Built-in-Functions.html)

フルの64ビット幅にアクセスできる `__builtin_aarch64_{get,set}_fpcr64` が追加されたのはGCC 11で、それ以前は `__builtin_aarch64_{get,set}_fpcr` でFPCRの下位32ビットしかアクセスできませんでした。`__builtin_aarch64_{get,set}_fpcr` が実装されたのはGCC 5です。

ちなみに、GCC 10以降では `__has_builtin` が使えるので、それで `__builtin_aarch64_{get,set}_fpcr64` を検出することもできます。

使用例：

```c
__attribute__((always_inline))
uint64_t get_fpcr(void)
{
#if __GNUC__ >= 11
    return __builtin_aarch64_get_fpcr64();
#elif __GNUC__ >= 5
    return (uint64_t)__builtin_aarch64_get_fpcr();
#else
#error unsupported compiler
#endif
}
__attribute__((always_inline))
void set_fpcr(uint64_t x)
{
#if __GNUC__ >= 11
    __builtin_aarch64_set_fpcr64(x);
#elif __GNUC__ >= 5
    __builtin_aarch64_set_fpcr((unsigned long)x);
#else
#error unsupported compiler
#endif
}
```

# インラインアセンブリー

GCCとClangのどちらでも使える方法として、インラインアセンブリーがあります。筆者が `__arm_rsr64`, `__arm_wsr64` の存在を知るまではClang向けにはこれを使っていたので、ここで供養しておきます。

```c
__attribute__((always_inline))
uint64_t get_fpcr(void)
{
    uint64_t fpcr;
    asm volatile("mrs %0, fpcr" : "=r"(fpcr));
    return fpcr;
}
__attribute__((always_inline))
void set_fpcr(uint64_t x)
{
    asm volatile("msr fpcr, %0" : : "r"(x));
}
```

---

コンパイラー間で共通する方法が使えないと面倒ですね。なんとかしてくれ。
