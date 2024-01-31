---
title: "Arm32のSIMD (NEON) と非正規化数"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [arm, simd, "float", "浮動小数点数"]
published: true
---

Armには命令セット拡張としてNEONと呼ばれるSIMD命令があります。C言語からは `float32x4_t` みたいな型と、`<arm_neon.h>` の組み込み関数でNEONを利用することができます。

```c
// test.c
#include <stdio.h>
#include <arm_neon.h>

int main(void) {
    volatile float32x4_t a = vdupq_n_f32(0x1p-126f);
    volatile float32x4_t b = vdupq_n_f32(0x0.c0ffeep0f);
    volatile float32x4_t c = vdupq_n_f32(0x0.c0ffeep-126f);

    {
        // Armの組み込み関数
        volatile float32x4_t x = vmulq_f32(a, b);
        printf("%a\n", vgetq_lane_f32(x, 0));
        volatile float32x4_t y = vsubq_f32(a, c);
        printf("%a\n", vgetq_lane_f32(y, 0));
    }

    {
        // GCC拡張では演算子も使える
        volatile float32x4_t x = a * b;
        printf("%a\n", vgetq_lane_f32(x, 0));
        volatile float32x4_t y = a - c;
        printf("%a\n", vgetq_lane_f32(y, 0));
    }
}
```

Arm64 (AArch64) ではこれは普通に動作します。

```
$ uname -m
aarch64
$ gcc test.c && ./a.out
0x1.81ffdcp-127
0x1.f8009p-129
0x1.81ffdcp-127
0x1.f8009p-129
```

一方、Arm32ではどうでしょうか。ここではRaspberry Pi 4に入れた64ビットUbuntuの中で32ビットコードを動かしてみます。

```
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon test.c && ./a.out
0x0p+0
0x1p-126
0x1.81ffdcp-127
0x1.f8009p-129
```

組み込み関数を使った方の結果が変わりました。

実はArm32のNEONのベクトル命令は非正規化数（絶対値が小さすぎる浮動小数点数）を強制的に0にするのです。flush to zeroとかいうやつです。`0x1p-126f` と `0x0.c0ffeep0f` の積は非正規化数になるので0になり、`0x1p-126f` と `0x0.c0ffeep-126f` の差は入力の `0x0.c0ffeep-126f` が非正規化数なので0扱いされたのです。

一方、GCC拡張（演算子を使った方）ではflush to zeroが起こっていません。GCCはデフォルトでは浮動小数点数のベクトルに対してNEONのSIMD命令は使わないようです。これは自動ベクトル化でも同じで、例えば素朴なループを書いてもGCCはNEONのSIMD命令は使いません：

（引数で使っているrestrictキーワードについては[自動ベクトル化とC言語のrestrict](vectorization-and-restrict)を参照してください。）

```c
// mul-loop.c
#include <stdio.h>

#if defined(__GNUC__)
__attribute__((noinline))
#endif
void mul(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] * b[i];
    }
}

int main(void) {
    float a[5] = {0x1p-126f, 0x1p-126f, 0x1p-126f, 0x1p-126f, 0x1p-126f};
    float b[5] = {0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f};
    float c[5];
    mul(5, a, b, c);
    for (size_t i = 0; i < 5; ++i) {
        printf("%a\n", c[i]);
    }
}
```

```
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -O3 mul-loop.c && ./a.out
0x1.81ffdcp-127
0x1.81ffdcp-127
0x1.81ffdcp-127
0x1.81ffdcp-127
0x1.81ffdcp-127
```

生成されるアセンブリコードを見ても、スカラーの乗算命令しか使われていないことがわかります：

```
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -S -O3 mul-loop.c
$ grep -i mul mul-loop.s
	.file	"mul-loop.c"
	.global	mul
	.type	mul, %function
mul:
	vmul.f32	s15, s15, s14
	.size	mul, .-mul
	bl	mul(PLT)
```

では、GCCの自動ベクトル化や、ベクトル型に対する演算子では、NEONのSIMD命令を利用することはできないのでしょうか？

そんなことはありません。コンパイラーに対して「非正規化数はどうなってもいい」と伝えれば、コンパイラーはNEONのSIMD命令を使ってくれるようになります。GCCの場合、`-ffast-math` あるいは `-funsafe-math-optimizations` オプションを指定すれば「非正規化数はどうなってもいい」と伝えたことになります。

```
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -funsafe-math-optimizations test.c && ./a.out
0x0p+0
0x1p-126
0x0p+0
0x1p-126
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -O3 -funsafe-math-optimizations mul-loop.c -lm && ./a.out
0x0p+0
0x0p+0
0x0p+0
0x0p+0
0x0p+0
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -S -O3 -funsafe-math-optimizations mul-loop.c
$ grep -i mul mul-loop.s
	.file	"mul-loop.c"
	.global	mul
	.type	mul, %function
mul:
	vmul.f32	q8, q8, q9
	vmul.f32	s15, s15, s14
	vmul.f32	s15, s15, s14
	vmul.f32	s15, s15, s14
	.size	mul, .-mul
	bl	mul(PLT)
```

GCCの自動ベクトル化でSIMD命令が使われるようになりました。また、スカラーで処理される端数分でもflush to zeroになっているので、制御レジスター（FPSCR）のflush to zeroビットを立てる処理がプログラムの初期化の際に行われているようです。

Arm32においてはNEONのSIMD命令は常にflush to zeroが起こるのに対し、スカラー命令ではFPSCRのflush to zeroビットが考慮されるので、FPSCRのflush to zeroビットをクリアしてやると「自動ベクトル化でベクトル命令を使う部分とスカラー命令を使う部分で結果が違う」という状況を引き起こすことができます。FPSCRを戻すためにここでは `fesetenv(FE_DFL_ENV)` を使います（glibcではこれでflush to zeroの設定がクリアされるようです）。

```c
// mul-loop-2.c
#include <stdio.h>
#include <fenv.h>

#if defined(__GNUC__)
__attribute__((noinline))
#endif
void mul(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] * b[i];
    }
}

int main(void) {
    fesetenv(FE_DFL_ENV);
    float a[5] = {0x1p-126f, 0x1p-126f, 0x1p-126f, 0x1p-126f, 0x1p-126f};
    float b[5] = {0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f, 0x0.c0ffeep0f};
    float c[5];
    mul(5, a, b, c);
    for (size_t i = 0; i < 5; ++i) {
        printf("%a\n", c[i]);
    }
}
```

```
$ arm-linux-gnueabihf-gcc -march=armv7-a+neon -O3 -funsafe-math-optimizations mul-loop-2.c -lm && ./a.out
0x0p+0
0x0p+0
0x0p+0
0x0p+0
0x1.81ffdcp-127
```

SIMDで処理できなかった端数の部分だけ0でない「正しい（IEEE準拠な）」結果が得られました。

Arm32のNEONのベクトル命令に関してはflush to zeroの他にDefault NaN（NaNのペイロードが伝播しなくなるやつ）も常時有効だった気がしますが、この記事では割愛します。

AArch64ではベクトル命令でも制御レジスター（FPCR）のflush to zeroビットが考慮されるので、コンパイラーは `-ffast-math` 系のオプションを指定しなくても自動ベクトル化できる部分にベクトル命令を使ってくれます。

2024年においてArm32がどの程度重要なのかはわかりませんが、浮動小数点数に関する小ネタでした。
