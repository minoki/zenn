---
title: "浮動小数点演算の丸めモードを関数型言語から制御するためのそれなりに冴えたやり方"
emoji: "👻"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [浮動小数点数]
published: true
---

浮動小数点演算では、演算の結果を出力の型（浮動小数点形式）で正確に表現できないことが多く、そういう場合は通常は**丸め**が行われます。典型的には「最も近い浮動小数点数、ただし真の結果が表現可能な浮動小数点数から等距離にある場合は最下位の桁が偶数のものを選ぶ、それもできない場合は絶対値が大きい方を選ぶ」というやり方（IEEE 754で言うroundTiesToEven）が採用されますが、環境によってはそれ以外の方法を使うことができます。

こういう丸めモードの制御ができると、**区間演算**で便利です。

この辺の話題については「[浮動小数点数の演算の丸めモードを制御する](https://blog.miz-ar.info/2020/01/fp-rounding-mode/)」に書きましたが、ここでは、丸めモードの制御を関数型言語からやることを考えます。

まず、C言語の `fesetround` は手続き的なやり方であり、純粋関数的な考え方とは合いません（Standard MLには `fesetround` に相当する関数があったりしますが……）。純粋関数型でなくても、コンパイラーの最適化と相性が悪いことが多いです。

さらに、Haskellのような遅延評価が基本の言語だと、同じ式を評価しても評価のタイミングの丸めモードで結果が変わってしまうということにもなります。

```haskell
f :: Float -> Float -> IO Float
f x y = do
  fesetround FE_UPWARD
  let !z = x + y
  fesetround FE_TONEAREST
  pure z

main = f (1 + 0x1p-100) 2 -- 1 + 0x1p-100 はどのタイミングで計算される？
```

そこで、関数型言語では丸めモードを追加の引数として渡すインターフェースが考えられます：

```haskell
roundedAdd :: Float -> Float -> RoundingMode -> Float
```

（このやり方では、浮動小数点環境の側面のうち浮動小数点例外は扱えませんが、この記事では無視します。）

追加の引数を使うメリットは、コンパイラーの最適化や遅延評価との相性が良いこと、命令に丸めモードを埋め込める命令セット（AVX-512やRISC-V）を自然に活用できることです。

デメリットは、丸めモードの変更を頻繁に行う場合に変更を最小限で済ませる工夫がしづらい（コンパイラーが十分に賢ければ工夫してくれるかもしれないが、そういう最適化をしてくれるコンパイラーは少ないと思われる）ことです。

## どの演算に丸めモードを指定できるか

一般論として、ハードウェア実装されている浮動小数点演算には丸めモードの変更が効くことが多いです。逆に、`sin` や `exp` などの複雑な処理をする関数では丸めモードが思った通りにならないかもしれません。

ハードウェア実装されることが多い浮動小数点演算というのは、具体的には以下です：

* 四則演算：`+`, `-`, `*`, `/`
* FMA
* 平方根
* 型キャスト：幅の縮小、整数値への変換

なので、関数型言語でもこれらの演算に対して丸めモードを指定できるバージョンを提供すると良いでしょう。

## `RoundingMode` 型について

さて、 `RoundingMode` 型はどのように定義するのが良いでしょうか？

まず、ML系言語で普通に考えられるのは代数的データ型を使うことです。

```haskell
data RoundingMode
  = RoundTiesToEven
  | RoundTowardNegative
  | RoundTowardPositive
  | RoundTowardZero
```

Standard MLでは実際に `rounding_mode` 型が代数的データ型として定義されています：[The IEEEReal structure](https://smlfamily.github.io/Basis/ieee-float.html)

```sml
datatype rounding_mode
  = TO_NEAREST
  | TO_NEGINF
  | TO_POSINF
  | TO_ZERO
```

この定義には2つの問題があり得ます：

* 環境によっては丸め方が4種類ではない。RISC-Vでは5番目の丸め方、roundTiesToAwayが使えるので、プログラミング言語からもそれを扱えるようにしたい。あるいは、JavaScriptやWebAssemblyでは丸め方が標準のroundTiesToEvenしか使えないので制限したい。
* （本質的な問題ではないが）コンパイラーのプリミティブを提供することを考える場合、代数的データ型よりも整数型の方が扱いやすいかもしれない。C言語でも `int` だし。

そこで、`RoundingMode` 型は整数型のエイリアスにして、環境依存な値を突っ込める（どの値が有効かは環境に依存する）ことにする方法が考えられます。環境によって使える丸め方が違う問題は条件コンパイルするか、ライブラリー等で吸収してもらうことにします。

```haskell
newtype RoundingMode = RM Int

roundTiesToEven, roundTowardNegative, roundTowardPositive, roundTowardZero, roundTiesToAway :: RoundingMode

roundedAdd :: Float -> Float -> RoundingMode -> Float
```

`roundedAdd` 関数の実装は、C言語で次のように書けるでしょう：

```c
#pragma STDC FENV_ACCESS ON

float roundedAdd(float x, float y, int rounding)
{
    int prev = fegetround();
    fesetround(rounding);
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    float result = x + y;
    FORCE_EVAL(result); // 最適化防止
    fesetround(prev);
    return result;
}
```

## どういう整数値で表すべきか

`RoundingMode` 型を整数で表すとして、どういう整数値を使うべきでしょうか。

まず考えられるのは、C言語の `FE_TONEAREST`, `FE_DOWNWARD`, `FE_UPWARD`, `FE_TOWARDZERO`, `FE_TONEARESTFROMZERO` などの値をそのまま使うことです。しかし、これらはCのマクロであり、関数型言語の中で定数として使うのが難しいかもしれません。

次に考えられるのは、すべての環境で

```
roundTiesToEven = 0
roundTowardNegative = 1
roundTowardPositive = 2
roundTowardZero = 3
roundTiesToAway = 4
```

という風に値を決め打ちして、Cで実装された `roundedAdd` 関数の方で値を変換するというやり方です。実装例は次のようになります：

```c
#pragma STDC FENV_ACCESS ON

float roundedAdd(float x, float y, int rounding)
{
    int prev = fegetround();
    int c_rounding;
    switch (rounding) {
    case 0: c_rounding = FE_TONEAREST; break;
    case 1: c_rounding = FE_DOWNWARD; break;
    case 2: c_rounding = FE_UPWARD; break;
    case 3: c_rounding = FE_TOWARDZERO; break;
#if defined(FE_TONEARESTFROMZERO)
    case 4: c_rounding = FE_TONEARESTFROMZERO; break;
#endif
    default: unreachable();
    }
    fesetround(c_rounding);
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    float result = x + y;
    FORCE_EVAL(result); // 最適化防止
    fesetround(prev);
    return result;
}
```

`switch` 文は一般には条件分岐やテーブル参照にコンパイルされるわけですが、もっと効率化を図ることはできるでしょうか？

最適化のヒント `unreachable()` を使った上でコンパイラーが賢ければ、「環境非依存に定義した値」と「`FE_TONEAREST` 等の値」が一致する場合、あるいはビットシフト一発で変換できる場合に、その事実を活用してコンパイルしてくれる可能性があります。

例えば、x86では

```
roundTiesToEven = 0
roundTowardNegative = 1
roundTowardPositive = 2
roundTowardZero = 3
```

とするのがx87 Control Word / SSE MXCSR的に都合が良く、実際にlibcでは

```c
#define FP_TONEAREST (0 << 10)
#define FP_DOWNWARD (1 << 10)
#define FP_UPWARD (2 << 10)
#define FP_TOWARDZERO (3 << 10)
```

と定義していることが多そうです（10ビット分のシフトはControl Word中のRCフィールドの位置に対応する）。

一方で、AArch64では

```
roundTiesToEven = 0
roundTowardPositive = 1
roundTowardNegative = 2
roundTowardZero = 3
```

とするのがFPCR的に都合が良く、実際にlibcでは

```c
#define FP_TONEAREST (0 << 22)
#define FP_UPWARD (1 << 22)
#define FP_DOWNWARD (2 << 22)
#define FP_TOWARDZERO (3 << 22)
```

と定義していることが多そうです（22ビット分のシフトはFPCR中のRModeフィールドの位置に対応する）。

そうすると、関数型言語に露出する定数を

```
roundTiesToEven = 0
roundTowardNegative = 1
roundTowardPositive = 2
roundTowardZero = 3
```

と定義した場合、AArch64では変換のコストが小さい（ビットシフトだけで済む）のに対し、x86ではコストが大きい（テーブル参照や条件分岐が発生する）ということになります。一方で、x86に合わせるとAArch64でコストが増えます。

「良いとこ取り」、つまり、すべての環境で同じ（`<fenv.h>` に依存しない）値を使いつつ、主要なアーキテクチャーで浮動小数点制御レジスターに設定する値を得るためのコストが小さくなるような方式はないのでしょうか？

## 丸めモードを表す技巧的な整数値

「丸めモードを表す整数値」は自由に設定できるので、必ずしも連番である必要はありません。では、こういうのはどうでしょうか：

```
roundTiesToEven     = 0b00_00 = 0
roundTowardNegative = 0b10_01 = 9
roundTowardPositive = 0b01_10 = 6
roundTowardZero     = 0b11_11 = 15
                        ^^ ^^
                         |  +-- x86向けの2ビット
                         +----- AArch64向けの2ビット
```

この方式では、x86のControl Word.RC / MXCSR.RCに割り当てる値を計算するには、ビット論理積で下位2ビットを取り出すだけで済みます。そして、AArch64のFPCR.RModeに割り当てる値を計算するには、0b1100 = 0xCとの論理積を取って適宜ビットシフトすれば良いです。

もう少し洗練させて、各アーキテクチャーの浮動小数点制御レジスターのビットフィールドの位置を考慮することもできます：

```
roundTiesToEven     = 0b00000000_00_0000000_00_0_00_00_000_000_00
roundTowardNegative = 0b00000000_10_0000000_01_0_01_11_010_000_11
roundTowardPositive = 0b00000000_01_0000000_10_0_10_10_011_000_10
roundTowardZero     = 0b00000000_11_0000000_11_0_11_01_001_000_01
roundTiesToAway     = 0b00000000_00_0000000_00_0_00_00_100_000_00
                                 ^^         ^^   ^^ ^^ ^^^     ^^
                                  |          |    |  |  |       +- PowerPC, MIPS向けの2ビット
                                  |          |    |  |  +--------- RISC-V向けの3ビット
                                  |          |    |  +------------ LoongArch向けの2ビット
                                  |          |    +--------------- x86 (x87 Control Word) 向けの2ビット
                                  |          +-------------------- x86 (MXCSR) 向けの2ビット
                                  +------------------------------- AArch64向けの2ビット
```

これを使うと、適切な位置のビット論理積を取るだけで浮動小数点制御レジスターに設定すべき値が得られます。これは `fesetround` の引数と一致する環境が多いと思われるので、`fesetround` に渡す場合にも使えます。

これを使った `roundedAdd` 関数の実装例は次のようになります：

```c
#include <fenv.h>

#pragma STDC FENV_ACCESS ON

// ここではダミー
#define GUARD(x) (void)x
#define FORCE_EVAL(x) (void)x

#if FE_TONEAREST == 0 && FE_DOWNWARD == (1 << 10) && FE_UPWARD == (2 << 10) && FE_TOWARDZERO == (3 << 10)
// x86
#define MASK (3 << 10)
#elif FE_TONEAREST == 0 && FE_DOWNWARD == (2 << 22) && FE_UPWARD == (1 << 22) && FE_TOWARDZERO == (3 << 22)
// AArch64
#define MASK (3 << 22)
#elif FE_TONEAREST == 0 && FE_DOWNWARD == 2 && FE_UPWARD == 3 && FE_TOWARDZERO == 1
// RISC-V
#define MASK (7 << 5)
#elif FE_TONEAREST == 0 && FE_DOWNWARD == (3 << 8) && FE_UPWARD == (2 << 8) && FE_TOWARDZERO == (1 << 8)
// LoongArch
#define MASK (3 << 8)
#elif FE_TONEAREST == 0 && FE_DOWNWARD == 3 && FE_UPWARD == 2 && FE_TOWARDZERO == 1
// PowerPC, MIPS
#define MASK 3
#else
#define MASK (-1)
#endif

int to_c_rounding(int rounding)
{
    switch (rounding & MASK) {
    case 0b00000000'00'0000000'00'0'00'00'000'000'00 & MASK:
        return FE_TONEAREST;
    case 0b00000000'10'0000000'01'0'01'11'010'000'11 & MASK:
        return FE_DOWNWARD;
    case 0b00000000'01'0000000'10'0'10'10'011'000'10 & MASK:
        return FE_UPWARD;
    case 0b00000000'11'0000000'11'0'11'01'001'000'01 & MASK:
        return FE_TOWARDZERO;
#if defined(FE_TONEARESTFROMZERO)
    case 0b00000000'00'0000000'00'0'00'00'100'000'00 & MASK:
        return FE_TONEARESTFROMZERO;
#endif
    default: unreachable();
    }
}

float roundedAdd(float x, float y, int rounding)
{
    int prev = fegetround();
    int c_rounding = to_c_rounding(rounding);
    fesetround(c_rounding);
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    float result = x + y;
    FORCE_EVAL(result); // 最適化防止
    fesetround(prev);
    return result;
}
```

これをCompiler Explorerに投げてClangでコンパイルすると、x86の場合はこうなります：

```
to_c_rounding:
        mov     eax, edi
        and     eax, 3072
        ret
```

実質and命令一発です。

AArch64ならこうなります：

```
to_c_rounding:
        and     w0, w0, #0xc00000
        ret
```

これもand命令一発ですね。

RISC-Vの場合はFSCR.FRMフィールドに `fsrm` 擬似命令でアクセスできるのでビットシフトの必要はなかったかもしれません。

GCCはClangほど賢くはないようなので、and命令一個にコンパイルしたかったら別の戦略が必要となります。

ここに書いたテクニックはかなり実装者の気分によるところがある（上記以外の謎のアーキテクチャーへの対応が必要になったら変えるのか？）ので、具体的なマジックナンバーはコンパイラーの実装の詳細にとどめる工夫をしたいところです。

## その他の話題

`GUARD` マクロと `FORCE_EVAL` マクロは、`#pragma STDC FENV_ACCESS` プラグマに対応していないGCCのために必要となります。x86とAArch64であれば次のように定義できます：

```c
#if defined(__SSE2__)
#define GUARD(x) __asm__ __volatile__("" : "+x"(x))
#define FORCE_EVAL(x) __asm__ __volatile__("" : : "x"(x))
#elif defined(__aarch64__)
#define GUARD(x) __asm__ __volatile__("" : "+w"(x))
#define FORCE_EVAL(x) __asm__ __volatile__("" : : "w"(x))
#else
#error unsupported architecture
#endif
```

x86では、たかが演算一回のためにx87 Control WordとMXCSRの両方を触るのは効率が悪いかもしれません。SSE2が有効で浮動小数点演算にSSE系の命令を使うことがわかっているなら、MXCSRだけを触るという手があります。GCC/Clangの場合は浮動小数点演算に使う命令の系統は `-mfpmath` オプションで指定できて、SSEが使用される場合は次のマクロが定義されるようです：

```
__SSE_MATH__
__SSE2_MATH__
```

つまり、`roundedAdd` 関数の実装は次のように効率化できます：

```c
#if defined(__SSE_MATH__) || defined(__SSE2_MATH__)
#include <immintrin.h>
#endif

#if defined(__SSE_MATH__)
// floatの演算にSSEが使われる
float roundedAddFloat(float x, float y, int rounding)
{
    unsigned int csr = _mm_getcsr();
    unsigned int new_csr = (csr & ~(3u << 13)) | (rounding & (3u << 13));
    _mm_setcsr(new_csr)
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    float result = x + y;
    FORCE_EVAL(result); // 最適化防止
    _mm_setcsr(csr);
    return result;
}
#else
float roundedAddFloat(float x, float y, int rounding)
{
    int prev = fegetround();
    int c_rounding = to_c_rounding(rounding);
    fesetround(c_rounding);
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    float result = x + y;
    FORCE_EVAL(result); // 最適化防止
    fesetround(prev);
    return result;
}
#endif

#if defined(__SSE2_MATH__)
// doubleの演算にSSE2が使われる
double roundedAddDouble(double x, double y, int rounding)
{
    unsigned int csr = _mm_getcsr();
    unsigned int new_csr = (csr & ~(3u << 13)) | (rounding & (3u << 13));
    _mm_setcsr(new_csr)
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    double result = x + y;
    FORCE_EVAL(result); // 最適化防止
    _mm_setcsr(csr);
    return result;
}
#else
double roundedAddDouble(double x, double y, int rounding)
{
    int prev = fegetround();
    int c_rounding = to_c_rounding(rounding);
    fesetround(c_rounding);
    GUARD(x); // 最適化防止
    GUARD(y); // 最適化防止
    double result = x + y;
    FORCE_EVAL(result); // 最適化防止
    fesetround(prev);
    return result;
}
#endif
```

ここまでやるなら全部アセンブリー言語で書いても良いのでは、という気分になってきますね。

まあ、本当に効率を追求するなら、「浮動小数点制御レジスターを触るのはCPUの気持ち的に遅くなりがち」と考えて、丸めモードを変えずにTwoSum/FastTwoSumを使って頑張るべきなのかもしれません。あるいは、本当に効率を追求する人はAVX-512やRISC-Vのマシンを買って静的に丸めモードを制御すれば良い、と割り切っても良いのか。

---

Haskell処理系のGHCにこれを実装したら面白いかなあと思って色々考えたわけですが、私はハードウェア実装された浮動小数点数を端点とする区間演算をゴリゴリ使うわけではないし、需要があるかわからない機能を実装してもなあと迷っているところです。使いたい人からの要望があれば実装します。
