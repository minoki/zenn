---
title: "IEEE 754-2019のminimum/maximum/minimumNumber/maximumNumber演算を実装する"
emoji: "📚"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [ieee754, c言語]
published: true
---

浮動小数点数のmin/max演算にはいくつかバリエーションがあるというのは「[浮動小数点数の min / max](https://qiita.com/mod_poppo/items/41a09bd40acfceec6ec8)」で述べました。

ここでは、IEEE 754-2019で規定された

* minimum/maximum: NaNを伝播させる
* minimumNumber/maximumNumber: NaNを入力の欠落として扱う

をC言語で実装してみます。

素朴な `x < y ? x : y` みたいなやつと比べると、IEEE 754-2019で規定されたものは「可換である」「結合的である」などの良い性質を持ちます。これらの性質は並列化で役立つかもしれないので、SIMD版も載せておきます。

## minimum/maximum

最初はminimum/maximum演算について見ていきます。まず、仕様を確認します。

### 仕様

これらは、ゼロの符号については-0 < +0とします。入力の少なくとも一方が（quietまたはsignaling）NaNであれば、（signaling NaNに対して例外を発生させつつ）quiet NaNを返します。

つまり、minimumは、x < yであればxを、y < xであればyを返します。一方がNaNであれば（signaling NaNに対して例外を発生させつつ）何らかのquiet NaNを返します。符号も含めてx = yであれば、xまたはyのいずれかを返します。

一方でmaximumは、x > yであればxを、y > xであればyを返します。一方がNaNであれば（signaling NaNに対して例外を発生させつつ）何らかのquiet NaNを返します。符号も含めてx = yであれば、xまたはyのいずれかを返します。

「x = yであればxまたはyのいずれか」について補足しておきます。我々が普段使う `float` 型や `double` 型は、（NaNでなくて）数値的に等しく、符号ビットも等しければビットパターンも等しく、両者の区別はつきません。しかし、**IEEE 754が定めている他の浮動小数点形式では、数値的に等しく、符号ビットも等しい場合であってもビットパターンが同じとは限りません**。具体的には、十進形式ではケチ表現を採用しない関係で、同じ数値でも `1.` と `1.00` という風に「末尾の0の個数」が異なる可能性があります。そういうわけで、IEEE 754ではそういう場合に明示的に「どちらを返しても良い」と許可しています。

結果のビットパターンのcanonical性については、特に言及がないのでcanonicalであることが期待されているものと思われますが、自信はありません。

浮動小数点例外に関しては、「入力にquiet NaNが含まれている場合は（他の入力がsignaling NaNだった場合を除いて）例外を発生させない」というのが原則なので、

* 入力の少なくとも一方がsignaling NaNだった→invalid operation例外を発生させる。デフォルトの処理方法ではフラグを立ててquiet NaNを返す。
* 入力にsignaling NaNが含まれず。quiet NaNが含まれた→例外は発生させず、quiet NaNを返す。

と解釈できそうです。

### C言語での実装例

以上を踏まえると、minimum演算のC言語での実装例は次のようになります：

```c
#include <fenv.h>
#include <math.h>

float my_fminimumf(float x, float y)
{
#pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        // 入力の少なくとも一方がNaNだった場合
        return x + y;
    } else {
        // 入力が数値的に等しい場合
        return signbit(x) ? x : y;
    }
}
```

`double` についても同様です。

C23では `fminimum`/`fmaximum` 系の名前でIEEE 754-2019 minimum/maximum演算が使えるようになりますが、執筆時点では対応する環境はまだ少ないと思います。ただし、ここで実装する関数名はC23のものに倣いました。

動作ですが、まず、「入力にquiet NaNが含まれていても例外は発生させない」という要件を満たすために `<` や `>` などの演算子ではなく、`isless` と `isgreater` を使って大小を比較します（「[浮動小数点数の比較について](https://qiita.com/mod_poppo/items/d30b71eb3eb957332145)」も参照）。

入力の少なくとも一方がNaNであることは、`isunordered` マクロで判別できます。これも入力がquiet NaNであれば例外は発生しません。

入力の少なくとも一方がNaNだった場合は出力にそれを伝播させるために、何でもいいので適当な二項演算を使います。ここでは `+` としました。入力にNaNが含まれるので、invalid operation以外の例外は発生しません。そして、invalid operation例外が発生するのはsignaling NaNが含まれる時に限ります。

入力が数値的に等しい場合でも、ゼロの符号は区別したいです。よって `signbit` マクロを使い、`x` の符号ビットが立っていたら `x` を、そうでなければ `y` を返します。この部分は、（ゼロの符号を除いてビットパターンは同一であると仮定して）ビット論理和を使うこともできます。

`float` は普通は二進交換形式であり、全てのビットパターンがcanonicalなので、非canonicalなパターンの考慮は不要です。

入力にsignaling NaNが含まれる場合は `isless`, `isgreater`, `isunordered`, `+` でinvalid operation例外が発生します。あるいは、C言語であれば関数の引数や返り値での受け渡しでinvalid operation例外が発生してquiet NaNに化ける可能性もあります。

### SIMD版・x86 SSEの場合

x86には最小値、最大値を返す `min{s,p}{s,d}`/`max{s,p}{s,d}` 命令がありますが、これらはC言語の次のコードと（例外に関する挙動も含めて）同一です：

```c
float minss(float x, float y) { return x < y ? x : y; }
float maxss(float x, float y) { return x > y ? x : y; }
```

つまり、

* 入力にNaNが含まれる場合はquiet NaNであっても例外を発生させ、`y` を返す
* 入力が両方0の場合は、符号ビットに拘らず `y` を返す

となります。というわけで、これをそのままminimum演算の実装に使うことはできません。x86のmin命令を使うには、

* 入力にNaNが含まれる場合は、あらかじめ無害な値に変換しておく
* ゼロの符号をケアする

ことが必要です。そこで、次のようなコードを考えます：

```c
__m128 my_fminimum_floatx4(__m128 x, __m128 y)
{
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128 eq = _mm_cmpeq_ps(x, y); // non-signaling compare
    __m128 result_eq = _mm_and_ps(eq, x);
    __m128 result_ord = _mm_or_ps(result_eq, result_ltgt); // ordered
    __m128 x_unord = _mm_andnot_ps(ord, x); // ~ord & x
    __m128 y_unord = _mm_andnot_ps(ord, y); // ~ord & y
    __m128 result_unord = _mm_add_ps(x_unord, y_unord); // propagate NaN
    return _mm_or_ps(result_ord, result_unord);
}
```

これは、C言語でいうところの次のような擬似コードをSIMD化したものです：

```c
float x_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : x;
float y_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : y;
float result_ltgt = /* x86 min */ x_ord < y_ord ? x_ord : y_ord;
float eq = /* bitwise and */ x == y ? x : 0.0f;
float result_ord = bitwise_or(eq, result_ltgt);
float x_unord = /* bitwise and-not */ isunordered(x, y) ? x : 0.0f;
float y_unord = /* bitwise and-not */ isunordered(x, y) ? y : 0.0f;
float result_unord = x_unord + y_unord; // NaNの伝播
return bitwise_or(result_ord, result_unord);
```

maximumの方は次のように実装できます：

```c
__m128 my_fmaximum_floatx4(__m128 x, __m128 y)
{
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_max_ps(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128 neq = _mm_cmpneq_ps(x, y); // non-signaling (LT || GT || UNORD)
    __m128 result_ord = _mm_and_ps(_mm_or_ps(neq, x), result_ltgt);
    __m128 x_unord = _mm_andnot_ps(ord, x); // ~ord & x
    __m128 y_unord = _mm_andnot_ps(ord, y); // ~ord & y
    __m128 result_unord = _mm_add_ps(x_unord, y_unord); // propagate NaN
    return _mm_or_ps(result_ord, result_unord);
}
```

これは、C言語でいうところの次のような擬似コードをSIMD化したものです：

```c
float x_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : x;
float y_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : y;
float result_ltgt = /* x86 max */ x_ord > y_ord ? x_ord : y_ord;
float mask = /* bitwise or */ x == y ? x : bitcast(~0)
float result_ord = bitwise_and(mask, result_ltgt);
float x_unord = /* bitwise and-not */ isunordered(x, y) ? x : 0.0f;
float y_unord = /* bitwise and-not */ isunordered(x, y) ? y : 0.0f;
float result_unord = x_unord + y_unord; // NaNの伝播
return bitwise_or(result_ord, result_unord);
```

### SIMD版・x86 AVXの場合

AVXには「マスクの全ての符号ビットが0であるか」を確認する命令があります。それを使って、「入力にNaNが含まれるかどうか」をチェックし、NaNが含まれていた場合のみ面倒な処理を行うことを考えます。

```c
__m256 my_fminimum_floatx8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Some NaN in input
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero
        __m256 result_ltgt = _mm256_min_ps(x_ord, y_ord);
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // non-signaling EQ compare
        __m256 result_eq = _mm256_and_ps(eq, xx);
        __m256 result_ord = _mm256_or_ps(result_eq, result_ltgt); // ordered
        __m256 x_unord = _mm256_and_ps(unord, xx); // ~ord & x
        __m256 y_unord = _mm256_and_ps(unord, yy); // ~ord & y
        __m256 result_unord = _mm256_add_ps(x_unord, y_unord); // propagate NaN
        return _mm256_or_ps(result_ord, result_unord);
    } else {
        // No NaN in input
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // EQ, non-signaling
        __m256 zz_ltgt = _mm256_min_ps(xx, yy);
        __m256 zz_eq = _mm256_and_ps(eq, xx);
        return _mm256_or_ps(zz_eq, zz_ltgt);
    }
}
```

AVXでできる改良はこのくらいです。

### SIMD版・x86 AVX-512の場合

AVX-512DQには、SSE以来の `min{s,p}{s,d}` とは別系統のmin/max命令が追加されます。`vrange{s,p}{s,d}` です。これは0の符号を-0 < +0として扱い、入力のquiet NaNを「入力の欠落」として扱います。つまりIEEE 754-2008のminNum/maxNum準拠ということです。このようにAVX-512ではIntelがIEEE 754-2008を意識している様子が見られるので、個人的には嫌いではないです。

これを使ったminimum演算の実装例は次のようになります：

```c
__m512 my_fminimum_floatx16(__m512 xx, __m512 yy)
{
    __m512 m = _mm512_range_ps(xx, yy, 4); // NaN is missing data
    __mmask16 unord = _mm512_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm512_mask_add_ps(m, unord, xx, yy); // propagate NaN
}
```

`vrange` 系の命令でminを計算し、入力にNaNが含まれるレーンだけ `x + y` で置き換えてやる感じです。

### x86 AVX10.2の場合

「[AVX10.2の新機能](new-features-of-avx10-2)」にも書きましたが、AVX10.2にはIEEE 754-2019のminimum演算に対応する命令が追加されます。良かったですね。

### AArch64の場合

AArch64には、IEEE 754-2019準拠のminimum演算に対応する命令があります。`fmin` です。なので、GCCのインラインアセンブリーを使って次のように実装できます：

```c
float my_fminimumf(float x, float y)
{
    float result;
    asm("fmin %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}
```

`double` の方はACLEの組み込み関数があります：

```c
double my_fminimum(double x, double y)
{
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmin_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmin %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
    */
}
```

SIMD版ももちろんあります。

```c
float32x4_t my_fminimum_floatx4(float32x4_t x, float32x4_t y)
{
    return vminq_f32(x, y);
}
```

x86と比べるとあっけなさすぎますね。これが後出しISAの力ですか。

## minimumNumber/maximumNumber

今度はminimumNumber/maximumNumber演算について見ていきます。まず、仕様を確認します。

### 仕様

これらも、ゼロの符号については-0 < +0とします。入力の少なくとも一方が（quietまたはsignaling）NaNであれば、（signaling NaNに対して例外を発生させつつ）**NaNを「入力の欠落」として扱います**。つまり、両方の入力がNaNであればquiet NaNを返し、片方の入力だけがNaNであれば（たとえそれがsignaling NaNであっても）もう片方を返します。

つまり、minimumNumberは、x < yであればxを、y < xであればyを返します。一方だけがNaNであれば（signaling NaNに対して例外を発生させつつ）もう片方を返し、両方がNaNであれば何らかのquiet NaNを返します。符号も含めてx = yであれば、xまたはyのいずれかを返します。

一方でmaximumNumberは、x > yであればxを、y > xであればyを返します。一方だけがNaNであれば（signaling NaNに対して例外を発生させつつ）もう片方を返し、両方がNaNであれば何らかのquiet NaNを返します。符号も含めてx = yであれば、xまたはyのいずれかを返します。

結果のビットパターンのcanonical性については、特に言及がないのでcanonicalであることが期待されているものと思われますが、自信はありません。

浮動小数点例外に関しては、「入力にquiet NaNが含まれている場合は（他の入力がsignaling NaNだった場合を除いて）例外を発生させない」というのが原則なので、

* 入力の少なくとも一方がsignaling NaNだった→invalid operation例外を発生させる。デフォルトの処理方法ではフラグを立てるが、この演算はNaNではない入力があればそれを返す。
* 入力にsignaling NaNが含まれず、quiet NaNが含まれた→例外は発生させない。

と解釈できそうです。

### C言語での実装例

以上を踏まえると、minimumNumber演算のC言語での実装例は次のようになります：

```c
#include <fenv.h>
#include <math.h>

static inline
float my_canonicalizef(float x)
{
    volatile float one = 1.0f;
    return x * one;
}

float my_fminimum_numf(float x, float y)
{
#pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        x = my_canonicalizef(x);
        y = my_canonicalizef(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
}
```

`double` についても同様です。

C23では `fminimum_num`/`fmaximum_num` 系の名前でIEEE 754-2019 minimumNumber/maximumNumber演算が使えるようになりますが、執筆時点では対応する環境はまだ少ないと思います。ただし、ここで実装する関数名はC23のものに倣いました。

入力のNaNは「入力の欠落」として扱います。quiet NaNだけを考えるのであれば、この挙動は

```c
return isnan(x) ? y : x;
```

で実装できそうです。しかし、signaling NaNを考えるとこれでは不十分です。入力の両方がsignaling NaNだった場合にsignaling NaNを返すわけにはいきません。

ここでは、数学的には恒等写像となるような何らかの浮動小数点演算を適用して、signaling NaNをquiet NaNに変換しておきます。`x * 1.0f` が適していますが、コンパイラーの最適化によって消されてしまわないように `volatile` を使ってみました。

「入力を数値的には変えないが、非canonicalなビットパターンをcanonicalなビットパターンに変換し、signaling NaNをquiet NaNに変える」関数はC23では `canonicalize` と呼ばれます。ここでの関数名はそれを真似てみました。

`my_canonicalizef` は次のように実装することもできます：

```c
#if defined(__SSE2__)
__attribute__((always_inline)) static inline
float my_canonicalizef(float x)
{
    asm volatile("mulss %1, %0" : "+x"(x) : "x"(1.0f));
    return x;
}
#elif defined(__aarch64__)
__attribute__((always_inline)) static inline
float my_canonicalizef(float x)
{
    float result;
    asm volatile("fmul %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(1.0f));
    return result;
}
#else
__attribute__((always_inline)) static inline
float my_canonicalizef(float x)
{
    volatile float one = 1.0f;
    return x * one;
}
#endif
```

### SIMD版・x86 SSEの場合

次のようなコードを考えます：

```c
__m128 my_fminimum_num_floatx4(__m128 x, __m128 y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("mulps %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulps %x1, %x0" : "+x"(y) : "x"(one));
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128 result_ltgt = _mm_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128 eq = _mm_cmpeq_ps(x, y); // non-signaling compare
    __m128 result_eq = _mm_and_ps(eq, x);
    __m128 result_ord = _mm_or_ps(result_eq, result_ltgt); // ordered
    __m128 x_nan_mask = _mm_cmpunord_ps(x, x);
    __m128 result_unord = _mm_or_ps(_mm_and_ps(x_nan_mask, y), _mm_andnot_ps(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_ps(result_ord, _mm_andnot_ps(ord, result_unord));
}
```

これは、C言語でいうところの次のような擬似コードをSIMD化したものです：

```c
x = x * 1.0f; // signaling NaNをquiet NaNに変換する
y = y * 1.0f; // signaling NaNをquiet NaNに変換する
float x_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : x;
float y_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : y;
float result_ltgt = /* x86 min */ x_ord < y_ord ? x_ord : y_ord;
float eq = /* bitwise and */ x == y ? x : 0.0f;
float result_ord = bitwise_or(eq, result_ltgt);
float result_unord = isunordered(x, x) ? y : x;
return bitwise_or(result_or, isunordered(x, y) ? result_unord : 0.0f);
```

maximumNumberは次のようになります：

```c
__m128 my_fmaximum_num_floatx4(__m128 x, __m128 y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("mulps %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulps %x1, %x0" : "+x"(y) : "x"(one));
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_max_ps(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128 neq = _mm_cmpneq_ps(x, y); // non-signaling (LT || GT || UNORD)
    __m128 result_ord = _mm_and_ps(_mm_or_ps(neq, x), result_ltgt);
    __m128 x_nan_mask = _mm_cmpunord_ps(x, x);
    __m128 result_unord = _mm_or_ps(_mm_and_ps(x_nan_mask, y), _mm_andnot_ps(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_ps(result_ord, _mm_andnot_ps(ord, result_unord));
}
```

これは、C言語でいうところの次のような擬似コードをSIMD化したものです：

```c
x = x * 1.0f; // signaling NaNをquiet NaNに変換する
y = y * 1.0f; // signaling NaNをquiet NaNに変換する
float x_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : x;
float y_ord = /* bitwise and */ isunordered(x, y) ? 0.0f : y;
float result_ltgt = /* x86 min */ x_ord < y_ord ? x_ord : y_ord;
float mask = /* bitwise or */ x == y ? x : bitcast(~0)
float result_ord = bitwise_and(mask, result_ltgt);
float result_unord = isunordered(x, x) ? y : x;
return bitwise_or(result_or, isunordered(x, y) ? result_unord : 0.0f);
```

### SIMD版・x86 AVXの場合

これも「入力にNaNが含まれるかどうか」で分岐することができます。

```c
__m256 my_fminimum_num_floatx8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Convert (possible) signaling NaN to quiet one
        __m256 one = _mm256_set1_ps(1.0f);
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(xx) : "x"(one));
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(yy) : "x"(one));
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero (avoid undue INVALID)
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero (avoid undue INVALID)
        __m256 result_ltgt = _mm256_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // non-signaling EQ compare
        __m256 result_eq = _mm256_and_ps(eq, xx);
        __m256 result_ord = _mm256_or_ps(result_eq, result_ltgt); // ordered
        __m256 x_nan_mask = _mm256_cmp_ps(xx, xx, 3); // non-signaling UNORD
        __m256 result_unord = _mm256_blendv_ps(xx, yy, x_nan_mask); // isnan(x) ? y : x
        return _mm256_blendv_ps(result_ord, result_unord, unord);
    } else {
        // No NaN in input
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // EQ, non-signaling
        __m256 zz_ltgt = _mm256_min_ps(xx, yy);
        __m256 zz_eq = _mm256_and_ps(eq, xx);
        return _mm256_or_ps(zz_eq, zz_ltgt);
    }
}
```

AVXでできる改良はこのくらいです。

### SIMD版・x86 AVX-512の場合

`vrange{s,p}{s,d}` は入力のsignaling NaNの扱いを除けば、IEEE 754-2019のminimumNumberと同じ挙動をします。なので、入力のsignaling NaNをあらかじめquiet NaNに変換しておけば `vrange{s,p}{s,d}` を使えます。

```c
__m512 my_fminimum_num_floatx16(__m512 xx, __m512 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512 one = _mm512_set1_ps(1.0f);
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_ps(xx, yy, 4); // NaN is missing data
}
```

### x86 AVX10.2の場合

「[AVX10.2の新機能](new-features-of-avx10-2)」にも書きましたが、AVX10.2にはIEEE 754-2019のminimumNumber演算に対応する命令が追加されます。良かったですね。

### AArch64の場合

AArch64には、IEEE 754-2008のminNum準拠で、-0 < +0として扱う `fminnm` 命令があります。これの入力に含まれるsignaling NaNをあらかじめquiet NaNに変換しておけば目的が果たせます。

```c
float my_fminimum_numf(float x, float y)
{
    float result;
    x = canonicalize_float(x);
    y = canonicalize_float(y);
    asm("fminnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
}
```

SIMD版は次のようになります：

```c
float32x4_t my_fminimum_num_floatx4(float32x4_t x, float32x4_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float32x4_t one = vdupq_n_f32(1.0f);
    asm volatile("fmul.4s %0, %0, %1" : "+w"(x) : "w"(one));
    asm volatile("fmul.4s %0, %0, %1" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vminnmq_f32(x, y);
}
```

## おしまい

IEEE 754-2019のminimum/maximum/minimumNumber/maximumNumberを実装してみました。C23準拠のlibcを実装したかったり、SIMDを使うコードで性質の良いmin/maxが欲しくなった場合は参考にしてください。
