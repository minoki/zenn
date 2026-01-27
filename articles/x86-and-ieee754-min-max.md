---
title: "IEEE 754-2019のmin/max命令とx86のmin/max命令の関係を考える"
emoji: "📑"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [浮動小数点数, ieee754, x86]
published: true
---

浮動小数点数の小さい方・大きい方を返す演算（min/max）にはいくつかのバリエーションがあると言うのは、「[浮動小数点数の min / max](https://qiita.com/mod_poppo/items/41a09bd40acfceec6ec8)」に書きました。IEEE 754で規定されたそれらを実装する方法は「[IEEE 754-2019のminimum/maximum/minimumNumber/maximumNumber演算を実装する](implementing-ieee754-min-max)」に書きました。

x86では[AVX10.2](https://www.intel.com/content/www/us/en/content-details/856721/intel-advanced-vector-extensions-10-2-intel-avx10-2-architecture-specification.html)でIEEE 754-2019準拠の命令が導入されますが、それらが使えるようになるまでにはまだ時間がかかりそうです。この記事では、特定の形のmin/maxをx86の伝統的な `{min,max}{s,p}{s,d}` 命令で表現できないか考えます。コンパイラーの最適化に役立てることを考えています。

## x86の伝統的なmin/max命令

x86のSSEで導入された浮動小数点数のmin/max命令は、Cコードで書けば次のような動作をします：

```c
// min{s,p}{s,d} xmm1, xmm2/m{32,64,128}
// SSEの場合は xmm1（第一オペランド、x）が出力を兼ねる
float pseudoMin(float x, float y)
{
    return x < y ? x : y;
}

// max{s,p}{s,d} xmm1, xmm2/m{32,64,128}
// SSEの場合は xmm1（第一オペランド、x）が出力を兼ねる
float pseudoMax(float x, float y)
{
    return x > y ? x : y;
}
```

疑似コードで場合分けっぽく書くと、次のようになります：

```haskell
pseudoMin :: Float -> Float -> Float
pseudoMin x y = case compare x y of
                  UNORD -> (raise INVALID; y)
                  LT -> x
                  EQ -> y
                  GT -> y

pseudoMax :: Float -> Float -> Float
pseudoMax x y = case compare x y of
                  UNORD -> (raise INVALID; y)
                  LT -> y
                  EQ -> y
                  GT -> x
```

`pseudoMin`/`pseudoMax` は引数の順序を入れ替えると結果が変わる場合があることに注意してください。

WebAssemblyのSIMDには、pmin/pmaxという名前でx86と同じ動作のmin/max演算が規定されています。pはpseudoの意で、上記コードでの `pseudoMin`/`pseudoMax` という名前はこれを参考にしています。ただし、引数の順序は違います。

* [Pseudo-Minimum and Pseudo-Maximum instructions by Maratyszcza · Pull Request #122 · WebAssembly/simd](https://github.com/WebAssembly/simd/pull/122)

## 片方の引数が固定の場合にどちらかをもう片方へ翻訳できるか

伝統的なx86にはIEEE 754-2019準拠のmin/max命令はなく、一方でAArch64にはIEEE 754-2019準拠(またはそれに近い)min/max命令はあってもx86と同じmin/max命令はありません（エミュレーションは難しくはないですし、実は[FEAT_AFP](arm-feat_afp)にありますが）。

すると、「特別な場合、例えば片方の引数が固定の場合にどちらかをもう片方へ翻訳できないか」という発想が出てきます。片方の引数を固定する状況というのは、clamp操作 `min (max x 0.0) 1.0` という形で自然に登場します。

### pseudoMinの第一引数を固定する

`a` を0でもNaNでもない数または無限大とします。

```haskell
pseudoMin a y
  | isNaN y = (raise INVALID; y)
  | otherwise = minimum a y
```

これはNaNを伝播するminということで、例外とsignaling NaNを無視すればIEEE 754-2019のminimum演算と一致しそうです。

`a` が符号付き0の場合はどうでしょうか。

```haskell
pseudoMin +0.0 y
  | isNaN y = (raise INVALID; y)
  | y == 0.0 = y
  | otherwise = minimum +0.0 y

pseudoMin -0.0 y
  | isNaN y = (raise INVALID; y)
  | y == 0.0 = y
  | otherwise = minimum -0.0 y
```

`a` が `+0.0` の場合は、やはり例外とsignaling NaNを無視すればminimum演算と一致しそうです。一方で、`-0.0` の場合は `y` が `+0.0` の場合にminimumと一致しません。具体的には

```
pseudoMin -0.0 +0.0 = +0.0
minimum -0.0 +0.0 = -0.0
```

となります。

`a` が無限大の場合も考えておきます。

```haskell
pseudoMin Infinity y
  | isNaN y = (raise INVALID; y)
  | otherwise = y

pseudoMin -Infinity y
  | isNaN y = (raise INVALID; y)
  | otherwise = -Infinity
```

`a` がNaNの場合は簡単で、これは常に第2引数を返す関数となります。

```haskell
pseudoMin NaN y = (raise INVALID; y)
```

ということで、`a` の場合に応じた（例外を無視した）等式は次のようになります：

```
pseudoMin Infinity y = y
pseudoMin NaN y = y
not (isNaN a || isNegativeZero a) && not (isSignaling y) => pseudoMin a y = minimum a y
```

### pseudoMinの第二引数を固定する

`b` を0でもNaNでもない数または無限大とします。

```haskell
pseudoMin x b
  | isNaN x = (raise INVALID; b)
  | otherwise = minimum x b
```

これはNaNを伝播しないminということで、例外を無視すればIEEE 754-2019のminimumNumber演算と一致しそうです。

`b` が符号付き0の場合はどうでしょうか。

```haskell
pseudoMin x +0.0
  | isNaN x = (raise INVALID; +0.0)
  | x == 0.0 = +0.0
  | otherwise = minimum x +0.0

pseudoMin x -0.0
  | isNaN x = (raise INVALID; -0.0)
  | x == 0.0 = -0.0
  | otherwise = minimum x -0.0
```

`b` が `-0.0` の場合は、やはり例外を無視すればminimumNumber演算と一致しそうです。一方で、`+0.0` の場合は `x` が `-0.0` の場合にminimumNumberと一致しません。

`b` が無限大の場合も考えておきます。

```haskell
pseudoMin x Infinity
  | isNaN x = (raise INVALID; Infinity)
  | otherwise = x

pseudoMin x -Infinity
  | isNaN x = (raise INVALID; -Infinity)
  | otherwise = -Infinity
```

`b` がNaNの場合は簡単で、これは常にNaNを返す関数となります。

```haskell
pseudoMin x b@NaN = (raise INVALID; b)
```

ということで、`b` の場合に応じた（例外を無視した）等式は次のようになります：

```
pseudoMin x -Infinity = -Infinity
pseudoMin x b@NaN = b
not (isNaN b || isPositiveZero b) => pseudoMin x b = minimumNumber x b
```

### pseudoMaxの第一引数を固定する

`a` を0でもNaNでもない数または無限大とします。

```haskell
pseudoMax a y
  | isNaN y = (raise INVALID; y)
  | otherwise = maximum a y
```

これはNaNを伝播するmaxということで、例外とsignaling NaNを無視すればIEEE 754-2019のmaximum演算と一致しそうです。

`a` が符号付き0の場合はどうでしょうか。

```haskell
pseudoMax +0.0 y
  | isNaN y = (raise INVALID; y)
  | y == 0.0 = y
  | otherwise = maximum +0.0 y

pseudoMax -0.0 y
  | isNaN y = (raise INVALID; y)
  | y == 0.0 = y
  | otherwise = maximum -0.0 y
```

`a` が `-0.0` の場合は、やはり例外とsignaling NaNを無視すればmaximum演算と一致しそうです。一方で、`+0.0` の場合は `y` が `-0.0` の場合にmaximumと一致しません。具体的には

```
pseudoMax +0.0 -0.0 = -0.0
maximum +0.0 -0.0 = +0.0
```

となります。

`a` が無限大の場合も考えておきます。

```haskell
pseudoMax Infinity y
  | isNaN y = (raise INVALID; y)
  | otherwise = Infinity

pseudoMax -Infinity y
  | isNaN y = (raise INVALID; y)
  | otherwise = y
```

`a` がNaNの場合は簡単で、これは常に第2引数を返す関数となります。

```haskell
pseudoMax NaN y = (raise INVALID; y)
```

ということで、`a` の場合に応じた（例外を無視した）等式は次のようになります：

```
pseudoMax -Infinity y = y
pseudoMax NaN y = y
not (isNaN a || isPositiveZero a) && not (isSignaling y) => pseudoMax a y = maximum a y
```

### pseudoMaxの第二引数を固定する

`b` を0でもNaNでもない数または無限大とします。

```haskell
pseudoMax x b
  | isNaN x = (raise INVALID; b)
  | otherwise = maximum x b
```

これはNaNを伝播しないmaxということで、例外を無視すればIEEE 754-2019のmaximumNumber演算と一致しそうです。

`b` が符号付き0の場合はどうでしょうか。

```haskell
pseudoMax x +0.0
  | isNaN x = (raise INVALID; +0.0)
  | x == 0.0 = +0.0
  | otherwise = maximum x +0.0

pseudoMax x -0.0
  | isNaN x = (raise INVALID; -0.0)
  | x == 0.0 = -0.0
  | otherwise = maximum x -0.0
```

`b` が `+0.0` の場合は、やはり例外を無視すればmaximumNumber演算と一致しそうです。一方で、`-0.0` の場合は `x` が `+0.0` の場合にmaximumNumberと一致しません。

`b` が無限大の場合も考えておきます。

```haskell
pseudoMax x Infinity
  | isNaN x = (raise INVALID; Infinity)
  | otherwise = Infinity

pseudoMax x -Infinity
  | isNaN x = (raise INVALID; -Infinity)
  | otherwise = x
```

`b` がNaNの場合は簡単で、これは常にNaNを返す関数となります。

```haskell
pseudoMax x b@NaN = (raise INVALID; b)
```

ということで、`b` の場合に応じた（例外を無視した）等式は次のようになります：

```
pseudoMax x Infinity = Infinity
pseudoMax x b@NaN = b
not (isNaN b || isNegativeZero b) => pseudoMax x b = maximumNumber x b
```

### IEEE 754-2019 minimum/maximumの書き換え

`a` がNaNでも `-0.0` でもない場合は、IEEE 754-2019のminimumについて

```
minimum a x = minimum x a = 1 * pseudoMin a x = pseudoMin a (1 * x)
```

という等式が成り立ちます（例外は無視）。

`a` がNaNでも `+0.0` でもない場合は、IEEE 754-2019のmaximumについて

```
maximum a x = maximum x a = 1 * pseudoMax a x = pseudoMax a (1 * x)
```

という等式が成り立ちます（例外は無視）。

「例外は無視」と書いたのは、pseudoMin/Maxでは片方のオペランドがNaNの場合にINVALID例外が発生するからです。minimum/maximumでは引数にsignaling NaNが含まれない限り例外は発生しません（つまり、quiet NaNと数、あるいはquiet NaN同士の場合に違いが出る）。

minimum/maximumでは、引数がsignaling NaNだった場合はINVALID例外が発生して（デフォルトの処理方法の場合）quiet NaNを返します。pseudoMin/Maxはsignaling NaNをそのまま返すので、右辺ではsignaling NaNをquiet NaNに変換するために `1 *` を使っています（参照：[浮動小数点数に1を掛ける操作は最適化できるか／浮動小数点数のビット列表現のカノニカル性について](floating-point-canonicity)）。

### IEEE 754-2019 minimumNumber/maximumNumberの書き換え

`a` がNaNでも `+0.0` でもない場合は、IEEE 754-2019のminimumNumberについて

```
minimumNumber a x = minimumNumber x a = pseudoMin x a
```

という等式が成り立ちます（例外は無視）。

`a` がNaNでも `-0.0` でもない場合は、IEEE 754-2019のmaximumNumberについて

```
maximumNumber a x = maximumNumber x a = pseudoMax x a
```

という等式が成り立ちます（例外は無視）。

「例外は無視」と書いたのは、pseudoMin/Maxでは片方のオペランドがNaNの場合にINVALID例外が発生するからです。minimumNumber/maximumNumberでは引数にsignaling NaNが含まれない限り例外は発生しません（つまり、quiet NaNと数、あるいはquiet NaN同士の場合に違いが出る）。

### pseudoMin/pseudoMaxの書き換え

AArch64ではIEEE 754-2019準拠の命令が利用できたりするので、pseudoMin/Maxを書き換える動機があります。まあpseudoMin/Maxはそのままでも2命令で済むはずですが。

すでに書きましたが、次のようになります：

```
pseudoMin Infinity y = y
pseudoMin NaN y = y
not (isNaN a || isNegativeZero a) && not (isSignaling y) => pseudoMin a y = minimum a y
```

```
pseudoMin x -Infinity = -Infinity
pseudoMin x b@NaN = b
not (isNaN b || isPositiveZero b) => pseudoMin x b = minimumNumber x b
```

```
pseudoMax -Infinity y = y
pseudoMax NaN y = y
not (isNaN a || isPositiveZero a) && not (isSignaling y) => pseudoMax a y = maximum a y
```

```
pseudoMax x Infinity = Infinity
pseudoMax x b@NaN = b
not (isNaN b || isNegativeZero b) => pseudoMax x b = maximumNumber x b
```

「実行時に判明する入力がsignaling NaNではない」という条件が厄介ですが、他の（符号ビットだけを操作する系のものを除く）浮動小数点演算の結果として生成される浮動小数点データはsignaling NaNではないことが保証されているので、他の演算の結果であることが静的に判明していれば変換して構いません。あるいは、結果をsignaling性を観測することなく他の浮動小数点演算に与えるのであれば大丈夫でしょう。

AArch64ではminimumNumber/maximumNumberも2命令〜3命令必要なので、それらに書き換える意義は薄いかもしれません。

---

小ネタでした。浮動小数点数をいい感じに扱うコンパイラーを書くときに役立ててください。
