---
title: "浮動小数点演算の結果が環境依存なのはどんなとき？"
emoji: "✨"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c"]
published: false
---

C言語の場合。

# データ型

大抵の環境では

* `float` 型はIEEE binary32
* `double` 型はIEEE binary64

です。`long double` 型の精度は環境依存が激しいのでポータブルな結果を得たい場合は避けましょう。

NaN

# 四則演算

* `+`, `-`, `*`, `/`

環境によってはflush-to-zeroが有効になっていて

x87 FPUの場合。

C言語の場合。

FMAが勝手に使われる場合。

# 数学関数

* `sqrt`, `remainder`
* `rint`, `lrint`, `llrint`
* conversions
* `isgreater`, `isgreaterequal`, `isless`, `islessequal`, `islessgreater`, `isunordered`
* `copysign`, `fabs`, unary `-`, `scalbn`, `scalbln`, `logb`, `nextafter`, `nexttoward`
* `isfinite`, `isnan`, `signbit`, `fpclassify`

十進小数からの変換は桁数が多すぎると危ないかも。

十進小数への文字列化？

変わりそうなやつ

- `acos`, `acosf`, `acosl`
- `asin`
- `atan`
- `atan2`
- `cos`
- `sin`
- `tan`
- `acosh`
- `asinh`
- `atanh`
- `cosh`
- `sinh`
- `tanh`
- `exp`
- `exp2`
- `expm1`
- `log`
- `log10`
- `log1p`
- `log2`
- `logb`
- `frexp`?
- `modf`?
- `scalbn`, `scalbln`?
- `cbrt`
- `fabs`?
- `hypot`
- `pow`
- `sqrt`
- `erf`
- `erfc`
- `lgamma`
- `tgamma`
- `ceil`
- `floor`
- `nearbyint`
- `rint`
- `lrint`, `llrint`
- `round`, `lround`, `llround`
- `trunc`
- `fmod`
- `remainder`
- `remquo`
- `copysign`
- `nan`
- `nextafter`
- `nexttoward`
- `fdim`
- `fmax`
- `fmin`
- `fma`

# 命令セット的な話

## 同じバイナリーを実行した時の結果は同じか？

## 同じ命令列を実行した時の結果は同じか？
