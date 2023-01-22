---
title: "IEEE 754の十進浮動小数点数の基本"
emoji: "🌟"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c言語", "float", "浮動小数点数"]
published: true
---

# 浮動小数点形式

IEEE 754で規定された浮動小数点形式は、基数$b$, 指数部の最大値$\mathit{emax}$, 精度$p$の3つのパラメーターで表されます。このうち基数$b$は2または10で、この記事で扱うのは$b=10$の場合です。

指数部の最小値$\mathit{emin}$は$1-\mathit{emax}$と定められています。

浮動小数点数として表現できる実数は、$0\le M<b^p$を満たす整数$M$（仮数部）および$\mathit{emin}-p+1\le e\le\mathit{emax}-p+1$を満たす整数$e$（指数部）および$0\le s\le 1$を満たす整数$s$（符号）について

$$
(-1)^s\times M\times b^{e}
$$

と表現できるものです。

$M$を範囲$b^{p-1}\le M<b^p$に収まるようにしたときに指数部が範囲に収まるものを**正規化数**と呼びます。

例えば$b=10$, $p=3$となる形式において$1\times 10^0$は仮数部の精度を使い切っていませんが、$100\times 10^{-2}$と表示した時に指数部が範囲に収まっているので正規化数です。

二進形式の場合は正規化数に対して$M$の範囲を$b^{p-1}\le M<b^p$に収まるようにして表現を一意的にしたりしますが、十進形式の場合はそういうのは行いません。つまり、同じ数に対して複数の表現があり得ます。例えば$(s,M,e)=(0,1,0),(0,100,-2)$は同じ数$1$に対する異なる2つの表現です。

（符号ビットも含めて）同じ数を表す表現の集合をその数の**cohort**と呼びます。$b=10$, $p=3$となる形式において1のcohortは$\{(0,1,0),(0,10,-1),(0,100,-2)\}$です。+0のcohortは$\{(0,0,e)\mid \mathit{emin}-p+1\le e\le\mathit{emax}-p+1\}$で、-0のcohortは$\{(1,0,e)\mid \mathit{emin}-p+1\le e\le\mathit{emax}-p+1\}$です。3.14のcohortは$\{(0,314,-2)\}$です。

同じ数に対する複数の表現（$1\times 10^0$ vs $1.00\times 10^0$）というのは有効数字と似ていますが、浮動小数点数の世界では$1\times 10^0+1\times 10^{-2}=1.01\times 10^0$となるので、有効数字とは似て非なる概念です。

# ビット列での表現

IEEE 754では、いくつかの浮動小数点形式に対してビット列での表現を定めています。これらの形式のことを**交換形式** (interchange format)と呼びます。

十進交換形式のビット列による表現は、二進交換形式の場合と同様に、おおまかに3つの部分に分かれます：

| ←上位 | 符号$S$ | 合成フィールド$G$ | 仮数部$T$ | 下位→ |
|-|-|-|-|-|
| | 1ビット | $w+5$ビット | $t$ビット |

よく使われる十進交換形式は、以下の3つです：

| | decimal32 | decimal64 | decimal128 |
|-|-|-|-|
| ビット幅$k$ | 32 | 64 | 128 |
| 精度（桁数）$p$ | 7 | 16 | 34 |
| 指数部の最大値$\mathit{emax}$ | 96 | 384 | 6144 |
| 指数部のバイアス | 101 | 398 | 6176 |
| 符号ビット | 1 | 1 | 1 |
| 合成フィールドの幅（ビット数）$(w+5)$ | 11 | 13 | 17 |
| 仮数部の幅（ビット数）$t$ | 20 | 50 | 110 |

IEEE 754では、十進交換形式のビット列へのエンコード方法を**十進エンコーディング** (decimal encoding)と**二進エンコーディング** (binary encoding)の二種類規定しています。これらは互換性がないので、プラットフォームのABIや使用するプロトコルがどちらかを決めなければなりません。

十進エンコーディングは仮数部を**DPD** (densely packed decimal)と呼ばれる**10ビットに十進3桁を詰め込む**方式で表現します。10ビット=1024通りと十進3桁=1000通りが近いので（古のBCD (binary-coded decimal)と比べて）効率的、ということのようです。

二進エンコーディングは仮数部を二進整数で表現します。**BID** (binary integer decimal)と呼ばれることもあります。

いずれのエンコーディングでも、浮動小数点数の表現$(s,M,e)$とビット列は一対一に対応しません。どうしてもビット列の方が余分になってしまいます。IEEE 754では、一つの表現に対応するビット列は一般に複数あることにして、そのうちの一つを**canonicalなエンコーディング**と定めています。ほとんどの浮動小数点演算で生成されるビット列はcanonicalです（canonicalではない結果を返す可能性のある演算は、符号ビットのみを操作する系です）。

それぞれの具体的なエンコード方法はここでは解説しません。IEEE 754を読んでください。

# ハードウェア実装とABI

IBM POWERシリーズやZシリーズはハードウェアでIEEE 754の十進浮動小数点形式をサポートしているらしいです。エンコーディングは十進（DPD）らしいです。

他の環境では、ソフトウェア実装を使うことになります。

ABI的には、POWER向けのABIでは十進（DPD）、x86\_64向けのSystem V ABIや[AArch64の標準呼び出し規約](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst#53decimal-floating-point)
では二進（BID）となっているようです。

RISC-Vでは "L" 拡張が十進浮動小数点数用ということになっていますが、レジスターを既存の浮動小数点数用のものと共用すること以外何も決まっていないようです。エンコーディングが十進か二進かすら、[ABIの文書](https://github.com/riscv-non-isa/riscv-elf-psabi-doc/blob/master/riscv-cc.adoc)を見る限りまだ決まってなさそうです。

ハードウェア実装はともかく、せめて十進エンコーディングか二進エンコーディングかを決めてもらわないとソフトウェア実装すら使えないのは厄介なところです。

# C言語での扱い

今年出るC言語の規格**C23**では十進浮動小数点数のサポートが盛り込まれます。ただ、例によってオプショナルな機能という扱いで、処理系がマクロ `__STDC_IEC_60559_DFP__` を定義している場合に使えることになっています。

提供される型の名前は `_Decimal32`, `_Decimal64`, `_Decimal128` です。処理系がさらに `__STDC_IEC_60559_TYPES__` を定義している場合は他の `_Decimal〈N〉` も使えるかもしれません。

リテラルのサフィックスは `d〈N〉` ですが、`_Decimal32`, `_Decimal64`, `_Decimal128` に対しては `df`, `dd`, `dl`, `DF`, `DD`, `DL` も使えます。2文字目は `float`/`double`/`long double` を意識したものだと思われます。十六進リテラルは使えません。

複素数には対応しません。

入出力のフォーマットの際の長さ修飾子 (length modifier)は

* `H`: `_Decimal32`
* `D`: `_Decimal64`
* `DD`: `_Decimal128`

です。変換指定子 (conversion specifier)は `a`, `A`, `e`, `E`, `f`, `F`, `g`, `G` のいずれかに対応します。

十進型に対する各種関数を使うには、ヘッダーを `#include` する前に `__STDC_WANT_IEC_60559_TYPES_EXT__` を定義しておく必要があります。

数学関数のサフィックスは `d〈N〉` です。例えば

```c
#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <math.h>
_Decimal32 sind32(_Decimal32 x);
_Decimal64 sind64(_Decimal64 x);
_Decimal128 sind128(_Decimal128 x);
```

という具合です。

最近のGCCはいくつかのターゲットで十進浮動小数点数型をサポートしています：

* [Decimal Float (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-12.2.0/gcc/Decimal-Float.html#Decimal-Float)

が、主要なlibc実装はまだ十進浮動小数点数をサポートしていないようです（IBMのプラットフォーム用のやつだと違うのかもしれない）。

# IEEE以外の十進浮動小数点数

プログラミング環境によっては、IEEE 754とは無関係な十進浮動小数点数を提供していることがあります。例をいくつか挙げます。

## Microsoft

OLEとか.NETで `Decimal` 型が使えます。

* [\[MS-OAUT\]: DECIMAL | Microsoft Learn](https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-oaut/b5493025-e447-4109-93a8-ac29c48d018d)
* [Decimal Struct (System) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.decimal?view=netcore-3.1)

表現できる数は$(-1)^{\mathit{sign}}\times M\times 10^{-\mathit{scale}}$の形をしており、仮数部は96ビット ($0\le M<2^{96}$), 指数部は$0\le\mathit{scale}\le 28$です。

## Java

Javaは任意精度の `BigDecimal` 型を提供しています。負のゼロ、無限大、NaNはありません。

* [BigDecimal (Java SE 19 & JDK 19)](https://docs.oracle.com/en/java/javase/19/docs/api/java.base/java/math/BigDecimal.html)

## Python

Pythonは任意精度の `decimal` モジュールを提供しています。負のゼロ、無限大、NaNを含みます。

* [decimal — Decimal fixed point and floating point arithmetic — Python 3.11.1 documentation](https://docs.python.org/3/library/decimal.html)
