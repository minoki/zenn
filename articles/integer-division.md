---
title: "整数除算の二つの流儀"
emoji: "🌊"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["int", "math"]
published: true
---

# 整数除算の流儀

整数除算は割り切れなかった時の商の扱い方によって何種類かに分類でき、商を0に向かって切り捨てるものと、$-\infty$に向かって切り下げる（床関数）ものの2種類がよく使われます。この記事ではこれらの関係を見ていきます。

ここではquot, rem, div, modを次のように定めます。ただし、$\mathrm{trunc}(x)$は$x$と同じ符号を持ち、絶対値が$\lvert x\rvert$を超えない最大の整数であるような整数です。

$$
\begin{aligned}
\mathrm{quot}(n,d)&:=\mathrm{trunc}(n/d), \\
\mathrm{rem}(n,d)&:=n-\mathrm{quot}(n,d)\cdot d, \\
\mathrm{div}(n,d)&:=\lfloor n/d\rfloor, \\
\mathrm{mod}(n,d)&:=n-\mathrm{div}(n,d)\cdot d
\end{aligned}
$$

quotとdivは商を返す関数、remとmodは余りを返す関数です。ここでの名前は（後述しますが）Standard MLとHaskellの流儀に沿ったものです。

余りの範囲は

$$
\begin{aligned}
0&\le\mathrm{rem}(n,d)<\lvert d\rvert & \text{if \(n\ge 0\),} \\
-\lvert d\rvert&<\mathrm{rem}(n,d)\le 0 & \text{if \(n\le 0\),} \\
0&\le\mathrm{mod}(n,d)<d & \text{if \(d>0\),} \\
d&<\mathrm{mod}(n,d)\le 0 & \text{if \(d<0\)}
\end{aligned}
$$

となります。$\mathrm{mod}(n,d)$の符号は$d$と同じ（または0）で、$\mathrm{rem}(n,d)$は$n$と同じ符号（または0）です。

# 色々な言語での整数除算

いくつかの言語について、整数除算がどちらの流儀を採用しているかまとめてみました。

| 言語 | quot / rem | div / mod |
|-|-|-|
| C | `/`, `%`, `div` | |
| Java | `/`, `%` | |
| C# | `/`, `%` | |
| JavaScript (BigInt) | `/`, `%` | |
| Python | | `//`, `%` |
| Lua 5.3 | | `//`, `%` |
| Standard ML | `quot`, `rem` | `div`, `mod` |
| OCaml | `/`, `mod` | |
| Haskell | `quot`, `rem` | `div`, `mod` |

C系の言語はquot / rem型が多いです。Standard MLとHaskellは両方提供しています。

# 両者の関係

$n/d\ge 0$または$d\mid n$（$d$が$n$を割り切る）の場合はdiv, quotとrem, modはそれぞれ一致します。

$n/d<0$で$d\nmid n$（$d$が$n$を割り切らない）の場合は$\lfloor n/d\rfloor=\mathrm{trunc}(n/d)-1$が成り立ちます。よって、

$$
\begin{aligned}
\mathrm{div}(n,d)&=\mathrm{quot}(n,d)-1, \\
\mathrm{mod}(n,d)%&=n-(\mathrm{quot}(n,d)-1)\cdot d \\
&=\mathrm{rem}(n,d)+d
\end{aligned}
$$

という関係となります。

# quot / remでdiv / modを実装する

C系の言語でdiv / modの挙動が欲しくなったとします。divとmodを一気に計算する関数は次のように実装できます：

```c
div_t divMod(int n, int d)
{
    assert(d != 0);
    assert(n != INT_MIN || d != -1); // 2の補数表現の場合、オーバーフローを起こす可能性がある
    int q = n / d;
    int r = n % d;
    if ((n >= 0 && d > 0) || (n <= 0 && d < 0) || r == 0) {
        return (div_t){.quot = q, .rem = r};
    } else {
        return (div_t){.quot = q - 1, .rem = r + d};
    }
}
```

条件 `(n >= 0 && d > 0) || (n <= 0 && d < 0)` がやや複雑です。符号が同じ（または0）ということなので、ビット演算を使って `(n ^ d) >= 0` とできるかもしれません。（[Luaの実装](https://github.com/lua/lua/blob/cf08915d62e338c987b71c078b148490510e9fe7/lvm.c#L714-L752)では実際にビット演算を使っているようです。）

# div / modでquot / remを実装する

div / modがプリミティブとして用意されている言語でquot / remを一気に計算する関数は次のように実装できます：

```python
def quotRem(n: int, d: int) -> tuple[int, int]:
    assert d != 0
    # 固定長整数の場合はオーバーフローの可能性がある
    q = n // d
    r = n % d
    if (n >= 0 and d > 0) or (n <= 0 and d < 0) or r == 0:
        return q, r
    else:
        return q + 1, r - d
```

# 他の流儀

ここでは商を0に向かって切り捨てるものと床関数で計算するものを扱いましたが、他にも「$+\infty$に向かって切り上げる（天井関数）」、「最も近い整数に丸める（最近接偶数丸め）」などのバリエーションが考えられます。さらに、以下の論文

* Raymond T. Boute. 1992. The Euclidean definition of the functions div and mod. ACM Trans. Program. Lang. Syst. 14, 2 (April 1992), 127–144. https://doi.org/10.1145/128861.128862

では「余りの絶対値が常に非負になるようにする」流儀をユークリッド除算 (Euclidean division) と呼んで、それを推しています。

Scheme界隈の

* [SRFI 141: Integer division](https://srfi.schemers.org/srfi-141/srfi-141.html)

では、上記の5つの他に、「余りが$-\lvert d/2\rvert\le r<\lvert d/2\rvert$を満たす」流儀（「商を最も近い整数に丸め」つつ、等距離の場合に余りを負に取る）を提案しています。
