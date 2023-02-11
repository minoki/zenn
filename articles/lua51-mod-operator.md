---
title: "Lua 5.1/5.2/LuaJITの%演算子と浮動小数点数の罠"
emoji: "🦁"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["lua", "float", "浮動小数点数"]
published: true
---

# Luaの `%` 演算子

整数型が導入される以前のLua（Lua 5.1/5.2/LuaJIT）の `%` 演算子は次のように定義されています：

```lua
a % b == a - math.floor(a/b)*b
```

* [2.5.1 – Arithmetic Operators - Lua 5.1 Reference Manual](http://www.lua.org/manual/5.1/manual.html#2.5.1)
* [3.4.1 – Arithmetic Operators - Lua 5.2 Reference Manual](http://www.lua.org/manual/5.2/manual.html#3.4.1)

これはおおよそflooring divisionに対する剰余を返します。[整数除算の二つの流儀](integer-divison)で言うところのmodです。

しかし、この定義は `math.floor` という語彙を使っていることから察せられるように、浮動小数点数演算を使っています。その結果、整数同士の剰余

```lua
(-9007199254740992) % 3
```

に対して間違った答え `0` を返してしまいます。`-9007199254740992` は$-2^{53}$で、

$$-2^{53}\equiv -(-1)^{53}\equiv 1\mod{3}$$

と計算できて正しい（浮動小数点数を使わない）答えは `1` です。実際、浮動小数点数を使わないLua 5.3で同じ計算を実行すると `1` が得られます。

というわけで、Lua 5.1/5.2/LuaJITの `%` で絶対値が大きな整数の剰余を計算するのはやばそうだということがわかります。Lua 5.1や5.2の開発はもう終了していますし、LuaJITはLua 5.1準拠なので、この挙動が「修正」されることはないでしょう。仕様バグです。

# 安全に使える十分条件

Luaの `%` 演算子が全く使えないかというとそんなことはなく、絶対値が大きくないところだとか、`%` の右側が2の累乗だったりすれば安全に使える気がします。確かめてみましょう。

整数$n$, $d$について `n % d` の計算を考えます。$n$, $d$の絶対値の範囲は$\lvert n\rvert\le 2^{53}$, $1\le\lvert d\rvert\le 2^{53}$とします。

実数$\mathbf{R}$から倍精度浮動小数点数$\mathbf{F}$へ最近接丸めを行う関数を$R\colon\mathbf{R}\to\mathbf{F}$とします。

Luaの `%` 演算子の定義を$R$を使って書いたものは

$$
n\mathbin{\%}d:=R(n-R(\lfloor R(n/d)\rfloor\cdot d))
$$

となりますが、

* [整数除算を浮動小数点演算でエミュレートできるか](https://blog.miz-ar.info/2023/01/emulating-integer-division-with-floating-point-division/)

に書いたようにこの$n$と$d$の範囲では$\lfloor R(n/d)\rfloor$は$\lfloor n/d\rfloor$に一致します。また、浮動小数点数のfloor $\lfloor\cdot\rfloor$は浮動小数点数として正確に表現できます（なので$\lfloor\cdot\rfloor$の周りに$R(\cdot)$は書いていません）。

ということで、

$$
n\mathbin{\%}d=R(n-R(\lfloor n/d\rfloor\cdot d))
$$

という風に、丸めが問題になりうるのは掛け算の際と引き算の際の2回です。

仮に掛け算の正確な値（整数値）が浮動小数点数として正確に表現可能であれば

$$
R(n-R(\lfloor n/d\rfloor\cdot d))=R(n-\lfloor n/d\rfloor\cdot d)
$$

となり、$n-\lfloor n/d\rfloor\cdot d$は絶対値が$d$未満なので浮動小数点数として正確に表現可能で、

$$
n\mathbin{\%}d=n-\lfloor n/d\rfloor\cdot d
$$

を得ます。よって、「積$\lfloor n/d\rfloor\cdot d$が浮動小数点数として正確に表現できるのはどのような時か」が焦点となります。

## nとdが同符号の場合

$n/d\ge 0$の場合は$0\le\lfloor n/d\rfloor\le n/d$なので、$0\le\bigl\lvert\lfloor n/d\rfloor\cdot d\bigr\rvert\le \lvert n\rvert$です。$\lvert n\rvert\le 2^{53}$と仮定しているので、整数$\lvert\lfloor n/d\rfloor\cdot d\rvert$は浮動小数点数で正確に表現できます。

よって、$n$と$d$が同符号の場合はLuaの `%` 演算子を安全に使えます。

## nの絶対値がd以下の場合

今度は$n$と$d$は異符号であること（$n/d<0$）と、$n$の絶対値が$d$の絶対値以下であること（$\lvert n\rvert\le\lvert d\rvert$）を仮定します。

この時、$\lfloor n/d\rfloor=-1$です。よって、積$\lfloor n/d\rfloor\cdot d$は浮動小数点数で正確に表現できます。

## nの絶対値がdよりも大きいが、大きすぎない場合

今度は$n$と$d$は異符号であること（$n/d<0$）と、$n$の絶対値が$d$の絶対値よりも大きいこと（$\lvert d\rvert<\lvert n\rvert$）を仮定します。

$\lfloor n/d\rfloor\cdot d$はおおよそ$n$付近であろうということは予想されるので、$n$の絶対値が大きすぎなければ$\lfloor n/d\rfloor\cdot d$は浮動小数点数として正確に表現可能でしょう。

まず、床関数の定義（と異符号であること）より

$$
n/d-1<\lfloor n/d\rfloor\le n/d<0
$$

です。絶対値を取って

$$
\lvert n/d\rvert\le\bigl\lvert\lfloor n/d\rfloor\bigr\rvert<\lvert n/d-1\rvert
$$

で、それぞれ$\lvert d\rvert$をかけて

$$
\lvert n\rvert\le\bigl\lvert\lfloor n/d\rfloor\cdot d\bigr\rvert<\lvert n-d\rvert
$$

を得ます。よって、$\lfloor n/d\rfloor\cdot d$の絶対値は最大で$\lvert n-d\rvert-1$程度になり得ます。$n$と$d$が異符号であることを考慮すると$\bigl\lvert\lfloor n/d\rfloor\cdot d\bigr\rvert$の上限は$\lvert n\rvert+\lvert d\rvert-1$になります。

最初に$\lvert d\rvert<\lvert n\rvert$すなわち$\lvert d\rvert\le\lvert n\rvert-1$を仮定したので、$\lvert n\rvert+\lvert d\rvert-1\le 2\lvert n\rvert-2$となります。

よって、$2\lvert n\rvert-2\le 2^{53}$であれば安全に `%` 演算子を使えます。この$n$の範囲は$-(2^{52}+1)\le n\le 2^{52}+1$です。$n$の絶対値が$d$よりも小さい場合はさっき安全だとわかったので、この結論に関して$n$の絶対値の下限を設定する必要はありません。

## dが2の累乗である場合

$\lvert d\rvert$が2の累乗$d=2^k$である場合は「$d$倍」は正確に計算できます。ここでは$n$も$d$もそこまで大きくないので、オーバーフローの心配はありません。$k$の範囲は$0\le k\le 53$です。

よって$\lvert d\rvert=2^k$ ($0\le k\le 53$)の場合は安全に `%` 演算子を使えます。偶数かどうかの判定が `n % 2 == 0` でできるのは嬉しいですね。

## 十分条件まとめ

絶対値が$2^{53}$以下の整数$n$, $d$について以下のいずれかが成り立てば、 `%` を安全に使えます：

* $n$と$d$が同符号
* $\lvert n\rvert\le\lvert d\rvert$
* $\lvert n\rvert\le 2^{52}+1$
* $\lvert d\rvert=2^k$ ($0\le k\le 53$)

# 回避策

一般の場合は、`%` 演算子を安全に使うことはできません。そこで、別の方法で剰余を計算することを考えます。

Luaの場合はtruncating divisionに関する剰余（[整数除算の二つの流儀](integer-divison)でいうrem）が `math.fmod` で利用できるので、[整数除算の二つの流儀](integer-divison)に書いた方法でmodをエミュレートすれば良いです。コード例を以下に載せます：

```lua
function mod(n, d)
  assert(d != 0)
  r = math.fmod(n, d)
  if r == 0 or n * d >= 0 then
    return r
  else
    return r + d
  end
end
```

コード例では、同符号かどうかの確認は `n * d >= 0` で行いました。

# 関連記事

* [整数除算を浮動小数点演算でエミュレートできるか](https://blog.miz-ar.info/2023/01/emulating-integer-division-with-floating-point-division/)
* [整数除算の二つの流儀](integer-divison)
* [Luaの数値型](lua-number-types)
