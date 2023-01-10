---
title: "浮動小数点数の丸めの相対誤差を計算機イプシロンで評価する"
emoji: "📘"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["float", "浮動小数点数"]
published: true
---

実数を浮動小数点数に丸めるときに相対誤差がどのくらいになるかを考えます。実数を浮動小数点数に丸める関数を$R\colon\mathbf{R}\rightarrow\mathbf{F}$とした時に

$$
\left|\frac{R(x)-x}{x}\right|
$$

あるいは

$$
\left|\frac{R(x)-x}{R(x)}\right|
$$

を評価したいです。

ここでは浮動小数点数の基数は$b$、精度は$p$桁とし、指数部の範囲は無制限とします。

区間$b^i\le \lvert x\rvert\le b^{i+1}$での浮動小数点数の刻み幅は$b^{i-p+1}$となります。この刻み幅を**ulp** (unit in last place) と呼びます。

丸め関数$R$は次の性質を満たすべきです：

* 単調である：$x\le y$の時、$R(x)\le R(y)$
* 元の値との差は1ulp未満である：$b^i\le \lvert x\rvert\le b^{i+1}$の時、$\lvert R(x)-x\rvert<b^{i-p+1}$
* $b^i$ の形の数を変えない：$R(\pm b^i)=\pm b^i$

丸め関数$R$が**最近接丸め**であるとは、さらに次の性質が成り立つことを言います：

* 元の値との差は0.5ulp以下である：$b^i\le \lvert x\rvert\le b^{i+1}$の時、$\lvert R(x)-x\rvert\le b^{i-p+1}/2$

丸め関数の例としては、以下があります：

* IEEE 754で規定された5種類
    * 最近接偶数丸め `roundTiesToEven`
    * 最近接丸め、ただし等距離の場合は絶対値が大きい方を選ぶ `roundTiesToAway`
    * $+\infty$方向への丸め `roundTowardPositive`
    * $-\infty$方向への丸め `roundTowardNegative`
    * 0への向かっての丸め（切り捨て） `roundTowardZero`
        * この記事ではこれを$R_{\mathrm{trunc}}$と書くことにします。
* 「0から離れる」丸め $R_{\mathrm{away}}$
    * 入力が正の時は `roundTowardPositive` と、入力が負の時は `roundTowardNegative` と一致するように定めます。

# 基本的な評価

丸め関数$R$による0でない実数$x$ ($b^i\le \lvert x\rvert\le b^{i+1}$)の丸めを考えます。丸め関数の性質を使うと、

$$
\left|\frac{R(x)-x}{x}\right|<\frac{b^{i-p+1}}{\lvert x\rvert}\le \frac{b^{i-p+1}}{b^i}=b^{-p+1}
$$

と評価できます。また、$b^i\le \lvert R(x)\rvert\le b^{i+1}$なので

$$
\left|\frac{R(x)-x}{R(x)}\right|<\frac{b^{i-p+1}}{\lvert R(x)\rvert}\le \frac{b^{i-p+1}}{b^i}=b^{-p+1}
$$

と評価できます。

$R$が最近接丸めの時（この時の$R$を$R_{\mathrm{nearest}}$と書くことにします）は$\lvert R_{\mathrm{nearest}}(x)-x\rvert\le b^{i-p+1}/2$なので、より良い評価ができます：

$$
\begin{gather*}
\left|\frac{R_{\mathrm{nearest}}(x)-x}{x}\right|\le\frac{b^{i-p+1}/2}{\lvert x\rvert}\le \frac{b^{i-p+1}}{2b^i}=\frac{b^{-p+1}}{2}, \\
\left|\frac{R_{\mathrm{nearest}}(x)-x}{R_{\mathrm{nearest}}(x)}\right|\le\frac{b^{i-p+1}/2}{\lvert R_{\mathrm{nearest}}(x)\rvert}\le \frac{b^{i-p+1}}{2b^i}=\frac{b^{-p+1}}{2}
\end{gather*}
$$

# 計算機イプシロン

評価の際に登場する$b^{-p+1}$のことを**計算機イプシロン**あるいは**マシンイプシロン** (machine epsilon) と呼びます。

この記事では計算機イプシロンを$\varepsilon_M:=b^{-p+1}$とおくことにします。

計算機イプシロンは浮動小数点数形式に依存して定まる量です。よく使われる倍精度浮動小数点数の場合は、$\varepsilon_M=2^{-52}$となります。

**計算機イプシロンは「最小の（正の）浮動小数点数」ではありません。** 倍精度の場合は$\varepsilon_M=2^{-52}$なのに対し、最小の正の浮動小数点数は$2^{-1074}$、最小の正の正規化数は$2^{-1022}$です。

計算機イプシロンは、「1の次に大きい浮動小数点数と1の差」として特徴づけることもできます。「`1<1+x` を満たす最小の `x`」とはちょっと違うので注意してください（`1+x` の計算で丸めが起こる可能性があるので）。

計算機イプシロンを使うと、前述の評価は

$$
\begin{gathered}
\left|\frac{R(x)-x}{x}\right|<\varepsilon_M=b^{-p+1}, \\
\left|\frac{R(x)-x}{R(x)}\right|<\varepsilon_M=b^{-p+1}, \\
\left|\frac{R_{\mathrm{nearest}}(x)-x}{x}\right|\le\frac{\varepsilon_M}{2}=\frac{b^{-p+1}}{2}, \\
\left|\frac{R_{\mathrm{nearest}}(x)-x}{R_{\mathrm{nearest}}(x)}\right|\le\frac{\varepsilon_M}{2}=\frac{b^{-p+1}}{2}
\end{gathered}
$$

と書くことができます。

# より精密な評価

具体的な丸め関数については、より精密な評価ができる場合があります。簡単のため、$x$が正の場合だけを考えます。

## 最近接丸め

$b^i\le x<b^i+b^{i-p+1}/2$の場合と$b^i+b^{i-p+1}/2\le x<b^{i+1}$の場合に分けて考えます。$b^{i-p+1}/2$というのはこの区間の0.5ulpに相当します。

$b^i\le x<b^i+b^{i-p+1}/2$の場合：$x=(1+\delta)b^i$とおきます。$0\le\delta<b^{-p+1}/2$です。この時、$R_{\mathrm{nearest}}(x)=b^i$です。すると相対誤差の評価は

$$
\left|\frac{R_{\mathrm{nearest}}(x)-x}{x}\right|=\frac{\lvert b^i-(1+\delta)b^i\rvert}{(1+\delta)b^i}=\frac{\delta}{1+\delta}<\frac{b^{-p+1}/2}{1+b^{-p+1}/2}
$$

とできます。ただし、関数$t\mapsto\frac{t}{1+t}$が$t>-1$で（狭義）単調増加であることを使いました。

$b^i+b^{i-p+1}/2\le x<b^{i+1}$の場合：

$$
\left|\frac{R_{\mathrm{nearest}}(x)-x}{x}\right|\le \frac{b^{i-p+1}/2}{\lvert x\rvert}\le\frac{b^{i-p+1}/2}{b^i+b^{i-p+1}/2}=\frac{b^{-p+1}/2}{1+b^{-p+1}/2}
$$

よって、最近接丸めに対しては

$$
\left|\frac{R_{\mathrm{nearest}}(x)-x}{x}\right|\le\frac{b^{-p+1}/2}{1+b^{-p+1}/2}=\frac{\varepsilon_M/2}{1+\varepsilon_M/2}
$$

という評価ができることがわかりました。

## 切り捨て

$b^i\le x<b^i+b^{i-p+1}$の場合と$b^i+b^{i-p+1}\le x<b^{i+1}$の場合に分けて考えます。$b^{i-p+1}$というのはこの区間の1ulpに相当します。

$b^i\le x<b^i+b^{i-p+1}$の場合：$x=(1+\delta)b^i$とおきます。$0\le\delta<b^{-p+1}$です。この時、$R_{\mathrm{trunc}}(x)=b^i$です。すると相対誤差の評価は

$$
\left|\frac{R_{\mathrm{trunc}}(x)-x}{x}\right|=\frac{\lvert b^i-(1+\delta)b^i\rvert}{(1+\delta)b^i}=\frac{\delta}{1+\delta}<\frac{b^{-p+1}}{1+b^{-p+1}}
$$

とできます。ただし、関数$t\mapsto\frac{t}{1+t}$が$t>-1$で（狭義）単調増加であることを使いました。

$b^i+b^{i-p+1}\le x<b^{i+1}$の場合：

$$
\left|\frac{R_{\mathrm{trunc}}(x)-x}{x}\right|<\frac{b^{i-p+1}}{\lvert x\rvert}\le\frac{b^{i-p+1}}{b^i+b^{i-p+1}}=\frac{b^{-p+1}}{1+b^{-p+1}}
$$

よって、切り捨てに対しては

$$
\left|\frac{R_{\mathrm{trunc}}(x)-x}{x}\right|<\frac{b^{-p+1}}{1+b^{-p+1}}=\frac{\varepsilon_M}{1+\varepsilon_M}
$$

という評価ができることがわかりました。

## 0から離れる丸め（絶対値の切り上げ）

$b^i<x\le b^i+b^{i-p+1}$の場合と$b^i+b^{i-p+1}<x\le b^{i+1}$の場合に分けて考えます。$b^{i-p+1}$というのはこの区間の1ulpに相当します。

$b^i<x\le b^i+b^{i-p+1}$の場合：$R_{\mathrm{away}}(x)=b^i+b^{i-p+1}$なので、

$$
\left|\frac{R_{\mathrm{away}}(x)-x}{R_{\mathrm{away}}(x)}\right|<\frac{b^{i-p+1}}{b^i+b^{i-p+1}}=\frac{b^{-p+1}}{1+b^{-p+1}}
$$

と評価できます。

$b^i+b^{i-p+1}<x\le b^{i+1}$の場合：$b^i+b^{i-p+1}<R_{\mathrm{away}}(x)\le b^{i+1}$なので、

$$
\left|\frac{R_{\mathrm{away}}(x)-x}{R_{\mathrm{away}}(x)}\right|<\frac{b^{i-p+1}}{\lvert R_{\mathrm{away}}(x)\rvert}<\frac{b^{i-p+1}}{b^i+b^{i-p+1}}=\frac{b^{-p+1}}{1+b^{-p+1}}
$$

と評価できます。

よって、$R_{\mathrm{away}}$については

$$
\left|\frac{R_{\mathrm{away}}(x)-x}{R_{\mathrm{away}}(x)}\right|<\frac{b^{-p+1}}{1+b^{-p+1}}
$$

と評価できることがわかりました。

# まとめ

計算機イプシロンは$\varepsilon_M:=b^{-p+1}$と定義される量です。

$\left|\frac{R(x)-x}{x}\right|$の評価は次のようになります：

$$
\left|\frac{R(x)-x}{x}\right|\begin{dcases}
<b^{-p+1}=\varepsilon_M & \text{(一般の場合)} \\
\le \frac{b^{-p+1}/2}{1+b^{-p+1}/2}=\frac{\varepsilon_M/2}{1+\varepsilon_M/2} & \text{(最近接丸めの場合)} \\
<\frac{b^{-p+1}}{1+b^{-p+1}}=\frac{\varepsilon_M}{1+\varepsilon_M} & \text{(切り捨ての場合)} \\
\end{dcases}
$$

$\left|\frac{R(x)-x}{R(x)}\right|$の評価は次のようになります：

$$
\left|\frac{R(x)-x}{R(x)}\right|\begin{dcases}
<b^{-p+1}=\varepsilon_M & \text{(一般の場合)} \\
\le \frac{b^{-p+1}}{2}=\frac{\varepsilon_M}{2} & \text{(最近接丸めの場合)} \\
<\frac{b^{-p+1}}{1+b^{-p+1}}=\frac{\varepsilon_M}{1+\varepsilon_M} & \text{(0から離れる場合)} \\
\end{dcases}
$$

これらの評価が最良であることは、次の場合を考えればわかります：

* 最近接丸め：$x=1+b^{-p+1}/2$
    * 等距離の場合の扱い次第では、$\le$を$<$にできるかもしれません。
* 切り捨て：$x=1+t$, $0<t<b^{-p+1}$, $t\to b^{-p+1}$
* 0から離れる：$x=1+t$, $0<t<b^{-p+1}$, $t\to 0$

「浮動小数点数の基本定理」を選ぶとしたらこれらの不等式は有力候補だと思いますが、どうでしょうか。

# 参考文献

ちゃんとした数値計算の教科書ならこういう話に多少は触れているはずです。手持ちの本では

* 大石 進一 編著「精度保証付き数値計算の基礎」コロナ社，2018年 <https://www.coronasha.co.jp/np/isbn/9784339028874/>
* 齊藤 宣一 著「数値解析入門」東京大学出版会，2012年 <http://www.utp.or.jp/book/b306462.html>

が該当しました。
