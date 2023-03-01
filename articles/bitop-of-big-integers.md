---
title: "多倍長整数のビット演算実装メモ"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["int", "多倍長整数"]
published: true
---

現代の環境では、符号付きの固定長整数は2の補数表現を使うことが多いです。ビット演算も2の補数表現に対して行われます。

一方、多倍長整数は符号と絶対値で表現することが多いかと思います。この時、多倍長整数のビット演算は「2の補数表現の拡張」として規定されることが多いわけですが、具体的にはどうすれば良いのでしょうか。

この記事では、多倍長整数のビット演算を

* 多倍長自然数に関するビット演算
* 多倍長整数に関する加減乗除

で表現することを目指します。

ビット演算の記号はC言語風のものを使います。

# 2の補数表現

幅 $M$ ビットの固定長整数型を考えます。ビット列が次のように並んでいる時（左端がMSB, 右端がLSB）、

$$
s v_{M-2} v_{M-3} \cdots v_2 v_1 v_0\quad (s, v_i\in\{0,1\})
$$

最上位ビット$s$を符号ビット、残りのビット$v_i$を値ビットと呼びます。2の補数表現では、このビット列が表す整数値$A$は

$$
A=-2^{M-1}\cdot s + \sum_{i=0}^{M-2} 2^i \cdot v_i
$$

です。

# ビット否定 (bitwise not)

ビットごとの否定（ビット反転、bitwise not, bitwise complement）は、それぞれのビットに$b\mapsto 1-b$を適用します。2の補数表現による整数$A$のビット否定を${\sim}A$と書くと、

$$
\begin{aligned}
{\sim}A&=-2^{M-1}\cdot (1-s) + \sum_{i=0}^{M-2} 2^i \cdot (1-v_i) \\
&=\left(-2^{M-1} + \sum_{i=0}^{M-2} 2^i\right)-\left(2^{M-1}\cdot s + \sum_{i=0}^{M-2} 2^i \cdot v_i\right) \\
&=-1-A \\
\end{aligned}
$$

となります。逆に

$$
{\sim}A:=-1-A
$$

と定義すれば、ビット否定を多倍長整数に一般化できます。

# ビット論理積 (bitwise and)

まず、自然数同士のビット論理積は既知とします。ここでは$A\ge 0$, $B\ge 0$のビット論理積を$A\mathbin{\&}B$と書くことにします。具体的には、$n$を十分大きい整数として

$$
A=\sum_{i=0}^{n} 2^i \cdot v_i, \quad
B=\sum_{i=0}^{n} 2^i \cdot v'_i
$$

の時

$$
A\mathbin{\&}B:=\sum_{i=0}^{n} 2^i \cdot v_i v'_i
$$

です。

非負整数をビット否定したものと非負整数のビット論理積も非負整数として定義できます：

$$
({\sim}A)\mathbin{\&}B:=\sum_{i=0}^{n} 2^i \cdot (1-v_i) v'_i
$$

負の整数$A<0$と非負整数$B\ge 0$のビット論理積を考えます。
この時、${\sim}A=-1-A$は非負整数です。すると

$$
A\mathbin{\&}B=({\sim}({\sim}A))\mathbin{\&}B=({\sim}(-1-A))\mathbin{\&}B
$$

となり、右辺は「非負整数をビット否定したものと非負整数のビット論理積」なので非負整数として計算できます。

$A\ge 0$, $B<0$の場合も同様です。

$$
A\mathbin{\&}B=A\mathbin{\&}({\sim}(-1-B))
$$

$A<0$, $B<0$の場合は、ビット論理積とビット論理和$X\mathbin{|}Y$についてド・モルガンの法則$({\sim}X)\mathbin{\&}({\sim}Y)={\sim}(X\mathbin{|}Y)$が成り立つことを使うと、

$$
\begin{aligned}
A\mathbin{\&}B&=({\sim}(-1-A))\mathbin{\&}({\sim}(-1-B)) \\
&={\sim}((-1-A)\mathbin{|}(-1-B)) \\
&=-1-((-1-A)\mathbin{|}(-1-B))
\end{aligned}
$$

と計算できます。

# ビット論理和 (bitwise or)

非負整数同士のビット論理和は既知とします。

負の整数$A<0$と非負整数$B\ge 0$のビット論理和は、ド・モルガンの法則により

$$
A\mathbin{|}B={\sim}(({\sim}A)\mathbin{\&}({\sim}B))=-1-((-1-A)\mathbin{\&}({\sim}B))
$$

と、「非負整数をビット否定したものと非負整数のビット論理積」を使って計算できます。

$A\ge 0$と$B<0$の場合も同様です：

$$
A\mathbin{|}B={\sim}(({\sim}A)\mathbin{\&}({\sim}B))=-1-(({\sim}A)\mathbin{\&}(-1-B))
$$

$A<0$と$B<0$の場合はド・モルガンの法則でビット論理積に帰着できます：

$$
A\mathbin{|}B={\sim}(({\sim}A)\mathbin{\&}({\sim}B))=-1-((-1-A)\mathbin{\&}(-1-B))
$$

# ビット排他的論理和 (bitwise xor)

非負整数同士のビットごとの排他的論理和$X\oplus Y$は既知とします。

また、非負整数$X$, $Y$について${\sim}(({\sim}X)\oplus Y)$は非負整数となります。これも既知とします。

さらに、任意の整数$A$, $B$に対して$A\oplus B={\sim}(({\sim}A)\oplus ({\sim}B))$が成り立つことを（固定長整数の拡張で）認めます。

負の整数$A<0$と非負整数$B\ge 0$のビット排他的論理和は、

$$
A\oplus B={\sim}({\sim}(({\sim}({\sim}A))\oplus B))=-1-{\sim}(({\sim}(-1-A))\oplus B)
$$

と計算できます。

$A\ge 0$, $B<0$の場合も同様に

$$
A\oplus B=-1-{\sim}(A\oplus ({\sim}(-1-B)))
$$

と計算できます。

$A<0$, $B<0$の場合は

$$
A\oplus B={\sim}(({\sim}A)\oplus ({\sim}B))=-1-((-1-A)\oplus (-1-B))
$$

と計算できます。

# 左シフト

整数$A$を左に$n$ビットシフトすることを考えます。

$A$が非負の場合、ビット列表現を

$$
\begin{aligned}
A&=[v_{k-1} v_{k-2} \cdots v_2 v_1 v_0]_2 \\
&=\sum_{i=0}^{k-1} 2^i \cdot v_i
\end{aligned}
$$

とすると、これを左に$n$ビットシフトしたものは

$$
\begin{aligned}
A\ll n&=[v_{k-1} v_{k-2} \cdots v_2 v_1 v_0\underbrace{00\cdots 00}_{n\text{ bits}}]_2 \\
&=\sum_{i=0}^{k-1} 2^{i+n} \cdot v_i
\end{aligned}
$$

となり、$A\ll n=2^n A$がわかります。

$A$が負の場合、ビット列表現を

$$
\begin{aligned}
A&=[\ldots 111\overline{v_{k-1}} \overline{v_{k-2}} \cdots \overline{v_2} \overline{v_1} \overline{v_0}]_2\quad (v_i\in\{0,1\}) \\
{\sim}A&=[v_{k-1}v_{k-2}\cdots v_2v_1v_0]_2 \\
&=\sum_{i=0}^{k-1} 2^i \cdot v_i \\
A&=-1-\sum_{i=0}^{k-1} 2^i \cdot v_i
\end{aligned}
$$

とします。ただし、$\overline{v}$は$v$のビット否定です。これを左に$n$ビットシフトしたものは

$$
\begin{aligned}
A\ll n&=[\ldots 111\overline{v_{k-1}} \overline{v_{k-2}} \cdots \overline{v_2} \overline{v_1} \overline{v_0}\underbrace{00\cdots 00}_{n\text{ bits}}]_2 \\
{\sim}(A\ll n)&=[v_{k-1}v_{k-2} \cdots v_2v_1v_0\underbrace{11\cdots 11}_{n\text{ bits}}]_2 \\
&=\sum_{i=0}^{M-2} 2^{i+n} \cdot v_i+\sum_{j=0}^{n-1} 2^j \\
&=\sum_{i=0}^{M-2} 2^{i+n} \cdot v_i+2^n-1 \\
A\ll n&={\sim}\left(\sum_{i=0}^{M-2} 2^{i+n} \cdot v_i+2^n-1\right) \\
&=-1-\left(\sum_{i=0}^{M-2} 2^{i+n} \cdot v_i+2^n-1\right) \\
&=-2^n-\sum_{i=0}^{M-2} 2^{i+n} \cdot v_i \\
&=2^n\left(-1-\sum_{i=0}^{M-2} 2^{i} \cdot v_i\right) \\
&=2^nA
\end{aligned}
$$

となります。

よって、$A$の符号に関わらず

$$
A\ll n=2^nA
$$

です。

# 算術右シフト

右シフトには符号ビットも含めて右シフトする論理右シフトと、符号ビットを維持し、値ビットの上位を符号ビットと同じビットで埋める算術右シフトがあります。

多倍長整数の場合、論理右シフトはwell-definedではなく、算術右シフトだけが意味を持ちます。

$A$が非負の場合、

$$
\begin{aligned}
A&=[v_{k-1} v_{k-2} \cdots v_2 v_1 v_0]_2\quad (v_i\in\{0,1\}) \\
&=\sum_{i=0}^{k-1} 2^i \cdot v_i
\end{aligned}
$$

とします。ただし、$k$はこれからシフトする幅$n$よりも大きく取ります。

すると、これを右に$n$ビットシフトしたものは

$$
\begin{aligned}
A\gg n&=[v_{k-1} v_{k-2} \cdots v_{n+1} v_n]_2 \\
&=\sum_{i=n}^{k-1} 2^{i-n} \cdot v_i
\end{aligned}
$$

であり、これは$\lfloor A/2^n\rfloor$と一致します。

$A$が負の場合、ビット列表現を

$$
\begin{aligned}
A&=[\ldots 111\overline{v_{k-1}} \overline{v_{k-2}} \cdots \overline{v_2} \overline{v_1} \overline{v_0}]_2\quad (v_i\in\{0,1\}) \\
{\sim}A&=[v_{k-1}v_{k-2}\cdots v_2v_1v_0]_2 \\
&=\sum_{i=0}^{k-1} 2^i \cdot v_i \\
A&=-1-\sum_{i=0}^{k-1} 2^i \cdot v_i
\end{aligned}
$$

とします。これを右に$n$ビットシフトしたものは

$$
\begin{aligned}
A\gg n&=[\ldots 111\overline{v_{k-1}} \overline{v_{k-2}} \cdots \overline{v_{n+1}} \overline{v_n}]_2 \\
{\sim}(A\gg n)&=[v_{k-1}v_{k-2}\cdots v_{n+1}v_n]_2 \\
&=\sum_{i=n}^{k-1} 2^{i-n} \cdot v_i \\
&=\lfloor ({\sim}A)/2^n\rfloor \\
A\gg n&=-1-({\sim}(A\gg n)) \\
&=-1-\lfloor ({\sim}A)/2^n\rfloor \\
&=-1-\lfloor (-1-A)/2^n\rfloor \\
&=-1+\lceil (1+A)/2^n\rceil
\end{aligned}
$$

となります。これを不等式で書き直すと、

$$
(A\gg n)<(1+A)/2^n\le (A\gg n)+1
$$

$2^n$倍して

$$
2^n\cdot(A\gg n)<1+A\le 2^n\cdot((A\gg n)+1)
$$

それぞれ整数であることを使って

$$
2^n\cdot(A\gg n)+1\le 1+A\le 2^n\cdot((A\gg n)+1)
$$

1を引いて

$$
2^n\cdot(A\gg n)\le A\le 2^n\cdot(A\gg n)+2^n-1
$$

主張を少し弱めて

$$
2^n\cdot(A\gg n)\le A<2^n\cdot(A\gg n)+2^n
$$

$2^n$で割って

$$
(A\gg n)\le A/2^n<(A\gg n)+1
$$

よって、$A\gg n=\lfloor A/2^n\rfloor$を得ます。

結局、$A$の符号に関わらず

$$
A\gg n=\lfloor A/2^n\rfloor
$$

であることがわかりました。これはflooring divisionです。

---

結論はこれでいいと思いますが、なんか正当化が雑な気がします。まあメモなので……。
