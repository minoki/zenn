---
title: "多倍長整数のビット演算実装メモ"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["int"]
published: false
---

現代の環境では、符号付きの固定長整数は2の補数表現を使うことが多いです。ビット演算も2の補数表現に対して行われます。

一方、多倍長整数は符号と絶対値で表現することが多いかと思います。この時、多倍長整数のビット演算は「2の補数表現の拡張」として規定されることが多いわけですが、具体的にはどうすれば良いのでしょうか。

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

まず、非負整数同士のビット論理積は既知とします。ここでは$A\ge 0$, $B\ge 0$のビット論理積を$A\mathbin{\&}B$と書くことにします。具体的には、$n$を十分大きい整数として

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

さらに、任意の整数$A$, $B$に対して$A\oplus B={\sim}(({\sim}A)\oplus ({\sim}B))$が成り立つことを認めます。

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

整数$A$を左に$n$ビットシフトすることを考えます。2の補数表現をベースにしたいので、適当な$M$について

$$
A=-2^{M-1}\cdot s + \sum_{i=0}^{M-2} 2^i \cdot v_i
$$

とします。$M$は気持ち的には（$A$の桁数やシフト幅に対して）十分大きい整数です。

$A$が正であればビット列表現は

$$
0\underbrace{00\cdots 00}_{M-k-1\text{ bits}}v_{k-1} v_{k-2} \cdots v_2 v_1 v_0\quad (s, v_i\in\{0,1\})
$$

となり、これを左に$n$ビットシフトしたものは、

$$
0\underbrace{00\cdots 00}_{M-k-n-1\text{ bits}}v_{k-1} v_{k-2} \cdots v_2 v_1 v_0\underbrace{00\cdots 00}_{n\text{ bits}}\quad (s, v_i\in\{0,1\})
$$

すなわち

$$
A\ll n=\sum_{i=0}^{k-1} 2^{i+n} \cdot v_i=A\cdot 2^n \quad(A\ge 0)
$$

となります（後出しですが、$M$はシフト幅に対しても十分大きい、$M\ge k+n+1$とします）。

$A$が負であればビット列表現は

$$
1\underbrace{11\cdots 11}_{M-k-1\text{ bits}}v_{k-1} v_{k-2} \cdots v_2 v_1 v_0\quad (s, v_i\in\{0,1\})
$$

となり、これを左に$n$ビットシフトしたものは

$$
1\underbrace{11\cdots 11}_{M-k-n-1\text{ bits}}v_{k-1} v_{k-2} \cdots v_2 v_1 v_0\underbrace{00\cdots 00}_{n\text{ bits}}\quad (s, v_i\in\{0,1\})
$$

すなわち

$$
\begin{aligned}
A\ll n&=-2^{M-1}+\sum_{i=0}^{M-k-n-2} 2^{i+n+k}+ \sum_{i=0}^{k-1} 2^{i+n} \cdot v_i \\
&=-2^{M-1}+(2^{M-k-n-1}-1)\cdot 2^{n+k}+ \sum_{i=0}^{k-1} 2^{i+n} \cdot v_i \\
&=-2^{n+k}+ \sum_{i=0}^{k-1} 2^{i+n} \cdot v_i \\
&=\left(-2^k+ \sum_{i=0}^{k-1} 2^i \cdot v_i\right)\cdot 2^n \\
&=A\cdot 2^n
\end{aligned}
$$

です（最後ごまかした）。

# 算術右シフト
