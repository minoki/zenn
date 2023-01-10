---
title: "浮動小数点数オタクがAtCoder Beginner Contest 284のD問題をガチで解説してみる"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["atcoder"]
published: true
---

こんにちは、浮動小数点数オタクのmod\_poppoです。

昨日開催されたABC284のD問題でsqrtがどうのこうのという声がツイッターで観測されたので、ガチで考察してみます。

## 問題文（引用）

まず最初に問題文を引用しておきます。

* [AtCoder Beginner Contest 284 | D - Happy New Year 2023](https://atcoder.jp/contests/abc284/tasks/abc284_d)

> **問題文**
>
> 正整数$N$が与えられます。$N$は、2つの相異なる素数$p$, $q$を用いて$N=p^2q$と表せることがわかっています。
>
> $p$, $q$を求めてください。
>
> $T$個のテストケースが与えられるので、それぞれについて答えを求めてください。
>
> **制約**
>
> * 入力は全て整数
> * $1\le T\le 10$
> * $1\le N\le 9\times 10^{18}$
> * $N$は、2つの相異なる素数$p$, $q$を用いて$N=p^2q$と表せる

2023が$7\times 17^2$と素因数分解できることにちなんだ問題のようです。

## 方針

まず、$p$と$q$の少なくとも一方は$\sqrt[3]{N}$以下です（$p$と$q$の両方が$\sqrt[3]{N}$よりも大きいと仮定すると矛盾が出ます）。制約から$\sqrt[3]{N}\le \sqrt[3]{9}\times 10^6\approx 2.08\times 10^6$なので、試し割りによって$N$の素因数を一個見つけることは難しくないでしょう。

（試し割りの際はあらかじめ素数を列挙しておいて素数だけで割るのが効率的ですが、速度を気にしないなら全ての整数で割っても良いでしょう。）

試し割りによって見つかった$N$の素因数を$a$とします。$a=p$なのか$a=q$なのかが問題です。

$a=p$の場合、$N/a$は再び$a$で割り切れ、$q=N/a^2$と計算できます。

$a=q$の場合、$N/a=p^2$なので、$N/a$の平方根を取ることで$p$が求まります。

整数除算は問題ないですが、平方根の計算は問題です。安易に浮動小数点数のsqrt関数を使ってしまうと誤差が心配です。$N/a$の大きさは（$a$は素数なので$a\ge 2$を考慮して）最大で$4.5\times 10^{18}\approx 2^{62}$程度になり、この規模の整数は倍精度浮動小数点数で正確に表現できません。

この平方根の計算をどうするか？というのがこの記事の主題です。

平方根の計算以外の部分をC言語で実装すると次のようになります：

```c
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    // どう実装する？
}

struct result {
    int64_t p, q;
};

// 入力：N <= 9*10^18
struct result solve(int64_t N)
{
    // 2080083^3 < 9*10^18 < 2080084^3
    for (int64_t a = 2; a <= 2080083; ++a) {
        if (N % a == 0) {
            int64_t b = N / a;
            if (b % a == 0) {
                // a = p
                return (struct result){.p = a, .q = b / a};
            } else {
                // a = q
                return (struct result){.p = isqrt(b), .q = a};
            }
        }
    }
    abort();
}

int main()
{
    int T;
    scanf("%d", &T);
    for (int i = 0; i < T; ++i) {
        int64_t N;
        scanf("%" SCNd64, &N);
        struct result r = solve(N);
        printf("%" PRId64 " %" PRId64 "\n", r.p, r.q);
    }
}
```

## 解法1：整数演算で済ませる

浮動小数点数の計算が心配なら、全部整数演算で片付ければ良いのです。そう、伝家の宝刀†二分探索†を使わせていただきますぞ！

```c
// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    // 4.5*10^18 < 2^62
    // 真の答えは 2 <= _ < 2^31 の範囲にある
    int64_t low = 2, high = INT64_C(1) << 31;
    while (low < high) {
        int64_t mid = (low + high) / 2;
        int64_t mid2 = mid * mid; // 最初の上界を2^31程度に抑えているのでここではオーバーフローしない
        if (mid2 < n) {
            low = mid;
        } else if (mid2 == n) {
            return mid;
        } else {
            high = mid;
        }
    }
    return low;
}
```

## 解法2：80ビットのlong doubleを使う

x86のGCCでは `long double` が80ビットあって、仮数部も64ビットあります。なので、64ビット整数を正確に表現できます。もちろん、四倍精度が使えるならそれでも構いません。

* [long doubleの話](https://qiita.com/mod_poppo/items/8860505f38e2997cd021)

C言語では `long double` に対応する `sqrtl` があり、これは正確な（真の値に最も近い浮動小数点数を返す）ことが期待されるのでうまくいきます（x87にはFSQRTという命令があるのでそれを使っていることが期待できます）。

```c
#include <assert.h>
#include <float.h>
#include <math.h>

static_assert(LDBL_MANT_DIG >= 64, "not enough precision");

// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    return (int64_t)sqrtl((long double)n);
}
```

この方法は `long double` が64ビットな環境（MSVCとかApple Silicon Macとか）や `long double` を提供していないプログラミング言語からは使えないのがネックです。

## 解法3：sqrtとroundを使う

$N/a$が最大で$2^{62}$程度（なので `double` で正確に表現できない可能性がある）とは言っても、平方根を取ってしまえば$2^{31}$程度になり、この範囲の整数値は倍精度浮動小数点数で正確に表現できます。

何が言いたいかというと、`sqrt(N/a)` は整数$p$に極めて近いことが期待され、与えられた浮動小数点数に最も近い整数を返す関数 `round` を適用してやれば正しい答えが得られるだろう、ということです。

```c
// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    return llround(sqrt((double)n)); // llroundはroundの結果をlong longで返す関数
}
```

この解法はACしますが、「単にテストケースが弱い」のではなく、「本当に正しい」ことを証明してみましょう。

倍精度浮動小数点数で表現できる実数のなす集合を$\mathbf{F}$とし、最近接偶数丸めを行う写像を$F\colon\mathbf{R}\rightarrow\mathbf{F}$とします。この時、次の定理が成り立ちます：

**定理1**．実数$x$を浮動小数点数に丸める際にオーバーフロー・アンダーフローが起こらないとする。この時、次が成り立つ：

$$
\left|\frac{F(x)-x}{x}\right|\le\frac{\varepsilon}{2}
$$

ただし、$\varepsilon$は計算機イプシロン (machine epsilon) と呼ばれる値で、倍精度浮動小数点数の場合は$\varepsilon=2^{-52}$である。

【追記】この定理についてはより詳しい記事を書きました：[浮動小数点数の丸めの相対誤差を計算機イプシロンで評価する](floating-point-relative-error)

$x>0$の場合に定理1を言い換えると、$(1-\varepsilon/2)x\le F(x)\le(1+\varepsilon/2)x$となります。

C言語の `sqrt` は大抵の処理系ではIEEE 754に準拠、つまり「真の平方根に最も近い浮動小数点数を返す」ようになっています（正確に言うならC処理系がC言語の規格のAnnex Fに準拠する場合これが成り立つ）。

なので、証明したいのは$n=p^2$に対して

$$
p-\frac{1}{2}<F\bigl(\sqrt{F(n)}\bigr)<p+\frac{1}{2}
$$

となります。

まず$F(n)$を定理1で評価すると

$$
(1-\varepsilon/2)n\le F(n)\le(1+\varepsilon/2)n
$$

となり、それぞれ平方根を取ると

$$
\sqrt{1-\varepsilon/2}\cdot p\le\sqrt{F(n)}\le\sqrt{1+\varepsilon/2}\cdot p
$$

となります。これを浮動小数点数に丸めると

$$
F\bigl(\sqrt{1-\varepsilon/2}\cdot p\bigr)\le F\bigl(\sqrt{F(n)}\bigr)\le F\bigl(\sqrt{1+\varepsilon/2}\cdot p\bigr)
$$

となり、左と右を評価すると

$$
(1-\varepsilon/2)^{3/2}p\le F\bigl(\sqrt{F(n)}\bigr)\le (1+\varepsilon/2)^{3/2}p
$$

となります。

あとは

$$
p-\frac{1}{2}<(1-\varepsilon/2)^{3/2}p,\quad (1+\varepsilon/2)^{3/2}p<p+\frac{1}{2}
$$

を示せば十分です。

前半について。実数$t\ge-1$について不等式$(1+t)^{3/2}\geq 1+\frac{3}{2}t$が成り立つことを利用すると、

$$
\begin{aligned}
(1-\varepsilon/2)^{3/2}p-\left(p-\frac{1}{2}\right)
&\geq\left(1-\frac{3}{2}\cdot\frac{\varepsilon}{2}\right)p-\left(p-\frac{1}{2}\right) \\
&=\frac{1}{2}-\frac{3}{2}\cdot\frac{\varepsilon}{2} p
\end{aligned}
$$

と評価できます。あとは$\varepsilon=2^{-52}$と$p<2^{31}$を使えば$p-\frac{1}{2}<(1-\varepsilon/2)^{3/2}p$が言えます。

後半について。実数$0<t<\frac{1+\sqrt{5}}{2}$について$(1+t)^{3/2}<1+2t$が成り立つことを使うと、

$$
\begin{aligned}
\left(p+\frac{1}{2}\right)-(1+\varepsilon/2)^{3/2}p
&>p+\frac{1}{2}-(1+\varepsilon)p \\
&=\frac{1}{2}-\varepsilon p
\end{aligned}
$$

と評価でき、あとは$\varepsilon p$の範囲に注意すれば欲しい不等式を得ます。

よって、「倍精度浮動小数点数でsqrtしてからroundする解法」が正しいことが証明できました。

## 解法4：sqrtと切り捨てを使う

C系の言語では浮動小数点数を整数にキャストする際は0方向への切り捨てが行われます。解法3で `round` の代わりにキャストしてしまうとどうなるでしょうか？

```c
// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    return (int64_t)sqrt((double)n);
}
```

実はこれもACしますが、これで問題ないことを証明するのはかなり厄介です。

とりあえずこの方法が問題ないことを実験的に確認してみましょう：

```c
#include <inttypes.h>
#include <math.h>
#include <stdio.h>

int main()
{
    // 3037000499^2 < 2^63-1 < 3037000500^2
    for (int64_t i = 0; i <= 3037000499; ++i) {
        int64_t n = i * i;
        int64_t j = (int64_t)sqrt((double)n); // sqrtの結果を切り捨てる
        if (i != j) {
            printf("%" PRId64 "\n", i);
            return 0;
        }
    }
    puts("Done");
}
```

このプログラムが `Done` を出力すれば反例はありません。（このプログラムを `float` / `sqrtf` で実行すると普通に反例が出てきます。$p$の大きさが浮動小数点数で正確に表現できる程度に小さいことは当たり前ですが重要です。）

では証明の時間です。

$p$を2乗して浮動小数点数に変換し、その平方根を取ったものを再び浮動小数点数に変換した時に元の$p$よりも小さくならないことを証明したいです。

実数$x$を浮動小数点数に変換した時に元の$p$より小さくならないというのは、「$p$の次に小さい浮動小数点数」を$\mathsf{nextDown}(p)$とした時に$\frac{\mathsf{nextDown}(p)+p}{2}<x$が成り立てば十分です。

$p$の指数部を整数$i$として$2^i< p\le 2^{i+1}$とします。$p=2^{i+1}$の場合は$p^2$は浮動小数点数で正確に表現でき、その平方根も正確なので突っ込んだ議論の必要はありません。よって、評価を精密にして$2^i<p\le 2^{i+1}-2^i\varepsilon$の場合を考えます。

すると$\mathsf{nextDown}$は$\mathsf{nextDown}(p)=p-2^i\varepsilon$と書けるので、示したいのは

$$
p-2^{i-1}\varepsilon<\sqrt{F(p^2)}
$$

となります。右辺は$\sqrt{1-\varepsilon/2}\cdot p\le\sqrt{F(p^2)}$と評価できるので、

$$
p-2^{i-1}\varepsilon<\sqrt{1-\varepsilon/2}\cdot p
$$

を示せば十分です。

実数$0\le t<2\sqrt{2}-2\approx 0.8284$について$\sqrt{1-t}>1-\frac{1}{2}t-\frac{1}{4}t^2$が成り立つので、

$$
\begin{aligned}
\sqrt{1-\varepsilon/2}\cdot p-\left(p-2^{i-1}\varepsilon\right)
&>\left(1-\frac{1}{2}\cdot\frac{\varepsilon}{2}-\frac{1}{4}\cdot\frac{\varepsilon^2}{4}\right)p-p+2^{i-1}\varepsilon \\
&=2^{i-1}\varepsilon-\frac{\varepsilon p}{4}-\frac{\varepsilon^2 p}{16} \\
&=2^{i-1}\varepsilon\left(1-\frac{1}{2}\cdot\frac{p}{2^i}-\frac{\varepsilon p}{8\cdot 2^i}\right)
\end{aligned}
$$

ここで$p\le 2^{i+1}-2^i\varepsilon$を思い出すと$\frac{p}{2^i}\le 2-\varepsilon$なので、

$$
\begin{aligned}
\sqrt{1-\varepsilon/2}\cdot p-\left(p-2^{i-1}\varepsilon\right)
&>2^{i-1}\varepsilon\left(1-\frac{1}{2}\cdot\frac{p}{2^i}-\frac{\varepsilon p}{8\cdot 2^i}\right) \\
&\ge 2^{i-1}\varepsilon\left(1-\frac{2-\varepsilon}{2}-\frac{\varepsilon p}{8\cdot 2^i}\right) \\
&= 2^{i-4}\varepsilon^2\left(4-\frac{p}{2^i}\right) \\
&>0
\end{aligned}
$$

と評価できます。

よって、「倍精度浮動小数点数でsqrtしてから切り捨てる解法」が正しいことが証明できました。

## 雑感

浮動小数点数の誤差評価は面倒なのでなるべくやりたくないですね。特に切り捨ての解法の正当性の証明は面倒なので、浮動小数点数を整数に変換する際は安易なキャストではなく `round` 関数を使うようにしましょう。浮動小数点数を使わないやり方ができれば一番良いです。

~~安易にキャスト（切り捨て）して通ってしまう問題はあまり教育的ではない気もする。~~

## 過去の類似記事

* [浮動小数点数オタクが AtCoder Beginner Contest 169 のC問題をガチで解説してみる](https://qiita.com/mod_poppo/items/910b5fb9303baf864bf7)
