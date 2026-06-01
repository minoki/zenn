---
title: "RISC-Vベクトル拡張（RVV）はじめの一歩：C言語から触ってみる"
emoji: "➡️"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語, riscv]
published: true
---

RISC-Vのベクトル拡張（RVV）に準拠したハードウェアが出回るようになってきて、私も一つ入手しました（[SpacemiT K3搭載ボードを買った](https://blog.miz-ar.info/2026/05/spacemit-k3-arrived/)）。ここでは、RISC-Vのベクトル拡張をC言語から触ってみます。

RISC-Vのベクトル拡張をC言語から触るためのインターフェースは次のリポジトリで文書化されています。執筆時点（2026年）ではv1.0が正式なリリースとして出ています。

* [riscv-non-isa/riscv-rvv-intrinsic-doc](https://github.com/riscv-non-isa/riscv-rvv-intrinsic-doc/)

RISC-Vのベクトル拡張全般についてのメモを以前書いたので、それも参考にしてください：

* [RISC-V ベクトル拡張（Vector, RVV）メモ](https://zenn.dev/mod_poppo/scraps/c04489c3759e20)

私が動作確認に使っているSpacemiT K3は、アプリケーション用のコアであるX100とAI向けのコアであるA100を持っており、前者のベクトル長は256ビット、後者は1024ビットです。

ベクトル拡張が動くRISC-Vの実機を持っていない方も多いと思うので、QEMUでの動作例も載せておきます。

実機でのコンパイラーとしてはGCC 15とClang 21を使っています。

## QEMUを使う場合の環境構築

x86とかArmのLinux環境でQEMUを使って動作確認する場合の準備です。まずはクロスコンパイラーとQEMUを入れます。

```
$ sudo apt install gcc-14-riscv64-linux-gnu qemu-user
$ riscv64-linux-gnu-gcc-14 --version
riscv64-linux-gnu-gcc-14 (Ubuntu 14.2.0-4ubuntu2~24.04.1) 14.2.0
Copyright (C) 2024 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

$ qemu-riscv64 --version
qemu-riscv64 version 8.2.2 (Debian 1:8.2.2+ds-0ubuntu1.16)
Copyright (c) 2003-2023 Fabrice Bellard and the QEMU Project developers
```

QEMUを使った動作確認ではUbuntu 24.04ベースの環境を使っています。

## ベクトル長の取得

x86のSIMD拡張（SSE, AVX, AVX-512）に対するRISC-Vのベクトル拡張の特徴は、ISAではなくハードウェアによってベクトルレジスターの幅（ビット数）が決まることです。

ベクトルレジスターの幅（ビット数）は仕様の中では `VLEN` と呼ばれます。まずは `VLEN` を取得するプログラムを書いてみましょう。

ベクトル長をバイト数で計ったもの（`VLEN/8`）を取得する組み込み関数は `__riscv_vlenb()` です。ヘッダーは `<riscv_vector.h>` です。

```c:vlen.c
#include <stdio.h>
#include <riscv_vector.h>

int main()
{
    printf("%u\n", (unsigned int)__riscv_vlenb());
}
```

`__riscv_vlenb()` の返り値の型は現行の仕様では `unsigned` となっていますが、試したバージョンのGCCでもClangでも、`unsigned long` が返ってきました。なのでここではキャストしています。

---

SpacemiT K3での実行結果：

```
$ gcc -std=c23 -Wall -o vlen vlen.c
  # または clang -std=c23 -Wall -o vlen vlen.c
$ ./vlen
32
```

`VLEN/8=32` で、ベクトルレジスターの幅は256ビットであることがわかりました。

私が動作確認したOS（Ubuntu 26.04ベース）ではRVA23プロファイルを前提にするということで、特にオプションをつけなくてもベクトル拡張が使えました。

---

QEMUでもやってみます。Ubuntu 24.04ベースの環境ではコンパイル時に `-march=rv64gcv` のようなオプションを指定してベクトル拡張を有効化する必要があります。

```
$ riscv64-linux-gnu-gcc-14 -std=c23 -Wall -o vlen -march=rv64gcv vlen.c
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./vlen
16
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64,v=true,vlen=256,vext_spec=v1.0 ./vlen
32
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu max,vlen=512 ./vlen
64
```

QEMUにオプションをつけずに実行した場合は出力が16なので、ベクトルレジスターの幅は128ビットです。「V」拡張に準拠するCPUは `VLEN` が128以上である必要があります。

QEMUがエミュレートするCPUのVLENを変更する場合は、`-cpu` オプションの `vlen=` で指定します。ベースとなるCPUの名前が `rv64` の場合は `v=true,vext_spec=v1.0` も指定してやる必要がありました。

---

SpacemiT K3では `/proc/set_ai_thread` にPID/TIDを書き込むことによってA100というAIコアでプログラムを動作させることができます。

```c:vlen_a100.c
#include <stdio.h>
#include <unistd.h>
#include <riscv_vector.h>

int main()
{
    FILE *fp = fopen("/proc/set_ai_thread", "w");
    fprintf(fp, "%d", (int)getpid());
    fclose(fp);
    printf("%u\n", (unsigned int)__riscv_vlenb());
}
```

SpacemiT K3での実行結果：

```
$ gcc -std=c23 -Wall -o vlen_a100 vlen_a100.c
  # または clang -std=c23 -Wall -o vlen_a100 vlen_a100.c
$ ./vlen_a100
128
```

A100では `VLEN/8=128`、すなわち `VLEN` が1024（ビット）であることがわかりました。

## ベクトルの加算

配列（ベクトル）に入った浮動小数点数の加算を書いてみます。普通のC言語で書けば次のようなコードになるでしょう：

```c
void vec_add_f(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = a[i] + b[i];
    }
}
```

最近のコンパイラーは賢いので適切な最適化オプションをつければこのコードでもベクトル拡張を使ったバイナリーを作ってくれますが、ここではRVVの組み込み型と関数を使って同等のコードを書いてみます。

RVVで単精度浮動小数点数を要素とするベクトルレジスター1本に対応するC言語の型は `vfloat32m1_t` となります。ベクトル拡張に馴染みのない方には「`m1`」が若干謎かもしれませんが、それ以外はベクトルの「`v`」、要素型の「`float32`」、型名の「`_t`」で構成されており、素直なのではないかと思います。

RISC-Vのベクトル拡張は処理する**要素数**を中心に考えます。x86のSIMDでは、要素数は「ベクトル長を要素幅で割ったものとして定まる付随的な量」ですが、RVVでは要素数を第一に考えます。

x86のSIMDでは「単精度浮動小数点数からなるベクトル2つをを足す組み込み関数」は

```c
__m128 _mm_add_ps(__m128 a, __m128 b);
```

という風に処理対象のベクトルだけを引数に取って、処理する要素数は「ベクトル長を要素幅で割ったもの」すなわちこの例なら4として決まります。処理したいのが3要素だけであっても、要素数を明示的に指定することはできません。一方で、RVVでは、「ベクトル2つを足す」ような演算に対応する組み込み関数一つ一つに「処理する要素数」`vl` の引数があります。

```c
vfloat32m1_t __riscv_vfadd_vv_f32m1(vfloat32m1_t vs2, vfloat32m1_t vs1, size_t vl);
```

関数名を解体してみましょう。RISC-Vの `__ricsv_` とベクトル拡張を表す `v` と、浮動小数点数の `f` と、加算を表す `add`、そして入力が二つともベクトルであることを表す `vv`、そして要素型 `f32`、そして謎の `m1` です。ベクトルとスカラーの足し算だと `vv` じゃなくて `vf` になったりします。

C言語のRVV組み込み関数は型についてオーバーロードされたインターフェースも提供しており、後半部を省略して `__riscv_vfadd` で呼び出すこともできます。

配列からのベクトルの読み書きは次の関数で行います。これらも要素数 `vl` を受け取ります。 

```c
vfloat32m1_t __riscv_vle32_v_f32m1(const float *rs1, size_t vl);
void __riscv_vse32_v_f32m1(float *rs1, vfloat32m1_t vs3, size_t vl);
```

要素数はどのように決めれば良いのでしょうか。大きな配列をループで処理する場合は、残りの要素数は `配列の長さ - 現在のインデックス` という式で計算できるでしょう。しかし、RVVでは一つのベクトルレジスターに収まる要素数はせいぜい `VLEN/要素幅` で、上限があります。

RVVには、「処理したい要素数」を元に「現在の周回で処理する要素数」を計算するための組み込み関数 `vsetvl` があります。正確な関数名は `__riscv_vsetvl_e32m1` となります。要素幅が32ビットなら `e32` です。`m1` は後で説明するやつです。

`vsetvl` を使えば、現在の周回で処理できる要素数を計算して、インデックスに加算することができます。

まとめると、RVVの組み込み関数を使った配列の加算は次のように実装できます：

```c
#include <riscv_vector.h>

void vec_add_f_rvvm1(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    size_t vl;
    for (size_t i = 0; i < n; i += vl) {
        vl = __riscv_vsetvl_e32m1(n - i);
        vfloat32m1_t vx = __riscv_vle32_v_f32m1(&a[i], vl);
        vfloat32m1_t vy = __riscv_vle32_v_f32m1(&b[i], vl);
        vfloat32m1_t vresult = __riscv_vfadd_vv_f32m1(vx, vy, vl);
        __riscv_vse32_v_f32m1(&result[i], vresult, vl);
    }
}
```

試しに実行してみましょう：

```c
#include <stdio.h>

int main()
{
    float a[100], b[100], result[100];
    for (size_t i = 0; i < 100; ++i) {
        a[i] = (float)i;
        b[i] = (float)i / 10.0f;
    }
    vec_add_f_rvvm1(100, a, b, result);
    for (size_t i = 0; i < 100; ++i) {
        printf("result[%zu]=%g\n", i, result[i]);
    }
}
```

実機の場合：

```
$ gcc -std=c23 -Wall -o vecadd_simple vecadd_simple.c
$ ./vecadd_simple
result[0]=0
result[1]=1.1
result[2]=2.2
result[3]=3.3
...
result[96]=105.6
result[97]=106.7
result[98]=107.8
result[99]=108.9
```

QEMUの場合：

```
$ riscv64-linux-gnu-gcc-14 -std=c23 -march=rv64gcv -Wall -o vecadd_simple vecadd_simple.c
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./vecadd_simple
result[0]=0
result[1]=1.1
result[2]=2.2
result[3]=3.3
...
result[96]=105.6
result[97]=106.7
result[98]=107.8
result[99]=108.9
```

良い感じですね。

## 複数のレジスターを束ねる

仮に `VLEN` が256の場合は、2本のレジスターの入力を加算する命令は単精度浮動小数点数なら最大で8要素同時に処理できます。しかし、実際のCPUには8要素分だけではなく、もっとたくさんの演算器が積まれているかもしれません。そういう場合、ループ1周あたりで使うレジスターの本数と命令の個数を倍にして並べれば、CPUに搭載されている演算器をフル活用できます。

```c
#include <riscv_vector.h>

void vec_add_f_rvvm1x2(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    size_t vl0, vl1;
    for (size_t i = 0; i < n; i += vl0 + vl1) {
        size_t i0 = i;
        vl0 = __riscv_vsetvl_e32m1(n - i);
        size_t i1 = i + vl0;
        vl1 = __riscv_vsetvl_e32m1(n - i1);
        vfloat32m1_t vx0 = __riscv_vle32_v_f32m1(&a[i0], vl0);
        vfloat32m1_t vx1 = __riscv_vle32_v_f32m1(&a[i1], vl1);
        vfloat32m1_t vy0 = __riscv_vle32_v_f32m1(&b[i0], vl0);
        vfloat32m1_t vy1 = __riscv_vle32_v_f32m1(&b[i1], vl1);
        vfloat32m1_t vresult0 = __riscv_vfadd_vv_f32m1(vx0, vy0, vl0);
        vfloat32m1_t vresult1 = __riscv_vfadd_vv_f32m1(vx1, vy1, vl1);
        __riscv_vse32_v_f32m1(&result[i0], vresult0, vl0);
        __riscv_vse32_v_f32m1(&result[i1], vresult1, vl1);
    }
}
```

RISC-Vのベクトル拡張はこのような使い方も想定しており、複数のレジスターを束ねて使う仕組みが用意されています。

これまで「謎」としていた `m1` の部分を `m2` に変えれば変数1つにつきレジスターを2本使うようになります。`m4` にすれば4本のレジスターを、`m8` にすれば8本のレジスターを束ねて使うことができます。

```c
void vec_add_f_rvvm2(size_t n, const float a[n], const float b[n], float result[restrict n])
{
    size_t vl;
    for (size_t i = 0; i < n; i += vl) {
        vl = __riscv_vsetvl_e32m2(n - i);
        vfloat32m2_t vx = __riscv_vle32_v_f32m2(&a[i], vl);
        vfloat32m2_t vy = __riscv_vle32_v_f32m2(&b[i], vl);
        vfloat32m2_t vresult = __riscv_vfadd_vv_f32m2(vx, vy, vl);
        __riscv_vse32_v_f32m2(&result[i], vresult, vl);
    }
}
```

RISC-Vではこの仕組みのことをvector register group multiplier (`LMUL`) と呼びます。

`sizeof(vfloat32m2_t)` とかを計算できればわかりやすいのかもしれませんが、残念ながらRVVではベクトル型のサイズを `sizeof` で計算することはできません。

## ベクトルの加算の実行時間を測定する

せっかく実機があるので、パフォーマンス特性も測ってみましょう。

スカラーのループ、RVVの `m1` から `m8` までを使ったループを試せるプログラムを [vecadd.c](https://github.com/minoki/rvv-test/blob/b2840d769122a84dc2389a2b6baf6b78d83199e9/vecadd.c) に置きました。`--ai` オプションをつけることでSpacemiT K3のA100コアで実行することもできます。

処理内容は単精度10000要素の加算です。

```
$ gcc -std=c23 -O2 -Wall -fno-tree-vectorize -o vecadd vecadd.c
```

まずはスカラーのループです。自動ベクトル化を無効にするために `-fno-tree-vectorize` を指定しています。

```
$ ./vecadd plain
OK
14021.9 ns
```

14マイクロ秒です。

RVVを使ったものも試してみましょう。`LMUL` を変えたものを `m1`, `m2`, `m4`, `m8` で呼び出せます。

```
$ ./vecadd m1
OK
5985.41 ns
$ ./vecadd m2
OK
4002.42 ns
$ ./vecadd m4
OK
3913.52 ns
$ ./vecadd m8
OK
4064.71 ns
```

「複数のレジスターを束ねて使う」のやつを手動でやったものも試してみます。同じ命令が2つずつ並ぶことになります。

```
$ ./vecadd m1x2
OK
4294.78 ns
$ ./vecadd m2x2
OK
4218.83 ns
$ ./vecadd m4x2
OK
4374.49 ns
```

ループ1回で処理する要素数を増やしても、X100での実行結果としては4マイクロ秒でサチるという結果になりました。

A100でも試してみましょう。

```
$ ./vecadd --ai plain
OK
40836.9 ns
$ ./vecadd --ai m1
OK
9036.53 ns
$ ./vecadd --ai m2
OK
8771.09 ns
$ ./vecadd --ai m4
OK
6973.84 ns
$ ./vecadd --ai m8
OK
5443.43 ns
$ ./vecadd --ai m1x2
OK
8264.66 ns
$ ./vecadd --ai m2x2
OK
7230.49 ns
$ ./vecadd --ai m4x2
OK
4708.55 ns
```

ちょっと実行結果のばらつきが大きそうなのでその辺を考慮して見て欲しいです（hyperfineとかを使えば良いんでしょうけど）。

A100はX100よりも強力なベクトル演算器を積んでいそうなのにも関わらず、全体的にX100よりも遅い結果となりました。まあ「ベクトルの加算」というタスクはメモリーの読み書きで律速されると考えると、驚くべき結果ではありません。A100の真価を発揮させるには他のタスク、行列乗算とかをさせるべきなのでしょう。

---

簡単ですが、RISC-Vのベクトル拡張を実機で試してみました。暇があれば次は行列乗算編となるのでしょうか。
