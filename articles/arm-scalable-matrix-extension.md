---
title: "ArmのScalable Matrix Extension (SME)を試す"
emoji: "🙆"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["arm64", "aarch64"]
published: true
---

最近のCPUには行列乗算に役立つ命令が載っていることがあります。IntelのAdvanced Matrix Extensions (AMX)、AppleのAMX、IBM PowerのMatrix-Multiply Assist (MMA)，そしてここで取り上げるArmのScalable Matrix Extension (SME)です。

SMEはここ数年（2021年ごろから？）話は聞きますが、実物の話は聞かないという状況でした（私の中では）。それが最近発表されたApple M4に実装されているという噂を聞いて、俄然興味が出てきました。Apple M4の実物は私は持っていませんが、QEMUを使うとSMEの動作確認ができるようです。やってみましょう。

【2025年10月4日 追記】Apple M4での動作結果を載せました。環境はmacOS 15.7.1 / Xcode 26.0.1 / Apple Clang 17.0.0です。【/追記】

## 環境構築とベクトル長

Ubuntu 24.04上のGCC 14/Clang 18とQEMU 8.2で動作確認します。Ubuntuはx86_64で動いています。

```
$ sudo apt install gcc-14-aarch64-linux-gnu clang-18 qemu-user
```

SMEはSVEの拡張みたいな感じなので、ベクトル長がCPU依存です。SMEを使うコードはStreaming ModeとNon-streaming Modeの状態を持ち、Streaming Modeではベクトル長が通常のものから変わる可能性があります。Streaming Modeでのベクトル長（ビット単位）を$\mathrm{SVL}$で表記し、バイト単位のベクトル長を$\mathrm{SVL}_{\mathrm{B}}=\mathrm{SVL}/8$、16ビット（ハーフワード）単位のベクトル長を$\mathrm{SVL}_{\mathrm{H}}=\mathrm{SVL}/16$、32ビット単位のベクトル長を$\mathrm{SVL}_{\mathrm{S}}=\mathrm{SVL}/32$、64ビット単位のやつを$\mathrm{SVL}_{\mathrm{D}}=\mathrm{SVL}/64$、128ビット単位のやつを$\mathrm{SVL}_{\mathrm{Q}}=\mathrm{SVL}/128$と表記します。

Hello world代わりに、それぞれのモードでのベクトル長を確認するコードを書いてみましょう。

```c
#include <stdio.h>
#include <arm_sme.h>

__arm_locally_streaming
void streaming_fn(void)
{
    printf("Streaming mode: svcntw() = %u, svcntsw() = %u\n", (unsigned)svcntw(), (unsigned)svcntsw());
}

int main(void)
{
    printf("Has SME? %s\n", __arm_has_sme() ? "Yes" : "No");
    printf("Non-streaming mode: svcntw() = %u, svcntsw() = %u\n", (unsigned)svcntw(), (unsigned)svcntsw());
    streaming_fn();
}
```

`__arm_locally_streaming` 属性をつけた関数の内部でStreaming Modeが有効になります。`svcnt` 系の関数は従来のSVEにもあったやつで、これの値がStreaming Modeかどうかで変化します。`svcnts` 系の関数はSMEのやつで、Streaming Mode中でのベクトル長を返します。

~~[ACLE](https://arm-software.github.io/acle/main/acle.html#changing-streaming-mode-locally)だと `__arm_locally_streaming` じゃなくて `__attribute__((arm_locally_streaming))` のようですが、実際のコンパイラーに実装されているのは `__arm_locally_streaming` です。謎です。将来変わるのかもしれません。~~ 執筆時点のACLEだと `__attribute__` を使っていましたが、ACLE 2024 Q1で `__arm_locally_streaming` のようなキーワードを使うように改訂されたようです。コンパイラーに実装されたのは改訂後の仕様です。

【2025年10月4日 追記】Apple M4は非Streaming ModeでのSVEをサポートしていないため、上記のコードはSIGILLで落ちます。「Non-streaming mode」の行をコメントアウトしてください。【/追記】

御託はいいので、コンパイルして実行してみましょう。

GCCの場合：

```
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -static -o veclen veclen.c
$ qemu-aarch64 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 16, svcntsw() = 8
Streaming mode: svcntw() = 8, svcntsw() = 8
```

Clangの場合：

```
$ clang-18 -O2 --target=aarch64-linux-gnu -march=armv9-a+sme -static -o veclen veclen.c
$ qemu-aarch64 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 16, svcntsw() = 8
Streaming mode: svcntw() = 8, svcntsw() = 8
```

【2025年10月4日 追記】Apple M4の場合

```
$ clang -O2 -march=armv8-a+sme -o veclen veclen.c
$ ./veclen
Has SME? Yes
Streaming mode: svcntw() = 16, svcntsw() = 16
```

【/追記】

出力されたベクトル長を解釈します。Non-streaming modeでの `svcntw()` は16を返しました。通常のSVEでのベクトル長がワード（32ビット）単位で16個、つまり512ビットということですね。一方、`svcntsw()` は8を返しているので、Streaming SVE vectorは256ビットのようです。

【2025年10月4日 追記】Apple M4では `svcntw()` が16を返したので、Streaming SVE vectorは512ビットのようです。$\mathrm{SVL}=512$です。【/追記】

なお、GCC 13のクロスコンパイラーが入った状態でClangでスタティックリンクすると ``undefined reference to `__arm_sme_state'`` というリンクエラーが出ました。動的リンクして `qemu-aarch64 -L /usr/aarch64-linux-gnu/` とするか、ここでの手順のようにGCC 14を入れましょう。

【2025年10月4日 追記】古いmacOS / Xcodeでも `___arm_sme_state` に関するリンクエラーが出ていました。エラーが出る場合はXcode等のバージョンを上げてみてください。【/追記】

QEMUでは `-cpu` オプションでベクトル長を変えることができます。Non-streaming時のベクトル長は `sve-default-vector-length` で、Streamingベクトル長は `sme-default-vector-length` で指定できます（詳細は[Arm CPU Features — QEMU documentation](https://www.qemu.org/docs/master/system/arm/cpu-features.html)を参照）。単位はバイトです。指定をいじってみましょう。

```
$ qemu-aarch64 -cpu max,sve-default-vector-length=256,sme-default-vector-length=128 ./veclen
Has SME? Yes
Non-streaming mode: svcntw() = 64, svcntsw() = 32
Streaming mode: svcntw() = 32, svcntsw() = 32
```

Non-streaming時のベクトル長を256バイト（2048ビット）、Streaming時のベクトル長を128バイト（1024ビット）に指定しました。`svcnt` 系の関数の出力が変わっているのが見て取れます。

## 行列乗算のための命令とレジスター

SMEのコアとなる命令は、ベクトルの外積を計算・累積するMOP（sum of outer products）命令群です。浮動小数点数で加算するMOP命令はFMOPA（floating-point sum of outer products and accumulate）という感じです。

ベクトルの外積は数式で書くと

$$
\begin{pmatrix}
a_0\\a_1\\a_2
\end{pmatrix}
\begin{pmatrix}
b_0\\b_1\\b_2
\end{pmatrix}^T
=\begin{pmatrix}
a_0b_0 & a_0b_1 & a_0b_2 \\
a_1b_0 & a_1b_1 & a_1b_2 \\
a_2b_0 & a_2b_1 & a_2b_2
\end{pmatrix}
$$

みたいなやつですね。これを繰り返して累積すれば行列乗算になるという寸法です。

乗算と加算は1回の丸め（FMA相当）で計算されます。

レジスターとしては、ZAというでかい領域が追加されます。これは$\mathrm{SVL}_{\mathrm{B}}\times\mathrm{SVL}_{\mathrm{B}}$バイトあります。$\mathrm{SVL}=128$なら256バイト（2048ビット）、$\mathrm{SVL}=512$なら4096バイト（32768ビット）となるでしょう。

ZA領域は$\mathrm{SVL}_{\mathrm{B}}$バイトごとに区切って、$\mathrm{SVL}_{\mathrm{B}}$個のベクトルとして見ることができます。それぞれのベクトルは `ZA[N]` ($0\leq N\leq \mathrm{SVL}_{\mathrm{B}}-1$)という名前がつきます。それぞれのベクトルを型（というか要素のビット数）のついたベクトルとして見る場合は、8ビット要素なら `ZA.B[N]`、16ビット要素なら `ZA.H[N]`、32ビット要素なら `ZA.S[N]` 等という風に呼びます。

ZA領域を分割してタイルと呼ばれる領域を取り出すこともできます。8ビット要素なら$\mathrm{SVL}_{\mathrm{B}}\times\mathrm{SVL}_{\mathrm{B}}$要素のタイルが1個取り出せて、これを `ZA0.B` と呼びます。32ビット要素なら$\mathrm{SVL}_{\mathrm{S}}\times\mathrm{SVL}_{\mathrm{S}}$要素のタイルが4個取り出せて、これらを `ZA0.S`, `ZA1.S`, `ZA2.S`, `ZA3.S` と呼びます。

タイルの要素は行ベクトル、列ベクトルとしてアクセスできます。これらのベクトルのことをスライスと呼びます。行ベクトルは水平方向 (horizontal) の `H` を、列ベクトルは垂直方向 (vertical) の `V` をつけて、`ZA0H.S[i]` とか `ZA2V.S[j]` という風に呼びます。

## 行列乗算の実装

行列乗算$A\times B=C$を実装してみましょう。ここでは32ビット浮動小数点数を要素とし、C言語の多次元配列の順序で要素が格納された行列に対する乗算を実装します。

まずは、基本となるリファレンス実装です。速度とかは気にしません。ループの順番を入れ替えると早くなるみたいな話もありますが、ここでは考えません。

そこそこのサイズの行列積を計算して、左上の10×10を表示します。

```c
#include <stdio.h>

void matmul(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[l][n])
{
    for (size_t i = 0; i < l; ++i) {
        for (size_t k = 0; k < n; ++k) {
            C[i][k] = 0.0f;
            for (size_t j = 0; j < m; ++j) {
                C[i][k] += A[i][j] * B[j][k];
            }
        }
    }
}

int main(void)
{
    float A[100][200];
    float B[200][150];
    float C[100][150];
    for (size_t i = 0; i < 100; ++i) {
        for (size_t j = 0; j < 200; ++j) {
            A[i][j] = (float)i + (float)j;
        }
    }
    for (size_t i = 0; i < 200; ++i) {
        for (size_t j = 0; j < 150; ++j) {
            B[i][j] = (float)i - (float)j;
        }
    }
    matmul(100, 200, 150, A, B, C);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C[i][j]);
        }
        puts("");
    }
}
```

GCCでのコンパイル・実行例：

```
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -static -o matmul matmul.c
$ qemu-aarch64 ./matmul
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06
```

SMEを使った行列乗算ですが、$A$の複数の行と$B$の複数の列をまとめて乗算し、$C$のブロックに格納できると良さそうです。行、列の個数は最大で$\mathrm{SVL}_{\mathrm{S}}$個ずつとします。

![](/images/arm-sme-1.jpg)

内側のループ（$\sum_{j} a_{ij}\cdot b_{jk}$の$j$についてのループ）はどのように回せば良いでしょうか？一つ考えられるやり方は、1列／1行ずつ処理するやり方です。

![](/images/arm-sme-2.jpg)

しかし、$B$の行を読み込むのは「連続したメモリ領域からのロード」なので問題なくできますが、$A$の列を読み込むためには飛び飛びのメモリ領域を読み込むことになるので、ギャザーが必要です。SVEにはギャザーがありますが、Streaming SVE Modeでは使えるとは限らない（拡張 `FEAT_SME_FA64` を実装していれば使える）ようです。

ここでは、$j$についてもブロック化を行い、$A$の複数の列を一括で読み込むことにします。行列は実際に転置しなくても、「行をタイルに読み込む」→「タイルから列ベクトルを取り出す」ことによっていい感じにできます。

![](/images/arm-sme-3.jpg)

まずは関数宣言です。SMEを使うので `__arm_locally_streaming` を指定し、さらにZAを使用することを表す属性をつけます。ZAの状態は呼び出し元から受け継がないので、`__arm_new("za")` を指定します。これもACLEとコンパイラーの実装で乖離しているように見えますが、ここでは現在のコンパイラーが受理してくれる記法を使います。

```c
__arm_locally_streaming
__arm_new("za")
void matmul_sme(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[l][n])
{
    // ...
}
```

$i$と$k$についてのループを書きます。マスクも用意しておきます。この辺はSVEと同様かと思います。

```c
    for (size_t i = 0; i < l; i += svcntw()) {
        svbool_t maskA = svwhilelt_b32_s64(i, l);
        for (size_t k = 0; k < n; k += svcntw()) {
            svbool_t maskB = svwhilelt_b32_s64(k, n);
            // ...
        }
    }
```

32ビットだとタイルを4つ使用できます。ここではアキュムレーター用に0番を、$A$の部分行列を保持するために1番を使用します。SMEのタイルは変数ではなく、組み込み関数へ整数定数を渡すことによって指定します。

まずは、タイルのクリアと、$A$の部分行列のロードを実装します。

```c
#define TILE_ACC 0
#define TILE_A 1
            svzero_za(); // 全てのタイルをゼロクリアする
            size_t limit_ii = min(l, i + svcntw());
            for (size_t j = 0; j < m; j += svcntw()) {
                svbool_t maskTA = svwhilelt_b32_s64(j, m);
                svzero_mask_za(17 << TILE_A); // TILE_A をゼロクリアする（指定方法が独特なので注意）
                for (size_t ii = i; ii < limit_ii; ++ii) {
                    svld1_hor_za32(TILE_A, ii /*, 0 */, maskTA, &A[ii][j]);
                }
                // ... この辺で外積の計算と加算を行う ...
            }
            // ... この辺で C の部分行列を書き込む ...
```

`svld1_hor_za32` はメモリからタイルの列ベクトルにロードする組み込み関数です。ACLEの記述と実際の実装で引数の個数が違いました。謎です。スライスのインデックスは適宜modで計算されるので、`ii - i` みたいなことをする必要はありません。

次は、外積の計算と加算です。

```c
                size_t limit_jj = min(m, j + svcntw());
                for (size_t jj = j; jj < limit_jj; ++jj) {
                    svfloat32_t a;
                    a = svread_ver_za32_f32_m(a, maskA, TILE_A, jj); // タイルから列ベクトルを取得する
                    svfloat32_t b = svld1_f32(maskB, &B[jj][k]);
                    svmopa_za32_f32_m(TILE_ACC, maskA, maskB, a, b); // 外積を計算してタイルに加算する
                }
```

`svread_ver_za32_f32_m` はベクトルを読み出す関数だと思うのですが、なぜか引数にもベクトルを指定する必要があります。謎です。プレディケートで無効になった要素が引数から取得されるんでしょうか。これもスライスのインデックスは適宜modされるので、`jj - j` とする必要はありません。

最後に、累積したタイルを$C$の部分行列として書き込みます。

```c
            for (size_t ii = i; ii < limit_ii; ++ii) {
                svst1_hor_za32(TILE_ACC, ii /*, 0 */, maskB, &C[ii][k]);
            }
```

`svst1_hor_za32` もACLEと実際の実装で引数の個数が違いました。謎です。これもスライスのインデックスは適宜modされるので、`ii - i` とする必要はありません。

完全なソースコードはこんな感じになります：

```c
#include <stdio.h>
#include <arm_sme.h>

void matmul(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[l][n])
{
    for (size_t i = 0; i < l; ++i) {
        for (size_t k = 0; k < n; ++k) {
            C[i][k] = 0.0f;
            for (size_t j = 0; j < m; ++j) {
                C[i][k] += A[i][j] * B[j][k];
            }
        }
    }
}

static inline size_t min(size_t x, size_t y) {
    return x > y ? y : x;
}

__arm_locally_streaming
__arm_new("za")
void matmul_sme(size_t l, size_t m, size_t n, const float A[l][m], const float B[m][n], float C[l][n])
{
    for (size_t i = 0; i < l; i += svcntw()) {
        svbool_t maskA = svwhilelt_b32_s64(i, l);
        for (size_t k = 0; k < n; k += svcntw()) {
            svbool_t maskB = svwhilelt_b32_s64(k, n);
#define TILE_ACC 0
#define TILE_A 1
            svzero_za();
            size_t limit_ii = min(l, i + svcntw());
            for (size_t j = 0; j < m; j += svcntw()) {
                svbool_t maskTA = svwhilelt_b32_s64(j, m);
                svzero_mask_za(17 << TILE_A);
                for (size_t ii = i; ii < limit_ii; ++ii) {
                    svld1_hor_za32(TILE_A, ii /*, 0 */, maskTA, &A[ii][j]);
                }
                size_t limit_jj = min(m, j + svcntw());
                for (size_t jj = j; jj < limit_jj; ++jj) {
                    svfloat32_t a;
                    a = svread_ver_za32_f32_m(a, maskA, TILE_A, jj);
                    svfloat32_t b = svld1_f32(maskB, &B[jj][k]);
                    svmopa_za32_f32_m(TILE_ACC, maskA, maskB, a, b);
                }
            }
            for (size_t ii = i; ii < limit_ii; ++ii) {
                svst1_hor_za32(TILE_ACC, ii /*, 0 */, maskB, &C[ii][k]);
            }
        }
    }
}

int main(void)
{
    float A[100][200];
    float B[200][150];
    float C[100][150];
    for (size_t i = 0; i < 100; ++i) {
        for (size_t j = 0; j < 200; ++j) {
            A[i][j] = (float)i + (float)j;
        }
    }
    for (size_t i = 0; i < 200; ++i) {
        for (size_t j = 0; j < 150; ++j) {
            B[i][j] = (float)i - (float)j;
        }
    }
    matmul(100, 200, 150, A, B, C);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C[i][j]);
        }
        puts("");
    }
    puts("---");
    matmul_sme(100, 200, 150, A, B, C);
    for (size_t i = 0; i < 10; ++i) {
        for (size_t j = 0; j < 10; ++j) {
            printf("%g ", C[i][j]);
        }
        puts("");
    }
}
```

【2025年10月4日 追記】上記コードの `min` 関数はStreaming Mode対応かどうか明記されていないため、インライン化されません（Apple Clangで確認）。`__arm_streaming_compatible` と `__attribute__((always_inline))` をつけるとインライン化されるようになります：

```c
__attribute__((always_inline))
static inline size_t min(size_t x, size_t y) __arm_streaming_compatible
{
    return x > y ? y : x;
}
```

【/追記】

GCCでコンパイル、QEMUで実行してみましょう。

```
$ aarch64-linux-gnu-gcc-14 -O2 -march=armv9-a+sme -static -o matmul-sme matmul-sme.c
$ qemu-aarch64 ./matmul-sme
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06
---
2.6467e+06 2.6268e+06 2.6069e+06 2.587e+06 2.5671e+06 2.5472e+06 2.5273e+06 2.5074e+06 2.4875e+06 2.4676e+06
2.6666e+06 2.6465e+06 2.6264e+06 2.6063e+06 2.5862e+06 2.5661e+06 2.546e+06 2.5259e+06 2.5058e+06 2.4857e+06
2.6865e+06 2.6662e+06 2.6459e+06 2.6256e+06 2.6053e+06 2.585e+06 2.5647e+06 2.5444e+06 2.5241e+06 2.5038e+06
2.7064e+06 2.6859e+06 2.6654e+06 2.6449e+06 2.6244e+06 2.6039e+06 2.5834e+06 2.5629e+06 2.5424e+06 2.5219e+06
2.7263e+06 2.7056e+06 2.6849e+06 2.6642e+06 2.6435e+06 2.6228e+06 2.6021e+06 2.5814e+06 2.5607e+06 2.54e+06
2.7462e+06 2.7253e+06 2.7044e+06 2.6835e+06 2.6626e+06 2.6417e+06 2.6208e+06 2.5999e+06 2.579e+06 2.5581e+06
2.7661e+06 2.745e+06 2.7239e+06 2.7028e+06 2.6817e+06 2.6606e+06 2.6395e+06 2.6184e+06 2.5973e+06 2.5762e+06
2.786e+06 2.7647e+06 2.7434e+06 2.7221e+06 2.7008e+06 2.6795e+06 2.6582e+06 2.6369e+06 2.6156e+06 2.5943e+06
2.8059e+06 2.7844e+06 2.7629e+06 2.7414e+06 2.7199e+06 2.6984e+06 2.6769e+06 2.6554e+06 2.6339e+06 2.6124e+06
2.8258e+06 2.8041e+06 2.7824e+06 2.7607e+06 2.739e+06 2.7173e+06 2.6956e+06 2.6739e+06 2.6522e+06 2.6305e+06
```

ループで書いたやつと一致してそうです。よかった！

もちろん、Clangでもコンパイルできます。

```
$ clang-18 -O2 --target=aarch64-linux-gnu -march=armv9-a+sme -static -o matmul-sme matmul-sme.c
$ qemu-aarch64 ./matmul-sme
... 略 ...
```

【2025年10月4日 追記】Apple M4の場合

```
$ clang -O2 -march=armv8-a+sme -o matmul-sme matmul-sme.c
$ ./matmul-sme
... 略 ...
```

【/追記】

## 雑感

SMEで行列乗算を実装してみました。

AppleのCPUが行列乗算用の専用命令（AMX）を持っていたことは知られていましたが、SMEという形で実装されれば利用しやすくなって嬉しいです。SMEが実装されたらApple AMXは引退ですかね。

他のプラットフォームとの比較で言うと、IntelやIBMのサーバー向けCPUはなかなか個人では手が出ませんが（逸般の誤家庭にはあるのかもしれませんが）、AppleのCPUは普通に個人でも使えます。つまり、Apple M4がMacに搭載されれば個人のパソコンでSMEプログラミングができるようになり、命令セットオタク（CPUの総合的な性能よりも命令セット星取表を重視する輩）としては楽しみです。私の手元にはIntel MacとかApple M1 Macとかが転がっていてそろそろ買い替えたい気持ちが高まっていますが、Apple M4 Macが出てくるまで待つべきかもしれません。

~~気がかりなのは、ACLEの記述とGCC 14/Clang 18の実装に差があったことです。ここに書いたコードは将来動かなくなってしまうかもしれません。アセンブリーでゴリゴリ書く人には関係ないのかもしれませんが。~~ 執筆時点のACLEは古かったようですが、ここに書いたコードは改訂後の仕様に沿っているので大丈夫そうです。

念の為ですが、私は行列乗算については素人ですし、ここで書いた行列乗算のコードにも間違いが含まれる可能性があります。この記事を参考にする場合はその点を頭に入れておいてください。

## 参考リンク

* [Arm Architecture Reference Manual for A-profile architecture](https://developer.arm.com/documentation/ddi0487/ka/?lang=en)
* [The Scalable Matrix Extension (SME), for Armv9-A](https://developer.arm.com/documentation/ddi0616/ba/?lang=en)
    * 現在はArchitecuter Reference ManualにマージされたのでこれはRETIRED扱い
* ACLE: [Arm C Language Extensions](https://arm-software.github.io/acle/main/)
* LLVM: [Support for AArch64 Scalable Matrix Extension in LLVM — LLVM 19.0.0git documentation](https://llvm.org/docs/AArch64SME.html)
* Clang: [Attributes in Clang — Clang 19.0.0git documentation](https://clang.llvm.org/docs/AttributeReference.html#aarch64-sme-attributes)
* QEMU: [Arm CPU Features — QEMU documentation](https://www.qemu.org/docs/master/system/arm/cpu-features.html)
