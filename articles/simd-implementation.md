---
title: "言語処理系にSIMD命令を実装することについて（主にx86向け）"
emoji: "📑"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["simd", "sse"]
published: true
---

私は最近、Haskell処理系であるGHCのx86 NCG (native code generator) にSIMDプリミティブを実装する作業をやっています。LLVMなんかはSIMDに対するいい感じの抽象化を提供しているのですが、それを自前でやろうというわけです。

この記事は、x86のSSE等の命令を使ってSIMDプリミティブを実装する際に使う命令などに関するメモです。「[SIMD命令比較](simd-comparison)」も参考になるかもしれません。

この記事では基本的に128ビット幅のベクトルを想定します。shuffle以外は256ビット幅や512ビット幅に応用するのは難しくないと思います。というか、256ビットとかだとAVXを仮定できるので色々楽だと思います。

## 基本：GCCやClangの出力を確認する

基本的な方針として、既存のコンパイラーの出力するアセンブリコードを参考にすると良いでしょう。

GCCでは型に `__attribute__((vector_size(16)))` などの属性をつけることで、ベクトル型や付随する組み込み関数を使うことができます。ベクトル型については四則演算が使えるので、「x86にはint8x16に使えそうな乗算命令が見当たらない」という状況でも、GCCが出力するコードを参考にすることが可能です。

詳しくは「[Vector Extensions (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-14.2.0/gcc/Vector-Extensions.html)」を参照してください。

コード例です：

```c
#include <stdint.h>

typedef int8_t int8x16_t __attribute__((vector_size(16)));

int8x16_t mul_int8x16(int8x16_t a, int8x16_t b)
{
    return a * b;
}

int8x16_t negate_int8x16(int8x16_t a)
{
    return -a;
}

typedef int16_t int16x8_t __attribute__((vector_size(16)));

int16x8_t mul_int16x8(int16x8_t a, int16x8_t b)
{
    return a * b;
}

int16x8_t shuffle_int16x8(int16x8_t a, int16x8_t b)
{
    return __builtin_shufflevector(a, b, 0, 1, 2, 3, 9, 10, 11, 4);
}

typedef int32_t int32x4_t __attribute__((vector_size(16)));

int32x4_t mul_int32x4(int32x4_t a, int32x4_t b)
{
    return a * b;
}

typedef int64_t int64x2_t __attribute__((vector_size(16)));

int64x2_t mul_int64x2(int64x2_t a, int64x2_t b)
{
    return a * b;
}
```

## 基本：Intel Intrinsics Guideを見る

x86でSIMDプログラミングをする上で、どの命令セット拡張でどういう命令が使えるのかを把握しておくことは大事です。Intel SDMでも良いですが、「[Intel® Intrinsics Guide](https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html)」が検索しやすくて便利です。

ただ、Intel Intrinsics Guideを過信するのも禁物です。例えば、執筆時点では `_mm_range_ss` がIntel Intrinsics Guideに載っていません。

## 浮動小数点演算

まずは浮動小数点数です。お馴染みの単精度 (binary32) と倍精度 (binary64) に加えて、半精度 (binary16) も紹介します。

半精度に関しては「[半精度（16ビット）浮動小数点数をC言語から使う](half-precision-floating-point)」も参考になるかもしれません。

### 足し算、引き算、掛け算、割り算、平方根

よくあるやつです。

* 単精度：`addps`, `subps`, `mulps`, `divps`, `sqrtps` (SSE)
* 倍精度：`addpd`, `subpd`, `mulpd`, `divpd`, `sqrtpd` (SSE2)
* 半精度：`vaddph`, `vsubph`, `vmulph`, `vdivph`, `vsqrtph` (AVX-512 FP16+VL OR AVX10.1)

AVX-512 FP16が実装されていない（F16C拡張しかない）古いCPUで半精度演算をやりたい場合は、半精度のベクトルを一旦単精度に変換して、単精度で演算した上で半精度に戻します。このやり方では、半精度ネイティブの命令を使った場合と（数値的に）同じ結果が得られると思います。ただ、浮動小数点数的な正しさよりも速度を追求したい場合は、C言語のexcess precision的な考え方を使って単精度→半精度→単精度の変換を省略したいかもしれません。

F16C拡張で使える変換命令は以下です：

* 単精度→半精度：`vcvtps2ph` (F16C)
* 半精度→単精度：`vcvtph2ps` (F16C)

### 符号反転と絶対値

x86には浮動小数点数の符号反転を行う命令はないので、特定のビットパターン（マスク）を作ってやってビット演算します。

符号反転なら `0x8000_0000` を4つ並べたもの（単精度）か `0x8000_0000_0000_0000` を2つ並べたもの（倍精度）を作ってやって `xorps` (SSE) / `xorpd` (SSE2) します。

絶対値は `0x7FFF_FFFF` を4つ並べたもの（単精度）か `0x7FFF_FFFF_FFFF_FFFF` を2つ並べたもの（倍精度）を作ってやって `andps` (SSE) / `andpd` (SSE2) します。

x86のSIMD命令には複数系統のビット演算命令があります（整数向け `pxor`、単精度向け `xorps`、倍精度向け `xorpd`）が、半精度に対してどれを使えばいいのかは私にはよくわかりません。既存のコンパイラーの出力を参考にしてください。

### FMA

FMA拡張でFMA命令が使えます。

関連：「[FMA (fused multiply-add) の話](https://qiita.com/mod_poppo/items/e6577df362f44a3ef8dd)」

### min, max

IEEE 754準拠のmin/maxを実装する話は「[IEEE 754-2019のminimum/maximum/minimumNumber/maximumNumber演算を実装する](implementing-ieee754-min-max)」に書きました。

x86の浮動小数点数に関する伝統的なmin/maxは

```c
float min(float x, float y) { return x < y ? x : y; }
float max(float x, float y) { return x > y ? x : y; }
```

と（例外も含めて）等価です。特に、可換ではない（第1オペランドと第2オペランドの区別がある）ので注意してください。ベクトル版は

* 単精度：`minps`, `maxps` (SSE)
* 倍精度：`minpd`, `maxpd` (SSE2)
* 半精度：`vminph`, `vmaxph` (AVX-512 FP16+VL OR AVX10.1)

です。

### 比較

* 単精度：`cmpps` (SSE)
* 倍精度：`cmppd` (SSE2)
* 半精度：`vcmpph` (AVX-512 FP16+VL OR AVX10.1)

SSE版では比較方法を8通りから選択できます。AVX版は32通りから選択できます。

浮動小数点数を比較する際には注意事項があります。

まず、2つの実数の間の関係は `=`, `<`, `>` の3択なのに対し、2つの浮動小数点数の間の関係は

* `x = y` （両方ゼロの場合は符号ビットが異なっていても等しいと判定される）
* `x < y`
* `x > y`
* 順序づけられない（unordered; 少なくとも一方がNaNである）

の4択です。

次に、signaling/non-signalingという区別があって、quiet NaNが入力に含まれていた時に浮動小数点例外を発生させるかどうかが比較方法によって異なります。signaling NaNに関しては常に例外が発生します。

「[浮動小数点数の比較について](https://qiita.com/mod_poppo/items/d30b71eb3eb957332145)」も参考にしてください。

## 整数演算

整数演算は結構厄介です。

### 足し算、引き算、符号反転

整数の通常の足し算と引き算（オーバーフローしたらwrap aroundする）には `padd{b,w,d,q}`, `psub{b,w,d,q}` を使います。これらはSSE2で利用できます。

符号反転は `pxor` で0を作ってから `psub{b,w,d,q}` します。

### 掛け算

整数の掛け算（オーバーフローしたらwrap aroundする）は少々トリッキーです。

int16x8だったら `pmullw` を使えば良いです。

int8x16はゼロベクトルとinterleaveさせてint16x8を作り、下位8ビットずつを取り出します。

int32x4とint64x2が問題です。SSE2の段階では、他に使えそうな命令は `pmuludq` くらいしかありません。`pmuludq` はint32x4の偶数番目→int64x2という風に拡幅しつつ乗算を行う関数です（上位ビットも取れます）。疑似コードで書くと次のようになります：

```c
uint64x2_t pmuludq(uint32x4_t a, uint32x4_t b)
{
    uint64_t c0 = (uint64_t)a[0] * (uint64_t)b[0];
    uint64_t c1 = (uint64_t)a[2] * (uint64_t)b[2];
    return (uint64x2_t){c0, c1};
}
```

int32x4の乗算は `pmuludq` を2回使って、下位32ビットずつ取り出せば良いです。SSE4.1になるとint32x4の乗算を一発で行える `pmulld` (SSE4.1) が使えます。

int64x2の乗算は、32ビットずつに分割し、`pmuludq` を3回使って

* 下位32ビット×下位32ビット
* 下位32ビット×上位32ビット
* 上位32ビット×下位32ビット
* （上位32ビット×上位32ビットはwrap aroundで捨てられるので不要）

を計算し、`padd` で適宜足し合わせます。

このほか、SSE4.1には `pmuldq` (SSE4.1) というのもあります。

AVX-512DQにはint64x2の乗算を一発で行える `vpmullq` (AVX-512DQ+VL OR AVX10.1) があります。

### min, max

x86で整数のmin/maxに使える命令は以下の通りです：

* 8ビット、符号あり：`pminsb`, `pmaxsb` (SSE4.1)
* 16ビット、符号あり：`pminsw`, `pmaxsw` (SSE2)
* 32ビット、符号あり：`pminsd`, `pmaxsd` (SSE4.1)
* 64ビット、符号あり：`vpminsq`, `vpmaxsq` (AVX512F+VL OR AVX10.1)
* 8ビット、符号なし：`pminub`, `pmaxub` (SSE2)
* 16ビット、符号なし：`pminuw`, `pmaxuw` (SSE4.1)
* 32ビット、符号なし：`pminud`, `pmaxud` (SSE4.1)
* 64ビット、符号なし：`vpminuq`, `vpmaxuq` (AVX512F+VL OR AVX10.1)

SSE2での符号付き8ビットなど、専用命令が使えない場合は、他の命令でエミュレートする必要があります。つまり、比較して小さい方・大きい方を選択します。比較に使える命令は次のようになります：

* 8ビット、符号あり：`pcmpgtb` (SSE2)
* 16ビット、符号あり：`pcmpgtw` (SSE2)
* 32ビット、符号あり：`pcmpgtd` (SSE2)
* 64ビット、符号あり：`pcmpgtq` (SSE4.2)

符号なし整数の比較命令はAVX-512にありますが、AVX-512を仮定できない状況で符号なし整数の比較を行いたい場合もあるでしょう。その場合、比較前に両オペランドの符号ビットを反転させればSSEにある符号付き整数の比較命令を活用できます。

64ビット整数の比較にSSE4.2が必要なのがトリッキーです。SSE2しか使えない状況（コンパイラーに `-msse4.2` 等のオプションが指定されていない場合はSSE2がベースラインとなると思います）では、スカラー命令等も使って頑張る必要がありそうです。

### その他

ビット演算とかももちろんあります。

他、型によっては飽和加算・減算もあったりします。

## シャッフルについて

GCCやClangは `__builtin_shufflevector` という組み込み関数を提供しています。動作としては、例えば `int32x4_t` なら

```c
int32x4_t shufflevector(int32x4_t a, int32x4_t b, int i0, int i1, int i2, int i3)
{
    int32_t combined[8] = {a[0], a[1], a[2], a[3], b[0], b[1], b[2], b[3]};
    return (int32x4_t){
        combined[i0],
        combined[i1],
        combined[i2],
        combined[i3],
    };
}
```

という感じです。インデックスに `-1` を指定するとそこは不定になります。GCCもClangも、インデックスはコンパイル時定数である必要があります。

### シャッフル関数があるとどういう時に便利か

`__builtin_shufflevector` は何の役に立つのでしょうか？以下に、この関数の役割を私の推測交じりで書いておきます。私はSIMDの専門家でもヘビーユーザーでもないことは断っておきます。

まず、メモリ上に

```
x0, y0, x1, y1, x2, y2, x3, y3
```

みたいな順序（array of structure）で値が並んでいたとします。これを処理する際に、`x` からなるベクトル `{x0, x1, x2, x3}` と `y` からなるベクトル `{y0, y1, y2, y3}` を作れると便利です。しかし、SIMD命令で普通に読み込むと

```c
int32_t arr[];
int32x4_t a = *(int32x4_t *)&arr[i]; // => {x0, y0, x1, y1}
int32x4_t b = *(int32x4_t *)&arr[i + 4]; // => {x2, y2, x3, y3}
```

という風に `x` と `y` が混じったベクトルができてしまいます。これに対し、`__builtin_shufflevector` があると

```c
int32x4_t x = __builtin_shufflevector(a, b, 0, 2, 4, 6); // => {x0, x1, x2, x3}
int32x4_t y = __builtin_shufflevector(a, b, 1, 3, 5, 7); // => {y0, y1, y2, y3}
```

という風に `x` だけ、`y` だけからなるベクトルを作れて便利です。

別の観点ですが、x86には `unpckl`/`unpckh` という命令群があります。単精度浮動小数点数なら `unpcklps` という具合です。この関数の動作は

```c
float32x4_t unpcklps(float32x4_t a, float32x4_t b)
{
    return (float32x4_t){a[0], b[0], a[1], b[1]};
}
```

という風になります。入力の下位要素（`unpckh` なら上位要素）同士をinterleaveさせるということです。この命令に対応する組み込み関数はどういうものであるべきでしょうか？

一つの方針は、命令ごとに対応する組み込み関数を提供することです。Intel Intrinsics Guideには `unpcklps` に対応する `_mm_unpacklo_ps` という関数が載っています。この方針だと、命令の数だけ組み込み関数を提供する必要があって大変です。

別の方針は、`unpck{l,h}` 系の命令を一般化した関数を言語処理系が提供することです。そう、`__builtin_shufflevector` 関数はx86が提供する `unpck{l,h}` 系の命令の一般化だと考えられます。この方針では、コードの移植性を向上させやすいです。例えばArmに `unpck{l,h}` 命令がなくても、コンパイラーが頑張ればプログラムの変更なしに移植できます。代わりに、コンパイラーがコード生成時に頑張る必要があります。

そういうわけで、`__builtin_shufflevector` に相当する組み込み関数があると便利そうです。

### どうやって実装するか

x86の `unpck{l,h}` 関数を挙げましたが、シャッフル関数の実装は「このパターンに使える命令があれば使う」という感じでかなりアドホックになります。コンパイラーの実装に使う言語のパターンマッチ力が試されます。

#### int8x16

使えそうな命令は

* `punpcklbw` (SSE2)
* `punpckhbw` (SSE2)
* `pslldq` (SSE2): 片方が0の場合
* `psrldq` (SSE2): 片方が0の場合
* `pshufb` (SSSE3)

あたりです。

SSSE3があると `pshufb` 命令が使えて、疑似コードで書くと

```
a' <- pshufb a {インデックスのうち、a由来のものを残し、他は255を指定する（そこの値は0になる）}
b' <- pshufb b {インデックスのうち、b由来のものを残し、他は255を指定する（そこの値は0になる）}
result <- por a', b'
```

という風に実装できます。

SSSE3が使えない場合は、汎用レジスター（GPR）上で値を32ビットずつ・64ビットずつ構築して、`movd`/`movq` でXMMに持っていき、`punpckl` 系の命令で組み立てる感じになるでしょう。指定した位置の値を取得するには `pextrw` からの `0xFF` か `0xFF00` でマスクすると良いでしょう。

#### int16x8

使えそうな命令は

* `pshuflw` (SSE2): 下位4要素に関するシャッフル
* `pshufhw` (SSE2): 上位4要素に関するシャッフル
* `punpcklwd` (SSE2)
* `punpckhwd` (SSE2)
* `pblendw` (SSE4.1)
* `pextrw` (SSE2)
* `pinsrw` (SSE2)
* int8x16でも使えた連中
    * `pslldq` (SSE2): 片方が0の場合
    * `psrldq` (SSE2): 片方が0の場合

あたりでしょう。

一般の場合は、指定された位置の値を `pextrw` で取ってきて `pinsrw` で挿入、となるでしょう。

#### int32x4

使えそうな命令は

* `pshufd` (SSE2): 一つのベクトルに対するシャッフル
* `punpckldq` (SSE2)
* `punpckhdq` (SSE2)
* `pinsrd` (SSE4.1)
* `pextrd` (SSE4.1)
* `shufps` (SSE)
* int16x8でも使えた連中
    * `pblendw` (SSE4.1)
    * `pslldq` (SSE2): 片方が0の場合
    * `psrldq` (SSE2): 片方が0の場合

あたりでしょう。

`shufps` は浮動小数点数用の命令で、整数ベクトルに使うとペナルティーがあるかもしれませんが、他に良い命令がなければ使うしかないでしょう。

#### int64x2

使えそうな命令は

* `punpcklqdq` (SSE2)
* `punpckhqdq` (SSE2)
* `pinsrq` (SSE4.1)
* `pextrq` (SSE4.1)
* int32x4でも使えた連中
    * `pshufd` (SSE2): 一つのベクトルに対するシャッフル
    * `pslldq` (SSE2): 片方が0の場合
    * `psrldq` (SSE2): 片方が0の場合

あたりでしょう。

インデックスの指定方法は（`-1` を考慮しなければ）16パターンなので、コンパイラーの実装にハードコードしても良いと思います。

#### float32x4

使えそうな命令は

* `movss` (SSE): あるベクトルの下位1要素と別のベクトルの上位3要素を混ぜるのに使える
* `shufps` (SSE)
* `movlhps` (SSE)
* `movhlps` (SSE)
* `unpcklps` (SSE)
* `unpckhps` (SSE)
* `insertps` (SSE4.1)
* `extractps` (SSE4.1)

あたりでしょう。

#### float64x2

使えそうな命令は

* `movsd` (SSE2): あるベクトルの下位1要素と別のベクトルの上位1要素を混ぜるのに使える
* `shufpd` (SSE2)
* `unpcklpd` (SSE2)
* `unpckhpd` (SSE2)

です。というか全部 `shufpd` でできるっぽい？

## 自前主義は大変だ

たまに新興の言語が「この言語はC言語よりも速い！」みたいな主張をしています。そういう主張はよく見ると、「C言語では標準ではSIMDが使えないのに対し、俺の言語では標準でSIMDが使えるので高速なプログラムを移植性を保ったまま書きやすい」という要素が含まれていたりします。

> Speaking of performance, Zig is faster than C.
>
> [Overview ⚡ Zig Programming Language](https://ziglang.org/learn/overview/)

> Benchmarks show that high-level Haskell code written using our compiler and libraries can produce code that is faster than both compiler- and hand-vectorized C.
>
> [Exploiting Vector Instructions with Generalized Stream Fusion](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/haskell-beats-C.pdf)

では、「移植性のあるSIMD」は誰が実装するのでしょうか？

LLVMを使うとSIMDの実装を全部LLVMに押し付けることができます。自動ベクトル化もありますし、使いやすいプラットフォーム非依存のベクトル型・命令もあります。

しかし、LLVMに依存して「移植性のあるSIMD」を実装すると、LLVM依存を脱却するのが困難になります。Haskell/GHCではSIMDを使うためにLLVMバックエンドが必要な状態が10年以上続きました。そして、LLVMがやっていたSIMD周りのコード生成を自前でやるのにはこの記事に書いたような面倒事があり、結構なコストがかかります。私が今回GHCに送ったパッチ（整数の算術とシャッフル）は、コード生成部だけで800行くらいの追加がありました。時間で言うと、整数のシャッフルの実装に1ヶ月くらいの時間がかかりました（余暇の時間で、ですが）。

そういうわけで、自前主義の言語処理系で使いやすいSIMDを実装するのは大変なのです。例えば、Go言語について軽く調べた感じではSIMDはなさそうです（直接アセンブリ言語を書くという手はありそうですが）。

ZigもLLVMをオプショナルな依存にしたいという話がある（[Zig's New Relationship with LLVM | Loris Cro's Blog](https://kristoff.it/blog/zig-new-relationship-llvm/)）ようですが、SIMD周りをどうするのか気になります。

（「[言語処理系がLLVMに依存することの良し悪し](https://blog.miz-ar.info/2024/12/depending-on-llvm/)」にも似たような話を書きました。）

私はGHCのNCGをSIMDに対応させたいという野望を胸に色々やっているわけですが、正直に言うと、大変なことに足を踏み出してしまったのではないかと思っています。まあ、速度にこだわらなければただ作業量が多いだけとも言えますが。
