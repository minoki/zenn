---
title: "Armv8.7のFEAT_AFPをApple M4で試す、あるいはx86とArmの浮動小数点演算の違い"
emoji: "🔖"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["arm64", "aarch64", "浮動小数点数", "Rosetta2"]
published: true
---

## 浮動小数点数の重箱の隅

浮動小数点数に関する標準としてはIEEE 754があり、今日のプロセッサーはほとんどがそれに何らかの形で準拠しています。しかし、浮動小数点例外やNaNに関係する一部の詳細はIEEE 754では完全には定めておらず、一部が実装依存となっています。前に書いた「[NaNのビットパターンを使ってWebAssemblyからCPUの命令セットを推測する](detect-isa-via-nan)」もそういう実装依存な項目を利用してアーキテクチャーを検出するものでした。

今回は、そういう実装依存な項目に関するArmの拡張機能「FEAT_AFP」を試します。

この記事を読むには、浮動小数点例外やNaNのビットパターン等の知識が必要になります。これらについてよく知らない方は「[Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)」を読んでください。「#71 浮動小数点例外」「#74 NaNを深掘りする」などに記述があります。そして、この記事の内容は「#75 浮動小数点数のアーキテクチャごとの差異に触れる」の続きのような形となります。

もちろん、著者としては記事をなるべく自己完結的にしたいところですが、本に書いた内容を繰り返すのはしんどいです。よっぽど浮動小数点数に詳しい人は別ですが、そうでない人は「Binary Hacks Rebooted」を読むのがおすすめです。

私が動作確認に使ったのはApple M4 Pro搭載Mac mini（macOS Sequoia 15.1.1）です。x86はZen 4のRyzen搭載マシンのWSL2で検証しています。

## x86とArmの違い、そしてRosetta 2

まず、x86（SSE系）とArmの挙動の違いを見ていきます。macOSのRosetta 2でも動作確認します。

ここで挙げる違いは多くが浮動小数点例外やNaNのビットパターンに関するものであり、四則演算（加減乗除、sqrt、FMA）だけを使う限りにおいて**数値的な違いは基本的には出ません**（flush to zeroが有効な場合を除く）。記事のタイトルだけ見て「浮動小数点演算の結果ってx86とArmで違うんだ。こわい」となる人がいないことを祈ります。数値的な違いが出るのは[long doubleを使った時](https://qiita.com/mod_poppo/items/8860505f38e2997cd021)や[x87 FPUを使った時](https://qiita.com/mod_poppo/items/9588b6f425ffe4b5c7bf)ですね。あと「[浮動小数点演算の結果が環境依存なのはどんなときか](floating-point-portability)」も読んでください。

### アンダーフロー例外の発生条件

IEEE 754で認められている差異の一つがアンダーフロー例外の発生条件（判定のタイミング）です。[tininess.c](https://github.com/oreilly-japan/binary-hacks-rebooted/blob/main/ch07_data/75_floating-point-and-isa/tininess.c)（「Binary Hacks Rebooted」のサンプルプログラム）をx86とArmでそれぞれ実行してみましょう。

```
$ git clone https://github.com/oreilly-japan/binary-hacks-rebooted.git
$ cd binary-hacks-rebooted/ch07_data/75_floating-point-and-isa/

$ # x86_64での実行結果
$ gcc -o tininess tininess.c -lm
$ ./tininess
0x1.0000001p-1022 * 0x1.ffffffep-1 = 0x1p-1022, without UNDERFLOW
Tininess is detected after rounding

$ # Armでの実行結果
$ clang -o tininess tininess.c -lm
$ ./tininess
0x1.0000001p-1022 * 0x1.ffffffep-1 = 0x1p-1022, with UNDERFLOW
Tininess is detected before rounding

$ # Rosetta 2での実行結果
$ clang -arch x86_64 -o tininess-x86 tininess.c -lm
$ ./tininess-x86
0x1.0000001p-1022 * 0x1.ffffffep-1 = 0x1p-1022, without UNDERFLOW
Tininess is detected after rounding
```

詳しくは本を参照してもらうとして、実行結果がx86とArmで異なること、Rosetta 2ではx86の挙動が再現されていることが確認できました。

### FMAとinvalid operation例外

IEEE 754で認められている別の差異が、$\mathrm{FMA}(0,\infty,\mathrm{qNaN})$でのinvalid operation例外の有無です。[fma-exception.c](https://github.com/oreilly-japan/binary-hacks-rebooted/blob/main/ch07_data/75_floating-point-and-isa/fma-exception.c)をそれぞれの環境で実行してみましょう。

```
$ # x86_64での実行結果
$ gcc -o fma-exception -mfma -O2 fma-exception.c -lm
$ ./fma-exception
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, does not raise INVALID

$ # Armでの実行結果
$ clang -o fma-exception -O2 fma-exception.c
$ ./fma-exception
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, raises INVALID

$ # Rosetta 2での実行結果
$ clang -arch x86_64 -mfma -o fma-exception-x86 -O2 fma-exception.c -lm
$ ./fma-exception-x86
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, does not raise INVALID
```

詳しくは本を参照してもらうとして、実行結果がx86とArmで異なること、Rosetta 2ではx86の挙動が再現されていることが確認できました（[Apple M1が出た頃の実行結果](https://qiita.com/mod_poppo/items/fb18f2a1441e74af29a3)とは異なり、最近のRosetta 2はFMA命令に対応しているんですね）。

### NaNのビットパターン

「[NaNのビットパターンを使ってWebAssemblyからCPUの命令セットを推測する](detect-isa-via-nan)」の `detect-isa-via-nan.c` も試してみましょう。

```
$ # x86_64での実行結果
$ gcc -o detect-isa-via-nan detect-isa-via-nan.c -lm
$ ./detect-isa-via-nan
nan: 0xffc00000
y: 0x7fc00001
Guess: x86

$ # Armでの実行結果
$ clang -o detect-isa-via-nan detect-isa-via-nan.c
$ ./detect-isa-via-nan
nan: 0x7fc00000
y: 0x7fc00001
Guess: Other (Arm?)

$ # Rosetta 2での実行結果
$ clang -arch x86_64 -o detect-isa-via-nan-x86 detect-isa-via-nan.c
$ ./detect-isa-via-nan-x86
nan: 0xffc00000
y: 0x7fc00001
Guess: x86
```

これもx86とArmで実行結果が異なること、Rosetta 2ではx86の挙動が再現されていることがわかりました。つまり、生成されたNaNの符号ビットが立ちます。

### flush to zero

IEEE 754からは逸脱しますが、x86やArmは浮動小数点数の非正規化数を0とするモード、いわゆるflush to zeroを実装しています。似たような名前ですが、これの挙動もx86とArmと微妙に異なります。「Binary Hacks Rebooted」の#71 浮動小数点例外のサンプルコード[flushtozero.c](https://github.com/oreilly-japan/binary-hacks-rebooted/blob/main/ch07_data/71_floating-point-exception/flushtozero.c)を実行してみましょう。

```
$ cd ../71_floating-point-exception

$ # x86_64での実行結果
$ gcc -o flushtozero flushtozero.c -lm
$ ./flushtozero FZ
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is set.
0x0p+0

$ # Armでの実行結果
$ clang -o flushtozero flushtozero.c
$ ./flushtozero FZ
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is not set.
0x1p-1022

$ # Rosetta 2での実行結果
$ clang -arch x86_64 -o flushtozero-x86 flushtozero.c
$ ./flushtozero-x86 FZ
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is set.
0x0p+0
```

似たような機能でも、x86とArmで実行結果が異なること、Rosetta 2はx86の挙動を再現していることが確認できました。

「例外の発生条件が違う」とか「NaNのビットパターンが違う」というような話は浮動小数点演算の結果に数値として影響することは基本的にありませんが、flush to zeroの違いは実行例でもわかるように数値的に影響します。まあ、flush to zeroという環境依存な挙動を使う人が浮動小数点演算の結果の再現性にこだわるとは思えませんが。

### NaNの伝播とsignaling NaN

「Binary Hacks Rebooted」の「#74 NaNを深掘りする」では、NaNはquiet NaNとsignaling NaNの2種類に大別されること、NaNのペイロードの伝播方法はアーキテクチャに依存することを見ました。それを確かめるサンプルコード[nan-propagation.c](https://github.com/oreilly-japan/binary-hacks-rebooted/blob/main/ch07_data/74_floating-point-nan/nan-propagation.c)もあります。

では、入力の一方がquiet NaNで、もう一方がsignaling NaNの場合はどうなるでしょうか。試してみましょう。

```c
// nan-propagation2.c
#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

float make_nan(uint32_t payload) {
  float a;
  uint32_t pattern = UINT32_C(0x7fc00000) | payload;
  memcpy(&a, &pattern, 4);
  return a;
}

void print_f32rep(const char *label, float a) {
  uint32_t pattern;
  memcpy(&pattern, &a, 4);
  printf("%13s: 0x%08" PRIx32 "\n", label, pattern);
}

#if defined(__GNUC__)
__attribute__((noinline))
#endif
float add(float x, float y) {
  return x + y;
}

int main(void) {
  float qNaN1 = make_nan(0xaaaa);
  float qNaN2 = make_nan(0xbbbb);
  float sNaN1, sNaN2;
  {
    uint32_t pattern = UINT32_C(0x7f801234);
    memcpy(&sNaN1, &pattern, 4);
  }
  {
    uint32_t pattern = UINT32_C(0x7f805678);
    memcpy(&sNaN2, &pattern, 4);
  }
  print_f32rep("qNaN1 + qNaN2", add(qNaN1, qNaN2));
  print_f32rep("qNaN1 + sNaN1", add(qNaN1, sNaN1));
  print_f32rep("sNaN1 + qNaN1", add(sNaN1, qNaN1));
  print_f32rep("sNaN1 + sNaN2", add(sNaN1, sNaN2));
}
```

```
$ # x86_64での実行結果
$ gcc -o nan-propagation2 nan-propagation2.c
$ ./nan-propagation2
qNaN1 + qNaN2: 0x7fc0aaaa
qNaN1 + sNaN1: 0x7fc0aaaa
sNaN1 + qNaN1: 0x7fc01234
sNaN1 + sNaN2: 0x7fc01234

$ # Armでの実行結果
$ clang -o nan-propagation2 nan-propagation2.c 
$ ./nan-propagation2 
qNaN1 + qNaN2: 0x7fc0aaaa
qNaN1 + sNaN1: 0x7fc01234
sNaN1 + qNaN1: 0x7fc01234
sNaN1 + sNaN2: 0x7fc01234

$ # Rosetta 2での実行結果
$ clang -arch x86_64 -o nan-propagation2-x86 nan-propagation2.c
$ ./nan-propagation2-x86 
qNaN1 + qNaN2: 0x7fc0aaaa
qNaN1 + sNaN1: 0x7fc0aaaa
sNaN1 + qNaN1: 0x7fc01234
sNaN1 + sNaN2: 0x7fc01234
```

x86、Arm共に、quiet NaN同士、signaling NaN同士の場合は左のオペランドのペイロードが伝播しますが、左のオペランドがquiet NaNで、右のオペランドがsignaling NaNの場合の結果が異なります。例によってRosetta 2はx86の結果を再現しています。

Armではquiet NaNとsignaling NaNが混じっていた場合はsignaling NaNが優先され、x86は種別を問わず「一番左のNaN」が採用されます。

## Rosetta 2はどうやって浮動小数点演算の違いを処理しているか

Rosetta 2は、浮動小数点演算の挙動の重箱の隅まできっちりx86を再現していることがわかりました。これはどのように実現されているのでしょうか？

普通に考えると、演算ごとに面倒なチェック（アンダーフローの検査、NaNの検査など）が必要になります。これは普通にやるとパフォーマンス低下の要因になります。これらの重箱の隅を気にするx86アプリケーションは多くはないと思われるので、パフォーマンスを低下させてまで重箱の隅をx86準拠にすることは割に合わないように思えます。JITコンパイルを前提にして良いならチェック等を省略できる技法があるかもしれませんが、Rosetta 2はAOTコンパイルも結構やるという噂なので、チェックを省略できない場面が多いでしょう。

しかし、AppleはCPUを自前で設計でき、Rosetta 2が動くCPUはApple製だと仮定できるので、もっと簡単な解決方法があります。CPUに隠しモードを実装して、そのモードでは挙動がx86準拠になるようにすれば良いのです。当たり前ですが、Appleの設計するCPUがArm準拠であることには変わりないので、隠しモードが有効でない場合はArm準拠の挙動とします。

Apple M1では実際にそのようにしていたという記述が次のページにあります：

* [Why is Rosetta 2 fast? | dougallj](https://dougallj.wordpress.com/2022/11/09/why-is-rosetta-2-fast/)

Rosetta 2が出力するArmの機械語を見ればこれを確かめられるかもしれませんが、SIPを切ったりで面倒そうなので私はやっていません。

AppleとArmの間でどのようなやり取りがあったのかは私にはわかりませんが、このAppleの独自拡張と同等の機能がArm標準に取り入れられることになったようです。つまり、Armv8.7のFEAT_AFPです。

FEAT_AFPは “Alternate floating-point behavior”、日本語で言うなら「代替動作」ですが、実質的には「x86ライクな動作」と思って良いでしょう。x86ライクな動作をする命令セット拡張をArmが導入するのはこれが初めてではありません（「Binary Hacks Rebooted」を読んでください）。

（「Appleの独自拡張と同等の機能がArm標準に取り入れられる」のは、Apple AMX→SMEも同じような流れですね。）

## FEAT_AFP

というわけで、FEAT_AFPの解説です。と言っても重要なことはすでに述べてしまいましたが……。

FEAT_AFPによって追加される命令はありません。なので、LLVMの定義ファイルを見てもCPUが対応しているかはわかりません。

FEAT_AFPで規定される代替動作は、FPCR.AHをセットすることによって有効になります。

C言語からFPCRへのアクセス方法は「[Arm64 (AArch64) のFPCRにC言語からアクセスする](arm64-fpcr-access)」も参照してください。コードとしては次のようになります：

```c
#if defined(__clang__) || __GNUC__ >= 14
#include <arm_acle.h>
#define get_fpcr() __arm_rsr64("fpcr")
#define set_fpcr(x) __arm_wsr64("fpcr", x)
#elif __GNUC__ >= 11
#define get_fpcr() __builtin_aarch64_get_fpcr64()
#define set_fpcr(x) __builtin_aarch64_set_fpcr64(x)
#else
#error unsupported compiler
#endif

uint64_t fpcr = get_fpcr();
set_fpcr(fpcr | 0x2u);  // AH
fpcr = get_fpcr();
if (fpcr & 0x2u) {
  puts("FPCR.AH set.");
} else {
  puts("FPCR.AH not set.");
}
```

FEAT_AFPに対応していないCPUではFPCR.AHに書き込んでも当該ビットが0のままなので、それによってCPUがFEAT_AFPに対応しているかを判断できます。

### 使えるプロセッサー

私が知っている限りでは、Apple M3とApple M4がFEAT_AFPに対応しています。これらに対応するモバイル向けのプロセッサーでも使えます（M3に相当するA16など）。Apple M1は非対応でした（FEAT_AFPが発表される前に出荷されたので）。Apple M2がどうだったのか私は知りません。

このほか、Armv8.7対応を謳うCPUなら対応しているはずです。QEMUは今のところ対応していないようです。

### 効果

Arm Architecture Reference Manualを読むと、FPCR.AHの効果がわかります。「FPCR.AH」で検索すると、以下がヒットしました：

* アンダーフロー判定のタイミング
* FMAとinvalid operation例外
* flush to zero
* NaNの符号
* NaNの伝播とsignaling NaN
* FABS, FNEG
* FMIN/FMAX

これ以外にも低精度浮動小数点数や逆数・平方根の逆数の近似命令の挙動が変わるみたいなことが書かれていますが、ここでは紹介・実験しません。

列挙した中でまだ紹介していないのはFABS/FNEGとFMIN/FMAXです。

まず、x86のSSE系には符号を操作するFABS/FNEG系の命令はありません。x86のSSE系で絶対値や符号反転をやる場合は、ビット操作命令（ANDPSやXORPS）を使います。なので、x86エミュレーションの目的からするとArmのFABSやFNEG命令はどうでもいいのです。

Armの中の人が何を思ったかはわかりませんが、FEAT_AFPでFPCR.AH=1の状態ではFABSやFNEGの動作が変わり、「入力がNaNの場合は符号ビットを変化させない」という挙動になります。入力がsignaling NaNの場合、出力はsignaling NaNのままです（これは通常時と同じ）。

（いにしえのMIPSではFABSがNaNの符号ビットを変えない動作だったという噂を耳にしたことがあります。関係ないと思いますが）

浮動小数点数のmin/max演算が何種類かあるというのは「[浮動小数点数の min / max](https://qiita.com/mod_poppo/items/41a09bd40acfceec6ec8)」に書きました。x86にはMINSS/MAXSSやMINSD/MAXSDなどの命令があり、AArch64にはFMIN/FMAXなどの命令があります。これらも例によって環境によって差があり、0の符号の扱いや入力にNaNが含まれた時の挙動が異なります。そして、FEAT_AFPでFPCR.AH=1の場合はFMIN/FMAX命令がx86のMINSS系の命令の挙動を真似るようになります。AArch64にはFMINNM/FMAXNMというやつもありますが、こっちは挙動に変化がないようです。

### 実験

では、プログラムを書いて試してみましょう。これまで書いてきたプログラムをごった煮にして、コマンドラインオプションでFPCR.AHを切り替えられるようにします。一応x86でも動くようにしておきます。

```c
// arm-altfp2.c
#include <fenv.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#pragma STDC FENV_ACCESS ON

#if defined(__SSE2__)
#include <immintrin.h>
#elif defined(__aarch64__)
#if defined(__clang__)
#include <arm_acle.h>
#define get_fpcr() __arm_rsr64("fpcr")
#define set_fpcr(x) __arm_wsr64("fpcr", x)
#elif __GNUC__ >= 11
#define get_fpcr() __builtin_aarch64_get_fpcr64()
#define set_fpcr(x) __builtin_aarch64_set_fpcr64(x)
#else
#error unsupported compiler
#endif
#endif

void print_f32rep(const char *label, float a) {
  uint32_t pattern;
  memcpy(&pattern, &a, 4);
  printf("%s: 0x%08" PRIx32 "\n", label, pattern);
}

void underflow_after_rounding(void) {
  volatile double x = 0x1.0000001p-1022;
  volatile double y = 0x0.fffffffp0;
  // x * y = 0x0.ffffffffffffffp-1022 -(rounding)-> 0x1.0000000000000p-1022
  feclearexcept(FE_UNDERFLOW);
  volatile double z = x * y;
  bool underflow = fetestexcept(FE_UNDERFLOW) != 0;
  printf(
      "%a * %a, which yields %a, %s\n", x, y, z,
      underflow
          ? "raises UNDERFLOW; underflow is detected before rounding"
          : "does not raise UNDERFLOW; underflow is detected after rounding");
}

void fma_exception(void) {
#if defined(FP_FAST_FMA)
  puts("FP_FAST_FMA is defined");
#else
  puts("FP_FAST_FMA is not defined");
#endif
  feclearexcept(FE_INVALID);
  double r = fma(0, INFINITY, NAN);
  printf("fma(0, INFINITY, NAN) = %g, %s INVALID\n", r,
         fetestexcept(FE_INVALID) ? "raises" : "does not raise");
}

void flush_to_zero(void) {
  volatile double a = 0x1p-1022;
  volatile double b = 0x0.deadbeefp0;
  volatile double c = 0x0.deadbeefp-1022;
#if defined(__SSE2__)
  unsigned int csr = _mm_getcsr();
  _mm_setcsr(csr | (1u << 15)); // Set FTZ (Flush to Zero)
#elif defined(__aarch64__)
  // Set FZ (Flushing denormalized numbers to zero)
  uint64_t fpcr = get_fpcr();
  set_fpcr(fpcr | (1u << 24));
#endif
  feclearexcept(FE_UNDERFLOW);
  volatile double x = a * b;
  printf("FE_UNDERFLOW is %sset.\n",
         fetestexcept(FE_UNDERFLOW) ? "" : "not ");
  printf("%a\n", x);
  feclearexcept(FE_UNDERFLOW);
  volatile double y = a - c;
  printf("FE_UNDERFLOW is %sset.\n",
         fetestexcept(FE_UNDERFLOW) ? "" : "not ");
  printf("%a\n", y);
#if defined(__SSE2__)
  _mm_setcsr(csr);
#elif defined(__aarch64__)
  set_fpcr(fpcr);
#endif
}

void nan_bitpattern(void) {
  volatile float a = 0.0;
  print_f32rep("0.0 / 0.0", a / a);
}

void nan_propagation(void) {
  float qNaN;
  float sNaN;
  uint32_t q = UINT32_C(0x7fc01234);
  uint32_t s = UINT32_C(0x7f80cafe);
  memcpy(&qNaN, &q, 4);
  memcpy(&sNaN, &s, 4);
  print_f32rep("qNaN(1234) + sNaN(cafe)", qNaN + sNaN);
}

void test_sign_of_nan(const char *s, float x) {
  uint32_t pattern;
  memcpy(&pattern, &x, 4);
  printf("%s: 0x%08" PRIx32 "\n", s, pattern);
  feclearexcept(FE_INVALID);
  float neg = -x;
  int invalid = fetestexcept(FE_INVALID);
  memcpy(&pattern, &neg, 4);
  printf("-%s: 0x%08" PRIx32 ", INVALID=%d\n", s, pattern, !!invalid);
  feclearexcept(FE_INVALID);
  float a = fabsf(x);
  invalid = fetestexcept(FE_INVALID);
  memcpy(&pattern, &a, 4);
  printf("fabsf(%s): 0x%08" PRIx32 ", INVALID=%d\n", s, pattern, !!invalid);
}

void sign_of_nan(void) {
  float qNaN;
  float sNaN;
  uint32_t q = UINT32_C(0xffc01234);
  uint32_t s = UINT32_C(0xff80cafe);
  memcpy(&qNaN, &q, 4);
  memcpy(&sNaN, &s, 4);
  test_sign_of_nan("qNaN", qNaN);
  test_sign_of_nan("sNaN", sNaN);
}

void arm_fmin_test(const char *xs, float x, const char *ys, float y) {
  float z;
  feclearexcept(FE_INVALID);
#if defined(__SSE2__)
  __m128 xx = _mm_set_ss(x);
  __m128 yy = _mm_set_ss(y);
  _mm_store_ss(&z, _mm_min_ss(xx, yy));
#elif defined(__aarch64__)
  asm volatile("fmin %s0, %s1, %s2" : "=w"(z) : "w"(x), "w"(y));
#else
  z = 0.0f / 0.0f;
#endif
  int invalid = fetestexcept(FE_INVALID);
  uint32_t zp;
  memcpy(&zp, &z, 4);
  printf("fmin(%s, %s) = %g (0x%08" PRIx32 "), INVALID=%d\n", xs, ys, z, zp, !!invalid);
}

void fmin_test(void) {
  float qNaN;
  float sNaN;
  uint32_t q = UINT32_C(0x7fc01234);
  uint32_t s = UINT32_C(0x7f80cafe);
  memcpy(&qNaN, &q, 4);
  memcpy(&sNaN, &s, 4);
  arm_fmin_test("0.0", 0.0f, "-0.0", -0.0f);
  arm_fmin_test("-0.0", -0.0f, "0.0", 0.0f);
  arm_fmin_test("3.0", 3.0f, "qNaN", qNaN);
  arm_fmin_test("qNaN", qNaN, "3.0", 3.0);
  arm_fmin_test("qNaN(1234)", qNaN, "sNaN(cafe)", sNaN);
  arm_fmin_test("sNaN(cafe)", sNaN, "qNaN(1234)", qNaN);
}

int main(int argc, char *argv[]) {
#if defined(__aarch64__)
  if (argc > 1 && strcmp(argv[1], "AH") == 0) {
    uint64_t fpcr = get_fpcr();
    set_fpcr(fpcr | 0x2u);  // AH
    fpcr = get_fpcr();
    if (fpcr & 0x2u) {
      puts("FPCR.AH set.");
    } else {
      puts("FPCR.AH not supported.");
    }
  } else {
    puts("Not setting FPCR.AH");
  }
#else
  puts("Not AArch64");
#endif
  puts("=== underflow ===");
  underflow_after_rounding();
  puts("=== FMA ===");
  fma_exception();
  puts("=== flush to zero ===");
#if defined(__SSE2__) || defined(__aarch64__)
  flush_to_zero();
#else
  puts("Unsupported platform");
#endif
  puts("=== bit pattern of NaN ===");
  nan_bitpattern();
  puts("=== propagation of NaN ===");
  nan_propagation();
  puts("=== FNEG, FABS of NaN ===");
  sign_of_nan();
  puts("=== FMIN ===");
#if defined(__SSE2__) || defined(__aarch64__)
  fmin_test();
#else
  puts("Unsupported platform");
#endif
}
```

Armでの実行結果（FPCR.AH=0）：

```
$ clang -o arm-altfp2 arm-altfp2.c
$ ./arm-altfp2
Not setting FPCR.AH
=== underflow ===
0x1.0000001p-1022 * 0x1.ffffffep-1, which yields 0x1p-1022, raises UNDERFLOW; underflow is detected before rounding
=== FMA ===
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, raises INVALID
=== flush to zero ===
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is not set.
0x1p-1022
=== bit pattern of NaN ===
0.0 / 0.0: 0x7fc00000
=== propagation of NaN ===
qNaN(1234) + sNaN(cafe): 0x7fc0cafe
=== FNEG, FABS of NaN ===
qNaN: 0xffc01234
-qNaN: 0x7fc01234, INVALID=0
fabsf(qNaN): 0x7fc01234, INVALID=0
sNaN: 0xff80cafe
-sNaN: 0x7f80cafe, INVALID=0
fabsf(sNaN): 0x7f80cafe, INVALID=0
=== FMIN ===
fmin(0.0, -0.0) = -0 (0x80000000), INVALID=0
fmin(-0.0, 0.0) = -0 (0x80000000), INVALID=0
fmin(3.0, qNaN) = nan (0x7fc01234), INVALID=0
fmin(qNaN, 3.0) = nan (0x7fc01234), INVALID=0
fmin(qNaN(1234), sNaN(cafe)) = nan (0x7fc0cafe), INVALID=1
fmin(sNaN(cafe), qNaN(1234)) = nan (0x7fc0cafe), INVALID=1
```

Armでの実行結果（FPCR.AH=1）：

```
$ ./arm-altfp2 AH
FPCR.AH set.
=== underflow ===
0x1.0000001p-1022 * 0x1.ffffffep-1, which yields 0x1p-1022, does not raise UNDERFLOW; underflow is detected after rounding
=== FMA ===
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, does not raise INVALID
=== flush to zero ===
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is set.
0x0p+0
=== bit pattern of NaN ===
0.0 / 0.0: 0xffc00000
=== propagation of NaN ===
qNaN(1234) + sNaN(cafe): 0x7fc01234
=== FNEG, FABS of NaN ===
qNaN: 0xffc01234
-qNaN: 0xffc01234, INVALID=0
fabsf(qNaN): 0xffc01234, INVALID=0
sNaN: 0xff80cafe
-sNaN: 0xff80cafe, INVALID=0
fabsf(sNaN): 0xff80cafe, INVALID=0
=== FMIN ===
fmin(0.0, -0.0) = -0 (0x80000000), INVALID=0
fmin(-0.0, 0.0) = 0 (0x00000000), INVALID=0
fmin(3.0, qNaN) = nan (0x7fc01234), INVALID=1
fmin(qNaN, 3.0) = 3 (0x40400000), INVALID=1
fmin(qNaN(1234), sNaN(cafe)) = nan (0x7f80cafe), INVALID=1
fmin(sNaN(cafe), qNaN(1234)) = nan (0x7fc01234), INVALID=1
```

x86_64での実行結果：

```
$ gcc -mfma -O -o arm-altfp2 arm-altfp2.c -lm
$ ./arm-altfp2
Not AArch64
=== underflow ===
0x1.0000001p-1022 * 0x1.ffffffep-1, which yields 0x1p-1022, does not raise UNDERFLOW; underflow is detected after rounding
=== FMA ===
FP_FAST_FMA is defined
fma(0, INFINITY, NAN) = nan, does not raise INVALID
=== flush to zero ===
FE_UNDERFLOW is set.
0x0p+0
FE_UNDERFLOW is set.
0x0p+0
=== bit pattern of NaN ===
0.0 / 0.0: 0xffc00000
=== propagation of NaN ===
qNaN(1234) + sNaN(cafe): 0x7fc01234
=== FNEG, FABS of NaN ===
qNaN: 0xffc01234
-qNaN: 0x7fc01234, INVALID=0
fabsf(qNaN): 0x7fc01234, INVALID=0
sNaN: 0xff80cafe
-sNaN: 0x7f80cafe, INVALID=0
fabsf(sNaN): 0x7f80cafe, INVALID=0
=== FMIN ===
fmin(0.0, -0.0) = -0 (0x80000000), INVALID=0
fmin(-0.0, 0.0) = 0 (0x00000000), INVALID=0
fmin(3.0, qNaN) = nan (0x7fc01234), INVALID=1
fmin(qNaN, 3.0) = 3 (0x40400000), INVALID=1
fmin(qNaN(1234), sNaN(cafe)) = nan (0x7f80cafe), INVALID=1
fmin(sNaN(cafe), qNaN(1234)) = nan (0x7fc01234), INVALID=1
```

ArmでFPCR.AH=1とすると挙動の多く（つまり、FABS/FNEGに関するもの以外）がx86_64準拠になることがわかりました。

## おしまい

浮動小数点数の重箱の隅をつつきまくりました。この記事を読んで「わけがわからないよ」となった方は「Binary Hacks Rebooted」を読み直してください。

参照した記事の人も書いていますが、ArmがJavaScript用の命令（参照：「[ArmにあるというJavaScript専用命令とは何か、あるいは浮動小数点数を整数に変換する方法について](https://qiita.com/mod_poppo/items/66663d9a05790579b5e4)」）を追加した時は命令名に「JavaScript」を入れたのに、x86を真似る命令を入れる時は回りくどい言い方になるのは面白いですね。そのおかげで私のような人間が「これは実はx86の挙動を真似ているんだよ！」と記事にできる訳ですが。かくいう私も最初にFEAT_AFPを知った時は意味がわからず、「代替動作を使いたい個々の命令の前後でFPCR.AHを操作するのか？」とか考えていました。

Rosetta 2はCPUの隠し機能というせこい手段でエミュレーションしていることが推測されました。こういうせこい手段を使えない（と思われる）Windows on Armのx86エミュレーターがどうしているのか気になっていますが、私は実機を持っていないので、誰か試せる人がいたら試して報告していただけると嬉しいです（Armマシンで動くx86バイナリーの挙動がx86のものになっているのか、それともエミュレートを諦めてArmの挙動になっているのか）。
