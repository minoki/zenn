---
title: "NaNのビットパターンを使ってWebAssemblyからCPUの命令セットを推測する"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [WebAssembly, 浮動小数点数]
published: true
---

浮動小数点数にはIEEE 754という標準規格がありますが、規格の範囲内であっても僅かに実装による差異が認められています。その一つがNaNのビットパターンです。

WebAssemblyはNaNのビットパターンに関しては実装依存の結果を許容しており、Wasmランタイムは特に深い理由がなければホストCPUの浮動小数点命令をそのまま用いると考えられるため、結果として「WasmからNaNのビットパターンを観測することでホストのCPUの命令セットを推測する」ことができます。

ここでは、「x86(\_64)」「RISC-V」「その他（Arm）」の3択で判定することを目指します。

## 判定コード

判定するコードは以下です：

```c
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

float u32_to_f32(uint32_t x)
{
    float y;
    memcpy(&y, &x, 4);
    return y;
}

uint32_t f32_to_u32(float x)
{
    uint32_t y;
    memcpy(&y, &x, 4);
    return y;
}

int main(void)
{
    volatile float zero = 0.0f;
    float nan = zero / zero;
    uint32_t nan_pattern = f32_to_u32(nan);
    float crafted_nan = u32_to_f32(0x7fc00001);
    float y = crafted_nan + 1.0f;
    uint32_t y_pattern = f32_to_u32(y);
    printf("nan: 0x%08" PRIx32 "\n", nan_pattern);
    printf("y: 0x%08" PRIx32 "\n", y_pattern);
    if (nan_pattern == 0xffc00000) {
        puts("Guess: x86");
    } else if (y_pattern == 0x7fc00000) {
        puts("Guess: RISC-V");
    } else {
        puts("Guess: Other (Arm?)");
    }
}
```

wasi-sdkが入っていれば次のコマンドでコンパイルできます：

```
${WASI_SDK_PATH}/bin/clang --sysroot=${WASI_SDK_PATH}/share/wasi-sysroot detect-isa-via-nan.c -o detect-isa-via-nan.wasm
```

コンパイルする環境を整えるのが面倒な人向けに、コンパイル済みのバイナリーを <https://miz-ar.info/detect-isa-via-nan.wasm> に用意しています。

```
$ curl -LO https://miz-ar.info/detect-isa-via-nan.wasm
$ wasmtime detect-isa-via-nan.wasm
```

## 実行例

x86_64での実行例は以下です：

```
$ uname -m
x86_64
$ wasmtime detect-isa-via-nan.wasm
nan: 0xffc00000
y: 0x7fc00001
Guess: x86
```

AArch64での実行例は以下です：

```
$ uname -m
arm64
$ wasmtime detect-isa-via-nan.wasm 
nan: 0x7fc00000
y: 0x7fc00001
Guess: Other (Arm?)
```

QEMUを使ってRISC-Vとして動作させた実行例は以下です：

```
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ wasmtime-v25.0.0-riscv64gc-linux/wasmtime detect-isa-via-nan.wasm
nan: 0x7fc00000
y: 0x7fc00000
Guess: RISC-V
```

同一のWasmバイナリーを使って3択が判定できていることがわかります。

## 仕組み

浮動小数点数演算の環境依存性については「Binary Hacks Rebooted」に書きました（宣伝）。

* [O'Reilly Japan - Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)
    * \#70 浮動小数点数のビット列表現を理解する
    * \#74 NaNを深掘りする
    * \#75 浮動小数点数のアーキテクチャごとの差異に触れる

要点だけ書いておくと、

* 通常の数同士の演算で（invalid operation例外により）NaNが発生した場合、x86では符号ビットの立ったNaNが返るが、それ以外のアーキテクチャでは符号ビットの立っていないNaNが返ってくる
* RISC-VではNaNがオペランドにある場合、入力のペイロードは無視されて、常に「符号ビットが立っておらず、仮数部下位は（quiet NaNを表す）最上位を除いて全て0であるようなNaN」（RISC-VではこれをカノニカルなNaNと呼ぶ）が返る

となります。

本にも書いたように、浮動小数点例外に触れるともっと深く環境依存性に切り込めるのですが、Wasmからは浮動小数点例外には触れないのでNaNのビットパターンを利用した感じです。

浮動小数点数から固定長整数への変換も環境依存性がある要素ですが、WebAssemblyでは例外的な場合はtrapすることとなっているようです。実際に試してみても、Cコンパイラーが適切な処置を施すためか、環境依存性のある結果は得られませんでした。

## 意義

WebAssemblyといえども若干の環境依存性があることがわかりました。何らかの事情で真に環境非依存な実行環境が必要な場合は、WebAssemblyランタイムに細工をして演算結果のNaNのビットパターンの観測時に常に同じビットパターンが返るようにする必要があります。

## リンク

同様の原理でJavaScriptからCPUがIntelかどうかを判定する記事が以前にありました：

* [JavaScript で CPU が Intel かどうかを判定する（ついでに JIT を検知する）](http://nmi.jp/2023-01-11-Detecting-Intel-Arch-in-JavaScript)
