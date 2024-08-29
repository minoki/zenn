---
title: "bfloat16 (brain float16) をC言語から使う"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: []
published: false
---

## C++での扱い

## ハードウェア実装について

### ハードウェア実装：x86編

`__bfloat16`

AVX-512 BF16 / AVX10.1

* VCVTNE2PS2BF16: 2本のベクトルを1本のベクトルに変換する。
* VCVTNEPS2BF16: 1本のベクトルを1本のベクトルに変換する。
* VDPBF16PS

変換の際は常にround to nearest (even)を使う。flush to zero

AMX-BF16

* TDPBF16PS

### ハードウェア実装：Arm編

[Clang Language Extensions — Clang 19.0.0git documentation](https://clang.llvm.org/docs/LanguageExtensions.html#half-precision-floating-point)

`__bf16`: an alternative format

`bfloat16_t`: `<arm_neon.h>` によって提供されるエイリアス。

FEAT_BF16: Armv8.2以降のオプショナルな機能で、Armv8.6で必須。

* BFCVT: Floating-point convert from single-precision to BFloat16 format (scalar)
* BFCVTN, BFCVTN2: Floating-point convert from single-precision to BFloat16 format (vector)
* BFCVTNT
* BFDOT
* BFMLALB, BFMLALT
* BFMMLA

FEAT_EBF16: Armv8.2以降のオプショナルな機能。拡張された動作を可能にする。

Round to Odd:

* FCVTXN, FCVTXN2, FCVTX, FCVTXNT
