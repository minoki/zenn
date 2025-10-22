---
title: "SIMD命令比較"
emoji: "🔖"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["simd"]
published: true
---

# 比較項目

* 持っている演算
    * [x] 四則演算
    * [x] sqrt, abs, FMA
    * [x] ビット演算
    * [ ] 変換
    * [x] 比較演算
    * [x] 選択
    * [x] gather, scatter
* マスク関連
* 対応するデータ型

# x86系 (SSE/AVX/AVX-512)

とりあえず128ビット幅のやつ。

## 浮動小数点数

TODO: bfloat16 (AVX-512)

| 演算 | float16 | float32 | float64 |
|-|-|-|-|
| 足し算 | `vaddph` (AVX512FP16+VL OR AVX10.1) | `addps` (SSE) | `addpd` (SSE2) |
| 引き算 | `vsubph` (AVX512FP16+VL OR AVX10.1) | `subps` (SSE) | `subpd` (SSE2) |
| 掛け算 | `vmulph` (AVX512FP16+VL OR AVX10.1) | `mulps` (SSE) | `mulpd` (SSE2) |
| 割り算 | `vdivph` (AVX512FP16+VL OR AVX10.1) | `divps` (SSE) | `divpd` (SSE2) |
| sqrt | `vsqrtph` (AVX512FP16+VL OR AVX10.1) | `sqrtps` (SSE) | `sqrtpd` (SSE2) |
| 絶対値 | ビット演算 | ビット演算：`andps` (SSE) | ビット演算：`andpd` (SSE2) |
| 符号反転 | ビット演算 | ビット演算：`xorps` (SSE) | ビット演算：`xorpd` (SSE2) |
| FMA | `vfmadd{132,213,231}ph` (AVX512FP16+VL OR AVX10.1) | `vfmadd{132,213,231}ps` (FMA) | `vfmadd{132,213,231}pd` (FMA) |
| 丸め[^rounding] | | `roundps` (SSE4.1) | `roundpd` (SSE4.1) |
| min (`x < y ? x : y`) | `vminph` (AVX512FP16+VL OR AVX10.1) | `minps` (SSE) | `minpd` (SSE2) |
| max (`x > y ? x : y`) | `vmaxph` (AVX512FP16+VL OR AVX10.1) | `maxps` (SSE) | `maxpd` (SSE2) |
| =, \<, ≤, unord, ≠, not \<, not ≤, ord | `vcmpph` (AVX512FP16+VL OR AVX10.1) | `cmpps` (SSE) | `cmppd` (SSE2) |
| gather（32ビットインデックス） | | `vgatherdps` (AVX2) | `vgatherdpd` (AVX2) |
| gather（64ビットインデックス） | | `vgatherqps` (AVX2) | `vgatherqpd` (AVX2) |
| scatter（32ビットインデックス） | | `vscatterdps` (AVX512F+VL) | `vscatterdpd` (AVX512F+VL) |
| scatter（64ビットインデックス） | | `vscatterdqs` (AVX512F+VL) | `vscatterqpd` (AVX512F+VL) |

[^rounding]: 丸めはceil, floor, roundeven, trunc, nearbyintの5通り、例外は有無を選べる。

## 整数

| 演算 | int8 | int16 | int32 | int64 |
|-|-|-|-|-|
| 足し算 | `paddb` (SSE2) | `paddw` (SSE2) | `paddd` (SSE2) | `paddq` (SSE2) |
| 引き算 | `psubb` (SSE2) | `psubw` (SSE2) | `psubd` (SSE2) | `psubq` (SSE2) |
| 掛け算（下位ビット） | ※ | `pmullw` (SSE2) | `pmulld` (SSE4.1) ※ | `vpmullq` (AVX512DQ+VL) ※ |
| 足し算（飽和、符号あり） | `paddsb` (SSE2) | `paddsw` (SSE2) | | |
| 引き算（飽和、符号あり） | `psubsb` (SSE2) | `psubsw` (SSE2) | | |
| 足し算（飽和、符号なし） | `paddusb` (SSE2) | `paddusw` (SSE2) | | |
| 引き算（飽和、符号なし） | `psubusb` (SSE2) | `psubusw` (SSE2) | | |
| 絶対値（符号あり） | `pabsb` (SSSE3) | `pabsw` (SSSE3) | `pabsd` (SSSE3) | `vpabsq` (AVX512F+VL OR AVX10.1) |
| min（符号あり） | `pminsb` (SSE4.1) | `pminsw` (SSE2) | `pminsd` (SSE4.1) | `vpminsq` (AVX512F+VL OR AVX10.1) |
| min（符号なし） | `pminub` (SSE2) | `pminuw` (SSE4.1) | `pminud` (SSE4.1) | `vpminuq` (AVX512F+VL OR AVX10.1) |
| max（符号あり） | `pmaxsb` (SSE4.1) | `pmaxsw` (SSE2) | `pmaxsd` (SSE4.1) | `vpmaxsq` (AVX512F+VL OR AVX10.1) |
| max（符号なし） | `pmaxub` (SSE2) | `pmaxuw` (SSE4.1) | `pmaxud` (SSE4.1) | `vpmaxuq` (AVX512F+VL OR AVX10.1) |
| bitwise not | | | | |
| bitwise and | `pand` (SSE2) | ← | ← | ← |
| bitwise or | `por` (SSE2) | ← | ← | ← |
| bitwise xor | `pxor` (SSE2) | ← | ← | ← |
| bitwise andnot | `pandn` (SSE2) | ← | ← | ← |
| left shift by scalar | | `psllw` (SSE2) | `pslld` (SSE2) | `psllq` (SSE2) |
| arithmetic right shift by scalar | | `psraw` (SSE2) | `psrad` (SSE2) | `vpsraq` (AVX512F+VL OR AVX10.1) |
| logical right shift by scalar | | `psrlw` (SSE2) | `psrld` (SSE2) | `psrlq` (SSE2) |
| left shift by vector | | `vpsllvw` (AVX512BW+VL OR AVX10.1) | `vpsllvd` (AVX2) | `vpsllvq` (AVX2) |
| arithmetic right shift by vector | | `vpsravw` (AVX512BW+VL OR AVX10.1) | `vpsravd` (AVX2) | `vpsravq` (AVX512F+VL OR AVX10.1) |
| logical right shift by vector | | `vpsrlvw` (AVX512BW+VL OR AVX10.1) | `vpsrlvd` (AVX2) | `vpsrlvq` (AVX2) |
| = | `pcmpeqb` (SSE2) | `pcmpeqw` (SSE2) | `pcmdeqd` (SSE2) | `pcmdeqq` (SSE4.1) |
| \>（符号あり） | `pcmpgtb` (SSE2) | `pcmpgtw` (SSE2) | `pcmpgtd` (SSE2) | `pcmpgtq` (SSE4.2) |
| gather（32ビットインデックス） | | | `vpgatherdd` (AVX2) | `vpgatherdq` (AVX2) |
| gather（64ビットインデックス） | | | `vpgatherqd` (AVX2) | `vpgatherqq` (AVX2) |
| scatter（32ビットインデックス） | | | `vpscatterdd` (AVX512F+VL) | `vpscatterdq` (AVX512F+VL) |
| scatter（64ビットインデックス） | | | `vpscatterqd` (AVX512F+VL) | `vpscatterqq` (AVX512F+VL) |

※SSE2で整数乗算をやる場合：

* 8ビット：16ビット整数ベクトル2つに分けて `pmullw` を2回実行する。
* 32ビット：`pmuludq`（32ビット×32ビット→64ビット）を2回適用する。
* 64ビット：`pmuludq` を3回適用する。

## ビットマスク

| 演算 | 8ビット | 16ビット | 32ビット | 64ビット |
|-|-|-|-|-|
| 密なビット列への変換 | `pmovmskb` (SSE2) | | `movmskps` (SSE) | `movmskpd` (SSE2) |
| 選択（マスクの各要素のMSBのみを使用） | `pblendvb` (SSE4.1) | | `blendvps` (SSE4.1) | `blendvpd` (SSE4.1) |

trueの要素の全てのビットが立ったマスクであれば `pblendvb` によって他の幅の選択もできる。

選択は `andp{s,d}`/`pand`, `andnp{s,d}`/`pandn`, `orp{s,d}`/`por`, `xorp{s,d}`/`pxor` などの論理命令でもできそう（速度はともかく）。

# Arm64 (ASIMD)

gather / scatterはASIMDにはなく、SVEにあるようだ。

## 浮動小数点数

TODO: bfloat16

| 演算 | float16 | float32 | float64 |
|-|-|-|-|
| 足し算 | `fadd` (FEAT_FP16) | `fadd` | `fadd` |
| 引き算 | `fsub` (FEAT_FP16) | `fsub` | `fsub` |
| 掛け算 | `fmul` (FEAT_FP16) | `fmul` | `fmul` |
| 割り算 | `fdiv` (FEAT_FP16) | `fdiv` | `fdiv` |
| sqrt | `fsqrt` (FEAT_FP16) | `fsqrt` | `fsqrt` |
| 絶対値 | `fabs` (FEAT_FP16) | `fabs` | `fabs` |
| 符号反転 | `fneg` (FEAT_FP16) | `fneg` | `fneg` |
| FMA | `fmla` (FEAT_FP16) | `fmla` | `fmla` |
| 丸め[^rounding-neon] | `frint{a,i,m,n,p,x,z}` (FEAT_FP16) | `frint{a,i,m,n,p,x,z}` | `frint{a,i,m,n,p,x,z}` |
| min（NaN伝播） | `fmin` (FEAT_FP16) | `fmin` | `fmin` |
| max（NaN伝播） | `fmax` (FEAT_FP16) | `fmax` | `fmax` |
| min（NaN無視） | `fminnm` (FEAT_FP16) | `fminnm` | `fminnm` |
| max（NaN無視） | `fmaxnm` (FEAT_FP16) | `fmaxnm` | `fmaxnm` |
| = | `fcmeq` (FEAT_FP16) | `fcmeq` | `fcmeq` |
| \> | `fcmgt` (FEAT_FP16) | `fcmgt` | `fcmgt` |
| ≥ | `fcmge` (FEAT_FP16) | `fcmge` | `fcmge` |

[^rounding-neon]: `frinta` はto nearest, ties to away、`frinti` は現在の丸めモードを使用（「不正確」例外なし）、`frintm` は負の無限大方向（floor）、`frintn` はto nearest, ties to even、`frintp` は正の無限大方向（ceil）、`frintx` は現在の丸めモードを使用（「不正確」例外あり）、`frintz` は0方向（trunc）。

## 整数

| 演算 | int8 | int16 | int32 | int64 |
|-|-|-|-|-|
| 足し算 | `add` | `add` | `add` | `add` |
| 引き算 | `sub` | `sub` | `sub` | `sub` |
| 掛け算 | `mul` | `mul` | `mul` | |
| 符号反転（符号あり） | `neg` | `neg` | `neg` | `neg` |
| 絶対値（符号あり） | `abs` | `abs` | `abs` | `abs` |
| 足し算（飽和、符号あり） | `sqadd` | `sqadd` | `sqadd` | `sqadd` |
| 引き算（飽和、符号あり） | `sqsub` | `sqsub` | `sqsub` | `sqsub` |
| 絶対値（飽和、符号あり） | `sqabs` | `sqabs` | `sqabs` | `sqabs` |
| 符号反転（飽和、符号あり） | `sqneg` | `sqneg` | `sqneg` | `sqneg` |
| 足し算（飽和、符号なし） | `uqadd` | `uqadd` | `uqadd` | `uqadd` |
| 引き算（飽和、符号なし） | `uqsub` | `uqsub` | `uqsub` | `uqsub` |
| min（符号あり） | `smin` | `smin` | `smin` | |
| min（符号なし） | `umin` | `umin` | `umin` | |
| max（符号あり） | `smax` | `smax` | `smax` | |
| max（符号なし） | `umax` | `umax` | `umax` | |
| bitwise not | `not` | ← | ← | ← |
| bitwise and | `and` | ← | ← | ← |
| bitwise or | `orr` | ← | ← | ← |
| bitwise xor | `eor` | ← | ← | ← |
| bitwise ornot | `orn` | ← | ← | ← |
| bitwise andnot | `bic` | ← | ← | ← |
| left shift by immediate | `shl` | `shl` | `shl` | `shl` |
| signed right shift by immediate | `sshr` | `sshr` | `sshr` | `sshr` |
| unsigned right shift by immediate | `ushr` | `ushr` | `ushr` | `ushr` |
| signed left shift by vector | `sshl` | `sshl` | `sshl` | `sshl` |
| signed right shift by vector | `sshr` | `sshr` | `sshr` | `sshr` |
| unsigned left shift by vector | `ushl` | `ushl` | `ushl` | `ushl` |
| unsigned right shift by vector | `ushr` | `ushr` | `ushr` | `ushr` |
| = | `cmeq` | `cmeq` | `cmeq` | `cmeq` |
| \>（符号あり） | `cmgt` | `cmgt` | `cmgt` | `cmgt` |
| \>（符号なし） | `cmhi` | `cmhi` | `cmhi` | `cmhi` |
| ≥（符号あり） | `cmge` | `cmge` | `cmge` | `cmge` |
| ≥（符号なし） | `cmhs` | `cmhs` | `cmhs` | `cmhs` |

ビットシフトの量（変数）は負の値も取れて、その場合は逆方向へのシフトとなる。

## ビットマスク

ビットマスクによる選択：`bsl` (bitwise select), `bit` (bitwise insert if true), `bif` (bitwise insert if false)

# WebAssembly

[simd/SIMD.md at main · WebAssembly/simd · GitHub](https://github.com/WebAssembly/simd/blob/main/proposals/simd/SIMD.md)

## 浮動小数点数

| 演算 | float32 | float64 |
|-|-|-|
| 足し算 | `f32x4.add` | `f64x2.add` |
| 引き算 | `f32x4.sub` | `f64x2.sub` |
| 掛け算 | `f32x4.mul` | `f64x2.mul` |
| 割り算 | `f32x4.div` | `f64x2.div` |
| sqrt | `f32x4.sqrt` | `f64x2.sqrt` |
| 絶対値 | `f32x4.abs` | `f64x2.abs` |
| 符号反転 | `f32x4.neg` | `f64x2.neg` |
| FMA | | |
| 丸め[^rounding-wasm] | `f32x4.{ceil,floor,trunc,nearest}` | `f64x2.{ceil,floor,trunc,nearest}` |
| min（NaN伝播） | `f32x4.min` | `f64x2.min` |
| max（NaN伝播） | `f32x4.max` | `f64x2.max` |
| min (`x < y ? x : y`) | `f32x4.pmin` | `f64x2.pmin` |
| max (`x > y ? x : y`) | `f32x4.pmax` | `f64x2.pmin` |
| = | `f32x4.eq` | `f64x2.eq` |
| ≠ | `f32x4.ne` | `f64x2.ne` |
| \< | `f32x4.lt` | `f64x2.lt` |
| ≤ | `f32x4.le` | `f64x2.le` |
| \> | `f32x4.gt` | `f64x2.gt` |
| ≥ | `f32x4.ge` | `f64x2.ge` |

[^rounding-wasm]: nearestはties to even

## 整数

| 演算 | int8 | int16 | int32 | int64 |
|-|-|-|-|-|
| 足し算 | `i8x16.add` | `i16x8.add` | `i32x4.add` | `i64x2.add` |
| 引き算 | `i8x16.sub` | `i16x8.sub` | `i32x4.sub` | `i64x2.sub` |
| 掛け算 | | `i16x8.mul` | `i32x4.mul` | `i64x2.mul` |
| 符号反転 | `i8x16.neg` | `i16x8.neg` | `i32x4.neg` | `i64x2.neg` |
| 絶対値 | `i8x16.abs` | `i16x8.abs` | `i32x4.abs` | `i64x2.abs` |
| 足し算（飽和、符号あり） | `i8x16.add_sat_s` | `i16x8.add_sat_s` | | |
| 足し算（飽和、符号なし） | `i8x16.add_sat_u` | `i16x8.add_sat_u` | | |
| 引き算（飽和、符号あり） | `i8x16.sub_sat_s` | `i16x8.sub_sat_s` | | |
| 引き算（飽和、符号なし） | `i8x16.sub_sat_u` | `i16x8.sub_sat_u` | | |
| min（符号あり） | `i8x16.min_s` | `i16x8.min_s` | `i32x4.min_s` | |
| min（符号なし） | `i8x16.min_u` | `i16x8.min_u` | `i32x4.min_u` | |
| max（符号あり） | `i8x16.max_s` | `i16x8.max_s` | `i32x4.max_s` | |
| max（符号なし） | `i8x16.max_u` | `i16x8.max_u` | `i32x4.max_u` | |
| left shift by scalar | `i8x16.shl` | `i16x8.shl` | `i32x4.shl` | `i64x2.shl` |
| arithmetic right shift by scalar | `i8x16.shr_s` | `i16x8.shr_s` | `i32x4.shr_s` | `i64x2.shr_s` |
| logical right shift by scalar | `i8x16.shr_u` | `i16x8.shr_u` | `i32x4.shr_u` | `i64x2.shr_u` |
| bitwise not | `v128.not` | ← | ← | ← |
| bitwise and | `v128.and` | ← | ← | ← |
| bitwise or | `v128.or` | ← | ← | ← |
| bitwise xor | `v128.xor` | ← | ← | ← |
| bitwise andnot | `v128.andnot` | ← | ← | ← |
| = | `i8x16.eq` | `i16x8.eq` | `i32x4.eq` | `i64x2.eq` |
| ≠ | `i8x16.ne` | `i16x8.ne` | `i32x4.ne` | `i64x2.ne` |
| \<（符号あり） | `i8x16.lt_s` | `i16x8.lt_s` | `i32x4.lt_s` | `i64x2.lt_s` |
| \<（符号なし） | `i8x16.lt_u` | `i16x8.lt_u` | `i32x4.lt_u` | |
| ≤（符号あり） | `i8x16.le_s` | `i16x8.le_s` | `i32x4.le_s` | `i64x2.le_s` |
| ≤（符号なし） | `i8x16.le_u` | `i16x8.le_u` | `i32x4.le_u` | |
| \>（符号あり） | `i8x16.gt_s` | `i16x8.gt_s` | `i32x4.gt_s` | `i64x2.gt_s` |
| \>（符号なし） | `i8x16.gt_u` | `i16x8.gt_u` | `i32x4.gt_u` | |
| ≥（符号あり） | `i8x16.ge_s` | `i16x8.ge_s` | `i32x4.ge_s` | `i64x2.ge_s` |
| ≥（符号なし） | `i8x16.ge_u` | `i16x8.ge_u` | `i32x4.ge_u` | |

## ビットマスク

| 演算 | 8ビット | 16ビット | 32ビット | 64ビット |
|-|-|-|-|-|
| 密なビット列への変換 | `i8x16.bitmask` | `i16x8.bitmask` | `i32x4.bitmask` | `i64x2.bitmask` |

ビットマスクによる選択：`v128.bitselect`

「密なビット列への変換」は直接対応する命令がArm64にないにも関わらず追加されている。水平方向の加算を使えばエミュレートできるらしい。

* [Add .bitmask instruction family by zeux · Pull Request #201 · WebAssembly/simd](https://github.com/WebAssembly/simd/pull/201)

# LLVM IR

(TODO)
