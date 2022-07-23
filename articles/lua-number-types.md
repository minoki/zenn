---
title: "Luaの数値型"
emoji: "🌕"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["lua"]
published: true
---

# Lua 5.2まで

Lua 5.2までは、Luaの数値型は全て浮動小数点数（典型的には倍精度浮動小数点数）でした。

組み込み環境向けの場合はコンパイル時にカスタマイズすることで整数型を使うようにもできました。

倍精度浮動小数点数の場合は、 $-2^{53}$ 以上 $2^{53}$ 以下の整数値をサブセットとして表現できます。特に、符号付き32ビット整数 $[-2^{31},2^{31}-1]$ や符号なし32ビット整数 $[0,2^{32}-1]$ を表現できます。

この辺は伝統的なJavaScriptと同じですね。

32ビット整数の演算をwrap-aroundさせたい場合は、加減算の場合は

```lua
function add32(x, y)
  return (x + y) % 0x100000000
end
function sub32(x, y)
  return (x - y) % 0x100000000
end
```

とすれば良いです。乗算は上位ビットと下位ビットを分けるような工夫が必要です。整数除算は

```lua
function div32(x, y)
  return math.floor(x / y)
end
function quot32(x, y)
  return (math.modf(x / y))
end
```

とすれば良いでしょう。

## Lua 5.2のbit32

入力として $(-2^{51},2^{51})$ （開区間）の範囲の整数を受け付け、$2^{32}$で割った余りをビット演算に使用します。結果は区間 $[0,2^{32}-1]$、つまり符号なし整数として返されます。

* `bit32.arshift(x, disp)`
    * 算術右シフト
    * シフト量 `disp` が負の場合は左シフトが行われます。
    * シフト量の絶対値が31より大きい場合の結果は 0 または 0xFFFFFFFF となります。
* `bit32.band(...)`
* `bit32.bnot(x)`
* `bit32.bor(...)`
* `bit32.btest(...)`
    * `bit32.band(...) ~= 0` みたいなやつです。
* `bit32.bxor(...)`
* `bit32.extract(n, field [, width])`
* `bit32.replace(n, v, field [, width])`
* `bit32.lrotate(x, disp)`
* `bit32.lshift(x, disp)`
    * 左シフト
    * シフト量 `disp` が負の場合は右シフトが行われます。
    * シフト量の絶対値が31より大きい場合の結果は 0 となります。
* `bit32.rrotate(x, disp)`
* `bit32.rshift(x, disp)`
    * 論理右シフト
    * シフト量 `disp` が負の場合は左シフトが行われます。
    * シフト量の絶対値が31より大きい場合の結果は 0 となります。

# Lua 5.3/5.4

* [Lua 5.3以降の整数型 - Qiita](https://qiita.com/mod_poppo/items/8666badf5d4ac2268e59)

を参照してください。

ビット演算に関して、算術右シフトの演算子はありません。 `x // (1 << n)` を使うことになるでしょう。

# LuaJIT

LuaJITは基本的にLua 5.1と互換ですが、一部独自拡張が入っています。

## ビット演算

ビット演算として、Lua 5.2の `bit32` とは無関係な[Lua BitOp](https://bitop.luajit.org/)というライブラリーが組み込まれています。なお、登場したのはLua BitOpの方がLua 5.2よりも早いです。

Lua BitOpは数値型が符号付き32ビット整数な環境も考慮しています。そのため、たとえ各演算の内部で符号なし32ビット整数が使われていたとしても、返り値は符号付き32ビット整数となります。

Luaの数値型が倍精度浮動小数点数の場合、$\pm 2^{51}$の範囲の入力が許容されます。入力は実装依存の丸め方法で整数に変換され、下位32ビットが使用されます。

関数一覧：

* `bit.tobit(x)`
    * 与えられた数を符号付き32ビット整数に正規化します。
* `bit.tohex(x [, n])`
    * 与えられた数を指定された桁数（`n` の絶対値）で十六進文字列化します。
    * `n` が正の場合は小文字、負の場合は大文字が使用されます。
    * `n` の絶対値は1以上8以下で、指定された桁数では引数を文字列化できない場合は下位 `|n|` 桁だけが返されます。
* `bit.bnot(x)`
* `bit.band(x1 [, x2...])`
* `bit.bor(x1 [, x2...])`
* `bit.bxor(x1 [, x2...])`
* `bit.lshift(x, n)`
    * 論理左シフト
    * `n` の下位5ビットのみが使用されます。
* `bit.rshift(x, n)`
    * 論理右シフト
    * `n` の下位5ビットのみが使用されます。
* `bit.arshift(x, n)`
    * 算術右シフト
    * `n` の下位5ビットのみが使用されます。
* `bit.rol(x, n)`
    * `n` の下位5ビットのみが使用されます。
* `bit.ror(x, n)`
    * `n` の下位5ビットのみが使用されます。
* `bit.bswap`

LuaJIT 2.1系の場合は、後述する64ビット整数もLua BitOpで扱えるようです。その場合、`tobit` は下位32ビットを符号付き32ビット整数として返し、シフト演算は下位6ビットが使用されるようです。`band` 等に複数の型を混ぜて与えた場合、`uint64_t`, `int64_t`, 符号付き32ビット整数の順に優先されるようです。

Lua 5.2のbit32と比較すると、返り値の範囲が $[0,2^{32}-1]$（符号なし整数）か $[-2^{31},2^{31}-1]$（符号あり整数）かというのと、シフト演算のシフト量が範囲外の場合の挙動が異なります。

## 64ビット整数

LuaJITのFFIライブラリーでは（ボックス化された）64ビット整数を扱えます。

* [FFI Library](http://luajit.org/ext_ffi.html)

コンストラクター `uint64_t`, `int64_t` は次のように取得できます：

```lua
local ffi = require("ffi")
local uint64_t = ffi.typeof("uint64_t")
local int64_t = ffi.typeof("int64_t")
print(uint64_t(1)) --> 1ULL
print(int64_t(1)) --> 1LL
```

文字列化の際には `ULL`, `LL` のサフィックスがつきます。また、これらのサフィックスを使ったリテラルをソース中に記述できます（パーサーの拡張 <http://luajit.org/ext_ffi_api.html#literals>）。

64ビット整数には通常の算術演算（`+`, `-`, `*`, `/`, `%`, `^`, 単項マイナス）やLua BitOpの演算が使えます。引数の型を混在させた場合は、`uint64_t`, `int64_t`, 通常の浮動小数点数の順で優先されます。つまり、どちらか一方が `uint64_t` であればもう一方も `uint64_t` に変換した上で演算が行われます。

比較は通常の大小比較 `<` が使えます。片方が `uint64_t` であればもう一方も `uint64_t` に変換した上で比較されます。

64ビット整数を通常の浮動小数点数に変換するには `tonumber` が使えます。

## dual number modeと負のゼロ

LuaJITは一部のアーキテクチャー（AArch64を含む）上では**dual number mode**を使用します。これは、符号付き32ビット整数で表現できる値を内部的に32ビット整数で表すものです。

dual number modeでは単項マイナスと乗算がIEEE 754の「負のゼロ」を考慮しない場合があります。

単項マイナスの代わりに `0 / (-1) - x` を、乗算の代わりに

```lua
function float_mul(x, y)
  local z = x * y
  if z == 0 then
    if x < 0 then
      return 0 / (-1) * y
    elseif y < 0 then
      return 0 / (-1) * x
    end
  end
  return z
end
```

というようなコードを使うことで回避できます。

# おまけ：32ビット浮動小数点数のエミュレート

JavaScriptの `Math.fround` みたいな、倍精度浮動小数点数を単精度浮動小数点数にキャストする関数があれば単精度浮動小数点数の演算をエミュレートできます。

Lua 5.3以降では `string.pack` を使うことで単精度浮動小数点数へのキャストが行えます：

```lua
function to_f32(x)
  return (string.unpack("f", string.pack("f", x)))
end
print(to_f32(3.14)) --> 3.1400001049042
```

LuaJITでは `ffi.typeof "float"` を使うことで単精度浮動小数点数へのキャストが行えます：

```lua
local ffi = require "ffi"
local float = ffi.typeof "float"
function to_f32(x)
  return tonumber(float(x))
end
print(to_f32(3.14)) --> 3.1400001049042
```
