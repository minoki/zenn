---
title: "Luaの数値型"
emoji: "🐈"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["lua"]
published: false
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

# Lua 5.3/5.4

* [Lua 5.3以降の整数型 - Qiita](https://qiita.com/mod_poppo/items/8666badf5d4ac2268e59)

# LuaJIT

LuaJITは基本的にLua 5.1と互換ですが、一部独自拡張が入っています。

ビット演算として、Lua 5.2の `bit32` とは無関係な[Lua BitOp](https://bitop.luajit.org/)というライブラリーが組み込まれています。

64ビット整数
<http://luajit.org/ext_ffi_api.html#literals>

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

Lua 5.3以降では `string.pack` を使うことで単精度浮動小数点数へのキャストが行えます。

LuaJITでは `ffi.typeof "float"` を使うことで単精度浮動小数点数へのキャストが行えます。
