---
title: "LuaJITの64ビット整数"
emoji: "👻"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["lua"]
published: true
---

この記事は[Lua Advent Calendar 2022](https://qiita.com/advent-calendar/2022/lua)の21日目の記事です。

---

Lua 5.2まではLuaの数値型は倍精度浮動小数点数だけでした。Lua 5.1互換であるLuaJITもその原則に従うのですが、通常の数値型と別系統の拡張として64ビット整数も扱えます。

LuaJITの64ビット整数は、符号付き `int64_t` と符号なし `uint64_t` の2種類があります。C FFIの一環として提供されているので、名前がC言語風です。

`type` 関数を64ビット整数に適用すると、 `"number"` でも `"userdata"` でもなく `"cdata"` が返ってきます。

公式ドキュメントは

* [ffi.* API Functions](http://luajit.org/ext_ffi_api.html)
* [FFI Semantics](http://luajit.org/ext_ffi_semantics.html)

の辺りです。

この記事は執筆時点のv2.1ブランチの最新版（[8625eee71f16a3a780ec92bc303c17456efc7fb3](https://github.com/LuaJIT/LuaJIT/commit/8625eee71f16a3a780ec92bc303c17456efc7fb3)）に基づいています。リリース版（2017年のv2.1.0-beta3）とは細かい挙動が異なる可能性があります（筆者が気づいた差異についてはその旨を書いています）。

# 値の作り方・変換

整数リテラルにサフィックス `LL` または `ULL` を付けることで64ビット整数を作れます。サフィックスの大文字・小文字は区別されないようです。

```lua
local a = 1LL -- int64_tの1
local b = 0x100ull -- uint64_tの256
```

実行時の値（`number` 型）から変換したい場合は、 `ffi.new("int64_t", ...)` もしくは `ffi.typeof` で取得できるコンストラクターを使います。

```lua
local ffi = require "ffi"
local int64_t = ffi.typeof "int64_t"
local uint64_t = ffi.typeof "uint64_t"
print(int64_t(127)) -- 127LL
print(uint64_t(0x100)) -- 256ULL
```

負の数を直接 `uint64_t` に変換しようとするのは（C言語と同様に）未定義動作だそうです：

* [LuaJIT 2.1 ffi.cast("uint64_t", -1) returns wrong value #459](https://github.com/LuaJIT/LuaJIT/issues/459)

`tonumber` を使うと、64ビット整数を普通の数値（倍精度浮動小数点数）に変換できます。変換後の型は精度53ビットなので、丸めが起こる可能性があります。

```
> =tonumber(-1LL)
-1
> =tonumber(-1ULL)
1.844674407371e+19
```

# 比較

`==`, `~=`, `<`, `>`, `<=`, `>=` などのおなじみの比較演算子が使えます。

`int64_t` と `uint64_t` を比較した場合は両方とも `uint64_t` に変換された上で比較されるようです。例えば `-1LL < 1ULL` はfalseになります。

もっと言うと、64ビット整数と `number` は型が違いますが、64ビット整数に変換した上で値が等しければ `==` がtrueを返します。例えば、 `1LL == 1.1` はtrueになります。通常のLuaでは型が違うとその時点で `==` はfalseを返すので、この動作はLuaの原則から外れています。

LuaJITの64ビット整数は通常の数値 (`number`) と異なりボックス化されており、例えば `rawequal(1LL, 1LL)` はfalseになります（最適化の実装次第では同じソースに記述した同じ値のリテラルが同一になっても不思議ではありませんが、現在のLuaJITはそういう最適化はしていないようです）。

# 四則演算

`+`, `-`, `*`, `/`, `%`, `^`, 単項マイナス などの演算子が使えます。

型が混在している場合は、`uint64_t` が1つでもあれば `uint64_t` として、そうでなければ `int64_t` として扱われます。64ビット整数とLuaの数値型の混在も可能です。表にするとこんな感じです：

| 入力の型1 | 入力の型2 | 演算に使われる型・結果の型 |
|-|-|-|
| `int64_t` | `int64_t` | `int64_t` |
| `int64_t` | `number` | `int64_t` |
| `number` | `int64_t` | `int64_t` |
| `uint64_t` | `uint64_t` | `uint64_t` |
| `uint64_t` | `number` | `uint64_t` |
| `number` | `uint64_t` | `uint64_t` |
| `uint64_t` | `int64_t` | `uint64_t` |
| `int64_t` | `uint64_t` | `uint64_t` |

足し算、引き算、掛け算、割り算、冪乗のオーバーフロー時には法 `2^64` の剰余計算が行われます。C言語では符号付き整数のオーバーフローは未定義動作ですが、LuaJITの実装では符号なし整数で計算しているので関係ありません。

（ただし、単項マイナスは `int64_t` で計算しているので `-2ULL^63` でUBを踏みます。[Issue](https://github.com/LuaJIT/LuaJIT/issues/928)を立てておきました。）

割り算は整数除算が行われます（一方、Lua 5.3では整数どうしの `/` は浮動小数点数を返します）。0除算の際には、 `-2^63` が返ります。

割り算の商は0に向かって切り捨てられ、余りも `n == (n / d) * d + (n % d)` が成り立つように計算されます。Lua標準の数値に関する `%` やLua 5.3の整数除算 `//` は商がマイナス無限大に向かって切り下げられるので、挙動が違います。

```lua
> = (-1) % 2 -- Lua標準の動作（LuaJITでもこれは同じ）
1
> = (-1LL) / 2 -- LuaJITの64ビット整数の余り
0LL
> = (-1LL) % 2 -- LuaJITの64ビット整数の余り
-1LL
> = (-1) // 2 -- Lua 5.3の整数除算
-1
```

冪乗にも先ほどの変換規則が適用されて、「`uint64_t` の `uint64_t` 乗」または「`int64_t` の `int64_t` 乗」が計算されます。0の0乗は1です。後者の指数が負の場合は、次の擬似コードみたいな感じで計算されます：

```lua
function int64_pow(x, k)
  if k >= 0LL then
    -- 普通に計算する
  else -- k < 0
    if x == 0LL then
      -- ゼロ除算なので数学的には定義されない
      return 9223372036854775807LL -- 2^63 - 1
    elseif x == 1LL then
      return 1LL
    elseif x == -1LL then
      return (-1LL) ^ (2 + k % 2) -- 1LL or -1LL
    else -- |x| > 0
      return 0LL
    end
  end
end
```

この辺の細かいことは実装（[lj_carith.c](https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/lj_carith.c)）を読んだ方が早いかもしれません。

`math.abs` は64ビット整数に対しては使えません。

# ビット演算

LuaJITには、Lua 5.2のものともLua 5.3のものとも異なるビット演算ライブラリーが付属します。

* [Lua Bit Operations Module](https://bitop.luajit.org/)

これらはLuaJITの64ビット整数にも使えるようになっているようです（LuaJIT 2.1.0-beta3で確認）。

[FFI Semantics](http://luajit.org/ext_ffi_semantics.html)の最後の方に

> Other missing features:
> * Bit operations for 64 bit types.

とありますが、情報が古いようです。

シフトに関しては、シフト量の下位6ビットが利用されます。

# 文字列化

`tostring` で文字列化すると末尾に `LL` または `ULL` のサフィックスがつきます。

gitから取ってこれる最新版では `string.format` の `%d`, `%i`, `%o`, `%u`, `%x`, `%X` に対しても64ビット整数を与えられるようです。`%d` と `%i` は引数を `int64_t` に変換し、`%o`, `%u`, `%x`, `%X` は引数を `uint64_t` に変換するようです。

2017年のv2.1.0-beta3の `string.format` は64ビット整数に非対応です。

# 注意事項

LuaJITの64ビット整数はボックス化されており、同じ値でも異なる実体がありうるので、テーブルのキーには適しません。

# 関連記事

* [Lua 5.3以降の整数型](https://qiita.com/mod_poppo/items/8666badf5d4ac2268e59)
* [Luaの数値型](https://zenn.dev/mod_poppo/articles/lua-number-types)
