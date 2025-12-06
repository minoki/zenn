---
title: "Luaをコンパイル先とする言語処理系を作る際のTips"
emoji: "🌙"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [lua]
published: true
---

この記事は「[Lua Advent Calendar 2025](https://qiita.com/advent-calendar/2025/lua)」および「[言語実装 Advent Calendar 2025](https://qiita.com/advent-calendar/2025/lang_dev)」の6日目の記事です。

Luaは個人的には好きな言語ですが、静的な型システムの不在や、比較的冗長な記述などにより、人や用途によっては不満を感じることもあるかもしれません。Lua自体はオープンソースで提供されていますが、開発体制はクローズド寄りなので、ユーザーの希望が通るとも限りません。しかし、Luaを組み込んだアプリケーションはいくつか存在し、プログラムをLuaとして動かしたい需要はあるでしょう。そこで、「他の言語からLuaへコンパイルしよう」という発想が出てきます。Luaへコンパイルする言語としては、

* [MoonScript](https://moonscript.org/)
* [YueScript](https://yuescript.org/)
* [Teal](https://teal-language.org/)
* [TypeScriptToLua](https://typescripttolua.github.io/)
* [Haxe](https://haxe.org/)
* [LunarML](https://github.com/minoki/LunarML)（拙作）

などがあります。

この記事では、私がLunarMLを開発する上で遭遇した、Luaへコンパイルする上でのTipsを紹介します。

## どのLuaをターゲットとするか

Luaと一口に言っても複数のバージョンや実装があり、それぞれ微妙に非互換です。他の言語、例えばECMAScriptであれば、ES5向けに書かれたプログラムは最新のECMAScriptでもほぼ同じ動作をすることが期待できますが、Lua 5.1向けに書かれたプログラムがLua 5.4で別の動作をすることは普通にあり得ます。

Luaが広く使われるようになったのはLua 5.1（2006年リリース）の頃からではないでしょうか。なので、Lua 5.1以降を前提に説明します。

2011年リリースのLua 5.2では、`_ENV` の導入、`goto` 文の導入、`bit32` ライブラリーの導入などがありました。

2015年リリースのLua 5.3では、64ビット整数の導入、ビット演算子の導入、`utf8` ライブラリーの導入などがありました。

2020年リリースのLua 5.4では、to-be-closed変数、const変数などが導入されました。

公式のLua処理系（PUC Lua）とは別に、LuaJITという処理系もあります。これは名前の通りJITコンパイルを特徴としますが、C FFIやいくつかの独自拡張を備えています。LuaJITはLua 5.1をベースとしていて、互換性を壊さない範囲でLua 5.2やLua 5.3の文法を取り込んでいます。

![](/images/lua-versions.png)

Luaをターゲットとする言語処理系を作成する場合は、どのバージョン・処理系をターゲットとするか決めておくと良いでしょう。私が作っているLunarMLでは、「Lua 5.3/5.4（これらの差異は小さいので両方で動くコードを生成しやすい）」と「LuaJIT」の二通りのターゲットを設定しています。

## 制御構造：gotoがあると便利

コンパイル先の言語にgotoがあると、複雑な制御フローのコンパイルに役立ちます。例えば、パターンマッチで共通する処理を関数に括り出すと実行時のコストがかかりますが、gotoなら低コストで済むでしょう。Luaには `continue` 文がありませんが、`goto` があると `continue` の代わりになるという側面もあります。

Luaは5.2以降でgoto文を備えています。LuaJITもgoto文を取り込んでいます。なので、これらをターゲットとする場合はgoto文が利用できます。

素のLua 5.1にはgoto文はありませんが、バイトコードを出力するようにすればgotoが実現できるという話をどこかで見た気がします。

他の制御構造としては、コルーチンの活用も考えられます。コルーチンを使って限定継続をやるという話は以前「[Luaでワンショット限定継続](https://zenn.dev/mod_poppo/articles/delimited-continuations-in-lua)」に書きました。

## データ型、特に数値

Luaの数値型周りは、バージョンによる違いが大きい部分です。

Lua 5.1、5.2、LuaJITでは、Luaのデフォルトの数値型は64ビット（仮数部53ビット）の倍精度浮動小数点数です。BigInt以前のJavaScriptと同じということです。

マイコン等への組み込みを考えると、Luaのデフォルトの数値型を整数にしたいという要望もあったようです。Lua 5.1の時代はおそらくコンパイル時に設定を変えることでそうしていたようですが、Lua 5.3では、数値型は64ビット整数と浮動小数点数のハイブリッドとなりました。ただし、コンパイル時にカスタマイズすれば整数の幅を32ビットにすることもできます。

Lua 5.3には符号なし整数型はありませんが、`math.ult` 関数や `>>` 演算子や `string.format("%u", ...)` はオペランドを符号なし整数として扱います。

Luaの数値型については以前書いた記事もあるので、紹介しておきます。

* [Lua 5.3以降の整数型](https://qiita.com/mod_poppo/items/8666badf5d4ac2268e59)
* [Luaの数値型](lua-number-types)
* [Lua 5.1/5.2/LuaJITの%演算子と浮動小数点数の罠](https://zenn.dev/mod_poppo/articles/lua51-mod-operator)

LuaJITには独自拡張として64ビット整数があります。

* [LuaJITの64ビット整数](luajit-int64)

### LuaJITとゼロの符号

LuaJITで浮動小数点数を扱う場合の注意点にも触れておきます。IEEE 754の浮動小数点数は `+0` と `-0` を区別することができ、例えば逆数等で違いが出ます。しかし、LuaJITはAArch64を含む一部のアーキテクチャーで「小さな整数値を（浮動小数点数ではなく）32ビット整数で表現する」という最適化を行なっており、符号反転の方法で `-0` を作ることができません。

```console
$ uname -m
arm64
$ lua5.1
Lua 5.1.5  Copyright (C) 1994-2012 Lua.org, PUC-Rio
> =-0
-0
> =1/(-0)  -- 負の無限大になる
-inf
> 
$ luajit
LuaJIT 2.1.1736781742 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/
JIT: ON fold cse dce fwd dse narrow loop abc sink fuse
> =-0
0
> =1/(-0)  -- 負の無限大になってほしかったが、正の無限大になった
inf
> 
```

LuaJITで `-0` を作り出すには、`0 / (-1)` を使うという手があります。

```console
$ luajit
LuaJIT 2.1.1736781742 -- Copyright (C) 2005-2025 Mike Pall. https://luajit.org/
JIT: ON fold cse dce fwd dse narrow loop abc sink fuse
> NEGATIVE_ZERO = 0 / (-1)
> =NEGATIVE_ZERO
-0
> =1/NEGATIVE_ZERO
-inf
```

LuaJITをターゲットとする言語でIEEE 754準拠の浮動小数点型を提供する場合は、符号反転には `-x` の代わりに `NEGATIVE_ZERO - x` を出力すると良いでしょう。乗算も、「結果が0だったら符号を補正する」という処理が必要になります。

## 実装上の制限

LunarMLを開発する上で遭遇した、Luaの実装上の制限についても触れておきます。いずれも、コンパイラー側の工夫で回避することができます。

### ローカル変数の個数

Luaの関数内ローカル変数の個数は、同時にスコープに入るものは200個に制限されています。つまり、

```lua
function f()
  local a1
  local a2
  ...
  local a200
  local a201 -- 201個目
end
```

というコードはパースエラーになるのです。このことは、次のコードにより実験できます：

```lua
function test(n)
  local t = {"function f()"}
  for i = 1, n do
    table.insert(t, "  local a" .. i)
  end
  table.insert(t, "end")
  return load(table.concat(t, "\n"))
end
```

```lua
$ lua5.4
Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio
> function test(n)
>>   local t = {"function f()"}
>>   for i = 1, n do
>>     table.insert(t, "  local a" .. i)
>>   end
>>   table.insert(t, "end")
>>   return load(table.concat(t, "\n"))
>> end
> test(199)
function: 0x6000018e3390
> test(200)
function: 0x6000018e4180
> test(201)
nil	[string "function f()..."]:203: too many local variables (limit is 200) in function at line 1 near 'end'
```

Luaのソースにおけるこの制限は、[lparser.c](https://www.lua.org/source/5.4/lparser.c.html#MAXVARS) に記述されています。バイトコード形式の制限によるようです。

「普通にプログラムを書いていてローカル変数を200個も使うことなんてないじゃないか」と思うかもしれませんが、インライン化などのコンパイラーの最適化を適用したらその制限に引っ掛かる可能性はありますし、ファイルのトップレベル（チャンク）も関数なので「ファイルローカル変数」を `local` 文で作りまくると大きなファイルで制限に引っかかりやすくなるということです。

対策としては、`do end` でローカル変数のスコープを狭くして同時に見える変数の個数を減らすというものがあります。

```lua
function f()
  do
    local a1
    -- a1を使う処理
  end
  do
    local a2
    -- a2を使う処理
  end
  ...
  do
    local a200
    -- a200を使う処理
  end
  do
    local a201
    -- a201を使う処理
  end
end
```

それでも制限に引っ掛かる場合は、溢れるローカル変数をテーブルに逃してやります。つまり、

```lua
function f()
  local a1
  local a2
  ...
  local a199
  local LOCAL = {}
  LOCAL[1] = ... -- a200の代わり
  LOCAL[2] = ... -- a201の代わり
end
```

とするのです。ただし、関数内関数にキャプチャーされている変数をこの方法でテーブルに逃すとループ等で書き換えが起こった時に意図しない挙動になるかもしれないので注意してください。

### 上位値の個数

Luaの関数が使う変数のうち、外側のスコープで `local` によって宣言されたものを**上位値** (upvalue) と呼びます。Luaには上位値の個数に関する制限も存在します。

この制限は `LUAI_MAXUPVALUES` または `MAXUPVAL` と呼ばれており、Lua 5.2以降では255です。しかし、Lua 5.1では60でした。また、LuaJITもLua 5.1の制限を受け継いでおり、上位値は60個に制限されています。

* Lua 5.1: [luaconf.h](https://www.lua.org/source/5.1/luaconf.h.html#LUAI_MAXUPVALUES)
* Lua 5.2: [llimits.h](https://www.lua.org/source/5.2/llimits.h.html#MAXUPVAL)
* Lua 5.3: [lfunc.h](https://www.lua.org/source/5.3/lfunc.h.html#MAXUPVAL)
* Lua 5.4: [lfunc.h](https://www.lua.org/source/5.4/lfunc.h.html#MAXUPVAL)
* LuaJIT: [lj_def.h](https://github.com/LuaJIT/LuaJIT/blob/6f21cb8ace60b297cd144c3b6925865b043095d2/src/lj_def.h#L69)

これも、制限に引っ掛かる場合は変数をテーブルに逃すことで対処できます。

### PUC Lua: `pcall` がCスタックを消費する

Luaでエラーを捕捉するには、`pcall` 関数を使います。専用のtry-catch構文の代わりに関数を使うという発想はミニマリズムを感じて嫌いではありませんが、欠点もあります。

再帰呼び出しを深く行う、次のコードを実行してみましょう：

```lua
function deep(n)
  if n >= 500 then
    return 0
  else
    return deep(n + 1) + 1
  end
end
function deep_pcall(n)
  if n >= 500 then
    return 0
  else
    local ok, result = pcall(function()
      return deep_pcall(n + 1) + 1
    end)
    if ok then
      return result
    else
      error(result, 0)
    end
  end
end
```

```lua
$ lua5.4
Lua 5.4.8  Copyright (C) 1994-2025 Lua.org, PUC-Rio
> function deep(n)
>>   if n >= 500 then
>>     return 0
>>   else
>>     return deep(n + 1) + 1
>>   end
>> end
> function deep_pcall(n)
>>   if n >= 500 then
>>     return 0
>>   else
>>     local ok, result = pcall(function()
>>       return deep_pcall(n + 1) + 1
>>     end)
>>     if ok then
>>       return result
>>     else
>>       error(result, 0)
>>     end
>>   end
>> end
> deep(0)
500
> deep_pcall(0)
C stack overflow
stack traceback:
	[C]: in function 'error'
	stdin:11: in function 'deep_pcall'
	(...tail calls...)
	[C]: in ?
```

通常の再帰呼び出しでは500段再帰してもエラーは出ませんが、`pcall` を介した方は「C stack overflow」というエラーが出ます。

LuaのVMはいわゆる**スタックレス**というやつで、Luaの関数で再帰呼び出しする分にはCスタックを消費しないのですが、`pcall` はCで実装されているためCスタックを消費します。Lua VMが消費できるCスタックは `LUAI_MAXCCALLS` という定数で決まっており、Lua 5.4のデフォルトでは200となっています：

* Lua 5.4: [llimits.h](https://www.lua.org/source/5.4/llimits.h.html#LUAI_MAXCCALLS)

`pcall` を200段もネストすることなんてないだろう、と思われるかもしれませんが、LunarMLでHaMLetというStandard ML処理系をコンパイル・実行したところ、この制限に引っかかりました。

この制限は、コルーチンを工夫して使うことにより回避できます。詳しくは「[Luaでワンショット限定継続](https://zenn.dev/mod_poppo/articles/delimited-continuations-in-lua)」を見てください。

なお、LuaJITでは `pcall` がVM自身で実装されているのか、上述の制限は（少なくとも手の届くところには）存在しません。

```lua
$ luajit
LuaJIT 2.1.1720049189 -- Copyright (C) 2005-2023 Mike Pall. https://luajit.org/
JIT: ON SSE3 SSE4.1 BMI2 fold cse dce fwd dse narrow loop abc sink fuse
> function deep(n)
>>   if n >= 500 then
>>     return 0
>>   else
>>     return deep(n + 1) + 1
>>   end
>> end
> function deep_pcall(n)
>>   if n >= 500 then
>>     return 0
>>   else
>>     local ok, result = pcall(function()
>>       return deep_pcall(n + 1) + 1
>>     end)
>>     if ok then
>>       return result
>>     else
>>       error(result, 0)
>>     end
>>   end
>> end
> 
> =deep(0)
500
> =deep_pcall(0)
500
```

### LuaJIT: 大きな関数の後半に内部関数を書くとコンパイルに失敗する

今度はLuaJIT特有の制限です。LuaJITで次の形の関数を書くと、「function too long for return fixup near 'end'」というエラーでコンパイルに失敗することがあります：

```lua
function f()
  ...巨大なコード(returnを含む、内部関数を含まない)...
  ...内部関数を含むコード...
end
```

次のコードで試せます：

```lua
function make(n)
  local t = {}
  for i = 1, n do
    table.insert(t, "i = i + i")
  end
  return load(string.format("function f(b) local i, g = 0; if b then return i end; %s; g = function() end; return t, g end", table.concat(t, ";")))
end
```

```lua
$ luajit
LuaJIT 2.1.1720049189 -- Copyright (C) 2005-2023 Mike Pall. https://luajit.org/
JIT: ON SSE3 SSE4.1 BMI2 fold cse dce fwd dse narrow loop abc sink fuse
> function make(n)
>>   local t = {}
>>   for i = 1, n do
>>     table.insert(t, "i = i + i")
>>   end
>>   return load(string.format("function f(b) local i, g = 0; if b then return i end; %s; g = function() end; return t, g end", table.concat(t, ";")))
>> end
> 
> =make(10000)
function: 0x010ad2b000
> =make(20000)
function: 0x010ad2b4f8
> =make(30000)
function: 0x010a605e80
> =make(40000)
nil	[string "function f(b) local i, g = 0; if b then retur..."]:1: function too long for return fixup near 'end'
> 
```

対策ですが、内部関数を含む巨大な関数を出力するときは、関数の先頭にダミーの内部関数

```lua
if true then
else
  local dummy = function() end
end
```

を仕込んでおくと良いでしょう。この対策を施すと、エラーが発生しなくなることがわかります：

```lua
function make2(n)
  local t = {}
  for i = 1, n do
    table.insert(t, "i = i + i")
  end
  return load(string.format("function f(b) if true then else local _ = function() end end local i, g = 0; if b then return i end; %s; g = function() end; return t, g end", table.concat(t, ";")))
end
```

```lua
> function make2(n)
>>   local t = {}
>>   for i = 1, n do
>>     table.insert(t, "i = i + i")
>>   end
>>   return load(string.format("function f(b) if true then else local _ = function() end end local i, g = 0; if b then return i end; %s; g = function() end; return t, g end", table.concat(t, ";")))
>> end
> 
> =make2(30000)
function: 0x010ad2d528
> =make2(40000)
function: 0x010a609878
> =make2(50000)
function: 0x010a608888
> =make2(60000)
function: 0x010a6059e8
> =make2(70000)
function: 0x010a60a020
```

## 終わり

LunarMLの開発で遭遇したTipsを紹介しました。この記事に書いたテクニックを使えば、10万行を超えるような巨大なLuaコードを出力することも不可能ではありません。最強のaltLua言語を作りましょう。
