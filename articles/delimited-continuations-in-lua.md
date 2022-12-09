---
title: "Luaでワンショット限定継続"
emoji: "👏"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["lua"]
published: true
---

これは[言語実装 Advent Calendar 2022](https://qiita.com/advent-calendar/2022/lang_dev)の9日目の記事です。Luaをターゲットとする言語を実装する際に役立つかもしれない情報です。

---

私が作っているStandard ML処理系LunarMLでは限定継続を提供しています。限定継続についての話はブログの方に何回か書きました：

* [LunarMLと継続](https://blog.miz-ar.info/2022/05/lunarml-and-continuations/)
* [限定継続いろいろ](https://blog.miz-ar.info/2022/10/delimited-continuations/)

これまでのLunarMLではJS-CPSバックエンドでCPS変換による限定継続の実装を行なっていましたが、最近Luaバックエンド（の亜種）でコルーチンによるワンショット限定継続の実装を追加しました。

この記事では、Luaのコルーチンを使った限定継続の実装の解説を試みます。

# Luaのコルーチン

コルーチンというのは途中で中断できる関数みたいなやつのことです（ざっくり）。

いろんな言語でのコルーチンについてはn月刊ラムダノートの遠藤さんの記事を読んでください：

* [n月刊ラムダノート Vol.1, No.1(2019) – 技術書出版と販売のラムダノート](https://www.lambdanote.com/collections/frontpage/products/nmonthly-vol-1-no-1-2019)

ここではLuaのコルーチンを扱います。例を見てみましょう：

```lua
local co = coroutine.create(function()
  print("Hello")
  local a = coroutine.yield(1 + 2)
  print(a..a..a)
end)
print("created a coroutine")
local _, r1 = coroutine.resume(co)
print("result 1", r1)
coroutine.resume(co, "ABC")
```

このLuaコードの出力は

```
created a coroutine
Hello
result1 3
ABCABCABC
```

となります。

`coroutine.create` がコルーチンを作る関数、 `coroutine.yield` はコルーチンを中断する関数、 `coroutine.resume` はコルーチンを起動・再開する関数です。

`coroutine.create` を呼んだ段階ではコルーチンの処理は実行されません。`coroutine.resume` を呼ぶことによってコルーチンの処理が実行されます。コルーチンの中で `coroutine.yield` が呼ばれると処理がそこで中断して、制御が `coroutine.resume` の呼び出し元に返ります。もう一度 `coroutine.resume` を呼ぶとコルーチンが中断箇所から再開します。

Luaのコルーチンを気取った専門用語で言うとstackful asymmetric coroutineとなります。

stackfulというのは中断操作が関数呼び出しを跨げるということです。例えば、`coroutine.yield` をラップした関数を作ることができます：

```lua
function ask_yes_no()
  local result = coroutine.yield()
  if result == "Yes" then
    return true
  elseif result == "No" then
    return false
  else
    error("Invalid input")
  end
end

local co = coroutine.create(function()
  ...
  if ask_yes_no() then
    ...
  else
    ...
  end
  ...
end)
```

また、 `coroutine.yield` は普通の関数として定義されており、変数に入れたり他の関数に渡したりできます。

逆にstackfulでないコルーチン（stacklessコルーチン）というのは、言語に `yield` キーワードみたいなのが組み込まれていて、関数呼び出しを跨ぐには呼び出し側で `yield*` みたいな構文を使う必要があるやつです。ジェネレーターと呼ばれることも多いです。

asymmetric（非対称）というのは、親子関係があるということです。制御を移す操作が `coroutine.resume` と `coroutine.yield` に分かれており、普通の関数呼び出しのような親子関係がコルーチンにもあります。逆にassymmetricではないコルーチン（symmetric（対称）コルーチン）というのは、制御を移す操作が一種類しかなく、常に遷移先のコルーチンを明示します。

Luaのコルーチンについて論文を読みたい人は

* De Moura AL, Rodriguez N, Ierusalimschy R (2004) Coroutines in Lua. JUCS - Journal of Universal Computer Science 10(7): 910-925. https://doi.org/10.3217/jucs-010-07-0910

を読んでください。

Luaのコルーチンを使うと色々面白いことができます。コルーチンは一種の埋め込みDSLを作るためのツールだと思うことができ、 `coroutine.create` に渡す関数がDSLの記述、`coroutine.yield` をラップする関数やメインルーチン（`coroutine.resume` を呼び出す側）がDSLの実装だと思えます。

# コルーチンと限定継続

Luaのコルーチンはワンショット限定継続と等価らしいです。ワンショット限定継続というのは、継続の呼び出しが1回に制限されたもののことです。

対応関係はざっくりこんな感じです：

| コルーチン | 限定継続 | 解説 |
|-|-|-|
| `coroutine.create` | `reset`, `prompt` | 中断可能な処理の作成 |
| `coroutine.yield` | `shift`, `control`, ... | 処理の中断・継続のキャプチャー |
| `coroutine.resume` | `k` | 継続の起動 |

この記事ではコルーチンを使って（ワンショット）限定継続を実装する方をやってみます。ここでは `control0` / `prompt0` を実装してみます。お馴染みの `shift` / `reset` は `control0` / `prompt0` を使うことで実装できます。

まず、`prompt0` では `coroutine.create` でコルーチンを作ってすぐに呼び出し（`resume`）します。コルーチンが `return` で制御を返したら返ってきた値をそのまま返します。関数が `yield` で制御および「継続を受け取る関数」を返したら `control0` が呼ばれたとみなし、「継続を表す値」を作って「継続を受け取る関数」を呼び出します。

継続が起動されたら、コルーチンを `resume` で再開します。コルーチンが `return` で制御を返したら値をそのまま返し、`yield` で制御を返したら `control0` が呼ばれたとみなして先ほどと同様の処理を行います。

コルーチンを何回も `resume` すると毎回違う場所から処理が再開します。つまり、コルーチンをコピーできない以上、同じ場所からは1回しか復帰できません。このことと継続は1回しか使えないことが対応します。継続を起動する際に「使用済み」フラグを検査して、未使用の場合にのみ、「使用済み」フラグを立てた上で継続を起動します。

# single promptな限定継続

まずは「区切り」が一種類の限定継続を実装してみます。さっき言葉で説明したことをLuaコードで書けばOKです。

```lua
-- 継続を関数っぽく呼び出せるようにするためのメタテーブル
-- 継続はフィールドcoとしてコルーチンを、フィールドdoneとして使用済みか否かを保持する。
local sk_meta = {}

-- コルーチンを起動（再開）する。
-- コルーチンがreturnで制御を返したら値を返し、yieldで制御を返したら継続を構築する。
local function run(co, ...)
  local status, a, b = coroutine.resume(co, ...)
  if status then
    if a == "return" then
      -- bには返すべき値が入っている
      return b
    elseif a == "capture" then
      -- bには呼び出すべき関数が入っている
      local k = setmetatable({co=co, done=false}, sk_meta)
      return b(k)
    else
      error("unexpected result from coroutine: "..tostring(a))
    end
  else
    error(a)
  end
end

-- prompt0ではコルーチンを作成する。
function prompt0(f)
  local co = coroutine.create(function()
    -- returnで制御を返したのかyieldで制御を返したのか判別できるように、最初の返り値は "return" とする。
    return "return", f()
  end)
  return run(co)
end

-- 継続をキャプチャーする。
function control0(f)
  local command, g = coroutine.yield("capture", f)
  if command == "resume" then
    return g()
  else
    error("unexpected command to coroutine: "..tostring(command))
  end
end

-- 継続を起動する。
function pushSubCont(subcont, f)
  if subcont.done then
    error("cannot resume continuation multiple times")
  end
  subcont.done = true
  return run(subcont.co, "resume", f)
end
function sk_meta:__call(a)
  return pushSubCont(self, function() return a end)
end

reset = prompt0
function shift(f)
  return control0(function(k)
    return prompt0(function()
      return f(function(x)
        return prompt0(function()
          return k(x)
        end)
      end)
    end)
  end)
end
```

試してみましょう。

まずは簡単な例から。

```lua
local result = reset(function()
  return 3 * shift(function(k)
    -- k には 3 * _ が束縛される。
    return 1 + k(5)
  end)
end)
print("result1", result) -- 16
```

`shift` 自身も「区切り」として動作する例：

```lua
local result = reset(function()
  return 1 + shift(function(k)
    -- k = 1 + _
    return 2 * shift(function(l)
      -- l = 2 * _
      return k(l(5))
    end)
  end)
end)
print("result2", result) -- 11
```


キャプチャーした継続を `reset` の外で利用する例：

```lua
local k = reset(function()
  local f = shift(function(k) return k end)
  return 3 * f()
end)
-- k(f) = reset(function() return 3 * f() end)
print("result3", k(function() return 7 end)) -- 21
```

他の例はGitHubを見てください：

* [delimited-continuations-in-lua/single-prompt.lua at master · minoki/delimited-continuations-in-lua](https://github.com/minoki/delimited-continuations-in-lua/blob/master/single-prompt.lua)

# multi promptな限定継続

一手間加えると「区切り」が複数種類の限定継続も実装できます。Monadic Frameworkの論文のやつを実装してみます。

```lua
-- プロンプトのタグとしてテーブル（の同一性）を使う
function newPromptTag()
  return {}
end

-- 継続を関数っぽく呼び出せるようにするためのメタテーブル
local sk_meta = {}

-- コルーチンを起動（再開）する
local function runWithTag(tag, co, ...)
  local status, a, b, c = coroutine.resume(co, ...)
  if status then
    if a == "return" then
      -- 値が返ってきたらそれでよし
      return b
    elseif a == "capture" then
      -- 継続のキャプチャー要求が飛んできた
      -- b: tag
      -- c: callback
      if b == tag then
        local k = setmetatable({co=co, done=false}, sk_meta) -- キャプチャーした継続
        return c(k)
      else
        -- より上位のハンドラーに処理してもらう
        return runWithTag(tag, co, coroutine.yield("capture", b, c))
      end
    else
      error("unexpected result from the function: "..tostring(a))
    end
  else
    error(a)
  end
end

-- 区切りの中で処理を実行する
function pushPrompt(tag, f)
  local co = coroutine.create(function()
    return "return", f()
  end)
  return runWithTag(tag, co)
end

-- 継続のキャプチャーと大域脱出
function withSubCont(tag, f)
  local command, a = coroutine.yield("capture", tag, f)
  if command == "resume" then
    return a()
  else
    error("unexpected command to coroutine: "..tostring(command))
  end
end

-- 継続の起動
function pushSubCont(subcont, f)
  if subcont.done then
    error("cannot resume captured continuation multiple times")
  end
  subcont.done = true
  return runWithTag(nil, subcont.co, "resume", f)
end
function sk_meta:__call(a)
  return pushSubCont(self, function() return a end)
end

resetAt = pushPrompt
function shiftAt(tag, f)
  return withSubCont(tag, function(k)
    return pushPrompt(tag, function()
      return f(function(x)
        return pushPrompt(tag, function()
          return k(x)
        end)
      end)
    end)
  end)
end
```

指定されたタグに対応するプロンプトがない場合は、「メインスレッドで `coroutine.yield` しようとした」という旨のエラーが出ます。

タグを固定してやるとsingle promptな限定継続もエミュレートできます：

```lua
local tag = newPromptTag()
local function reset(f)
  return resetAt(tag, f)
end
local function shift(f)
  return shiftAt(tag, f)
end
```

サンプルコードとかはGitHubを見てください：

* [delimited-continuations-in-lua/multi-prompt.lua at master · minoki/delimited-continuations-in-lua](https://github.com/minoki/delimited-continuations-in-lua/blob/master/multi-prompt.lua)

# おまけ：LuaとCスタック

LuaからLuaで実装された関数を呼び出す際はCスタックは消費されません。

しかし、 `coroutine.resume` は（PUC Luaでは）Cで実装されており、呼び出しの際にCスタックを消費します。したがって、以下のプログラムはCスタックオーバーフローを起こします：

```lua
local function recur(n)
  if n == 0 then
    return "OK!!!"
  else
    return reset(function()
      return recur(n - 1)
    end)
  end
end
local result = recur(500)
print("Does not consume C stack?", result)
```

もっと身近な例としては、 `pcall` を深くネストさせるとCスタックオーバーフローします：

```lua
local function recur(n)
  if n == 0 then
    return "OK!!!"
  else
    local success, result = pcall(function()
      return recur(n - 1)
    end)
    if success then
      return result
    else
      error(result)
    end
  end
end
local result = recur(500)
print("Does not consume C stack?", result)
```

普通にLuaコードを書く分には `pcall` を100段ネストするようなことはあまりないかと思われますが、大規模なプログラムをLuaにコンパイルするとこの問題を踏むことになります。LunarMLの場合はHaMLetをLuaにコンパイルするときに遭遇しました。

こういうCスタックオーバーフローは、末尾呼び出し最適化のためのトランポリンみたいなことをコルーチンを使ってやってやると回避できます：

```lua
local _depth = 0
function pcallX(f)
  --[[
    単に
    local c = coroutine.create(function()
        return "return", f()
    end)
    return coroutine.yield("handle", c)
    でも良いが、ネストが浅い場合はpcallを使った方が速い
  ]]
  local success, result
  if _depth > 150 then
    local c = coroutine.create(function()
        return "return", f()
    end)
    local olddepth = _depth
    _depth = 0
    success, result = coroutine.yield("handle", c)
    _depth = olddepth
  else
    local olddepth = _depth
    _depth = olddepth + 1
    success, result = pcall(f)
    _depth = olddepth
  end
  return success, result
end

-- インタープリターっぽい
function _run(f)
  local c = coroutine.create(function()
      return "return", f()
  end)
  local stack = {c}
  local values = {}
  while #stack > 0 do
    local status, a, b = coroutine.resume(stack[#stack], table.unpack(values))
    if status then
      if a == "return" then
        table.remove(stack)
        values = {true, b}
      elseif a == "handle" then
        table.insert(stack, b)
        values = {}
      else
        error("unexpected result from the function: " .. tostring(a))
      end
    else
      table.remove(stack)
      if #stack > 0 then
        values = {false, a}
      else
        error(a)
      end
    end
  end
  return table.unpack(values)
end

_run(function()
  local function recur(n)
    if n == 0 then
      return "OK!!!"
    else
      local success, result = pcallX(function()
        return recur(n - 1)
      end)
      if success then
        return result
      else
        error(result)
      end
    end
  end
  local result = recur(500)
  print("Does not consume C stack?", result)
end)
```

従来はトップレベルに書いていた内容を `_run` という関数呼び出しで囲う必要があります。これは末尾呼び出し最適化のためのトランポリンと同様です。

この手法は限定継続にも適用できます。詳しくはGitHubのコードを読んでください：

* [delimited-continuations-in-lua/multi-prompt-stackless.lua at master · minoki/delimited-continuations-in-lua](https://github.com/minoki/delimited-continuations-in-lua/blob/master/multi-prompt-stackless.lua)

なお、LuaJITでは `coroutine.resume` や `pcall` が組み込み扱いされているのか、（少なくとも500段程度では）Cスタックオーバーフローは起きないようです。
