---
title: "Haskellでのデバッグ手法"
---

この章は [Haskell でのデバッグ手法あれこれ](https://blog.miz-ar.info/2018/01/debugging-haskell-program/)（2018年1月）を改編したものです。

---

プログラムにバグはつきものです。強力な型システムを備えている Haskell でもそれは同じです。この記事では、 Haskell プログラムのデバッグ手法をいくつか挙げてみます。

この記事は元々 GHC 8.2.2 の頃に書かれましたが、一部の記述は GHC 9.0.1 向けに改めてあります。

# 心構え：処理を分割せよ

Haskell は純粋な言語です。IOが絡まない関数であれば、同じ引数に対しては同じ結果が返ってくることが期待されます。

よって、処理を細かく（純粋な）関数に分割し、それぞれ GHCi で動作を確かめていけば、どこにバグが潜んでいるのか絞り込むことができます。

この際、関数の実行結果の型は Show クラスのインスタンスにしておくと便利です。

```haskell
-- 良くないコード：
main = do
  x <- ...
  ...でかい処理...
  print result
```

```haskell
-- 良いコード：
foo :: X -> Y
foo x = ...ひとまとまり処理...

bar :: Y -> Z
bar y = ...ひとまとまりの処理...

main = do
  x <- ...
  let result = bar (foo x)
  print result
-- 機能を分割したことにより、 foo や bar を GHCi で個別にテストできる！
```

# Haskell 版 printf デバッグ：Debug.Trace

C言語を始めとする手続き型プログラミング言語では、デバッグしたい箇所に printf などの命令を挟むことにより、「そのコードパスが実行されること」「その時点での各種変数の値」を確認することがあります。このような手法は俗に「printf デバッグ」と呼ばれます。

一方 Haskell では、純粋な関数においては基本的に、ログ出力のような副作用のある処理を実行できません。

しかしそれでは不便なので、 Debug.Trace というモジュールに「純粋な関数でログ出力できる関数」が（抜け道的に）用意されています：

```haskell
-- Debug.Trace
trace :: String -> a -> a
traceShow :: Show a => a -> b -> b
```

他にもいくつかログ出力の関数が用意されています。ドキュメントを読むなり、 “Haskell Debug.Trace” でググるなりしてください。

* [Debug.Trace のドキュメント (Hackage)](https://hackage.haskell.org/package/base/docs/Debug-Trace.html)

（Debug.Trace はもちろん内部的には `unsafePerformIO` を使っていますが、 `unsafePerformIO` を直で使うよりも便利で安全だと思います。）

もちろん、 Haskell はデフォルトで遅延評価の言語ですし、 GHC は処理が純粋だと仮定して最適化するので、ログ出力の順番や回数は期待したものとは異なるかもしれません。

例えば、下記のコード

```haskell
import Debug.Trace
main = do
  let x = trace "foo" 42
  let y = trace "bar" 36
  print (x * y)
```

を筆者の環境の GHC 8.2.2 で試したところ、 `stack runghc` で実行した場合は

```
foo
bar
1512
```

が、 `stack ghc -- -O3` でコンパイルした場合は

```
bar
foo
1512
```

が出力されました（i.e. 最適化によって出力の順番が違う）。

## 多相関数の printf デバッグ：recover-rttiパッケージ

`trace` で具体的な型の値を表示するのは良いのですが、多相関数をデバッグするときは型パラメーターに `Show` 制約を加える必要があって大変です。

例えば、

```haskell
foo :: Num a => a -> a
foo n = n^2 + n_plus_1
  where n_plus_1 = n + 1

bar :: Num a => a -> a
bar n = foo (n^2 - 3 * n)

main = print (bar 2)
```

というプログラムがあったとして、 `foo` の呼び出し時に `n` の値を表示したい、としましょう。このとき、

```haskell
import Debug.Trace

foo :: Num a => a -> a
foo n = trace ("n=" ++ show n) $ n^2 + n_plus_1
  where n_plus_1 = n + 1
```

と書くのではうまくいきません（コンパイルが通りません）。型変数 `a` には `Show` 制約がついていないからです。`Show` 制約を書き足すとなると `foo` だけでなく呼び出し元の `bar` の型も変える必要があり、大変です。

そこで使えるのが[recover-rttiパッケージ](https://hackage.haskell.org/package/recover-rtti)です。このパッケージは

```haskell
anythingToString :: a -> String
```

という関数を提供しており、**`Show` 制約なしで値を文字列化することができます。** `anythingToString` 関数を `show` の代わりに使えば、

```haskell
import Debug.Trace
import Debug.RecoverRTTI

foo :: Num a => a -> a
foo n = trace ("n=" ++ anythingToString n) $ n^2 + n_plus_1
  where n_plus_1 = n + 1

bar :: Num a => a -> a
bar n = foo (n^2 - 3 * n)

main = print (bar 2)
```

と、`foo` や `bar` の型を変えることなくtraceによるデバッグを行えます。

もちろん、recover-rttiパッケージは万能ではなく、欠点もあります。詳しくはドキュメントを読んでください。

* [recover-rtti: Recover run-time type information from the GHC heap](https://hackage.haskell.org/package/recover-rtti) (Hackage)

# Control.Exception.assert

プログラムの特定の箇所で、期待する条件が満たされているかチェックしたい場合、普通は assert という機能を使うと思います。

普通のプログラミング言語では assert を文として記述するかと思いますが、純粋な（モナドに包まれていない） Haskell コード片では、式の評価時にチェックされるような assert を書くことになるでしょう。

もちろん、自前で次のような assert を定義することはできます：

```haskell
assert :: Bool -> a -> a
assert False _ = error "Assertion failed"
assert True  x = x
```

しかしこの程度の関数を自分で書く必要はなく、 GHC 標準の assert が Control.Exception モジュールで提供されています。

Control.Exception モジュールで提供される assert の特徴は、

* コンパイルオプション `-fignore-assert` または最適化フラグ `-O` が有効な場合に assert 呼び出しが消滅する（最適化モードで assert が消えて欲しくない場合は `-fno-ignore-assert` を指定すれば良い）
* 呼び出し箇所のファイル名と行番号を取得できる（後述する HasCallStack 制約に対応しているので。もちろん、自前の assert でも HasCallStack 制約をつければ可能）

点でしょうか。

プログラミング言語によっては、 assert 対象の式を文字列化してエラーメッセージに加えてくれるものがありますが、 Haskell の assert 関数は、関数である以上、そういう特殊なことはできません。（Template Haskell を使えば可能かもしれませんが、そういうライブラリーは軽くググった感じでは見当たりませんでした）

例：

```haskell
import Control.Exception (assert)

foo :: Int -> Int
foo n = assert (n >= 0) $ n^2

bar :: Int -> Int
bar n = foo (n^2 - 3 * n)

main = print (bar 2)
```

* [6.19.4. Assertions — Glasgow Haskell Compiler 9.0.1 User's Guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/assert.html)
* [Control.Exception](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Exception.html#g:14)
* do式やletの途中でassertしたい場合は [Haskellのassertを文っぽく使う](https://qiita.com/mod_poppo/items/b3b415ea72eee210d222) が役に立つかもしれません。

# 対話的デバッグ： GHCi Debugger

GHCi にはデバッガーとしての機能が備わっています。

例えば、 `:break 〈関数名〉` でその関数にブレークポイントを設置できます。 `:step` でステップ実行ができます。詳しくはドキュメントを見てください。

欠点として、 GHCi のバイトコードインタープリターで実行できないコードに対しては適用できません。例えば、 unboxed tuples や unboxed sums を使っているとバイトコードコンパイルに失敗するようです。そのほか、高度な型レベルの機能に対しては対応が十分でないようです（[例](https://gitlab.haskell.org/ghc/ghc/-/issues/13201)）。

参照：

* [3. Using GHCi — Glasgow Haskell Compiler 9.0.1 User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-debugger)
* [GHCiデバッガ – www.kotha.net](http://www.kotha.net/ghcguide_ja/latest/ghci-debugger.html) 日本語訳
* [GHCi debugger を使ってみた – khibino blog](http://khibino.hatenadiary.jp/entry/20111212/1323680441)

# バックトレースの取得：プロファイリング情報

コードの深いところでエラーが出ても、そのコード片がどこから呼ばれたのかわからないと困ります。いわゆるバックトレースが欲しくなります。

GHC では、プロファイリング情報を利用したバックトレースを取得できます。

プロファイリングを有効にして、バックトレースを取得するにはコンパイル時に `-prof -fprof-auto` オプションをつけます。

例：

```haskell
-- Test.hs
foo :: Int -> Int
foo n | n < 0 = error "negative!"
      | otherwise = n^2

bar :: Int -> Int
bar n = foo (n^2 - 3 * n)

main = print (bar 2)
```

`stack ghc -- -prof -fprof-auto Test.hs` でコンパイルし、 `./Test` で実行します：

```
$ stack ghc -- -prof -fprof-auto Test.hs
$ ./Test
Test: negative!
CallStack (from HasCallStack):
  error, called at Test.hs:2:17 in main:Main
CallStack (from -prof):
  Main.foo (Test.hs:(2,1)-(3,23))
  Main.bar (Test.hs:6:1-25)
  Main.main (Test.hs:8:1-20)
  Main.CAF (<entire-module>)
  ```

出力のうち、 `CallStack (from -prof)` がプロファイリング情報によるバックトレースです。

GHCi でプロファイリング情報を有効にするには、 `-fexternal-interpreter` を使う必要があるようです。例えば、 stack 経由であれば、次のように実行します：

```
$ stack repl Test.hs --ghci-options=-fexternal-interpreter --ghc-options=-prof --ghc-options=-fprof-auto
```

参照：

* [GHC でスタックトレース – あどけない話](http://d.hatena.ne.jp/kazu-yamamoto/20130205/1360051153)
* [【Haskell】Debug.Traceとプロファイリングで幸せなデバッグ生活 – yu-i9.tmp](http://yu-i9.hatenablog.com/entry/2014/09/04/000000)
* [Profiling call stacks -- GHC.Stack](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Stack.html#g:1)
* [Stack traces in GHCi, coming in GHC 8.0.1 · Simon Marlow](http://simonmar.github.io/posts/2016-02-12-Stack-traces-in-GHCi.html)

# バックトレースの取得：HasCallStack 制約

GHC 8 では HasCallStack 制約という機能が追加されました。これは関数の型に制約として書くことができ、その関数内で使う error 呼び出しに関数の呼び出し元の情報を含めることができます。

「プロファイリング情報によるバックトレース」に比べた利点は、

* 特別なコンパイルオプションが要らない
* GHCi でも利用できる

あたりでしょうか。手軽なんです。

例を見てみましょう：

```haskell
import GHC.Stack (HasCallStack)

foo :: HasCallStack => Int -> Int
foo n | n < 0 = error "negative!" -- error 関数に foo の呼び出し元の情報が渡される
      | otherwise = n^2

bar :: Int -> Int
bar n = foo (n^2 - 3 * n) -- bar の情報が foo に渡される

main = print (bar 2) -- main の情報は foo には渡らない
```

実行結果：

```
Test.hs: negative!
CallStack (from HasCallStack):
  error, called at Test.hs:4:17 in main:Main
  foo, called at Test.hs:8:9 in main:Main
```

foo 関数に HasCallStack 制約をつけない場合は、 error が foo 関数（Test.hs の4行目）で発生したことしかわかりませんが、 foo 関数に HasCallStack 制約をつけた場合は、エラーメッセージに foo の呼び出し元の情報（Test.hs の 8 行目；つまり bar 関数）も含まれるようになります。

制限として、 HasCallStack 制約を持たない関数があるとそこでバックトレースが途切れます。例では、関数 bar は HasCallStack 制約を持たないので、「foo の呼び出し元が bar である」ことはわかっても、「bar の呼び出し元が誰なのか」はわかりません。

（Haskell に詳しい人は関数の型クラス制約は詰まるところ暗黙の引数なんだということはご存知でしょう。 HasCallStack 制約も暗黙の引数の一種であって、型レベルのナニカ、あるいはオブジェクトコードのデバッグ情報的なナニカではなく、単に呼び出し元の情報をデータとして渡しているだけのようです。そう考えれば、 HasCallStack 制約が途切れるとバックトレースが途切れるという動作にも納得がいくでしょう。）

なお、トップレベル関数の型注釈を省略した場合、通常の型クラス制約は型推論で補われますが、 HasCallStack 制約は補われません。HasCallStack 制約を書きましょう。

参照：

* [6.19.5. HasCallStack — Glasgow Haskell Compiler 9.0.1 User's Guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/callstack.html)
* [HasCallStack call stacks -- GHC.Stack](https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Stack.html#g:2)

## 小ネタ：プロジェクトごとにPreludeを持っているとHasCallStackデバッグに便利

最近は代替Preludeを使うことも一般的になってきましたね&lbrack;要出典&rbrack;。（参考記事：[Prelude を カスタムPrelude で置き換える](https://haskell.e-bigmoon.com/posts/2018/05-23-extended-prelude.html)）

ここではbaseかrioかというのはどうでもよくて、プロジェクトごとにPreludeの役割をするモジュールを持っておくと便利という話をします。

例えば、エラー発生箇所がPreludeの

```haskell
head :: [a] -> a
```

で、head関数の呼び出し元をHasCallStackで突き止めたいとします（この話はPreludeのエラーを投げる関数ならどれでも当てはまります。筆者が実際に遭遇したのは `(^) :: (Num a, Integral b) => a -> b -> a` でした。部分関数を排除した代替Preludeを使っている方には関係ないかもしれません）。

困ったことにheadはPreludeの関数なので、ソースをいじってHasCallStack制約をつけるということができません。head関数を独自に

```haskell
import Prelude hiding (head)
import GHC.Stack (HasCallStack)
head :: HasCallStack => [a] -> a
head [] = error "empty list"
head (x:_) = x
```

と定義すればHasCallStackデバッグができますが、プロジェクトに含まれるたくさんのモジュール全てに `import Prelude hiding (head)` を書き足すのは大変です。

そんな時、プロジェクト独自のPrelude（ここではMyPreludeというモジュール名とします）を

```haskell
module MyPrelude (module Prelude) where
import Prelude
```

と定義しておけば、head関数をHasCallStackデバッグしたくなったらMyPreludeを

```haskell
module MyPrelude (module Prelude,head) where
import Prelude hiding (head)
import GHC.Stack
head :: HasCallStack => [a] -> a
head [] = error "empty list"
head (x:_) = x
```

といじるだけでhead関数に対するHasCallStackデバッグができるようになります。

このテクニックはHasCallStackの利用以外にも応用できそうです。

# 実行トレース：debug パッケージ

※このパッケージの紹介はGHC 8.2時代のものであり、最新のGHCではこの通りに動作するとは限りません。

debugパッケージを使うと、実行トレースを取得できます。

準備として、デバッグしたい関数の定義を `debug [d| … |]` で囲みます。

デバッグ実行の手順としては、まず普通に GHCi で関数を実行します。その後に（debug パッケージの提供する） debugView 関数を呼ぶと、ブラウザーが立ち上がり、関数呼び出しにおける実際の引数、ローカル変数、返り値が確認できるという寸法です。

例：

```haskell
{-# LANGUAGE TemplateHaskell, ViewPatterns, PartialTypeSignatures #-}
import Debug
debug [d|
  foo :: Int -> Int
  foo n = n^2 + n_plus_1
    where n_plus_1 = n + 1
          n_plus_2 = n + 2
  bar :: Int -> Int
  bar n = foo (n^2 - 3 * n)
  main = print (bar 2)
      |]
```

GHCi で次のように実行：

```
*Main> main      --> まず普通に実行する
3
*Main> debugView --> ブラウザーが立ち上がる
```

すると次のような画面が表示されます：

![](https://storage.googleapis.com/zenn-user-upload/0s0tgrrj1lnp7td4uit0hlh4n5w9)

![](https://storage.googleapis.com/zenn-user-upload/o53pgi6yale3nh45zvv9jis668wh)

注意点として、実際に評価されない変数（この例だと `n_plus_2`）の値は確認できません。そのほか、 値を表示するためには値が Show のインスタンスである必要があります。

また、途中でエラーが発生する場合には使えません。

いちいち Template Haskell を有効にしたり `debug [d| |]` で囲ったりするのが面倒だ、という人のために、プリプロセッサーが用意されているようです。詳しくはマニュアルを見てください。

* <https://github.com/ndmitchell/debug>
* <https://hackage.haskell.org/package/debug>
* [Neil Mitchell’s Haskell Blog: Announcing the ‘debug’ package](http://neilmitchell.blogspot.jp/2017/12/announcing-debug-package.html)

# その他

[Debugging – HaskellWiki](https://wiki.haskell.org/Debugging) もこの記事と同様の趣旨のページで、ここで取り上げていないツールもいくつか紹介されているようです。
