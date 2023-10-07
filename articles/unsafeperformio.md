---
title: "unsafePerformIOではじめる愉快なHaskellプログラミング"
emoji: "😽"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

Haskellは「入出力のためにIOモナドを使う必要があるので難しい」というイメージを持たれがちです&lbrack;要出典&rbrack;。この記事ではそのイメージに異を唱えることを目指します。すなわち、モナドを使わずに入出力を含むプログラミングをやってみます。

と言っても、Haskell標準の入出力関数には `IO` 型がついていることに変わりはありません。ですが、 `unsafePerformIO` という魔法の関数を使うと、`IO` 型を外して値を取り出すことができるのです。この記事ではこれを最大限活用します。

```haskell
module System.IO.Unsafe where

-- 魔法の関数
unsafePerformIO :: IO a -> a
```

`IO` を気にしなくて良い楽園へ、いざ！

## 文字列の出力

例えば、文字列を出力して改行する `putStrLn` 関数の「IOがつかない」バージョンは次のように書けます：

```haskell
{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> ()
putStrLn' x = unsafePerformIO (putStrLn x)
```

`OPAQUE` プラグマはおまじないだと思ってください。これはGHC 9.4で追加された機能なので、この記事のコードを試す際はGHC 9.4以降で実行してください。

`putStrLn'` を使うと、Hello worldは次のように書けます：

```haskell
-- Main1.hs
import System.IO.Unsafe (unsafePerformIO)

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> ()
putStrLn' x = unsafePerformIO (putStrLn x)

main' :: ()
main' = putStrLn' "Hello world!"

main = main' `seq` pure ()
```

`main'` が我々の書きたいプログラムで、`main` はそれを実行するためのおまじないです。

実行結果はこうなります：

```
$ ghc Main1.hs
$ ./Main1
Hello world!
```

いい感じですね。Haskell完全に理解した。

## 最適化と戦う

今度はHello worldを2回表示してみましょう。

```haskell
-- Main2.hs
import System.IO.Unsafe (unsafePerformIO)

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> ()
putStrLn' x = unsafePerformIO (putStrLn x)

main' :: ()
main' = case putStrLn' "Hello world!" of
          () -> putStrLn' "Hello world!"

main = main' `seq` pure ()
```

これも実行してみましょう：

```
$ ghc Main2.hs
$ ./Main2
Hello world!
Hello world!
```

いい感じですね。……本当に？

念の為、最適化を有効にしてコンパイル・実行してみましょう：

```
$ ghc -O2 Main2.hs
$ ./Main2
Hello world!
```

なんてこった！1回しか表示されない！

これはコンパイラーが、**共通部分式の削除** (common subexpression elimination) という最適化を行なったことによります。つまり、「同じ関数を同じ引数で呼び出したら結果は同じだよね☆」ということで、`main'` 関数の中身が次のように最適化されたのです：

```haskell
main' :: ()
main' = let a = putStrLn' "Hello world!"
        in case a of
             () -> a
```

共通部分式の削除は、どんなプログラミング言語でもできるわけではありません。例えば、C言語のコンパイラーは中身がわからない関数の呼び出しを削減することはしないでしょう。Haskellコンパイラーがこれを行うのは、Haskellの全ての関数が**純粋**だと思っているからです。

対策として、関数にダミーの引数を持たせることにします。型は `Dummy` として、中身は `Int` とします。

```haskell
-- Main2b.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Dummy -> ()
putStrLn' x _ = unsafePerformIO (putStrLn x)

main' :: ()
main' = case putStrLn' "Hello world!" (Dummy 0) of
          () -> putStrLn' "Hello world!" (Dummy 1)

main = main' `seq` pure ()
```

実行結果は次のようになります：

```
$ ghc -O2 Main2b.hs
$ ./Main2b
Hello world!
Hello world!
```

今度こそ、いい感じですね。

もちろん、関数をすべてインライン化してしまえばダミーの引数が実際には利用されていないことがコンパイラーにバレてしまいますが、`OPAQUE` プラグマがあるのでコンパイラーはそのことがわかりません。

## 逐次実行

今度はもう少し対話的なプログラムを書いてみましょう。名前と年齢を尋ねて、それを出力する感じで行きましょう。

文字列を標準入力から読み取るには `getLine` 関数が、文字列を整数に変換するには `read` 関数が使えます。

```haskell
-- Main3.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Dummy -> ()
putStrLn' x _ = unsafePerformIO (putStrLn x)

{-# OPAQUE getLine' #-}
getLine' :: Dummy -> String
getLine' dummy = unsafePerformIO (dummy `seq` getLine)

getInt :: Dummy -> Int
getInt dummy = read (getLine' dummy)

main' :: ()
main' = case putStrLn' "What is your name?" (Dummy 0) of
          () -> case getLine' (Dummy 1) of
                  name -> case putStrLn' "How old are you?" (Dummy 2) of
                            () -> case getInt (Dummy 3) of
                                    age -> putStrLn' (name ++ ", " ++ show age) (Dummy 4)

main = main' `seq` pure ()
```

実行例：

```
$ ghc -O2 Main3.hs
$ ./Main3
What is your name?
How old are you?
mod_poppo↩︎
65535↩︎
mod_poppo, 65535
```

おやおや、名前を聞いてから年齢を尋ねたいのに、先に「How old are you?」まで表示されてしまいました。どうやら、逐次実行ができていないようです。

普通のプログラミング言語では関数は記述した順番に評価される必要があるかと思いますが、Haskellは**純粋**な言語なので、必ずしもソースコードに書かれた順番に評価されるとは限らないのですね。

ともかく、逐次実行ができないのではプログラムとして成り立ちません。`seq` とか `BangPatterns` を使った技もありますが、ここは正攻法（？）で行きたいです。

ここでは、ダミーの変数で依存関係を表現することにします。つまり、関数からは本来返したい値に加えて、ダミーの値を返し、それを次に実行する関数に渡すのです。

擬似コードで説明すると、

```haskell
let (dummy', x) = f dummy
    (dummy'', y) = g dummy'
    (dummy''', z) = h dummy''
```

という風にダミーの変数を受け渡しすれば、「`g` は `f` の返り値に依存するので `g` よりも前に `f` を評価しなければならない」「`h` は `g` の返り値に依存するので `h` よりも前に `g` を評価しなければならない」というのがコンパイラーに伝わります。

（Haskellは遅延評価で、`let` の束縛も遅延して起こるので、上記のコードそのままでは「`g` よりも前に `f` が評価される」ことは保証されません。なので「擬似コード」です。）

これまではダミーの値は1ずつ増やしてきましたが、`OPAQUE` でマークした関数の返り値はどっちみちコンパイラー的には未知の値なので、関数からはダミーの値はそのまま返しても大丈夫です。

この改良を施したコードは次のようになります：

```haskell
-- Main3b.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Dummy -> Dummy
putStrLn' x dummy = dummy `seq` unsafePerformIO (putStrLn x >> pure dummy)

{-# OPAQUE getLine' #-}
getLine' :: Dummy -> (Dummy, String)
getLine' dummy = dummy `seq` unsafePerformIO (getLine >>= \s -> pure (dummy, s))

getInt :: Dummy -> (Dummy, Int)
getInt dummy = let (dummy', s) = getLine' dummy
               in (dummy', read s)

main' :: Dummy
main' = let dummy0 = Dummy 0
            dummy1 = putStrLn' "What is your name?" dummy0
            (dummy2, name) = getLine' dummy1
            dummy3 = putStrLn' "How old are you?" dummy2
            (dummy4, age) = getInt dummy3
        in putStrLn' (name ++ ", " ++ show age) dummy4

main = main' `seq` pure ()
```

（`putStrLn'` と `getLine'` の内部で `seq` を使っているじゃん、というのは目を瞑ってください。そこはおまじないなので……。）

実行例：

```
$ ghc -O2 Main3b.hs
$ ./Main3b
What is your name?
mod_poppo↩︎
How old are you?
65535↩︎
mod_poppo, 65535
```

良いですね。

## 型を整える

`Main3b.hs` のコードを、意味を変えない範囲で型を整えてみましょう。まず、 `putStrLn'` の返り値の型を `(Dummy, ())` に変えると `getLine'` や `getInt` との統一感が出ます。また、`main'` も `dummy0` を引数に取るようにします。整えた後のコードは次のようになります：

```haskell
-- Main3c.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Dummy -> (Dummy, ())
putStrLn' x dummy = dummy `seq` unsafePerformIO (putStrLn x >> pure (dummy, ()))

{-# OPAQUE getLine' #-}
getLine' :: Dummy -> (Dummy, String)
getLine' dummy = dummy `seq` unsafePerformIO (getLine >>= \s -> pure (dummy, s))

getInt :: Dummy -> (Dummy, Int)
getInt dummy = let (dummy', s) = getLine' dummy
               in (dummy', read s)

main' :: Dummy -> (Dummy, ())
main' dummy0 = let (dummy1, ()) = putStrLn' "What is your name?" dummy0
                   (dummy2, name) = getLine' dummy1
                   (dummy3, ()) = putStrLn' "How old are you?" dummy2
                   (dummy4, age) = getInt dummy3
               in putStrLn' (name ++ ", " ++ show age) dummy4

main = main' (Dummy 0) `seq` pure ()
```

`Dummy -> (Dummy, a)` という形の関数が沢山出てきましたね。これを型エイリアス `Action a` として定義しましょう。

```haskell
-- Main3d.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int
type Action a = Dummy -> (Dummy, a)

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Action ()
putStrLn' x dummy = dummy `seq` unsafePerformIO (putStrLn x >> pure (dummy, ()))

{-# OPAQUE getLine' #-}
getLine' :: Action String
getLine' dummy = dummy `seq` unsafePerformIO (getLine >>= \s -> pure (dummy, s))

getInt :: Action Int
getInt dummy = let (dummy', s) = getLine' dummy
               in (dummy', read s)

main' :: Action ()
main' dummy0 = let (dummy1, ()) = putStrLn' "What is your name?" dummy0
                   (dummy2, name) = getLine' dummy1
                   (dummy3, ()) = putStrLn' "How old are you?" dummy2
                   (dummy4, age) = getInt dummy3
               in putStrLn' (name ++ ", " ++ show age) dummy4

main = main' (Dummy 0) `seq` pure ()
```

型はすっきりしましたが、ダミー変数をいちいち受け渡しするのは面倒ですね。ラップした関数を作りましょう。

まず、逐次実行する関数は次のように書けます：

```haskell
andThen :: Action a -> (a -> Action b) -> Action b
andThen f g = \dummy -> case f dummy of
                          (dummy', x) -> g x dummy'
```

次に、値 `x :: a` を `Action a` に埋め込む関数は次のように書けます：

```haskell
value :: a -> Action a
value x = \dummy -> (dummy, x)
```

これらを使うと、先ほどのプログラムは次のように書けます：

```haskell
-- Main3e.hs
import System.IO.Unsafe (unsafePerformIO)

newtype Dummy = Dummy Int
type Action a = Dummy -> (Dummy, a)

andThen :: Action a -> (a -> Action b) -> Action b
andThen f g = \dummy -> case f dummy of
                          (dummy', x) -> g x dummy'

value :: a -> Action a
value x = \dummy -> (dummy, x)

{-# OPAQUE putStrLn' #-}
putStrLn' :: String -> Action ()
putStrLn' x dummy = dummy `seq` unsafePerformIO (putStrLn x >> pure (dummy, ()))

{-# OPAQUE getLine' #-}
getLine' :: Action String
getLine' dummy = dummy `seq` unsafePerformIO (getLine >>= \s -> pure (dummy, s))

main = main' (Dummy 0) `seq` pure ()

-- ここまで準備
-- ここから本当に書きたいコード

getInt :: Action Int
getInt = getLine' `andThen` \s -> value (read s)

main' :: Action ()
main' = putStrLn' "What is your name?" `andThen` \() ->
        getLine' `andThen` \name ->
        putStrLn' "How old are you?" `andThen` \() ->
        getInt `andThen` \age ->
        putStrLn' (name ++ ", " ++ show age)
```

後半の「本当に書きたいコード」の部分からはダミー変数がなくなってすっきりしました。いい感じですね。

## 結論

純粋関数型言語で入出力 (IO) を期待した通りに行うには、ダミーの値を受け渡し `Dummy -> (Dummy, a)` するのが良いことがわかりました。そして、`type Action a = Dummy -> (Dummy, a)` と定義して `andThen :: Action a -> (a -> Action b) -> Action b` と `value :: a -> Action a` という2つの関数を使うと、ダミーの変数の受け渡しを表面上見えなくすることができました。

実は、HaskellコンパイラーであるGHCには、こういう「ダミーの値」を表す型、そしてそれを受け渡しする関数の型 `Action` に相当するものがすでに用意されています。

[GHC.IO](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-IO.html)モジュールを見てみましょう：

```haskell
module GHC.IO where

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
```

我々の `Dummy` 型に相当するものはGHCでは `State# RealWorld` と呼ばれています。そして、`Action` に相当するものはGHCでは `IO` と呼ばれています（`->` の右側の `(# ..., ... #)` はタプルの一種です）。

我々の `andThen` はGHCでは `(>>=)` と呼ばれ、我々の `value` はGHCでは `pure` または `return` と呼ばれています。

そう、我々は「`IO` に縛られずに自由に入出力を行いたい」と夢見て旅に出ましたが、純粋な言語であるHaskellで安心して入出力を行うには、結局 `IO` が最適だったのです。

しかし、旅をしたことでわかったこともあります。GHCで「現実世界」 `RealWorld` と大袈裟な名前で呼ばれているものは、コンパイラーの最適化を制御するための単なるダミーの引数に過ぎなかったのです。謎が一つ解けましたか？

なお、`State# RealWorld` を使った `IO` 型の定義はGHCのものであり、他の（過去に存在した）Haskellコンパイラーでは別の内部表現が使われている可能性があります。

## おまけ：Haskellが「純粋」とはどういうことか

Haskellは「純粋」と言われますが、それはどういうことでしょうか。数学的な関数がどうのこうの、みたいな説明もあるかと思いますが、ここではもっと具体的な挙動として説明してみましょう。

すなわち、Haskellは「コンパイラーに許される最適化の範囲が広い言語」だということです。この記事で見たように、コンパイラーは任意の関数呼び出しを含む式を共通部分式削除しても良いし、評価の順番を入れ替えても良いのです。

こういうことが許される言語は多くはありません。だからこそHaskellはわざわざ純粋と呼ばれるのでしょう。

## おまけ2：本当は怖い `unsafePerformIO`

`unsafePerformIO` は名前にunsafeがつく関数の中でもかなり危険な方で、これを使うと型システムを迂回することができます。

具体的には、ある型を別の型に無理やり変換する関数 `cast :: a -> b` が書けてしまうのです：

```haskell
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Maybe

cast :: a -> b
cast x = unsafePerformIO $ do
  let ref = unsafePerformIO (newIORef Nothing)
  writeIORef ref (Just x)
  fromJust <$> readIORef ref

main :: IO ()
main = print (cast "Hello!" :: Int)
```

このプログラムを実行すると、メチャクチャな値が表示されるかと思います。プログラムをクラッシュさせることも容易でしょう。`unsafePerformIO` は危険なのです。
