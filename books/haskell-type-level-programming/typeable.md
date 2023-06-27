---
title: "応用：実行時型情報（Typeable）"
---

Haskellでプログラミングをしていると、実行時に型を比較したいことがあるかもしれません。例えば、入力が文字列だったらそのまま返して、そうでなければ `show` を適用する関数 `toString` を実装したい、としましょう。仮にHaskellで型を実行時に比較することができれば、この関数は次のように書けるでしょう：

```haskell
-- 擬似コード
toString :: Show a => a -> String
toString x = if a is String then
               x
             else
               show x
```

Haskellで型に応じた処理を行うには型クラスを使うということはすでに述べました。したがって、`toString` をHaskellで真っ当に実装するなら次のようになるでしょう：

```haskell
{-# LANGUAGE FlexibleInstances #-}

class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance ToString Int where
  toString = show

-- 他にもたくさんのインスタンスが必要（もしくはoverlapped instanceを使う）
```

しかし、型に応じた処理ごとに型クラスを増やしていくのは大変です。なんとか、独自の型クラスを定義することなく、型に応じた処理の分岐を実現できないでしょうか？

Haskellでは型に応じた処理を行うには型クラスを使うのが原則なので、必要なのは「型に実行時比較の能力を与えるような一つの型クラス」となります。

GHCではそれが `Typeable` クラスという形で用意されています。`Data.Typeable` モジュールで提供されています。

* [Data.Typeable](https://hackage.haskell.org/package/base/docs/Data-Typeable.html)

`Data.Typeable` モジュールには `typeOf` と `typeRep` という関数があり、いずれも型を識別できる情報を持ったデータ型 `TypeRep` を返します。実行時型情報と呼んでも構わないかもしれません。

```haskell
module Data.Typeable where

class Typeable (a :: k)

type TypeRep

typeOf :: Typeable a => a -> TypeRep
typeRep :: Typeable a => proxy a -> TypeRep
```

`TypeRep` 型は `Eq`, `Ord`, `Show` などのインスタンスとなっているので、型を比較したり文字列化したりできます。試しに使ってみましょう：

```haskell
import Data.Proxy
import Data.Typeable

printType :: Typeable a => Proxy a -> IO ()
printType proxy = print (typeRep proxy)
```

```haskell
ghci> printType (Proxy :: Proxy Int)
Int
ghci> printType (Proxy :: Proxy String)
[Char]
```

これらを使って冒頭の `toString` 関数は書けるでしょうか？まず、比較の部分は良さそうです。

```haskell
toString :: (Show a, Typeable a) => a -> String
toString x = if typeOf x == typeRep (Proxy :: Proxy String) then
               -- ...
```

しかし、実行時に「`a` が `String` である」とわかっても、型検査器はそのifのthen部で `a` が `String` であることを知らないので、静的な型エラーが発生します。

```haskell
toString :: (Show a, Typeable a) => a -> String
toString x = if typeOf x == typeRep (Proxy :: Proxy String) then -- ここはあくまで実行時の比較
               x -- ここで型エラー
             else
               show x
```

実行時の比較によって型制約 `a ~ String` が増えることを型検査器に教えるにはどうしたらいいでしょうか？そう、GADTですね。

というわけで、GHCは `Data.Typeable` の `TypeRep` 型とは別に、静的型の情報も持った `TypeRep a` 型を `Type.Reflection` モジュールに用意しています。`Type.Reflection.TypeRep a` はシングルトン型です。

```haskell
module Type.Reflection where

-- Typeableの情報を封じ込めたシングルトン型と見ることもできる
data TypeRep a

typeOf :: Typeable a => a -> TypeRep a
typeRep :: Typeable a => TypeRep a
```

`TypeRep a` と `TypeRep b` から型の等価性を表すGADTである `a :~: b` を得るには、`Data.Type.Equality` モジュールの `testEquality` 関数を使うと良いでしょう。

```haskell
module Data.Type.Equality where

class TestEquality f where
  testEquality :: f a -> f b -> Maybe (a :~: b)

instance TestEquality Type.Reflection.TypeRep
-- testEquality :: Type.Reflection.TypeRep a -> Type.Reflection.TypeRep b -> Maybe (a :~: b)
```

というわけで、最初の `toString` 関数は次のように書けます：

```haskell
{-# LANGUAGE GADTs #-}
import Type.Reflection as R
import Data.Type.Equality

toString :: (Show a, Typeable a) => a -> String
toString x = case testEquality (R.typeOf x) (R.typeRep :: R.TypeRep String) of
               Just Refl -> x
               Nothing -> show x
```

```haskell
ghci> putStrLn $ toString "foo"
foo
ghci> putStrLn $ toString [1,2,3]
[1,2,3]
```

今回のケースではもっとシンプルな書き方もできます。`Data.Typeable` モジュールからは「実行時型情報に基づいて型キャストを試みる」`cast` 関数や「実行時型情報に基づいて型を比較し、 `Maybe (a :~: b)` として返す」`eqT` 関数が提供されているので、それを使うこともできます。

```haskell
module Data.Typeable where

cast :: (Typeable a, Typeable b) => a -> Maybe b
eqT :: (Typeable a, Typeable b) => Maybe (a :~: b)
```

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
import Data.Typeable

toString2 :: (Show a, Typeable a) => a -> String
toString2 x = case cast x :: Maybe String of
                Just s -> s
                Nothing -> show x

toString3 :: forall a. (Show a, Typeable a) => a -> String
toString3 x = case eqT :: Maybe (a :~: String) of
                Just Refl -> x
                Nothing -> show x
```

`Typeable` クラスは `k -> Constraint` というカインドを持ち、任意のカインドの型について適用することができます。これはPolyKinds拡張のおかげで、PolyKinds拡張が導入される前は

```haskell
class Typeable (a :: *)
class Typeable1 (f :: * -> *)
class Typeable2 (f :: * -> * -> *)
-- Typeable7まで
```

という風にカインドごとにクラスが分かれていて大変だったようです。

なお、`Typeable` クラスのインスタンスはGHCによって自動で導出され、ユーザーが独自に定義することはできません。GHC拡張にDeriveDataTypeableというものがあるので、昔は手動でのderivingが必要だったのかもしれません。

`Typeable` クラスを使うと、「何でも格納できるany型」のようなものを作れます。つまり、任意の型の値を格納でき、中身の型と期待する型が合致すれば安全に取り出せるようなデータ型です。

* [Data.Dynamic](https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Dynamic.html)

```haskell
module Data.Dynamic where

data Dynamic where
  Dynamic :: forall a. TypeRep a -> a -> Dynamic

toDyn :: Typeable a => a -> Dynamic
fromDynamic :: Typeable a => Dynamic -> Maybe a
```

この `Dynamic` 型を制限して、いくつかの型のうちのいずれかが入るような匿名のunion型を実装したライブラリーが[open-unionパッケージ](https://hackage.haskell.org/package/open-union)です。格納できる型の集合は型レベルリストを使って指定します。

```haskell
{-# LANGUAGE DataKinds #-}
import Data.OpenUnion

-- Int, String, Charのいずれかを格納できる型
type MyUnion = Union '[Int, String, Char]

someList :: [MyUnion]
someList = [liftUnion "Hello", liftUnion 'x', liftUnion (42 :: Int)]
```
