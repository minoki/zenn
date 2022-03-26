---
title: "シングルトン型と依存型の模倣"
---

現状のHaskellは項と型が厳格に分かれた言語であり、型に対してパターンマッチを行なって実行時の処理を切り替えるには通常の `case` 構文ではなく型クラスを使う必要があります。しかし、処理ごとに型クラスを定義していては大変です。なんとか、型のパターンマッチを `case` 構文によって行えないでしょうか。

`case` によって型を分岐させる（型の詳細化を起こす）にはGADTを使えば良いのでした。というわけで、型クラスと何らかのGADTを組み合わせればよさそうです。

ここでは、項と型を自在に行き来するための、シングルトンと呼ばれるやり方（デザインパターン）を紹介します。シングルトンパターンを使うと、Haskellで**依存型**（dependent type; 項に依存した型）を模倣することができます。

# シングルトン型

これまで `PeanoNat` 型を題材にしてきましたが、ここで次のようなGADTを考えてみましょう：

```haskell
data SPeanoNat (n :: PeanoNat) where
  SZero :: SPeanoNat 'Zero
  SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)
```

各 `n` に対して、 `SPeanoNat n` 型の（ボトムを含まない）値は一つずつしか存在しません。値を一つしか持たない型のことを**シングルトン型** (singleton type) と呼びます（ちなみに数学では要素をひとつしか持たない集合のことを singleton と呼びます。プログラミングでも要素をひとつしか持たないコレクションのことを singleton と呼びます）。型の名前の先頭につけた `S` は singleton の S です。

値がひとつしかないという意味では `Proxy a` もユニット型 `()` もシングルトン型なわけですが、ここで定義した `SPeanoNat n` は何が特別なのでしょうか。それは、**型レベルの情報と値レベルの情報の両方を持っており、型レベルの計算と実行時の計算を結びつける役割を果たす**という点です。

例えば、自然数の和を計算する関数は次のように書けました：

```haskell
-- 型レベル関数
type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

-- 参考：同等の値レベル関数
add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m
add (Succ n') m = Succ (add n' m)
```

ここで、型レベル関数 `Add` と値レベルの関数 `add` はそれぞれ独立しており、型レベルの `Add` を実行時に呼び出したり、値レベルの `add` を型レベルに持ち上げたりすることはできません。

一方、シングルトン型について同様の関数を書いてみましょう：

```haskell
sAdd :: SPeanoNat n -> SPeanoNat m -> SPeanoNat (Add n m)
sAdd SZero m = m
sAdd (SSucc n') m = SSucc (sAdd n' m)
```

この `sAdd` は値レベルで呼び出すことができる関数で、なおかつ型に型レベルの計算 `Add n m` も現れています。

他のシングルトン型も見てみます。`Bool` と `Ordering` に対応するシングルトン型は次のようになります：

```haskell
data SBool (x :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

data SOrdering (x :: Ordering) where
  SLT :: SOrdering 'LT
  SEQ :: SOrdering 'EQ
  SGT :: SOrdering 'GT
```

これを使って `compare` 関数を書いてみましょう。値レベル、型レベルの `compare` 関数は次のように書けるのでした：

```haskell
compare :: PeanoNat -> PeanoNat -> Ordering
compare Zero Zero = EQ
compare Zero (Succ _) = LT
compare (Succ _) Zero = GT
compare (Succ n) (Succ m) = compare n m

type Compare :: PeanoNat -> PeanoNat -> Ordering
type family Compare n m
type instance Compare 'Zero 'Zero = 'EQ
type instance Compare 'Zero ('Succ _) = 'LT
type instance Compare ('Succ _) 'Zero = 'GT
type instance Compare ('Succ n) ('Succ m) = Compare n m
```

これのシングルトン版は次のようになります：

```haskell
sCompare :: SPeanoNat n -> SPeanoNat m -> SOrdering (Compare n m)
sCompare SZero SZero = SEQ
sCompare SZero (SSucc _) = SLT
sCompare (SSucc _) SZero = SGT
sCompare (SSucc n) (SSucc m) = sCompare n m
```

# 型クラス

前章の最後に、「型レベル自然数に対するパターンマッチが行えるようなものを取り出せる型クラスが欲しい」という話をしました。シングルトン型を使うと、それが実現できます。型に対して対応するシングルトンを返す型クラスを定義すれば良いのです。`PeanoNat` に対しては、次のように定義できます：

```haskell
class PeanoNatI (n :: PeanoNat) where
  singPeanoNat :: SPeanoNat n

instance PeanoNatI 'Zero where
  singPeanoNat = SZero

instance PeanoNatI n => PeanoNatI ('Succ n) where
  singPeanoNat = SSucc singPeanoNat
```

シングルトン型の型パラメーターに型レベル自然数 `n` が含まれているため、`Proxy` 引数は不要です。

`Bool` や `Ordering` に対しても同様の型クラスが定義できます：

```haskell
class BoolI (x :: Bool) where
  singBool :: SBool x

instance BoolI 'True where
  singBool = STrue

instance BoolI 'False where
  singBool = SFalse

class OrderingI (x :: Ordering) where
  singOrdering :: SOrdering x

instance OrderingI 'LT where
  singOrdering = SLT

instance OrderingI 'EQ where
  singOrdering = SEQ

instance OrderingI 'GT where
  singOrdering = SGT
```

シングルトン型および型クラスを使うと、型レベル自然数を値レベルの整数に変換する関数は次のように書けます：

```haskell
sPeanoNatToInteger :: SPeanoNat n -> Integer
sPeanoNatToInteger SZero = 0
sPeanoNatToInteger (SSucc n') = 1 + sPeanoNatToInteger n'

peanoNatToInteger :: forall n. PeanoNatI n => Proxy n -> Integer
peanoNatToInteger _ = sPeanoNatToInteger (singPeanoNat :: SPeanoNat n)
```

# 値から型への持ち上げ

型レベル自然数 `n` からシングルトン `SPeanoNat n` を得るのは `PeanoNatI` クラスを使えばできるようになりました。逆に、シングルトン `SPeanoNat n` から `PeanoNatI n` 制約を捻出する関数（および存在型）は次のように書けます：

```haskell
data PeanoNatInstance (n :: PeanoNat) where
  PeanoNatInstance :: PeanoNatI n => PeanoNatInstance n

peanoNatInstance :: SPeanoNat n -> PeanoNatInstance n
peanoNatInstance SZero = PeanoNatInstance
peanoNatInstance (SSucc n') = case peanoNatInstance n' of
                                PeanoNatInstance -> PeanoNatInstance
```

最後に、値レベルの自然数（`PeanoNat` 型）をシングルトンに変換する関数は、次のように書けます：

```haskell
data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). SPeanoNat n -> SomePeanoNat

somePeanoNat :: PeanoNat -> SomePeanoNat
somePeanoNat Zero = SomePeanoNat SZero
somePeanoNat (Succ n) = case somePeanoNat n of
                          SomePeanoNat n -> SomePeanoNat (SSucc n)
```

`somePeanoNat` と `peanoNatInstance` 関数を組み合わせれば、「実行時の自然数（`PeanoNat` 型の値）を型レベルに持ち上げて、`PeanoNatI` 制約のついた関数（ここでは `peanoNatToInteger`）を呼び出す」処理を次のように書くことができます：

```haskell
peanoNatToInteger' :: PeanoNat -> Integer
peanoNatToInteger' x = case somePeanoNat x of
                         SomePeanoNat (s :: SPeanoNat n) -> -- ここで n は PeanoNat カインドの型変数
                           case peanoNatInstance s of
                             PeanoNatInstance -> -- ここで PeanoNatI n 制約が導入される
                               peanoNatToInteger (Proxy :: Proxy n)
```

# 引数への依存性

ここまでで、自然数に依存する（引数として受け取る）関数を書く方法をいくつか見てきました。そのような関数は、以下の二つの観点から分類できます：

* 自然数の値に実行時に依存できるかどうか（パターンマッチができるかどうか）
* 自然数に型レベルで依存できるかどうか（他の引数の型や返り値の型に型レベル自然数を含められるか）

それぞれの例を以下に挙げます：

```haskell
-- 自然数の値に実行時に依存できる
-- 型レベルの依存性はない
f1 :: PeanoNat -> ...

-- 型レベル自然数に依存している
-- 実行時の依存（パターンマッチ）はできない
f2 :: forall (n :: PeanoNat). Proxy n -> ...

-- 型レベル自然数に依存している
-- 実行時の依存（パターンマッチ）もできる
f3 :: forall n. PeanoNatI n => Proxy n -> ...

-- 型レベル自然数に依存している
-- 実行時の依存（パターンマッチ）もできる
f4 :: forall n. SPeanoNat n -> ...
```

`f1` は普通の関数です。実行時に自然数の値を利用できますが、型レベルでは依存していません。

`f2` は型レベル自然数に依存する関数ですが、実行時には自然数の値を利用できません。

`f3` と `f4` は型レベル自然数に依存する関数で、なおかつ実行時に自然数の値を利用（パターンマッチ）することもできます。`f3` と `f4` は等価で、 `singPeanoNat` 関数や `peanoNatInstance` 関数を通してお互いを呼び出すことができます。

Haskellで依存型を使うコードを読む際は、「型レベルで依存するか」「実行時に依存できるか」に注意して読むとよいでしょう。

# singletonsライブラリー

`PeanoNat` 型を型レベルに持ち上げたり諸々変換したりするのに、

```haskell
data SPeanoNat (n :: PeanoNat)

class PeanoNatI (n :: PeanoNat) where
  singPeanoNat :: SPeanoNat n

data PeanoNatInstance (n :: PeanoNat) where
  PeanoNatInstance :: PeanoNatI n => PeanoNatInstance n

peanoNatInstance :: SPeanoNat n -> PeanoNatInstance n

data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). SPeanoNat n -> SomePeanoNat

somePeanoNat :: PeanoNat -> SomePeanoNat
```

というデータ型・型クラス・関数を定義しました。Haskellで不自由なく依存型もどきをするには、他のデータ型、例えば `Bool` や `Ordering` などについても同様のデータ型・型クラス・関数を定義する必要があります。

[singletonsパッケージ](https://hackage.haskell.org/package/singletons)では、これらのデータ型・型クラス・関数を型族を使って統一的に扱うインターフェースを提供しています。カインド多相をフルに使っているので少し分かりにくいかもしれませんが、ここまでで紹介したものに対するインターフェース（singletons バージョン3.0.1現在）は次のようになっています：

```haskell
module Data.Singletons where

-- それぞれの型（カインド）に対してそのシングルトン型を対応させる型族
-- 型族としての引数の最低個数は0個
type family Sing :: k -> Type
{- 例：
type instance Sing = SPeanoNat -- k ~ PeanoNat の場合
type instance Sing = SBool -- k ~ Bool の場合
type instance Sing = SOrdering -- k ~ Ordering の場合
-}

-- 型からそのシングルトンを得るための型クラス
-- 先程の PeanoNatI n を一般化したもの
-- I は implicit の i
type SingI :: k -> Constraint
class SingI a where
  sing :: Sing a

-- SingI制約を封じ込めた存在型
-- 先程の PeanoNatInstance n を一般化したもの
type SingInstance :: k -> Type
data SingInstance a where
  SingInstance :: SingI a => SingInstance a

-- シングルトンを型クラス制約に変換する関数
-- 先程の peanoNatInstance :: SPeanoNat n -> PeanoNatInstance n を一般化したもの
-- compiler magic を使っているのでカインドに対して制約がかかっていない
singInstance :: Sing a -> SingInstance a

-- シングルトンを封じ込めた存在型
-- 先程の SomePeanoNat を一般化したもの
type SomeSing :: Type -> Type
data SomeSing k where
  SomeSing :: Sing (a :: k) -> SomeSing k

-- 通常の値とシングルトンの相互変換を行う型クラス
type SingKind :: Type -> Constraint
class SingKind k where
  -- Demote k はカインド k で表されるものを値レベルで表現するデータ型
  -- 通常は Demote k = k
  type Demote k :: Type

  -- シングルトンを通常の値に変換する
  -- 先程の peanoNatToInteger みたいなやつ
  fromSing :: Sing (a :: k) -> Demote k

  -- 通常の値をシングルトンに変換する
  -- 先程の somePeanoNat を一般化したもの
  toSing :: Demote k -> SomeSing k
```

統一されたインターフェースが提供されるとはいえ、自前で定義したデータ型について各種インスタンスの定義、自前で定義した関数について型関数、シングルトン版をいちいち書くのは大変です。[singleton-thパッケージ](https://hackage.haskell.org/package/singletons-th)では、Template Haskellでそれらを自動生成するための関数を用意しています。例えば、ペアノ自然数とその加法を次のように書くと、

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where
import Data.Singletons
import Data.Singletons.TH (singletons)
import Prelude.Singletons

$(singletons [d|
  data PeanoNat = Zero | Succ PeanoNat

  add :: PeanoNat -> PeanoNat -> PeanoNat
  add Zero m = m
  add (Succ n) m = Succ (add n m)
  |])
```

以下のデータ型・インスタンス・型族・関数を定義してくれます：

```haskell
data PeanoNat = Zero | Succ PeanoNat -- 元の定義
data SPeanoNat (n :: PeanoNat)
instance SingI 'Zero
instance SingI n => SingI ('Succ n)
instance SingKind PeanoNat

add :: PeanoNat -> PeanoNat -> PeanoNat -- 元のコード
type Add :: PeanoNat -> PeanoNat -> PeanoNat
sAdd :: SPeanoNat n -> SPeanoNat m -> SPeanoNat (Add n m)
```

`Bool` や `Ordering` など、標準ライブラリーで提供されている型や関数の対応物は、[singletons-baseパッケージ](https://hackage.haskell.org/package/singletons-base)で提供されています。

# 参考文献

singletonsについて解説した論文は以下です：

* Richard A. Eisenberg and Stephanie Weirich, Dependently Typed Programming with Singletons. <https://richarde.dev/papers/2012/singletons/paper.pdf>

が、現在のsingletonsパッケージは（GHCの進化に伴って）論文に記述されたものから変化を遂げています。
