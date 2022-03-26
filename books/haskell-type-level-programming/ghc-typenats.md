---
title: "応用：GHCの型レベル自然数（Natカインド）"
---

# 型レベル自然数の使い所

型レベル自然数の使い所はいろいろあります。

まず、**リスト・配列の長さ**を型レベルで管理できると便利です。「この関数は長さ n のリストを受け取って長さ n+1 のリストを返す」という風な情報が型に現れていると、ドキュメントの役割も果たします。

**行列のサイズ**も型レベルで決まっていれば、異なるサイズの行列の足し算や、サイズが噛み合わない行列積をコンパイル時に検出できます。

**モジュラー計算の法**も型で表すのが適当です。

```haskell
-- 型レベル自然数を表す Nat カインドが使えるとする

-- 長さ n の配列型
newtype SizedVector (n :: Nat) a = SizedVector (Vector a)

-- 型レベル自然数によってパラメーター化された行列型
newtype Matrix (n :: Nat) (m :: Nat) a = Mat (Array (Int, Int) a)

-- モジュラー計算の法 m をパラメーターとして取る型
newtype IntMod (m :: Nat) = IntMod Integer
```

# ペアノ自然数の欠点

この本ではこれまで、例としてペアノ自然数 `PeanoNat` を扱ってきました。ペアノ自然数では、自然数を

```haskell
'Succ ('Succ ... ('Succ 'Zero)...)
＼_______ n 個 _______／
```

という風に `'Succ` の個数で表現するのでした。このことは、大きな自然数を扱おうとすると型が膨張してしまい、コンパイラーの制限に引っかかりやすくなることを意味します。

試しに、以下のプログラムによりペアノ自然数で 256 を扱ってみましょう：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Kind
import Data.Proxy

data PeanoNat = Zero | Succ PeanoNat

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)

type Mul :: PeanoNat -> PeanoNat -> PeanoNat
type family Mul n m
type instance Mul Zero m = Zero
type instance Mul (Succ n) m = Add m (Mul n m)

type One = 'Succ 'Zero
type Two = 'Succ One
type Four = Mul Two Two
type N16 = Mul Four Four -- 4 * 4 = 16
type N256 = Mul N16 N16 -- 16 * 16 = 256

type PeanoNatToInteger :: PeanoNat -> Constraint
class PeanoNatToInteger n where
  natToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  natToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  natToInteger _ = 1 + natToInteger (Proxy :: Proxy n)

main = print (natToInteger (Proxy :: Proxy N256))
```

このコードをGHC 9.2でコンパイルしたところ、Reduction stack overflowというエラーが出ました。一応回避方法はあるようですが、ペアノ自然数を実用的なプログラムで多用するとコンパイルに時間がかかってしまうのは想像に難くないでしょう。

型が膨張しすぎない自然数の表現としては位取り記数法もありますが、本章ではGHC組み込みの型レベル自然数を扱います。

# GHC組み込みの型レベル自然数

GHCではDataKinds拡張を使うことによって、組み込みの**型レベル自然数**を使えるようになります。型レベル自然数としてはこの本ではこれまでペアノ流の自然数 `PeanoNat` を扱ってきましたが、GHC組み込みの型レベル自然数は

* 整数リテラルが使える：`Succ (Succ (Succ Zero))` の代わりに `3` と書けます。
* ペアノ自然数と比べて、大きな数を扱える
* 定義が不透明なので帰納法が使えない（欠点）

という特徴があります。

# 基本的な使い方

GHC組み込みの型レベル自然数に関連するデータ型や関数は、 `GHC.TypeNats` または `GHC.TypeLits` モジュールで提供されています。

基本となるインターフェースは次の3つです：

* `Nat` カインド
* `Nat` カインドの型から実行時に使える自然数の値を取り出すことのできる `KnownNat` クラス
* `KnownNat` 制約の下で型レベル自然数から実行時に使える自然数を取り出す `natVal` 関数

```haskell
data Nat
class KnownNat (n :: Nat)
natVal :: KnownNat n => proxy n -> Natural {- or Integer -}
```

`Nat` は普通のデータ型として定義されていますが、DataKinds拡張でカインドに持ち上げることが前提です。GHC 9.2以降では、 `Nat` 型は `Natural` 型（`Numeric.Natural` モジュール）のエイリアスとなっています。

`KnownNat` クラスの中身は公開されておらず、プログラマーが独自にインスタンスを定義することはできないようになっています。

`natVal` 関数の引数は `proxy n` となっており、`Proxy` を含め、`Nat` カインドの型引数を取る任意の型を渡せます。`GHC.TypeNats` モジュールからエクスポートされている `natVal` 関数は `Natural` を、`GHC.TypeLits` モジュールからエクスポートされている `natVal` 関数は `Integer` を返します。

（歴史的には `GHC.TypeLits` モジュールが先で、 `GHC.TypeNats` モジュールはGHC 8.2 (base-4.10.0.0) で追加された比較的新しいものです。）

例として、型レベルで 16×16 を計算して実行時の値を取り出すコードは次のようになります：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}
import GHC.TypeNats
import Numeric.Natural
import Data.Proxy

main = print (natVal (Proxy :: Proxy (16 * 16)) :: Natural)
```

# 実行時の自然数を型レベルに持ち上げる

実行時に得られた自然数を型レベル自然数として使うには、 `someNatVal` 関数を使います。この関数は、型レベル自然数および `KnownNat` 制約を包んだ存在型 `SomeNat` を返します。

`GHC.TypeNats` モジュールで提供されている `someNatVal` 関数は `Natural` 型を受け取って `SomeNat` 型を返しますが、`GHC.TypeLits` モジュールの `someNatVal` 関数は `Integer` を受け取って `Maybe` にくるんだ `SomeNat` 型を返します。後者は、負の数が与えられたときに `Nothing` を返します。

```haskell
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
{- GADT風に書けば
data SomeNat where
  SomeNat :: KnownNat n => Proxy n -> SomeNat
-}

-- GHC.TypeNats:
someNatVal :: Natural -> SomeNat

-- GHC.TypeLits:
someNatVal :: Integer -> Maybe SomeNat
```

# 四則演算ほか

GHC組み込みの型レベル自然数に対しては、GHC組み込みで四則演算などが型族として提供されています。

```haskell
type (+) :: Nat -> Nat -> Nat -- 足し算
type (*) :: Nat -> Nat -> Nat -- 掛け算
type (^) :: Nat -> Nat -> Nat -- 冪乗
type (-) :: Nat -> Nat -> Nat -- 引き算
type Div :: Nat -> Nat -> Nat -- 割り算
type Mod :: Nat -> Nat -> Nat -- 剰余
type Log2 :: Nat -> Nat       -- 底2の対数
```

型演算子を使うにはTypeOperators拡張が必要なほか、乗算 `*` を使う際にはNoStarIsType拡張を有効にしましょう。

ちなみに、減算 `-`, `Div`, `Mod`, `Log2` は部分関数です。定義されない入力を与えた場合、「型族の適用が簡約されない」という挙動になります。例えば、減算をGHCiの `:kind!` コマンドで計算すると、

```haskell
ghci> :kind! 10 - 5
10 - 5 :: Nat
= 5
ghci> :kind! 10 - 13
10 - 13 :: Nat
= 10 - 13
```

という風に `10 - 5` は簡約されますが、結果が自然数にならない `10 - 13` は簡約が起こらずに `10 - 13` のままとなります。

# 等価性

型レベル自然数が等しいかを実行時に判定することもできます。`sameNat` 関数を使います。

```haskell
sameNat :: (KnownNat a, KnownNat b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
```

`sameNat` 関数は二つの自然数が等しければその「証拠」`Refl` を `Just` に包んで返し、等しくなければ `Nothing` を返します。

例えば、サイズが噛み合った行列に対しての行列積を計算する関数

```haskell
matMul :: (Num a, KnownNat l, KnownNat m, KnownNat n)
       => Matrix l m a -> Matrix m n a -> Matrix l n a
```

が与えられた状態で、サイズの違いにより失敗するかもしれない行列積を計算する関数

```haskell
matMulMaybe :: forall a l m m' n
             . (Num a, KnownNat l, KnownNat m, KnownNat m', KnownNat n)
            => Matrix l m a -> Matrix m' n a -> Maybe (Matrix l n a)
```

は次のように定義できます：

```haskell
matMulMaybe a b = case sameNat (Proxy :: Proxy m) (Proxy :: Proxy m') of
                    Just Refl -> {- Refl にマッチしたのでここでは m と m' は同じ型 -}
                                 Just (matMul a b) -- 型が合致するので呼び出せる
                    Nothing -> Nothing
```

# 大小比較

自然数の大小を比較することもできます。`<=?` 型演算子は `Bool` カインドの型（型レベル `Bool`）を返すほか、`Ordering` を返す `CmpNat` 型族も提供されています。大小関係を制約として表す `<=` も使えます。

```haskell
type (<=) :: Nat -> Nat -> Constraint
type (<=?) :: Nat -> Nat -> Bool
type CmpNat :: Nat -> Nat -> Ordering

-- GHC 9.2以降
cmpNat :: (KnownNat a, KnownNat b) => proxy1 a -> proxy2 b -> Data.Type.Ord.OrderingI a b
```

GHC 9.2では実行時に比較を行う `cmpNat` が追加されたほか、 `<=` 以外の比較演算子が `Data.Type.Ord` モジュールからエクスポートされるようになりました：

```haskell
module Data.Type.Ord where

type Compare :: k -> k -> Ordering

data OrderingI a b where
  LTI :: Compare a b ~ 'LT => OrderingI a b
  EQI :: Compare a a ~ 'EQ => OrderingI a a
  GTI :: Compare a b ~ 'GT => OrderingI a b

type (<) :: k -> k -> Constraint
type (<=) :: k -> k -> Constraint
type (>) :: k -> k -> Constraint
type (>=) :: k -> k -> Constraint

type (<?) :: k -> k -> Bool
type (<=?) :: k -> k -> Bool
type (>?) :: k -> k -> Bool
type (>=?) :: k -> k -> Bool

type Max :: k -> k -> k
type Min :: k -> k -> k

type OrdCond :: Ordering -> k -> k -> k -> k
```

これらを使うと、特定の範囲の型レベル自然数を受け取る関数を書くことができます：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
import GHC.TypeNats
import Data.Proxy

-- 2以上7以下の型レベル自然数を受け取る関数
foo :: (KnownNat a, 2 <= a, a <= 7) => Proxy a -> IO ()
foo proxy = print $ natVal proxy

main = do
  -- foo (Proxy :: Proxy 1) -- コンパイルが通らない
  -- foo (Proxy :: Proxy 10) -- コンパイルが通らない
  foo (Proxy :: Proxy 2) -- コンパイルが通る
```

# GHC組み込みの限界

自前で定義したペアノ自然数は普通の代数的データ型として定義されており、足し算などの演算や、性質についての証明を自前で記述することができました。

一方、GHC組み込みの型レベル自然数は、抽象的なデータ型（カインド）として定義されており、自前でできることに制限があります。具体的には、

* 型の等価性に関する問題
* `KnownNat` インスタンスに関する問題

があります。

例として、次のコードを考えてみましょう：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, NoStarIsType #-}
import Prelude hiding (replicate)
import qualified Data.List as List
import Data.Proxy
import GHC.TypeNats

-- 長さを型パラメーター（幽霊型）で持つリスト型
-- 不変条件：長さが n である
newtype SizedList (n :: Nat) a = SizedList [a] deriving (Eq, Show)

replicate :: forall n a. KnownNat n => a -> SizedList n a
replicate x = SizedList (List.replicate (fromIntegral (natVal (Proxy :: Proxy n))) x)

append :: SizedList n a -> SizedList m a -> SizedList (n + m) a
append (SizedList xs) (SizedList ys) = SizedList (xs ++ ys)

foo :: forall n. KnownNat n => Proxy n -> Bool
foo _ = let xs = replicate 'a' :: SizedList (2 * n) Char
            ys = replicate 'a' :: SizedList n Char
            zs = append ys ys :: SizedList (n + n) Char
        in xs == zs

main = print (foo (Proxy :: Proxy 7))
```

`foo` の中では長さ `2 * n` のリスト `xs` と長さ `n + n` のリスト `zs` を比較しています。数学的には `2 * n = n + n` なのでこれらは同じ型であり、問題なく比較できて良いはずです。しかし実際にやってみると、

> Couldn't match type: `n + n` with: `2 * n`

という風なエラーが出ます。そう、GHCはこの等式が成り立つことを知らないのです。

「数学的には成り立つ等式がGHCにはわからない」というのがGHC組み込みの型レベル自然数の問題の一つです。自前で定義したペアノ自然数の場合は自分で証明を書くという手がありましたが、`Nat` カインドはGHC組み込みなので真っ当な方法ではそれはできません。

もう一つの問題は、`KnownNat` 制約についてです。次のコードを考えましょう：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators, NoStarIsType #-}
import Prelude hiding (replicate)
import qualified Data.List as List
import Data.Proxy
import GHC.TypeNats

-- 長さを型パラメーター（幽霊型）で持つリスト型
-- 不変条件：長さが n である
newtype SizedList (n :: Nat) a = SizedList [a] deriving (Eq, Show)

replicate :: forall n a. KnownNat n => a -> SizedList n a
replicate x = SizedList (List.replicate (fromIntegral (natVal (Proxy :: Proxy n))) x)

foo :: forall n. KnownNat n => Proxy n -> Bool
foo _ = let xs = replicate 'a' :: SizedList (2 * n) Char
        in xs == xs

main = print (foo (Proxy :: Proxy 7))
```

`foo` の中で `replicate` を呼び出すには、型 `2 * n` に対する `KnownNat` のインスタンスが必要です。`n` に関する `KnownNat` インスタンスは利用できるのでGHCが自動で `KnownNat (2 * n)` を導出してくれれば良いのですが、してくれません。

ペアノ自然数の場合はシングルトンを使ったトリックでなんとかすることができましたが、`Nat` カインドおよび `KnownNat` クラスはGHC組み込みなので真っ当な方法では対処できません。

*（以下執筆中）*

## コンパイラープラグインでの解決

* [ghc-typelits-natnormalise: GHC typechecker plugin for types of kind GHC.TypeLits.Nat](https://hackage.haskell.org/package/ghc-typelits-natnormalise)
* [ghc-typelits-presburger: Presburger Arithmetic Solver for GHC Type-level natural numbers.](https://hackage.haskell.org/package/ghc-typelits-presburger)
* [ghc-typelits-knownnat: Derive KnownNat constraints from other KnownNat constraints](https://hackage.haskell.org/package/ghc-typelits-knownnat)

## ライブラリーでの解決

[singletons-baseパッケージ](https://hackage.haskell.org/package/singletons-base-3.1)

```haskell
module Prelude.Singletons where

-- 実際はもっと一般化されているが、ここには Nat カインドの場合に特化したものを載せる
(%+) :: SNat x -> SNat y -> SNat (x + y)
(%-) :: SNat x -> SNat y -> SNat (x - y)
(%*) :: SNat x -> SNat y -> SNat (x * y)
```

```haskell
module GHC.TypeLits.Singletons where

data SNat (n :: Nat) = KnownNat n => SNat

withKnownNat :: SNat n -> (KnownNat n => r) -> r

(%^) :: SNat a -> SNat b -> SNat (a ^ b)
(%<=?) :: SNat a -> SNat b -> SBool (a <=? b)
sLog2 :: SNat x -> SNat (Log2 x)
sDiv :: SNat x -> SNat y -> SNat (Div x y)
sMod :: SNat x -> SNat y -> SNat (Mod x y)
```

[typelits-witnessesパッケージ](https://hackage.haskell.org/package/typelits-witnesses)

singletonsパッケージのサブセットみたいなパッケージです。0.4.0.0の時点では既知のバグがいくつかあるほか、GHC 9.2に対応していません。

[constraintsパッケージ](https://hackage.haskell.org/package/constraints)

```haskell
module Data.Constraint.Nat where
-- 抜粋

plusNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n + m)
minusNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n - m)
timesNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n * m)
powNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n ^ m)
minNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (Min n m)
maxNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (Max n m)
divNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (Div n m)
modNat :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (Mod n m)

plusZero :: forall n. Dict ((n + 0) ~ n)
minusZero :: forall n. Dict ((n - 0) ~ n)
timesZero :: forall n. Dict ((n * 0) ~ 0)
timesOne :: forall n. Dict ((n * 1) ~ n)
powZero :: forall n. Dict ((n ^ 0) ~ 1)
powOne :: forall n. Dict ((n ^ 1) ~ n)
plusAssociates :: forall m n o. Dict (((m + n) + o) ~ (m + (n + o)))
timesAssociates :: forall m n o. Dict (((m * n) * o) ~ (m * (n * o)))
plusCommutes :: forall n m. Dict ((m + n) ~ (n + m))
timesCommutes :: forall n m. Dict ((m * n) ~ (n * m))
plusDistributesOverTimes :: forall n m o. Dict ((n * (m + o)) ~ ((n * m) + (n * o)))
```

[type-naturalパッケージ](https://hackage.haskell.org/package/type-natural)

以前はペアノ自然数を扱うパッケージでしたが、バージョン1.0.0.0以降はGHC組み込みの `Nat` カインドを対象とするようになっているようです（この本を書いている時点での最新版は1.1.0.1）。
`Data.Type.Natural` モジュールにシングルトン型 `SNat` とその演算（`(%+) :: SNat x -> SNat y -> SNat (x + y)` など）が定義されているほか、 `Data.Type.Natural.Lemma.Arithmetic` モジュールや `Data.Type.Natural.Lemma.Order` モジュールで自然数に関する各種「定理」が提供されているようです。

バージョン1.1.0.1時点でのデータ型と関数の一部：

```haskell
module Data.Type.Natural where

data SNat (n :: Nat)
sNat :: KnownNat n => SNat n
sNatP :: KnownNat n => pxy n -> SNat n
toNatural :: SNat n -> Natural
withKnownNat :: SNat n -> (KnownNat n => r) -> r
(%+) :: SNat x -> SNat y -> SNat (x + y)
(%-) :: SNat x -> SNat y -> SNat (x - y)
(%*) :: SNat x -> SNat y -> SNat (x * y)
sDiv :: SNat x -> SNat y -> SNat (Div x y)
sMod :: SNat x -> SNat y -> SNat (Mod x y)
(%^) :: SNat a -> SNat b -> SNat (a ^ b)
sLog2 :: SNat x -> SNat (Log2 x)
(%<=?) :: SNat a -> SNat b -> SBool (a <=? b)
```

```haskell
module Data.Type.Natural.Lemma.Arithmetic where
plusMinus :: SNat n -> SNat m -> ((n + m) - m) :~: n
plusZeroL :: SNat n -> (0 + n) :~: n
plusZeroR :: SNat n -> (n + 0) :~: n
plusComm :: SNat n -> SNat m -> (n + m) :~: (m + n)
```
