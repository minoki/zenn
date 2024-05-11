---
title: "GHC 9.10で実装された可視なforallで遊ぶ"
emoji: "👻"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

*English version*: [Playing with Visible Forall in GHC 9.10 - Mizuki's Blog](https://minoki.github.io/posts/2024-05-11-playing-with-visible-forall.html)

本日、GHC 9.10.1がリリースされました。新機能の一つは、「可視な `forall`」（`RequiredTypeArguments` 拡張）です。この記事では、早速これを使って遊んでみたいと思います。

この機能についてのオフィシャルなドキュメントは以下です：

* [ghc-proposals/proposals/0281-visible-forall.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)
* [6.4.18. Required type arguments — Glasgow Haskell Compiler 9.10.1 User's Guide](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/exts/required_type_arguments.html)

## 基本：`id` 関数

最も簡単な例は、陽に型を受け取る `id` 関数でしょう。普通の `id` 関数と、可視な `forall` を使った `id` 関数はそれぞれ次のように書けます：

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

-- User's Guideより：

-- 普通の id 関数
id :: forall a. a -> a
id x = x

-- 可視な forall を使った id 関数
id_vdq :: forall a -> a -> a
id_vdq a x = x
```

GHCiで実行してみましょう：

```
ghci> :set +t
ghci> id 42  -- 型を推論させる
42
it :: Num a => a
ghci> id @Int 42  -- 型を明示的に与える
42
it :: Int
ghci> id_vdq _ 42  -- 型を推論させる
42
it :: Num w => w
ghci> id_vdq Int 42  -- 型を明示的に与える（@ を使わないことに注意！）
42
it :: Int
```

つまり、`forall ->` で関数を宣言することで、`@` を使わずに型を渡せるようになるわけですね。

注意点として、項と型に同じ表記が用いられる場合、項としての解釈が優先されます。`[Int]` 型を渡してみましょう：

```
ghci> id_vdq [Int] [42]
<interactive>:37:8: error: [GHC-83865]
    • Expected a type, but ‘[Int]’ has kind ‘[*]’
    • In the type ‘[Int]’
      In the expression: id_vdq [Int] [42]
      In an equation for ‘it’: it = id_vdq [Int] [42]
```

`[Int]` が「`Int` からなるリストの型」ではなく、「`Int` を要素として持つ型レベルリスト」として解釈されたためにエラーになりました。この問題を回避する方法は2つあります。

まず一つは、`ExplicitNamespaces` 拡張で使えるようになる `type` 式を使うことです。

```
ghci> :set -XExplicitNamespaces 
ghci> id_vdq (type [Int]) [42]
[42]
it :: [Int]
```

別の方法は、項と型で同じ表記を使うのをやめることです。リスト型やタプル型に対しては、`Prelude.Experimental` モジュールから `List` や `Tuple2` というような別名が提供されています。

```
ghci> :m + Prelude.Experimental
ghci> id_vdq (List Int) [42]
[42]
it :: [Int]
```

## 二項演算子

二項演算子でも型を受け取れるようになります。次の関数を考えてみましょう：

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

as :: forall a. a -> forall a' -> a ~ a' => a'
as x _ = x
```

この関数は、中置演算子として使うことで、型注釈 `::` のような役割を果たします：

```
ghci> :set +t
ghci> 42 `as` Integer
42
it :: Integer
ghci> 42 `as` Rational
42 % 1
it :: Rational
ghci> 42 `as` Double
42.0
it :: Double
```

これだけだとありがたみがないですが、「型の一部」を指定する関数も容易に作ることができます。

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

as' :: forall f a. f a -> forall a' -> a ~ a' => f a'
as' x _ = x
```

```
ghci> :m + Data.Functor.Identity
ghci> Identity 42 `as'` Int
Identity 42
it :: Identity Int
ghci> Identity 42 `as'` Rational
Identity (42 % 1)
it :: Identity Rational
```

もちろん、型の一部を指定するのは従来のHaskellでも `PartialTypeSignatures` 拡張で可能でした。

## 型クラス

型クラスのメソッドで型を受け取ることができると便利そうです。例えば、`sizeOf (undefined :: Int)` の代わりに `sizeOf Int` と書けると楽です。そのような定義は可能でしょうか？

```haskell
-- 架空のコード
class NewStorable a where
  sizeOf :: forall a -> Int
```

残念ながら、これはうまく行きません。型クラスの `a` と `forall` の `a` は別物扱いされてしまうのです。

```haskell
-- 実際の解釈
class NewStorable a where
  sizeOf :: forall a' -> Int

-- 外から見た型は sizeOf :: forall a. NewStorable a => forall a' -> Int となる
```

正攻法は、ラッパーを作ることです。

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}

class NewStorable a where
  sizeOf_ :: Int

-- ラッパー
sizeOf :: forall a -> NewStorable a => Int
sizeOf a = sizeOf_ @a
```

別の方法として、`~` を使ったトリックが考えられます。

```haskell
class NewStorable a where
  sizeOf :: forall a' -> a ~ a' => Int
```

ただ、余計な `=>` を使うと中間コードで微妙な違いが出る可能性があります。次のコードを考えます。

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
import Debug.Trace
import Data.Proxy

newtype Tagged t a = MkTagged { unTagged :: a }

class Foo a where
  someValueAmb :: Int
  someValueTagged :: Tagged a Int
  someValueProxy :: Proxy a -> Int
  someValueVis :: forall a' -> a ~ a' => Int

instance Foo Float where
  someValueAmb = trace "some heavy computation 1" 42
  someValueTagged = MkTagged (trace "some heavy computation 2" 42)
  someValueProxy _ = trace "some heavy computation 3" 42
  someValueVis _ = trace "some heavy computation 4" 42

main :: IO ()
main = do
  print (someValueAmb @Float)
  print (someValueAmb @Float)
  print (unTagged (someValueTagged :: Tagged Float Int))
  print (unTagged (someValueTagged :: Tagged Float Int))
  print (someValueProxy (Proxy @Float))
  print (someValueProxy (Proxy @Float))
  print (someValueVis Float)
  print (someValueVis Float)
```

`someValue` は計算に時間がかかる処理だとします。ここでは実際の計算の代わりに `trace` を呼んで計算回数がわかるようにしています。

`main` ではそれぞれの `someValue` を2回ずつ呼び出していますが、`someValue` の右辺は何回評価されるでしょうか？

まず、最適化がかかっている場合はどれも1回ずつ評価されます。

```
$ ghc-9.10 -O1 Test.hs
$ ./Test
some heavy computation 1
42
42
some heavy computation 2
42
42
some heavy computation 3
42
42
some heavy computation 4
42
42
```

一方で、最適化を切った場合はどうでしょうか。

```
$ ghc-9.10 -O0 Test.hs
$ ./Test
some heavy computation 1
42
42
some heavy computation 2
42
42
some heavy computation 3
42
some heavy computation 3
42
some heavy computation 4
42
some heavy computation 4
42
```

`someValueAmb` と `someValueTagged` は1回ずつしか計算されなかったのに対し、`someValueProxy` と `someValueVis` は2回ずつ計算されました。これは、これらの実体が値 `Int` なのか関数 `_ -> Int` なのかの違いを反映していると考えられます。

このような単純なプログラムでは最適化を有効にすれば違いは出ませんが、もっと複雑で入り組んだプログラムだったら最適化が十分に働かず、違いが出ることが考えられます。効率を最重要視する人は頭の片隅に入れておくと良いでしょう。

## 定理証明

Haskellでは型レベルプログラミングがよく行われます。例えば、型レベルリストの連結 `++` は次のように定義できます：

```haskell
type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)
```

型レベルリストの連結については結合法則 `xs ++ (ys ++ zs) = (xs ++ ys) ++ zs` が成り立ちますが、GHCの型検査器はそれを知りません。GHCの型検査器に非自明な等式を教えてやるには、定理証明を行います。つまり、

```haskell
import Data.Type.Equality ((:~:))

appendIsAssociative :: ... -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
```

という形の関数を定義します。

結合法則を証明するには、`xs` について構造帰納法を使えば良いです。つまり、`xs = '[]` の場合は自明、`xs = x : xss` の場合は

```
(x : xss) ++ (ys ++ zs)
  = x : (xss ++ (ys ++ zs))  （++ の定義より）
  = x : ((xss ++ ys) ++ zs)  （帰納法の仮定より）
  = (x : (xss ++ ys)) ++ zs   （++ の定義より）
  = ((x : xss) ++ ys) ++ zs   （++ の定義より）
```

と証明できます。

では実装してみましょう。証明では `xs` について場合分けが必要なので、

```haskell
data ProxyList xs where
  PNil :: ProxyList '[]
  PCons :: Proxy x -> ProxyList xs -> ProxyList (x ': xs)
```

という型を定義しておきます。`ys`, `zs` については場合分けは必要ないので引数は `Proxy` ライクなもので十分で、証明を表す関数の型は

```haskell
appendIsAssociative :: ProxyList xs -> proxy2 ys -> proxy3 zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
```

となります。証明の本体を等式変形に従って書くと

```haskell
appendIsAssociative PNil _ _ = Refl
appendIsAssociative (PCons (_ :: _ x) (xss :: _ xss)) (ys :: _ ys) (zs :: _ zs) = let
    pf1 :: (x : xss) ++ (ys ++ zs) :~: x : (xss ++ (ys ++ zs))
    pf1 = Refl
    pf2 :: x : (xss ++ (ys ++ zs)) :~: x : ((xss ++ ys) ++ zs)
    pf2 = apply Refl (appendIsAssociative xss ys zs)
    pf3 :: x : ((xss ++ ys) ++ zs) :~: (x : (xss ++ ys)) ++ zs
    pf3 = Refl
    pf4 :: (x : (xss ++ ys)) ++ zs :~: ((x : xss) ++ ys) ++ zs
    pf4 = Refl
  in pf1 `trans` pf2 `trans` pf3 `trans` pf4
```

となります。

さて、証明はできましたが、上の書き方は途中式を2回ずつ書いており、イケてないです。証明の可読性を保ったまま、コードを簡略化できないでしょうか？

理想を言うと、先ほどの

```
(x : xss) ++ (ys ++ zs)
  = x : (xss ++ (ys ++ zs))  （++ の定義より）
  = x : ((xss ++ ys) ++ zs)  （帰納法の仮定より）
  = (x : (xss ++ ys)) ++ zs   （++ の定義より）
  = ((x : xss) ++ ys) ++ zs   （++ の定義より）
```

をそのままコード化できると良いです。

少し前なら、こういうことをするときはシングルトン型を活用したかもしれません。しかし今は `RequiredTypeArguments` があります。`RequiredTypeArguments` を使ってこの記法を実現してみましょう。

基本的な流れは、

```
〈a = b の証明〉 === 〈c〉 `by` 〈b = c の証明〉
```

が型 `a :~: c` を持つように演算子 `===` と `by` を構成すれば良いです。`===` が `c` を受け取るために `forall ->` を使います。つまり、`===` と `by` の型は

```haskell
(===) :: a :~: b -> forall c -> ???
by :: ??? -> b :~: c -> a :~: c
```

となります。`???` の部分は `a`, `b`, `c` の情報を含む必要があります。ここでは `(a :~: b, Proxy c)` としておきます。

まとめると、証明を次のように書くことができるようになりました：

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
import Data.Type.Equality
import Data.Proxy
import Prelude hiding ((++))

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

infixl 1 ===, `by`

(===) :: a :~: b -> forall c -> (a :~: b, Proxy c)
(===) x _ = (x, Proxy)

by :: (a :~: b, Proxy c) -> b :~: c -> a :~: c
by (Refl, _) Refl = Refl

beginProof :: forall a -> a :~: a
beginProof _ = Refl

appendIsAssociative :: ProxyList xs -> proxy2 ys -> proxy3 zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
appendIsAssociative PNil _ _ = Refl
appendIsAssociative (PCons (_ :: _ x) (xss_ :: _ xss)) (ys_ :: _ ys) (zs_ :: _ zs)
  = beginProof ((x : xss) ++ (ys ++ zs))
           === x : (xss ++ (ys ++ zs)) `by` Refl
           === x : ((xss ++ ys) ++ zs) `by` apply Refl (appendIsAssociative xss_ ys_ zs_)
           === (x : (xss ++ ys)) ++ zs `by` Refl
           === ((x : xss) ++ ys) ++ zs `by` Refl

data ProxyList xs where ...
```

いくつか注意点があります。

* 型レベルの `++` を `forall ->` の引数に使うと項レベルの `++` と紛らわしいです。`type` を使うこともできますが記述量が多くなるので、ここでは `Prelude` の `++` を隠しました。
* さっきは `xss :: _ xss` という風に項レベルと型レベルで同じ変数名を使っていましたが、これも不都合なので項レベルの変数名を変えています。

さて、これで十分証明がかっこよくなりましたが、もっと改善できないでしょうか？例えば、「帰納法の仮定を使う」際に `apply Refl` するのではなく単純に `appendIsAssociative ...` と書けないでしょうか？

できます。以下に実装例を載せます。

```haskell
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RequiredTypeArguments #-}
import Data.Type.Equality
import Data.Proxy
import Prelude hiding ((++))

type (++) :: [k] -> [k] -> [k]
type family (++) xs ys
type instance '[] ++ ys = ys
type instance (x ': xs) ++ ys = x ': (xs ++ ys)

infixl 1 ===, `by`

(===) :: a :~: b -> forall c -> b ~ c => a :~: c
(===) Refl _ = Refl

by :: (s ~ t => prop) -> s :~: t -> prop
by proof Refl = proof

beginProof :: forall a -> a :~: a
beginProof _ = Refl

appendIsAssociative :: forall xs -> ProxyListI xs => forall ys -> forall zs -> xs ++ (ys ++ zs) :~: (xs ++ ys) ++ zs
appendIsAssociative xs ys zs = case proxyList' @xs of
  PNil' -> Refl
  PCons' @x @xss ->
    beginProof ((x : xss) ++ (ys ++ zs))
           === x : (xss ++ (ys ++ zs))
           === x : ((xss ++ ys) ++ zs) `by` appendIsAssociative xss ys zs
           === (x : (xss ++ ys)) ++ zs
           === ((x : xss) ++ ys) ++ zs

data ProxyList' xs where
  PNil' :: ProxyList' '[]
  PCons' :: forall x xs. ProxyListI xs => ProxyList' (x ': xs)

class ProxyListI xs where
  proxyList' :: ProxyList' xs

instance ProxyListI '[] where
  proxyList' = PNil'

instance ProxyListI xs => ProxyListI (x ': xs) where
  proxyList' = PCons' @x @xs
```

## 最後に

GHC 9.10リリースに携わったすべての人々に、特に依存型に取り組んでいるSerokell社のGHCチームに感謝します。ありがとう！
