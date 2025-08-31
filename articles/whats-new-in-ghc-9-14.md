---
title: "GHC 9.14の新機能"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

English version: [An Unofficial Guide to What's New in GHC 9.14 - Mizuki's Blog](https://minoki.github.io/posts/2025-08-31-whats-new-in-ghc-9-14.html)

GHC 9.14.1-alpha1が2025年8月20日にリリースされました。

* [GHC 9.14.1-alpha1 released - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-14-1-alpha1-released/12786)

この記事では、GHC 9.14の新機能を筆者の独断と偏見に基づき確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)
* [GHC 9.6の新機能](whats-new-in-ghc-9-6)
* [GHC 9.8の新機能](whats-new-in-ghc-9-8)
* [GHC 9.10の新機能](whats-new-in-ghc-9-10)
* [GHC 9.12の新機能](whats-new-in-ghc-9-12)

です。

この記事は網羅的な紹介記事とはなっていません。特に、筆者が詳しくないRTSやTemplate Haskell周りはカバーできていません。是非、公式のリリースノート類も参照してください：

* [2.1. Version 9.14.1 — Glasgow Haskell Compiler 9.14.0.20250819 User's Guide](https://downloads.haskell.org/ghc/9.14.0.20250819/docs/users_guide/9.14.1-notes.html)
    * [docs/users_guide/9.14.1-notes.rst · ghc-9.14 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/docs/users_guide/9.14.1-notes.rst)
<!-- * [Changelog for base-4.22.0.0 | Hackage](https://hackage.haskell.org/package/base-4.22.0.0/changelog) -->
* [libraries/base/changelog.md · ghc-9.14 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/libraries/base/changelog.md)
* [9.14 · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.14)

GHCのアルファ版をGHCupで試す場合は、prereleaseチャンネルを追加します。詳しくは「[Release channels - User Guide - GHCup](https://www.haskell.org/ghcup/guide/#release-channels)」を参照してください。

```
$ ghcup config add-release-channel prereleases
$ ghcup install ghc 9.14.0.20250819
```

新機能の記事をアルファ版の段階で公開するのは、より多くの人にアルファ版を試して頂くのが目的です。そうすれば、正式リリースまでの間に少しでも多く問題を洗い出すことができるでしょう。というわけで、是非試してみてください。

# 長期サポート（LTS）

* [GHC LTS Releases — The Glasgow Haskell Compiler](https://www.haskell.org/ghc/blog/20250702-ghc-release-schedules.html)
* 経緯
    * [#26067: Please revise the release policy · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26067)
    * [Please revise GHC release policy - Links - Haskell Community](https://discourse.haskell.org/t/please-revise-ghc-release-policy/12158)

これまで、GHCは6ヶ月ごとにメジャーバージョンをリリースする体制でした。そして、直近の3つくらいの系列にバグ修正等がバックポートされます（現状ではGHC 9.10, 9.12, 9.14。[参考](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC-status)）。

しかし、特定のメジャーバージョンがバグ修正で安定したり、エコシステムの対応が追いつくのには時間がかかります。そして、安定的に使えるようになったと思った頃にはサポートの打ち切りが近づいています。例えば、執筆時点（2025年8月）ではGHCupでは9.6.7がrecommendedになっていますが、9.6系のサポートは打ち切られています。

今回、安定したバージョンを長く使いたいユーザーのために、GHCのメジャーバージョンの一部に長期サポート（Long Term Support; LTS）が設定されることになりました。LTSには2〜3年程度のサポートが提供されます。最初のLTSはGHC 9.14となります。

# GHC 9.14に入る機能

## SPECIALIZEプラグマに式を書けるようになる

* [Allow expressions in SPECIALISE pragmas - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0493-specialise-expressions.rst)

GHCにおけるパラメーター多相と型クラスの実装は、型消去と辞書渡しを基本としており、一般にはコストがかかります。効率的なコードを出力させるには、**インライン化**や**特殊化**などの最適化を活用します。

（以前に書いた記事「[HaskellのPrimMonadとうまく付き合う その1](https://zenn.dev/mod_poppo/articles/haskell-primmonad)」でもその辺の話をしました。）

インライン化や特殊化をコンパイラーに指示する方法が、`INLINE` プラグマや `SPECIALIZE` プラグマです。

従来は、`SPECIALIZE` プラグマの文法は

```haskell
{-# SPECIALIZE <name> :: <type> #-}
```

でした。たとえば、`someFunc :: Monad m => m a -> m b -> m ([a], b)` を `m = StateT Int IO` に対して特殊化したかったら

```haskell
{-# SPECIALIZE someFunc :: StateT Int IO a -> State Int IO b -> StateT Int IO ([a], b) #-}
```

と書く必要があります。特殊化したい型が複数回出現する場合はその都度書く必要があり、面倒です。

今回、SPECIALIZEプラグマに式を書けるようになりました。特に、TypeApplications拡張が使えるので、先ほどの例は

```haskell
{-# SPECIALIZE someFunc @(StateT Int IO) #-}
```

と書けます。この他、引数の値について特殊化することもできます：

```haskell
foo :: Int -> [Int] -> [Int]
foo !a = map (+ a)
{-# INLINE [0] foo #-}
{-# SPECIALIZE foo 0 #-}
{- foo 0 = map (+ 0) という書き換え規則を定義するのと等価 -}
```

書き換え規則のように、`forall` で変数を使うこともできます。たとえば、

```haskell
pow :: Num a => a -> Int -> a
pow _ 0 = 1
pow x 1 = x
pow x n | even n = pow (x * x) (n `quot` 2)
        | otherwise = x * pow (x * x) (n `quot` 2)
```

という関数の2番目の引数について特殊化したかったら、

```haskell
{-# SPECIALIZE forall x. pow x 1 #-}
{-# SPECIALIZE forall x. pow x 2 #-}
{-# SPECIALIZE forall x. pow x 3 #-}
{-# SPECIALIZE forall x. pow x 4 #-}
{-# SPECIALIZE forall x. pow x 5 #-}
```

と書きます。

ドキュメント化されていない機能として、従来は

```haskell
{-# SPECIALIZE foo :: T1, T2 #-}
```

という風にカンマ区切りで複数の型を書けましたが、この書き方はGHC 9.18で廃止予定となります。

## `-Wall` で `-Wincomplete-record-selectors` が有効になる

GHC 9.10で、失敗する可能性のあるレコードセレクターが使用された時に警告する `-Wincomplete-record-selectors` オプションが追加されました。GHC 9.14では、これが `-Wall` で有効になるようになります。

```haskell
data T = A { a :: Int }
       | B { b :: String }

f :: T -> String
f t = b t ++ "\n"
```

```
$ ghc-9.14 -Wall recordsel.hs
recordsel.hs:5:7: warning: [GHC-17335] [-Wincomplete-record-selectors]
    Selecting the record field ‘b’ may fail for the following constructors:
      A
  |
5 | f t = b t ++ "\n"
  |       ^
```

## ScopedTypeVariables, TypeApplications, TypeAbstractions周りの変更

従来はScopedTypeVariables拡張とTypeApplications拡張が有効であれば、パターン中に型束縛を書けました。たとえば、以下のコードはGHC 9.12でコンパイルが通ります：

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

data T a = MkT a

f :: forall a. T a -> a
f t = case t of
    MkT @a2 x -> x
```

GHC 9.14以降では、こういうコードではTypeAbstractions拡張を有効にする必要があります。

## OverloadedRecordUpdateの脱糖方法の変更

* [HasField redesign - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0583-hasfield-redesign.rst)

Haskellのレコード周りの拡張に関連して、`HasField` というクラスがあります。

```haskell
module GHC.Records where
class HasField x r a | x r -> a where
  getField :: r -> a
```

DuplicateRecordFields拡張と組み合わせると、同じフィールド名を持つ異なるレコードを（`getField` 関数を介して）扱えます。

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
import GHC.Records

data S = S { foo :: Int }
data T = T { foo :: String }

main :: IO ()
main = do
  print $ getField @"foo" (S 42)
  putStrLn $ getField @"foo" (T "Hello")
```

現状では、`HasField` クラスではレコードフィールドの取得しかできません。これを、フィールドの更新もできるようにしよう、という計画があります。

従来は、次の型を持つ `setField` 関数を追加することでレコードフィールドの更新に対応させる予定でした：

```haskell
setField :: forall x r a. HasField x r a => r -> a -> r
```

実際に、[GHC 9.2](ghc-9-2-and-future)で実装されたRecord Dot Syntaxでは、この形の `setField` 関数に脱糖するようになっています。

新しい計画では、`setField` の引数の順番は次のようになります：

```haskell
class SetField x r a | x r -> a where
  ...
  setField :: a -> r -> r
```

つまり、先にフィールドの内容が来て、後にレコードが来ます。

OverloadedRecordDotの例は、`setField` の引数の順番を考慮すると次のようになります：

```haskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
import Prelude
import GHC.Records

data Foo = Foo { x :: Int } deriving Show

data Bar = Bar { foo :: Foo
               , y :: String
               } deriving Show

class HasField' x r a | x r -> a where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField :: a -> r -> r
#else
  setField :: r -> a -> r
#endif

instance HasField' "x" Foo Int where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField x _ = Foo x
#else
  setField _ x = Foo x
#endif

instance HasField' "foo" Bar Foo where
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0)
  setField foo Bar { foo = _, y = y } = Bar { foo = foo, y = y }
#else
  setField Bar { foo = _, y = y } foo = Bar { foo = foo, y = y }
#endif

u = Bar { foo = Foo { x = 42 }, y = "Hello!" }

main = print (u { foo.x = 37 })
```

## foreign importでのMultilineStringsの許容

* [#25157: Support multiline strings in foreign import · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25157)

foreign import宣言で複数行文字列リテラル（MultilineStrings拡張）が使えるようになりました。JavaScript FFIで便利だと思われます。

以下はissueからの例です。

```haskell
foreign import javascript
  """
  (() => {
    console.log("hello");
    console.log(1 + 1);
  })
  """
  action :: IO ()
```

## `coerce` と型のデフォルト化

* [#21003: Coercible constraints should be picked arbitrarily · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/21003)

`coerce` 関数 / `Coercible` クラスが関係する曖昧な型が、より積極的に解消されるようになりました。たとえば、以下のプログラムはGHC 9.12では型の曖昧さのエラーが出ていましたが、GHC 9.14ではコンパイルが通るようになります：

```haskell
import Data.Coerce

main :: IO ()
main = print $ coerce (42 :: Int)
```

```
$ runghc-9.12 coerce.hs
coerce.hs:4:16: error: [GHC-10283]
    • Couldn't match representation of type ‘a0’ with that of ‘Int’
        arising from a use of ‘coerce’
    • In the second argument of ‘($)’, namely ‘coerce (42 :: Int)’
      In the expression: print $ coerce (42 :: Int)
      In an equation for ‘main’: main = print $ coerce (42 :: Int)
  |
4 | main = print $ coerce (42 :: Int)
  |                ^^^^^^

$ runghc-9.14 coerce.hs
42
```

## LinearTypes拡張の下でレコードフィールドのmultiplicityを指定できる

* [ghc-proposals/proposals/0111-linear-types.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#records-and-projections)
* [#18462: Linear types syntax: multiplicity annotation on records · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/18462)

LinearTypes拡張で、レコードフィールドにmultiplicityを指定できるようになりました。

```haskell
{-# LANGUAGE LinearTypes #-}
import GHC.Exts

data A = MkA Int

data B where
  MkB :: Int -> B

data C where
  MkC :: Int %'Many -> Int %'One -> C

-- GHC 9.14の新機能
data R = R { foo %'Many :: Int, bar %'One :: Int }

fA :: A %1-> Int
fA (MkA x) = x

fB :: B %1-> Int
fB (MkB x) = x

-- エラー（yを捨てている）
-- fC1 :: C %1-> Int
-- fC1 (MkC x y) = x

fC2 :: C %1-> Int
fC2 (MkC x y) = y

-- エラー（barを捨てている）
-- fR1 :: R %1 -> Int
-- fR1 (R { foo, bar }) = foo

fR2 :: R %1 -> Int
fR2 (R { foo, bar }) = bar

main :: IO ()
main = pure ()
```

## ExplicitNamespaces拡張でdataを書けるようになる

* [Namespace-specified imports - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst)

Haskellには、型の名前空間とデータ（項）の名前空間があります。人工的な例ですが、次のコードは型の名前空間に `T :: Type` と `U :: Type -> Type` を定義し、データの名前空間に `U :: Int -> T` と `T :: a -> U a` を定義します。

```haskell
data T = U Int
data U a = T a
```

しかし、型とデータの名前空間の垣根は、DataKinds拡張やRequiredTypeArguments拡張の登場により、曖昧になりつつあります。型とデータで名前を分けておけば曖昧さがなくなり好ましいのですが、`Proxy` 型をはじめとして、型とデータで同じ名前を使う既存のコードはたくさんあります。

```haskell
data Proxy a = Proxy
```

ExplicitNamespaces拡張を使うと、importの際に型だけを選択することができます。さらに、PatternSynonyms拡張を使うと、データ構築子だけを選択できます。

```haskell
-- これは従来のGHC（9.12など）でも通る
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
import qualified Data.Proxy as PT (type Proxy)
import qualified Data.Proxy as PD (pattern Proxy)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

しかし、データ構築子をimportするのに `pattern` と記述するのは不自然です。今回、ExplicitNamespaces拡張が拡張され、`data` を使ってデータ構築子をimportできるようになります。

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
import qualified Data.Proxy as PT (type Proxy)
import qualified Data.Proxy as PD (data Proxy)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

将来的にはドット2つ `..` で型あるいはデータの名前空間の全てをimportできるようになる予定ですが、GHC 9.14の段階では実装されていません（[#25901: Wildcards `..` in import and export lists · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25901)）。

```haskell
-- GHC 9.14の時点ではまだ使えない
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
import qualified Data.Proxy as PT (type ..)
import qualified Data.Proxy as PD (data ..)

main :: IO ()
main = print (PD.Proxy :: PT.Proxy PD.Proxy)
```

これと引き換えに、import/exportリストの `pattern` は非推奨になります（削除の予定があるわけではなさそうです）。GHC 9.14では、import/exportリストに `pattern` が使われた場合に警告するオプション `-Wpattern-namespace-specifier`（`-Wcompat` に含まれる）が新設されます。

## DataKinds拡張なしでカインドにデータ型を使えてしまった不具合の修正

* [#22141: GHC-9.4 accepts "data" in kinds without DataKinds · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22141)

次のコードはデータ型 `Nat` をカインドとして使っているのでDataKinds拡張が必要ですが、GHC 9.4以降ではDataKinds拡張がなくても受理されていました：

```haskell
import Data.Kind (Type)
import GHC.TypeNats (Nat)

type T :: Nat -> Type
data T a = T
```

今回、この不具合が修正されました。

## MonadComprehensions拡張がParallelListComp拡張を含意する

* [#25645: MonadComprehensions does not imply ParallelListComp? · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25645)

MonadComprehensions拡張はParallelListComp拡張を含意することに[ドキュメント上では](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/monad_comprehensions.html#extension-MonadComprehensions)なっていましたが、実際はそうなっていませんでした。

GHC 9.14では次のコードのコンパイルが通ります：

```haskell
{-# LANGUAGE MonadComprehensions #-}

main :: IO ()
main = print [(x,y) | x <- [1,2,3] | y <- ["Alpha","Bravo","Charlie"]]
```

## データ構築子に可視なforallを使えるようになる

* [ghc-proposals/proposals/0281-visible-forall.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)
* [#25127: Visible forall in GADTs · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25127)
* [6.4.19. Required type arguments — Glasgow Haskell Compiler 9.14.0.20250819 User's Guide](https://downloads.haskell.org/ghc/9.14.0.20250819/docs/users_guide/exts/required_type_arguments.html#visible-forall-in-gadts)

GADTsで可視なforall (`forall a ->`) を使えるようになりました。以前書いた記事「[GHC 9.10で実装された可視なforallで遊ぶ](./playing-with-visible-forall)」に登場する `ProxyList'` 型は次のように書き直すことができます：

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

type ProxyList :: [k] -> Type
data ProxyList xs where
  PNil :: ProxyList '[]
  PCons :: forall x -> forall xs -> ProxyListI xs => ProxyList (x : xs)

class ProxyListI xs where
  proxyList :: ProxyList xs

instance ProxyListI '[] where
  proxyList = PNil

instance ProxyListI xs => ProxyListI (x : xs) where
  proxyList = PCons x xs
```

## x86 NCGでのSIMDサポートの拡大

* [#25487: x86 NCG SIMD: Implement pack/insert/broadcast/unpack for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25487)
* [#25643: x86 NCG SIMD: Implement arithmetic operations for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25643)
* [#26096: Better lowering for shuffleFloatX4# and shuffleDoubleX2# · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26096)

SIMDは一つの命令で複数のデータを扱える機能で、モダンなCPUで性能の出るコードを書くのに役立ちます。GHCもSIMDのためのデータ型と組み込み関数を備えています。

しかし、GHCのSIMDプリミティブは、長らくLLVMバックエンドのみの機能でした。最近進展があり、GHC 9.12では、一部の型と演算がx86 NCGバックエンドでもコンパイルできるようになりました。

GHC 9.14では、x86 NCGでサポートされる型と演算が増えます。また、浮動小数点ベクトルのshuffleはGHC 9.12のx86 NCGでは `-mavx` が必要でしたが、AVXがなくてもコンパイルできるようになりました。

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Prim
import GHC.Exts
import GHC.Int

data FloatX4 = FloatX4 FloatX4#

packFloatX4 :: (Float, Float, Float, Float) -> FloatX4
packFloatX4 (F# x0, F# x1, F# x2, F# x3) = FloatX4 (packFloatX4# (# x0, x1, x2, x3 #))

unpackFloatX4 :: FloatX4 -> (Float, Float, Float, Float)
unpackFloatX4 (FloatX4 v) = case unpackFloatX4# v of (# x0, x1, x2, x3 #) -> (F# x0, F# x1, F# x2, F# x3)

plusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
plusFloatX4 (FloatX4 u) (FloatX4 v) = FloatX4 (plusFloatX4# u v)

-- GHC 9.14では shuffleFloatX4# が -mavx なしで使えるようになった
reverseFloatX4 :: FloatX4 -> FloatX4
reverseFloatX4 (FloatX4 v) = FloatX4 (shuffleFloatX4# v v (# 3#, 2#, 1#, 0# #))

-- GHC 9.14では Int32X4# などがx86 NCGバックエンドで使えるようになった
data Int32X4 = Int32X4 Int32X4#

packInt32X4 :: (Int32, Int32, Int32, Int32) -> Int32X4
packInt32X4 (I32# x0, I32# x1, I32# x2, I32# x3) = Int32X4 (packInt32X4# (# x0, x1, x2, x3 #))

unpackInt32X4 :: Int32X4 -> (Int32, Int32, Int32, Int32)
unpackInt32X4 (Int32X4 v) = case unpackInt32X4# v of (# x0, x1, x2, x3 #) -> (I32# x0, I32# x1, I32# x2, I32# x3)

plusInt32X4 :: Int32X4 -> Int32X4 -> Int32X4
plusInt32X4 (Int32X4 u) (Int32X4 v) = Int32X4 (plusInt32X4# u v)

reverseInt32X4 :: Int32X4 -> Int32X4
reverseInt32X4 (Int32X4 v) = Int32X4 (shuffleInt32X4# v v (# 3#, 2#, 1#, 0# #))

main :: IO ()
main = do
  let a = packFloatX4 (1.0, 2.0, 3.0, 4.0)
      b = packFloatX4 (0.1, 0.2, 0.3, 0.4)
      c = plusFloatX4 a b
  print (unpackFloatX4 c)
  print (unpackFloatX4 (reverseFloatX4 c))
  let d = packInt32X4 (10, 20, 30, 40)
      e = packInt32X4 (1, 2, 3, 4)
      f = plusInt32X4 d e
  print (unpackInt32X4 f)
  print (unpackInt32X4 (reverseInt32X4 f))
```

現段階では、x86 NCGで扱えるベクトルの幅は128ビットに限られています。

## GHCiがmultiple home unitsに対応した

* [Making GHCi compatible with multiple home units - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2025/06/ghci-multiple-home-units/)
* [#20889: MHU: Most of GHCi commands do not work · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/20889)

GHCiで、複数の単位（ライブラリーや実行ファイル）からなるプロジェクトを上手く扱えるようになるようです。私の理解度では気の利いた説明はできないので、手を動かして試してみることにします。

まず、適当なプロジェクトを作ります：

```
$ mkdir mhu-example && cd mhu-example
$ cabal init -n --tests --libandexe -d "base >=4.19 && <4.23"
```

REPLで実行してみましょう：

```
$ cabal repl -w ghc-9.12 exe:mhu-example
Resolving dependencies...
Build profile: -w ghc-9.12.2 -O1
In order, the following will be built (use -v for more details):
 - mhu-example-0.1.0.0 (lib) (configuration changed)
 - mhu-example-0.1.0.0 (interactive) (exe:mhu-example) (configuration changed)
Configuring library for mhu-example-0.1.0.0...
Preprocessing library for mhu-example-0.1.0.0...
Building library for mhu-example-0.1.0.0...
[1 of 1] Compiling MyLib            ( src/MyLib.hs, dist-newstyle/build/x86_64-osx/ghc-9.12.2/mhu-example-0.1.0.0/build/MyLib.o, dist-newstyle/build/x86_64-osx/ghc-9.12.2/mhu-example-0.1.0.0/build/MyLib.dyn_o )
Configuring executable 'mhu-example' for mhu-example-0.1.0.0...
Preprocessing executable 'mhu-example' for mhu-example-0.1.0.0...
GHCi, version 9.12.2: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
ghci> main
Hello, Haskell!
someFunc
ghci>
```

良いですね。

REPLを開いたまま、`src/MyLib.hs` の内容を次のように書き換えてみましょう：

```haskell
module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc, revised"
```

REPLでリロード（`:r`）して再び実行すると、出力が変わって……いません！

```
ghci> :r
Ok, one module reloaded.
ghci> main
Hello, Haskell!
someFunc
```

詳しい説明はリンクを貼った記事を参照して欲しいのですが、これが旧来のGHCの限界ということのようです。

今度は、GHC 9.14で `--enable-multi-repl` を指定し、cabalに `lib:mhu-example` も渡してみましょう。

```
$ cabal repl -w ghc-9.14 --enable-multi-repl exe:mhu-example lib:mhu-example
Resolving dependencies...
Build profile: -w ghc-9.14.0.20250819 -O1
In order, the following will be built (use -v for more details):
 - mhu-example-0.1.0.0 (interactive) (lib) (configuration changed)
 - mhu-example-0.1.0.0 (interactive) (exe:mhu-example) (configuration changed)
Configuring library for mhu-example-0.1.0.0...
Preprocessing library for mhu-example-0.1.0.0...
Configuring executable 'mhu-example' for mhu-example-0.1.0.0...
Preprocessing executable 'mhu-example' for mhu-example-0.1.0.0...
GHCi, version 9.14.0.20250819: https://www.haskell.org/ghc/  :? for help
[1 of 3] Compiling MyLib            ( src/MyLib.hs, interpreted )[mhu-example-0.1.0.0-inplace]
[2 of 3] Compiling Main             ( app/Main.hs, interpreted )[mhu-example-0.1.0.0-inplace-mhu-example]
Ok, two modules loaded.
ghci> Main.main
Hello, Haskell!
someFunc, revised
```

良いですね。

再びREPLを開いたまま、`src/MyLib.hs` の内容を次のように書き換えてみましょう：

```haskell
module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc, revised^2"
```

今度はREPLをリロードするとライブラリー（`src/MyLib.hs`）の変更も反映されました。

```
ghci> :r
[1 of 3] Compiling MyLib            ( src/MyLib.hs, interpreted )[mhu-example-0.1.0.0-inplace] [Source file changed]
Ok, two modules reloaded.
ghci> Main.main
Hello, Haskell!
someFunc, revised^2
```

## ライブラリー

ライブラリー（主にbaseパッケージ）の変更も紹介します。

### `fail` に `HasCallStack` がつく

* [Add HasCallStack to Control.Monad.fail · Issue #327 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/327)

`HasCallStack` はスタックトレースを取るための軽量な仕組みで、失敗しうる関数（`error` を呼ぶ可能性のある関数）につけておくとデバッグが捗ります。

今回、`MonadFail` クラスの `fail` 関数に `HasCallStack` がつきました。`fail` はdo構文のパターンマッチ失敗で呼ばれる関数です。試してみましょう：

```haskell
someFunc :: [Int] -> IO ()
someFunc xs = do
  [a,b] <- pure xs
  print (a + b)

main :: IO ()
main = do
  someFunc [1,3]
  someFunc [2]
```

```
$ runghc-9.12 fail.hs
4
fail.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail.hs:3:3-7)
$ runghc-9.14 fail.hs
4
fail.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail.hs:3:3-7)

HasCallStack backtrace:
  throwIO, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:66:12 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a type signature in an instance, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:65:13 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a do statement, called at fail.hs:3:3 in main:Main
```

GHC 9.14ではエラーメッセージにスタックトレースがついてリッチになりました。まあ、この例だとそもそもメッセージ中に失敗したパターンマッチの位置（`fail.hs:3:3-7`）がついているのでありがたみが少ないかもしれません。

`HasCallStack` は、関数につけることで呼び出し元の情報を追加できます。`someFunc` に `HasCallStack` をつけると、`someFunc` をどこで呼び出したのかわかるようになります。

```haskell
import GHC.Stack (HasCallStack)

someFunc :: HasCallStack => [Int] -> IO ()
someFunc xs = do
  [a,b] <- pure xs
  print (a + b)

main :: IO ()
main = do
  someFunc [1,3]
  someFunc [2]
```

```
$ runghc-9.12 fail2.hs
4
fail2.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail2.hs:5:3-7)
$ runghc-9.14 fail2.hs
4
fail2.hs: Uncaught exception ghc-internal:GHC.Internal.Exception.ErrorCall:

user error (Pattern match failure in 'do' block at fail2.hs:5:3-7)

HasCallStack backtrace:
  throwIO, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:66:12 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a type signature in an instance, called at libraries/ghc-internal/src/GHC/Internal/Control/Monad/Fail.hs:65:13 in ghc-internal:GHC.Internal.Control.Monad.Fail
  a do statement, called at fail2.hs:5:3 in main:Main
  someFunc, called at fail2.hs:11:3 in main:Main
```

GHC 9.14では、スタックトレースに `main` 関数内の位置（`fail2.hs:11:3`）が追加されました。

### `Data.Enum.enumerate` の導入

* [enumerate Function · Issue #306 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/306)

列挙型の全ての要素を含むリストを返す関数 `enumerate` が追加されました。これまでは `[minBound..maxBound]` というパターンを使う人が多かったと思います。

```haskell
import Data.Enum

data Color = Red | Green | Blue deriving (Show, Enum, Bounded)

main :: IO ()
main = do
  print ([minBound..maxBound] :: [Color])
  print (enumerate @Color)
```

```
$ runghc-9.14 enum.hs 
[Red,Green,Blue]
[Red,Green,Blue]
```

# おまけ：私の貢献

私（@mod_poppo）が行なった貢献（バグ報告や修正など）で、GHC 9.14に入るものを備忘録代わりに書いておきます。

* x86 NCGバックエンドで整数のSIMDプリミティブをコンパイルできるようにする。
    * [#25487: x86 NCG SIMD: Implement pack/insert/broadcast/unpack for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25487)
    * [#25643: x86 NCG SIMD: Implement arithmetic operations for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25643)
* x86 NCGバックエンドで浮動小数点数のシャッフルを（AVXではなく）SSE2命令にコンパイルできるようにする。
    * [#26096: Better lowering for shuffleFloatX4# and shuffleDoubleX2# · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26096)
* AArch64 NCGでの算術右シフトの問題の修正。
    * [#26061: Sub-word arithmetic right shift with AArch64 NCG · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26061)
    * GHC 9.10.3へバックポートされる予定。9.12系へもバックポートされる予定。
* i386 NCGでのbswap64の修正。
    * [!14363: x86 NCG: Fix code generation of bswap64 on i386 · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/14363)
    * GHC 9.10.3へバックポートされる予定。9.12系へもバックポートされる予定。
* LLVMのバージョン検出の問題の修正。
    * [!13763: Fix LLVM version detection (#25606) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13763)
    * GHC 9.10.2へバックポート済み。9.12系へもバックポートされる予定。

これらの貢献は趣味としてやっています。私を支援したいと思った方には、Zennでバッジを送る、「だめぽラボ」の同人誌を買う、GitHub Sponsorsで支援するなどの手段があります。

* [同人サークル「だめぽラボ」](https://lab.miz-ar.info/)
* [Sponsor @minoki on GitHub Sponsors](https://github.com/sponsors/minoki)
    * 執筆時点では、@toyboot4e さんと @kevin-kmetz さんに支援していただいています。

自分でもGHCに貢献してみたい、という人は「[GHCへの私の貢献2023](https://blog.miz-ar.info/2023/12/my-contributions-to-ghc/)」に書いたことも参考にしてください。まずは[GitLab](https://gitlab.haskell.org/ghc/ghc)を眺めて雰囲気を掴むのが良いでしょうか。アカウント作成はスパム対策の関係で手間がかかるかもしれません。最近はAnubisの導入を試しているみたいです。
