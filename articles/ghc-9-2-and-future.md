---
title: "GHC 9.2の新機能と、GHCの動向2021"
emoji: "📝"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

[Haskell Day 2021](https://haskell.jp/haskell-day-2021/)で「GHCの動向2021」というタイトルで発表しました、mod\_poppoです。この記事では、発表の補足としてGHC 9.2の新機能の紹介と、 `Data.List` の単相化に関する補足を行います。

Haskell Day 2021の動画と筆者のスライドは、それぞれ次のリンクから参照できます：

* [Haskell Day 2021 - YouTube](https://www.youtube.com/watch?v=haZl-q6mfyk)
* [GHCの動向2021](https://drive.google.com/file/d/1kPEGux6w_9GgTziRwfDCPH5fRea_vhls/view?usp=sharing)

「GHCの動向」のタイトル的な元ネタ（？）は、筆者が3月にブログに書いた

* [GHCに初めてコントリビュートした／最近のGHC動向](https://blog.miz-ar.info/2021/03/my-first-contribution-to-ghc/)

です。時代遅れになった記述もあると思いますが、よかったら読んでみてください。

# GHC 9.2の新機能

2021年10月29日に、待望のGHC 9.2.1がリリースされました。公式の変更点は

* [2.1. Version 9.2.1 — Glasgow Haskell Compiler 9.2.1 User's Guide](http://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/9.2.1-notes.html)

を参照してください。

## 言語機能

### Record Dot Syntax

* [ghc-proposals/0282-record-dot-syntax.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst)

GHC 9.2時点でのGHC拡張としての名前は `OverloadedRecordDot` と `OverloadedRecordUpdate` です。

レコードのフィールドにドット `.` でアクセスできるようになります。例えば、次のようなレコード

```haskell
data Foo = Foo { x :: Int } deriving Show
data Bar = Bar { foo :: Foo
               , y :: String
               } deriving Show
u = Bar { foo = Foo { x = 42 }, y = "Hello!" }
```

があった時に、 `u.foo.x` や `u.y` でフィールドにアクセスできるようになります（これまではセレクター関数を使って `x (foo u)` とか `y u` と書く必要がありました）。

セクション `(.foo.x)` も使えます。

さて、この拡張は `HasField` クラスを利用しています。`HasField` クラスのインスタンスを独自に定義すれば、型に元々存在しないフィールドを生やすことができます。例として、行列を表す型に転置を返すフィールドを追加してみましょう：

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
import GHC.Records (HasField(..))
import Data.List qualified as List

newtype Matrix a = Matrix [[a]] deriving Show

instance HasField "transpose" (Matrix a) (Matrix a) where
  getField (Matrix m) = Matrix (List.transpose m)

main = do
  let m = Matrix [[1,2],[3,4]]
  print m
  print m.transpose
```

（このコードは `ImportQualifiedPost` 拡張や `MultiParamTypeClasses` 拡張を使っていますが、後述する `GHC2021` のおかげでそれらに対応する `LANGUAGE` プラグマを書かずに済んでいます。）

実行例は次のようになります：

```haskell
Matrix [[1,2],[3,4]]
Matrix [[1,3],[2,4]]
```

このような使い方は「濫用」と感じられる方もいるかもしれませんが、夢が広がることは間違いないでしょう。

`OverloadedRecordUpdate` を使うと `u { foo.x = 37 }` という風にネストしたフィールドの更新を簡潔に書ける……予定なのですが、GHC 9.2の段階では脱糖先の `setField` 関数がまだ実装されていないので実質使えないようです。一応 `RebindableSyntax` を有効にすれば

```haskell
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
  setField :: r -> a -> r

instance HasField' "x" Foo Int where
  setField _ x = Foo x

instance HasField' "foo" Bar Foo where
  setField Bar { foo = _, y = y } foo = Bar { foo = foo, y = y }

u = Bar { foo = Foo { x = 42 }, y = "Hello!" }

main = print (u { foo.x = 37 })
```

という風に独自に `setField` 関数を用意してやることによってフィールドの更新ができるようです。

### `NoFieldSelectors`

* [ghc-proposals/0160-no-toplevel-field-selectors.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst)
* [新しいGHC拡張、NoFieldSelectorsについて - モナドとわたしとコモナド](https://fumieval.hatenablog.com/entry/2020/12/29/190347)

例えば

```haskell
data Bar = Bar { foo :: Foo
               , y :: String
               }
```

というデータ型を定義すると、トップレベルにフィールドと同じ名前の

```haskell
foo :: Bar -> Foo
y :: Bar -> String
```

という関数（セレクター関数）が定義されます。`NoFieldSelectors` 拡張はこれを抑制します。

この拡張が有効な場合、フィールドへのアクセスは他の方法、レコード構文やコンストラクター、あるいは `HasField` を使って行うことになります。

### `GHC2021`

* [ghc-proposals/0372-ghc-extensions.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0372-ghc-extensions.rst)
* [ghc-proposals/0380-ghc2021.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0380-ghc2021.rst)

よく使われる、保守的で無害で枯れたGHC拡張がデフォルトで有効化されます。

例を挙げると、 `BangPatterns`, `RankNTypes`, `TypeApplications`, `ScopedTypeVariables`, `MultiParamTypeClasses`, `GeneralizedNewtypeDeriving` です。

完全なリストは次のとおりです：

* 構文：`EmptyCase`, `PostfixOperators`, `TupleSections`, `ImportQualifiedPost`, `NamedFieldPuns`, `BangPatterns`
* リテラル：`BinaryLiterals`, `HexFloatLiterals`, `NumericUnderscores`
* 型：`GADTSyntax`, `RankNTypes`, `TypeApplications`, `PolyKinds`, `StandaloneKindSignatures`, `ExistentialQuantification`, `TypeOperators`, `ConstraintKinds`
* 型注釈：`ExplicitForAll`, `KindSignatures`, `NamedWildCards`, `ScopedTypeVariables`
* クラスとインスタンスの宣言：`FlexibleContexts`, `FlexibleInstances`, `MultiParamTypeClasses`, `ConstrainedClassMethods`, `InstanceSigs`, `TypeSynonymInstances`
* deriving: `DeriveDataTypeable`, `DeriveFoldable`, `DeriveFunctor`, `DeriveTraversable`, `StandaloneDeriving`, `EmptyDataDeriving`, `DeriveLift`, `GeneralisedNewtypeDeriving`, `DeriveGeneric`

これは `Haskell98` や `Haskell2010` と並ぶ、言語を指定するGHC拡張として扱われます。`NoGHC2021` はありません。

`Haskell98` や `Haskell2010` を指定しない場合はデフォルトで有効になりますが、逆に言うと.cabalファイルに `default-language: Haskell2010` と書いているパッケージでは `GHC2021` は有効になりません。

### `UnliftedDatatypes`

* [ghc-proposals/0265-unlifted-datatypes.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0265-unlifted-datatypes.rst)

Haskellで定義する通常のデータ型はboxedかつliftedです。boxedというのは、実体がヒープ上に確保されて束縛がポインターで表現されること、liftedというのはサンクの状態（ボトムを含む）を許容することです。

例えば、

```haskell
{-# LANGUAGE MagicHash #-}
import GHC.Exts (ByteArray#)

data Foo = Foo Int String

main = do let x :: Foo
              x = undefined
          putStrLn "Hello"
          let y :: ByteArray#
              y = undefined
          putStrLn "world"
```

というコードを動かすと、 `Foo` はliftedな型なので束縛 `x = undefined` は成功しますが、 `ByteArray#` はunliftedな型なので `y = undefined` は実行時エラーになり、 `putStrLn "world"` は実行されません。

`UnliftedDatatypes` 拡張では、データ型の定義にカインド注釈をつけることにより、unliftedなデータ型をユーザーが定義することを可能にします。

次のコードをGHC 9.2で実行してみましょう：

```haskell
{-# LANGUAGE UnliftedDatatypes #-}
import GHC.Exts (TYPE, RuntimeRep(BoxedRep), Levity(Unlifted))

type Foo :: TYPE (BoxedRep Unlifted)
data Foo = Foo Int String

main = do let x :: Foo
              x = undefined
          putStrLn "Hello"
```

（`data Foo` の前の行で `type Foo ::` としているのはGHC 8.10で実装された `StandaloneKindSignatures` 拡張です。これも `GHC2021` に含まれているので `LANGUAGE` プラグマは省略できます。）

今度は、`x = undefined` で実行時エラーが出て `putStrLn "Hello"` が実行されないかと思います。

カインド注釈の書き方は、 `StandaloneKindSignatures` の他に `GADTSyntax`+`KindSignatures` でも可能です。

```haskell
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTSyntax #-}
import GHC.Exts (TYPE, RuntimeRep(BoxedRep), Levity(Unlifted))

data Foo :: TYPE (BoxedRep Unlifted) where
  Foo :: Int -> String -> Foo
```

### Quick-Look Impredicativity

* [ghc-proposals/0274-quick-look-impredicativity.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0274-quick-look-impredicativity.rst)

非可述多相 (impredicative polymorphism) についての拡張です。非可述多相については以前

* [非可述多相に触れる： GHC の ImpredicativeTypes 拡張](https://qiita.com/mod_poppo/items/806c9c3e0ccb46be92ae)

という記事を書きました。

これまでは `ImpredicativeTypes` は不安定で実験的な機能でしたが、Quick-Look Impredicativityが実装されることによってより安心して使えるようになるのではないかと思います（たぶん）。

`ImpredicativeTypes` は、lensをデータ型に入れようとすると必要になります。例えば、次のコードは `ImpredicativeTypes` が有効でないと怒られます（以下のコードは以前のGHCの `ImpredicativeTypes` でも動くのでQuick Lookとは関係ありませんが）：

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, microlens, generic-lens-lite
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
import Lens.Micro (Lens', (^.))
import Data.Generics.Lens.Lite (field)
import Control.Monad
import GHC.Generics (Generic)

data Foo = Foo { a :: Int
               , b :: Int
               , c :: Int
               } deriving Generic

fields :: [Lens' Foo Int] -- !!!
fields = [ field @"a"
         , field @"b"
         , field @"c"
         ]

main = do let foo = Foo 2 3 5
          forM_ fields (\lens -> print $ foo ^. lens)
```

## 標準ライブラリーの変更

公式の変更履歴は

* [2.1. Version 9.2.1 — Glasgow Haskell Compiler 9.2.1 User's Guide](http://downloads.haskell.org/ghc/9.2.1/docs/html/users_guide/9.2.1-notes.html#base-library)
* [Changelog for base-4.16.0.0 | Hackage](https://hackage.haskell.org/package/base-4.16.0.0/changelog)

を参照してください。

### `Nat` カインドが `Natural` 型のエイリアスに

* [ghc-proposals/0364-unify-natural.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0364-unify-natural.rst)

GHC組み込みの型レベル自然数（リテラルが書けるもの、GHC 7.6以降）は従来は `Nat` カインドを使用していました。これは、 `Nat` 型を `DataKinds` 拡張でカインドに持ち上げたものです。詳しくは[GHCの型レベル自然数を理解する](https://qiita.com/mod_poppo/items/3a37424d299a9f71b757)を参照してください。

一方、最近のGHC（7.10以降）には `Numeric.Natural` モジュールからエクスポートされる実行時の自然数型 `Natural` があります。

同じ自然数を表すのに、型レベルと実行時で異なる型を使い分けるのは不毛です。これには実害もあって、例えば自然数の組を表す型 `NatPair` の型レベル版（`DataKinds` 拡張でカインドに持ち上げて使う）と実行時版の両方を使いたい場合に

```haskell
data NatPair = MkNatPair Nat Nat
data NatPair' = MkNatPair' Natural Natural
-- Nat型とNatural型が同じであれば一つで済むのに……
```

と別々にデータ型を定義しなければならない、という事態になっています。

GHC 9.2では `Nat` 型と `Natural` 型が統合されます。`GHC.TypeLits` モジュールでは `Nat` が次のように定義されるので、既存のコードは概ねそのまま動くでしょう：

```haskell
type Nat = Natural
```

（`Nat` カインドについてインスタンスを定義していたパッケージ（singletons等）は `FlexibleInstances` が必要になったりするので、厳密には破壊的変更です。）

### `Char` カインド（型レベルChar）と文字列操作

* [ghc-proposals/0387-char-kind.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0387-char-kind.rst)

型レベルプログラミングに新しい仲間が登場です。これまでは、型レベル自然数（`Nat` 改め `Natural` カインド）や型レベル文字列（`Symbol` カインド）を型レベルのリテラルとして使うことができましたが、これからは型レベル文字（`Char` カインド）についても型レベルのリテラルを書くことができます：

```haskell
ghci> :set -XDataKinds
ghci> :kind 'a'
'a' :: Char
```

実行時の値との変換は `Nat` や `Symbol` と同じような感じで、

```haskell
class KnownChar (n :: Char)
charVal :: KnownChar n => proxy n -> Char
charVal' :: KnownChar n => Proxy# n -> Char
data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)
someCharVal :: Char -> SomeChar
```

を介して行います。[GHCの型レベル自然数を理解する](https://qiita.com/mod_poppo/items/3a37424d299a9f71b757)も参考にしてください。

まあ `Char` 単体は割とどうでもよくて、重要なのは、**型レベル文字列を分解・解析できる**ことでしょう。`GHC.TypeLits` には次のような（組み込み）型関数が追加されています：

```haskell
type ConsSymbol :: Char -> Symbol -> Symbol
type UnconsSymbol :: Symbol -> Maybe (Char, Symbol)
```

これによって、型レベル文字列のパースがこれまでよりも楽になることが期待されます（これまでは [symbols: Symbol manipulation](https://hackage.haskell.org/package/symbols) という手段がありました）。

### 型レベル比較演算

これまでは型レベル自然数や文字列の比較演算として、 `GHC.TypeLits` で

```haskell
type (<=) :: Nat -> Nat -> Constraint
type (<=?) :: Nat -> Nat -> Bool
type CmpNat :: Nat -> Nat -> Ordering
type CmpSymbol :: Symbol -> Symbol -> Ordering
```

が提供されていました。今回、型レベル文字が加わったこともあって、型レベル比較演算が再編されました。

新たなAPIは、`Data.Type.Ord` モジュールで

```haskell
-- k = Natural, Symbol, Char
type Compare :: k -> k -> Ordering
type (<) :: k -> k -> Constraint -- GHC 9.2.1ではバグっているので注意 (https://gitlab.haskell.org/ghc/ghc/-/issues/20625)
type (<?) :: k -> k -> Bool
type (>) :: k -> k -> Constraint
type (>?) :: k -> k -> Bool
type (<=) :: k -> k -> Constraint
type (<=?) :: k -> k -> Bool
type (>=) :: k -> k -> Constraint
type (>=?) :: k -> k -> Bool
```

と定義されています。カインドについてオーバーロードされているのが特徴です。

もちろん、 `GHC.TypeLits` の既存のAPIはGHC 9.2でも（再）エクスポートされています。ただ、 `(<=?)` の定義が変わったので、compiler pluginの類は改修が必要かもしれません。

### `IntN`, `WordN` (`N`=8, 16, 32)の内部表現の変更

これまでは `IntN`, `WordN` 型の内部表現は [`GHC.Int`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Int.html) / [`GHC.Word`](https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Word.html) で

```haskell
-- GHC.Int
data Int8 = I8# Int#
data Int16 = I16# Int#
data Int32 = I32# Int#

-- GHC.Word
data Word8 = W8# Word#
data Word16 = W16# Word#
data Word32 = W32# Word#
```

と定義されていましたが、GHC 9.2では新たに導入される `IntN#`, `WordN#` 型を使って

```haskell
-- GHC.Int
data Int8 = I8# Int8#
data Int16 = I16# Int16#
data Int32 = I32# Int32#

-- GHC.Word
data Word8 = W8# Word8#
data Word16 = W16# Word16#
data Word32 = W32# Word32#
```

と定義されます。

動機としては、Arm64 Darwin (Apple Silicon)のCの呼び出し規約に対応するにあたって整数型の正確な幅がわかっていないと不都合だ、というようなもののようです。変更履歴には何故か載っていないので、事情については

* [Sized Primitives (!4390) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4390)

を見てください。

GHC 9.2.1の段階では `Int64` / `Word64` の内部表現は従来通り（64ビット環境で `Int#` / `Word#`, 32ビット環境で `Int64#` / `Word64#`）です。これも将来変わるかもしれません。

### ビット演算の `Semigroup` / `Monoid` インスタンスを提供するnewtype wrapperの追加

モノイドについては、2019年の筆者の記事

* [Haskellerのためのモノイド完全ガイド](https://blog.miz-ar.info/2019/02/monoid-for-haskellers/)

を参照してください。

`Int` のような数値型には足し算、掛け算など、複数のモノイド構造があるため、 `Int` 自身は `Monoid` インスタンスになっておらず、`Sum` や `Product` 等のnewtype wrapperを通して `Monoid` インスタンスが提供されていることは「完全ガイド」で説明した通りです。

整数型には他にもビット演算等の他のモノイド構造もありますが、これまではそれらに対応するnewtype wrapperは提供されていませんでした。そう、GHC 9.2が登場するまでは。

GHC 9.2では、`Data.Bits` モジュールに次のようなnewtype wrapperが追加されました：

```haskell
newtype And a = And { getAnd :: a } -- bitwise AND
newtype Ior a = Ior { getIor :: a } -- bitwise (inclusive) OR
newtype Xor a = Xor { getXor :: a } -- bitwise XOR
newtype Iff a = Iff { getIff :: a } -- bitwise 'equality' (if and only if, ⇔)
```

これらは、それぞれのビット演算に対応する `Semigroup` / `Monoid` のインスタンスを提供します。

`And` と `Iff` の単位元は「すべてのビットが1であるような値」で、 `Natural` 型（自然数型）にはそういう値はないため、 `Monoid (And a)` と `Monoid (Iff a)` の制約は `Bits a` ではなく `FiniteBits a` となっています。

### 標準的な1要素タプル型：`Data.Tuple.Solo`

これまでのHaskellには、0要素タプル型 `()` や2要素以上のタプル型 `(,)`, `(,,)`, `(,,,)`, ...はありましたが、1要素のタプル型はありませんでした。

なければ作ればいいじゃない、ということで

* [OneTuple: Singleton Tuple](https://hackage.haskell.org/package/OneTuple)
    * `data OneTuple a = OneTuple a`
* [Only: The 1-tuple type or single-value "collection"](https://hackage.haskell.org/package/Only)
    * `newtype Only a = Only { fromOnly :: a }`

といったライブラリーがあったわけですが、この度標準ライブラリー(base)に正式に1要素タプル型が入ることになりました。

実は1要素タプルはGHC 8.0の頃からghc-primパッケージで [`Unit` という名前でひっそりと](https://downloads.haskell.org/~ghc/8.0.1/docs/html/libraries/ghc-prim-0.5.0.0/GHC-Tuple.html#t:Unit)公開されていたのですが、

* GHC 9.0で `Unit` から `Solo` という名前に変更され、
* GHC 9.2でbaseパッケージの `Data.Tuple` モジュールから再エクスポートされるようになった（一般ユーザー的に使いやすくなった）

のが変化です。

`Solo` は `newtype` ではなく `data` として定義されています。定義としては上記の `OneTuple` 型に近く、`Solo undefined` をWHNFまで評価してもエラーは出ません：

```haskell
GHCi, version 9.2.1: https://www.haskell.org/ghc/  :? for help
ghci> Data.Functor.Identity.Identity undefined `seq` () -- Identity型はnewtypeなので中にundefinedを入れた状態で評価するとエラーが飛ぶ
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at <interactive>:1:32 in interactive:Ghci1
ghci> Data.Tuple.Solo undefined `seq` () -- Solo型は（非正格な）dataなので中にundefinedを入れた状態で評価してもエラーにならない
()
```

### `Data.Semigroup.Option` の削除

`Data.Semigroup.Option` が何であったかというのは、[Haskellerのためのモノイド完全ガイド](https://blog.miz-ar.info/2019/02/monoid-for-haskellers/)で紹介しました。要するに `Maybe` のnewtype wrapperで、GHC 8.4（2018年3月リリース）で `Semigroup` が `Monoid` のスーパークラスとなってからは不要となっていました。

そもそも `Option` 型を使っている人がどのくらいいたのかはわかりませんが、 `Option` 型にインスタンスを提供しているライブラリーというのは一定数ありそうなので、そういうライブラリーはアップデートが必要そうです。

### Pointer Rep (`BoxedRep`)

* [ghc-proposals/0203-pointer-rep.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0203-pointer-rep.rst)

Levity polymorphism関連の変更です。

これまではliftedな型とunliftedな型は `RuntimeRep` の別々のコンストラクターで

```haskell
type TYPE :: RuntimeRep -> Type
data RuntimeRep = LiftedRep
                | UnliftedRep
                | IntRep
                | ...
type Type = TYPE 'LiftedRep
```

と表現されていましたが、これらは `BoxedRep` に統合されて

```haskell
type TYPE :: RuntimeRep -> Type
data RuntimeRep = BoxedRep Levity
                | IntRep
                | ...
data Levity = Lifted | Unlifted
type LiftedRep = 'BoxedRep 'Lifted
type UnliftedRep = 'BoxedRep 'Unlifted
type Type = TYPE LiftedRep
type UnliftedType = TYPE UnliftedRep
```

となります。互換性のために `LiftedRep` と `UnliftedRep` は型エイリアスとして定義されていますが、以前のようなプライムをつけた使い方はできないので注意してください。

GHC組み込みのunliftedな配列型 `Array#` はこれまでは

```haskell
type Array# :: Type -> UnliftedType
```

というカインドを持っていましたが、`BoxedRep` の導入により将来的には

```haskell
type Array# :: forall (v :: Levity). TYPE ('BoxedRep v) -> UnliftedType
```

という風にlifted性について多相にできるようになります（[WIP: Levity polymorphic arrays (!5218) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5218/diffs#8a5cd068459120cddf3814e7b9e02003b87647ba)）。

### `fromInteger :: Integer -> Float`/`Double` の丸め方

（この記事にこれを載せるのは完全に筆者の趣味です。）

これまでのGHCでは

```haskell
import Numeric
import Data.Word
 
main = do
  putStrLn $ "literal : " ++ showHFloat (0xFFFFFFFFFFFFFC00 :: Double) ""
  putStrLn $ "fromInteger : " ++ showHFloat (fromInteger 0xFFFFFFFFFFFFFC00 :: Double) ""
  putStrLn $ "fromRational : " ++ showHFloat (fromRational 0xFFFFFFFFFFFFFC00 :: Double) ""
  putStrLn $ "fromIntegral/Word64 : " ++ showHFloat (fromIntegral (0xFFFFFFFFFFFFFC00 :: Word64) :: Double) ""
```

というコード（`0xFFFFFFFFFFFFFC00` は `Double` で正確に表現できないことに注意）を書いた時に

最適化なしの出力：

```
literal : 0x1p64
fromInteger : 0x1.fffffffffffffp63
fromRational : 0x1p64
fromIntegral/Word64 : 0x1.fffffffffffffp63
```

最適化ありの出力：

```
literal : 0x1p64
fromInteger : 0x1.fffffffffffffp63
fromRational : 0x1p64
fromIntegral/Word64 : 0x1p64
```

という風に

* リテラルと `fromInteger :: Integer -> Double` で結果が食い違う
* `fromInteger :: Integer -> Double` と `fromRational :: Rational -> Double` で結果が食い違う
* `fromIntegral :: Word64 -> Double` の結果が最適化の有無で変わる

という事態になっていました。これは `fromInteger :: Integer -> Double` の丸め方法が一貫していない（入力が多倍長な時に最近接丸めではなく切り捨てを行う）ことによるものでしたが、筆者（mod\_poppo）が「常に最近接丸めを行う」という形で修正しました。

## コード生成

GHC 9.2ではバックエンドについての変化もありました。GHCのバックエンドについては最近記事を書いたので、それも参照してください：

* [GHCのバックエンドについて](https://blog.miz-ar.info/2021/11/backends-of-ghc/)

### AArch64 NCG

昨年はApple Siliconの発表がありましたし、Armの重要度が増しています。これまではArmをターゲットとする場合はLLVM backendを使う必要がありましたが、GHC 9.2ではAArch64（64ビットArm）に対するNative Code Generatorが実装されたので、64ビットArmの場合はLLVMなしでコード生成できるようになりました。32ビットの場合は従来通りLLVMが必要です。

### RISC-V 64 via LLVM 12+

GHC 9.2ではターゲットアーキテクチャーとしてRISC-Vを指定できるようになったようです。現時点では64ビットのみの対応のようです。

筆者はまだ試していないので現段階で使い物になるかはなんとも言えないのですが、スタートラインには立ったということではないかと思います。

# Data.Listの単相化について

何が起こる（かもしれない）のかはスライドを見て頂くとして、ここでは背景についてまとめます。まずはリンクから。

* [Data.List (singleton) - haskell-core-libraries](https://groups.google.com/g/haskell-core-libraries/c/q3zHLmzBa5E/m/OrHHKaJNAQAJ?pli=1)
    * 2019年
* [Data.List specialization (!5304) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5304)
    * 2021年
* [Document Data.List changes in 9.4 (#20025) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/20025)
* [Draft: Revert "Data.List specialization to \[\]" (!6409) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6409)

`Data.List` の単相化は2019年ごろに出てきた話で、一旦GHCのmasterブランチ（GHC 9.4以降に相当）に入りましたが、\#20025で炎上、一旦revertされることになったようです。

動機付けとしては

* 教育面、一貫性（他のコンテナーのモジュールに属する関数はコンテナーについて単相なのに `Data.List` はそうではない）

が主なもののようです。どういう形で決着するのか、今後も注視が必要です。

# まとめ

GHC 9.2には楽しい機能がいろいろ入りました。まだバージョンが `.1` なので安定する（プロダクションで使える）には少しかかるかもしれませんが、楽しそうな雰囲気は感じていただけるかと思います。

GHCの新しいバージョンは新機能だけではなく非互換性ももたらします。新しいバージョンを試したり、 `-Wcompat` を使ったりして備えましょう。
