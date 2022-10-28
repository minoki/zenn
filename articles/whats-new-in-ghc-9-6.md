---
title: "GHC 9.6の新機能"
emoji: "🙆"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: false
---

GHC 9.6.1は2023年の初め頃にリリースされる予定です。

この記事では、GHC 9.6の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)

です。

# GHC 9.6に入る機能

## JavaScriptバックエンド

まだマージされていません。

* [javascript backend · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend)

GHCJSのマージ。

## WebAssemblyバックエンド

まだマージされていません。

* [WebAssembly backend · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-backend)
* [WebAssembly goals · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-goals)

Asteriusのマージ。

`foreign import/export javascript` のないHaskellコードをビルドしたものはJavaScriptのない実行環境でも（WASIを使って）動作できるようにする予定。

## 限定継続のプリミティブ

* [Delimited continuation primops](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst)

```haskell
type PromptTag# (a :: Type)

newPromptTag# :: forall (a :: Type). State# RealWorld -> (State# RealWorld, PromptTag# a)

prompt#
  :: forall (a :: Type)
   . PromptTag# a
  -> (State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, a #)

control0#
  :: forall (a :: Type) (r :: RuntimeRep) (b :: TYPE r)
   . PromptTag# a
  -> (((State# RealWorld -> (# State# RealWorld, b #))
       -> State# RealWorld -> (# State# RealWorld, a #))
      -> State# RealWorld -> (# State# RealWorld, a #))
  -> State# RealWorld -> (# State# RealWorld, b #)
```

## データ構築子を伴わない型・カインド定義： `TypeData` 拡張

* [Define Kinds Without Promotion](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst)
* [Allow defining kinds alone, without a datatype (#6024) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/6024)

```haskell
type data T = MkT
```

## `OverloadedLabels` のラベル名の制限の緩和

* [Unrestricted Overloaded Labels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst)

## ライブラリーの変化

* `Applicative` クラスの `liftA2` が `Prelude` からエクスポートされる
* `GHC.TypeLits`/`GHC.TypeNats` が `natSing`, `symbolSing`, `charSing` をエクスポートする。`SNat`, `SSymbol`, `SChar` 型もエクスポートされる。

## リスト型に紛らわしくない名前を与える：`List` 型

* [Non-punning list and tuple syntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0475-tuple-syntax.rst)
* [Non-punning list and tuple syntax (-XListTuplePuns, -XNoListTuplePuns) (#21294) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/21294)

従来、Haskellのリスト型は `[]` で表記されてきました。一方、リストの項（値）レベルの表記にも `[]` が使われます。

通常は型の表記と項の表記は混ざらないのでこれでよかったのですが、将来のHaskell -- Dependent Haskell -- では項と型の区別がますます曖昧になっていく見込みです。

すでに現在、項と型の表記が混ざる場面があります。 `DataKinds` 拡張です。次のコードを見てみましょう：

```haskell
ghci> :kind []
[] :: Type -> Type
ghci> :kind [Int]
[Int] :: Type
```

これは大丈夫ですよね。リスト型構築子と具体的なリスト型のカインドを表示させただけです。

一方、「要素」の数を3つ以上にしたらどうなるでしょうか：

```haskell
ghci> :kind [Int,Int]
[Int,Int] :: [Type]
ghci> :kind [Int,Int,Int]
[Int,Int,Int] :: [Type]
ghci> :kind [Int,Int,Int,Int]
[Int,Int,Int,Int] :: [Type]
```

なんとエラーにはなりません。これは、項レベルの `[]` を型レベルに昇格させたもの、型レベルリストと判断されたのです。

空の型レベルリストおよび、要素数1の型レベルリストを表記するには、従来は頭に `'` をつけるしかありませんでした：

```haskell
ghci> :kind '[]
'[] :: [a]
ghci> :kind '[Int]
'[Int] :: [Type]
```

一方、項レベルの式の中に型を混在させたい時はどうなるでしょうか。将来のHaskellでは関数の引数に `@` なしで直接型を渡せるようになる（ような言語拡張が追加される）見込みです：

```haskell
sizeOf :: forall a -> Sized a => Int

main = print (sizeOf Int)
```

ここで、`[Int]` （`Int` のリスト）という型を `sizeOf [Int]` と渡そうとすると項の文法が優先されて、1要素の型レベルリストが引数になってしまいます！

まあ項レベルの名前を型で使うことを明示する `'` という表記があるのと同じように型レベルの名前を項で使うことを明示する `(type ...)` という表記が導入される予定なのでそれはいいのですが、そもそも**リスト型とリストリテラルの表記が別であれば**この件に関しては問題は起こらなかったわけです。

というわけで、常にリスト型を表す名前として `List` が導入されます。以下の定義が組み込まれると思って構いません：

```haskell
type List = []
```

タプルについても同じことが言えて、常にタプル型を表す名前として `Unit`, `Tuple<n>` などが導入されます。1要素タプル `Solo` はデータ構築子が `MkSolo` に改名されます。

最終的には `[]` や `()` が型を表さなくなる拡張 `NoListTuplePuns` が導入される予定です。

とりあえずGHC 9.6に `List` 型が入るのは確定です。タプルの名前や `NoListTuplePuns` は執筆段階では未実装です。
