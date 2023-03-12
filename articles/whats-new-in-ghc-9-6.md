---
title: "GHC 9.6の新機能"
emoji: "🙆"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

GHC 9.6.1が2023年3月12日にリリースされました。

* [GHC 9.6.1 is now available - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-6-1-is-now-available/5972)

この記事では、GHC 9.6の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)

です。

この記事は網羅的な紹介記事とはなっていません。是非公式のリリースノート類も参照してください：

* [2.1. Version 9.6.1 — Glasgow Haskell Compiler 9.6.1 User's Guide](https://downloads.haskell.org/ghc/9.6.1/docs/users_guide/9.6.1-notes.html)
* [Changelog for base-4.18.0.0 | Hackage](https://hackage.haskell.org/package/base-4.18.0.0/changelog)
* [GHC 9.6.x Migration Guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.6)

# GHC 9.6に入る機能

## JavaScriptバックエンド

* [javascript backend · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend)
* [JavaScript backend merged into GHC | IOG Engineering](https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/)

GHCにJavaScriptバックエンドを追加します。実質的にGHCJSのマージです。

現段階ではTemplate Haskellやforeign exportなど、色々欠けているようです。今後に期待しましょう。

GHC 9.6の段階ではGHCは「実行時のオプションでターゲットを切り替えられる」ようにはなっていないので、普通のGHCではなく、JSをターゲットとするクロスコンパイラーとしてビルドされたGHCが必要になります。ビルド手順は

* [building · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/javascript-backend/building)

あたりを参考にすると良いでしょう。

手前味噌ですが、私が書いたDockerfileを以下で公開しています：

* [minoki/ghc-docker: Dockerfile(s) for GHC](https://github.com/minoki/ghc-docker)

## WebAssemblyバックエンド

* [WebAssembly backend · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-backend)
* [WebAssembly goals · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-goals)
* [WebAssembly backend merged into GHC - Tweag](https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/)

GHCにWebAssemblyバックエンドを追加します。実質的にAsteriusのマージです。

WebAssemblyといえばWebですが、GHC 9.6の段階ではWASI専用のようです。将来的には `foreign import/export javascript` でJavaScriptとやりとりできるようになるでしょう。

GHC 9.6の段階ではGHCは「実行時のオプションでターゲットを切り替えられる」ようにはなっていないので、普通のGHCではなく、wasmをターゲットとするクロスコンパイラーとしてビルドされたGHCが必要になります。

手元で試したい方は、以下にNixでバイナリーを入れる手順やビルド手順が載っています：

* [Glasgow Haskell Compiler / ghc-wasm-meta · GitLab](https://gitlab.haskell.org/ghc/ghc-wasm-meta)

私もDockerfileを用意するつもりです。

HaskellのDiscourseにGHC WebAssembly Weekly Updateが投稿されています：

* [Haskell Community - Haskell](https://discourse.haskell.org/)

## 限定継続のプリミティブ

* [Delimited continuation primops](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst)

限定継続については浅井先生の

* [shift/reset プログラミング入門](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf)

や私の記事

* [限定継続いろいろ | 雑記帳](https://blog.miz-ar.info/2022/10/delimited-continuations/)

を見てください。

以下の型と関数が `GHC.Exts` モジュールから使えるようになります：

```haskell
type PromptTag# :: Type -> UnliftedType

newPromptTag# :: forall (a :: Type). State# RealWorld -> (# State# RealWorld, PromptTag# a #)

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

型が分かりにくいですが、 `State# RealWorld -> (# State# RealWorld, a #)` を `IO a` と思って読み替えると

```haskell
newPromptTag :: IO (PromptTag a)
prompt :: PromptTag a -> IO a -> IO a
control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
```

となります。ただ、既存の `withFile` 等のリソース管理と限定継続の兼ね合いが微妙なので `IO` でラップしたものは標準ライブラリーからは提供されません。

限定継続の操作は副作用なので、Haskellで使うには何らかのモナドを使う必要があります。したがって、他の言語での継続のサンプルコードを移植すると書き心地が悪く感じるかもしれません。

限定継続プリミティブを利用した拡張可能エフェクトのライブラリーが開発中のようです（最終更新が2020年なのでちょっと怪しいですが）：

* [hasura/eff: 🚧 a work in progress effect system for Haskell 🚧](https://github.com/hasura/eff)

限定継続を使ってalgebraic effectをやる記事も出ています：

* [Lysxia - From delimited continuations to algebraic effects in Haskell](https://blog.poisson.chat/posts/2023-01-02-del-cont-examples.html)

## Haskell Error Indexへの対応

* [The Haskell Error Index — Haskell Error Index](https://errors.haskell.org/)
* [Announcing the Haskell Error Index - Haskell Foundation - Haskell Community](https://discourse.haskell.org/t/announcing-the-haskell-error-index/5195)

GHCや周辺ツールがエラーメッセージに `[GHC-12345]` のようなコードを割り振って、オンラインで関連情報を参照できるようにしようという取り組みが始まりました。というわけで、GHC 9.6からエラーに番号がつくようになります。

Rustの同様の仕組みにインスパイアされたらしいです。

## データ構築子を伴わない型・カインド定義： `TypeData` 拡張

* [Define Kinds Without Promotion](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0106-type-data.rst)
* [Allow defining kinds alone, without a datatype (#6024) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/6024)

`DataKind` 拡張を使うと、型をカインドに、データ構築子を型レベルに持ち上げることができます。むしろ、データ型は要らないけど独自のカインドと型を定義するために `data` 宣言を使う、という場面があります。

`TypeData` 拡張を使うと、型・データ構築子を定義することなくカインドと型を定義することができるようになります。構文はこんな感じです：

```haskell
type data T = MkT
```

例：

```haskell
ghci> :set -XTypeData
ghci> type data T = MkT
ghci> :type MkT

<interactive>:1:1: error: [GHC-31891]
    • Illegal term-level use of the type constructor or class ‘MkT’
    • defined at <interactive>:2:15
    • In the expression: MkT
ghci> :kind MkT
MkT :: T
```

## `OverloadedLabels` のラベル名の制限の緩和

* [Unrestricted Overloaded Labels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst)

これまでは `#` の後に識別子が来る必要がありましたが、これからは `#3` とか `#"Foo"` とか色々書けるようになります。

## CApiFFIのための `ConstPtr` 型

* [#22043: CApiFFI does not account for const qualifier · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22043)
* [Foreign.C.ConstPtr](https://hackage.haskell.org/package/base-4.18.0.0/docs/Foreign-C-ConstPtr.html)

`capi` 擬似呼び出し規約を使うと、CのマクロやAArch64 Darwinの可変長引数関数など、普通のC FFIが対応していない関数っぽいものを呼び出すことができます。これは仲介役となるCコードをGHCが出力し、Cコンパイラーにコンパイルさせることで実現されています。

この際、Cの型注釈はHaskellの型注釈を基に決定されますが、`const` 修飾されたポインターはHaskell側で記述できないためCの型が合わないという問題がありました。

この問題を解決するために、今回、`const` 修飾されたポインターを表す `ConstPtr` 型が追加されました。

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

一方、項レベルの式の中に型を混在させたい時はどうなるでしょうか。将来のHaskellでは関数の引数に `@` なしで直接型を渡せるようになる（ような[言語拡張 `RequiredTypeArguments` が追加される](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)）見込みです：

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

sizeOf :: forall a -> Sized a => Int

main = print (sizeOf Int)
```

ここで、`[Int]` （`Int` のリスト）という型を `sizeOf [Int]` と渡そうとすると項の文法が優先されて、1要素の型レベルリストが引数になってしまいます！

まあ項レベルの名前を型で使うことを明示する `'` という表記があるのと同じように型レベルの名前を項で使うことを明示する `(type ...)` という表記が導入される予定なのでそれはいいのですが、そもそも**リスト型とリストリテラルの表記が別であれば**この件に関しては問題は起こらなかったわけです。

というわけで、常にリスト型を表す名前として `List` が導入されます。以下の定義が組み込まれると思って構いません：

```haskell
module GHC.Exts where

-- 本当は型エイリアスではない（TypeSynonymInstances/FlexibleInstancesなしでもインスタンスを定義できる）
type List = []
```

タプルについても同じことが言えて、常にタプル型を表す名前として `Unit`, `Tuple<n>` などが導入される予定です。1要素タプル `Solo` はデータ構築子が `MkSolo` に改名されます。

最終的には `[]` や `()` が型を表さなくなる拡張 `NoListTuplePuns` が導入される計画です。

とりあえずGHC 9.6には `List` 型が入り、 `Solo :: a -> Solo a` が `MkSolo` に改名されます。タプルの名前や `NoListTuplePuns` はもう少し先になりそうです。

## ライブラリーのその他の変化

* `Applicative` クラスの `liftA2` が `Prelude` からエクスポートされる
* `Data.Char.isUpperCase`/`isLowerCase`
    * 既存のものとはUnicode的な性質がちょっと違うらしいです。
* `GHC.TypeLits`/`GHC.TypeNats` の `Known` 系の型クラスから `natSing`, `symbolSing`, `charSing` をエクスポートする。`SNat`, `SSymbol`, `SChar` 型もエクスポートされる。
    * [Expose `KnownSymbol`'s method and `SSymbol` · Issue #85 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/85)
    * [Export symbolSing, SSymbol, and friends (CLC#85) (!9064) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9064)
    * `withDict` をこれらの型クラスに対して使えるようにする関係です。

## ビルドシステムがHadrianのみになる

従来はGHCをビルドする方法は二つありました：

* 伝統的なmake
* Haskell/Shakeで書かれたHadrian

ですが今回、伝統的なmakeベースのビルドシステムは削除されました。

Hadrianを使ってビルドするには

```sh
$ ./boot # Gitから取ってきたソースをビルドする場合
$ ./configure
$ hadrian/build -j
```

みたいな感じのコマンドを使います。詳しくはhadrian/README.mdとか `hadrian/build --help` を見てください。
