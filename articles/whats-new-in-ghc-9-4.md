---
title: "GHC 9.4の新機能"
emoji: "🐈"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

GHC 9.4.1が2022年8月7日にリリースされました（[GHC 9.4.1 released — The Glasgow Haskell Compiler](https://www.haskell.org/ghc/blog/20220807-ghc-9.4.1-released.html)）。

この記事では、GHC 9.4の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)

です。

# GHC 9.4に入る機能

ここでは筆者が独断と偏見で選んだ変更をリストしています。公式の変更リストは

* [2.1. Version 9.4.1 — Glasgow Haskell Compiler 9.4.1 User's Guide](https://downloads.haskell.org/~ghc/9.4.1/docs/users_guide/9.4.1-notes.html)
    * ソース：[docs/users_guide/9.4.1-notes.rst · ghc-9.4 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.4/docs/users_guide/9.4.1-notes.rst)
* [Changelog for base-4.17.0.0 | Hackage](https://hackage.haskell.org/package/base-4.17.0.0/changelog)
    * ソース：[libraries/base/changelog.md · master · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.4/libraries/base/changelog.md)
* Migration Guide [9.4 · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.4)

を参照してください。

リリース管理のやつ：

* [9.4.1 release tracking (#21127) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/21127)
* [9.4.1 · Milestones · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/milestones/370)


## 型の等価性 `~` が普通の型演算子になる

* [Export `~` from `Data.Type.Equality`](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md)

従来のGHCでは `GADTs` 拡張または `TypeFamilies` 拡張を有効にすると型演算子 `~` が使えるようになり、 `a ~ b` という制約で「型 `a` と型 `b` が等しい」ことを表現できました。

GHC 9.4では、 `~` の扱いが次のように変わります：

* 使用に `TypeOperators` 拡張が必要になる（一方で、 `GADTs` や `TypeFamilies` は必要なくなる）
    * 当面の間、 `TypeOperators` なしで `~` を使った場合は警告が出る
* `~` は `Prelude` および `Data.Type.Equality` からエクスポートされるようになる
* （`Prelude` からエクスポートされる定義を隠せば） `~` という名前を持ったユーザー定義の型演算子を定義できるようになる

## `\cases`

* [`\cases` - Multi-way lambda expressions](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst)

`LambdaCase` 拡張の下で `\cases` という構文が使えるようになります。複数の引数にマッチさせることができます。

例えば

```haskell
{-# LANGUAGE LambdaCase #-}

f = \cases
      (Left a) (b,c) -> a + b + c
      (Right a) (b,c) -> a * b + c
```

という感じです。

## `DeepSubsumption` 拡張

* [ghc-proposals/0511-deep-subsumption.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0511-deep-subsumption.rst)

GHC 9.0でsimplified subsumptionという変更が行われました。詳しくは

* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)

を見てください。

simplified subsumptionでは手動でη変換が必要になる場面が増えました。これはコンパイルの簡略化のためですが、手動でのη変換はやっぱり面倒なものです。

`DeepSubsumption` はsimplified subsumption以前の挙動を復活させます。これにより、自動でη変換される場面が増えます。

`Haskell2010` の下では `DeepSubsumption` はデフォルトで有効、 `GHC2021` の下では `DeepSubsumption` はデフォルトで無効です。

`DeepSubsumption` はGHC 9.2.4にもバックポートされています。GHCのマイナーリリースで新しくGHC拡張が実装されるのは異例のことです（GHC 9.2系がLTSということもあるのでしょうけど）。

## `ByteArray` と `MutableByteArray`

* [Data.Primitive.ByteArray.ByteArray + instances in base? (#20044) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/20044)
* [Add Data.ByteArray (!6152) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6152)
* [Rename Data.ByteArray to Data.Array.Byte + add Trustworthy (!6742) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6742)
* [Export MutableByteArray from Data.Array.Byte (!7785) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7785)

GHCにはヒープで管理されたバイト列を表すプリミティブ型として `ByteArray#` 型および `MutableByteArray#` 型があります。これらはunliftedな型で普段使いには辛いので、liftedなラッパーが[primitiveパッケージ](https://hackage.haskell.org/package/primitive)などで提供されていました。

今回、GHC付属のbaseパッケージでliftedなラッパーが提供されることになります。将来、primitiveパッケージが提供する型はbaseパッケージの再エクスポートになるかもしれません。

* [Reexport ByteArray and MutableByteArray from base on newer GHCs · Issue #336 · haskell/primitive](https://github.com/haskell/primitive/issues/336)

## Levity-polymorphic arrays

* [Levity-polymorphic arrays and mutable variables (!7299) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7299)

`GHC.Exts` の `Array#` 型のカインドが

```haskell
type Array# :: Type -> UnliftedType
```

から

```haskell
type Array# :: TYPE ('BoxedRep l) -> UnliftedType
```

へ一般化されました。これによって、 `Array#` の要素として通常の型の他にunliftedな型（サンクを許容しない型）が使えるようになります。

これまでは `ArrayArray#` 型というunliftedな配列の配列みたいな組み込み型がありましたが、それが不要になります。

まあ名前に `#` のつく型なので一般ユーザーにはあまり影響はないかもしれません。

## `ST` が `MonadFail` のインスタンスじゃなくなる

* [Remove MonadFail instances of ST (CLC proposal) (!7501) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7501)

`MonadFail` の趣旨はdo構文中のパターンマッチ失敗時に暗黙に `error` を発生させるのをやめよう、という感じのやつでした。

ですが、 `ST` モナドはこの趣旨に反して `fail = error` な `MonadFail` インスタンスを持っていました。今回、これがなくなります。

`ST` モナドで失敗しうるパターンマッチを使っていた人は明示的にパターンマッチして `error` を呼ぶか、irrefutable patternを使うように書き換える必要があります。

## `magicDict` が `withDict` になる

* [Redesign withDict (formerly magicDict), special-case it in the desugarer (!5573) · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5573)

型クラスのインスタンスを動的に作れると便利なことがあります。型レベル自然数の辞書である `KnownNat` のインスタンスを `Natural` 型の値から作る、というようなやつです。

従来はこれをやるのにGHC組み込みの `magicDict` を使うか、みんな大好き `unsafeCoerce` を使うやり方がありました。`base` では `magicDict` が、`reflection` や `singletons` 等のサードパーティーのライブラリーでは `unsafeCoerce` が好まれていた印象です。しかし、`unsafeCoerce` が遅くなる変更が最近（GHC 9.0）入ったため、サードパーティーも `magicDict` を使うようにしようという機運が高まりました。

ですが、 `magicDict` の型は

```haskell
magicDict :: a
```

という情報量皆無な代物で、正しく使うのは大変でした。GHC 9.4では `magicDict` が

```haskell
withDict :: meth -> (cls => r) -> r
```

に置き換えられることになりました。

## `Int64`/`Word64` の内部表現が変わる

* [Word64/Int64 use Word64#/Int64#](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.4#word64int64-use-word64int64)

`Word64` 型の定義は、従来は

```haskell
-- 32ビット環境
data Word64 = W64# Word64#

-- 64ビット環境
data Word64 = W64# Word#
```

でした。今回、これが

```haskell
data Word64 = W64# Word64#
```

に統一されます。GHC 9.2では `Word8`, `Word16`, `Word32` について似たような変更がありましたね。

`Int64` についても同様です。

## SPARC NCGの削除

前にこんな記事を書きましたが

* [GHCのバックエンドについて](https://blog.miz-ar.info/2021/11/backends-of-ghc/)

SPARC NCGがついに削除されました。

## その他

他にも

* Windows上でClangベースのツールチェインを使う
* `Semigroup`, `Monoid` をderiveするのに便利な `Generically`, `Generically1`
* multiple home packages

などなど新機能がありますが、今回の記事はこの辺にしておきます。
