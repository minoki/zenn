---
title: "GHC 9.14の新機能"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: []
published: false
---

GHC 9.14.1のリリース予定日は未定です。

* [GHC 9.12.1 is now available - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-12-1-is-now-available/11031)

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

<!-- * [2.1. Version 9.12.1 — Glasgow Haskell Compiler 9.12.1 User's Guide](https://downloads.haskell.org/ghc/9.12.1/docs/users_guide/9.12.1-notes.html) -->
<!--    * [docs/users_guide/9.12.1-notes.rst · ghc-9.12 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.12/docs/users_guide/9.12.1-notes.rst) -->
<!-- * [Changelog for base-4.21.0.0 | Hackage](https://hackage.haskell.org/package/base-4.21.0.0/changelog) -->
<!--    * [libraries/base/changelog.md · ghc-9.14 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.14/libraries/base/changelog.md) -->
* [9.14 · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.14)

# GHC 9.14に入る機能

## SPECIALIZEプラグマに式を書けるようになる

## `-Wall` で `-Wincomplete-record-selectors` が有効になる

## ScopedTypeVariablse, TypeApplications, TypeAbstractions周りの変更

## OverloadedRecordUpdateの脱糖方法の変更

## foreign importでのMultilineStringsの許容

JavaScriptで便利

## `coerce` の型推論の変更

## LinearTypes拡張の下でレコードフィールドが非線形になれる

## ExplicitNamespaces拡張でdataを書けるようになる

関連：`pattern` は非推奨になる。`-Wpattern-namespace-specifier`

## カインドにデータ型を使うにはDataKinds拡張が必要になる

## MonadComprehensions拡張がParallelListComp拡張を含意する

## x86 NCGでのSIMDサポートの拡大

## ライブラリー

### `fail` に `HasCallStack` がつく

### `Data.Enum.enumerate` の導入

## その他

LTS
