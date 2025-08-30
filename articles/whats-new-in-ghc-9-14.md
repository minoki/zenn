---
title: "GHC 9.14の新機能"
emoji: "😎"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
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

## `-Wall` で `-Wincomplete-record-selectors` が有効になる

## ScopedTypeVariables, TypeApplications, TypeAbstractions周りの変更

## OverloadedRecordUpdateの脱糖方法の変更

* [HasField redesign - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0583-hasfield-redesign.rst)

## foreign importでのMultilineStringsの許容

JavaScriptで便利

## `coerce` の型推論の変更

## LinearTypes拡張の下でレコードフィールドが非線形になれる

## ExplicitNamespaces拡張でdataを書けるようになる

* [Namespace-specified imports - ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0581-namespace-specified-imports.rst)

関連：`pattern` は非推奨になる。`-Wpattern-namespace-specifier`

## カインドにデータ型を使うにはDataKinds拡張が必要になる

## MonadComprehensions拡張がParallelListComp拡張を含意する

## x86 NCGでのSIMDサポートの拡大

* [#25487: x86 NCG SIMD: Implement pack/insert/broadcast/unpack for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25487)
* [#25643: x86 NCG SIMD: Implement arithmetic operations for integer vectors · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25643)
* [#26096: Better lowering for shuffleFloatX4# and shuffleDoubleX2# · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/26096)

128ビット整数、shuffleの改良

## GHCi multiple home units

## ライブラリー

### `fail` に `HasCallStack` がつく

### `Data.Enum.enumerate` の導入

* [enumerate Function · Issue #306 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/306)

## その他
