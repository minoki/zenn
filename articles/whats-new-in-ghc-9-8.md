---
title: "GHC 9.8の新機能"
emoji: "👌"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: false
---

この記事では、GHC 9.8の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)
* [GHC 9.6の新機能](whats-new-in-ghc-9-6)

です。

この記事は網羅的な紹介記事とはなっていません。是非、公式のリリースノート類も参照してください：

* [2.1. Version 9.8.1 — Glasgow Haskell Compiler 9.8.0.20230727 User's Guide](https://downloads.haskell.org/ghc/9.8.1-alpha1/docs/users_guide/9.8.1-notes.html)
    * [docs/users_guide/9.8.1-notes.rst · ghc-9.8 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8/docs/users_guide/9.8.1-notes.rst)
<!-- * [Changelog for base-4.19.0.0 | Hackage](https://hackage.haskell.org/package/base-4.19.0.0/changelog) -->
* [libraries/base/changelog.md · master · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8/libraries/base/changelog.md)
* [GHC 9.8.x Migration Guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.8)

# GHC 9.8に入る機能

## ExtendedLiterals拡張

* [ghc-proposals/proposals/0451-sized-literals.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0451-sized-literals.rst)

サイズ指定されたプリミティブ型（`Int8#` みたいなやつ）のリテラルを書けるようになります：

```haskell
ghci> :set -XExtendedLiterals
ghci> :m + GHC.Exts
ghci> :t 42#Int8
42#Int8 :: Int8#
```

## TypeAbstractions拡張：型宣言における不可視の束縛

* [ghc-proposals/proposals/0425-decl-invis-binders.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0425-decl-invis-binders.rst)

非互換注意

## `Unsatisfiable` クラス

* [ghc-proposals/proposals/0433-unsatisfiable.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst)

GHCで独自の型エラーを出す方法としては、`TypeError` 型族がありました。しかし、`TypeError` は型族として実装されていることによる使いづらさがありました。詳しくはProposalを読んでください。

次の型クラスと関数が追加されます：

```haskell
module GHC.TypeError where

type Unsatisfiable :: ErrorMessage -> Constraint
class Unsatisfiable msg

unsatisfiable :: Unsatisfiable msg => a
-- 正確には forall {rep} msg (a :: TYPE rep). Unsatisfiable msg => a
```

## 並列ビルドの改善：`-jsem` オプション

* [ghc-proposals/proposals/0540-jsem.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0540-jsem.rst)
* [Reducing Haskell parallel build times using semaphores - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2023/08/reducing-haskell-parallel-build-times/)

依存パッケージが大量にある大きなHaskellプロジェクトのビルド時間が短縮するかもしれません。

これまでは、Haskellプロジェクトをビルドする際の並列化は、

* パッケージ単位の並列化（cabalの `-j` オプション）
* モジュール単位の並列化（GHCの `-j` オプション）

がありました。パッケージ単位の並列化は多数の依存パッケージをビルドする際に有益で、モジュール単位の並列化は多数のモジュールを含むパッケージをビルドする際に有益です。

では、多数のパッケージに依存し、それ自身も多数のモジュールを含むパッケージのビルドでCPUコアを使い切るにはどうしたら良いでしょうか？cabalだけに `-j` を渡すとメインのパッケージのビルドが並列化されません。GHCだけに `-j` を渡すと多数の依存パッケージのビルドが並列化されません。cabalとGHCの両方に `-j` オプションを指定すると、依存パッケージのビルドの際に並列度が過剰になってしまいます。

今回GHCに実装された `-jsem` オプションでは、複数のGHCプロセスが協調してうまく並列度を調整できるようになります。これにはビルドツール側での対応も必要で、cabalは3.12で対応するようです。対応したGHCとcabalでは

```
cabal build -j --semaphore
```

でCPUコアをいい感じに使い切れるようになるでしょう（将来的には `--semaphore` オプションがデフォルトになるかもしれません）。

## 書き換え規則の強化

## Fused multiply-add

* [Fused multiply-add operations](https://downloads.haskell.org/ghc/9.8.1-alpha1/docs/libraries/ghc-prim-0.11.0-3709/GHC-Prim.html#g:17)

最近のCPUは融合積和 (fused multiply-add; FMA) を計算する命令を持つものが多いです。FMAについては前に書いた記事

* [FMA (fused multiply-add) の話 - Qiita](https://qiita.com/mod_poppo/items/e6577df362f44a3ef8dd)

を参照してください。

これまでGHCにFMA命令を出力させる方法はなく、FFIを使うしかありませんでしたが、今回FMA命令に対応するプリミティブが追加されました。追加されたのは以下の8つです：

```haskell
module GHC.Exts where

fmaddFloat# :: Float# -> Float# -> Float# -> Float# -- x * y + z
fmsubFloat# :: Float# -> Float# -> Float# -> Float# -- x * y - z
fnmaddFloat# :: Float# -> Float# -> Float# -> Float# -- - x * y + z
fnmsubFloat# :: Float# -> Float# -> Float# -> Float# -- - x * y - z

fmaddDouble# :: Double# -> Double# -> Double# -> Double# -- x * y + z
fmsubDouble# :: Double# -> Double# -> Double# -> Double# -- x * y - z
fnmaddDouble# :: Double# -> Double# -> Double# -> Double# -- - x * y + z
fnmsubDouble# :: Double# -> Double# -> Double# -> Double# -- - x * y - z
```

注意点として、FMAがサポートされているとは限らないアーキテクチャーではlibcのFMAを呼び出すため、libcのFMAがバグっているプラットフォーム（具体的にはx86系のWindows）では正しい答えが計算できない場合があります。

私の作っているパッケージfp-ieeeでもFMAを提供していますが、GHC 9.8以降でFMAがバグっていない環境であればFMAのプリミティブを使うようにしてみようかと思っています。

## `head` / `tail` に警告が出る

部分関数として悪名高い `head`/`tail` を使った際に警告が出るようになります。

```
ghci> head "foo"

<interactive>:1:1: warning: [GHC-63394] [-Wx-partial]
    In the use of ‘head’
    (imported from Prelude, but defined in Just GHC.List):
    "This is a partial function, it throws an error on empty lists. Use pattern matching or Data.List.uncons instead. Consider refactoring to use Data.List.NonEmpty."
'f'
ghci> tail "foo"

<interactive>:2:1: warning: [GHC-63394] [-Wx-partial]
    In the use of ‘tail’
    (imported from Prelude, but defined in Just GHC.List):
    "This is a partial function, it throws an error on empty lists. Replace it with drop 1, or use pattern matching or Data.List.uncons instead. Consider refactoring to use Data.List.NonEmpty."
"oo"
```

この警告は `-Wno-x-partial` オプションで抑制できます。

個人的にはそこまでする必要ある？と思いますが。

## その他のライブラリーの機能追加

```haskell
Data.List.!? :: [a] -> Int -> Maybe a
Data.List.unsnoc :: [a] -> Maybe ([a], a)
Data.Tuple.getSolo :: Solo a -> a
Data.Functor.unzip :: Functor f => f (a, b) -> (f a, f b)
```

`Data.List.!?` は安全な `!!` です。要素が範囲外の場合は `Nothing` を返します。

`Data.List.unsnoc` は `init` と `last` を同時に計算してくれるやつです。

`Data.Tuple.getSolo` は1要素タプル（`Solo` 型）の要素を取り出すやつです。

`Data.Functor.unzip` は `Data.List.unzip` を一般化したやつです。

## その他

* `-Wterm-variable-capture`: `RequiredTypeArguments` 拡張への地ならし
