---
title: "プロジェクトを作らずにHaskellをやる"
emoji: "🏕"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

Haskellで実用的なアプリケーションまたはライブラリーを書くときはCabalやStackでプロジェクトを作ります。しかし、プロジェクトを作るまでもなく何らかの作業をしたい、ということが時にはあるかと思います。

GHCに付属するライブラリーだけで済んでしまう場合は `ghc`, `ghci` や `runghc` を直接使えば良いのですが、もう少し実用的なことをやるにはHackageで公開されたライブラリーを使うことになります。

プロジェクトを作らずにライブラリーを使うにはグローバルな環境にインストールするという方法がありますが、グローバルな環境というのはいずれ管理しきれなくなることが目に見えています。この記事では、なるべくグローバルな環境を汚さずにライブラリーを使う方法を見ていきます。

# 対話環境でライブラリーを使う

まず対話環境についてですが、 `ghci` の代わりに `cabal repl` や `stack repl` を使います。

Cabalの場合は、 `--build-depends` オプションで依存するパッケージを指定します。例：

```sh
cabal repl --build-depends vector
```

Cabalで特定のバージョンのGHCを使いたい場合は、 `-w` オプション（`--with-compiler` の略）を指定します。例：`-w ghc-9.4.3`

Stackの場合はパッケージの指定には `--package` オプションを使います。また、resolverを指定するには `--resolver` オプションを使いますが、コマンドラインで指定する場合は単に `lts` と指定すれば最新のLTSを使ってくれます。

```sh
stack repl --resolver lts --package vector
```

Stackで特定のバージョンのGHCを使いたい場合は、そういうresolverを指定するか、 `--compiler` オプションを指定すると良いでしょう。例：`--compiler ghc-9.4.3`

# 書き捨てスクリプトでライブラリーを使う

単独のファイルからなるプログラムを動かす場合、依存関係を特定の形式のコメントとして書いて `cabal run` コマンドあるいは `stack` コマンドで動かせばライブラリーが利用できます。cabal scriptあるいはstack scriptと呼ばれます。

詳しくは

* [Haskellでちょっとしたスクリプトを書く](haskell-script)

を参照してください。

# ローカルの環境にパッケージをインストールする

複数のファイルからなるプログラムをコンパイルしたい場合はcabal scriptのような「その場その場で依存関係を明示する」方法は使えません。ここでは次善の策として、指定したディレクトリーに環境を作る方法を紹介します。

Cabalでライブラリーを「インストール」するには `--lib` オプション付きで `install` コマンドを使います。ただ、デフォルトだとグローバルな環境にインストールされてしまうので、それを防ぐために `--package-env` オプションで「環境」の場所を指定します。

例：

```sh
cabal install --lib vector --package-env .
```

この例では `--package-env .` でカレントディレクトリーに環境を保存しています。このディレクトリー（またはサブディレクトリー）でGHCを起動すると最初に

```
Loaded package environment from ほにゃらら/.ghc.environment.ほにゃらら
```

というようなメッセージが出力され、インストールしたパッケージが使えるようになります。「環境」の実体は `.ghc.environment.` から始まる名前のファイルです。

詳しくはCabalのマニュアルを見てください：

* [5.2.18.1. Adding libraries to GHC package environments](https://cabal.readthedocs.io/en/3.8/cabal-commands.html#adding-libraries-to-ghc-package-environments)

Stackの場合はプロジェクトの外でパッケージをインストール[^stack-build-library]しようとするとグローバル環境にインストールされてしまい[^stack-global-packages]、ローカルな環境を作る方法は見当たりません。大人しくプロジェクトを作りましょう。

[^stack-build-library]: `stack build <パッケージ名>` でグローバル環境にインストールできます。よく似たコマンドに `stack install` がありますが、これは「`stack build` + `~/.local/bin` に実行ファイルをコピー」の意味で、ライブラリーに使うコマンドではありません。

[^stack-global-packages]: `stack exec -- ghc-pkg list` でグローバルにインストールされたパッケージの一覧を見ることができます。
