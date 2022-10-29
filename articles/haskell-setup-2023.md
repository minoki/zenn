---
title: "Haskellの環境構築 2023年版"
emoji: "💨"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: false
---

# Stackか、GHCup+Cabalか、それが問題だ

Haskellの環境構築をするには、大きく分けて**Stack**と**GHCup+Cabal**の2通りがあります。それぞれ何をするツールなのかと言うと、

* Stack: GHCのインストールおよび、プロジェクトのビルドを行う
* GHCup: GHCや周辺ツール (Cabal/HLS/Stack) のインストールを行う
* Cabal (cabal-install)[^cabal]: プロジェクトのビルドを行う

[^cabal]: 大文字始まりの「Cabal」と書いた場合はHaskellライブラリーとしてのCabalを、「cabal-install」と書いた場合は実行可能コマンドとしての `cabal` を指すという使い分けが存在しますが、この記事では後者を単にCabalと呼ぶことにします。

となります。役割が被っているために対立して語られるわけです。

ですが、後述する設定をすればStackの「GHCのインストール」の部分をGHCupに任せることができます。また、プロジェクトファイルの形式はStackとCabalである程度互換性があります[^project-file]。なので、本稿では **GHCup, Cabal, Stackを全部インストールしてしまえ！** というヨクバリな方針を取ります。

[^project-file]: Cabalでは `.cabal` ファイルが使われ、Stackでは通常 `package.yaml` ファイルが使われますが、Stackでも `.cabal` ファイルを使うことができますし、Hpackというツールで後者から前者へ変換することができます。

インストールはそれでいいとして、実際にプロジェクトを作って開発する際はStackかCabalのどちらかを選ばなくてはいけません。筆者としては、

* でかいアプリケーションを書きたいならStack
* 汎用的なライブラリーを書きたい、あるいは最新のGHCを試したいならCabal

を使うのが良いと考えます。

# GHCupをインストールする

GHCupのインストール方法は、[公式ページ](https://www.haskell.org/ghcup/)を参照してください。

* <https://www.haskell.org/ghcup/>

GHCupでインストールした諸々のファイルは `~/.ghcup` 以下に保存されます。`ghcup` コマンドは `~/.ghcup/bin/ghcup` という具合です。

`~/.ghcup/bin` にPATHを通すようにしておいてください。

GHCupを削除したくなった場合は、 `~/.ghcup` を丸ごと削除すればOKです。

TODO: Windowsの場合

TODO: デフォルトで何がインストールされるんだっけ？

# GHCupでStackをインストールする

GHCupでStackをインストールするには次のコマンドを実行します：

```sh
ghcup install stack
```

Stack自身のインストーラーを使った場合は `stack` コマンドは `/usr/local/bin/stack` にインストールされますが、GHCupを使ってインストールした場合は `~/.ghcup/bin/stack` にインストールされます。

GHCupを使ってインストールした場合、 `stack upgrade` は使えないので注意してください。Stackのアップグレードにも `ghcup` コマンドを使うことになります。

Stack関連のファイル（`stack` コマンド自身を除く）は `~/.stack` に保存されます。Stackを削除したくなった場合は、 `~/.stack` を丸ごと削除すればOKです。

このままでは、Stackを使おうとするとGHCupとは独立にGHCがインストールされてしまいます。ディスクが余っている場合はそれでも構わないのですが、筆者のようにSSDがカツカツな場合は困るので、次の設定を行います。

# GHCupでインストールしたGHCをStackから使う

StackのデフォルトではシステムのGHCとは独立に、 `~/.stack/programs/` 以下にGHCがインストールされます。ですが、すでにGHCupでGHCをインストールした場合はこれは無駄なので、StackでもシステムのGHCを使う設定を行います。**大容量のストレージを使っていて重複が気にならない場合はこのセクションは飛ばしてください。**

次のコマンドを実行します：

```sh
stack config set install-ghc false --global
stack config set system-ghc true --global
```

または `~/.stack/config.yaml` に

```yaml
install-ghc: false
system-ghc: true
```

と書き込みます。

これで、GHCupでインストールしたGHCをStackが使うようになります。

注意点として、素のStackにあった「未インストールのバージョンのGHCを要求されたらその場でインストールする」という機能が `install-ghc: false` で無効になります。未インストールのGHCが必要になったら手動でGHCupを実行しましょう。

# Haskell Language Serverを使う

TODO

# LLVMバックエンドを使う

GHCはネイティブコードを生成できるコンパイラーですが、ネイティブコードの生成方法としてGHC自身のNCG (native code generator) バックエンドとLLVMバックエンドの2種類を持っています。

普通はNCGで良いのですが、「GHC 9.0またはそれ以前で64ビットArm向けコードを生成したい」「NCGとLLVMの最適化を比較したい」「SIMDなど、LLVMバックエンドでしか対応してない機能を使いたい」などの場合はLLVMバックエンドも使えるようにしておく必要があります。**LLVMバックエンドを使わない場合はこのセクションは飛ばしてください。**

まず、システムのパッケージマネージャーでLLVMをインストールします。GHCごとに対応するLLVMのバージョンが決まっているので、むやみに新しいLLVMを入れればいいというものではありません。具体的には、対応関係は次のようになります：

| GHC | LLVM |
|-|-|
| GHC 8.10.7 | LLVM 9以上12以下 |
| GHC 9.0.2 | LLVM 9以上12以下 |
| GHC 9.2 | LLVM 9以上12以下 |
| GHC 9.4 | LLVM 10以上13以下 |

ここではGHC 9.0.2 / LLVM 12を例とします。

Macユーザーの方は、Homebrewの場合は `brew install llvm@12` を、MacPortsの場合は `sudo port install llvm-12` を実行してLLVMをインストールします。

Ubuntuユーザーの場合は `sudo apt install llvm-12` とすれば良いでしょう。

GHCにとって重要なのは、インストール時にLLVMの `opt` コマンドと `llc` コマンドが見えることです。Ubuntuのように `opt-12`, `llc-12` コマンドにPATHが通っていれば自動検出されます。

```sh
sudo apt install llvm-12
ghcup install ghc 9.0.2 --force  # --forceにより、GHC 9.0.2がインストール済みであっても再インストールする
```

HomebrewやMacPortsの場合は一工夫必要です。GHCのインストール時に `opt` コマンドと `llc` コマンドの場所を教えてやる必要があります。具体的には、環境変数 `OPT` と `LLC` を設定します。

Homebrewの場合は `/opt/homebrew/opt/llvm@12/bin` 以下に `opt` コマンドと `llc` コマンドが存在するので、次のように `ghcup` を実行すれば良いでしょう：

```sh
OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 9.0.2 --force
```

MacPortsの場合は `opt` コマンドと `llc` コマンドの名前に `-mp-` が含まれているので、次のように `ghcup` を実行すれば良いでしょう：

```sh
OPT=/opt/local/bin/opt-mp-12 LLC=/opt/local/bin/llc-mp-12 ghcup install ghc 9.0.2 --force
```

ちなみに、ここで設定した `OPT` と `LLC` の値は `~/.ghcup/ghc/9.0.2/lib/ghc-9.0.2/settings` というファイルに記録されます。

LLVMバックエンドを実際に使うにはGHCオプションとして `-fllvm` を指定します。

# Haskell製ライブラリーをインストールする

Haskellで実用的なことをやろうとするとHackageで公開されたライブラリーを使うことになります。bytestring, text, vectorなどなど。

そういうライブラリーを使うには、まずインストール……**しません**。

まず、Haskellプロジェクトからライブラリーを使いたい場合は、プロジェクトファイルに使用するライブラリーを明示します。するとビルド時に依存関係が自動で使えるようになります。なので、「依存関係のインストール」という手順は必要ありません。

次に、対話環境や書き捨てスクリプトでライブラリーを使いたい場合です。この場合、グローバルな環境にインストールするという手がありますが、筆者的にはお勧めできません。グローバルな状態というのは、いずれ管理しきれなくなることが見えすいているからです。

ここでは、その場その場で依存ライブラリーを明示するやり方を紹介します。

## 対話環境でライブラリーを使う

まず対話環境についてですが、 `ghci` の代わりに `cabal repl` や `stack repl` を使います。

Cabalの場合は、 `--build-depends` オプションで依存するパッケージを指定します。例：

```sh
cabal repl --build-depends vector
```

Cabalで特定のバージョンのGHCを使いたい場合は、 `-w` オプションを指定すると良いでしょう。例：`-w ghc-9.2.4`

Stackの場合はパッケージの指定には `--package` オプションを使います。

```sh
stack repl --package vector
```

Stackで特定のバージョンのGHCを使いたい場合は、 `--resolver` や `--with-ghc` などのオプションも指定すると良いでしょう。例：`--with-ghc ghc-9.2.4`

## 書き捨てスクリプトでライブラリーを使う

TODO: cabal script / stack script

[Haskellでちょっとしたスクリプトを書く](https://zenn.dev/mod_poppo/scraps/e2891dbebb235d)

# Haskell製ツールをインストールする

HackageではHaskellで書かれたツールも色々公開されています。CabalやStackを使うとそれらを簡単にインストールすることができます。

ツールの例：Alex, Happy, Hpack, stylish-haskell, Pandoc

Cabalでツールをインストールするには、 `cabal install <パッケージ名>` を実行します。例：

```sh
cabal install stylish-haskell
```

Cabalで入れた実行コマンドは、デフォルトでは `~/.cabal/bin` にインストールされます。適宜PATHを通しましょう。

* [5.2.14. cabal install](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-install)

TODO: 特定のバージョンをインストールするやり方、`--program-suffix`

Stackでツールをインストールすることもできます。そのためには `stack install <パッケージ名>` を実行します。例：

```sh
stack install stylish-haskell
```

Stackで入れた実行コマンドは、デフォルトでは `~/.local/bin` にインストールされます（Unixの場合）。適宜PATHを通しましょう。実際のパスは `stack path --local-bin` で確認できます。

* [The stack install command and copy-bins option](https://docs.haskellstack.org/en/stable/GUIDE/#the-stack-install-command-and-copy-bins-option)
