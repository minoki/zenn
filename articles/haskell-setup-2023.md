---
title: "Haskellの環境構築 2023年版"
emoji: "🏗"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: false
---

この記事では、2022年12月時点のHaskellの環境構築手順を紹介します。多分2023年になっても通用します。

対象とする環境は以下の通りです：

* Unix系
    * macOS (Intel / Apple Silicon)
    * Linux (x86\_64 / arm / aarch64)
        * WSL2を含む（WSL1は不具合があった気がするので避けてください）
* Windows (x64)

TODO: 32ビットArmの場合 / 64ビットArmでGHC 9.0以前を使いたい場合は「LLVMバックエンド」のセクションに誘導する

# Stackか、GHCup+Cabalか

Haskellの環境構築をするには、大きく分けて**Stack**と**GHCup+Cabal**の2通りがあります。それぞれ何をするツールなのかと言うと、

* Stack: GHCのインストールおよび、プロジェクトのビルドを行う
* GHCup: GHCや周辺ツール (Cabal/HLS/Stack) のインストールを行う
* Cabal (cabal-install)[^cabal]: プロジェクトのビルドを行う

[^cabal]: 大文字始まりの「Cabal」と書いた場合はHaskellライブラリーとしてのCabalを、「cabal-install」と書いた場合は実行可能コマンドとしての `cabal` を指すという使い分けが存在しますが、この記事では後者を単にCabalと呼ぶことにします。

となります。役割が被っているために対立して語られるわけです。

ですが、後述する設定をすればStackの「GHCのインストール」の部分をGHCupに任せることができます。また、プロジェクトファイルの形式はStackとCabalである程度互換性があります[^project-file]。なので、本稿では **GHCup, Cabal, Stackを全部インストールしてしまえ！** というヨクバリな方針を取ります。

[^project-file]: Cabalでは `.cabal` ファイルが使われ、Stackでは通常 `package.yaml` ファイルが使われますが、Stackでも `.cabal` ファイルを使うことができますし、Hpackというツールで後者から前者へ変換することができます。

インストールはそれでいいとして、実際に開発する際はStackかCabalのどちらかを選ばなくてはいけません。筆者としては、

* でかいアプリケーションを書きたいならStack
* 汎用的なライブラリーを書きたい、あるいは最新のGHCを試したいならCabal

を使うのが良いと考えます。詳しくは後述します。

# GHCupをインストールする

GHCupのインストール方法は、[公式ページ](https://www.haskell.org/ghcup/)を参照してください。

* <https://www.haskell.org/ghcup/>

デフォルトでは

* GHC
* Cabal

がインストールされ、選択次第で以下のツールもインストールできます：

* HLS (Haskell Language Server)
* stack
* （Windowsの場合）msys2
    * msys2はWindows上でUnixライクなツール群（bashなど）を提供するソフトウェアで、ビルド時にconfigureを使うHaskellパッケージのために必要となります。

HLSとstackは後からインストールすることもできるので、この段階でインストールしなくても問題ありません。

2022年11月1日の時点では、デフォルトでGHC 9.2.4とCabal 3.6.2.0がインストールされました。

TODO: そのうち9.2.5が出る。

Unixの場合、GHCupでインストールした諸々のファイルは `~/.ghcup` 以下に保存されます。`ghcup` コマンドは `~/.ghcup/bin/ghcup` という具合です。`~/.ghcup/bin` にPATHを通すようにしておいてください。GHCupを削除したくなった場合は、 `~/.ghcup` を丸ごと削除すればOKです。

Windowsの場合、GHCupがインストールする諸々のファイルは `C:\ghcup` 以下に保存されます。Cabalのディレクトリーは `C:\cabal` となります。環境変数の設定やデスクトップへのショートカットの設置も行われるようです。

セットアップができたら早速 `ghcup` コマンドを使ってみましょう。まず、引数を何も与えずに `ghcup` コマンドを実行するとオプションの一覧みたいなものが出力されます。

```sh
ghcup
```

`ghcup` はコマンド名の直後にサブコマンド名を取るようになっています。例えば、今インストールされているもの・GHCupでインストールできるものを一覧するコマンドは

```sh
ghcup list
```

という具合です。

# GHCの管理

TODO: GHCの簡単な使い方を解説した方が良い？

まず、デフォルトではGHCupのインストール時に「それなりに新しくてしかも安定している」バージョンのGHCがインストールされます。この記事の執筆時点では9.2.4がインストールされました。実際のバージョンは次のコマンドで確認できます：

```sh
ghc --version
```

しかし、時と場合により最新版のGHCを使ったり、あえて古いバージョンのGHCを使ったりしたいことがあります。そんな時のために、GHCupでは複数のGHCを管理できます。

まず、特定のバージョンのGHCをインストールするコマンドは次の形です：

```sh
ghcup install ghc <バージョン>
```

例えば、GHC 9.4.2を入れるには次のコマンドを実行します：

```sh
ghcup install ghc 9.4.2
```

新しくインストールされたGHCのコマンド名は `ghc-<バージョン>` となります。9.4.2であれば

```sh
ghc-9.4.2 --version
```

という具合です。

バージョン番号がつかない `ghc` コマンドが指すものを変更するには、 `ghcup set` コマンドを使います。

```sh
ghcup set ghc <バージョン>
```

例：

```sh
ghcup set ghc 9.4.2
ghc --version  # →The Glorious Glasgow Haskell Compilation System, version 9.4.2
```

GHCの新しいバージョンがリリースされた場合など、古いバージョンが不要になることがあります。その場合は、

```sh
ghcup rm ghc <バージョン>
```

で個別に削除することができます。

# Cabalの管理

Cabalは、Haskellのライブラリーやプログラムをビルドするためのシステムです。

* [The Haskell Cabal | Overview](https://www.haskell.org/cabal/)

GHCupのインストール時にデフォルトでCabalがインストールされるので、普通に使う分にはそれで十分だと思います。

新しいバージョンのCabalがリリースされた時などの管理の方法はGHCと同様で、

```sh
ghcup install cabal <バージョン>
```

でインストール、

```sh
ghcup set cabal <バージョン>
```

で `cabal` コマンドの切り替え、

```sh
ghcup rm cabal <バージョン>
```

で削除ができます。

# Stackを使う

まず、Stackとはどういうものなのか説明します。

StackはHaskellのライブラリーやプログラムをビルドするほか、GHCの管理もできるツールです。ただ、「GHCの管理」の部分は今回はGHCupに譲るというのは既に説明しました。

* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/)

昔はCabalが不便だったのでStackが革新的だったのですが、Cabal側も改良されてStackの相対的なメリットは少なくなってきたように思います。そんな中、今でも言及する価値がある（と筆者が考える）Stackの特徴は以下の通りです：

* 安定したパッケージの集合である[Stackage](https://www.stackage.org/)の利用が容易である
* YAMLを使って簡潔にプロジェクトファイルを記述できる (`package.yaml`)

もう少し詳しく説明します。まず、Stackageについて。

HaskellのパッケージはHackageというサイトにアップロードされますが、個々のパッケージの作者が好きなタイミングで最新版をアップロードすると、パッケージ間の食い合わせが悪くて動作しない場合があります。そこで、Stackageという「主要パッケージの、動作確認されたバージョンの組み合わせ」を集めるデータベースができました。この「組み合わせ」のことをresolverと呼び、安定したバージョンであるLTS (long term support)系列と、より新しいが不安定なnightly系列があります。

Stackageのresolverは主要パッケージのバージョンだけでなく、GHCのバージョンも指定します。`lts-19.31` ならばGHC 9.0.2という具合です。

このStackageの利用が容易であるというのがStackの特徴の一つです[^cabal-stackage]。というか、Stackを使って開発をする際は常に何らかのresolverが使われます。

[^cabal-stackage]: CabalからStackageを利用することもできます。

もう一つの特徴である、YAMLを使ったプロジェクトファイルの記述について。

Cabalでは拡張子 `.cabal` を持つ独自の形式でプロジェクトの依存関係やモジュール一覧などを記述します。昔は `.cabal` 形式が不便だったので、HpackというYAML (`package.yaml`) でプロジェクトを記述できるツールが登場しました。

HpackをCabalで使うには手動で `hpack` コマンドを使って `.cabal` ファイルを生成する必要がありますが、StackはこのHpackを組み込んでいるので、 `stack` の各コマンドを実行するだけで `package.yaml` の内容が反映されます。

なお、Stackでも `.cabal` ファイルを利用できます。また、最近のCabal形式は便利になってきたのでHpackの優位性は昔ほどではないかもしれません。

逆に、Stackが不利な状況も挙げておきます。Stackはresolverを固定する状況（アプリケーションを書く場合など）には強い一方で、複数のGHCでテストしたい状況（汎用的なライブラリーなど）には不便です。また、最新のGHCへの対応が遅れる傾向にあるため、新しい物好きの人は直接GHC+Cabalを使った方が良いかもしれません。

StackとCabalの比較についてさらに興味のある人のための資料を紹介しておきます：

* [最近のstackとcabalについて簡単に](https://the.igreque.info/slides/2019-11-29-stack-cabal.html)
* [cabal コマンドとの対応表](https://haskell.e-bigmoon.com/stack/tips/cabal.html)

## GHCupでStackをインストールする

GHCupでStackをインストールするには次のコマンドを実行します：

```sh
ghcup install stack
```

Stack自身のインストーラーを使った場合は `stack` コマンドは `/usr/local/bin/stack` （Unixの場合）にインストールされますが、GHCupを使ってインストールした場合は `~/.ghcup/bin/stack` （Unixの場合）または `C:\ghcup\bin\stack.exe` （Windowsの場合）にインストールされます。

GHCupを使ってインストールした場合、 `stack upgrade` は使えないので注意してください。Stackのアップグレードにも `ghcup` コマンドを使うことになります。

Stack関連のファイル（`stack` コマンド自身を除く）は `~/.stack/` （Unixの場合）または `%APPDATA%\stack\` （Windowsの場合）に保存されます。Stackを削除したくなった場合は、 `~/.stack` を丸ごと削除すればOKです。

このままでは、Stackを使おうとするとGHCupとは独立にGHCがインストールされてしまいます。ディスクが余っている場合はそれでも構わないのですが、筆者のようにSSDがカツカツな場合は困るので、次の設定を行います。

## GHCupでインストールしたGHCをStackから使う

StackのデフォルトではシステムのGHCとは独立に、 `~/.stack/programs/` （Unixの場合）または `%LOCALAPPDATA%\Programs\stack\` （Windowsの場合）以下にGHCがインストールされます。ですが、すでにGHCupでGHCをインストールした場合はこれは無駄なので、StackでもシステムのGHCを使う設定を行います。**大容量のストレージを使っていて重複が気にならない場合はこのセクションは飛ばして構いません。**

次のコマンドを実行します：

```sh
stack config set install-ghc false --global
stack config set system-ghc true --global
```

または `~/.stack/config.yaml` （Unixの場合）／ `%APPDATA%\stack\config.yaml` （Windowsの場合）に

```yaml
install-ghc: false
system-ghc: true
```

と書き込みます。

これで、GHCupでインストールしたGHCをStackが使うようになります。

注意点として、素のStackにあった「未インストールのバージョンのGHCを要求されたらその場でインストールする」という機能が `install-ghc: false` で無効になります。未インストールのGHCが必要になったら手動でGHCupを実行しましょう。

# Haskell Language Serverを使う

TODO

# Haskell製ライブラリーをインストールする

Haskellで実用的なことをやろうとするとHackageで公開されたライブラリーを使うことになります。bytestring, text, vectorなどなど。

そういうライブラリーを使うには、まずインストール……**しません**。

Haskellプロジェクトからライブラリーを使いたい場合は、プロジェクトファイルに使用するライブラリーを明示します。するとビルド時に依存関係が自動で使えるようになります。なので、「依存関係のインストール」という手順は必要ありません。

次に、対話環境や書き捨てスクリプトでライブラリーを使いたい場合です。この場合、グローバルな環境にインストールするという手がありますが、筆者的にはお勧めできません。グローバルな状態というのは、いずれ管理しきれなくなることが目に見えているからです。

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

TODO: 執筆時点ではstylish-haskellが素直にインストールできない。別の例を使うべきか？

Cabalでツールをインストールするには、 `cabal install <パッケージ名>` を実行します。例：

```sh
cabal install stylish-haskell
```

Cabalで入れた実行コマンドは、デフォルトでは `~/.cabal/bin/` （Unixの場合）または `C:\cabal\bin\` （Windowsの場合）にインストールされます。適宜PATHを通しましょう。

* [5.2.14. cabal install](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-install)

TODO: 特定のバージョンをインストールするやり方、`--program-suffix`

Stackでツールをインストールすることもできます。そのためには `stack install <パッケージ名>` を実行します。例：

```sh
stack install stylish-haskell
```

Stackで入れた実行コマンドは、デフォルトでは `~/.local/bin/` （Unixの場合）または `%APPDATA%\local\bin\` （Windowsの場合）にインストールされます。適宜PATHを通しましょう。実際のパスは `stack path --local-bin` で確認できます。

* [The stack install command and copy-bins option](https://docs.haskellstack.org/en/stable/GUIDE/#the-stack-install-command-and-copy-bins-option)

# おまけ：LLVMバックエンドを使う

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

Homebrewの場合は `/opt/homebrew/opt/llvm@12/bin` （Apple Silicon Macの場合）または `/usr/local/opt/llvm@12/bin` （Intel Macの場合）以下に `opt` コマンドと `llc` コマンドが存在するので、次のように `ghcup` を実行すれば良いでしょう：

```sh
# Apple Silicon Macの場合
OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 9.0.2 --force

# Intel Macの場合
OPT=/usr/local/opt/llvm@12/bin/opt LLC=/usr/local/opt/llvm@12/bin/llc ghcup install ghc 9.0.2 --force
```

MacPortsの場合は `opt` コマンドと `llc` コマンドの名前に `-mp-` が含まれているので、次のように `ghcup` を実行すれば良いでしょう：

```sh
OPT=/opt/local/bin/opt-mp-12 LLC=/opt/local/bin/llc-mp-12 ghcup install ghc 9.0.2 --force
```

ちなみに、ここで設定した `OPT` と `LLC` の値は `~/.ghcup/ghc/9.0.2/lib/ghc-9.0.2/settings` というファイルに記録されます。

LLVMバックエンドを実際に使うにはGHCオプションとして `-fllvm` を指定します。

# おまけ：アルファ版・ベータ版のGHCを使う

* [(Pre-)Release channels](https://www.haskell.org/ghcup/guide/#pre-release-channels)
* [Installing custom bindists](https://www.haskell.org/ghcup/guide/#installing-custom-bindists)
