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
* Cabal (cabal-install): プロジェクトのビルドを行う

となります。役割が被っているために対立して語られるわけです。

ですが、後述する設定をすればStackの「GHCのインストール」の部分をGHCupに任せることができます。また、プロジェクトファイルの形式はStackとCabalである程度互換性があります[^project-file]。なので、本稿では **GHCup, Cabal, Stackを全部インストールしてしまえ！** というヨクバリな方針を取ります。

[^project-file]: Cabalでは `.cabal` ファイルが使われ、Stackでは通常 `package.yaml` ファイルが使われますが、Stackでも `.cabal` ファイルを使うことができますし、Hpackというツールで後者から前者へ変換することができます。

インストールはそれでいいとして、プロジェクトごとにStackかCabalのどちらを使うか決めなければいけません。筆者としては、

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

# GHCupでStackをインストールする

GHCupでStackをインストールするには次のコマンドを実行します：

```
ghcup install stack
```

Stack自身のインストーラーを使った場合は `stack` コマンドは `/usr/local/bin/stack` にインストールされますが、GHCupを使ってインストールした場合は `~/.ghcup/bin/stack` にインストールされます。

GHCupを使ってインストールした場合、 `stack upgrade` は使えないので注意してください。Stackのアップグレードにも `ghcup` コマンドを使うことになります。

Stack関連のファイル（`stack` コマンド自身を除く）は `~/.stack` に保存されます。Stackを削除したくなった場合は、 `~/.stack` を丸ごと削除すればOKです。

このままでは、Stackを使おうとするとGHCupとは独立にGHCがインストールされてしまいます。ディスクが余っている場合はそれでも構わないのですが、筆者のようにSSDがカツカツな場合は困るので、次の設定を行います。

# GHCupでインストールしたGHCをStackから使う

次のコマンドを実行します：

```
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

注意点として、素のStackにあった「未インストールのGHCを要求されたらその場でインストールする」という機能が `install-ghc: false` で無効になります。GHCが必要になったら手動でGHCupを実行しましょう。

# Haskell Language Serverを使う

TODO

# LLVMバックエンドを使う

GHCはネイティブコードを生成できるコンパイラーですが、ネイティブコードの生成方法としてGHC自身のNCG (native code generator) バックエンドとLLVMバックエンドの2種類を持っています。

普通はNCGで良いのですが、「GHC 9.0またはそれ以前で64ビットArm向けコードを生成したい」「NCGとLLVMの最適化を比較したい」「SIMDなど、LLVMバックエンドでしか対応してない機能を使いたい」などの場合はLLVMバックエンドも使えるようにしておく必要があります。

まず、システムのパッケージマネージャーでLLVMをインストールします。GHCごとに対応するLLVMのバージョンが決まっているので、むやみに新しいLLVMを入れればいいというものではありません。具体的には、対応関係は次のようになります：

| GHC | LLVM |
|-|-|
| GHC 8.10.7 | LLVM 9以上12以下 |
| GHC 9.0.2 | LLVM 9以上12以下 |
| GHC 9.2 | LLVM 9以上12以下 |
| GHC 9.4 | LLVM 10以上13以下 |

ここではGHC 9.0.2 / LLVM 12を例とします。

Macユーザーの方は、Homebrewの場合は `brew install llvm@12` を、MacPortsの場合は `sudo port install llvm-12` を実行してLLVMをインストールします。

Ubuntuユーザーの場合は `apt install llvm-12` とすれば良いでしょう。

GHCにとって重要なのは、インストール時にLLVMの `opt` コマンドと `llc` コマンドが見えることです。Ubuntuのように `opt-12`, `llc-12` コマンドにPATHが通っていれば自動検出されます。

HomebrewやMacPortsの場合は一工夫必要です。GHCのインストール時に `opt` コマンドと `llc` コマンドの場所を教えてやる必要があります。具体的には、環境変数 `OPT` と `LLC` を設定します。

Homebrewの場合は `/opt/homebrew/opt/llvm@12/bin` 以下に `opt` コマンドと `llc` コマンドが存在するので、次のように `ghcup` を実行すれば良いでしょう：

```
OPT=/opt/homebrew/opt/llvm@12/bin/opt LLC=/opt/homebrew/opt/llvm@12/bin/llc ghcup install ghc 9.0.2 --force
```

MacPortsの場合は `opt` コマンドと `llc` コマンドの名前に `-mp-` が含まれているので、次のように `ghcup` を実行すれば良いでしょう：

```
OPT=/opt/local/bin/opt-mp-12 LLC=/opt/local/bin/llc-mp-12 ghcup install ghc 9.0.2 --force
```

ちなみに、ここで設定した `OPT` と `LLC` の値は `.ghcup/ghc/9.0.2/lib/ghc-9.0.2/settings` というファイルに記録されます。
