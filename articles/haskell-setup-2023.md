---
title: "Haskellの環境構築2023"
emoji: "🏗"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

この記事は[Haskell Advent Calendar 2022](https://qiita.com/advent-calendar/2022/haskell)の1日目の記事です。

---

この記事では、2022年12月時点のHaskellの環境構築手順を紹介します。2023年になっても通用するといいなあ。

# 対象とする環境

対象とする環境は以下の通りです：

* Unix系
    * macOS (Intel / Apple Silicon)
    * Linux (x86\_64 / aarch64)
        * WSL2を含む（WSL1は不具合があった気がするので避けてください）
* Windows (x64)

Arm系CPU搭載のコンピューターを使っている場合は、別途LLVMが必要になる場合があります。以下に当てはまる場合は、「補遺：LLVMバックエンドを使う」も読んでください：

<!-- * 32ビットArm（Raspberry Pi OSの32ビット版など）を使う場合 -->
* 64ビットArm（Apple Silicon Macや、Raspberry Pi OSの64ビット版など）で、GHC 9.0またはそれ以前のバージョンを使う場合

Apple Silicon Macに関しては[Apple Silicon MacでのHaskell/GHCの現状・2022年3月編](https://qiita.com/mod_poppo/items/1abc155b5a5265d7dc45)も参考になるかもしれません。

# Stackか、GHCup+Cabalか

Haskellの環境構築をするには、大きく分けて**Stack**と**GHCup+Cabal**の2通りがあります。そのほかにOS固有のパッケージマネージャーを使うという方法もありますが、ここでは紹介しません。

StackとGHCup, Cabalがそれぞれ何をするツールなのかと言うと、

* Stack: GHCのインストールおよび、プロジェクトのビルドを行う
* GHCup: GHCや周辺ツール (Cabal/HLS/Stack) のインストールを行う
* Cabal (cabal-install)[^cabal]: プロジェクトのビルドを行う

[^cabal]: 大文字始まりの「Cabal」と書いた場合はHaskellライブラリーとしてのCabalを、「cabal-install」と書いた場合は実行可能コマンドとしての `cabal` を指すという使い分けが存在しますが、この記事では後者を単にCabalと呼ぶことにします。

となります。役割が被っているために対立して語られるわけです。

ですが、後述する設定をすればStackの「GHCのインストール」の部分をGHCupに任せることができます。また、プロジェクトファイルの形式はStackとCabalで互換性があります[^project-file]。なので、本稿では **GHCup, Cabal, Stackを全部インストールしてしまえ！** という欲張りな方針を取ります。

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
* Stack（Unixの場合自動でインストールされる。Windowsでは選択制）

がインストールされ、選択次第で以下のツールもインストールできます：

* HLS (Haskell Language Server)
* （Windowsの場合）MSYS2
    * MSYS2はWindows上でUnixライクなツール群（bashなど）を提供するソフトウェアで、ビルド時にconfigureを使うHaskellパッケージのために必要となります。
    * すでにMSYS2を導入している場合はそれを使うことができるようです。

StackやHLSは後からインストールすることもできるので、この段階でインストールしなくても問題ありません。

## Unixの場合

Unixの場合の選択項目は、主に3つです：

一つ目。

```
Detected bash shell on your system...
Do you want ghcup to automatically add the required PATH variable to "$HOME/.bashrc"?

[P] Yes, prepend  [A] Yes, append  [N] No  [?] Help (default is "P").
```

→環境変数を自動で設定するか聞かれます。よくわからなければデフォルトの `P` を、勝手にいじられたくない場合は `N` を選ぶと良いでしょう。追記される内容は

```sh
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
```

で、 `$HOME/.ghcup/env` の中では `$HOME/.ghcup/bin` と `$HOME/.cabal/bin` をPATHに追加しています。

二つ目。

```
Do you want to install haskell-language-server (HLS)?
HLS is a language-server that provides IDE-like functionality
and can integrate with different editors, such as Vim, Emacs, VS Code, Atom, ...
Also see https://haskell-language-server.readthedocs.io/en/stable/

[Y] Yes  [N] No  [?] Help (default is "N").
```

→Haskell Language Serverを入れるか聞かれます。後から入れることもできるので、この段階ではどちらでも構いません。

三つ目。

```
Do you want to enable better integration of stack with GHCup?
This means that stack won't install its own GHC versions, but uses GHCup's.
For more information see:
  https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation-experimental
If you want to keep stacks vanilla behavior, answer 'No'.

[Y] Yes  [N] No  [?] Help (default is "Y").
```

→StackでGHCをインストールする際にGHCupを使うような連携機能を有効にするか聞かれます。これは後から選択をやり直すことはできません。まあ後で欲しくなったらGHCupのインストーラーを再度実行すれば良いだけですが。

GHCのインストール時にCコンパイラーが見つからないようなことを言われた場合は、Cコンパイラーをインストール（Ubuntuの場合は `sudo apt install build-essential`）してやり直してください。

GHCupでインストールした諸々のファイルは `~/.ghcup/` 以下に保存されます。`ghcup` コマンドは `~/.ghcup/bin/ghcup` という具合です。そのため、`~/.ghcup/bin` にPATHを通す必要がありますが、すでに書いたようにインストール時の選択次第で `.bashrc` を自動でアップデートさせることができます。

`.bashrc` の変更後、シェルを立ち上げ直せば環境変数の設定が反映されます。シェルの再起動が面倒な場合は、 `source ~/.ghcup/env` すれば良いでしょう。

GHCupを削除したくなった場合は、 `~/.ghcup/` を丸ごと削除して、シェルの設定ファイルを元に戻せばOKです。

## Windowsの場合

Windowsの場合の質問事項はUnixとちょっと違います。

```
Where to install to (this should be a short Path, preferably a Drive like 'C:\')?
If you accept this path, binaries will be installed into 'C:\ghcup\bin' and msys2 into 'C:\ghcup\msys64'.
Press enter to accept the default [C:\]:
```

→GHCupのインストール先を聞かれます。そのままEnterを押せばデフォルトの `C:\` が選択され、 `C:\ghcup\` に諸々のファイルが入ります。

```
Specify Cabal directory (this is where haskell packages end up)
Press enter to accept the default [C:\\cabal]:
```

→Cabalのインストール先を聞かれます。そのままEnterを押せばデフォルトの `C:\cabal` が選択されます。

```
Install HLS
Do you want to install the haskell-language-server (HLS) for development purposes as well?
[Y] Yes  [N] No  [A] Abort  [?] Help (default is "N"):
```

→Haskell Language Serverを入れるか聞かれます。後から入れることもできるので、この段階ではどちらでも構いません。

```
Install stack
Do you want to install stack as well?
[Y] Yes  [N] No  [A] Abort  [?] Help (default is "N"): 
```

→Stackを入れるか聞かれます。後から入れることもできるので、この段階ではどちらでも構いません。

```
Install MSys2
Do you want GHCup to install a default MSys2 toolchain (recommended)?
[Y] Yes  [N] No  [?] Help (default is "Y"):
```

→MSYS2を入れるか聞かれます。Noを選択するとインストール済みのMSYS2の場所を要求されます。

一通り選択肢を選び終わると、MSYS2のインストールが始まります（未インストールの場合）。それが終わると、PowerShellとは別にMSYS2のターミナルが立ち上がってGHC等のインストールが行われます。

Windowsの場合、GHCupがインストールする諸々のファイルは `C:\ghcup\` 以下に保存されます。Cabalのディレクトリーは `C:\cabal\` となります。

インストーラーによって環境変数の設定やデスクトップへのショートカットの設置も行われるようです。

GHCupのインストールが終わったら、環境変数を反映させるためにコマンドプロンプト／PowerShellを再起動してください。

## GHCupを使ってみる

セットアップができたら早速 `ghcup` コマンドを使ってみましょう。まず、引数を何も与えずに `ghcup` コマンドを実行するとオプションの一覧が出力されます。

```sh
ghcup
```

`ghcup` はコマンド名の直後にサブコマンド名を取るようになっています。例えば、今インストールされているもの・GHCupでインストールできるものを一覧するコマンドは

```sh
ghcup list
```

という具合です。

コマンドをカチャカチャ打つのが面倒な人は

```sh
ghcup tui
```

で対話的なテキストユーザーインターフェースを起動することもできます（現時点ではUnix版のみ）。

# GHCを使う

デフォルトではGHCupのインストール時に「それなりに新しくてしかも安定している」バージョンのGHCがインストールされます。この記事の執筆時点では9.2.5がインストールされました。実際のバージョンは次のコマンドで確認できます：

```sh
ghc --version
```

## 対話環境を試す

対話環境を起動してみましょう。端末に `ghci` もしくは `ghc --interactive` と打ち込みます：

```sh
ghci
```

すると、 `ghci>` というプロンプトが表示されて入力待ちになります。

```
GHCi, version 9.2.5: https://www.haskell.org/ghc/  :? for help
ghci> 
```

5の階乗を計算してみましょう。プロンプトに `product [1..5]` と打ち込んでみます：

```
ghci> product [1..5]
120
```

無事計算できました。

`:quit` と打ち込むと対話環境が終了します。

## プログラムをコンパイル・実行する

今度はファイルに記述したHaskellプログラムをコンパイルしてみましょう。テキストエディタで `Hello.hs` というファイルを作って、以下の内容で保存します：

```haskell
main = do { putStrLn "Hello world!"; print (product [1..5]) }
```

端末で `Hello.hs` を保存したディレクトリーに移動して、次のコマンドを打ち込みます：

```sh
ghc Hello.hs
```

何事もなければ `Hello` （Windowsの場合は `Hello.exe`）という名前の実行ファイルが生成されます。実行してみましょう：

```sh
./Hello
```

以下の内容が表示されれば成功です：

```
Hello world!
120
```

簡単なプログラムの場合は、 `runghc` コマンド[^runhaskell]で直接実行することもできます：

[^runhaskell]: `runhaskell` という別名もあります。

```sh
runghc Hello.hs
```

## GHCの管理

GHCupが自動でインストールしたGHCではなく、最新版のGHCを使ったり、あえて古いバージョンのGHCを使ったりしたいことがあります。そんな時のために、GHCupでは複数のGHCを管理できます。

まず、特定のバージョンのGHCをインストールするコマンドは次の形です：

```sh
ghcup install ghc <バージョン>
```

例えば、GHC 9.4.3を入れるには次のコマンドを実行します：

```sh
ghcup install ghc 9.4.3
```

新しくインストールされたGHCのコマンド名は `ghc-<バージョン>` となります。9.4.3であれば

```sh
ghc-9.4.3 --version
```

という具合です。

デフォルトのGHC（バージョン番号がつかない `ghc` コマンドが指すもの）を変更するには、 `ghcup set` コマンドを使います。

```sh
ghcup set ghc <バージョン>
```

例：

```sh
ghcup set ghc 9.4.3
ghc --version  # The Glorious Glasgow Haskell Compilation System, version 9.4.3
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

Cabalを使ってプロジェクトを作るやり方はここでは扱いません。2年前の記事ですが

* [おすすめHaskellプロジェクト作成方法(ほぼ)2021年版](https://zenn.dev/autotaker/articles/haskell-setup-2021)

などを参考にしてください。

# Stackを使う

## Stackとは

まず、Stackとはどういうものなのか説明します。

StackはHaskellのライブラリーやプログラムをビルドするほか、GHCの管理もできるツールです。ただ、「GHCの管理」の部分は今回はGHCupに譲るというのは既に説明しました。

* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/)

昔はCabalが不便だったのでStackが革新的だったのですが、Cabal側も改良されてStackの相対的なメリットは少なくなってきたように思います。そんな中、今でも言及する価値がある（と筆者が考える）Stackの特徴は以下の通りです：

* 安定したパッケージの集合である[Stackage](https://www.stackage.org/)の利用が容易である
* YAMLを使って簡潔にプロジェクトファイルを記述できる (`package.yaml`)

もう少し詳しく説明します。まず、Stackageについて。

Haskellのパッケージは[Hackage](https://hackage.haskell.org/)というサイトにアップロードされますが、個々のパッケージの作者が好きなタイミングで最新版をアップロードすると、パッケージ間の食い合わせが悪くて動作しない場合があります。そこで、Stackageという「主要パッケージの、動作確認されたバージョンの組み合わせ」を集めるデータベースができました。この「組み合わせ」のことをresolverと呼びます。Stackageで提供されるresolverには、安定したバージョンであるLTS (long term support)系列と、より新しいが不安定なnightly系列があります。

Stackageのresolverは主要パッケージのバージョンだけでなく、GHCのバージョンも固定します。`lts-20.0` ならばGHC 9.2.5という具合です。

このStackageの利用が容易であるというのがStackの特徴の一つです[^cabal-stackage]。というか、Stackを使って開発をする際は常に何らかのresolverが使われます[^ghc-resolver]。

[^cabal-stackage]: CabalからStackageを利用することもできます。

[^ghc-resolver]: `ghc-<バージョン>` という、「GHC+付属ライブラリー」のみが含まれるresolverも指定できます。

もう一つの特徴である、YAMLを使ったプロジェクトファイルの記述について。

Cabalでは拡張子 `.cabal` を持つ独自の形式でプロジェクトの依存関係やモジュール一覧などを記述します。昔は `.cabal` 形式が不便だったので、HpackというYAML (`package.yaml`) でプロジェクトを記述できるツールが登場しました。

HpackをCabalで使うには手動で `hpack` コマンドを使って `.cabal` ファイルを生成する必要がありますが、StackはこのHpackを組み込んでいるので、 `stack` の各コマンドを実行するだけで `package.yaml` の内容が反映されます。

なお、Stackでも `.cabal` ファイルを利用できます。また、最近のCabal形式は便利になってきましたし、cabal-fmtのようなツールも登場しているので、Hpackの優位性は昔ほどではないかもしれません。

逆に、Stackが不利な状況も挙げておきます。Stackはresolverを固定する状況（アプリケーションを書く場合など）には強い一方で、複数のGHCを試したい状況（汎用的なライブラリーなど）には不便です。また、最新のGHCへ対応するまでタイムラグがあるため、新しい物好きの人は直接GHC+Cabalを使った方が良いかもしれません。

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

Stack関連のファイル（`stack` コマンド自身を除く）は `~/.stack/` （Unixの場合）または `%APPDATA%\stack\` （Windowsの場合）に保存されます。Stackを削除したくなった場合は、Unix系であれば `~/.stack/` を丸ごと削除すればOKです。

このままでは、Stackを使おうとするとGHCupとは独立にGHCがインストールされてしまいます。ディスクが余っている場合はそれでも構わないのですが、筆者のようにSSDがカツカツな場合は困るので、次の設定を行います。

## GHCupでインストールしたGHCをStackから使う

StackのデフォルトではシステムのGHCとは独立に、 `~/.stack/programs/` （Unixの場合）または `%LOCALAPPDATA%\Programs\stack\` （Windowsの場合）以下にGHCがインストールされます。ですが、すでにGHCupでGHCをインストールした場合はこれは無駄なので、StackでもシステムのGHCを使う設定を行います。**大容量のストレージを使っていて重複が気にならない場合はこのセクションは飛ばして構いません。**

この設定をするには次のコマンドを実行します：

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

ちなみに、Stackは使用するGHCのバージョンをresolverに基づいて独自に決定し、 `ghc-<バージョン>` の名前のコマンドがあればそれを使うので、 `ghcup set ghc` の結果は基本的にはStackには影響しません。

注意点として、素のStackにあった「未インストールのバージョンのGHCを要求されたらその場でインストールする」という機能が `install-ghc: false` で無効になります。適宜、手動でGHCupを実行しましょう。

なお、Stackで未インストールのGHCが必要になった時にGHCupを使ってその場でインストールするように連携する機能が実験中のようです。この機能は `system-ghc: true` の場合は無効になります。将来的には `system-ghc: true` を使わないやり方が主流になるかもしれません。

* [GHC installation customisation](https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation)

# Haskell Language Serverを使う

GHCupでHaskell Language Serverをインストールするには次のコマンドを実行します：

```sh
ghcup install hls
```

## Visual Studio Codeから使う

Visual Studio Codeの場合は「Haskell」拡張を入れてください。

* [Haskell - Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

HLS (Haskell Language Server)の管理方法を聞かれた場合は「Automatically via GHCup」を選んでください。

Windowsの場合は設定のGhcup Executable Pathを手動で `C:\ghcup\bin\ghcup.exe` と指定しなければいけないかもしれません。

GHCupのインストール時にHLSを入れなかった場合はここでHaskell Language Serverをインストールする必要があります。ターミナルに `ghcup install hls` と打ち込むか、VS CodeのGUIからインストールしましょう。

うまく動かない場合は、View > OutputからHaskell拡張のログを見ると良いです。

## HLSが最新のGHCに対応していない場合

HLSは最新のGHCに対応していない場合があります。その場合はGHCのバージョンを下げるのも手かもしれません。

別の方法として、HLSを自分でコンパイルするという手もあります。筆者がGHC 9.2.5 + HLS 1.8.0.0で試した時はstylish-haskell関係のプラグインとhlint関係のプラグインでビルドエラーが出たので、その2つを無効化する必要がありました：

```sh
ghcup compile hls --version 1.8.0.0 --ghc 9.2.5 -- --constraint "haskell-language-server -stylishhaskell -hlint"
```

# Haskell製ライブラリーをインストールする

Haskellで実用的なことをやろうとするとHackageで公開されたライブラリーを使うことになります。bytestring, text, vectorなどなど。

そういうライブラリーを使うには、まずインストール……**しません**。

Haskellプロジェクトからライブラリーを使いたい場合は、プロジェクトファイルに使用するライブラリーを明示します。するとビルド時に依存関係が自動で使えるようになります。なので、「依存関係のインストール」という手順は必要ありません。

次に、対話環境や書き捨てスクリプトでライブラリーを使いたい場合です。この場合、グローバルな環境にインストールするという手がありますが、筆者的にはお勧めできません。グローバルな状態というのは、いずれ管理しきれなくなることが目に見えているからです。

というわけで、プロジェクトを作ることなく、なおかつグローバルな環境を汚さずにライブラリーを使うことができると便利です。詳しくは

* [プロジェクトを作らずにHaskellをやる](haskell-without-project)

を読んでください。

# Haskell製ツールをインストールする

HackageではHaskellで書かれたツールも色々公開されています。CabalやStackを使うとそれらを簡単にインストールすることができます。

ツールの例：Alex, Happy, Hpack, stylish-haskell, cabal-fmt, Pandoc

Cabalでツールをインストールするには、 `cabal install <パッケージ名>` を実行します。例：

```sh
cabal install cabal-fmt
```

Cabalで入れた実行コマンドは、デフォルトでは `~/.cabal/bin/` （Unixの場合）または `C:\cabal\bin\` （Windowsの場合）にインストールされます。適宜PATHを通しましょう。

* [5.2.14. cabal install](https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-install)

:::message
ツールのバージョン間に互換性がなく、複数のバージョンを導入しなければならないことがあります。その場合は、 `--constraint` によるバージョン指定と、 `--program-suffix` による実行ファイル名のサフィックス付加を指定しましょう。

Happy 1.19を入れる例：

```sh
cabal install --program-suffix=-1.19 --constraint="happy==1.19.*" happy
```
:::

Stackでツールをインストールすることもできます。そのためには `stack install <パッケージ名>` を実行します。例：

```sh
stack install alex
```

Stackで入れた実行コマンドは、デフォルトでは `~/.local/bin/` （Unixの場合）または `%APPDATA%\local\bin\` （Windowsの場合）にインストールされます。適宜PATHを通しましょう。インストール先のパスは `stack path --local-bin` でも確認できます。

* [The stack install command and copy-bins option](https://docs.haskellstack.org/en/stable/GUIDE/#the-stack-install-command-and-copy-bins-option)

# 補遺：LLVMバックエンドを使う

GHCはネイティブコードを生成できるコンパイラーですが、ネイティブコードの生成方法としてGHC自身のNCG (native code generator) バックエンドとLLVMバックエンドの2種類を持っています。

普通はNCGで良いのですが、

* GHC 9.0またはそれ以前で64ビットArm向けコードを生成したい
* 32ビットArm向けコードを生成したい
* NCGとLLVMの最適化を比較したい
* SIMDなど、LLVMバックエンドしか対応してない機能を使いたい

などの場合はLLVMバックエンドも使えるようにしておく必要があります。**LLVMバックエンドを使わない場合はこのセクションは飛ばして構いません。**

## LLVMのインストール

まず、システムのパッケージマネージャーでLLVMをインストールします。GHCごとに対応するLLVMのバージョンが決まっているので、むやみに新しいLLVMを入れればいいというものではありません。具体的には、対応関係は次のようになります：

| GHC | LLVM |
|-|-|
| GHC 8.10.7 | LLVM 9以上12以下 |
| GHC 9.0.2 | LLVM 9以上12以下 |
| GHC 9.2 | LLVM 9以上12以下 |
| GHC 9.4 | LLVM 10以上13以下 |
| GHC 9.6 | LLVM 11以上15以下 |

ここではGHC 9.0.2 + LLVM 12を例とします。

Macユーザーの方は、Homebrewの場合は `brew install llvm@12` を、MacPortsの場合は `sudo port install llvm-12` を実行してLLVMをインストールします。

Ubuntuユーザーの場合は `sudo apt install llvm-12` とすれば良いでしょう。他のLinuxディストリビューションを使っている場合は各自調べてください。

何であれ重要なのは、LLVMの `opt` コマンドと `llc` コマンドがGHCから見えることです。

厄介なのがWindowsで、LLVMのWindows向け公式バイナリーには `opt.exe` と `llc.exe` が含まれません。Chocolateyは公式バイナリーを使っているので、Chocolateyを使ってLLVMを入れても意味がありません。どうしてもLLVMバックエンドを使いたい場合はMSYS2を使って `opt.exe` と `llc.exe` を入手すると良いでしょう。なお、GHC 9.4以降には `opt.exe` と `llc.exe` が付属する[^opt-llc-in-ghc-9-4]ようなので、将来的には（GHC 9.6以降？）それがデフォルトで利用できるようになるかもしれません[^llvm-on-windows]。

[^opt-llc-in-ghc-9-4]: 場所は `C:\ghcup\ghc\9.4.3\mingw\bin\{opt.exe,llc.exe}` という感じです。設定ファイル `C:\ghcup\ghc\9.4.3\lib\settings` を書き換えるか `-pgmlo` / `-pgmlc` オプションで指定してやると使えると思います（未確認）。

[^llvm-on-windows]: [Use bundled llc/opt on Windows (#22438) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22438)

## GHC側の設定

次にGHC側の設定方法ですが、Ubuntuのように、PATHの通った場所に `opt-12`, `llc-12` コマンドがあればGHCのインストール時に自動検出されます。

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

MacPortsの場合は `opt` コマンドと `llc` コマンドの名前に `mp` が含まれているので、次のように `ghcup` を実行すれば良いでしょう：

```sh
OPT=/opt/local/bin/opt-mp-12 LLC=/opt/local/bin/llc-mp-12 ghcup install ghc 9.0.2 --force
```

ちなみに、ここで設定した `OPT` と `LLC` の値は `~/.ghcup/ghc/9.0.2/lib/ghc-9.0.2/settings` というファイルに記録されます。

Stackで使うGHCをStack自身に管理させている場合は、次のコマンドを適切な `OPT`, `LLC` の下で実行してください：

```sh
OPT=ほにゃらら LLC=ほにゃらら stack setup 9.0.2 --reinstall
```

LLVMバックエンドを実際に使うにはGHCオプションとして `-fllvm` を指定します。

# 補遺：アルファ版・ベータ版のGHCを使う

新しいGHCのメジャーバージョンがリリースされる前にはアルファ版やベータ版が出るのが通例です。9.4系の場合はalpha1, alpha2, alpha3, rc1が出ました。

GHCupではprerelease channelというものを登録することによってそういう正式リリース前のバージョンを入れることができます。詳しくはGHCupのマニュアルを参照してください：

* [(Pre-)Release channels](https://www.haskell.org/ghcup/guide/#pre-release-channels)

アルファ版が出てからprerelease channelに登録されるまで若干タイムラグがある場合があります。待ちきれない場合は「Installing custom bindists」の手順で独自にインストールするという手があるかもしれません：

* [Installing custom bindists](https://www.haskell.org/ghcup/guide/#installing-custom-bindists)

# 追記：macOSで `<ffi.h>` が見つからないと言われる場合

macOSのアップグレード後に `<ffi.h>` が見つからないというエラーが出ることがあるようです。

その場合、 `/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk` が存在するか確認してください。存在しない場合は、

```
xcode-select --install
```

を実行すると直るかもしれません。

参考：

* [#22595: fatal error: 'ffi.h' file not found strikes back · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22595)
* ["fatal error: 'ffi.h' file not found" when trying to install hashable via cabal · Issue #814 · haskell/ghcup-hs](https://github.com/haskell/ghcup-hs/issues/814)

<!--
```
cabal user-config update --augment="extra-include-dirs: $(xcrun --sdk macosx --show-sdk-path)/usr/include/ffi" --augment="extra-lib-dirs: $(xcrun --sdk macosx --show-sdk-path)/usr/lib"
```
-->
