---
title: "GHCのLLVMバックエンドの使い方"
emoji: "🐉"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

この記事は[Haskellの環境構築2023](haskell-setup-2023)の一部として記載した内容を単体の記事として独立させたものです。

English version: [How to use GHC's LLVM backend](https://minoki.github.io/posts/2025-01-06-ghc-llvm-backend.html)

---

GHCはネイティブコードを生成できるコンパイラーですが、ネイティブコードの生成方法としてはGHC自身のNCG (Native Code Generator) バックエンドと、LLVMを使うLLVMバックエンド、それからC言語のソースを経由するvia Cバックエンドの3種類を持っています。

普段、x86やAArch64向けのコードを生成する際はNCGバックエンドが利用されますが、何らかの事情でLLVMバックエンドを使いたいこともあるかと思います。この記事では、GHCでLLVMバックエンドを使えるようにする方法を解説します。

<!-- GHCは2010年にリリースされたバージョン7.0からLLVMバックエンドを持っています。 -->

NCGバックエンドと比較した、LLVMバックエンドの特徴は以下になります：

* 多くのアーキテクチャーに対応している（執筆時点ではARM/AArch64/LoongArch/RISC-V/System Z/x86/x86-64）。
* SIMDプリミティブに対応している（NCGはGHC 9.12からx86-64向けで一部のSIMDプリミティブが使えるようになったが、まだ発展途上）。
* LLVMの最適化を利用できる。

ターゲットによっては、LLVMバックエンドの利用が必須であることもあります。特に、GHC <= 9.0でAArch64向けのコードを生成するにはLLVMバックエンドが必要となります。また、GHC <= 9.10でRISC-V向けのコードを生成する場合もLLVMバックエンドが必要となります。

なお、「LLVMのインストール」と「GHCにLLVMの位置を教える」はWindowsには当てはまらないので、Windowsの場合は「Windowsの場合」に飛んでください。

## LLVMのインストール

一般に、（Windows版を除いて）LLVMはGHCには同梱されておらず、システムのパッケージマネージャー等でGHCとは別にLLVMをインストールする必要があります。

LLVMにはバージョンによって変更が加えられていくので、特定のGHCには特定のバージョンのLLVMが必要です。対応関係をまとめた表は次のようになります：

| GHC | LLVM |
|:-|:-|
| GHC 8.10.7 | LLVM >= 9 && <= 12 |
| GHC 9.0.2 | LLVM >= 9 && <= 12 |
| GHC 9.2.8 | LLVM >= 9 && <= 12 |
| GHC 9.4.8 | LLVM >= 10 && <= 13（ただし、`-mavx` を使う場合はLLVM <= 12が必要）|
| GHC 9.6 | LLVM >= 11 && <= 15（ただし、`-mavx` を使う場合はLLVM <= 12が必要）|
| GHC 9.8 | LLVM >= 11 && <= 15（ただし、`-mavx` を使う場合はLLVM <= 12が必要）|
| GHC 9.10 | LLVM >= 13 && <= 15 |
| GHC 9.12 | LLVM >= 13 && <= 19 |

（注：GHC 9.8以下で `-mavx` を使う場合は[GHC #23870](https://gitlab.haskell.org/ghc/ghc/-/issues/23870)という問題があるので、LLVM 12を使ってください。）

ここではGHC 9.12.1 + LLVM 15の組み合わせをインストールしてみます。

Ubuntuなどのaptを使うシステムでは、

```
$ sudo apt install llvm-15 clang-15
```

を実行すれば良いでしょう。

macOSでHomebrewを使っている場合は

```
$ brew install llvm@15
```

を実行すれば良いでしょう。

macOSでMacPortsを使っている場合は

```
$ sudo port install llvm-15 clang-15
```

を実行すれば良いでしょう。

LLVMが提供するライブラリーや各種コマンド群のうち、GHCに必要なのは最適化器である `opt` コマンド、アセンブリーを出力する `llc` コマンド、それからGHC 9.10以降ではアセンブラーの役割を果たす `clang` コマンドです。これらがどこに配置されたか確認しましょう。

Ubuntuの場合は、コマンド名にサフィックス `-15` がつきました：

```
$ which opt-15 llc-15 clang-15
/usr/bin/opt-15
/usr/bin/llc-15
/usr/bin/clang-15
```

あるいは、`/usr/lib/llvm-15/bin` 以下にサフィックスのつかないコマンドが配置されています：

```
$ ls /usr/lib/llvm-15/bin/{opt,llc,clang}
/usr/lib/llvm-15/bin/clang
/usr/lib/llvm-15/bin/llc
/usr/lib/llvm-15/bin/opt
```

Homebrewの場合は、`$(brew --prefix llvm@15)/bin` 以下に各種コマンドが配置されました：

```
$ ls $(brew --prefix llvm@15)/bin/{opt,llc,clang}
/opt/homebrew/opt/llvm@15/bin/clang
/opt/homebrew/opt/llvm@15/bin/llc
/opt/homebrew/opt/llvm@15/bin/opt
```

MacPortsの場合は、コマンド名にサフィックス `-mp-15` がつきました：

```
$ which opt-mp-15 llc-mp-15 clang-mp-15
/opt/local/bin/opt-mp-15
/opt/local/bin/llc-mp-15
/opt/local/bin/clang-mp-15
```

あるいは、`/opt/local/libexec/llvm-15/bin` にサフィックスのつかないコマンドが配置されています：

```
$ ls /opt/local/libexec/llvm-15/bin/{opt,llc,clang}
/opt/local/libexec/llvm-15/bin/clang
/opt/local/libexec/llvm-15/bin/llc
/opt/local/libexec/llvm-15/bin/opt
```

サフィックスのつかないコマンドのあるディレクトリーにPATHを通しておくとGHCからも見つけやすくなって便利ですが、異なるGHCは異なるバージョンのLLVMを要求することがあるため、複数のLLVMを共存させるためにはサフィックスのつかないコマンドにPATHを通すことはお勧めしません。

## GHCにLLVMの位置を教える

GHCのLLVMバックエンドを使うには、GHCにLLVMのコマンドの位置を教える必要があります。方法としては、GHCの実行時に毎回教える方法と、GHCのインストール時に指定する方法があります。

### 毎回指定する方法

毎回教える場合は、`-pgmlo` で `opt` コマンドの位置を、`-pgmlc` で `llc` コマンドの位置を、GHC 9.10以降では `-pgmlas` で `clang` の位置を教えます。例えば、HomebrewでインストールしたLLVMをGHC 9.12.1から使う場合は次のようになるでしょう：

```
$ ghc-9.12.1 -fllvm -pgmlo $(brew --prefix llvm@15)/bin/opt \
    -pgmlc $(brew --prefix llvm@15)/bin/llc \
    -pgmlas $(brew --prefix llvm@15)/bin/clang Main.hs
```

### GHCのインストール時に指定する方法

GHCのインストール時にLLVMの位置を教えておけば、毎回 `-pgmlo` 等を指定する必要がなくなります。

まず、LLVMのツール群にPATHが通っており、コマンド名が

```
opt-15 opt-15.0 opt15 opt
llc-15 llc-15.0 llc15 llc
clang-15 clang-15.0 clang15 clang
```

の形をしていれば、GHCの `configure` コマンドによって自動的にLLVMのコマンド群が発見され、名前が記録されます。

GHCのインストール時点でLLVMが入っていなかった場合は、LLVMのインストール後に改めてGHCをセットアップする必要があります。GHCupであれば `ghcup install ghc 9.12.1 --force` という風に、Stackであれば `stack setup 9.12.1 --reinstall` とすれば良いでしょう。

そうでない場合、つまりHomebrewやMacPortsを使っており、PATHが通っていなかったりサフィックスが変則的な場合は、GHCの `configure` コマンドに手動で `opt` / `llc` / `clang` の位置を教えてやる必要があります。具体的には、GHCのインストール時に `OPT`, `LLC`, `LLVMAS` の各環境変数を指定します。

GHCupの場合は、次のような手順になります：

```
$ # Homebrewを使う場合
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      ghcup install ghc 9.12.1 --force

$ # MacPortsを使う場合
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      ghcup install ghc 9.12.1 --force
```

Stackを使ってGHCをインストールした場合は、次のような手順になります：

```
$ # Homebrewを使う場合
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      stack setup 9.12.1 --reinstall

$ # MacPortsを使う場合
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      stack setup 9.12.1 --reinstall
```

tarballから直接インストールする場合は、次のような手順になります：

```
$ curl -LO https://downloads.haskell.org/~ghc/9.12.1/ghc-9.12.1-aarch64-apple-darwin.tar.xz
$ tar xf ghc-9.12.1-aarch64-apple-darwin.tar.xz
$ cd ghc-9.12.1-aarch64-apple-darwin

$ # Homebrewを使う場合
$ env OPT=$(brew --prefix llvm@15)/bin/opt \
      LLC=$(brew --prefix llvm@15)/bin/llc \
      LLVMAS=$(brew --prefix llvm@15)/bin/clang \
      ./configure --prefix=/opt/ghc-9.12.1

$ # MacPortsを使う場合
$ env OPT=opt-mp-15 LLC=llc-mp-15 LLVMAS=clang-mp-15 \
      ./configure --prefix=/opt/ghc-9.12.1

$ sudo make install
```

GHCのセットアップ時に検出あるいは指定された `opt`, `llc`, `clang` の名前は、`lib/settings` というファイルに記録されます。見てみましょう：

```
$ # GHCupの場合
$ grep LLVM ~/.ghcup/ghc/9.12.1/lib/ghc-9.12.1/lib/settings
,("LLVM target", "arm64-apple-darwin")
,("LLVM llc command", "/opt/homebrew/opt/llvm@15/bin/llc")
,("LLVM opt command", "/opt/homebrew/opt/llvm@15/bin/opt")
,("LLVM llvm-as command", "/opt/homebrew/opt/llvm@15/bin/clang")

$ # Stackの場合
$ grep LLVM ~/.stack/programs/aarch64-osx/ghc-9.12.1/lib/ghc-9.12.1/lib/settings
,("LLVM target", "arm64-apple-darwin")
,("LLVM llc command", "/opt/homebrew/opt/llvm@15/bin/llc")
,("LLVM opt command", "/opt/homebrew/opt/llvm@15/bin/opt")
,("LLVM llvm-as command", "/opt/homebrew/opt/llvm@15/bin/clang")
```

注意して欲しいのは、セットアップ時に一時的に `PATH=$(brew --prefix llvm@15)/bin:$PATH` という風に `opt` 等のコマンドへのパスを通した場合は、`lib/settings` には（コマンドへのフルパスではなく）`opt` という名前しか記録されないことです。したがって、PATHを通すよりは、`OPT` 等の環境変数を使って指定するべきです。

## Windowsの場合

Windowsでは、GHC 9.4以降でGHC自身にLLVMが付属するようになりました。そして、GHC 9.12以降ではGHC自身に付属するLLVMが自動で利用できるようになります。

一方、GHC <= 9.10ではLLVMバックエンドで浮動小数点数を使おうとした際にリンクエラーが発生するので実質使い物になりません。詳しくは[GHC #22487](https://gitlab.haskell.org/ghc/ghc/-/issues/22487)を参照してください。

## LLVMバックエンドを使う

うまくセットアップできていれば、GHCに `-fllvm` オプションを渡すことでLLVMバックエンドを利用できます：

```
$ ghc -fllvm hello.hs
```

もちろん、LLVMのコマンドを「毎回指定する方法」を選んだ場合は、`-pgmlo` などのオプションが必要です。

## GitHub ActionsでLLVMバックエンドを使う

GitHub ActionsでLLVMバックエンドを有効にしたいこともあるかと思います。ここまでの知識を活用すれば、GitHub ActionsでLLVMバックエンドを有効化することは難しくないと思います。実際に、GitHub ActionsでLLVMバックエンドを有効にする例を[ghc-llvm-backend-test/.github/workflows/build.yaml](https://github.com/minoki/ghc-llvm-backend-test/blob/main/.github/workflows/build.yaml)に用意しました。

注意点としては、runner imageがすでにGHCを含んでいる場合があることです。そういう場合は、GHCupに `--force` オプションを渡す必要があります。現状のhaskell-actions/setupでは `--force` オプションを渡すことはできないので、GHCupを直接インストールするか、[haskell/ghcup-setup](https://github.com/marketplace/actions/ghcup-setup)を使うと良いかもしれません。

## LLVMバックエンドに関する既知の問題

LLVMバックエンドに関する既知の問題をいくつか挙げておきます。すでに言及したものもあります。

* GHC <= 9.8 && LLVM >= 13: [#23870: LLVM 13+ doesn't recognize `-stack-alignment` option · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/23870) `-mavx` を指定すると発現する。

* GHC <= 9.10 on Windows: [#22487: Link error when using LLVM backend on Windows: undefined symbol: `_fltused` · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/22487) 浮動小数点数を使うと発現する。

* GHC 9.10.1 on macOS: [#24999: LLVM version detection logic in configure doesn't work on macOS · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/24999)

## まとめ

GHCのLLVMバックエンドをガンガン使っていきましょう。
