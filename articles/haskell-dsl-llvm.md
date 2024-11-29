---
title: "HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜"
emoji: "😸"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell, llvm]
published: false
---

シリーズ：

* [HaskellでEDSLを作る：atomicModifyIORef編 〜自動微分を題材に〜](haskell-dsl-atomicmodifyioref)
* [HaskellでEDSLを作る：StableName編 〜共有の回復〜](haskell-dsl-stablename)
* HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜（この記事）

[HaskellでEDSLを作る：StableName編](haskell-dsl-stablename)では、`StableName` を使って計算の共有を回復する方法を見ました。

今回は、作ったDSLをLLVMでJITコンパイルする方法を見てみます。

## LLVMへのバインディング

HaskellからLLVMを呼び出すためのバインディングはいくつか存在します。ここでは、[llvm-hs](https://github.com/llvm-hs)のファミリーを利用します。

llvm-hsファミリーのパッケージの構成は以下のようになります：

* llvm-hs-pure: C++の部分には依存しない純Haskellの部分。
* llvm-hs: C++へのバインディング。
* llvm-hs-pretty: pretty printerだが、メンテされていない。

llvm-hsはHackageには古いやつしか上がっていないので、GitHubにあるものを利用します。llvm-15ブランチにLLVM 15対応のものがあるので、これを利用します。

Cabalのプロジェクトで、依存パッケージをGitから取ってくるには、`cabal.project` に次のように `source-repository-package` を記述します：

```:cabal.project
packages: ./*.cabal

source-repository-package
    type: git
    location: https://github.com/llvm-hs/llvm-hs.git
    tag: 5bca2c1a2a3aa98ecfb19181e7a5ebbf3e212b76
    subdir: llvm-hs-pure

source-repository-package
    type: git
    location: https://github.com/llvm-hs/llvm-hs.git
    tag: 5bca2c1a2a3aa98ecfb19181e7a5ebbf3e212b76
    subdir: llvm-hs
```

llvm-hsのビルドには、`llvm-config-15` コマンドあるいは `llvm-config` コマンドが必要となります。Homebrew等を利用しているとデフォルトではこれらへのPATHが通っていないので、`cabal.project.local` に以下のように記述してllvm-hsが `llvm-config` を見つけられるようにします：

```:cabal.project.local
package llvm-hs
    extra-prog-path: /opt/homebrew/opt/llvm@15/bin
```

Homebrewの場合は、具体的な場所は `echo $(brew --prefix llvm@15)/bin` でわかります。

llvm-hsのllvm-15ブランチに対するHaddockはHackage等では見られないので、ドキュメントが見たい方は自分で `git clone` してきて `cabal haddock` を実行してください。

## 参考にできるコード

[llvm-hs-examples](https://github.com/llvm-hs/llvm-hs-examples)に幾つかサンプルがありますが、masterブランチはLLVM 9向けで、他にはllvm-12ブランチがあるくらいなので、古いです。

## コンパイルした関数の呼び出し

FFI import, libffi
