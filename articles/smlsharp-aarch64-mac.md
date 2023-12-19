---
title: "SML# (smlsharp) をApple Silicon Macに入れる"
emoji: "#️⃣"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [sml, standardml, smlsharp]
published: true
---

この記事は[ML (Meta Languages) Advent Calendar 2023](https://adventar.org/calendars/8849)の25日目の記事です。

---

SML\#は、Standard MLにレコード多相を含むいくつかの拡張機能を実装したプログラミング言語処理系です。

* [SML#プロジェクト](https://smlsharp.github.io/ja/)

ですが、現在（2023年末）のところ、対応しているのはx86\_64のUnix系OSのみで、特に、Apple Silicon MacのようなAArch64の環境には対応していません。

SML\#はコード生成にLLVMを使っているので、他のアーキテクチャーへの移植は難しくないはずです。実際、私 (mod\_poppo) が去年ぐらいに移植作業をして、簡単なプログラムが動作することを確認しました（複雑なプログラムは試していません）。

この記事では、私以外の人もSML\#をApple Silicon Macで動かせるようにする方法を解説します。

## 準備

Homebrewを前提とします。LLVM 13とGMPとlibffiを入れます。他にも必要なツールがあるかもしれませんが、各位で適当に判断してください。

```sh-session
$ brew install llvm@13 gmp libffi
```

`brew --prefix llvm@13` とか `brew --prefix gmp` とか `brew --prefix libffi` でパッケージのインストール先を参照できます。

以下の手順に登場する `make -j8` のコア数はマシンに合わせて適当にいじってください。私は8コアのApple M1を使っているので8を指定しています。

[ドキュメント](https://smlsharp.github.io/ja/documents/4.0.0/Ch5.S1.html)によると推奨されるLLVMのバージョンは11.1.0らしいです。が、私が前に試したときに使ったのがLLVM 13で、別のバージョンで試すのは怖い（面倒な）ので、LLVM 13を使うことにします。

作業用ディレクトリーを `$WORKDIR` とします。適当に掘ってください。パスに空白を含まない場所が良いでしょう。例：

```sh-session
$ WORKDIR=$HOME/smlsharp-work
$ mkdir -p $WORKDIR
```

SML\#（とMassiveThreads）の最終的なインストール先は `$PREFIX_AARCH64` とします。ここでは `/opt/smlsharp` とします。`$PREFIX_AARCH64` 以下にはAArch64のネイティブバイナリーのみが入るようにします。

```sh-session
$ PREFIX_AARCH64=/opt/smlsharp
```

### AArch64版MassiveThreadsのビルド

MassiveThreadsは軽量スレッドのライブラリーで、SML\#が使っています。SML\#開発チームの上野氏が2021年ごろにAArch64に対応させ、パッチがマージされているようですが、リリースされていないので、自分でソースを取ってきてビルドします。

```sh-session
$ cd $WORKDIR
$ git clone https://github.com/massivethreads/massivethreads.git
$ cd massivethreads
$ mkdir build-aarch64 && cd build-aarch64
$ ../configure --prefix="$PREFIX_AARCH64" CFLAGS="-Wall -O3"
$ make -j8
$ sudo make install
```

## 私 (mod_poppo) を信頼できる方向けの方法

SML\#処理系はSML\#で記述されています。つまり、SML\#をビルドするにはSML\#の処理系が必要です。コンパイラーの記述にSML\#の拡張機能を使っているため、普通のStandard MLコンパイラーではコンパイルできません。

しかし、SML\#はSML\#がない環境でも `make` コマンドでビルドできるようになっています。これは一体どうなっているのかというと、SML\#をLLVM IRにコンパイルしたものがソースアーカイブに含まれているのです。具体的には `precompiled/x86_64.ll.xz` が該当します。

なので、誰かが `aarch64.ll.xz` を用意すればそれを利用して他の人がSML\#をビルドできるようになります。ということで、私が用意した `aarch64.ll.xz` を含むブランチが[support-aarch64-precompiled](https://github.com/minoki/smlsharp/tree/support-aarch64-precompiled)になります。

ただ、`aarch64.ll.xz` の内容を人間が見て不正なコードが含まれていないか確認するというのは現実的ではないため、これは `aarch64.ll.xz` の作成者を信頼できる場合の方法となります。ということで、私 (mod\_poppo) を信頼できる場合にこの方法を使ってください。

```sh-session
$ cd $WORKDIR
$ git clone -b support-aarch64-precompiled https://github.com/minoki/smlsharp.git smlsharp-precompiled
$ cd smlsharp-precompiled
$ ./configure --prefix="$PREFIX_AARCH64" --with-llvm="$(brew --prefix llvm@13)" --with-massivethreads="$PREFIX_AARCH64" CFLAGS="-I$(brew --prefix gmp)/include -I$(brew --prefix libffi)/include" LDFLAGS="-L$(brew --prefix gmp)/lib -L$(brew --prefix libffi)/lib"
$ make -j8
$ file src/compiler/smlsharp
src/compiler/smlsharp: Mach-O 64-bit executable arm64
```

ビルドできました。動作確認してみましょう。

```sh-session
$ src/compiler/smlsharp -Bsrc
SML# 4.0.1-35.gfd6a5036 (2023-12-19 22:12:58 JST) for arm64-apple-darwin22.6.0 with LLVM 13.0.1
# 1 + 1;
val it = 2 : int
# ^D
```

良さそうですね。インストールしましょう。

```sh-session
$ sudo make install
$ export PATH="$PREFIX_AARCH64/bin:$PATH"
$ smlsharp
SML# 4.0.1-35.gfd6a5036 (2023-12-19 22:12:58 JST) for arm64-apple-darwin22.6.0 with LLVM 13.0.1
# 1 + 1;
val it = 2 : int
# fun fact 0 = 1 | fact n = n * fact (n - 1) : IntInf.int;
val fact = fn : intInf -> intInf
# fact 100;
val it =
  93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  : intInf
# ^D
```

いい感じですね。

## 全部自分でビルドする方法

私が作った `aarch64.ll.xz` を信頼できない場合は、SML\#開発チームが用意した `x86_64.ll.xz` からスタートすることになります。幸い、MacにはRosetta 2があるのでx86\_64のバイナリーも動かせます。Linuxの人はQEMUを使うといけるんでしょうか。

なお、x86\_64版のHomebrewを使えば依存関係のインストールが楽になると思いますが、私はApple Silicon Macにx86\_64版のHomebrewを入れたくないので、依存関係は手作業で用意します。MacPortsはUniversal Binaryに対応しているので、MacPortsを使うという手もあるかもしれません。

x86\_64版の依存ライブラリー等は `$PREFIX_X86_64` 以下にインストールすることにします。事が終わればこれは不要になるので、ここでは作業ディレクトリー以下に用意します。

```sh-session
$ PREFIX_X86_64=$WORKDIR/prefix-x86_64
```

### SML\#のチェックアウト

この先、SML\#をx86\_64向けとAArch64向けの2回ビルドすることになります。ソースディレクトリーとビルドディレクトリーを分ければ1回のcloneで済ませることができますが、SML\#はそういうの（out-of-place build）に対応していなさそうなので、ソースツリーの複製を2個用意することになります。ネット回線が遅いと `git clone` が遅いので、一旦ローカルにcloneしてそれを改めてcloneすることにします。

```sh-session
$ cd $WORKDIR
$ git clone -b support-aarch64 https://github.com/minoki/smlsharp.git
```

### x86\_64版GMPのビルド

先述のようにx86\_64版のHomebrewを入れたくないので、自分でGMPをビルドします。

私の環境ではデフォルト設定だと `make check` がコケたので、configure時に `--disable-assembly` を指定しています。

```sh-session
$ cd $WORKDIR
$ curl -LO https://gmplib.org/download/gmp/gmp-6.3.0.tar.xz
$ tar xf gmp-6.3.0.tar.xz
$ cd gmp-6.3.0
$ arch -x86_64 ./configure --prefix="$PREFIX_X86_64" --disable-assembly
$ arch -x86_64 make -j8
$ arch -x86_64 make check -j8  # 好みに応じて
$ arch -x86_64 make install
```

### x86\_64版libffiのビルド

元々のSML\#はlibffiへは依存しませんが、私が改造したsupport-aarch64ブランチではlibffiに依存しているので、libffiを用意します。

```sh-session
$ cd $WORKDIR
$ curl -LO https://github.com/libffi/libffi/releases/download/v3.4.4/libffi-3.4.4.tar.gz
$ tar xf libffi-3.4.4.tar.gz
$ cd libffi-3.4.4
$ arch -x86_64 ./configure --prefix="$PREFIX_X86_64"
$ arch -x86_64 make -j8
$ arch -x86_64 make install
```

### x86\_64版LLVMの準備

LLVMを自分でビルドすると時間がいくらあっても足りないので、ビルド済みバイナリーを取ってきます。

```sh-session
$ cd $WORKDIR
$ curl -LO https://github.com/llvm/llvm-project/releases/download/llvmorg-13.0.1/clang+llvm-13.0.1-x86_64-apple-darwin.tar.xz
$ tar xf clang+llvm-13.0.1-x86_64-apple-darwin.tar.xz
$ LLVM_X86_64_DIR=$WORKDIR/clang+llvm-13.0.1-x86_64-apple-darwin
```

### x86\_64版MassiveThreadsのビルド

先ほどcloneしたMassiveThreadsをx86\_64向けにビルドします。

```sh-session
$ cd $WORKDIR/massivethreads
$ mkdir build-x86_64 && cd build-x86_64
$ arch -x86_64 ../configure --prefix="$PREFIX_X86_64" CFLAGS="-Wall -O3"
$ arch -x86_64 make -j8
$ arch -x86_64 make install
```

### x86\_64版SML\#のビルド

ビルドします。

```sh-session
$ cd $WORKDIR
$ git clone "$WORKDIR/smlsharp" smlsharp-x86_64
$ cd smlsharp-x86_64
$ arch -x86_64 ./configure --with-llvm="$LLVM_X86_64_DIR" --with-massivethreads="$PREFIX_X86_64" CC=clang CXX=clang++ CFLAGS="-I$PREFIX_X86_64/include" LDFLAGS="-L$PREFIX_X86_64/lib"
$ arch -x86_64 make -j8
$ file src/compiler/smlsharp
src/compiler/smlsharp: Mach-O 64-bit executable x86_64
```

x86\_64向けとはいえ、せっかくビルドした `smlsharp` なので、動作確認してみましょう。

```sh-session
$ src/compiler/smlsharp -Bsrc
SML# 4.0.1-34.ge642f71e (2023-12-19 09:29:51 JST) for x86_64-apple-darwin22.6.0 with LLVM 13.0.1
# 1 + 1;
val it = 2 : int
# ^D
```

良さそうです。

### `aarch64.ll.xz` を準備する

ブートストラップに使う `aarch64.ll.xz` を用意しましょう。`precompile.mk` を使います。

`SMLFORMAT_DEP=` みたいな設定は、`UserError.ppg.sml is older than UserError.ppg smlformat` というエラーを回避するのに必要です（よくわかってない）。

さっき落としたLLVMのバイナリーに含まれる `libc++.dylib` がシステムのlibc++と被って嫌な感じになるので、`CXX="clang++ -Wl,-rpath,$LLVM_X86_64_DIR/lib"` を指定しています。これで `src/llvm/main/anonymize` のリンク時と実行時の両方で `$LLVM_X86_64_DIR/lib/libc++.dylib` を使うようになるはずです。

```sh-session
$ arch -x86_64 make -f precompile.mk -j8 all ARCH=aarch64 SMLLEX_DEP= SMLYACC_DEP= SMLFORMAT_DEP= CXX="clang++ -Wl,-rpath,$LLVM_X86_64_DIR/lib"
```

### AArch64版SML\#のビルド

`aarch64.ll.xz` を利用してAArch64版SML\#をビルドします。

```sh-session
$ cd $WORKDIR
$ git clone "$WORKDIR/smlsharp" smlsharp-aarch64
$ cd smlsharp-aarch64
$ cp ../smlsharp-x86_64/precompiled/aarch64.ll.xz precompiled/
$ ./configure --prefix="$PREFIX_AARCH64" --with-llvm="$(brew --prefix llvm@13)" --with-massivethreads="$PREFIX_AARCH64" CC=clang CXX=clang++ CFLAGS="-I$(brew --prefix gmp)/include -I$(brew --prefix libffi)/include" LDFLAGS="-L$(brew --prefix gmp)/lib -L$(brew --prefix libffi)/lib"
$ make -j8
$ file src/compiler/smlsharp
src/compiler/smlsharp: Mach-O 64-bit executable arm64
```

いい感じですね。動作確認してみましょう。

```sh-session
$ src/compiler/smlsharp -Bsrc
SML# 4.0.1-34.ge642f71e (2023-12-19 09:29:51 JST) for arm64-apple-darwin22.6.0 with LLVM 13.0.1
# 42 * 37; 
val it = 1554 : int
# ^D
```

良さそうですね。インストールしましょう。

```sh-session
$ sudo make install
$ export PATH=$PREFIX_AARCH64/bin:$PATH
$ smlsharp
SML# 4.0.1-34.ge642f71e (2023-12-19 09:29:51 JST) for arm64-apple-darwin22.6.0 with LLVM 13.0.1
# 1 + 1;
val it = 2 : int
# fun fact 0 = 1 | fact n = n * fact (n - 1) : IntInf.int;
val fact = fn : intInf -> intInf
# fact 100;
val it =
  93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  : intInf
# ^D
```

いい感じですね。

## おまけ：全部自分でビルドする方法（別解）

クロスコンパイラー的な感じでビルドする方法もあるので紹介しておきます。

「x86\_64版SML\#のビルド」までは「全部自分でビルドする方法」と同様です。

### AArch64版SML\#のビルド

クロスコンパイラーと言えばbuild/host/targetです。ここでは

* build=x86\_64（smlsharpのビルドを走らせるプラットフォーム）
* host=arm64（ビルドしたsmlsharpが動作するプラットフォーム）
* target=arm64（ビルドしたsmlsharpの出力したコードが動作するプラットフォーム）

とします。

```sh-session
$ cd $WORKDIR
$ git clone "$WORKDIR/smlsharp" smlsharp-aarch64-cross
$ cd smlsharp-aarch64-cross
$ arch -x86_64 ./configure --with-llvm="$LLVM_X86_64_DIR" --with-massivethreads="$PREFIX_AARCH64" --with-smlsharp="$WORKDIR/smlsharp-x86_64" --host=arm64-apple-darwin --target=arm64-apple-darwin CC=clang CFLAGS="-arch arm64 -I$(brew --prefix gmp)/include -I$(brew --prefix libffi)/include" CXX=clang++ CXXFLAGS='-arch arm64' LDFLAGS="-arch arm64 -L$(brew --prefix gmp)/lib -L$(brew --prefix libffi)/lib"
$ arch -x86_64 make -j8
$ file src/compiler/smlsharp
src/compiler/smlsharp: Mach-O 64-bit executable arm64
```

なんかできましたね。動かしてみましょう。

```sh-session
$ src/compiler/smlsharp -Bsrc
SML# 4.0.1-34.ge642f71e (2023-12-19 09:29:51 JST) for x86_64-apple-darwin22.6.0 with LLVM 13.0.1
# 1 + 1;
dynamic link failed: dlsym(0xa26bfa90, _SML_load): symbol not found
# ^D
```

ダメそうですね。なんか `x86_64-apple-darwin` とか出てるし。

実は `--target` オプションを指定するとうまくいきます。

```sh-session
$ src/compiler/smlsharp -Bsrc --target=arm64-apple-darwin
SML# 4.0.1-34.ge642f71e (2023-12-19 09:29:51 JST) for arm64-apple-darwin with LLVM 13.0.1
# 1 + 1;
val it = 2 : int
# ^D
```

ファイルのコンパイルも試してみましょう。

```sh-session
$ cat > hello.sml
print "Hello world!\n";
$ cat > hello.smi
_require "basis.smi"
$ src/compiler/smlsharp -Bsrc --target=arm64-apple-darwin -ohello hello.sml
$ ./hello
Hello world!
```

Standard ML完全に理解した！

`--target` オプションが必要なのは、多分x86\_64版のLLVMを参照しているのがいけないのだと思います。`src/config.mk` を書き換えてやるとうまく行くかもしれません（試してない）。あるいは、SML\#がLLVMに対して常に `--target` を明示的に指定してくれると良いのかもしれません。

### `aarch64.ll.xz` を準備する

ブートストラップに使う `aarch64.ll.xz` を用意しましょう。

`src/config.mk` がx86\_64向けのLLVMを参照しているのを上書きするために、`LLC`, `OPT`, `LLVM_AS`, `LLVM_DIS` の各変数を設定します。

ここではHomebrewによるLLVMを使うので、libc++周りの面倒な問題はありません。

```sh-session
$ make -f precompile.mk -j8 all ARCH=aarch64 LLC="$(brew --prefix llvm@13)/bin/llc" OPT="$(brew --prefix llvm@13)/bin/opt" LLVM_AS="$(brew --prefix llvm@13)/bin/llvm-as" LLVM_DIS="$(brew --prefix llvm@13)/bin/llvm-dis" SMLLEX_DEP= SMLYACC_DEP= SMLFORMAT_DEP=
$ file precompiled/aarch64.ll.xz
precompiled/aarch64.ll.xz: XZ compressed data, checksum CRC64
```

良さそうですね。あとは「全部自分でビルドする方法」の「AArch64版SML\#のビルド」の手順に従えばbuild=host=target=arm64なSML\#が出来上がります。

## 終わりに

それではみなさん、快適なSML\#ライフを！

SML\#も良いですが、私が開発しているLunarMLもよろしくお願いします。最近v0.1をリリースしました。

* [LunarML: LuaやJavaScriptを出力するStandard MLコンパイラー](https://blog.miz-ar.info/2023/12/lunarml-release/)
