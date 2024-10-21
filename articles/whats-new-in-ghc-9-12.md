---
title: "GHC 9.12の新機能"
emoji: "📝"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

GHC 9.12.1-alpha1が2024年10月16日にリリースされました。

* [GHC 9.12.1-alpha1 is now available! - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-12-1-alpha1-is-now-available/10544)

そのうち、GHCupのprerelease channelでも使えるようになるのではないかと思います（[Haskellの環境構築2023](haskell-setup-2023)の「補遺：アルファ版・ベータ版のGHCを使う」も参考）。

この記事では、GHC 9.12の新機能を筆者の独断と偏見に基づき確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)
* [GHC 9.6の新機能](whats-new-in-ghc-9-6)
* [GHC 9.8の新機能](whats-new-in-ghc-9-8)
* [GHC 9.10の新機能](whats-new-in-ghc-9-10)

です。

この記事は網羅的な紹介記事とはなっていません。是非、公式のリリースノート類も参照してください：

* [2.1. Version 9.12.1 — Glasgow Haskell Compiler 9.12.20241014 User's Guide](https://downloads.haskell.org/~ghc/9.12.1-alpha1/docs/users_guide/9.12.1-notes.html)
    * [docs/users_guide/9.12.1-notes.rst · ghc-9.12 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.12/docs/users_guide/9.12.1-notes.rst)
* [libraries/base/changelog.md · ghc-9.12 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.12/libraries/base/changelog.md)
* [9.12 · Wiki · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.12)

# GHC 9.12に入る機能

## MultilineStrings拡張

* [ghc-proposals/proposals/0569-multiline-strings.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0569-multiline-strings.rst)

複数行文字列リテラルがMultilineStrings拡張として実装されます。

従来のHaskellで複数行にわたって文字列リテラルを書くには、`unlines` 関数を使う方法や、ギャップ（文字列リテラル中のバックスラッシュで囲まれた空白が無視される機能）などがありました。

```haskell
str1 = unlines
  [ "aaa"
  , "bbb"
  , "ccc"
  ]
-- -> "aaa\nbbb\nccc\n"

str2 = "aaa\n\
       \bbb\n\
       \ccc\n"
-- -> "aaa\nbbb\nccc\n"
```

一方、MultilineStrings拡張を使うと、ダブルクォート3つで複数行文字列リテラルを書けるようになります。

```haskell
{-# LANGUAGE MultilineStrings #-}

str3 = """
       aaa
       bbb
       ccc
       """
-- -> "aaa\nbbb\nccc"
```

複数行文字列リテラルを実装する言語は色々ありますが、言語によって微妙に書き方が違ったりします。GHCに実装されたものの特徴を何点か挙げておきます。

* 共通するインデントは削除される。
* 先頭が改行であれば、改行 `\n` が1個削除される。
* 末尾が改行であれば、改行 `\n` が1個削除される。
* 入力ファイルの改行コードがCRLFであっても、文字列に埋め込まれる改行コードはLFとして扱われる（予定）
    * alpha1の段階ではこの挙動は実装されていませんが、正式版が出るまでに直る予定です。

このことがわかる例も載せておきます：

```haskell
{-# LANGUAGE MultilineStrings #-}

str4 = """
       aaa
          bbb
       ccc

       """
-- -> "aaa\n   bbb\nccc\n"

str5 = """
       aaa
       bbb
          ccc\n
       """
-- -> "aaa\nbbb\n   ccc\n"
```

## OrPatterns拡張

* [ghc-proposals/proposals/0522-or-patterns.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0522-or-patterns.rst)

パターンマッチでは、複数の枝で同じ処理をしたいことがあります。例えば、次のコードを考えます：

```haskell
data T = Foo | Bar | Baz

f :: T -> IO ()
f Foo = putStrLn "A"
f Bar = putStrLn "B"
f Baz = putStrLn "B" -- f Barと同じ！
```

ここでは、`f Bar` と `f Baz` で同じ処理をしたいとしましょう。ここでは同じ処理を2回書きました。

この例で「2回書く」以外の方法としては、「ワイルドカードパターン `_` を使う」という方法もあります。

```haskell
data T = Foo | Bar | Baz

f :: T -> IO ()
f Foo = putStrLn "A"
f _ = putStrLn "B"
```

しかし、ワイルドカードパターンを使うとパターンマッチ対象のデータ構築子を増やした時にコードの修正漏れが発生する可能性が上がります。つまり、`T` の定義が `Foo | Bar | Baz | Bazz` となった時に「警告やエラーが出たところを修正する」というやり方が通用しなくなります。

そこで、OrPatterns拡張です。これを使うと、セミコロン区切りで複数のパターンを書けるようになります：

```haskell
{-# LANGUAGE OrPatterns #-}

data T = Foo | Bar | Baz

f :: T -> IO ()
f Foo = putStrLn "A"
f (Bar; Baz) = putStrLn "B"
```

曖昧さがない場合は、括弧を使わずに書くことも可能です：

```haskell
{-# LANGUAGE OrPatterns #-}

g :: T -> IO ()
g x = case x of
        Foo -> putStrLn "A"
        Bar; Baz -> putStrLn "B"
```

レイアウトによるセミコロン挿入も有効です：

```haskell
{-# LANGUAGE OrPatterns #-}

h :: T -> IO ()
h x = case x of
        Foo -> putStrLn "A"
        Bar
        Baz -> putStrLn "B"
```

一方で、関数定義のパターンマッチでは括弧は省略できません：

```haskell
{-# LANGUAGE OrPatterns #-}

f :: T -> IO ()
f Foo = putStrLn "A"
f Bar; Baz = putStrLn "B" -- 不可
-- レイアウト規則的には
--   f Foo = putStrLn "A"
--   f Bar
--   Baz = putStrLn "B"
-- と書いたのと同じことになる（のでエラー）
```

## NamedDefaults拡張：default宣言の一般化

* [ghc-proposals/proposals/0409-exportable-named-default.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0409-exportable-named-default.rst)
* [6.11.3. Named default declarations — Glasgow Haskell Compiler 9.12.20241014 User's Guide](https://downloads.haskell.org/~ghc/9.12.1-alpha1/docs/users_guide/exts/named_defaults.html)

Haskellでは、型の曖昧性が発生する場合があります。例えば、

```haskell
main = print ((777 :: Integer) ^ 3)
```

の指数部の `3` の型はどうなるべきでしょうか？別の例として、

```haskell
main = print (read "123")
```

というコードにおいて `read` する型はどうなるべきでしょうか？

[Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-790004.3.4)では、曖昧な型変数に `Num` 系の制約がついている場合に、`default` 宣言によってこれを解決することを可能にします。具体的には、

* 型変数 `v` に対する制約が `C v` の形に限られること
* 制約しているクラスの少なくとも数値系（`Num` またはそのサブクラス）であること
* 制約しているクラスが全てPreludeか標準ライブラリーのクラスであること

という条件が満たされる場合に、

```haskell
default (t1, ..., tn)
```

という形の `default` 宣言に記述された型を順番に試すようにします（defaulting）。何も書かなかった場合は

```haskell
default (Integer, Double)
```

という `default` 宣言が有効なので、先の例の指数部の `3` は `Integer` に解決されます。一方、`print (read ...)` は数値系のクラスが絡まないのでエラーとなります。

GHCが拡張されるにつれて、このdefaultingに関する規則も拡張されてきました。例えば、GHCiではExtendedDefaultRulesという拡張が有効で、`print (read ...)` の例が通ります。OverloadedStrings拡張を使うと、`IsString` クラスにもdefaultingが働き、`String` 型がdefaultの候補に入ります。一方で、OverloadedLists拡張にはdefaultingは働きません。

NamedDefaults拡張では、`default` 宣言において

```haskell
default C (t1, ..., tn)
```

のようにクラスを指定することができます。そして、defaultingが発動する条件は

* 型変数 `v` に対する制約の中に `C v` の形のものが1つ以上あること

と緩和され、候補が `default C` の中から探索されます。該当するクラスが複数ある場合は、同じ候補に解決される必要があります。

また、モジュールから `default` 宣言をエクスポートすることもできるようになります。

詳細はGHC Proposalやドキュメントを見てください。

注意点として、GHC Proposalの例とは裏腹に、`IsList` に関しては実質使えないと思った方が良さそうです。要素を指定しないリスト `[]` 型は `IsList` のインスタンスではない（インスタンスとなるのは `[Int]` のように要素を指定した型）ので、

```haskell
default IsList ([])
```

という宣言はできません。そして、要素型を指定してみても

```haskell
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedDefaults #-}
import GHC.IsList

default IsList ([Char])

main = print ['a']
```

というコードは型推論の都合か何かでうまくいきません。

## `HasField` クラスとrepresentation polymorphism

GHCは、`HasField` クラスでレコードのフィールドにアクセスできる仕組みを持っています。例えば、GHC 9.2で追加されたOverloadedRecordDot拡張は、`HasField` クラスを使ってドット記法を脱糖しています。

`HasField` クラスは、従来は次のように定義されていました：

```haskell
module GHC.Records where

class HasField (x :: k) r a | x r -> a where
  getField :: r -> a
```

`x` はフィールド名で、典型的には `Symbol` カインドの型です。`r` はレコードの型、`a` はフィールドの型です。

`HasField` と `OverloadedRecordDot` の使用例は次のようになります：

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
import GHC.Records

instance HasField "successor" Int Int where
  getField x = x + 1

main :: IO ()
main = do
  print $ (37 :: Int).successor -- 37の次の整数（38）
```

さて、`HasField` クラスのカインドは、従来は `k -> Type -> Type -> Constraint` でした：

```
GHCi, version 9.10.1: https://www.haskell.org/ghc/  :? for help
ghci> :m + GHC.Records
ghci> :set -fprint-explicit-runtime-reps -fprint-explicit-kinds -XNoStarIsType
ghci> :k HasField
HasField :: k -> Type -> Type -> Constraint
```

このことは、レコードの型やフィールドの型としてunboxedな型やunliftedな型は使えないことを意味します。実際、次のコードはGHC 9.10ではコンパイルできませんでした：

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
import GHC.Exts
import GHC.Records

instance HasField "successor" Int# Int# where
  getField x = x +# 1#

main :: IO ()
main = do
  print $ I# (37# :: Int#).successor
```

この制限がGHC 9.12では緩和されます。GHC 9.12では `HasField` のカインドは次のようになります：

```
ghci> :m + GHC.Records
ghci> :set -fprint-explicit-runtime-reps -fprint-explicit-kinds -XNoStarIsType
ghci> :k HasField
HasField :: k -> TYPE r_rep -> TYPE a_rep -> Constraint
```

そして、`Int#` の例も通るようになります。

ちなみに、`TYPE` を使ってunboxedな型を統一的に扱えるようにする仕組みは当初はlevity polymorphismと呼ばれていましたが、これは今はrepresentation polymorphismと呼ばれています。[GHC 9.2](https://zenn.dev/mod_poppo/articles/ghc-9-2-and-future)でlifted boxed←→unlifted boxedのみを統一的に扱う「本物の（？）levity polymorphism」（`BoxedRep`）が導入されたことによります。

## RequiredTypeArguments拡張の強化（項の中に `->` と `=>` を書けるようになる）

* [Dependent Types in Haskell, Part 4](https://serokell.io/blog/serokell-s-work-on-ghc-dependent-types-part-4)

GHC 9.10で導入されたRequiredTypeArguments拡張（参照：[GHC 9.10で実装された可視なforallで遊ぶ](https://zenn.dev/mod_poppo/articles/playing-with-visible-forall)）ですが、GHC 9.10の時点では関数などの矢印は項のレベルでは使えませんでした（`type` の明示が必要）。この制限が緩和され、`->` や `=>` を `type` なしで書いても型として扱えるようになりました。

```haskell
{-# LANGUAGE RequiredTypeArguments #-}

id' :: forall a -> a -> a
id' _ x = x

main = do
  let f = id' (Int -> Int) (+ 5)
  -- GHC 9.10ではExplicitNamespaces拡張を使って
  -- let f = id' (type (Int -> Int)) (+ 5)
  -- と書く必要があった
  print $ f 37
```

## Unboxed `Float#`/`Double#` のHexFloatLiterals

HexFloatLiterals拡張を使うと、浮動小数点数の十六進表記（参考：[浮動小数点数の16進表記](https://qiita.com/mod_poppo/items/3fa4cdc35f9bfb352ad5)）ができるようになります。`0x1.cafep100` みたいなやつです。

これがunboxedな `Float#`/`Double#` 型でも使えるようになりました。例：

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE HexFloatLiterals #-}
import GHC.Exts

main :: IO ()
main = do
  print (F# 0x1.cafep0#)
  print (D# 0x1.cafep0##)
```

## UnliftedFFITypes拡張の制限の緩和

UnliftedFFITypes拡張を使うと、unliftedな型をFFIで受け渡しできます。`ByteArray#` やSIMDの型のように、UnliftedFFITypesを使わないと受け渡しできない型もあります。

今回、空のタプルを引数として扱えるようになりました。例：

```haskell
foreign import ccall unsafe foo :: (# #) -> Int32#
```

## NCGバックエンドのRISC-V（64ビット）対応

RISC-Vは新興の命令セットアーキテクチャーで、組み込み方面から勢力を伸ばしています。スマホやパソコンの市場を置き換えるものになるかはわかりませんが、SBC（ラズパイみたいなやつ）は色々登場しています。

そういうわけで、GHCもRISC-Vへの対応を進めています。GHC 9.2ではLLVMバックエンドで64ビットRISC-Vに対応しました。

今回、NCG (Native Code Generator) が64ビットRISC-Vに対応して、LLVMなしでもビルドできるようになります。

現時点では公式からはビルド済みのRISC-V向けGHCは配布されていないので、RISC-V向けコード生成を試したかったら自前でビルドすることになるでしょう。GHCをクロスコンパイラーとしてビルド・インストールする手順は次のようになります：

```sh
$ # 依存関係のインストール（Ubuntuの場合）
$ sudo apt install build-essential curl autoconf gcc-riscv64-linux-gnu g++-riscv64-linux-gnu
$ sudo apt install qemu-user

$ # ghcupを使ってGHC（9.6以降）をインストールしておく
$ ghcup install ghc 9.6.6 --set
$ cabal install alex happy

$ GHC_VERSION=9.12.20241014
$ curl -LO https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-src.tar.xz && tar -xJf ghc-$GHC_VERSION-src.tar.xz
$ cd ghc-$GHC_VERSION
$ ./configure --target=riscv64-linux-gnu

$ # ビルド（時間がかかる）
$ hadrian/build --bignum=native -j binary-dist-dir

$ # 生成されたバイナリーのインストール
$ cd _build/bindist/ghc-$GHC_VERSION-riscv64-linux-gnu
$ ./configure --target=riscv64-linux-gnu --prefix=$HOME/ghc-rv64 CC=riscv64-linux-gnu-gcc CXX=riscv64-linux-gnu-g++
$ make install
```

この手順で動かなかったら適宜修正してください。現時点ではビルド済みバイナリーの `configure` 時にも色々設定する必要があるのがポイントです。

実行例は次のようになります：

```sh
$ echo 'main = putStrLn "Hello world!"' > hello.hs
$ ~/ghc-rv64/bin/riscv64-linux-gnu-ghc hello.hs
$ file hello
hello: ELF 64-bit LSB executable, UCB RISC-V, RVC, double-float ABI, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux-riscv64-lp64d.so.1, BuildID[sha1]=250f432c120ef3948b7936b16a26b4add734ae69, for GNU/Linux 4.15.0, not stripped
$ qemu-riscv64 -L /usr/riscv64-linux-gnu/ ./hello
Hello world!
```

GHCが本格的に対応ということになってくると、RISC-Vの実機が欲しくなってきますね。

## x86 NCGでのSIMDサポート

* 関連記事：[Haskell/GHCのSIMDについて考える](https://blog.miz-ar.info/2023/08/haskell-simd/)（2023年8月）

SIMDはsingle instruction, multiple dataの略で、一つの命令で複数のデータを処理できるCPUの機能のことです。

専用の命令を使うので、活用にはコンパイラー側の対応が必要です。具体的には、普通のループをコンパイラー側で書き換えてSIMD命令を活用する（自動ベクトル化）か、専用のデータ型と組み込み関数を用意してプログラマーにSIMD命令を活用させるか、です。

現状のGHCでのやり方は後者で、`FloatX4#` のようなデータ型と `plusFloatX4#` のような組み込み関数が用意されています。ただ、これまではこれらに対応しているのはLLVMバックエンドに限られており、一般のライブラリーで活用するにはハードルが高い状態でした。

今回、x86向けのNCGが一部のSIMDデータ型と組み込み関数に対応しました。具体的には、128ビット幅の浮動小数点数ベクトル、つまり `FloatX4#` と `DoubleX2#` です。整数や256ビット以上には未対応です。また、LLVMではSSE2向けにコンパイルできるコードでもSSE 4.1を要求したりします。

とはいえ、実装のための面倒な部分（レジスターのスタックへの退避）が今回片付いたようなので、あとはやる気のある人が手を動かせば対応状況は改善していくのではないかと思います。私も暇があれば貢献するつもりです。

GHCのSIMDのサンプルコードも載せておきます。

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts

main :: IO ()
main = do
  let v = packFloatX4# (# 1.1#, 2.2#, 3.3#, 4.4# #)
      w = packFloatX4# (# 0.1#, 0.2#, 0.3#, 0.4# #)
      x = minusFloatX4# v w
      (# a, b, c, d #) = unpackFloatX4# x
  print (F# a, F# b, F# c, F# d)
```

コンパイルには、今回新しく対応したx86 NCGで

```
$ ghc -msse4 simdtest.hs
```

とするか、従来から対応しているLLVMバックエンドで

```
$ ghc -fllvm simdtest.hs
```

とします。

SIMDを真面目に使うには何らかのラッパーライブラリーが欲しいところですが、Hackageにあるやつ（[simd](https://hackage.haskell.org/package/simd)、[primitive-simd](https://hackage.haskell.org/package/primitive-simd)）は最終更新日時が古く、使えるか不明です。誰かが新たに作るべきかもしれません。

## SIMDプリミティブの追加

x86 NCGへのSIMDの実装と関連して、いくつかプリミティブが追加されました。例を挙げます：

```haskell
module GHC.Prim where
fmaddFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -- x * y + z
fmsubFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -- x * y - z
fnmaddFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -- - x * y + z
fnmsubFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -- - x * y - z
shuffleFloatX4# :: FloatX4# -> FloatX4# -> (# Int#, Int#, Int#, Int# #) -> FloatX4#
minFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
maxFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
```

浮動小数点数のmin/maxも追加されました

```haskell
module GHC.Prim where
minFloat# :: Float# -> Float# -> Float#
maxFloat# :: Float# -> Float# -> Float#
```

が、環境によって動作が違うので、将来仕様変更されるかもしれません（[#25350: Floating-point min/max primops should have consistent behavior across platforms · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/25350)）。

なお、新たに追加されたプリミティブは `GHC.Exts` からはエクスポートされません。ラッパーがないと普通のHaskellユーザーには縁遠いかもしれません。

## Windows上で何もしなくてもLLVMバックエンドを使える

[Haskellの環境構築2023](./haskell-setup-2023)では「Windows上にLLVMのツールを用意するのは厄介だ」というようなことを書きました。当時は `opt.exe` と `llc.exe` が公式の配布バイナリーに含まれなかったのです（今は含まれるようです）。しかも、何らかの方法でこれらを用意しても、浮動小数点数を使うとリンクエラーが出たりします。

今回、これらの問題が解決されて、Windows上で何もしなくてもLLVMバックエンドが使えるようになりました。つまり、`opt.exe` と `llc.exe` はGHCに付属のものが使われるようになり（実は少し前にWindows向けのGHCはClangを使うようになっており、LLVM自体は付属するようになっていたのでした）、浮動小数点数絡みのリンクエラーも解決しました。

## ライブラリー

### `Data.List{.NonEmpty}.compareLength`

```haskell
Data.List.compareLength :: [a] -> Int -> Ordering
Data.List.NonEmpty.compareLength :: NonEmpty a -> Int -> Ordering
```

`compare (length xs) n` の安全で高速な代替物です。つまり、`xs` の要素を全て数える必要がありませんし、`xs` が無限リストでも使えます。

```
ghci> compareLength ['A','B','C'] 3
EQ
ghci> compareLength [0..] 3
GT
```

### `flip` がrepresentation polymorphicになる

* [Representation-polymorphic `flip` · Issue #245 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/245)

なりました。

```
ghci> :set -fprint-explicit-runtime-reps
ghci> :type flip
flip
  :: forall (repc :: GHC.Types.RuntimeRep) a b (c :: TYPE repc).
     (a -> b -> c) -> b -> a -> c
```

型引数が増えた結果、 `flip` に型適用する際の挙動が変わるので注意してください。

### `read` が整数の二進表記に対応

* [Make `read` accept binary integer notation · Issue #177 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/177)

しました。

```
ghci> read "0b1011" :: Integer
11
ghci> read "0b1011" :: Int
11
```

### `Data.List.{inits1,tails1}`

* [Still more NonEmpty variants of inits & tails · Issue #252 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/252)

```haskell
module Data.List where

inits1 :: [a] -> [NonEmpty a]
tails1 :: [a] -> [NonEmpty a]
```

`inits1` は「前から `n` 個取って作った部分列」のリストを返します。`inits` と異なり、`n` は1以上となります。

`tails1` は「前の `n` 個を取り除いて作った部分列」のリストを返します。`tails` と異なり、`n` は1以上となります。

```
ghci> inits1 ["A","B","C","D"]
["A" :| [],"A" :| ["B"],"A" :| ["B","C"],"A" :| ["B","C","D"]]
ghci> tails1 ["A","B","C","D"]
["A" :| ["B","C","D"],"B" :| ["C","D"],"C" :| ["D"],"D" :| []]
```

### `Data.Bitraversable.{firstA,secondA}`

* [Extend Data.Bitraversable API with firstA and secondA · Issue #172 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/172)

```haskell
module Data.Bitraversable where

firstA :: (Bitraversable t, Applicative f) => (a -> f c) -> t a b -> f (t c b)
secondA :: (Bitraversable t, Applicative f) => (b -> f c) -> t a b -> f (t a b)
```

`Bitraversable` は要素型が2つある `Traversable` みたいなやつです（たぶん）。標準ライブラリーの中では `Either` やタプル `(,)` がインスタンスとなります。

`Bitraversable` は

```haskell
bitraverse :: (Bitraversable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
```

というメソッドを持っており、これを特殊化したものが今回追加された `firstA` と `secondA` と言って良さそうです。

# おまけ：私の貢献

私（@mod_poppo）が行なった貢献（バグ報告や修正など）で、GHC 9.12に入るものを備忘録代わりに書いておきます。x86 NCGにSIMDを実装するやつに感化された活動がちょいちょいあります。

* プリプロセスされるアセンブリソース `.S` のinclude pathを他と揃える（5月〜6月） [!12692: Set package include paths when assembling .S files · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12692)
    * `.S` で `#include <ghcconfig.h>` みたいなことができるようになり、前に書いた「[【低レベルHaskell】Haskell (GHC) でもインラインアセンブリに肉薄したい！](https://qiita.com/mod_poppo/items/793fdb08e62591d6f3fb)」みたいなことをする人が恩恵を受けます。
* macOS上でLLVMの検出がうまくいっていなかった件のバグ報告（6月17日） [#24999: LLVM version detection logic in configure doesn't work on macOS · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/24999)
* x86 NCG SIMDのnegateの実装にコメント（6月28日）
    * 0の符号を正しく扱うようにしてもらいました。
* Windows上でのLLVMバックエンド（8月〜9月） [!13183: Fix fltused errors on Windows with LLVM · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13183)
    * 私は調査と解決法の提案をやりました。
* primitive string literalのドキュメント化（9月） [!13220: Document primitive string literals and desugaring of string literals · Merge requests · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13220)
    * 「[Haskellの文字列リテラルはGHCでどのようにコンパイルされるか](https://qiita.com/mod_poppo/items/80c442a1d95471e6ac55)」で調査・説明した内容をGHC公式のドキュメントに書きました。本当は英語ネイティブの人に書いてほしかったところですが、4年間誰もやらなかったので……。
* LLVMバックエンドで `-msse4.2` がうまく動いていなかった（10月）
    * 状況の調査を行いました。
* MultilineStrings拡張とCRLFについて（10月）
    * ProposalがCRLFの挙動に関して不明瞭で、実装されたものも意図しない挙動をしているように思えたので、報告しました。

これらの貢献は趣味として、無償でやっています。私を支援したいと思った方には、Zennでバッジを送る、「だめぽラボ」の同人誌を買う、GitHub Sponsorsで支援するなどの手段があります。

* [同人サークル「だめぽラボ」](https://lab.miz-ar.info/)
* [Sponsor @minoki on GitHub Sponsors](https://github.com/sponsors/minoki)

自分でもGHCに貢献してみたい、という人は「[GHCへの私の貢献2023](https://blog.miz-ar.info/2023/12/my-contributions-to-ghc/)」に書いたことも参考にしてください。まずは[GitLab](https://gitlab.haskell.org/ghc/ghc)を眺めて雰囲気を掴むのが良いでしょうか。アカウント作成はスパム対策の関係で人手での承認が必要なのがトリッキーです。
