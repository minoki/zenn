---
title: "GHC 8.10とGHC 9.0の新機能"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

[Haskell Day 2021](https://haskell.jp/haskell-day-2021/)で「GHCの動向2021」というタイトルで発表しました、mod\_poppoです。先日、

* [GHC 9.2の新機能と、GHCの動向2021](https://zenn.dev/mod_poppo/articles/ghc-9-2-and-future)

という記事を書きましたが、GHC 8.10とGHC 9.0の新機能も紹介する価値があると思ったので、ここでまとめてみます。

先日の記事もですが、筆者の興味の方向性には偏りがあるため、あまり紹介できていない分野もあります。Template HaskellとかIDE関連とか。

# GHC 8.10の新機能

GHC 8.10.1は2020年3月にリリースされ、公式のリリースノートは

* [3. Release notes for version 8.10.1 — Glasgow Haskell Compiler 8.10.1 User's Guide](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html)

です。

## `UnliftedNewtypes` 拡張

* [ghc-proposals/0098-unlifted-newtypes.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0098-unlifted-newtypes.rst)

unliftedなデータ型についてはGHC 9.2の紹介記事に書いたのでそっちを見てください。

この拡張ではunliftedな型のnewtypeを作れるようになります。

例として、これまではカインド `TYPE 'IntRep` を持つ型は `Int#` だけでしたが、この拡張を使うと

```haskell
Prelude> :m + GHC.Exts
Prelude GHC.Exts> :set -XUnliftedNewtypes -XMagicHash
Prelude GHC.Exts> newtype I = I Int#
Prelude GHC.Exts> :kind I
I :: TYPE 'IntRep
```

という風に「カインド `TYPE 'IntRep` を持つが `Int#` とは異なる型」を作れるようになります。

これに合わせて `Data.Coerce` モジュールの `coerce` もunliftedな型を扱えるように拡張されています。型は

```haskell
coerce :: forall {k :: RuntimeRep} (a :: TYPE k) (b :: TYPE k). Coercible a b => a -> b
```

みたいな感じになります。

## `StandaloneKindSignatures` 拡張

* [ghc-proposals/0054-kind-signatures.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0054-kind-signatures.rst)

Haskellの特徴的な文法の一つは、

```haskell
foo :: Int -> String -> IO ()
foo = ...
```

という風に型注釈を本体とは別の行に書けることです。しかし、これまでは型のカインド注釈を別の行に書くことはできませんでした。例えば、型引数 `a` を取る型定義で `a` のカインドが `Type -> Type` であることを明示したければ

```haskell
data Foo (a :: Type -> Type) = ...
```

と書く必要がありました。

新たに導入された `StandaloneKindSignatures` 拡張を使うと、

```haskell
type Foo :: (Type -> Type) -> Type
data Foo a = ...
```

という風にカインド注釈にまるまる一行使うことができます。

この機能は `data` の他に、 `type` (`family`), `newtype`, `class` について使うことができます。

`StandaloneKindSignatures` を使うと型レベルの多相再帰ができるらしいです。

この拡張は、GHC 9.2以降で使える `GHC2021` の一部となっています。

## `ImportQualifiedPost` 拡張

```haskell
import qualified Data.Vector as V
```

の代わりに

```haskell
import Data.Vector qualified as V
```

と書けるようになります。

この拡張は、GHC 9.2以降で使える `GHC2021` の一部となっています。

## 低レイテンシーGC

* [Talk: A low-latency garbage collector - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2018/11/nonmoving-gc-talk/)
* [Low-latency garbage collector merged for GHC 8.10 - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2019/10/nonmoving-gc-merge/)

GHCのGCは世代別コピーGCを採用しており、リアルタイム性が必要ないプログラムの場合はこれはうまく動きます。しかし、リアルタイム性が必要なプログラムの場合は、GCによる停止時間（いわゆるStop the World）が無視できない問題となります（…らしいです。筆者はそういうプログラムを書かないので受け売りですが…）。

GHC 8.10では、GCによる停止時間の問題を軽減するために、新たに

* 並行に動作する (concurrent)
* 非ムーブ (non-moving)
* マーク＆スイープ (mark-and-sweep)

GCを導入します。このGCは古い世代に対して適用され、若い世代に対しては従来のコピーGCが使われます。

この新しいハイブリッドなGCはデフォルトでは無効で、 `--nonmoving-gc` RTSオプション（`./myprogram +RTS --nonmoving-gc` という風に使う）で有効になります。

この新しいマーク＆スイープGCは東北大の上野、大堀らの論文を参考にしているそうです。

## カインド変数の `forall` に関する特別扱いをやめる

* [ghc-proposals/0103-no-kind-vars.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0103-no-kind-vars.rst)

GHCでのカインド変数は元々は型変数と完全に区別されていましたが、徐々に共通化されていきました。その中で最後まで残った特別扱いが `forall` に関するものです。

まず、型変数の `forall` に関しては、「全て列挙するか、全く書かない (forall-or-nothing)」規則があります。これは、型注釈に `forall` を使う場合、登場する型変数全てを量化しなければならない、という規則です。コード例を挙げます：

```haskell
foo :: a -> b -> a  -- OK; 暗黙に全称量化される
bar :: forall a b. a -> b -> a  -- OK; 全ての変数を量化している
baz :: forall a. a -> b -> a  -- エラー；forallが登場しているにもかかわらずbが量化されていない
```

しかし、GHC 8.8までのカインド変数にはこの規則が当てはまらず、次のようなコードを書くことができました：

```haskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
import Data.Proxy

foo :: forall (a :: k). Proxy a -> ()  -- forallが登場しているが、kは量化されていない
foo _ = ()
```

GHC 8.10ではこの特別扱いを廃止し、 `forall` を使う場合にカインド変数の束縛を必須にします。

```haskell
-- GHC 8.10以降
foo :: forall (a :: k). Proxy a -> ()  -- エラー：kが束縛されていない
bar :: forall k (a :: k). Proxy a -> ()  -- OK
```

## `forall k ->`: 可視な依存量化子 (visible, dependent quantifiers)

* [ghc-proposals/0081-forall-arrow.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0081-forall-arrow.rst)

GHC 8.0以降ではカインド注釈がそれ以前に登場した型レベルの変数に依存することができます。例えば

```haskell
{-# LANGUAGE PolyKinds #-}
data T k (a :: k)
```

という型定義では、2番目の型引数のカインドが1番目の型引数に依存しています。ではこの `T` のカインドはどうなるでしょうか？

実際にGHCiで確認すると、

```haskell
Prelude> :set -XPolyKinds
Prelude> data T k (a :: k) = MkT
Prelude> :kind T
T :: forall k -> k -> *
```

という風に `forall k ->` という謎の表記が登場していました。これは

* `->` 以下が束縛された変数に依存できる（通常の `forall` と同じ）
* 型引数として明示的に与える必要がある（通常の型引数と同じ）

という2つの性質を持ちます。

GHC 8.8までは `forall k ->` はGHCiの `:kind` で見られるだけで、実際にソースコード中に書くことはできなかったのですが、GHC 8.10では、 `forall k ->` をソースコード中に書くことができるようになりました。

```haskell
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE PolyKinds #-}
import Data.Kind

type T :: forall k -> k -> Type
data T k a = MkT -- aのカインドはk
```

## `-Wno-deriving-defaults`

derivingに関する拡張、 `DeriveAnyClass` と `GeneralizedNewtypeDerivng` はバッティングします。

```haskell
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

newtype U = U Int deriving (Show, Num) -- Num はどうやって導出される？

main = print (U 0 - U 0)
```

この際、GHCは警告メッセージを出します。この動作は以前からです。

GHC 8.8まで：

```
DerivingTest.hs:3:35: warning:
    • Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled
      Defaulting to the DeriveAnyClass strategy for instantiating Num
      Use DerivingStrategies to pick a different strategy
    • In the newtype declaration for ‘U’
  |
3 | newtype U = U Int deriving (Show, Num) -- Num はどうやって導出される？
  |                                   ^^^
```

GHC 8.10以降：

```
DerivingTest.hs:3:35: warning: [-Wderiving-defaults]
    • Both DeriveAnyClass and GeneralizedNewtypeDeriving are enabled
      Defaulting to the DeriveAnyClass strategy for instantiating Num
      Use DerivingStrategies to pick a different strategy
    • In the newtype declaration for ‘U’
  |
3 | newtype U = U Int deriving (Show, Num) -- Num はどうやって導出される？
  |                                   ^^^
```

何が変わったかというと、警告にderiving-defaultsという名前がついたこと、それによってこの警告を抑制することができるようになった (`-Wno-deriving-defaults`) ことです。

この辺のderivingにまつわる話は以下の記事も参照してください：

* [独断と偏見で語るGHCのderiving系拡張](https://qiita.com/mod_poppo/items/1867d06f8a9903441e3c)

## GHCiで `UnboxedTuples` や `UnboxedSums` が使用された時に自動で `-fobject-code` が有効になる

見出しの通りです。

## `bitReverse`

命令セットによっては符号なし整数のビット列の反転の命令を持っていることがあります（Armとか）。それに対応する組み込み関数が追加されました。

`Data.Word` モジュールの

```haskell
bitReverse8 :: Word8 -> Word8
bitReverse16 :: Word16 -> Word16
bitReverse32 :: Word32 -> Word32
bitReverse64 :: Word64 -> Word64
```

という形で使えます。

GHC 9.2の時点では、どのNCGもこれを専用の命令に落とすことはせず、Cで書かれた実装を呼び出す形になっています。LLVM backendを使えばLLVMの `llvm.bitreverse.*` 組み込み関数に変換されるのでターゲットがそういう命令を持っていればそれにコンパイルされる可能性はあります。

# GHC 9.0の新機能

GHC 9.0.1は2021年2月にリリースされ、公式のリリースノートは

* [2.1. Version 9.0.1 — Glasgow Haskell Compiler 9.0.1 User's Guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html)

です。

## `LinearTypes` 拡張

* [ghc-proposals/0111-linear-types.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst)
* [HaskellのLinearTypes言語拡張について少し調べた - TSUGULOG](https://yoshitsugu.net/posts/2021-02-13-linear-haskell.html)
* [線形型の刹那的不変データ構造への利用 (Haskell Day 2021)](http://slide.kakkun61.com/linear-ephemeral-data-structure/)

線形型の登場です。詳しい説明は他の記事やスライドに譲ります。

`LinearTypes` 拡張の下で型について `a %m -> b` という構文が追加されます。通常の `->` と `%m ->` はそれぞれ次のように脱糖されます：

```haskell
a -> b     ~~>  a %'Many -> b
a %1 -> b  ~~>  a %'One -> b
a %m -> b  ~~>  FUN m a b
```

`FUN` とか `Many` とか `One` は次のように定義されています：

```haskell
-- GHC.Exts
type FUN :: forall (n :: Multiplicity) -> forall {q :: RuntimeRep} {r :: RuntimeRep}. TYPE q -> TYPE r -> Type
data FUN m a b

-- GHC.Types (ghc-prim)
data Multiplicity = Many | One
type MultMul :: Multiplicity -> Multiplicity -> Multiplicity
```

なお、GHC 9.0.1のalpha版までは `%1 ->` ではなく `#->` という構文でした。古い記事等を読む際は注意してください。

## `QualifiedDo` 拡張

* [ghc-proposals/0216-qualified-do.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst)
* [Qualified do: rebind your do-notation the right way - Tweag](https://www.tweag.io/blog/2020-07-13-qualified-do-announcement/)

世の中にはモナドっぽいけど `Monad` クラスのインスタンスにはならないやつがあります。indexed monadやgraded monadというやつや、線形型版のモナドです、

そういう「モナドっぽいけど `Monad` クラスのインスタンスにならないやつ」でdo構文を使いたいと思ったら、従来は `RebindableSyntax` 拡張を使うしかありませんでした。しかし、`RebindableSyntax` は影響する構文が多く、単に「do構文を置き換えたい」時に使うには大掛かりすぎました。

`QualifiedDo` 拡張を使うと、個々のdo構文について脱糖方法を選ぶことができるようになります。

コードの書き方としては、`do` をモジュール名で修飾します。モジュール名が `SugoiMonad` であれば `SugoiMonad.do` です。こうすることで

* `x <- u` の脱糖には `SugoiMonad.>>=` が、
* `u; ...` の脱糖には `SugoiMonad.>>` が、
* 失敗しうるパターンマッチ `pat <- u` の脱糖には `SugoiMonad.fail` が

それぞれ使われるようになります。`ApplicativeDo` 拡張が有効な場合は `SugoiMonad.<$>`, `SugoiMonad.<*>`, `SugoiMonad.join`, `SugoiMonad.return` も使われるようになり、`RecursiveDo` 拡張が有効であれば `SugoiMonad.mfix` と `SugoiMonad.return` も使われるようになります。

注意しなければならないのは、ソース中に記述する `>>=` や `return` が勝手にモジュール名で修飾されることはないということです。つまり、

```haskell
SugoiMonad.do
  x <- foo >>= bar
  return (baz x)
```

ではなく

```haskell
SugoiMonad.do
  x <- foo SugoiMonad.>>= bar
  SugoiMonad.return (baz x)
```

と記述することになります。

## `LexicalNegation` 拡張と `NegativeLiterals` の変更

* [ghc-proposals/0344-negative-literals-improved.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0344-negative-literals-improved.rst)
* [6.2.21. Lexical negation — Glasgow Haskell Compiler 9.0.1 User's Guide](https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/lexical_negation.html)

Haskellでは `(演算子 オペランド)` と書くことによって `\x -> x 演算子 オペランド` と同等の関数を得ることができます（セクション）。……演算子がマイナス `-` でなければ。演算子がマイナスの場合は単項マイナスとして解釈されます。

従来のHaskellでの解決方法（減算のセクションを書く方法）は `(- オペランド)` の代わりに `subtract オペランド` と書くことでしたが、`LexicalNegation` は別の方法を提供します。

`LexicalNegation` 拡張が有効な場合、マイナス記号 `-` の前後の空白の有無によって二項演算と単項演算が切り替わります。

```haskell
x-y   -- 二項演算
x- y  -- 二項演算
x -y  -- 単項マイナス：x (-y)
x - y -- 二項演算
```

```haskell
(- x) -- セクション：\y -> y - x
(-x)  -- 単項マイナス：negate x
```

また、単項マイナスは他の二項演算子よりも強く結合します。

GHC 9.0では `NegativeLiterals` 拡張にも変更が入り、`NegativeLiterals` 拡張の下でも `x-1` が `x (-1)` ではなく `x - 1` と解釈されるようになりました。詳しい規則はGHC proposalを見てください。

## Simplified Subsumption

* [ghc-proposals/0287-simplify-subsumption.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst)

従来は

```haskell
foo :: forall x y. x -> y -> Int
bar :: forall x. x -> forall y. y -> Int
```

という `forall` の位置が異なる二つの関数は（型適用の文脈を除き）区別されませんでした。例えば、

```haskell
f :: (forall x y. x -> y -> Int) -> Int
```

に対して `f bar` はコンパイルが通りました。これは暗黙のη変換が行われていたためです。

```haskell
f bar ~~> f (\x y -> bar x y)
```

しかし、GHC 9.0以降では型変数の順番や位置は区別されるようになります。これによって、手動でη変換が必要になる場面が増えます。

## `forall {a}.`

多相関数に対してはいつでも `TypeApplications` を使えるわけではなく、関数の定義の仕方によっては使えないこともあるという話は

* [リテラルにTypeApplicationsを使えない理由とその対策、あるいはTypeApplicationsの注意点](https://qiita.com/mod_poppo/items/478846822828da57fa33)

に書きました。型適用できない型変数を持つ関数は、GHCiで `:set -fprint-explicit-foralls` した状態で型を表示させると

```haskell
ghci> :set -fprint-explicit-foralls
ghci> foo x y = x + y
ghci> :type foo
foo :: forall {a}. Num a => a -> a -> a
```

という風に型変数が波括弧 `{}` で括られた状態で表示されます。

GHC 9.0では、ソースコード中に書く `forall` の型変数についても波括弧を書くことができるようになります。

```haskell
{-# LANGUAGE ExplicitForAll #-}

foo :: forall {a} b. a -> b -> a
foo x y = x
-- foo @Int と型適用すると b=Int となる
```

```haskell
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PolyKinds #-}

data MyProxy a where
  MyProxy :: forall {k} (a :: k). MyProxy a
-- MyProxy @Int と型適用すると a=Int となる
```

## WinIO

* [MuniHac 2020: Tamar Christina - The new Windows I/O manager (WinIO) in GHC - YouTube](https://www.youtube.com/watch?v=kgNh5mdZ1xw)

Windows向けの新しいIOマネージャーが登場しました。9.0の段階ではプレビュー版という位置付けで、デフォルトではOFFで、RTSオプション `--io-manager=native` （`myprogram.exe +RTS --io-manager=native` という風に使う）で有効になります。

従来のIOマネージャーとWinIOの一つの違いとして、コンソールの入出力にワイド版APIを使うというものがあるようです。つまり、コンソールのコードページが65001じゃなくてもUnicode文字を読み書きできます：

```
> cat hello.hs
main = putStrLn "\x1F97A"
> chcp
現在のコード ページ: 932
> ghc-8.10.7 -fforce-recomp hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello.exe ...
> .\hello.exe
hello.exe: <stdout>: commitBuffer: invalid argument (invalid character)
> ghc-9.0.1 -rtsopts -fforce-recomp hello.hs
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello.exe ...
> .\hello.exe
hello.exe: <stdout>: commitBuffer: invalid argument (invalid character)
> .\hello.exe +RTS --io-manager=native
🥺
```

## 多倍長整数型の新たな実装：ghc-bignum

従来のGHCでは、多倍長整数（`Integer` 型と `Natural` 型）の実装は

* GMPによる実装 integer-gmp
* 純Haskellな実装 integer-simple

の2種類あり、そのどちらかが利用されるようになっていました。しかし、この体制にはいくつかの問題点がありました：

1. integer-simpleの効率が良くない（`[Word]` っぽい連結リストによる表現）
2. 一般のパッケージが `Integer` 型の内部表現にアクセスするのが面倒
    * integer-gmpとintege-simpleで `Integer` 型の内部表現が違う（のでCPPで切り替える羽目になる）。依存するべきパッケージも違う（のでpackage flagで切り替える羽目になる）。
3. 新たなバックエンド（例：GMPよりもライセンスの緩いOpenSSL）を増やしづらい

```haskell
-- integer-gmpでの内部表現
data Integer = S# !Int#    -- 1ワードに収まる値
             | Jp# !BigNat -- 正の多倍長
             | Jn# !BigNat -- 負の多倍長

-- integer-simpleでの内部表現
data Integer = Positive !Positive
             | Negative !Positive
             | Naught
```

そこで登場したのがghc-bignumパッケージです。ghc-bignumパッケージは `Integer` 型と `Natural` 型の内部表現がそれぞれ統合されます。

```haskell
-- ghc-bignum
data Integer = IS !Int#    -- integer-gmpのS#と同等
             | IP !BigNat# -- integer-gmpのJp#と同等
             | IN !BigNat# -- integer-gmpのJn#と同等
```

演算の実装は従来通りGMPを使うものと純Haskellなもの（こちらはnativeと呼ばれる）が提供されますが、純Haskellな実装はinteger-simpleよりも高速なことが期待されます。

なお、これまで `Integer` 型の内部表現に直接アクセスしていたパッケージとの互換性のために、integer-gmpパッケージも引き続き提供されます（`PatternSynonyms` を使って `S#`, `Jp#`, `Jn#` を提供する）。

## 32ビットWindowsのサポートの削除、Windows Vistaのサポートの削除

見出しの通りです。

[`winapi` 擬似呼び出し規約](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0277-winapi-callconv.rst)は実装されないまま32ビットWindowsのサポートが終了してしまいました。残念。

なお32ビットWindows向けの公式のバイナリーがリリースされたのは8.6.5が最後のようです。それより新しいGHCを32ビットWindowsで動かしたかったら、自前でビルドする必要がありそうです。

* <https://downloads.haskell.org/~ghc/8.6.5/>

## GHCiのプロンプトが変わった

これまではGHCiでは `Prelude>` という風にスコープにあるモジュール名がプロンプトに表示されていましたが、GHC 9.0ではプロンプトが単に `ghci>` になります。

スコープにあるモジュール一覧が見たい場合は `:show imports` を実行してください。

従来のプロンプトが良かった、という場合は

```
:set prompt "%s> "
:set prompt-cont "%s| "
```

を実行する（あるいは `.ghci` に記述する）と良いでしょう。

## unboxed文字列リテラルの長さ：`cstringLength#`

unboxed文字列リテラルについては

* [Haskellの文字列リテラルはGHCでどのようにコンパイルされるか](https://qiita.com/mod_poppo/items/80c442a1d95471e6ac55)

を参照してください。要は暗黙にNUL終端されたバイト列のリテラルです。

GHC 9.0では `GHC.CString` / `GHC.Exts` モジュールに `cstringLength# :: Addr# -> Int#` という関数が追加されました、これはCの `strlen` 関数と同等で、最初のNULまでのバイト数を返します。

重要なのは、この関数はコンパイラーによって定数畳み込みの対象になることです。これによって、ユーザー定義の文字列型をリテラルから構築する際に実行時にバイト数を数えなくて良くなります。

## `touch#` と `keepAlive#`

* [The tale of keepAlive# - Well-Typed: The Haskell Consultants](https://well-typed.com/blog/2021/06/keepAlive-story/)
* [Rethinking touch# primop (#17760) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/17760)

HaskellのGCで管理するオブジェクトをC FFIで使う場合、FFI呼び出し中にHaskellのGCが発動してオブジェクトが解放されてしまうと困ります。FFI呼び出し中に解放されないようにするためには、従来は `touch` 関数をFFI呼び出しの後に入れていました。

```haskell
do arr <- newPinnedByteArray len
   ...arrのアドレスをC FFIで使う...
   touch arr -- FFI中にarrが解放されないためのおまじない
```

しかし、FFIの部分で無限ループしたり例外を投げている場合、GHCは `touch` の呼び出しに制御が到達しないと判定し、コードを削除してしまうことがあります（dead-continuation eliminationというらしいです）。そうするとFFIで使っているメモリー領域がGCによって改修されて厄介な問題を引き起こす可能性があります。

この問題に対しては、従来は無限ループと判定されうる部分を `NOINLINE` でマークするというような緩和策が取られてきましたが、GHC 9.0では抜本的な解決策として新たなプリミティブが導入されました：

```haskell
keepAlive# :: forall (q :: RuntimeRep) (a :: TYPE q) (r :: RuntimeRep) (b :: TYPE r).
              a -> State# RealWorld -> (State# RealWorld -> b) -> b
```

GHC 9.0以降の `withForeignPtr` ではこの `keepAlive#` が使われています。

`keepAlive#` は従来の `touch#` プリミティブを完全に置き換えるわけではなく、従来の `touch#` プリミティブも引き続き残されています（プログラマーの責任で使用）。

## `Data.List.singleton` と `Data.List.NonEmpty.singleton`

一個の要素からなるリストを返す関数が追加されました。これまでは `(:[])` みたいなパターンを使う方が多かったと思います。

```haskell
-- Data.List
singleton :: a -> [a]

-- Data.List.NonEmpty
singleton :: a -> NonEmpty a
```

`Data.List` に `singleton` を追加する議論から派生して `Data.List` の単相化の話が持ち上がるわけですが、それはまた別の話。
