---
title: "GHC 9.10の新機能"
emoji: "🍣"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

GHC 9.10.1が2024年5月11日にリリースされました。

* [GHC 9.10.1 is now available! - Announcements - Haskell Community](https://discourse.haskell.org/t/ghc-9-10-1-is-now-available/9523)

この記事では、GHC 9.10の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)
* [GHC 9.6の新機能](whats-new-in-ghc-9-6)
* [GHC 9.8の新機能](whats-new-in-ghc-9-8)

です。

この記事は網羅的な紹介記事とはなっていません。是非、公式のリリースノート類も参照してください：

* [2.1. Version 9.10.1 — Glasgow Haskell Compiler 9.10.1 User's Guide](https://downloads.haskell.org/ghc/9.10.1/docs/users_guide/9.10.1-notes.html)
    * [docs/users_guide/9.10.1-notes.rst · ghc-9.10 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/docs/users_guide/9.10.1-notes.rst)
* [libraries/base/changelog.md · ghc-9.10 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/libraries/base/changelog.md)
* [GHC 9.10.x Migration Guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.10)

# GHC 9.10に入る機能

## GHC2024

* [ghc-proposals/proposals/0613-ghc2024.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0613-ghc2024.rst)

以下の拡張が含まれます：

* DataKinds
* DerivingStrategies
* DisambiguateRecordFields
* ExplicitNamespaces
* GADTs
* MonoLocalBinds
* LambdaCase
* RoleAnnotations

GHC 9.10の時点では、デフォルト言語はGHC2021のままです。

GHC2024で有効になる拡張の中だとMonoLocalBindsは要注意です。例えば、以下のコードはMonoLocalBindsのせいでコンパイルが通りません：

```haskell
{-# LANGUAGE GHC2024 #-}
import Control.Monad.ST
import Data.STRef

foo :: Int -> Int -> Int
foo a b = runST action
  where
    -- action :: ST s Int の s が多相にならない
    action = do
      counter <- newSTRef a
      modifySTRef' counter (+ b)
      readSTRef counter

main = print (foo 3 5)
```

## RequiredTypeArguments拡張

* [ghc-proposals/proposals/0281-visible-forall.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)

「必須の型引数」です。"Visible forall in types of terms" とも呼ばれます。

従来のHaskellでは、関数に「型だけ」を渡したいときは、

* ダミーの引数を渡す
* `Proxy` で渡す
* AllowAmbiguousTypesとTypeApplications拡張を組み合わせる

のいずれかを利用する必要がありました。コードで書けばそれぞれ次のようになります。

```haskell
sizeOf :: Storable a => a -> Int
sizeOfProxy :: Storable a => Proxy a -> Int
sizeOfTypeApp :: Storable a => Int -- 要 AllowAmbiguousTypes

main = do
  print $ sizeOf (undefined :: Int)
  print $ sizeOfProxy (Proxy :: Proxy Int)
  print $ sizeOfTypeApp @Int -- 要 TypeApplications
```

今回、新たな方法が追加されました。特徴は

* `forall a ->` という形の量化子を使う（これは型／カインドのレベルではすでに使えるようになっていました）
* 引数はそのまま（`@` なしで）型名を書ける（`type` 構文で明示することもできる）

ことです。

```haskell
-- 要 RequiredTypeArguments
sizeOfRTA :: forall a -> Storable a => Int
sizeOfRTA a = sizeOf (undefined :: a)

main = do
  print $ sizeOfRTA Int
  print $ sizeOfRTA (type Int) -- 要 ExplicitNamespaces
```

注意点として、`[Int]` や `(Int, String)` みたいな型をそのまま渡すと、項レベルのリストや項レベルのタプルを型に昇格したもの（型レベルリスト、型レベルタプル）と解釈されます（型レベルで書くと `'[Int]` とか `'(Int, String)` だったやつ）。リスト型やタプル型を渡したい場合は、ExplicitNamespaces拡張を使って `(type ...)` と書くか、新しく導入された型エイリアスを使って `List Int` や `Tuple2 Int String` と書く必要があります。

残念ながら、型クラスのメソッドでは「自身の型」を `forall ->` で受け取ることはできません。

```haskell
class NewStorable a where
  sizeOf :: forall a -> Int -- できない（NewStorable のインスタンスの a と引数の a は別物になる）
```

型の等式 `~` を使ったHackは思いつきましたが、どうでしょうか（Coreレベルでは同一にならないかもしれませんが）。

```haskell
class NewStorable2 a where
  sizeOf :: forall a' -> a ~ a' => Int
```

おまけですが、RequiredTypeArgumentsを使うと型注釈を新たな方法で書けるようになります：

```haskell
as :: a -> forall a' -> a ~ a' => a
as x _ = x

main = print (42 `as` Integer)
```

追記：より詳しい記事を書いてみました→[GHC 9.10で実装された可視なforallで遊ぶ](https://zenn.dev/mod_poppo/articles/playing-with-visible-forall)

## 線形なlet/where束縛

LinearTypes拡張の下で、線形な `let` あるいは `where` 束縛が使えるようになります。例えば、以下のコードのコンパイルが通ります：

```haskell
{-# LANGUAGE LinearTypes #-}

f :: Int %1 -> Int
f x = let y = x
      in y

main = print (f 42)
```

## forallのキーワード化

これまでは `forall` を項レベルの変数名として使うことができましたが、GHC 9.10ではエラーになるようになりました。型レベルでは以前からキーワード扱いだったので、これで `forall` は常にキーワード扱いされるようになります。

## fixity宣言やWARNING/DEPRECATEDプラグマにおいて型と項の名前空間を指定できる

* [ghc-proposals/proposals/0065-type-infix.rst at master · ghc-proposals/ghc-proposals](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0065-type-infix.rst)

型レベルと項レベルに同じ名前があるとき、`infix` 宣言や `WARNING`/`DEPRECATED` プラグマにおいて、型と項の片方だけを指定できます。

```haskell
{-# LANGUAGE TypeOperators #-}

-- 型レベルの $$
type f $$ x = f x

-- 項レベルの $$
f $$ x = f x

infixr 9 type $$
infixr 0 data $$

main :: IO $$ () -- ここでの $$ は infixr 9
main = putStrLn $$ "Hello " ++ "world" -- ここでの $$ は infixr 0
```

## NoListTuplePuns拡張

これまでは `Int` を要素とするリストの型は `[Int]`、`Int` と `String` からなるタプルの型は `(Int, String)` と書いていましたが、NoListTuplePuns拡張を使うとこれらの記法を無効化できます。

リスト型を書きたいときは `Data.List` からエクスポートされる `List` 型を使って `List Int` と、2要素のタプル型を書きたいときは `GHC.Tuple` あるいは `Prelude.Experimental` からエクスポートされる `Tuple2` 型を使って `Tuple2 Int String` という風に書くことができます。

## 関数定義における不可視の型束縛

TypeAbstractions拡張（GHC 9.8で導入）が拡張され、項レベルの関数を定義するときにも使えるようになりました。

```haskell
{-# LANGUAGE TypeAbstractions #-}

id :: forall a. a -> a
id @a x = x :: a
```

## JavaScriptバックエンド：Cソースのサポート

JavaScriptバックエンドが、Cソースをサポートするようになりました。CソースはEmscriptenによってWasmにコンパイルされます。

ちなみに、最近のGHCupはJavaScript向けGHCのインストールに実験的に対応しているようです：[GHC JS cross bindists (experimental)](https://www.haskell.org/ghcup/guide/#ghc-js-cross-bindists-experimental)

## WebAssemblyバックエンド：JavaScript FFIのサポート

* [15.5. JavaScript FFI in the wasm backend](https://downloads.haskell.org/ghc/9.10.1-alpha1/docs/users_guide/wasm.html#wasm-jsffi)

WebAssemblyバックエンドが、JavaScript FFIをサポートするようになりました。詳しくはドキュメントを参照してください。

ちなみに、最近のGHCupはWasm向けGHCのインストールに実験的に対応しているようです：[GHC WASM cross bindists (experimental)](https://www.haskell.org/ghcup/guide/#ghc-js-cross-bindists-experimental)

## ghc-internalパッケージとghc-experimentalパッケージ

* [tech-proposals/proposals/accepted/051-ghc-base-libraries.rst at main · haskellfoundation/tech-proposals](https://github.com/haskellfoundation/tech-proposals/blob/main/proposals/accepted/051-ghc-base-libraries.rst)

GHCに付属する基本的なパッケージとしては、`Prelude` を含む標準ライブラリーであるbase、一般のユーザーが直接触ることは基本的にありませんがGHCのプリミティブを含む[^ghc-prim]ghc-prim、多倍長整数の実装を含むghc-bignumなどがありました。

[^ghc-prim]: GHCのプリミティブを触りたい場合はghc-primパッケージの `GHC.Prim` モジュールではなく、baseパッケージの `GHC.Exts` モジュールを利用するべきです。

今回、新たにghc-internalパッケージとghc-experimentalパッケージが追加されます。ghc-internalパッケージはGHCの内部的な機能を、ghc-experimentalパッケージは将来baseに入るかもしれない実験的な機能を含みます。

現在のところ、ghc-experimentalパッケージは `Tuple2` などをエクスポートする `Data.Tuple.Experimental` や `Prelude.Experimental` などのモジュールを含みます。

## ライブラリーの変化

* `foldl'` が `Prelude` からエクスポートされます。
* `List` 型が `Data.List` からエクスポートされます。
