---
title: "GHC 9.10の新機能"
emoji: "🍣"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: false
---

GHC 9.10.1が202X年XX月XX日にリリースされました。

この記事では、GHC 9.10の新機能を確認していきます。過去の類似の記事は

* [GHC 9.2の新機能と、GHCの動向2021](ghc-9-2-and-future)
* [GHC 8.10とGHC 9.0の新機能](ghc-8-10-and-9-0)
* [GHC 9.4の新機能](whats-new-in-ghc-9-4)
* [GHC 9.6の新機能](whats-new-in-ghc-9-6)
* [GHC 9.8の新機能](whats-new-in-ghc-9-8)

です。

この記事は網羅的な紹介記事とはなっていません。是非、公式のリリースノート類も参照してください：

* [docs/users_guide/9.10.1-notes.rst · ghc-9.10 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/docs/users_guide/9.10.1-notes.rst)
* [libraries/base/changelog.md · ghc-9.10 · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.10/libraries/base/changelog.md)
* [GHC 9.10.x Migration Guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.10)

# GHC 9.10に入る機能

## GHC2024

以下の拡張が含まれます：

* DataKinds
* DerivingStrategies
* DisambiguateRecordFields
* ExplicitNamespaces
* GADTs
* MonoLocalBinds
* LambdaCase
* RoleAnnotations

GADTsとMonoLocalBindsが入ったのが大きいかもしれません。

GHC 9.10の時点では、デフォルト言語はGHC2021のままです。

## RequiredTypeArguments拡張

「必須の型引数」です。"Visible forall in types of terms" とも呼ばれます。

従来のHaskellでは、関数に「型だけ」を渡したいときは、

* ダミーの引数を渡す
* `Proxy` で渡す
* `AllowAmbiguousTypes` と `TypeApplications` 拡張を組み合わせる

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
* パターンの変数は型として使える（？）
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

注意点として、`[Int]` や `(Int, String)` みたいな型をそのまま渡すと、項レベルのリストや項レベルのタプルを型に昇格したものと解釈されます（型レベルで書くと `'[Int]` とか `'(Int, String)` だったやつ）。リスト型やタプル型を渡したい場合は、ExplicitNamespaces拡張を使って `(type ...)` と書くか、新しく導入された型エイリアスを使って `List Int` や `Tuple2 Int String` と書く必要があります。

残念ながら、型クラスのメソッドでは「自身の型」を `forall ->` で受け取ることはできません。

```haskell
class NewStorable a where
  sizeOf :: forall a -> Int -- できない（NewStorable のインスタンスの a と引数の a は別物になる）
```

型の等式 `~` を使ったHackは思いつきましたが、どうでしょうか（Coreレベルでは同一にならないかもしれない）。

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

## 線形なlet/where束縛

## forallのキーワード化

## JavaScriptバックエンド：Cソースのサポート

Emscriptenを使用する

## WebAssemblyバックエンド：JavaScript FFIのサポート

## ライブラリーの変化

* `foldl'` がPreludeからエクスポートされる
* `List` 型がData.Listからエクスポートされる

## その他
