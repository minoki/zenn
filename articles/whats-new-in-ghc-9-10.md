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

DataKinds, DerivingStrategies, DisambiguateRecordFields, ExplicitNamespaces, GADTs, MonoLocalBinds, LambdaCase, RoleAnnotations

GADTsとMonoLocalBindsが入ったのが大きいです。

現時点では、デフォルト言語はGHC2021のままです。

## RequiredTypeArguments拡張

"Visible forall in types of terms" とも呼ばれます。

ExplicitNamespaces拡張

従来のHaskellでは、関数に「型だけ」を渡したいときは、

* ダミーの引数を渡す
* `Proxy` で渡す
* `AllowAmbiguousTypes` と `TypeApplications` 拡張を組み合わせる

のいずれかを利用する必要がありました。コードで書けばそれぞれ次のようになります。

```haskell
sizeOf :: Storable a => a -> Int
sizeOfProxy :: Storable a => Proxy a -> Int
sizeOfTypeApp :: Storable a => Int

main = do
  print $ sizeOf (undefined :: Int)
  print $ sizeOfProxy (Proxy :: Proxy Int)
  print $ sizeOfTypeApp @Int
```

今回、新たな方法が追加されました。

```haskell
sizeOfRTA :: forall a -> Storable a => Int
sizeOfRTA a = sizeOf (undefined :: a)

main = do
  print $ sizeOfRTA Int
  print $ sizeOfRTA (type Int)
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
