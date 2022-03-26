---
title: "応用：Constraintカインド"
---

現代のGHCでは、ConstraintKinds拡張を有効にすることにより、型クラスや型の等価性 `~` などの**制約**も型の一種として扱うことができます。制約は `Constraint` カインド（`Data.Kind` モジュールより）を持ちます。

GHCiで確認してみましょう。

```haskell
ghci> :set -XConstraintKinds -XNoStarIsType
ghci> :kind Show
Show :: Type -> Constraint
ghci> :kind Monad
Monad :: (Type -> Type) -> Constraint
ghci> :kind (~)
(~) :: k -> k -> Constraint
```

`Show` クラスは `Type -> Constraint` というカインドを、`Monad` クラスは `(Type -> Type) -> Constraint` というカインドを、型の等価性 `(~)` は `k -> k -> Constraint` という（多相な）カインドを持つことがわかります。

ConstraintKinds拡張のわかりやすいご利益として、`type` 宣言により制約のエイリアスを作ることができます。

```haskell
type ReadAndShow a = (Read a, Show a)
```

カインド `Constraint` を持つ型は `=>` の左側に置くことができ、逆に `=>` の左側に置かれたもののカインドは `Constraint` と推論されます。

```haskell
ghci> type Foo a = a => Int
ghci> :kind Foo
Foo :: Constraint -> Type
```

[constraintsパッケージ](https://hackage.haskell.org/package/constraints)は、ConstraintKinds拡張を活用するためのデータ型等を提供しています。

```haskell
module Data.Constraint where

type Dict :: Constraint -> Type
data Dict where
  Dict :: a => Dict a

type (:-) :: Constraint -> Constraint -> Type
newtype a :- b = Sub (a => Dict b)
```

型 `Dict a` は制約 `a` をデータ型として封じ込めたものです。データ型 `a :~: b` は制約 `a ~ b` を封じ込めたものでしたが、 `Dict` はそれを任意の制約に一般化したものだと思えます。

型 `a :- b` は「制約 `a` は制約 `b` を含意する」ことを表します。
