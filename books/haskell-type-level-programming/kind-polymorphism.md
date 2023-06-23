---
title: "発展：カインド多相"
---

この節ではカインド多相について扱います。この後の話にはそこまで関係しないと思うので、興味のない方は読み飛ばしても構いません。

# カインド推論とカインド多相

Haskellでは多くの場合、データ型の定義の際にカインドを指定する必要はありません。これは**カインド推論** (kind inference) が行われるためです。

例えば、次の型定義を見てみましょう：

```haskell
data A f a b = A1 a b | A2 (f b)
```

型構築子 `A` の引数 `f`, `a`, `b` のカインドはどうなるでしょうか。制約として、データ構築子のフィールドとして現れる型のカインドは `Type` でなければいけません。`a` と `b` は `A1` のフィールドとして現れるので、カインドについて

```
a :: Type
b :: Type
```

が要請されます。また、 `f` は `b` に適用したものが `A2` のフィールドとして現れるので、

```
f b :: Type
```

で、 `b` のカインドは `Type` だとわかっているので、

```
f :: Type -> Type
```

と推論できます。

よって、型 `A` のカインドは

```
A :: (Type -> Type) -> Type -> Type -> Type
```

と推論できます。

一方で、情報が足りない場合はカインドが完全に決定できないことがあります。次の型定義はどうでしょうか。

```haskell
data B f a b = B1 (f a)
```

この型定義から得られる制約は

```
f a :: Type
```

だけで、`a` と `b` のカインドは決定しません。

こういう場合、2つの選択肢があります。

1. わからなかった部分はデフォルトのカインドとして `Type` を当てはめる。この場合 `f :: Type -> Type`, `a, b :: Type` となる。
2. 多相的なカインドを使い、最も一般的なカインドを当てはめる。この場合、カインド変数 `k1`, `k2` について `f :: k1 -> Type`, `a :: k1`, `b :: k2` となる。

古典的なHaskell (Haskell 2010) では選択肢1が採用されてきました。一方で、GHC拡張PolyKindsを有効にした場合（言語GHC2021により暗黙に有効化される場合を含む）は、選択肢2が採用されます。

前の節で `Proxy` 型のカインドを `k -> Type` と説明しましたが、これは `Proxy` 型を定義する際にPolyKinds拡張が有効になっていたことを意味します。

```haskell
{-# LANGUAGE PolyKinds #-}

data Proxy a = Proxy
```

一方で、自分で同様の型を定義する際は、PolyKinds拡張の有無によって推論されるカインドが変わります。PolyKinds拡張を無効にした場合は

```
ghci> :set -XNoPolyKinds
ghci> data MyProxy1 a = MyProxy1
ghci> :kind MyProxy1
MyProxy1 :: * -> *
```

となり、PolyKinds拡張を有効にした場合は

```
ghci> :set -XPolyKinds
ghci> data MyProxy2 a = MyProxy2
ghci> :kind MyProxy2
MyProxy2 :: k -> *
```

となります。

PolyKinds拡張はGHC2021に含まれるのでGHC 9.2以降ではデフォルトで有効ですが、cabal等のプロジェクトでdefault-languageをHaskell2010と指定する場合はデフォルトで無効となります。

# カインド変数の量化

型定義の際に、カインド注釈によってカインドが多相であることを明示することができます。

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
import Data.Kind

data MyProxy3 (a :: k) = MyProxy3

type MyProxy4 :: k -> Type
data MyProxy4 a = MyProxy4
```

`MyProxy3` と `MyProxy4` のカインドはいずれも `k -> Type` となります。

型変数と同様に、カインド変数は暗黙に全称量化 (`forall`) されます。GHCiで全称量化を明示的に表示するには、 `:set -fprint-explicit-foralls` コマンドを使います。

```
ghci> :set -fprint-explicit-foralls
ghci> :kind MyProxy3
MyProxy3 :: forall k. k -> *
ghci> :kind MyProxy4
MyProxy4 :: forall k. k -> *
```

先ほどの `MyProxy2` も全称量化されていますが、表示が少し異なります。具体的には、`forall` で型変数が波括弧 `{ }` で囲われています。

```
ghci> :kind MyProxy2
MyProxy2 :: forall {k}. k -> *
```

これはカインド変数がソースコード中に明示的に現れるかどうかが影響しています。ソース中に普通にカインド変数を書くと波括弧で囲われない変数になりますが、暗黙に導入された変数は波括弧がつきます。

GHC 9.0以降ではソースコード中のカインド注釈で波括弧を使うこともできます。

```haskell
type MyProxy5 :: forall {k}. k -> Type
data MyProxy5 a = MyProxy5
```

`forall` の波括弧の有無は何を意味しているのでしょうか。それは、TypeApplications拡張によるカインド適用の可否です。

TypeApplications拡張による型適用 `exp @ty` は多相的な値の `forall` された型変数を明示的に指定することを可能にしますが、カインド適用 `ty @kind` はカインド多相的な型の `forall` されたカインド変数を明示的に指定することを可能にします。

`forall k.` の形で量化されたカインド変数はカインド適用可能で、`forall {k}.` の形で量化された変数はカインド適用できません。試してみましょう：

```
ghci> :m + Data.Kind
ghci> :kind MyProxy2 @(Type -> Type)

<interactive>:1:1: error:
    • Cannot apply function of kind ‘k0 -> *’
      to visible kind argument ‘(Type -> Type)’
    • In the type ‘MyProxy2 @(Type -> Type)’
ghci> :kind MyProxy3 @(Type -> Type)
MyProxy3 @(Type -> Type) :: (* -> *) -> *
```

`MyProxy2 :: forall {k}. k -> *` へのカインド適用は失敗し、`MyProxy3 :: forall k. k -> *` へのカインド適用は成功しました。

`forall {k1}.` の内部に `forall k2.` があるようなカインドを持つ型にカインド適用をした場合は、`k1` は無視されて `k2` に適用されます。

```
ghci> :{
ghci| type T :: forall {k1} k2. k1 -> k2 -> Type
ghci| data T a b = T
ghci| :}
ghci> :kind T
T :: forall {k1} k2. k1 -> k2 -> *
ghci> :kind T @(Type -> Type)
T @(Type -> Type) :: forall {k1}. k1 -> (* -> *) -> *
```

# 依存カインド

現在のGHCでは、型とカインドは統合され、**型に依存するカインド**を書くことができます。例えば、次の定義は合法です：

```haskell
{-# LANGUAGE PolyKinds #-}

data T k (a :: k) = MkT
```

この型のカインドはどうなるでしょうか。

```
ghci> :kind T
T :: forall k -> k -> *
```

`forall k ->` という見慣れない量化子が登場しました。これは、陽に指定される型引数であって、しかも後続のカインドでそれを参照できるようなもの、となります。この `k` を指定するには、型適用の `@` は使わず、普通の型引数と同じように指定します：

```
ghci> :kind T (Type -> Type)
T (Type -> Type) :: (* -> *) -> *
ghci> :kind T (Type -> Type) Maybe
T (Type -> Type) Maybe :: *
```

# Type :: Type

現在のGHCではカインドと型が統合されました。これは `Int` などの型をカインドとして使えるだけでなく、 `Type` などのカインドも型として使えることを意味します。

では、 `Type` のカインドはどうなっているのでしょうか。確かめてみましょう。

```
ghci> :set -XNoStarIsType
ghci> :kind Type
Type :: Type
```

GHCにおいては `Type` のカインドは `Type` 自身となっているようです。関連して、昔はTypeInTypeというGHC拡張がありましたが、現在ではもはや意味がないので（常時有効なので）GHC拡張としては非推奨になっています。

論理的にはこの規則（`Type :: Type`）を認めてしまうとRusselのパラドックス（型理論の場合はGirardのパラドックスとも呼ばれる）により矛盾が導かれますが、プログラミング言語としては起こるのはせいぜい無限ループであり、Haskellの場合はすでに無限ループを起こす手段が色々あるので `Type :: Type` であっても実害はないと判断されたようです。

ちなみに、`Type` は型なので次のような関数も書けます：

```haskell
f :: Type -> Type
f x = x
```

と言っても、 `Type` 型の値を作り出したり操作したりする手段はなさそうなので、使い道はありません。
