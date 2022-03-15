---
title: "幽霊型とProxy"
---

# 幽霊型

幽霊型 (phantom type) とは、データ型の定義の右辺に現れない型パラメーターを持つデータ型のことです[^phantom-type]。用途の一つは、型に関する不変条件の表現です。

[^phantom-type]: 『[「幽霊型」の意味がややこしい](http://akabe.github.io/2015/06/PhantomTypeTechTerm)』によると、「幽霊型」で何を指すかについてはいくつか流儀があるようです。

例を見てみましょう。「長さが `n` であるようなリスト型」 `SizedList` を次のように定義します：

```haskell
-- 長さ n のリスト型
newtype SizedList (n :: PeanoNat) a = SizedList [a]
```

`a` は要素を表す型で、定義の右辺に現れています。一方、長さを表す `n` は右辺に現れない、幽霊型です（`n :: PeanoNat` は型変数のカインドが `PeanoNat` であるという意味で、詳しくは後述します）。

この `n` という型パラメーターはデータ型の定義そのものには役立ちませんが、ライブラリーを書く人が「`SizedList n a` を扱う関数では `n` をリストの長さと一致させる」という約束（不変条件）を守ることによって、ある種の実行時エラーを防ぐことができます。

例えば、部分関数として悪名高い `head :: [a] -> a` をラップした

```haskell
head :: SizedList ('Succ m) a -> a
head (SizedList xs) = head xs
```

は安全な関数となります（`'Succ m` は「何らかの自然数 `m` に1を加えたもの」、要するに「1以上の自然数」という意味で、詳しくは後述します）。

幽霊型のより高度な例として、型付き構文木を見てみましょう。以下の4種類のノードからなる構文木を考えます：

* 整数定数
* 整数同士の加算
* 整数同士の比較（真偽値を返す）
* if式（条件分岐）

これを素直にHaskellのデータ型として定義すると次のようになるでしょう：

```haskell
data Expr = Const Int
          | Add Expr Expr
          | Equal Expr Expr
          | IfThenElse Expr Expr Expr
```

この定義の欠点として、不正な構文木を構築できてしまうという点があります。例えば、 `Add (Equal (Const 42) (Const 37)) (Const 1)` は真偽値（`Equal` ノード）に対して加算（`Add` ノード）を適用しているので、不正な構文木です。

この問題を幽霊型を使って緩和してみます。`Expr` 型構築子に幽霊型変数 `a` を持たせ、型の不変条件として「`a` が `Int` の時は式は整数を表し、`a` が `Bool` の時は式は真偽値を表す」と約束します。そして、次のように定義します：

```haskell
data Expr a = Const Int -- 不変条件：a = Int
            | Add (Expr Int) (Expr Int) -- 不変条件：a = Int
            | Equal (Expr Int) (Expr Int) -- 不変条件：a = Bool
            | IfThenElse (Expr Bool) (Expr a) (Expr a)

mkConst :: Int -> Expr Int
mkConst = Const

mkAdd :: Expr Int -> Expr Int -> Expr Int
mkAdd = Add

mkEqual :: Expr Int -> Expr Int -> Expr Bool
mkEqual = Equal
```

この `Expr` 型を使う側からは `Const`, `Add`, `Equal` は直接使わずに `mkConst`, `mkAdd`, `mkEqual` を通して使うようにします。すると、先程の不正な構文木を作ろうとした段階でHaskellの型エラーが発生します：

```haskell
ghci> mkAdd (mkEqual (mkConst 42) (mkConst 37)) (mkConst 1)

<interactive>:36:8: error:
    • Couldn't match type ‘Bool’ with ‘Int’
      Expected: Expr Int
        Actual: Expr Bool
    • In the first argument of ‘mkAdd’, namely
        ‘(mkEqual (mkConst 42) (mkConst 37))’
      In the expression:
        mkAdd (mkEqual (mkConst 42) (mkConst 37)) (mkConst 1)
      In an equation for ‘it’:
          it = mkAdd (mkEqual (mkConst 42) (mkConst 37)) (mkConst 1)
```

幽霊型の別の（不変条件を表現しない）例としては、続けて紹介する `Proxy` 型があります。

# Proxy 型

標準ライブラリーの `Proxy` 型は次のように定義されています：

```haskell
module Data.Proxy where

data Proxy t = Proxy
```

こちらは特に不変条件を表現しているわけではありません。`Proxy` 型は、関数に型を渡したい場合に使います。

例えば、固定長整数型の最大値を `Integer` として返す関数を定義したい、としましょう。この関数は

```haskell
maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Integer
maxBoundAsInteger = toInteger (maxBound :: a)
```

と書ける……と言いたいところですが、古典的なHaskellではこれは不正な型です[^ambiguous-types]。この関数の引数（ここでは0個）にも戻り値の型にも `a` という型変数が現れず、 `a` が曖昧となってしまうからです。

[^ambiguous-types]: GHC拡張のAllowAmbiguousTypesおよびTypeApplicationsを使うとこれは有効な定義となります。

この場合、 `Proxy a` というダミーの引数を追加すれば型の曖昧さを除去できます。実装例は次のようになります：

```haskell
maxBoundAsInteger :: forall a. (Integral a, Bounded a) => Proxy a -> Integer
maxBoundAsInteger _proxy = toInteger (maxBound :: a)
```

ちなみに、 `Proxy` 型は任意のカインドを持つ型に対して適用できます。`Proxy Int` が正当な型であるのはもちろんですが、 `Proxy Maybe` や `Proxy Either` も正当な型となります。`Proxy` 型のカインドは（任意の `k` について） `k -> Type` となります。これは**カインド多相** (kind polymorphism) の例です。

```haskell
λ> :kind Proxy
Proxy :: k -> *
```
