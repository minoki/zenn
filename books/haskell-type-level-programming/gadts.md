---
title: "一般化された代数的データ型 (GADT)"
---

# GADTs

通常の代数的データ型はデータ構築子（タグ）により分岐の種類やその他値レベルの情報を持っていますが、**一般化された代数的データ型** (generalized algebraic data type; GADT) を使うとデータ構築子に型に関する情報を持たせることができます。

例えば、幽霊型のところで紹介した型付きの構文木はGADTを使うと次のように書けます：

```haskell
data Expr (a :: Type) where
  Const :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Equal :: Expr Int -> Expr Int -> Expr Bool
  IfThenElse :: Expr Bool -> Expr a -> Expr a -> Expr a
```

データ構築子 `Const` と `Add` は型変数 `a` が `Int` の時だけ有効で、 `Equal` は `a` が `Bool` の時だけ有効です。幽霊型の場合は不変条件を守るために `mkInt`, `mkAdd`, `mkEqual` などのラッパーを用意し、

* ラッパーを書く際に不変条件が守られること
* 外部からはデータ構築子を直接使わずにラッパーを使うこと

をプログラマーが遵守する必要がありましたが、GADTを使えばデータ構築子を普通に使うだけでコンパイラーが不変条件を強制してくれます。

別の例を見てみましょう。幽霊型の例で「長さがちょうど n のリスト型」を扱いましたが、そこでのデータ構築子は普通のリストをラップしているだけで、型の不変条件を守るものはプログラマーの注意力だけでした。一方、GADTを使うとデータ構築子の段階で不変条件が強制される「長さがちょうど n のリスト型」を定義することができます：

```haskell
data SizedList (n :: PeanoNat) a where
   Nil :: SizedList 'Zero a
   Cons :: a -> SizedList n a -> SizedList ('Succ n) a
```

空リストを表す `Nil` がとりうる型は `SizedList 'Zero a` の形のものだけです。空リストに誤って `SizedList ('Succ 'Zero) a` という型がつくことはあり得ません。同様に、 `xs :: SizedList n a` について `Cons x xs` の型は `SizedList ('Succ n) a` となり、長さが `xs` よりもちょうど1大きくなります。

さて、GADTの重要な特徴は、パターンマッチによって新たな型制約を導入できることです。先程の構文木を評価する関数を書いてみましょう。

```haskell
eval :: Expr a -> a
eval e = case e of
  Const x -> {- ここでは a が Int となっている -}
             x
  Add e1 e2 -> {- ここでは a が Int となっている -}
               eval e1 + eval e2
  Equal e1 e2 -> {- ここでは a が Bool となっている -}
                 eval e1 == eval e2
  IfThenElse cond then_ else_ -> {- ここでは a は抽象的な型のまま -}
                                 if eval cond then eval then_ else eval else_
```

パターンマッチの枝ごとに型の詳細化が起こっています。データ構築子 `Const` が `a = Int` の時しか使えないということは、逆に言うとデータ構築子 `Const` にマッチしたときは `a = Int` という制約を導入できるということなのです。`Add` や `Equal` についても同様です。

なお、GHCでは型の等価性を表す制約は `~` と書かれます（例：`a ~ Int`）。これはGADTs拡張またはTypeFamilies拡張の下で使えるようになります。

# 型の等価性

型制約 `~` を使うと型の等価性を制約として表現できました。一方、GADTにより型の等価性をデータ型として表現することもできます：

```haskell
type (:~:) :: k -> k -> Type
data a :~: b where
  Refl :: a :~: a
```

これは [Data.Type.Equality モジュール](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Type-Equality.html)で定義されています。

`a :~: b` は `a` と `b` が同じ型のときのみ停止する値 `Refl` を持ちます。`a :~: b` 型の値に `Refl` でパターンマッチすることにより、`a` と `b` が同じ型であることをコンパイラーに教えることができます。

`Refl` は2つの型が同じである「証拠」と思うことができ、 `a :~: b` 型はその「証拠」を持って回りたい時に便利です。例えば、2つの型レベル自然数が等しいかどうかを判断し、等しい場合はその「証拠」を `Maybe` に包んで返す関数 `sameNat :: Proxy n -> Proxy m -> Maybe (n :~: m)` は次のように定義できます：

```haskell
-- 要 MultiParamTypeClasses ほか
class SameNat (n :: PeanoNat) (m :: PeanoNat) where
  sameNat :: Proxy n -> Proxy m -> Maybe (n :~: m)

instance SameNat 'Zero 'Zero where
  sameNat _ _ = Just Refl

instance SameNat 'Zero ('Succ m) where
  sameNat _ _ = Nothing

instance SameNat ('Succ n) 'Zero where
  sameNat _ _ = Nothing

instance SameNat n m => SameNat ('Succ n) ('Succ m) where
  sameNat _ _ = case sameNat (Proxy :: Proxy n) (Proxy :: Proxy m) of
                  Just Refl -> {- ここでは n と m が同じ型となっている -}
                               Just Refl {- この Refl は 'Succ n :~: 'Succ m 型だが、 n と m が同じ型なのでコンパイルが通る -}
                  Nothing -> Nothing
```

ちなみに、GADT以前は型の等価性を表すデータ型の表現として次のようなものが使われることがありました：

```haskell
newtype Equal a b = Equal (forall f. f a -> f b)
```

# 存在型

先程の `SizedList` 型について、述語を満たす要素を取り出す `filter` 関数を定義したい、としましょう。この `filter` 関数の返り値の型はどう書けばいいでしょうか？

```haskell
filter :: (a -> Bool) -> SizedList n a -> SizedList ??? a
```

`filter` 関数の返り値の長さは静的にはわかりません。「静的にはわからない型」を表現する方法として、**存在型**があります。この場合、「長さがわからない `SizedList`」を表す型として次のような型を定義できます：

```haskell
data SomeSizedList a = forall n. SomeSizedList (SizedList n a)
```

右辺の型変数 `n` が `forall` で束縛され、左辺に登場しないことに注意してください。GADTの文法を使うと、次のように書くこともできます：

```haskell
data SomeSizedList a where
  SomeSizedList :: SizedList n a -> SomeSizedList a
```

この `SomeSizedList` を使うと、 `filter` は次のように実装できます：

```haskell
filter :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter f Nil = SomeSizedList Nil
filter f (Cons x xs) | f x = case filter f xs of
                               SomeSizedList ys -> SomeSizedList (Cons x ys)
                     | otherwise = filter f xs
```

型制約を含んだデータ構築子を作ることもできます。例えば、「`Show` クラスの何らかのインスタンス」を表す型 `Showable` は次のように定義できます： 

```haskell
data Showable = forall a. Show a => Showable a

showSomething :: Showable -> String
showSomething (Showable x) = show x
```

データ構築子に、存在量化されていない型変数についての型制約を持たせることもできます。このようなデータ型は、ある型がある制約を満たすことの「証拠」を保持していると思うことができます。

例えば、ある型 `a` が `Show` クラスのインスタンスであることの「証拠」を持ったデータ型 `ShowWitness` は次のように定義・利用できます：

```haskell
data ShowWitness a = Show a => ShowWitness a

showSomethingWithWitness :: ShowWitness a -> a -> String -- a に Show 制約は課されていない
showSomethingWithWitness x y = case x of
                                 ShowWitness x' -> {- ここでは a は Show のインスタンス -}
                                                   "(" ++ show x' ++ "," ++ show y ++ ")"
```

GHCでは、存在量化されていない型変数についての型制約をデータ構築子に持たせるためにExistentialQuantification拡張またはGADTs拡張を要求します。そのため、この本ではこの機能も存在型の一種として扱います。

# 寄り道：GADTを解体する

GADTを使うと型の等価性をデータ型として表現できました。逆に、型の等価性と存在型を組み合わせることでGADTを再現することができます。例えば、型付き構文木と長さ付きリストの例は等価性制約および存在型の組み合わせで次のように書けます：

```haskell
data Expr a = a ~ Int =>  Const Int
            | a ~ Int =>  Add (Expr Int) (Expr Int)
            | a ~ Bool => Equal (Expr Int) (Expr Int)
            |             IfThenElse (Expr Bool) (Expr a) (Expr a)

data SizedList n a =            n ~ 'Zero =>    Nil
                   | forall n'. n ~ 'Succ n' => Cons a (SizedList n' a)
```
