---
title: "型レベル関数と型族、型演算子"
---

# 型レベル関数と型族（TypeFamilies拡張）

型レベル関数とは、型を受け取って型を返す関数です。

まずは単純な例から見ていきましょう。先ほど定義した `PeanoNat` 型（を昇格したカインド）について、「2を加える」型レベル関数は、次のように定義できます：

```haskell
-- type Plus2 :: PeanoNat -> PeanoNat
type Plus2 (n :: PeanoNat) = 'Succ ('Succ n)
```

`Plus2` は `Succ` を2回適用するだけの簡単な関数で、この程度なら型シノニム（型の別名）として定義できます。再帰や条件分岐、パターンマッチなどのもっと複雑な処理を行う型レベル関数を書くには、**型族** (type family) を使います。

型族には「開かれた」 (open) ものと「閉じた」 (closed) ものがあります。どちらも `type family` 宣言で宣言します。

例として、与えられた自然数よりもひとつ小さい自然数（ただし入力が0の場合は0）を返す型レベル関数 `Pred :: PeanoNat -> PeanoNat` を定義してみましょう。

```haskell
-- type Pred :: PeanoNat -> PeanoNat
type family Pred (n :: PeanoNat) :: PeanoNat
type instance Pred 'Zero = 'Zero
type instance Pred ('Succ m) = m

{- 参考：同等の値レベル関数
pred :: PeanoNat -> PeanoNat
pred Zero = Zero
pred (Succ m) = m
-}
```

ここで使ったのは開かれた型族の方です。開かれた型族は最初に `type family` でカインドを宣言し、 `type instance` により個々のパターンに対するインスタンスを宣言していきます。

型族の計算結果はGHCiの `:kind!` コマンドで確認できます。

```haskell
ghci> :kind! Pred 'Zero
Pred 'Zero :: PeanoNat
= 'Zero
ghci> :kind! Pred ('Succ 'Zero)
Pred ('Succ 'Zero) :: PeanoNat
= 'Zero
ghci> :kind! Pred ('Succ ('Succ 'Zero))
Pred ('Succ ('Succ 'Zero)) :: PeanoNat
= 'Succ 'Zero
```

型族を使うと、再帰的な定義も可能となります。例えば、自然数の加法は次のように定義できます：

```haskell
-- type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

{- 参考：同等の値レベル関数
add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m
add (Succ n') m = Succ (add n' m)
-}
```

再帰的な型族を定義する際は、停止性が保守的に検査されます。例えば、 `Add` の定義を少し変えた

```haskell
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = Add n' ('Succ m)
```

の停止性はGHCには明らかではないのでデフォルトではコンパイルエラーとなります。エラーメッセージを見ればわかりますが、UndecidableInstances拡張を有効にすると停止性が（GHCにとって）不明な型族の定義が許容されるようになります。

自然数の加法があると、「長さが型レベルで管理されているリスト型」の連結演算は次のような型を持つ関数として表現できます：

```haskell
append :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
```

さて、開かれた型族にはいくつか制限があります。一つは、「左辺のパターンに同じ変数は2回以上出現できない」というものです。これは通常の（値レベルの）パターンマッチでおなじみでしょう。もう一つは、「パターンにマッチしなかった場合の処理を記述できない」というものです。閉じた型族 (closed type family) を使うとこれらの制限を回避できます。

やや人工的な例ですが、型引数が `Int` の場合は `'True` となり、それ以外の場合は `'False` となるような型関数 `IsInt` を定義してみましょう。

仮に `IsInt` を開かれた型族で定義しようとすると、

```haskell
type family IsInt (t :: Type) :: Bool
type instance IsInt Int = 'True
type instance IsInt Integer = 'False
type instance IsInt Float = 'False
type instance IsInt Double = 'False
type instance IsInt (Maybe a) = 'False
--- ...
```

という風に無数の `type instance` 宣言が必要となります。

一方、閉じた型族を使うと `IsInt` はシンプルに

```haskell
type family IsInt (t :: Type) :: Bool where
  IsInt Int = 'True
  IsInt a = 'False
```

と定義できます。

閉じた型族は `type family` 宣言に `where` を続け、等式による定義を並べていきます。適用の際には上から順番にパターンにマッチするか確かめられ、最初にマッチした定義の右辺で置き換えられます。

GHCiで `IsInt` の動作を確認してみましょう。

```haskell
ghci> :kind! IsInt Int
IsInt Int :: Bool
= 'True
ghci> :kind! IsInt Double
IsInt Double :: Bool
= 'False
```

`IsInt Int` は `'True` に、 `IsInt Double` は `'False` に簡約されました。

値レベルのパターンマッチと異なり、閉じた型族におけるパターンでは同じ変数が複数回出現できます。先程の `IsInt` を一般化して、与えられた二つの型が等しいかどうかを返す型関数 `Equal` は次のように定義できます：

```haskell
type family Equal a b where
  Equal a a = 'True
  Equal a b = 'False
```

別の例も見てみましょう。Haskellでは多変数関数は `->` を複数使って表現されますが、関数型が与えられた時の「引数の個数」を計算する関数 `Arity` を書いてみましょう。

```haskell
type family Arity (t :: Type) :: PeanoNat where
  Arity (a -> b) = 'Succ (Arity b)
  Arity a = 'Zero
```

まず `a -> b` に対してパターンマッチを行い、マッチした場合は「`b` の引数の個数」に1を加えたものを、マッチしなかった場合は `'Zero` を返しています。GHCiで動作を確かめてみましょう。

```haskell
ghci> :kind! Arity (Int -> Int -> Int)
Arity (Int -> Int -> Int) :: PeanoNat
= 'Succ ('Succ 'Zero)
ghci> :kind! Arity Double
Arity Double :: PeanoNat
= 'Zero
ghci> :kind! Arity ShowS
Arity ShowS :: PeanoNat
= 'Succ 'Zero
```

`Int -> Int -> Int` の引数の個数は 2 (= `Succ (Succ Zero)`), `Double` の引数の個数は 0 (= `Zero`), `ShowS` （これは `String -> String` の別名）の引数の個数は 1 (= `Succ Zero`) と判定されました。

<!-- 閉じた型族の注意点は、多相型との相性が悪いことです。 -->

<!-- TODO -->

（型族に関係する機能としてdata familyやassociated type familyがありますが、「型レベルプログラミング」という観点からは重要ではないので、ここでは説明しません。）

# 寄り道：型演算子（TypeOperators拡張）

TypeOperators拡張を使うと、

* 記号列からなる型・型クラスを定義できる
* アルファベットからなる型名をバッククォート `` ` `` で囲むことにより、中置演算子として使えるようになる

ようになります。値レベルの演算子は名前の先頭がコロン `:` か否かで扱いが変わりますが、型演算子は先頭が `:` であるかどうかは関係ありません。

先程の `Add` 型レベル関数を `+` という名前で改めて定義するとすれば、

```haskell
type family (+) (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance (+) 'Zero m = m
type instance (+) ('Succ n') m = 'Succ (n' + m)
```

あるいは

```haskell
type family (n :: PeanoNat) + (m :: PeanoNat) :: PeanoNat
type instance 'Zero + m = m
type instance 'Succ n' + m = 'Succ (n' + m)
```

となります。

# 部分適用と引数の個数

値レベルの関数はカリー化されており部分適用ができますが、型レベルの関数の部分適用には制限があります。

まず、型構築子は部分適用できます。例えば `Either :: Type -> Type -> Type` は2つの型を受け取る型構築子ですが、部分適用した `Either a` を他の型関数に渡すことができます。

```haskell
ghci> :kind Proxy (Either Int)
Proxy (Either Int) :: Type
```

一方で、型シノニムや型族は「引数の個数」が決まっており、所定の個数未満の引数で部分適用することはできません。例えば

```haskell
type Foo a b = Either
```

という型シノニムを定義してみます。この `Foo` のカインドは `Type -> Type -> Type -> Type -> Type` （PolyKinds拡張が有効な場合は `k1 -> k2 -> Type -> Type -> Type`）で、フルに適用すると4つの型引数を受け取ります。

```haskell
ghci> type Foo a b = Either
ghci> :kind Foo
Foo :: Type -> Type -> Type -> Type -> Type
```

この `Foo` にいくつかの引数を与えたものを `Proxy` 型の引数にしてみましょう。まず、引数が2個以上4個以下の場合は問題ありません：

```haskell
ghci> :m + Data.Proxy
ghci> :kind Proxy (Foo Int Int Int Int)
Proxy (Foo Int Int Int Int) :: Type
ghci> :kind Proxy (Foo Int Int Int)
Proxy (Foo Int Int Int) :: Type
ghci> :kind Proxy (Foo Int Int)
Proxy (Foo Int Int) :: Type
```

一方、引数が1個または0個の場合はエラーとなります：

```haskell
ghci> :kind Proxy (Foo Int)

<interactive>:1:1: error:
    The type synonym ‘Foo’ should have 2 arguments, but has been given 1
ghci> :kind Proxy Foo

<interactive>:1:1: error:
    The type synonym ‘Foo’ should have 2 arguments, but has been given none
```

つまり、`Foo` には引数を最低でも2つ以上与えなくてはなりません。この「引数の最低個数」の情報はカインドには現れません。

更なる例として、新たな型関数 `Bar` を定義してみます。

```haskell
type Bar a b c d = Either c d
```

型関数としては `Foo` と `Bar` は同じ意味ですが、引数の個数は違います。`Foo` の引数は最低2個で良かったのに対し、`Bar` は必ず4個フルに与えなくてはなりません：

```haskell
ghci> type Bar a b c d = Either c d
ghci> :kind Proxy (Bar Int Int)

<interactive>:1:1: error:
    The type synonym ‘Bar’ should have 4 arguments, but has been given 2
```

ここで挙げた例はいずれも型シノニムでしたが、型族も同じように「引数の最低個数」が決まっています。
