---
title: "復習・小まとめ"
---

ここまでの知識でできるようになった型レベルプログラミングの例をまとめてみます。

使用するGHC拡張とモジュールは以下の通りです：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-} -- GHC 8.10以降
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Kind -- Type
import Data.Proxy
```

REPL (GHCi)で試す場合は次のコマンドを使うとこれらを有効化できます：

```haskell
ghci> :set -XDataKinds -XGADTs -XTypeFamilies -XStandaloneKindSignatures -XUndecidableInstances -XScopedTypeVariables
ghci> :m + Data.Kind Data.Proxy
```

# 自然数

ここではペアノ流に自然数を構成します。DataKinds拡張により `PeanoNat` 型がカインドへ、データ構築子 `Zero` および `Succ` が型構築子へ持ち上げられます。

```haskell
data PeanoNat = Zero | Succ PeanoNat

{- DataKinds拡張により得られる型構築子：
type 'Zero :: PeanoNat
type 'Succ :: PeanoNat -> PeanoNat
-}
```

「1小さい自然数（ただし0に対しては0）を返す関数」の値レベル版と型レベル版はそれぞれ次のように定義できます：

```haskell
pred :: PeanoNat -> PeanoNat
pred Zero = Zero
pred (Succ m) = m

type Pred :: PeanoNat -> PeanoNat
type family Pred n
type instance Pred 'Zero = 'Zero
type instance Pred ('Succ m) = m
```

先に載せたコードではカインド注釈を `type family Pred (n :: PeanoNat) :: PeanoNat` と書いていましたが、ここではStandaloneKindSignatures拡張（GHC 8.10以降）を使って別の行にカインド注釈を書いています。

自然数の加法は次のように実装できます：

```haskell
add :: PeanoNat -> PeanoNat -> PeanoNat
add Zero m = m 
add (Succ n') m = Succ (add n' m)

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

{- 別の定義の仕方（要UndecidableInstances）
type family Add (n :: PeanoNat) (m :: PeanoNat) :: PeanoNat
type instance Add 'Zero m = m
type instance Add ('Succ n') m = Add n' ('Succ m)
-}
```

自然数の乗算は次のようになります：

```haskell
mul :: PeanoNat -> PeanoNat -> PeanoNat
mul Zero m = Zero
mul (Succ n') m = add m (mul n' m)

type Mul :: PeanoNat -> PeanoNat -> PeanoNat
type family Mul n m
type instance Mul 'Zero m = 'Zero
type instance Mul ('Succ n') m = Add m (Mul n' m) -- 要UndecidableInstances
```

乗算をREPLで試してみましょう。2×3を型レベルで計算してみます。実行例：

```haskell
ghci> type Two = 'Succ ('Succ 'Zero)
ghci> type Three = 'Succ Two
ghci> :kind! Mul Two Three
Mul Two Three :: PeanoNat
= 'Succ ('Succ ('Succ ('Succ ('Succ ('Succ 'Zero)))))
```

結果の型には `'Succ` が6個含まれているので、2×3=6が確かに計算できたことがわかります。

自然数の大小比較を書いてみます。

```haskell
compare :: PeanoNat -> PeanoNat -> Ordering
compare Zero Zero = EQ
compare Zero (Succ _) = LT
compare (Succ _) Zero = GT
compare (Succ n) (Succ m) = compare n m

type Compare :: PeanoNat -> PeanoNat -> Ordering
type family Compare n m
type instance Compare 'Zero 'Zero = 'EQ
type instance Compare 'Zero ('Succ _) = 'LT
type instance Compare ('Succ _) 'Zero = 'GT
type instance Compare ('Succ n) ('Succ m) = Compare n m
```

実行例：

```haskell
ghci> :kind! Compare Two Three
Compare Two Three :: Ordering
= 'LT
ghci> :kind! Compare Three Two
Compare Three Two :: Ordering
= 'GT
```

与えられた自然数が偶数かどうかを計算する関数を書いてみます：

```haskell
even :: PeanoNat -> Bool
even Zero = True
even (Succ Zero) = False
even (Succ (Succ n)) = even n

type Even :: PeanoNat -> Bool
type family Even n
type instance Even 'Zero = 'True
type instance Even ('Succ 'Zero) = 'False
type instance Even ('Succ ('Succ n)) = Even n
```

`PeanoNat` カインドの型として表現された自然数（型レベル自然数）を値レベルの自然数に変換するには、型クラスを使います。

```haskell
class PeanoNatToInteger (n :: PeanoNat) where
  peanoNatToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  peanoNatToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  peanoNatToInteger _ = 1 + peanoNatToInteger (Proxy :: Proxy n)
```

# 長さの決まったリスト型

自然数とGADTの応用として、「長さの決まったリスト型」を書いてみます。

```haskell
type SizedList :: PeanoNat -> Type -> Type
data SizedList n a where
  Nil :: SizedList 'Zero a
  Cons :: a -> SizedList m a -> SizedList ('Succ m) a
```

リストを連結する関数は、先程定義した型レベル `Add` 関数を使うと次のように書けます：

```haskell
append :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)
```

この定義の中の部分式に型注釈をつけると、次のようになります：

```haskell
-- 疑似コード
append (Nil :: SizedList 'Zero a) (ys :: SizedList m a)
  = ys :: SizedList m a {- ~ SizedList (Add 'Zero m) a -}
append (Cons x (xs :: SizedList n' a) :: SizedList (Succ n') a) (ys :: SizedList m a)
  = let zs = append xs ys :: SizedList (Add n' m) a
    in Cons x zs :: SizedList ('Succ (Add n' m)) a {- ~ SizedList (Add ('Succ n') m) a -}
```

言葉でも説明した方が良いでしょう。最初の引数が `Nil` の場合（この場合 `n ~ 'Zero` です）、右辺の型は `SizedList m a` なのに対して、期待される型は `SizedList (Add 'Zero m) a` です。型の等式 `m ~ Add 'Zero m` が必要になりますが、 `Add` の定義では `Add 'Zero m = m` となっているのでGHCはこの等式を正しいと判定します。

最初の引数が `Cons x xs` の場合（この場合、適当な自然数 `n'` について `n ~ 'Succ n'` です）、右辺の型は `SizedList ('Succ (Add n' m)) a` なのに対して、期待される型は `SizedList (Add ('Succ n') m) a` です。型の等式 `'Succ (Add n' m) ~ Add ('Succ n') m` が必要になりますが、 `Add` の定義では `Add ('Succ n') m = 'Succ (Add n' m)` となっているのでGHCはこの等式を正しいと判定します。

注意しなければならないのは、この `append` の定義は `Add` の定義に依存していることです。試しに `Add` の定義を「別の定義の仕方」に変えてみると、 `Could not deduce: Add m1 ('Succ m) ~ 'Succ (Add m1 m)` という風なコンパイルエラーが出ると思います。

`filter` 関数を書いてみましょう。`filter` 関数は返すリストの長さが事前にわからないため、存在型が必要になるのでした：

```haskell
data SomeSizedList a where
  SomeSizedList :: SizedList n a -> SomeSizedList a
```

```haskell
filter :: (a -> Bool) -> SizedList n a -> SomeSizedList a
filter f Nil = SomeSizedList Nil
filter f (Cons x xs) | f x = case filter f xs of
                               SomeSizedList ys -> SomeSizedList (Cons x xs)
                     | otherwise = filter f xs
```

より複雑な例として、リストを反転する関数を書いてみましょう。まず、通常のリストに対する反転は次のように書けます：

```haskell
-- 参考：通常のリストの反転
reverse :: [a] -> [a]
reverse xs = revAppend [] xs

-- 補助関数：第2引数を反転させた上で第1引数の前にくっつける
revAppend :: [a] -> [a] -> [a]
revAppend acc [] = acc
revAppend acc (x : xs) = revAppend (x : acc) xs
```

これを長さ付きリストに対して移植すると、次のようになるでしょう。`revAppend` の返り値の長さは引数二つの長さの和なので、 `Add` を使えば良いでしょう：

```haskell
reverse :: SizedList n a -> SizedList n a
reverse xs = revAppend Nil xs

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = acc
revAppend acc (Cons x xs) = revAppend (Cons x acc) xs
```

しかし、このコードはすんなりとコンパイルが通りません。`Add` の定義が最初に紹介した

```haskell
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)
```

の場合は「Could not deduce: `Add n 'Zero ~ n` （〜を推論できない）」「Could not deduce: `Add n ('Succ m1) ~ 'Succ (Add n m1)` （〜を推論できない）」というようなエラーが出ると思います。

ここでGHCが「推論できない」と言っている等式は通常の算数の表記を使えば

* `n + 0 = n`
* `n + (m1 + 1) = (n + m1) + 1`

なので我々からすると当然成り立つはずなのですが、GHCにはこれらの等式は非自明なのです。

GHCにとって非自明な型レベル等式が成り立つことをGHCに教えてやるには、いわゆる**定理証明**と呼ばれる作業が必要になります。これについては後の方で紹介します。

最後に、「リストの i 番目の要素を取得する」関数を書いてみましょう。通常のリストに対する `(!!)` 関数はリストの範囲外アクセスの場合は実行時エラーを投げますが、ここでは「型レベルで長さがわかっているリスト」を扱っているので、インデックスも型レベル自然数で与えて適宜制約を書けば「絶対に範囲外アクセスにならないインデックス関数」が書けるはずです。型はこんな感じになると想定されます：

```haskell
index :: Compare i n ~ 'LT => Proxy i -> SizedList n a -> a
```

型制約の部分で先ほど実装した `Compare` 関数を使っています。普通の算数っぽく書けば `i < n` です。型引数の `i` を曖昧にしないために、関数の型に `Proxy` を含めています。

実装ですが、……このままでは書けません。`i` の値に依存して処理を書く必要がありますが、`i` の値を取得できる型制約はついていないし、引数もただの `Proxy` なのでパターンマッチしても何も起こりません。

ここでは、専用の型クラスを定義することにします。型クラスを使うと、安全な `index` 関数は次のように定義できます：

```haskell
class Index (i :: PeanoNat) where
  index :: Compare i n ~ 'LT => Proxy i -> SizedList n a -> a

instance Index 'Zero where
  -- index _ Nil = undefined -- GADTによる詳細化と型制約の兼ね合いで到達不能判定されるので書かなくて良い
  index _ (Cons x _) = x

instance Index j => Index ('Succ j) where
  -- index _ Nil = undefined -- GADTによる詳細化と型制約の兼ね合いで到達不能判定されるので書かなくて良い
  index _ (Cons _ xs) = index (Proxy :: Proxy j) xs
```

ここで定義した `index` の型は次のようになります：

```haskell
index :: (Index i, Compare i n ~ 'LT) => Proxy i -> SizedList n a -> a
```

REPLで試してみましょう。まずは `'A' : 'B' : 'C' : []` に相当するリストの、2番目（先頭が0番目）の要素を取得してみます：

```haskell
ghci> index (Proxy :: Proxy ('Succ ('Succ 'Zero))) (Cons 'A' (Cons 'B' (Cons 'C' Nil)))
'C'
```

期待通り、 `'C'` が取得できました。今度は3番目（リストの範囲外）の要素を取得してみます：

```haskell
ghci> index (Proxy :: Proxy ('Succ ('Succ ('Succ 'Zero)))) (Cons 'A' (Cons 'B' (Cons 'C' Nil)))

<interactive>:277:1: error:
    • Couldn't match type ‘'EQ’ with ‘'LT’
        arising from a use of ‘index’
    • In the expression:
        index
          (Proxy :: Proxy ('Succ ('Succ ('Succ 'Zero))))
          (Cons 'A' (Cons 'B' (Cons 'C' Nil)))
      In an equation for ‘it’:
          it
            = index
                (Proxy :: Proxy ('Succ ('Succ ('Succ 'Zero))))
                (Cons 'A' (Cons 'B' (Cons 'C' Nil)))
```

リストの範囲外のため、型エラーが発生しました。期待通りです。

# 次章予告

型レベルのものを値レベルに持ってくるには、何らかの形で型クラスを使う必要があります。現状では `peanoNatToInteger` に対して `PeanoNatToInteger` クラス、`index` 関数に対して `Index` クラスという風に、処理ごとに型クラスを導入しているわけですが、これは煩雑です。

単に煩雑なだけではなく、根本的な問題もあります。「値レベルの自然数を型レベルに持ち上げる」関数 `somePeanoNat` を書いてみましょう。返り値は「`PeanoNat` カインドの型を封じ込めた」存在型となります。実装例は次のようになるでしょう：

```haskell
data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). Proxy n -> SomePeanoNat

somePeanoNat :: PeanoNat -> SomePeanoNat
somePeanoNat Zero = SomePeanoNat (Proxy :: Proxy 'Zero)
somePeanoNat (Succ n) = case somePeanoNat n of
                          SomePeanoNat (_ :: Proxy n) -> SomePeanoNat (Proxy :: Proxy ('Succ n))
```

存在型 `SomePeanoNat` には何らかの型レベル自然数 `n :: PeanoNat` が封じ込められています。……が、型変数 `n` には何の制約もついていないため、これに対して `peanoNatToInteger` や `index` などの操作を行うことはできません。

存在型 `SomePeanoNat` にこれらの型制約を持たせるようにすれば、 `peanoNatToInteger` や `index` などを適用可能にできます：

```haskell
data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). (PeanoNatToInteger n, Index n) => Proxy n -> SomePeanoNat
```

が、この方法はスケールしません。自然数に対する処理を追加するたびに存在型の定義をいじるか、型クラスにメソッドを追加するのでしょうか？また、複数の型レベル自然数にまたがる `compareNat` のような処理にはこの方法では対応できません。

必要なのは、ひとつの万能な型クラス `SingI_PeanoNat` であって、それがあれば型レベル自然数に対してパターンマッチが行えるようなものです。それがあれば、存在型 `SomePeanoNat` や型レベル自然数を使った各種処理は以下のように書けるでしょう：

```haskell
class SingI_PeanoNat (n :: PeanoNat) where ...
instance SingI_PeanoNat 'Zero where ...
instance SingI_PeanoNat n => SPeanoNat ('Succ n) where ...

data SomePeanoNat where
  SomePeanoNat :: forall (n :: PeanoNat). SingI_PeanoNat n => Proxy n -> SomePeanoNat

peanoNatToInteger :: SingI_PeanoNat n => Proxy n -> Integer
index :: (SingI_PeanoNat i, Compare i n ~ 'LT) => Proxy i -> SizedList n a -> a
compareNat :: (SingI_PeanoNat n, SingI_PeanoNat m) => Proxy n -> Proxy m -> Ordering
```

次章では、この「万能な」型クラス `SingI_PeanoNat` の定義を扱います。
