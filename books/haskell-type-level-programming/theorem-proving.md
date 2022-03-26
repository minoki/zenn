---
title: "定理証明"
---

以前、長さ付きリストの反転を書こうとしましたが、

```haskell
reverse :: SizedList n a -> SizedList n a
reverse xs = revAppend Nil xs

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = acc
revAppend acc (Cons x xs) = revAppend (Cons x acc) xs
```

このコードはそのままでは「GHCが型レベル等式を導けない」のでコンパイルが通りませんでした。具体的には、

* `Add n 'Zero ~ n`
* `Add n ('Succ m) ~ 'Succ (Add n m)`

という型レベル等式がGHCにとって非自明なのでした。

この章では、GHCにとって非自明な等式をGHCに教えてやる方法、**定理証明**を扱います。

型レベル等式をGHCに教えるには、型レベル等式の証拠（`(:~:)` の値）を返す関数を定義して、 `Refl` に対してパターンマッチしてやれば良いです。具体的には、

```haskell
rightZero :: ... -> Add n 'Zero :~: n
rightSucc :: ... -> Add n ('Succ m) :~: 'Succ (Add n m)
```

という形の関数 `rightZero` と `rightSucc` を定義し、

```haskell
revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil
  = case rightZero ... of
      Refl -> {- ここで型レベル等式 Add n 'Zero ~ n が利用可能になる -}
              acc
revAppend acc (Cons x (xs :: SizedList m' a))
  = case rightSucc ... of
      Refl -> {- ここで型レベル等式 Add n ('Succ m') ~ 'Succ (Add n m') が利用可能になる -}
              revAppend (Cons x acc) xs
```

という風に `revAppend` を定義すればコンパイルが通ります。

では、`rightZero` と `rightSucc` はどのように定義すればよいでしょうか。

数学の証明だったら、 `n` についての数学的帰納法を使えばよさそうです。`rightZero` に相当する証明は以下のようになります：

* `n ~ 'Zero` の場合：普通に成り立つ。
* 適当な `n'` について `n ~ 'Succ n'` の場合：
    * （`Add ('Succ n') 'Zero ~ 'Succ n'` を証明したい）
    * 帰納法の仮定より `Add n' 'Zero ~ n'`
    * 両辺に `'Succ` を適用すれば `'Succ (Add n' 'Zero) ~ 'Succ n'`
    * `Add` の定義より `Add ('Succ n') 'Zero ~ 'Succ (Add n' 'Zero)` なので `Add ('Succ n') 'Zero ~ 'Succ n'`

型による場合分けが必要なので、 `rightZero` ではシングルトン `SPeanoNat n` を受け取ることにします。数学的帰納法の部分は、Haskellのコードでは再帰呼び出しとして表現できます。まとめると、「`Add n 'Zero ~ n` の証明」`rightZero` は次のように実装できます：

```haskell
rightZero :: SPeanoNat n -> Add n 'Zero :~: n
rightZero SZero = Refl
rightZero (SSucc s) = {- sの型は、 n ~ 'Succ n' となるような n' について SPeanoNat n' -}
                      case rightZero s of
                        Refl {- :: Add n' 'Zero :~: n' -} ->
                          Refl {- :: Add ('Succ n') 'Zero :~: 'Succ n' -}
```

同様に、「`Add n ('Succ m) ~ 'Succ (Add n m)` の証明」 `rightSucc` は次のように書けます：

```haskell
rightSucc :: SPeanoNat n -> Proxy m -> Add n ('Succ m) :~: 'Succ (Add n m)
rightSucc SZero _ = Refl
rightSucc (SSucc s) proxy = case rightSucc s proxy of
                              Refl -> Refl
```

`rightSucc` の中では `n` について帰納法を行いますが、 `m` については場合分けも帰納法も必要ないので、 `m` は `Proxy` として受け取っています。

`revAppend` から `rightZero` や `rightSucc` を呼び出すには、シングルトン型 `SPeanoNat n` の値を用意する必要があります。ここでは、引数に受け取ったリストの長さをシングルトンとして返すヘルパー関数を用意することにします。

```haskell
-- リストの長さをシングルトンとして返すヘルパー関数
sizedLength :: SizedList n a -> SPeanoNat n
sizedLength Nil = SZero
sizedLength (Cons _ xs) = SSucc (sizedLength xs)

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = case rightZero (sizedLength acc) of
                      Refl -> {- ここで型レベル等式 Add n 'Zero ~ n が利用可能になる -}
                              acc
revAppend acc (Cons x (xs :: SizedList m' a))
  = case rightSucc (sizedLength acc) (Proxy :: Proxy m') of
      Refl -> {- ここで型レベル等式 Add n ('Succ m') ~ 'Succ (Add n m') が利用可能になる -}
              revAppend (Cons x acc) xs
```

まとめると、長さ付きリストの反転は次のように書けます：

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (reverse)
import Data.Kind
import Data.Proxy
import Data.Type.Equality

data PeanoNat = Zero | Succ PeanoNat

data SPeanoNat (n :: PeanoNat) where
  SZero :: SPeanoNat 'Zero
  SSucc :: SPeanoNat n -> SPeanoNat ('Succ n)

type Add :: PeanoNat -> PeanoNat -> PeanoNat
type family Add n m
type instance Add 'Zero m = m
type instance Add ('Succ n') m = 'Succ (Add n' m)

type SizedList :: PeanoNat -> Type -> Type
data SizedList n a where
  Nil :: SizedList 'Zero a
  Cons :: a -> SizedList m a -> SizedList ('Succ m) a

-- Add n 'Zero ~ n の証明
rightZero :: SPeanoNat n -> Add n 'Zero :~: n
rightZero SZero = Refl
rightZero (SSucc s) = case rightZero s of
                        Refl -> Refl

-- Add n ('Succ m) ~ 'Succ (Add n m) の証明
rightSucc :: SPeanoNat n -> Proxy m -> Add n ('Succ m) :~: 'Succ (Add n m)
rightSucc SZero _ = Refl
rightSucc (SSucc s) proxy = case rightSucc s proxy of
                              Refl -> Refl

-- リストの長さをシングルトンとして返すヘルパー関数
sizedLength :: SizedList n a -> SPeanoNat n
sizedLength Nil = SZero
sizedLength (Cons _ xs) = SSucc (sizedLength xs)

reverse :: SizedList n a -> SizedList n a
reverse xs = revAppend Nil xs

revAppend :: SizedList n a -> SizedList m a -> SizedList (Add n m) a
revAppend acc Nil = case rightZero (sizedLength acc) of
                      Refl -> acc
revAppend acc (Cons x (xs :: SizedList m' a))
  = case rightSucc (sizedLength acc) (Proxy :: Proxy m') of
      Refl -> revAppend (Cons x acc) xs

-- 以下、動作確認用のコード

type Three = 'Succ ('Succ ('Succ 'Zero))

someList :: SizedList Three String
someList = Cons "foo" (Cons "bar" (Cons "baz" Nil))

toList :: SizedList n a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

main = do print (toList someList)
          print (toList (reverse someList))
```
