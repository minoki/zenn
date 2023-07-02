---
title: "応用：GHC.Generics"
---

この章では、GHCのジェネリックプログラミングを簡単に紹介します。

Haskellの特徴の一つは、データ型に対する型クラスの導出（deriving）メカニズムです。`deriving Eq` と書くだけで `==` の定義がコンパイラーによって生成され、`deriving Show` と書けば `show` の定義が生成されます。

この自動導出を、コンパイラー組み込みの型クラスだけでなく、ライブラリー定義の型クラスに対しても行えると便利です。例えば、JSONとの変換関数を自動生成できると便利でしょう。`GHC.Generics` を使うとそういうことができます。

# ジェネリックプログラミングの考え方

`GHC.Generics` の紹介に入る前に、簡単な例で考え方を紹介します。例えば、次のような代数的データ型 `T` を定義したとしましょう：

```haskell
data T = A String Int | B | C Char
```

この型は、汎用的なタプルと `Either` を使って次のように「表現」することができます：

```haskell
type T' = Either (String, Int) (Either () Char)
```

つまり、変換関数 `T -> T'` とその逆 `T' -> T` が存在します。

後者の表現のメリットは、タプルや `Either` という、ライブラリー作者もよく知っているであろう型（汎用品とでも呼びましょうか）を使って構成されているので、ライブラリー側でインスタンスを提供するのが容易であることです。`Eq` クラスなら、次を用意しておけば良いでしょう：

```haskell
instance Eq ()
instance (Eq a, Eq b) => Eq (a, b)
instance (Eq a, Eq b) => Eq (Either a b)
```

つまり、`T` に対して汎用品を使った表現 `T'` と変換関数 `from :: T -> T'`, `to :: T' -> T` さえ用意してやれば、ライブラリーの提供するインスタンス定義を利用できるだろう、ということです。

ここでは「表現」の側ではデータ構築子の名前や（レコードの場合）フィールドの名前が失われてしまっています。実際のGHCのジェネリックプログラミングでは、その辺りも型を使ってエンコードされます。

GHCでは、汎用品を使った表現と変換関数を用意する部分は、DeriveGeneric拡張による `Generic` 型クラスの自動導出という形でコンパイラーがやってくれます。あとはライブラリーが汎用品に対するインスタンスを用意すれば、ライブラリー定義の型クラスに対する自動導出ができるようになります。

# GHC.Generics

`GHC.Generics` はGHCに付属するモジュールの名前で、`Generic` 型クラスや、「汎用品」に相当するデータ型が定義されています。

まずは `Generic` 型クラスを見ていきましょう。

```haskell
module GHC.Generics where

class Generic a where
  type Rep a :: Type -> Type
  from :: a -> Rep a x
  to :: Rep a x -> a
```

`Generic` クラスは関連型 (associated type) `Rep a` を持ちます。関連型は型クラスに紐づいた型族です。`Rep` 型族はカインド `Type -> Type -> Type` を持ち、最初の型引数は元となる代数的データ型で、二番目の型引数はダミーです。

`from` 関数と `to` 関数は、汎用品を使った表現と元のデータ型を相互に変換する関数です。

GHCiの `:kind!` コマンドで、実際どのような型が表現として使われるのか見てみましょう。

```haskell
ghci> :m + GHC.Generics
ghci> :set -XDeriveGeneric 
ghci> data T = A String Int | B | C Char deriving Generic
ghci> :kind! Rep T
Rep T :: * -> *
= M1
    D
    ('MetaData "T" "Ghci1" "interactive" 'False)
    (M1
       C
       ('MetaCons "A" 'PrefixI 'False)
       (M1
          S
          ('MetaSel
             'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
          (K1 R [Char])
        :*: M1
              S
              ('MetaSel
                 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
              (K1 R Int))
     :+: (M1 C ('MetaCons "B" 'PrefixI 'False) U1
          :+: M1
                C
                ('MetaCons "C" 'PrefixI 'False)
                (M1
                   S
                   ('MetaSel
                      'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                   (K1 R Char))))
ghci> from (A "a" 42)
M1 {unM1 = L1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = "a"}} :*: M1 {unM1 = K1 {unK1 = 42}}})}
```

思ったより複雑な型が現れました。ただ、膨大に見える情報の多くは構築子やフィールドの情報を保持するメタデータで、骨格の部分を抜き出すと次のようになっています：

```haskell
type Rep T = (K1 R [Char] :*: K1 R Int)
             :+: (U1
                  :+: (K1 R Char))
```

`:*:` がタプル（直積）に相当し、 `:+:` が直和に相当します。実際、これらは次のように定義されています：

```haskell
type (:+:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:+:) f g p = L1 (f p) | R1 (g p)

type (:*:) :: (k -> Type) -> (k -> Type) -> k -> Type
data (:*:) f g p = f p :*: g p
```

ダミーの引数 `p` を無視すれば、 `Either` やタプルと同じような定義だとわかります。

`K1` は次のように定義されており、 `K1 R a` は `a` の別名です。

```haskell
type K1 :: Type -> Type -> k -> Type
newtype K1 i c p = K1 { unK1 :: c }
```

`U1` はユニット型にダミーの引数を加えたものです。

```haskell
type U1 :: k -> Type
data U1 p = U1
```

いずれもダミーの引数があって鬱陶しいですが、これは型引数を取る型に対してのジェネリックプログラミングである `Generic1` クラスに関連して使われるようです。この記事では `Generic1` クラスは扱いません。

先の節で例として挙げた `T'` には構築子やフィールドの名前の情報は入っていませんでした。GHCの導出した `Rep` 型には `M1` 型としてそういうメタデータが含まれています。`M1` 型自体はただの `newtype` です：

```haskell
type M1 :: Type -> Meta -> (k -> Type) -> k -> Type
newtype M1 i c f p = M1 { unM1 :: f p }
```

`M1` 型の第2引数（`Meta` カインド）には型レベル文字列などの形でメタ情報が入っています。本書で学んだ型レベルプログラミングの技法を使えば、活用は難しくないでしょう。

# 例：データ構築子の名前を取得する

ここでは例として、データ構築子の名前を取得する関数を書いてみましょう。

目標としては、このようなデータ型の定義に対し、

```haskell
data T = A String Int | B | C Char
       deriving Generic
```

このようなインスタンスを導出することを目標とします：

```haskell
-- ライブラリーでの定義
class ConstructorName a where
  constructorName :: a -> String

-- データ型ごとに導出される定義
instance ConstructorName T where
  constructorName A {} = "A"
  constructorName B = "B"
  constructorName C {} = "C"
```

まずは、何らかの `Generic` のインスタンスがあったときに「データ構築子の名前を取得する」関数を書きましょう。ただし、ここでは `Rep a` の内容を実行時に利用する必要があるため、型制約は `Generic a` だけでは足りません。

```haskell
genericConstructorName :: (Generic a, ???) => a -> String
```

`Rep a` の指すものは `:+:` かもしれないし `M1` かもしれないし `U1` かもしれません。`Rep a` の指すものによって場合分けが必要です。Haskellで型によって場合分けをするには何を使えばいいでしょうか？そう、型クラスですね。

ここではカインド `(k -> Type) -> Constraint` を持つ型クラス `ConstructorName'` を定義します：

```haskell
class ConstructorName' f where
  constructorName' :: f x -> String
```

`genericConstructorName` 関数は `ConstructorName' (Rep a)` を制約に取り、引数を `from` に通して `Rep a x` に変換し、実際の作業を `constructorName'` に任せます。

```haskell
genericConstructorName :: (Generic a, ConstructorName' (Rep a)) => a -> String
genericConstructorName = constructorName' . from
```

では `ConstructorName'` のインスタンスの定義です。まず、データ構築子に対するメタデータが得られた場合を定義しましょう。

データ構築子に対するメタデータは `M1 C (MetaCons n f s) f'` の形をしています（`C` はconstructorのCと思われます）。 `n :: Symbol` はデータ構築子の名前、`f :: FixityI` はfixity、`s :: Bool` はレコードか否かです。ここでは `n` からデータ構築子の名前を取得すれば良いでしょう。

```haskell
instance KnownSymbol n => ConstructorName' (M1 C (MetaCons n f s) f') where
  constructorName' _ = symbolVal (Proxy :: Proxy n)
```

`:*:` や `U1` は `M1 C` の内側に来るので考える必要はありません。

考える必要があるのは `M1 D` と `:+:` です（本当は「コンストラクターを持たないデータ型」を表す `V1` も来る可能性がありますが、ここでは割愛します）。`M1 D` は中身に丸投げすればよくて、`:+:` は場合分けするだけです。

```haskell
instance ConstructorName' f => ConstructorName' (M1 D m f) where
  constructorName' (M1 x) = constructorName' x

instance (ConstructorName' f, ConstructorName' g) => ConstructorName' (f :+: g) where
  constructorName' (L1 x) = constructorName' x
  constructorName' (R1 y) = constructorName' y
```

早速試してみましょう。`genericConstructorName` と `ConstructorName'` の定義をファイルに書いてGHCiに読み込み、データ型を定義して呼び出してみます：

```haskell
ghci> :{
ghci| data T = A String Int | B | C Char
ghci|        deriving Generic
ghci| :}
ghci> genericConstructorName (A "a" 42)
"A"
```

うまくいったようです。

あとは `ConstructorName` クラスを `deriving` っぽく使えるようにする方法ですが、大きく分けて二通りのやり方があります。

一つ目はDefaultSignatures拡張とDeriveAnyClass拡張を組み合わせる方法です。

ライブラリーを定義する側でDefaultSignatures拡張を使うと、型クラスのメソッドのデフォルト定義に追加で型制約をつけられるようになります。

```haskell
{-# LANGUAGE DefaultSignatures #-}

class ConstructorName a where
  constructorName :: a -> String

  default constructorName :: (Generic a, ConstructorName' (Rep a)) => a -> String
  constructorName x = constructorName' (from x)
```

ライブラリーを利用する側は、空のインスタンス定義を書くか、

```haskell
data T = A String Int | B | C Char
       deriving Generic

instance ConstructorName T
```

DeriveAnyClass拡張を使って `deriving` 節で導出します。

```haskell
data T = A String Int | B | C Char
       deriving (Generic, ConstructorName)
```

もう一つの方法は、DerivingVia拡張と、GHC 9.4で追加された `Generically` 型（newtype wrapper）を使う方法です。こちらの方法では、ライブラリー側では `Generically` 型に対するインスタンス定義を用意します。

```haskell
{-# LANGUAGE UndecidableInstances #-}

instance (Generic a, ConstructorName' (Rep a)) => ConstructorName (Generically a) where
  constructorName (Generically x) = constructorName' (from x)
```

利用する側は、DerivingVia拡張を使って `Generically` 型経由でインスタンスを導出します。

```haskell
{-# LANGUAGE DerivingVia #-}

data T = A String Int | B | C Char
  deriving Generic
  deriving ConstructorName via Generically (U a)
```

# もっと知りたい方へ

GHCのジェネリックプログラミングについてもっと詳しく知りたい型は、公式のドキュメントを読むと良いでしょう：

* [Generic programming — Glasgow Haskell Compiler User's Guide](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/generics.html)
* [GHC.Generics](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)
* [GHC.Generics - HaskellWiki](https://wiki.haskell.org/GHC.Generics)

注意点として、型に `Generic` を実装するということはデータ型の中身を曝け出すということです。データ構築子を公開する型なら問題ないでしょうが、抽象データ型の場合は `Generic` を使うべきではないでしょう。

また、ジェネリックプログラミングで効率の良いコードを生成するには、インライン化が望ましいです。適宜 `INLINE` プラグマを使うべきかもしれません。
