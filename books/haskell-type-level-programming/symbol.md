---
title: "応用：GHCの型レベル文字列（Symbolカインド）"
---

# 型レベル文字列

GHCではDataKinds拡張の下で型レベル文字列を扱うことができます。型レベル文字列はカインド `Symbol` を持ちます。

```haskell
ghci> :set -XDataKinds
ghci> :m + GHC.TypeLits
ghci> :kind "foo"
"foo" :: Symbol
```

関連する型と関数は `GHC.TypeLits` モジュールで定義されています。

```haskell
module GHC.TypeLits where

data Symbol

class KnownSymbol (n :: Symbol)

symbolVal :: KnownSymbol n => proxy n -> String
```

型レベル文字列の内容が実行時に利用できることを表す型クラスは `KnownSymbol` で、型レベル文字列を実行時に利用する関数は `symbolVal` です。

実行時の文字列を型レベル文字列に持ち上げる（`KnownSymbol` 制約のついた型に変換する）には `someSymbolVal` 関数と `SomeSymbol` 型を使います。

```haskell
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)
someSymbolVal :: String -> SomeSymbol
```

型レベル文字列が等しいかを実行時に判定するには `sameSymbol` 関数を使います。これは二つの文字列が等しければその「証拠」 `Refl` を `Just` に包んで返し、等しくなければ `Nothing` を返します。

```haskell
sameSymbol :: (KnownSymbol a, KnownSymbol b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
```

比較も行えます。

```haskell
type (<=) :: Symbol -> Symbol -> Constraint -- GHC 9.2以降
type (<=?) :: Symbol -> Symbol -> Bool -- GHC 9.2以降
type CmpSymbol :: Symbol -> Symbol -> Ordering
cmpSymbol :: (KnownSymbol a, KnownSymbol b) => proxy1 a -> proxy2 b -> OrderingI a b -- GHC 9.2以降
```

文字列特有の演算として、「連結」と「先頭への文字の追加」、「先頭文字と残りへの分解」もあります。

```haskell
type AppendSymbol :: Symbol -> Symbol -> Symbol
type ConsSymbol :: Char -> Symbol -> Symbol -- GHC 9.2以降
type UnconsSymbol :: Symbol -> Maybe (Char, Symbol) -- GHC 9.2以降
```

```haskell
ghci> :kind! AppendSymbol "foo" "bar"
AppendSymbol "foo" "bar" :: Symbol
= "foobar"
ghci> :kind! ConsSymbol 'a' "bc"
ConsSymbol 'a' "bc" :: Symbol
= "abc"
ghci> :kind! UnconsSymbol "abc"
UnconsSymbol "abc" :: Maybe (Char, Symbol)
= 'Just '('a', "bc")
```

# 型レベル文字

`ConsSymbol` と `UnconsSymbol` で使われていますが、GHC 9.2以降では `Char` カインドを持つ型レベル文字を扱うことができます。`GHC.TypeLits` モジュールで基本的な型や関数が提供されています。使い方は型レベル自然数や型レベル文字列と同様です。

```haskell
module GHC.TypeLits where

class KnownChar (n :: Char)
charVal :: KnownChar n => proxy n -> Char
data SomeChar = forall n. KnownChar n => SomeChar (Proxy n)
someCharVal :: Char -> SomeChar
sameChar :: (KnownChar a, KnownChar b) => proxy1 a -> proxy2 b -> Maybe (a :~: b)
cmpChar :: (KnownChar a, KnownChar b) => proxy1 a -> proxy2 b -> OrderingI a b
type (<=) :: Char -> Char -> Constraint
type (<=?) :: Char -> Char -> Bool
type CmpChar :: Char -> Char -> Ordering
```

文字特有の演算として、先ほど述べた文字列操作の他に、自然数との相互変換もあります。

```haskell
type ConsSymbol :: Char -> Symbol -> Symbol
type UnconsSymbol :: Symbol -> Maybe (Char, Symbol)
type CharToNat :: Char -> Nat
type NatToChar :: Nat -> Char
```

`NatToChar` は部分関数で、範囲外（`0x110000` 以上）の値を与えられると「簡約されない」という動作になります。

```haskell
ghci> :kind! CharToNat '\x3042'
CharToNat '\x3042' :: Natural
= 12354
ghci> :kind! NatToChar 12354
NatToChar 12354 :: Char
= '\12354'
ghci> :kind! NatToChar 0xDC00
NatToChar 0xDC00 :: Char
= '\56320'
ghci> :kind! NatToChar 0x110000
NatToChar 0x110000 :: Char
= NatToChar 1114112
```

# 関連：オーバーロードされたラベル

型レベル文字列に関連するGHC拡張として、OverloadedLabelsを紹介します。

OverloadedLabels拡張を使うと、識別子の先頭に `#` をつけた `#honyarara` という形の式を使うことができます。この式は、`GHC.OverloadedLabels` モジュールで定義されている `fromLabel` 関数を使って `fromLabel @"honyarara"` という形に脱糖されます。

`fromLabel` は `IsLabel` クラスの唯一のメソッドです：

```haskell
module GHC.OverloadedLabels where

class IsLabel (x :: Symbol) a where
  fromLabel :: a
```

というわけで、 `#honyarara` は `IsLabel` クラスのインスタンスについてオーバーロードされた型を持ちます：

```haskell
ghci> :set -XOverloadedLabels
ghci> :type #honyarara
#honyarara :: GHC.OverloadedLabels.IsLabel "honyarara" a => a
```

実際にOverloadedLabels拡張を使ってみる最小限の例は次のようになるでしょう：

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
import GHC.TypeLits
import GHC.OverloadedLabels
import Data.Proxy

newtype T = T String deriving (Eq, Show)

instance KnownSymbol s => IsLabel s T where
  fromLabel = T (symbolVal (Proxy :: Proxy s))

main = print (#foo :: T)
```

GHC 9.6ではラベルの制限が緩和され、`#` の後に識別子だけではなく数字やシングルクォートで始まる文字の並びや、文字列リテラルを書くことができるようになりました。例：

```haskell
ghci> (#1 :: T)
T "1"
ghci> (#"foo" :: T)
T "foo"
ghci> (#"Hello world!" :: T)
T "Hello world!"
ghci> (#0xdeadbeef :: T)
T "0xdeadbeef"
ghci> (#'a' :: T)
T "'a'"
ghci> (#' :: T)
T "'"
ghci> (#'foo :: T)
T "'foo"
ghci> (#012'345'678 :: T)
T "012'345'678"
```
