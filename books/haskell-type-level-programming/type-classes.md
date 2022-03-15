---
title: "型レベル計算の結果を実行時に利用する：型クラス"
---

これまではGHCiの `:kind!` コマンドで型レベル演算の結果を確認してきました。しかし、実用的なプログラムを書く上では型レベル演算の結果をプログラムの実行の過程で利用したいものです。

Haskellには、型レベル演算の結果をプログラムの実行時に利用する手段があります。そう、**型クラス**です。

例として、先ほど定義した `PeanoNat` カインドの型を `Integer` に変換する型クラスを書いてみます。

```haskell
class PeanoNatToInteger (n :: PeanoNat) where
  peanoNatToInteger :: Proxy n -> Integer

instance PeanoNatToInteger 'Zero where
  peanoNatToInteger _ = 0

instance PeanoNatToInteger n => PeanoNatToInteger ('Succ n) where
  -- 要 ScopedTypeVariables 拡張
  peanoNatToInteger _ = 1 + peanoNatToInteger (Proxy :: Proxy n)
```

`peanoNatToInteger` 関数の型を単に `Integer` としてしまうと型の曖昧性でエラーとなるので、 `Proxy` を引数に取っています。GHCiで動作を確認してみましょう。

```haskell
ghci> peanoNatToInteger (Proxy :: Proxy ('Succ 'Zero))
1
ghci> peanoNatToInteger (Proxy :: Proxy (Plus2 ('Succ 'Zero)))
3
ghci> peanoNatToInteger (Proxy :: Proxy (Add ('Succ 'Zero) (Plus2 ('Succ 'Zero))))
4
```

`peanoNatToInteger` を内部で利用したい多相関数には、制約部にその型クラスを記述する必要があります。

```haskell
lengthOfSizedList :: forall n. PeanoNatToInteger n => SizedList n a -> Integer
lengthOfSizedList _ = peanoNatToInteger (Proxy :: Proxy n)
```

別の例として、自然数の大小比較結果を実行時の値として得る型クラスは次のように書けます：

```haskell
-- 要 MultiParamTypeClasses, ScopedTypeVariables
class CompareNat (n :: PeanoNat) (m :: PeanoNat) where
  compareNat :: Proxy n -> Proxy m -> Ordering

instance CompareNat 'Zero 'Zero where
  compareNat _ _ = EQ

instance CompareNat 'Zero ('Succ m) where
  compareNat _ _ = LT

instance CompareNat ('Succ n) 'Zero where
  compareNat _ _ = GT

instance CompareNat n m => CompareNat ('Succ n) ('Succ m) where
  compareNat _ _ = compareNat (Proxy :: Proxy n) (Proxy :: Proxy m)
```

ただ、処理ごとに型クラスを定義していると大変です。複数の処理を組み合わせたい関数の制約がえらいことになります。ひとつのクラスにそれぞれの処理に対応するメソッドを押し込めるという手もありますが、他のパッケージで処理を定義するには結局型クラスを複数定義することになります。

この問題の解決方法は後で見ます。
