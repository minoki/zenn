---
title: "データ型の昇格（DataKinds拡張）"
---

現代のGHCでは、DataKinds拡張を有効にすることで任意のデータ型をカインドとして使用することができます（データ型のカインドへの昇格）。この時、データ型のデータ構築子も型構築子へ昇格されます。

例えば、自然数を表すデータ型を次のように定義したとしましょう：

```haskell
data PeanoNat = Zero
              | Succ PeanoNat
```

ペアノ流の自然数の定義に馴染みのない方のために解説しておくと、`Zero` は名前の通りゼロを、`Succ x` は `x` に1加えた自然数（`x` の後続者 (successor)）を表します。この流儀では例えば3という自然数は `Succ (Succ (Succ Zero))` と表現されます。

通常のHaskellでは、ここで定義される型は `PeanoNat` だけ

```haskell
-- 型構築子とそのカインド
PeanoNat :: Type
```

ですが、DataKinds拡張が有効な場合、 `PeanoNat` がカインドとしても使えるようになる他、

```haskell
-- 型構築子とそのカインド
'Zero :: PeanoNat
'Succ :: PeanoNat -> PeanoNat
```

という型構築子も定義されます。昇格によって得られた型構築子の名前にはシングルクォート `'` がつきますが、曖昧さのない場合は省略可能です。

<!-- 図 -->

```haskell
ghci> :kind PeanoNat
PeanoNat :: *
ghci> :kind 'Zero
'Zero :: PeanoNat
ghci> :kind 'Succ
'Succ :: PeanoNat -> PeanoNat
ghci> :kind Succ  -- Succ という型が定義されていない場合は頭の ' を省略可能
Succ :: PeanoNat -> PeanoNat
```

DataKindsの使用例は幽霊型のところで書いた

```haskell
-- 長さ n のリスト型
newtype SizedList (n :: PeanoNat) a = SizedList [a]
```

です。ここで登場する `PeanoNat` カインドというのは、 `PeanoNat` 型を昇格して得られたカインドです。

「データ型とデータ構築子を昇格させる」というとすごそうに聞こえますが、逆に言うとDataKinds拡張ができるのはそれだけです。DataKinds拡張を使っても依存型のように型と項を混ぜて使えるようにはなりません。

DataKinds拡張でできることは、DataKinds拡張を使わなくても模倣できます。例えば、自然数と `SizedList` の例であれば、

```haskell
-- 通常の型定義
data Zero
data Succ a

newtype SizedList n a = ...
-- n は Zero と Succ から構成される型と約束する
```

という通常のHaskellコードで似たようなことができます。（このコードでは `SizedList Bool Int` のような「型レベル自然数が期待される箇所にそうじゃない型を適用できてしまう」という問題を防げないのでDataKinds拡張ができたわけですが。）

データ型とそれに対応するカインドを結びつけ、依存型を模倣するには、型クラスやGADTといった機構の力を借りる必要があります。

その前に、型レベル関数についてもう少し詳しく扱います。

ちなみに、 `newtype SizedList (n :: PeanoNat) a` という風に型の定義時にカインド注釈 (`:: PeanoNat`) を書けるのはKindSignaturesというGHC拡張拡張の機能です。
