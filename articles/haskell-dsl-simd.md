---
title: "HaskellでEDSLを作る：SIMD編"
emoji: "💨"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell, dsl, simd]
published: false
---

シリーズ：

* [HaskellでEDSLを作る：atomicModifyIORef編 〜自動微分を題材に〜](haskell-dsl-atomicmodifyioref)
* [HaskellでEDSLを作る：StableName編 〜共有の回復〜](haskell-dsl-stablename)
* [HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜](haskell-dsl-llvm)
* HaskellでEDSLを作る：SIMD編（この記事）

[HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜](haskell-dsl-llvm)ではLLVMを使って自動ベクトル化を行い、SIMD命令を活用しました。一方で、GHCにはSIMD命令を直接使うプリミティブ型と関数があります。これらを活用できないでしょうか？

例によってサンプルコードは[haskell-dsl-example/simd](https://github.com/minoki/haskell-dsl-example/tree/main/simd)に置いています。

## SIMDとは

SIMDとはsingle instruction, multiple dataの略で、一つの命令で複数のデータを扱う技術を指します。典型的には、複数の値を保持できるSIMDレジスターがあり、専用の命令を使うとそれらをまとめて処理できます。

Haskell風に書くと、こういうデータ型があると考えてください：

```haskell
data FloatX4 = FloatX4 !Float !Float !Float !Float
data DoubleX2 = DoubleX2 !Double !Double
```

そして、一つの命令で `FloatX4` 同士の要素ごとの足し算ができます。Haskell風のコードで書くと、

```haskell
plusFloatX4 :: FloatX4 -> FloatX4 -> FloatX4
plusFloatX4 (FloatX4 a b c d) (FloatX4 a' b' c' d') = FloatX4 (a + a') (b + b') (c + c') (d + d')
```

という処理がCPUに組み込まれていて1命令で実行できる感じです。

典型的には、SIMDレジスターの幅はアーキテクチャーによって128ビットあるいは256ビットあるいは512ビットという風に決まっています。ですので、ベクトルあたりの要素数は型によって変わります。128ビット幅のレジスターを持つx86 SSEやArm NEONであれば、`Float` は4要素、`Double` なら2要素、とという具合です。

## GHCのSIMDプリミティブ

前に[Haskell/GHCのSIMDについて考える](https://blog.miz-ar.info/2023/08/haskell-simd/)という記事を書いたので、そちらも参照してください。

GHCは7.8.1以降でSIMDプリミティブを実装しています。具体的には、以下のプリミティブ型と、

```haskell
-- 128ビット版
type Int8X16#
type Int16X8#
type Int32X4#
type Int64X2#
type Word8X16#
type Word16X8#
type Word32X4#
type Word64X2#
type FloatX4#
type DoubleX2#

-- 256ビット版、512ビット版もある
```

いくつかのプリミティブ関数です：

```haskell
packFloatX4# :: (# Float#, Float#, Float#, Float# #) -> FloatX4#
unpackFloatX4# :: FloatX4# -> (# Float#, Float#, Float#, Float# #)
insertFloatX4# :: FloatX4# -> Float# -> Int# -> FloatX4#
broadcastFloatX4# :: Float# -> FloatX4#
plusFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
minusFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
timesFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
divideFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
negateFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
indexFloatX4Array# :: ByteArray# -> Int# -> FloatX4#
readFloatX4Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, FloatX4# #)
writeFloatX4Array# :: MutableByteArray# s -> Int# -> FloatX4# -> State# s -> State# s
indexFloatX4OffAddr# :: Addr# -> Int# -> FloatX4#
readFloatX4OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, FloatX4# #)
writeFloatX4OffAddr# :: Addr# -> Int# -> FloatX4# -> State# s -> State# s
indexFloatArrayAsFloatX4# :: ByteArray# -> Int# -> FloatX4#
readFloatArrayAsFloatX4# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, FloatX4# #)
writeFloatArrayAsFloatX4# :: MutableByteArray# s -> Int# -> FloatX4# -> State# s -> State# s
indexFloatOffAddrAsFloatX4# :: Addr# -> Int# -> FloatX4#
readFloatOffAddrAsFloatX4# :: Addr# -> Int# -> State# s -> (# State# s, FloatX4# #)
writeFloatOffAddrAsFloatX4# :: Addr# -> Int# -> FloatX4# -> State# s -> State# s
```

256ビット、512ビットのベクトル型は常に256ビット、512ビットのSIMDレジスターに対応します。つまり、x86だとAVXやAVX-512を必要とし、「SSEレジスター2本」にはなりません。

注意しないといけないのは、これらは使えるバックエンドに制限があります。具体的には、

* x86: GHC 7.8以降のLLVMバックエンド、またはGHC 9.12以降のNCGバックエンド（一部の型のみ）
* AArch64: GHC 9.8以降のLLVMバックエンド

が必要です。

ちなみに、GHC 9.12ではいくつかプリミティブ関数が追加されました。

まあ、アレですね。型や関数の名前が `#` で終わっているのは上級者向けの目印です。一般ユーザーが使うには、何らかの形でこれらをラップする必要があります。

## どうラップするか

### 既存のラッパー

HackageにSIMDプリミティブのラッパーがいくつか上がっています。

[simd](https://hackage.haskell.org/package/simd)パッケージは、以下のようなインターフェースを提供しています：

```haskell
module Data.SIMD.SIMD4 where

class SIMD4 a where
  data X4 a
  plusX4 :: X4 a -> X4 a -> X4 a
  indexArrayAsX4 :: ByteArray -> Int -> X4 a
  broadcastX4 :: a -> X4 a
  -- 略

class SIMD4 a => SIMD4Float a where
  divideX4 :: X4 a -> X4 a -> X4 a

instance (Show a, SIMD4 a) => Show (X4 a)
instance (Num a, SIMD4 a) => Num (X4 a)
instance (Fractional a, SIMD4Float a) => Fractional (X4 a)

instance SIMD4 Float
instance SIMD4 Double
-- 略

vectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector a -> VU.Vector (X4 a)
unVectorizeUnboxedX4 :: (SIMD4 a, VU.Unbox a) => VU.Vector (X4 a) -> VU.Vector a
vectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector a -> VS.Vector (X4 a)
unVectorizeStorableX4 :: (SIMD4 a, Storable a, Storable (X4 a)) => VS.Vector (X4 a) -> VS.Vector a
```

要素数に応じた型族を使っているのが特徴です。ただ、このライブラリーでは `X4 Double` は `DoubleX4#` のラッパーになっています。AVXがないと使えないってことですね。

`Vector a` を処理する際は `Vector (X4 a)` にキャストするようですが、端数があると処理できません。

もう一つ、[primitive-simd](https://hackage.haskell.org/package/primitive-simd)パッケージは（型族ではない）個別の型を提供しています。

```haskell
module Data.Primitive.SIMD where

class (Num v, Real (Elem v)) => SIMDVector v where
  type Elem v
  type ElemTuple v
  broadcastVector :: Elem v -> v
  mapVector :: (Elem v -> Elem v) -> v -> v
  - ...

data FloatX4
data FloatX8
data FloatX16
data DoubleX2
data DoubleX4
data DoubleX8
-- ...
```

`DoubleX4` 型は `DoubleX4#` 型のラッパーかというとそういうわけでもなく、デフォルトでは `DoubleX2#` を2つ使い、パッケージフラグで使用するベクトルの最大長をno-vec, vec256, vec512という風に選択することができます。

スカラーの配列 `Vector a` の処理に関するサポートは特になさそうです。

そういうわけで、simdパッケージもprimitive-simdパッケージも一長一短です。特に、一般の `Vector Float` などを処理する手段がなさそうなのは欠点だと思います。なので、ここでは新しいラッパーを作ることにします。

### 新しく作るラッパー

まず、simdパッケージの「型族を使う」というアイディアは良さそうなので採用したいです。つまり、ベクトルのビット数ではなく、要素数に応じて型族を作るというアイディアです。また、primitive-simdパッケージの「ベクトル長が短いアーキテクチャでは `DoubleX4` は `DoubleX2#` を2つ使って表現する」というアイディアも良さそうなので採用します。

```haskell
-- 使えるベクトル長が128ビットの場合
data X4 a
data instance X4 Float = FloatX4 FloatX4#
data instance X4 Double = DoubleX2X2 DoubleX2# DoubleX2#
```

型クラスのインスタンスについても考えます。simdパッケージでは `SIMD4` というでかいクラスがあり、そこに四則演算も含めていました。しかし、四則演算は個別の型クラスに分けた方が良いと私は思います。一つ考えられるのは、次のようなクラス構成です：

```haskell
class BroadcastX4 a where
  broadcastX4 :: a -> X4 a

instance BroadcastX4 Float
instance BroadcastX4 Double

class BroadcastX4 a => NumX4 a where
  plusX4 :: X4 a -> X4 a -> X4 a
  -- 略

instance NumX4 Float
instance NumX4 Double
```

が、`X4` の部分は他の要素数も考慮して一般化するべきかもしれません：

```haskell
class Broadcast f a where
  broadcast :: a -> f a

instance Broadcast X4 Float
instance Broadcast X4 Double

class MonoMap f a where
  monoMap :: (a -> a) -> f a -> f a

instance MonoMap X4 Float
instance MonoMap X4 Double

class Broadcast f a => NumF f a where
  plusF :: f a -> f a -> f a
  minusF :: f a -> f a -> f a
  timesF :: f a -> f a -> f a
  absF :: f a -> f a
  signumF :: f a -> f a
  negateF :: f a -> f a

instance NumF X4 Float
instance NumF X4 Double

instance (NumF X4 a, Num a) => Num (X4 a)
```

`Data.Vector.Storable.Vector a` を扱えるように、`Storable` 系のクラスとインスタンスも用意します。

```haskell
class Storable a => StorableF f a where
  peekElemOffF :: Ptr a -> Int -> IO (f a)
  pokeElemOffF :: Ptr a -> Int -> f a -> IO ()

instance StorableF X4 Float
instance StorableF X4 Double
```

storable vectorに対して「4要素ごとに処理して」「端数も処理する」map関数は次のように書けます：

```haskell
mapStorable :: (StorableF X4 a, StorableF X4 b) => (X4 a -> X4 b) -> (a -> b) -> VS.Vector a -> VS.Vector b
mapStorable fv f !v = unsafePerformIO $ do
  let !n = VS.length v
  !result <- VSM.unsafeNew n
  VS.unsafeWith v $ \ !inputPtr ->
    VSM.unsafeWith result $ \ !resultPtr -> do
      let loopVector !i | i + 4 > n = loopScalar i
                        | otherwise = do
                          !a <- peekElemOffF inputPtr i
                          pokeElemOffF resultPtr i (fv a)
                          loopVector (i + 4)
          loopScalar !i | i >= n = pure ()
                        | otherwise = do
                          !a <- peekElemOff inputPtr i
                          pokeElemOff resultPtr i (f a)
                          loopScalar (i + 1)
      loopVector 0
  VS.unsafeFreeze result
{-# INLINE mapStorable #-}
```

簡単のため、fusion系の最適化は実装していません。

使う側は

```haskell
mapStorable (\v -> (v + 1)^(10 :: Int)) (\x -> (x + 1)^(10 :: Int)) (VS.fromList [0..10] :: VS.Vector Float)
```

という風になります。

### 端数を統一的に扱う

上記の `mapStorable` 関数には、SIMDによる4要素ごとの処理と、端数の処理を別々に渡す必要があります。どうせ演算子オーバーロードで同じように書けるので、無駄ですね。

例えば `(x + 1)^10` という関数なら関数の型は `Num a => a -> a` と一般化できるので、

```haskell
mapStorable' :: (StorableF X4 a, Num a, NumF X4 a) => (forall v. Num v => v -> v) -> VS.Vector a -> VS.Vector a
mapStorable' f = mapStorable f f
```

とできます。しかし、これでは `Num` 制約を特別扱いすることになってしまい、汎用性が低いです。

そこで、別のやり方を考えます。具体的には、端数部分を処理する関数の型に細工を加えて、`Identity a -> Identity b` と考えます。すると、ベクトル部分の関数も端数部分の関数も `f a -> f b` という形になり、都合がよさそうです。つまり、`X4` と `Identity` をインスタンスとするような何らかの型クラス `SIMD` によって

```haskell
mapStorable' :: (StorableF X4 a, StorableF X4 b) => (forall f. SIMD f => f a -> f b) -> VS.Vector a -> VS.Vector b
mapStorable' f = mapStorable f (runIdentity . f . Identity)
```

とするのです。

`SIMD f` 制約がどういうものであるべきか考えます。例えば、`f` は `Num (f Float)` や `Num (f Double)` などのインスタンスを持つ必要があります。理想を言えば任意の `Num a` に対して `Num (f a)` があって欲しいです。つまり

```haskell
class (forall a. Num a => Num (f a)) => SIMD f
instance SIMD Identity
instance SIMD X4
```

となって欲しいですが、これは不可能です。`Float` と `Double` に対してしか `Num (X4 a)` は定義されていませんからね。ですので、それも制約に加えます。

```haskell
class (forall a. (Num a, NumF X4 a) => Num (f a)) => SIMD f
instance SIMD Identity
instance SIMD X4
```

8要素、16要素のSIMD型を実装すると、`(Num a, NumF X4 a)` の部分は `(Num a, NumF X4 a, NumF X8 a, NumF X16 a)` という風に増えていきます。ですので、この部分をまとめて

```haskell
type NumElement a = (Num a, NumF X4 a) -- X8, X16も増えるかもしれない
class (forall a. NumElement a => Num (f a)) => SIMD f
instance SIMD Identity
instance SIMD X4
```

とします。`Num` 以外のクラスに対応させる場合は

```haskell
type NumElement a = (Num a, NumF X4 a) -- X8, X16も増えるかもしれない
type FractionalElement a = (Fractional a, FractionalF X4 a)
type FloatingElement a = (Floating a, FloatingF X4 a)
type MonoidElement a = (Monoid a, MonoidF X4 a)
class ( forall a. NumElement a => Num (f a)
      , forall a. FractionalElement a => Fractional (f a)
      , forall a. FloatingElement a => Floating (f a)
      , forall a. MonoidElement a => Monoid (f a)
      ) => SIMD f
instance SIMD Identity
instance SIMD X4
```

という風になります。

利用者側は

```haskell
f :: Num a => a -> a
f x = (x + 1)^10

mapStorable' f (VS.fromList [0..10])
```

と書いても良いですし、

```haskell
g :: (SIMD f, NumElement a) => f a -> f a
g x = (x + 1)^10

mapStorable' g (VS.fromList [0..10])
```

と書くこともできます。

## サンプルコードを動かす

サンプルコードは[haskell-dsl-example/simd](https://github.com/minoki/haskell-dsl-example/tree/main/simd)に置いています。

```
$ git clone https://github.com/minoki/haskell-dsl-example.git
$ cd haskell-dsl-example/simd
```

私のメイン環境はApple Silicon Macなので、GHC 9.8以降でLLVMバックエンドが使えるようにしておく必要があります。[Haskellの環境構築2023](https://zenn.dev/mod_poppo/articles/haskell-setup-2023)にも書きましたが、Homebrewを使っている人は

```
$ OPT=$(brew --prefix llvm@15)/bin/opt LLC=$(brew --prefix llvm@15)/bin/llc ghcup install ghc --force 9.8.4
```

という感じでGHCを入れ直す必要があるかもしれません。

LLVMバックエンドを使うにはGHCに `-fllvm` オプションを渡す必要があります。Cabalなら `--ghc-options=-fllvm` です。毎回指定するのはだるいので、最初に

```
$ cabal configure -w ghc-9.8.4 --ghc-options=-fllvm
```

を実行しておきましょう。このコマンドによって `cabal.project.local` というファイルができ、設定内容が書き込まれます。

`cabal run` で

```
$ cabal run
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.8247526e8,1.0737418e9,3.4867843e9,1.0e10,2.5937424e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.8247526e8,1.0737418e9,3.4867843e9,1.0e10,2.5937424e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.8247526e8,1.0737418e9,3.4867843e9,1.0e10,2.5937424e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.8247526e8,1.0737418e9,3.4867843e9,1.0e10,2.5937424e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.82475249e8,1.073741824e9,3.486784401e9,1.0e10,2.5937424601e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.82475249e8,1.073741824e9,3.486784401e9,1.0e10,2.5937424601e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.82475249e8,1.073741824e9,3.486784401e9,1.0e10,2.5937424601e10]
[1.0,1024.0,59049.0,1048576.0,9765625.0,6.0466176e7,2.82475249e8,1.073741824e9,3.486784401e9,1.0e10,2.5937424601e10]
```

という風な内容が出てきたら正常です。

ベンチマークに使うコードは[simd/benchmark/Main.hs](https://github.com/minoki/haskell-dsl-example/blob/main/simd/benchmark/Main.hs)です。

### Apple M4 Proでの結果

GHC 9.8.4 / LLVMバックエンドでの結果を載せておきます。

```
$ cabal bench -w ghc-9.8.4 --ghc-options=-fllvm -O2
benchmarking Float/f/scalar
time                 3.206 μs   (3.200 μs .. 3.211 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.207 μs   (3.201 μs .. 3.215 μs)
std dev              22.70 ns   (15.40 ns .. 37.09 ns)

benchmarking Float/f/vector
time                 892.3 ns   (889.6 ns .. 894.8 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 893.0 ns   (890.9 ns .. 894.5 ns)
std dev              6.179 ns   (4.750 ns .. 7.933 ns)

benchmarking Float/f/vector (unified)
time                 897.8 ns   (895.3 ns .. 900.2 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 898.7 ns   (896.3 ns .. 902.2 ns)
std dev              9.800 ns   (6.691 ns .. 16.75 ns)

benchmarking Float/g/scalar
time                 3.215 μs   (3.211 μs .. 3.219 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.213 μs   (3.210 μs .. 3.220 μs)
std dev              15.61 ns   (8.731 ns .. 28.17 ns)

benchmarking Float/g/vector
time                 893.0 ns   (890.7 ns .. 895.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 894.2 ns   (892.1 ns .. 896.7 ns)
std dev              7.870 ns   (6.011 ns .. 11.68 ns)

benchmarking Float/g/vector (unified)
time                 901.2 ns   (897.6 ns .. 904.0 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 898.0 ns   (895.9 ns .. 899.9 ns)
std dev              6.522 ns   (5.186 ns .. 8.885 ns)

benchmarking Double/f/scalar
time                 3.185 μs   (3.179 μs .. 3.193 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.188 μs   (3.184 μs .. 3.199 μs)
std dev              21.43 ns   (11.81 ns .. 36.86 ns)

benchmarking Double/f/vector
time                 3.649 μs   (3.621 μs .. 3.674 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.662 μs   (3.640 μs .. 3.674 μs)
std dev              54.79 ns   (39.52 ns .. 73.07 ns)
variance introduced by outliers: 13% (moderately inflated)

benchmarking Double/f/vector (unified)
time                 3.671 μs   (3.652 μs .. 3.688 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.672 μs   (3.654 μs .. 3.683 μs)
std dev              45.43 ns   (27.65 ns .. 83.19 ns)

benchmarking Double/g/scalar
time                 3.221 μs   (3.214 μs .. 3.228 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.220 μs   (3.215 μs .. 3.225 μs)
std dev              17.02 ns   (12.05 ns .. 28.72 ns)

benchmarking Double/g/vector
time                 1.658 μs   (1.656 μs .. 1.662 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.662 μs   (1.659 μs .. 1.673 μs)
std dev              16.55 ns   (4.710 ns .. 36.25 ns)

benchmarking Double/g/vector (unified)
time                 1.661 μs   (1.658 μs .. 1.664 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.658 μs   (1.656 μs .. 1.662 μs)
std dev              9.381 ns   (4.704 ns .. 17.62 ns)
```

`Float` の方は3.206/0.8923≈3.59でした。3倍以上4倍未満です。4並列なのでこんなもんですかね。

`Double` の方は、`^` を使って計算した方はあまり速度が向上していません。冪乗を展開した方は3.221/1.658≈1.94でした。おおよそ2倍、2並列なので妥当ですね。

#### Ryzen 9 7940HSでの結果（LLVMバックエンド）

AVX-512が使えるRyzen 9 7940HS（Zen 4）での結果も載せておきます。OSはWSL2上のUbuntu 22.04です。

```
$ cabal bench -w ghc-9.8.4 --ghc-options=-fllvm -O2
benchmarking Float/f/scalar
time                 4.190 μs   (4.154 μs .. 4.231 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.201 μs   (4.172 μs .. 4.261 μs)
std dev              139.4 ns   (77.76 ns .. 238.0 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking Float/f/vector
time                 1.288 μs   (1.276 μs .. 1.302 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.293 μs   (1.282 μs .. 1.308 μs)
std dev              42.72 ns   (30.33 ns .. 65.09 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Float/f/vector (unified)
time                 1.290 μs   (1.279 μs .. 1.301 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.291 μs   (1.280 μs .. 1.304 μs)
std dev              42.05 ns   (30.81 ns .. 60.24 ns)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Float/g/scalar
time                 4.187 μs   (4.148 μs .. 4.235 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.200 μs   (4.165 μs .. 4.257 μs)
std dev              149.5 ns   (109.6 ns .. 213.1 ns)
variance introduced by outliers: 46% (moderately inflated)

benchmarking Float/g/vector
time                 1.359 μs   (1.350 μs .. 1.370 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.363 μs   (1.355 μs .. 1.375 μs)
std dev              32.39 ns   (21.39 ns .. 48.15 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking Float/g/vector (unified)
time                 1.366 μs   (1.354 μs .. 1.378 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.365 μs   (1.356 μs .. 1.378 μs)
std dev              35.55 ns   (26.86 ns .. 46.75 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Double/f/scalar
time                 4.214 μs   (4.183 μs .. 4.246 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.225 μs   (4.195 μs .. 4.280 μs)
std dev              131.8 ns   (87.25 ns .. 222.4 ns)
variance introduced by outliers: 39% (moderately inflated)

benchmarking Double/f/vector
time                 2.159 μs   (2.140 μs .. 2.182 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 2.208 μs   (2.180 μs .. 2.253 μs)
std dev              116.2 ns   (77.12 ns .. 170.5 ns)
variance introduced by outliers: 67% (severely inflated)

benchmarking Double/f/vector (unified)
time                 2.146 μs   (2.131 μs .. 2.165 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.153 μs   (2.138 μs .. 2.172 μs)
std dev              57.85 ns   (44.10 ns .. 86.56 ns)
variance introduced by outliers: 34% (moderately inflated)

benchmarking Double/g/scalar
time                 4.192 μs   (4.159 μs .. 4.225 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.195 μs   (4.171 μs .. 4.234 μs)
std dev              100.5 ns   (76.68 ns .. 130.0 ns)
variance introduced by outliers: 27% (moderately inflated)

benchmarking Double/g/vector
time                 2.187 μs   (2.171 μs .. 2.205 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.192 μs   (2.177 μs .. 2.215 μs)
std dev              61.57 ns   (44.70 ns .. 88.71 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Double/g/vector (unified)
time                 2.181 μs   (2.159 μs .. 2.207 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.193 μs   (2.176 μs .. 2.225 μs)
std dev              75.01 ns   (45.34 ns .. 123.4 ns)
variance introduced by outliers: 46% (moderately inflated)
```

`Float` の方は4.190/1.288≈3.25でした。これも3倍以上4倍未満です。4並列なのでこんなもんですかね。

こちらも、`Double` の方は、`^` を使って計算した方はあまり速度が向上していません。冪乗を展開した方は4.192/2.181≈1.92でした。おおよそ2倍、2並列なので妥当ですね。

#### Ryzen 9 7940HSでの結果（NCGバックエンド）

GHC 9.12で実装されたNCGバックエンドでのSIMDサポートも試してみましょう。

```
$ cabal bench -w ghc-9.12.0.20241128 --builddir=dist-ncg -O2 --allow-newer
benchmarking Float/f/scalar
time                 37.44 μs   (37.14 μs .. 37.76 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 37.34 μs   (37.09 μs .. 37.86 μs)
std dev              1.080 μs   (665.7 ns .. 1.866 μs)
variance introduced by outliers: 29% (moderately inflated)

benchmarking Float/f/vector
time                 7.048 μs   (6.981 μs .. 7.101 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.047 μs   (6.995 μs .. 7.129 μs)
std dev              212.2 ns   (133.8 ns .. 324.3 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Float/f/vector (unified)
time                 7.014 μs   (6.959 μs .. 7.071 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.034 μs   (6.988 μs .. 7.108 μs)
std dev              186.6 ns   (146.7 ns .. 252.1 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking Float/g/scalar
time                 13.74 μs   (13.61 μs .. 13.88 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 13.54 μs   (13.40 μs .. 13.66 μs)
std dev              458.4 ns   (356.4 ns .. 604.5 ns)
variance introduced by outliers: 40% (moderately inflated)

benchmarking Float/g/vector
time                 1.408 μs   (1.388 μs .. 1.441 μs)
                     0.996 R²   (0.993 R² .. 0.998 R²)
mean                 1.478 μs   (1.448 μs .. 1.529 μs)
std dev              136.4 ns   (83.51 ns .. 203.8 ns)
variance introduced by outliers: 87% (severely inflated)

benchmarking Float/g/vector (unified)
time                 1.180 μs   (1.171 μs .. 1.191 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.176 μs   (1.169 μs .. 1.187 μs)
std dev              32.02 ns   (21.83 ns .. 50.35 ns)
variance introduced by outliers: 36% (moderately inflated)

benchmarking Double/f/scalar
time                 38.91 μs   (38.57 μs .. 39.29 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 38.59 μs   (38.25 μs .. 39.06 μs)
std dev              1.361 μs   (1.070 μs .. 1.724 μs)
variance introduced by outliers: 38% (moderately inflated)

benchmarking Double/f/vector
time                 8.440 μs   (8.366 μs .. 8.537 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 8.456 μs   (8.399 μs .. 8.541 μs)
std dev              232.4 ns   (160.3 ns .. 309.1 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking Double/f/vector (unified)
time                 8.149 μs   (8.068 μs .. 8.241 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 8.199 μs   (8.127 μs .. 8.305 μs)
std dev              292.6 ns   (229.5 ns .. 450.0 ns)
variance introduced by outliers: 44% (moderately inflated)

benchmarking Double/g/scalar
time                 13.86 μs   (13.73 μs .. 13.98 μs)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 13.71 μs   (13.62 μs .. 13.85 μs)
std dev              374.7 ns   (285.7 ns .. 525.6 ns)
variance introduced by outliers: 30% (moderately inflated)

benchmarking Double/g/vector
time                 2.641 μs   (2.621 μs .. 2.664 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.652 μs   (2.631 μs .. 2.682 μs)
std dev              83.22 ns   (60.53 ns .. 118.6 ns)
variance introduced by outliers: 41% (moderately inflated)

benchmarking Double/g/vector (unified)
time                 2.221 μs   (2.201 μs .. 2.244 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.231 μs   (2.213 μs .. 2.267 μs)
std dev              80.59 ns   (46.99 ns .. 135.7 ns)
variance introduced by outliers: 48% (moderately inflated)
```

`Float` の方は `f` が37.44/7.014≈5.34倍、`g` が13.74/1.180≈11.6倍でした。4並列なのに4倍以上速度向上しています。

`Double` の方は、`f` は38.91/8.149≈4.77倍、`g` は13.86/2.221=6.24倍でした。これも2並列なのに2倍以上速度向上しています。

NCGの方でSIMDの利用による速度向上幅が大きいということは、スカラーのコードの最適化が足りないのでしょうか。

## 一般論

これまで何回か、HaskellでEDSLを作る際に役立つ手法を見てきました。いずれも、演算子オーバーロードで普通のHaskellっぽく書けるようになっています。使った型を見ると、自動微分では `Reverse s a`、StableName編では `Exp` または `Exp a` の形をしていて、SIMDでは `X4 a` という形をしています。

これらは多くが `f a` という形をしています（`a` は `Float` や `Double` など、要素の型）。まあ演算子オーバーロードする都合上 `f a` の形になるのは当然なんですが、何かの意味を見出すことはできるでしょうか？
