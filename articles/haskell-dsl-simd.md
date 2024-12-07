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

注意しないといけないのは、これらは使えるバックエンドに制限があります。具体的には、

* x86: GHC 7.8以降のLLVMバックエンド、またはGHC 9.12以降のNCGバックエンド（一部の型のみ）
* AArch64: GHC 9.8以降のLLVMバックエンド

が必要です。

ちなみに、GHC 9.12ではいくつかプリミティブ関数が追加されました。

まあ、アレですね。型や関数の名前が `#` で終わっているのは上級者向けの目印です。一般ユーザーが使うには、何らかの形でこれらをラップする必要があります。

## どうラップするか

HackageにSIMDプリミティブのラッパーがいくつか上がっています。

[simd](https://hackage.haskell.org/package/simd)パッケージは、以下のようなインターフェースを提供しています：

```haskell
module Data.SIMD.SIMD4 where

class SIMD4 a where
  data X4 a
  plusX4 :: X4 a -> X4 a -> X4 a
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

```
$ cabal configure -w ghc-9.8.4 --ghc-options=-fllvm
```

#### Apple M4 Proでの結果

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
