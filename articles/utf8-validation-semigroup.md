---
title: "UTF-8のバリデーションとモノイドと半群"
emoji: "🐡"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [unicode, utf8]
published: true
---

この記事は[UTF-8のバリデーションとオートマトン](utf8-validation)の続きです。

前回はUTF-8のバリデーションが8状態のオートマトン (DFA) で表現できることを見ました。状態と遷移を擬似コードで書けば次のようになるでしょう：

```haskell
-- 8つの状態
data State = START | TAILx1 | TAILx2 | TAILx3 | A | B | C | D

-- 入力バイトに応じて次の状態を返す。次の状態が該当しなかったら Nothing を返す
next :: Word8 -> State -> Maybe State
```

さて、高速にバリデーションを行うために、並列化を試したいです。並列化と言えばモノイドです。入力の8ビット整数を何らかのモノイドに変換して、並列に結合すると良さそうです。

```
+----+----+-----+----+
| a0 | a1 | ... | aN | 8ビット整数列
+----+----+-----+----+
   |    |          |
   v    v          v
+----+----+-----+----+
| m0 | m1 | ... | mN | モノイドの元
+----+----+-----+----+
    \_/        \_/
     |          |       並列に結合
     v          v
  +-----+   +---------+
  | m01 |...| m(N-1)N |
  +-----+   +---------+
        .....
         \_/
          |
          v
        +---+
        | m |
        +---+
```

容易に得られるモノイドとして、遷移関数 `next` を部分適用したもの `State -> Maybe State` はモノイドになります。`State` は有限集合なので `State -> Maybe State` も有限集合となり、元の個数は $9^8=43046721$ 個です。

注意点として、`State -> Maybe State` を関数として表現すると結合演算の際に複雑さが増していくのでよろしくありません。計算の際には `Map State State` のようなデータ構造を使うか、固定長整数のようなコンパクトな表現を使いたいです。

表現方法として、それぞれの状態を要素が0, 1のベクトルに対応させるものがあります。

$$
v_{\mathrm{START}}=\begin{pmatrix}1\\0\\0\\0\\0\\0\\0\\0\end{pmatrix},
v_{\mathrm{TAILx1}}=\begin{pmatrix}0\\1\\0\\0\\0\\0\\0\\0\end{pmatrix},\ldots,
v_{\mathrm{D}}=\begin{pmatrix}0\\0\\0\\0\\0\\0\\0\\1\end{pmatrix}
$$

$\{0,1\}$を論理積と論理和による半環とみなして、その上の行列とベクトルを考える感じです。遷移関数の `State -> Maybe State` の部分は行列に対応します。「不正なUTF-8だった」という状態はゼロベクトルに相当します。

別の表現方法として、それぞれの `State -> Maybe State` に番号を割り当てて、結合演算は乗算表を使うやり方があります。この場合、43046721通り全てに番号を割り当てる必要はなく、実際にUTF-8の検証の際に現れるものだけを考えれば良いです。

## 行列表現の検討

最近のx86にはGFNIという命令セット拡張があります。命令セットオタクならきっと対応CPU搭載マシンを持っていることでしょう。私はもちろん持っています。Galois Fieldとか言ってますがこれはビット演算の強化版と思うこともできて、例えばbit reverseなんかが実現できます。

GFNIを使うと「0, 1からなる8×8行列の乗算」ができそうに見えます。これはいけるか！？と一瞬思ったんですが、GFNIで使う環は「論理積と排他的論理和」からなる環（有限体 $\mathbf{F}_2$）なので、ダメそうです。

## 乗算表の生成

UTF-8の検証で現れる遷移を列挙して乗算表を作るプログラムを適当に用意します。

ここでは「1バイト以上のバイト列を変換・結合できること」が必要で、遷移の単位元は特に必要ないので、モノイドではなく半群で十分です。

```haskell
{- cabal:
build-depends: base, containers, array
-}
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Array as A
import Control.Monad
import Data.Maybe

data State = START | TAILx3 | TAILx2 | TAILx1 | A | B | C | D
            deriving (Eq, Show, Ord, Enum)

allStates :: [State]
allStates = [START, TAILx3, TAILx2, TAILx1, A, B, C, D]

newtype Transition = T (Map.Map State State) deriving (Eq, Show, Ord)

instance Semigroup Transition where
  T a <> T b = T (Map.mapMaybe (`Map.lookup` b) a)

{-
instance Monoid Transition where
  mempty = T (Map.fromList $ [(s, s) | s <- allStates])
-}

g00, g80_8F, g90_9F, gA0_BF, gC2_DF, gE0, gE1_EC_EE_EF, gED, gF0, gF1_F3, gF4, gINVALID :: Transition
g00 = T $ Map.fromList [(START, START)]
g80_8F = T $ Map.fromList [(TAILx1, START), (TAILx2, TAILx1), (TAILx3, TAILx2), (B, TAILx1), (D, TAILx2)]
g90_9F = T $ Map.fromList [(TAILx1, START), (TAILx2, TAILx1), (TAILx3, TAILx2), (B, TAILx1), (C, TAILx2)]
gA0_BF = T $ Map.fromList [(TAILx1, START), (TAILx2, TAILx1), (TAILx3, TAILx2), (A, TAILx1), (C, TAILx2)]
gC2_DF = T $ Map.fromList [(START, TAILx1)]
gE0 = T $ Map.fromList [(START, A)]
gE1_EC_EE_EF = T $ Map.fromList [(START, TAILx2)]
gED = T $ Map.fromList [(START, B)]
gF0 = T $ Map.fromList [(START, C)]
gF1_F3 = T $ Map.fromList [(START, TAILx3)]
gF4 = T $ Map.fromList [(START, D)]
gINVALID = T Map.empty

-- 生成元のリスト
generators :: [Transition]
generators =
  [ g00
  , g80_8F
  , g90_9F
  , gA0_BF
  , gC2_DF
  , gE0
  , gE1_EC_EE_EF
  , gED
  , gF0
  , gF1_F3
  , gF4
  , gINVALID
  ]

loop :: Set.Set Transition -> Set.Set Transition
loop g = let g' = g `Set.union` Set.fromList [a <> b | a <- Set.toList g, b <- Set.toList g]
         in if g == g' then g else loop g'

-- 全ての元の集合
allTrans :: Set.Set Transition
allTrans = loop (Set.fromList generators)

-- 全ての元のリスト
allTransList :: [Transition]
allTransList = Set.toList allTrans

-- 遷移を整数でコードしたものを返す
transToInt :: Transition -> Int
transToInt t = head [i | (i, u) <- zip [0..] allTransList, u == t]

-- 8ビット整数に遷移を対応させる配列
byteToTransArray :: A.Array Int Transition
byteToTransArray = A.listArray (0, 255) (replicate 256 gINVALID)
                   A.// [(i, g00) | i <- [0..0x7F]]
                   A.// [(i, g80_8F) | i <- [0x80..0x8F]]
                   A.// [(i, g90_9F) | i <- [0x90..0x9F]]
                   A.// [(i, gA0_BF) | i <- [0xA0..0xBF]]
                   A.// [(i, gC2_DF) | i <- [0xC2..0xDF]]
                   A.// [(0xE0, gE0)]
                   A.// [(i, gE1_EC_EE_EF) | i <- [0xE1..0xEC]]
                   A.// [(0xED, gED)]
                   A.// [(i, gE1_EC_EE_EF) | i <- [0xEE..0xEF]]
                   A.// [(0xF0, gF0)]
                   A.// [(i, gF1_F3) | i <- [0xF1..0xF3]]
                   A.// [(0xF4, gF4)]

main :: IO ()
main = do print allTransList
          print (Set.size allTrans)
          putStrLn "byteToTrans:"
          let byteToTrans = map transToInt $ A.elems byteToTransArray
              byteToTransTable = List.unfoldr (\xs -> case splitAt 16 xs of ([], _) -> Nothing; (h, t) -> Just (h, t)) byteToTrans
          forM_ byteToTransTable $ \line ->
            print line
          writeFile "byte_to_trans.txt" $ unlines [ List.intercalate ", " (map show row) ++ "," | row <- byteToTransTable]
          putStrLn "multiplication:"
          let mulTable = [[transToInt (a <> b) | b <- allTransList] | a <- allTransList]
          forM_ mulTable $ \row ->
            print row
          writeFile "multiplication.txt" $ unlines [ "{" ++ List.intercalate ", " (map show row) ++ "}," | row <- mulTable]
          writeFile "multiplication_transposed.txt" $ unlines [ "{" ++ List.intercalate ", " (map show row) ++ "}," | row <- List.transpose mulTable]
          let stateTrans = [0 : [maybe 0 ((+ 1) . fromEnum) (Map.lookup s t) | s <- allStates] | T t <- allTransList]
          writeFile "state_trans.txt" $ unlines [ "{" ++ List.intercalate ", " (map show row) ++ "}," | row <- stateTrans]
```

このプログラムを実行すると半群の元の個数が55であることがわかり、乗算表やら何やらのファイルが書き出されます。

## 検証プログラムその1（素朴な状態遷移）

入力ファイルが正当なUTF-8であるかを検証するプログラムを用意します。まずは逐次実行型から。

```c
#include <stdio.h>

static const unsigned char byte_to_trans[256] = {
#include "byte_to_trans.txt"
};

static const unsigned char state_trans[][9] = {
#include "state_trans.txt"
};

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        fputs("No input file given\n", stderr);
        return 1;
    }
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fputs("Failed to open input file\n", stderr);
        return 1;
    }
    unsigned char state = 1;
    for (;;) {
#define N 4096
        unsigned char buf[N];
        size_t n = fread(buf, 1, sizeof(buf), fp);
        for (size_t i = 0; i < n; ++i) {
            state = state_trans[byte_to_trans[buf[i]]][state];
        }
        if (n < sizeof(buf)) {
            if (feof(fp)) {
                break;
            } else {
                fputs("Error ocurred while reading\n", stderr);
            }
        }
    }
    if (state == 1) {
        puts("VALID");
    } else {
        printf("INVALID (state=%d)\n", state);
    }
    fclose(fp);
}
```

## 検証プログラムその2（乗算表の利用）

今度は、「並列にリダクションする」やつを実装してみます。

```c
#include <stdio.h>

static const unsigned char byte_to_trans[256] = {
#include "byte_to_trans.txt"
};

static const unsigned char multiplication[64][64] = {
#include "multiplication.txt"
};

static const unsigned char state_trans[][9] = {
#include "state_trans.txt"
};

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        fputs("No input file given\n", stderr);
        return 1;
    }
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fputs("Failed to open input file\n", stderr);
        return 1;
    }
    unsigned char state = 1;
    for (;;) {
#define N 16
        unsigned char buf[N];
        size_t n = fread(buf, 1, sizeof(buf), fp);
        if (n > 0) {
            unsigned char trans[N];
            for (size_t i = 0; i < n; ++i) {
                trans[i] = byte_to_trans[buf[i]];
            }
            for (size_t j = 1; j < n; j *= 2) {
                for (size_t k = 0; k + j < n; k += j) {
                    trans[k] = multiplication[trans[k]][trans[k + j]];
                }
            }
            state = state_trans[trans[0]][state];
        }
        if (n < sizeof(buf)) {
            if (feof(fp)) {
                break;
            } else {
                fputs("Error ocurred while reading\n", stderr);
            }
        }
    }
    if (state == 1) {
        puts("VALID");
    } else {
        printf("INVALID (state=%d)\n", state);
    }
    fclose(fp);
}
```

## 検証プログラムその3（AVX2）

AVX2にはgather命令があり、乗算表から引っ張ってくる部分をSIMD化できます。これを使ったプログラムが以下です。16バイトずつ引っ張ってきて16要素→8要素→4要素→2要素→1要素みたいな感じで合成していきます。

```c
#include <stdio.h>
#include <stdint.h>
#include <immintrin.h>

static const unsigned char byte_to_trans[256 + 3] = {
#include "byte_to_trans.txt"
0, 0, 0 // sentinel
};

static const int byte_to_trans_i32[256] = {
#include "byte_to_trans.txt"
};

static const unsigned char multiplication[64][64] = {
#include "multiplication.txt"
};

static const unsigned char multiplication_transposed[256][256] = {
#include "multiplication_transposed.txt"
};

static const unsigned char state_trans[][9] = {
#include "state_trans.txt"
};

int main(int argc, char *argv[])
{
    if (argc <= 1) {
        fputs("No input file given\n", stderr);
        return 1;
    }
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fputs("Failed to open input file\n", stderr);
        return 1;
    }
    unsigned char state = 1;
    for (;;) {
#define N 4096
        _Alignas(16) unsigned char buf[N];
        size_t n = fread(buf, 1, sizeof(buf), fp);
        if (n > 0) {
            size_t i = 0;
            __m256i mask = _mm256_set_epi64x(0x000000FF000000FF, 0x000000FF000000FF, 0x000000FF000000FF, 0x000000FF000000FF);
            __m256i idx = _mm256_set_epi32(6, 4, 2, 0, 6, 4, 2, 0);
            for (; i + 16 < n; i += 16) {
                __m128i v0 = _mm_load_si128((const __m128i *)&buf[i]); // uint8x16
                __m256i v1_lo = _mm256_cvtepu8_epi32(v0); // uint32x8
                __m256i v1_hi = _mm256_cvtepu8_epi32(_mm_unpackhi_epi64(v0, v0)); // uint32x8
                __m256i v2_lo = _mm256_i32gather_epi32((int *)byte_to_trans, v1_lo, 1); // uint32x8
                __m256i v2_hi = _mm256_i32gather_epi32((int *)byte_to_trans, v1_hi, 1); // uint32x8
                v2_lo = _mm256_and_si256(v2_lo, mask); // uint32x8 {t0, t1, t2, t3, t4, t5, t6, t7}
                v2_hi = _mm256_and_si256(v2_hi, mask); // uint32x8 {t8, t9, t10, t11, t12, t13, t14, t15}
                __m256i v3_lo = _mm256_or_si256(v2_lo, _mm256_bsrli_epi128(v2_lo, 3)); // uint32x8 {t1:t0, t2:t1, t3:t2, t3, t5:t4, t6:t5, t7:t6, t7}
                __m256i v3_hi = _mm256_or_si256(v2_hi, _mm256_bsrli_epi128(v2_hi, 3)); // uint32x8 {t9:t8, t10:t9, t11:t10, t11, t13:t12, t14:t13, t15:t14, t15}
                __m256i v4_lo = _mm256_permutevar8x32_epi32(v3_lo, idx); // uint32x8 {t1:t0, t3:t2, t5:t4, t7:t6, t1:t0, t3:t2, t5:t4, t7:t6}
                __m256i v4_hi = _mm256_permutevar8x32_epi32(v3_hi, idx); // uint32x8 {t9:t8, t11:t10, t13:t12, t15:t14, t9:t8, t11:t10, t13:t12, t15:t14}
                __m256i v4 = _mm256_permute2x128_si256(v4_lo, v4_hi, 0x20); // uint32x8 {t1:t0, t3:t2, t5:t4, t7:t6, t9:t8, t11:t10, t13:t12, t15:t14}
                __m256i v5 = _mm256_i32gather_epi32((int *)multiplication_transposed, v4, 1); // uint32x8
                v5 = _mm256_and_si256(v5, mask); // uint32x8 {t1*t0, t3*t2, t5*t4, t7*t6, t9*t8, t11*t10, t13*t12, t15*t14}
                __m256i v6 = _mm256_or_si256(v5, _mm256_bsrli_epi128(v5, 3)); // uint32x8 {t3*t2:t1*t0, _, t7*t6:t5*t4, _, t11*t10:t9*t8, _, t15*t14:t13*t12, _}
                __m256i v7 = _mm256_i32gather_epi32((int *)multiplication_transposed, v6, 1); // uint32x8
                v7 = _mm256_and_si256(v7, mask); // uint32x8 {t3*t2*t1*t0, _, t7*t6*t5*t4, _, t11*t10*t9*t8, _, t15*t14*t13*t12, _}
                __m256i v8 = _mm256_or_si256(v7, _mm256_bsrli_epi128(v7, 7)); // uint32x8
                __m256i v9 = _mm256_i32gather_epi32((int *)multiplication_transposed, v8, 1); // uint32x8
                v9 = _mm256_and_si256(v9, mask); // uint32x8 {*, _, _, _, *, _, _, _}
                v9 = _mm256_permute4x64_epi64(v9, 8); // uint32x8 {*, _, *, _, _, _, _, _}
                __m128i v10 = _mm256_extracti128_si256(v9, 0); // uint32x4 {*, _, *, _}
                __m128i v11 = _mm_or_si128(v10, _mm_bsrli_si128(v10, 7)); // uint32x8
                __m128i v12 = _mm_i32gather_epi32((int *)multiplication_transposed, v11, 1); // uint32x8
                v12 = _mm_and_si128(v12, _mm256_extracti128_si256(mask, 0)); // uint32x8
                uint32_t t = _mm_extract_epi32(v12, 0);
                state = state_trans[t][state];
            }
            for (; i < n; ++i) {
                state = state_trans[byte_to_trans[buf[i]]][state];
            }
        }
        if (n < sizeof(buf)) {
            if (feof(fp)) {
                break;
            } else {
                fputs("Error ocurred while reading\n", stderr);
            }
        }
    }
    if (state == 1) {
        puts("VALID");
    } else {
        printf("INVALID (state=%d)\n", state);
    }
    fclose(fp);
}
```

## 検証用データ

検証用データをスクリプトで適当に用意します。

```lua
local f = assert(io.open("valid1.txt", "wb"))
for i = 1, 12345677 do
  f:write("ABCDEFGHIJK\n")
end
f:close()
local f = assert(io.open("valid2.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:close()
local f = assert(io.open("invalid1.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:write("\x80")
f:close()
local f = assert(io.open("invalid2.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:write("\xFF")
f:close()
local f = assert(io.open("invalid3.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:write("\xC0\x80")
f:close()
local f = assert(io.open("invalid4.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:write("\xC2")
f:close()
local f = assert(io.open("invalid5.txt", "wb"))
for i = 1, 12345677 do
  f:write("A\u{80}B\u{100}\u{2000}C\u{3042}D\u{10000}\u{10FFFF}E\u{FFFF}FK\n")
end
f:write("\x80")
f:close()
```

## 検証

実際に動かしてみましょう。コンパイルします。

```
$ gcc -O3 -o validate1 validate1.c         # その1（素朴な状態遷移）
$ gcc -O3 -o validate2 validate2.c         # その2（乗算表）
$ gcc -O3 -mavx2 -o validate3 validate3.c  # その3（AVX2版）
```

実行してみます。

```
$ ./validate1 valid1.txt
VALID
$ ./validate1 valid2.txt
VALID
$ ./validate1 invalid1.txt
INVALID (state=0)
$ ./validate1 invalid2.txt
INVALID (state=0)
$ ./validate1 invalid3.txt
INVALID (state=0)
$ ./validate1 invalid4.txt
INVALID (state=4)
$ ./validate1 invalid5.txt
INVALID (state=0)
```

```
$ ./validate2 valid1.txt
VALID
$ ./validate2 valid2.txt
VALID
$ ./validate2 invalid1.txt
INVALID (state=0)
$ ./validate2 invalid2.txt
INVALID (state=0)
$ ./validate2 invalid3.txt
INVALID (state=0)
$ ./validate2 invalid4.txt
INVALID (state=4)
$ ./validate2 invalid5.txt
INVALID (state=0)
```

```
$ ./validate3 valid1.txt
VALID
$ ./validate3 valid2.txt
VALID
$ ./validate3 invalid1.txt
INVALID (state=0)
$ ./validate3 invalid2.txt
INVALID (state=0)
$ ./validate3 invalid3.txt
INVALID (state=0)
$ ./validate3 invalid4.txt
INVALID (state=4)
$ ./validate3 invalid5.txt
INVALID (state=0)
```

良さそうです。

ベンチマークを走らせてみましょう。使った環境はRyzen 9 7940HS上のWindowsのWSL2で動くUbuntu 22.04です。

```
$ hyperfine "./validate1 valid2.txt" "./validate2 valid2.txt" "./validate3 valid2.txt"
Benchmark 1: ./validate1 valid2.txt
  Time (mean ± σ):     337.8 ms ±   7.4 ms    [User: 318.3 ms, System: 18.0 ms]
  Range (min … max):   330.5 ms … 352.6 ms    10 runs

Benchmark 2: ./validate2 valid2.txt
  Time (mean ± σ):     526.3 ms ±   4.3 ms    [User: 510.7 ms, System: 15.0 ms]
  Range (min … max):   518.7 ms … 532.8 ms    10 runs

Benchmark 3: ./validate3 valid2.txt
  Time (mean ± σ):     447.3 ms ±   3.1 ms    [User: 429.6 ms, System: 17.1 ms]
  Range (min … max):   442.5 ms … 452.3 ms    10 runs

Summary
  './validate1 valid2.txt' ran
    1.32 ± 0.03 times faster than './validate3 valid2.txt'
    1.56 ± 0.04 times faster than './validate2 valid2.txt'
$ hyperfine "./validate1 invalid5.txt" "./validate2 invalid5.txt" "./validate3 invalid5.txt"
Benchmark 1: ./validate1 invalid5.txt
  Time (mean ± σ):     331.6 ms ±   4.5 ms    [User: 321.0 ms, System: 10.0 ms]
  Range (min … max):   323.5 ms … 338.7 ms    10 runs

Benchmark 2: ./validate2 invalid5.txt
  Time (mean ± σ):     525.1 ms ±   3.0 ms    [User: 502.9 ms, System: 22.0 ms]
  Range (min … max):   519.6 ms … 530.0 ms    10 runs

Benchmark 3: ./validate3 invalid5.txt
  Time (mean ± σ):     446.0 ms ±   3.5 ms    [User: 420.4 ms, System: 25.0 ms]
  Range (min … max):   440.6 ms … 451.3 ms    10 runs

Summary
  './validate1 invalid5.txt' ran
    1.35 ± 0.02 times faster than './validate3 invalid5.txt'
    1.58 ± 0.02 times faster than './validate2 invalid5.txt'
```

いかがでしたか？半群とかを使わない、素朴なやつが一番早いという結果になりました。私にはSIMD命令の活用は早かったのかもしれません。

## もっと知りたい人へ

私はこのトピックについては ~~飽きた~~ 疲れてしまったのでここまでにしておきますが、UTF-8の高速なバリデーションに興味のある方に参考になりそうな雑多なリンクを挙げておきます。

* [高速UTF-8バリデーションの世界 #高速化 - Qiita](https://qiita.com/saka1_p/items/9719d71e702aed92a07c)
* [\[2010.03090\] Validating UTF-8 In Less Than One Instruction Per Byte](https://arxiv.org/abs/2010.03090)
    * [Validating UTF-8 strings using as little as 0.7 cycles per byte – Daniel Lemire's blog](https://lemire.me/blog/2018/05/16/validating-utf-8-strings-using-as-little-as-0-7-cycles-per-byte/) ↑の著者の一人による記事
    * [Ridiculously fast unicode (UTF-8) validation – Daniel Lemire's blog](https://lemire.me/blog/2020/10/20/ridiculously-fast-unicode-utf-8-validation/)
* [\[2109.10433\] Transcoding Billions of Unicode Characters per Second with SIMD Instructions](https://arxiv.org/abs/2109.10433)
* [超高速なUTF-8バリデーション、rangeアルゴリズムの紹介 - てきとうなさいと。べぇたばん](http://tekitoh-memdhoi.info/views/872)
* [simdutf/simdutf: Unicode routines (UTF8, UTF16, UTF32): billions of characters per second using SSE2, AVX2, NEON, AVX-512. Part of Node.js and Bun.](https://github.com/simdutf/simdutf)

AVX2とオートマトンと半群は以下のスライドが参考になりました。以下のスライドが出た当時（2012年）はAVX2搭載のCPUは出てなかったのに対して今はAVX-512とかGFNIがあるので、思えば遠いところまで来たものですね。

* [AVX2時代の正規表現マッチング 〜半群でぐんぐん！〜](https://www.slideshare.net/sinya8282/avx2)
