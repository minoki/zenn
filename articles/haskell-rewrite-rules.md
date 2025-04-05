---
title: "Haskell/GHCの書き換え規則（rewrite rules）覚え書き"
emoji: "📝"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

GHCで使える**書き換え規則** (rewrite rules) は、Haskellに特徴的な最適化技法の一つと言って良いでしょう。Haskell初級者向けの機能ではありませんが、自分でライブラリーを書くような中級者、上級者は書き換え規則を使えるようになっておくと便利です。

この記事には、書き換え規則について知っておくと良さそうなことを乱雑に載せておきます。基本的なことはGHCのUser's Guideも見てください：

* [6.19.1. Rewrite rules — Glasgow Haskell Compiler 9.12.2 User's Guide](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/rewrite_rules.html#rewrite-rules)

## 使い所1：fusion

Haskellではリストや配列をよく使います。それらは大抵、「リスト（配列）を受け取ってリスト（配列）を返す関数」として定義されています。これらを組み合わせると、「中間のリストが構築されてすぐに消費される」というパターンが現れます。例えば、次のコードを考えましょう：

```haskell
ys = map f (map g xs)
```

このコードでは、`map g xs` で作られたリストはすぐに `map f` で消費されます。一方、

```haskell
ys = map (f . g) xs
```

と書けば、中間のリストはそもそも生成されません。前者の非効率なコードを後者の効率的なコードへ自動的に最適化できたら便利ではないでしょうか。このような中間のデータ構造を削減する最適化は**fusion**と呼ばれます。

まあ、このくらい単純だったら「プログラマーが気をつけて手動で変換すれば良い」となるかもしれません。では、次の例はどうでしょうか：

```haskell
allEqual xs ys = and $ zipWith (==) xs ys
```

これも `zipWith (==) xs ys` で生成されたリストはすぐに `and` で消費されます。しかし、書き換え先の関数は `map` の場合ほど明らかではありません。この場合でも中間のデータ構造を削減する最適化ができると便利です。

この場合は、`stream`/`unstream` という補助的な関数を導入し、リストを一旦ストリームと呼ばれるいい感じの形式に変換します。そして、ストリームに対する関数 `zipWithS`, `andS` を導入し、

```
zipWith f xs ys → unstream (zipWithS f (stream xs) (stream ys))
and bs → andS (stream bs)
```

と書き換えます。そして、

```
allEqual xs ys = and $ zipWith (==) xs ys
               = andS (stream (unstream (zipWithS f (stream xs) (stream ys))))
               = andS (zipWithS f (stream xs) (stream ys))
```

と変換すれば、中間のリストを作らずに済みます。もちろん、これが効率の良いコードであるためには、ストリームの変換が効率よく定義できる必要があります。

このような、「中間のデータ構造を削減する最適化」をHaskellで可能にする機構が**書き換え規則**です。`map` の例の場合は `map f (map g xs)` を `map (f . g) xs` に書き換え、ストリームの例の場合は `stream (unstream s)` を `s` に書き換えました。

## 使い所2：関数の特殊化

型に応じて実装を切り替えるには、Haskellでは基本的には型クラスを使います。しかし、型クラスの使用は大げさだけれども、最適化として実装を切り替えたい時があります。

例えば、整数型を他の数値型に変換する `fromIntegral` という関数があります。この関数は次のように定義されます：

```haskell
fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger
```

`toInteger` で多倍長整数型 `Integer` に変換し、それを `fromInteger` で目的の型に変換しているわけですね。

しかし、この定義では `Int` や `Word` といった固定長整数型を変換する際にも多倍長整数型を経由することになり、効率が悪いです。もちろん、最適化が有効ではない場合はそれでも良いかもしれませんが、最適化が有効な場合は効率的なコードになって欲しいです。

そこで登場するのが書き換え規則です。例えば、次のように規則を定義すれば、`Int` を `Word` に変換する場合に `intToWord` という効率の良い関数を使うようになります。

```haskell
intToWord :: Int -> Word -- 何らかの効率の良い関数

{-# RULES
"fromIntegral/Int->Word" fromIntegral = intToWord
  #-}
```

ただ、注意点もあります。書き換え規則で動作が変わってしまうと、「最適化によって動作が違う」という事態が起こります。例えば、標準ライブラリーには `realToFrac` という関数がありますが、これに無限大を与えてみましょう：

```haskell
main :: IO ()
main = print (realToFrac (1 / 0 :: Float) :: Double)
```

```
$ ghc -fforce-recomp realtofrac.hs
$ ./realtofrac
3.402823669209385e38
$ ghc -fforce-recomp -O2 realtofrac.hs
$ ./realtofrac
Infinity
```

最適化の有無で挙動が変わってしまいました。これは、`realToFrac` の実装が不適切で、「デフォルトの実装は無限大を扱えない」のが書き換え規則によって「無限大を扱える変換関数」に変換されるためです。

真っ当なHaskellコードは、たとえ書き換え規則が発動しなくてもまともな動作をするべきです。

なお、この記事の例では、書き換え規則の発動を確認するために、書き換え規則によってあえて動作を変える場合があります。

## 書き方

書き換え規則は `RULES` プラグマの中に書きます。規則の名前（文字列）、登場する変数（forall）、書き換え前の式、イコール、書き換え後の式の順で書きます。例を載せます：

```haskell
{-# RULES
"map/map" forall f g x. map f (map g xs) = map (f . g) xs
  #-}
```

この規則は、`map f (map g xs)` という形の式（`f`, `g`, `xs` は任意）があったときにそれを `map (f . g) xs` へ置き換えよ、ということを表しています。

一つの規則を複数行に分けて書く場合は、レイアウト規則が有効なことに注意してください。以下の例は、規則の2行の「`map`」のカラムが規則の名前（`"map/map"`）と同じ位置なのでダメです：

```haskell
-- ダメな例
{-# RULES
"map/map" forall f g x.
map f (map g xs) = map (f . g) xs
  #-}
```

次のように、規則の2行目のカラムを右にずらすと大丈夫です：

```haskell
-- 良い例
{-# RULES
"map/map" forall f g x.
  map f (map g xs) = map (f . g) xs
  #-}
```

## 段階制御（フェーズコントロール）について

高度な書き換え規則を書くには、インライン化や書き換え規則に優先順位をつけられると便利です。そこで、GHCはインライン化や書き換え規則に対して、それが発動する段階（フェーズ）を設定できるようにしています。

段階の制御方法等の基本的なことはGHCのマニュアルも参照してください：

* [Phase control - 6.20. Pragmas — Glasgow Haskell Compiler 9.12.2 User's Guide](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/exts/pragmas.html#phase-control)

段階は大まかには Phase 2 → Phase 1 → Phase 0 の順番で遷移します。内部的には、この3段階の前に走る段階 (InitialPhase) と後に走る段階 (FinalPhase) があります。段階の数はコンパイルオプション `-fsimplifier-phases` で変更できます。

書き換え規則を定義する関数には、基本的に何らかの (`NO`)`INLINE` プラグマを書いて、少なくとも1つ以上の段階でインライン化が抑制されるようにします。

```haskell
-- 悪い例：fが先にインライン化されるかもしれない
import Debug.Trace

f :: Int -> Int
f x = trace "f" (x + 1)

{-# RULES
"f" f = \x -> trace "rule fired" (x + 1)
  #-}

main = print (f 5)
```

```haskell
-- 良い例：fはPhase 1以降（Phase 1, Phase 0）でインライン化される
-- Phase 2では書き換え規則が発動する
import Debug.Trace

f :: Int -> Int
f x = trace "f" (x + 1)
{-# INLINE [1] f #-}

{-# RULES
"f" f = \x -> trace "rule fired" (x + 1)
  #-}

main = print (f 5)
```

段階の数は次のコードで確認できます：

```haskell
foo :: String
foo = "foo"
{-# NOINLINE foo #-}

{-# RULES
"foo/0" [0] foo = "phase 0" -- Phase 0以降で有効になる
"foo/1" [1] foo = "phase 1" -- Phase 1以降で有効になる
"foo/2" [2] foo = "phase 2" -- Phase 2以降で有効になる
"foo/3" [3] foo = "phase 3" -- Phase 3以降で有効になる
"foo/4" [4] foo = "phase 4" -- Phase 4以降で有効になる
  #-}

main :: IO ()
main = putStrLn foo
```

実行例：

```
$ ghc -O2 -fforce-recomp phasenum.hs 
$ ./phasenum
phase 2
$ ghc -O2 -fforce-recomp -fsimplifier-phases=4 phasenum.hs
$ ./phasenum                                              
phase 4
$ ghc -O2 -fforce-recomp -fsimplifier-phases=3 phasenum.hs
$ ./phasenum                                              
phase 3
$ ghc -O2 -fforce-recomp -fsimplifier-phases=2 phasenum.hs
$ ./phasenum                                              
phase 2
$ ghc -O2 -fforce-recomp -fsimplifier-phases=1 phasenum.hs
$ ./phasenum                                              
phase 1
$ ghc -O2 -fforce-recomp -fsimplifier-phases=0 phasenum.hs
$ ./phasenum                                              
phase 0
```

注意して欲しいのは、Phase 2の実行中には「Phase 4以降 `[4]`」「Phase 3以降 `[3]`」で指定した規則も有効なので、上記の実行結果が得られたのは偶然の産物だということです。実際、規則の記述順を変えると発火する規則が変わったりします。

また、`[~n]` の形で指定した規則はInitialPhaseで有効になるので、段階の最大値が `n` より小さくても発火する可能性があります：

```haskell
foo :: String
foo = "foo"
{-# INLINE [4] foo #-}

{-# RULES
"foo" [~4] foo = "rule fired" -- -fsimplifier-phases=2 でも発火する可能性がある
  #-}

main :: IO ()
main = putStrLn foo
```

## 型クラスのメソッド

型クラスのメソッドに対しては暗黙に書き換え規則が定義されるので、ユーザー定義の書き換え規則の左辺で言及することはできません。

```haskell
class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

-- 動かない
{-# RULES
"myFmap" forall f g x.
  myFmap f (myFmap g x) = myFmap (f . g) x
#-}
```

型クラスのインスタンスの実装を普通の関数として定義して、それに対して書き換え規則を定義することはできます。

```haskell
class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

myFmapIO :: (a -> b) -> IO a -> IO b
myFmapIO f x = do
  putStrLn "myFmap"
  t <- x
  pure (f t)
{-# INLINE [1] myFmapIO #-}

instance MyFunctor IO where
  myFmap = myFmapIO
  {-# INLINE myFmap #-}

-- OK
{-# RULES
"myFmapIO" forall f g x.
  myFmapIO f (myFmapIO g x) = myFmapIO (f . g) x
#-}

main = myFmapIO (+ 2) (myFmapIO (* 3) (pure 5)) >>= print
```

## newtypeとcoerce

`data` で作ったデータ構築子は書き換え規則の左辺から言及できますが、`newtype` で作ったデータ構築子は言及できないようです。

```haskell
import Debug.Trace

newtype A = MkA Int
data B = MkB Int

f :: A -> Int
f (MkA x) = x + 1
{-# INLINE [1] f #-}

g :: B -> Int
g (MkB x) = x + 1
{-# INLINE [1] g #-}

{-# RULES 
"f" forall x. f (MkA x) = trace "f/MkA" (x + 1)
"g" forall x. g (MkB x) = trace "g/MkB" (x + 1)
  #-}

main :: IO ()
main = do
  print (f (MkA 4)) -- 発動しない
  print (g (MkB 4)) -- 発動する
```

一方で、左辺に `coerce` を書くことはできます。`coerce` と直接書いた場合だけでなく、newtype constructorやnewtypeのフィールドも `coerce` にマッチします。

```haskell
import Data.Coerce
import Debug.Trace

newtype A = MkA { unA :: Int }

f :: A -> Int
f (MkA x) = x + 1
{-# INLINE [1] f #-}

g :: Int -> Int
g x = x + 1
{-# INLINE [1] g #-}

{-# RULES 
"f" forall (x :: Int). f (coerce x) = trace "f/coerce" (x + 1)
"g" forall (x :: A). g (coerce x) = trace "g/coerce" (unA x + 1)
  #-}

g' :: A -> Int
g' = g . unA -- 発動する
{-# NOINLINE g' #-}

main :: IO ()
main = do
  print (f (MkA 4)) -- 発動する
  print (g' (MkA 4))
```

## 関数に対する書き換え規則

最初の動機付けとして `fromIntegral` の例を出しました。こういう、「単独の関数」を書き換える際は注意が必要です。次のコードを考えます：

```haskell
{-# LANGUAGE MagicHash #-}
import Debug.Trace
import GHC.Exts

myFromIntegral :: (Integral a, Num b) => a -> b
myFromIntegral = fromInteger . toInteger
{-# INLINE [1] myFromIntegral #-}

-- よくない書き方
{-# RULES
"int2Word" forall x. myFromIntegral x = trace "int2Word" (case x of I# i# -> W# (int2Word# i#))
  #-}

main :: IO ()
main = do
  print (myFromIntegral (42 :: Int) :: Word)
  print (map myFromIntegral ([2,3,4] :: [Int]) :: [Word])
```

このプログラムをコンパイル・実行して、書き換え規則が発動するか見てみましょう。

```
$ ghc -fforce-recomp -O2 fromintegral.hs
$ ./fromintegral                        
int2Word
42
[2,3,4]
```

`myFromIntegral` に直接引数を与えた方は書き換え規則が発動しましたが、`map` の引数として与えた方は発動しませんでした。

実は書き換え規則は引数の個数が重要で、規則の左辺に `myFromIntegral x` と書いたらその個数の引数（この場合は1個）が与えられないとマッチしないのです。

より多くの場面で書き換え規則を発動させるには、関数の引数は右辺に持っていくと良いでしょう：

```haskell
{-# LANGUAGE MagicHash #-}
import Debug.Trace
import GHC.Exts

myFromIntegral :: (Integral a, Num b) => a -> b
myFromIntegral = fromInteger . toInteger
{-# INLINE [1] myFromIntegral #-}

-- 良い書き方
{-# RULES
"int2Word" myFromIntegral = \x -> trace "int2Word" (case x of I# i# -> W# (int2Word# i#))
  #-}

main :: IO ()
main = do
  print (myFromIntegral (42 :: Int) :: Word)
  print (map myFromIntegral ([2,3,4] :: [Int]) :: [Word])
```

```
$ ghc -fforce-recomp -O2 fromintegral.hs
$ ./fromintegral                        
int2Word
42
int2Word
int2Word
int2Word
[2,3,4]
```

これは、**ポイントフリースタイルとは関係がない**ので注意してください。左辺を関数合成にしてしまうと、かえって規則が発動しない場合があります：

```haskell
import Debug.Trace

f :: Int -> Int
f x = x + 1
{-# INLINE [1] f #-}

g :: Int -> Int
g x = 2 * x
{-# INLINE [1] g #-}

-- 発動しない場合がある
{-# RULES
"g.f" g . f = \x -> trace "g.f" (2 * (x + 1))
  #-}

main :: IO ()
main = do
  print (g (f 3)) -- 発動しない
  print (map (g . f) [4,5,6])
```

```
$ ghc -fforce-recomp -O2 comp.hs
[1 of 2] Compiling Main             ( comp.hs, comp.o )

comp.hs:12:1: warning: [GHC-95396] [-Winline-rule-shadowing]
    Rule "g.f" may never fire because ‘.’ might inline first
    Suggested fix: Add an INLINE[n] or NOINLINE[n] pragma for ‘.’
   |
12 | "g.f" g . f = \x -> trace "g.f" (2 * (x + 1))
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[2 of 2] Linking comp
$ ./comp
8
g.f
g.f
g.f
[10,12,14]
```

一方で、左辺を普通に関数適用のネスト `g (f x)` にすれば、関数適用のネストであっても関数合成 `g . f` であっても規則が発動します：

```haskell
import Debug.Trace

f :: Int -> Int
f x = x + 1
{-# INLINE [1] f #-}

g :: Int -> Int
g x = 2 * x
{-# INLINE [1] g #-}

-- 関数合成 g . f に対しても発動する
{-# RULES
"g.f" forall x. g (f x) = trace "g.f" (2 * (x + 1))
  #-}

main :: IO ()
main = do
  print (g (f 3))
  print (map (g . f) [4,5,6])
```

```
$ ghc -fforce-recomp -O2 comp.hs
$ ./comp                        
g.f
8
g.f
g.f
g.f
[10,12,14]
```

これは、関数合成 `(.)` がインライン化されると `\x -> g (f x)` の形になって関数適用が現れるからです。

## 多相と型クラス

型クラスが関係する規則を書く場合、左辺から導出されない型クラスを右辺で使うことはできません。

例えば、`fromIntegral` の「ターゲットの型に安全に変換できる場合に限り変換する」ようなバージョンを作ってみましょう。`Data.Bits` にはこれと同じ動作をする関数 `toIntegralSized` があるので、これを使える場合は使いたいです。そこで、次のコードを書いてみます：

```haskell
import Debug.Trace
import Data.Bits

fromIntegralMaybe :: forall a b. (Integral a, Integral b, Bounded b) => a -> Maybe b
fromIntegralMaybe x
  | toInteger (minBound :: b) <= toInteger x, toInteger x <= toInteger (maxBound :: b)
  = Just (fromInteger (toInteger x))
  | otherwise = Nothing
{-# INLINE [1] fromIntegralMaybe #-}

{-# RULES
"fromIntegralMaybe" fromIntegralMaybe = \x -> trace "used toIntegralSized" (toIntegralSized x)
  #-}
```

これはコンパイルが通りません。`toIntegralSized` は入出力の型に `Bits` 制約が必要で、それは左辺には存在しないからです。

個別の型を列挙した規則を作ることは可能です：

```haskell
import Debug.Trace
import Data.Bits

fromIntegralMaybe :: forall a b. (Integral a, Integral b, Bounded b) => a -> Maybe b
fromIntegralMaybe x
  | toInteger (minBound :: b) <= toInteger x, toInteger x <= toInteger (maxBound :: b)
  = Just (fromInteger (toInteger x))
  | otherwise = Nothing
{-# INLINE [1] fromIntegralMaybe #-}

{-# RULES
"fromIntegralMaybe/Int->Word" fromIntegralMaybe = (\x -> trace "used toIntegralSized" (toIntegralSized x)) :: Int -> Maybe Word
"fromIntegralMaybe/Word->Int" fromIntegralMaybe = (\x -> trace "used toIntegralSized" (toIntegralSized x)) :: Word -> Maybe Int
  #-}

main :: IO ()
main = print (fromIntegralMaybe (42 :: Int) :: Maybe Word)
```

## コンパイラーの内部の情報を吐き出させる

発動した書き換え規則の情報等をGHCに出力させることもできます。思い通りの出力コードが得られない場合に確認すると良いでしょう。具体的には、以下のオプションを使うと良いでしょう：

* `-ddump-rule-firings`
* `-ddump-rule-rewrites`
* `-ddump-simpl-iterations`
* `-ddump-simpl-stats`

他にも役に立つオプションがあるかもしれません。

これらはデフォルトで標準出力に書き出されますが、`-ddump-to-file` オプションによりファイルに書き出させることもできます。

## リンク

書き換え規則について取り上げている日本語の記事へのリンクも貼っておきます。

* [Rewrite Rulesについて軽く - Qiita](https://qiita.com/ruicc/items/4c33fe2501fa956cd9a5)
* [本物のプログラマはHaskellを使う > 第37回　書き換え規則を使って不要な計算や中間データを除去](https://xtech.nikkei.com/it/article/COLUMN/20100112/343099/)
* [本物のプログラマはHaskellを使う > 第38回　書き換え規則を強力にサポートする段階制御](https://xtech.nikkei.com/it/article/COLUMN/20100302/345243/)

書き換え規則の応用として、私が以前書いた「Haskellでの型レベルプログラミング」（同人誌版）では、書き換え規則を使って定理証明の実行時間を0にする技法を紹介しました：

* [だめぽラボ 「Haskellでの型レベルプログラミング」](https://lab.miz-ar.info/haskell-type-level/)

別の応用として、文字列を受け取る関数に書き換え規則を定義すれば、文字列リテラルを受け取る際に文字のリストを構築しないようにできる話も書きました：

* [Haskellの文字列リテラルはGHCでどのようにコンパイルされるか - Qiita](https://qiita.com/mod_poppo/items/80c442a1d95471e6ac55)

書き換え規則は強力な技法です。使いこなしてHaskell上級者を目指しましょう。
