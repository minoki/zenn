---
title: "HaskellのPrimMonadとうまく付き合う その1"
emoji: "⛳"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

Haskellで可変な参照や配列を使うには、`IO` や `ST` などのモナドを使います。`IO` や `ST` の上に `ReaderT` や `StateT` 等のモナド変換子を重ねたモナドでも、`IO` や `ST` のアクションを `lift` すれば可変な参照や配列を扱えます。

[primitive](https://hackage.haskell.org/package/primitive)パッケージでは、このような「可変な参照や配列を扱えるモナド」を統一的に扱える `PrimMonad` という型クラスを用意しています。

```haskell
module Control.Monad.Primitive where

class Monad m => PrimMonad m where
  type PrimState m
  -- ...

instance PrimMonad IO
instance PrimMonad (ST s)
```

この記事では、`PrimMonad` を使う際にパフォーマンスを低下させないための注意点について論じます。

サンプルコードはGitHubの[haskell-primmonad-example/count-prime](https://github.com/minoki/haskell-primmonad-example/tree/main/count-prime)に上がっています。

## 題材：エラトステネスの篩

例題として、素数を見つけるためのエラトステネスの篩を実装してみます。まずは `ST` モナドで書きます。

```haskell
module MyLib where
import           Control.Monad (forM_, when)
import           Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as VUM

mkSieveST :: Int -> ST s (VUM.MVector s Bool)
mkSieveST !n = do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec
```

本当は `VU.create` で不変な `Vector` を返すようにすると純粋関数にできて使いやすいですが、今回は話の都合上、可変な配列を返すアクションとして定義します。

呼び出す側も書いてみます。10000000以下の素数の個数を数えるようにしてみましょう。

```haskell
module Main where
-- ...
import           Control.Monad.ST (stToIO)

main :: IO ()
main = do
  let n = 10000000
  vec <- stToIO $ mkSieveST n
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
```

使う側は `IO` モナドなので、 `stToIO :: ST RealWorld a -> IO a` 関数を使って `IO` アクションに変換しています。

簡単に実行時間を計測してみましょう。

```
$ cabal build -O2
$ cabal exec -O2 hyperfine "count-prime st"
Benchmark 1: count-prime st
  Time (mean ± σ):     401.7 ms ±  12.2 ms    [User: 316.7 ms, System: 74.6 ms]
  Range (min … max):   388.2 ms … 424.9 ms    10 runs
```

## `PrimMonad` への一般化と、その罠

先ほどの実装では、`IO` モナドから使う際に `stToIO` 関数を噛ませる必要があって面倒でした。この問題は、`PrimMonad` を使って型を一般化すると解消します：

```haskell
-- MyLib.hs

mkSievePM :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSievePM !n = do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec
```

`PrimMonad` を使う際は、`ST s` の `s` に相当する部分が `PrimState m` になります。

利用側からは、`stToIO` が外れました：

```haskell
-- Main.hs

main :: IO ()
main = do
  let n = 10000000
  vec <- mkSievePM n
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
```

良いですね。

実行時間を測ってみましょう。GitHubに上げたサンプルコードでは引数 `pm` を渡すことで実験できます。

```
$ cabal build -O2
$ cabal exec -O2 hyperfine "count-prime pm"
Benchmark 1: count-prime pm
  Time (mean ± σ):      1.231 s ±  0.017 s    [User: 1.139 s, System: 0.082 s]
  Range (min … max):    1.210 s …  1.260 s    10 runs
```

なんと、3倍くらい遅くなりました。

これは、`mkSieve` に対して出力されるコードで使われるモナドが、`ST s` という具体的なモナドから、抽象的なモナド `m` に変わってしまったことに起因します。`IO` や `ST s` なら効率的なネイティブコードが生成されるのに対し、一般のモナドだったらアクションを実行するたびに関数を作り、 `>>=` を呼び出すイメージです。

もちろん、オーバーヘッドを回避できる場合もあります。例えば、`mkSievePM` と利用側（`main`）が同じモジュールであれば、**特殊化** (specialization) のような最適化が働いて `ST s` 用の `mkSievePM` の実体が生成され、効率的なネイティブコードになるでしょう。

## モジュールを跨いだ特殊化

モジュールを跨いだ場合でも特殊化を発生させる方法もあります。例えば、`INLINE` プラグマを使えば関数の定義（`=` の右辺）が利用側のモジュールからも見えるので、特殊化が可能になります。「特殊化は可能にしたいけどインライン化は（コンパイル時間の兼ね合い等で）しなくてもよい」という場合は `INLINABLE` プラグマが利用できます。

```haskell
{-# INLINABLE mkSievePM #-}
mkSievePM :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSievePM !n = ...
```

関数を定義する側のモジュールで、「どういうモナドについて適用されるかの候補」がわかっている場合は、`SPECIALIZE` プラグマを使うこともできます。

```haskell
{-# SPECIALIZE mkSievePM :: Int -> IO (VUM.MVector RealWorld Bool) #-}
mkSievePM :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSievePM !n = ...
```

`INLINABLE` プラグマを使った例も実行してみましょう。GitHubに上げたサンプルコードでは引数 `pmi` を渡すことで実験できます。

```
$ cabal exec -O2 hyperfine "count-prime pmi"
Benchmark 1: count-prime pmi
  Time (mean ± σ):     386.9 ms ±   6.9 ms    [User: 309.2 ms, System: 67.4 ms]
  Range (min … max):   378.7 ms … 400.1 ms    10 runs
```

`ST` で実装したものと同等の実行時間になりました。

## モナドスタックが積まれている場合：`stToPrim` の利用

`PrimMonad` を使うと、`IO` や `ST s` の上に `ReaderT` やら `StateT` やら `WriterT` やら `ContT` やらのモナド変換子を重ねていても気にしないで状態の破壊的更新ができます。

適当に積み重ねた例を作ってみましょう：

```haskell
-- Main.hs

-- 真面目にやるなら RWST を使うべきだが、段数を重ねたいのであえてこうする
type M = ReaderT Int (StateT Int (WriterT String IO))

runM :: M a -> IO a
runM action = fst <$> runWriterT (evalStateT (runReaderT action 0) 0)

main :: IO ()
main = do
  let n = 10000000
  -- mkSievePMInlinable には INLINABLE がついているので特殊化が起こるはず
  vec <- runM $ mkSievePMInlinable n
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
```

サンプルコードでは引数に `stacked-pmi` を渡すことで上記と同等のコードを実行できます。

```
$ cabal exec -O2 hyperfine "count-prime stacked-pmi"
Benchmark 1: count-prime stacked-pmi
  Time (mean ± σ):     699.6 ms ±   7.4 ms    [User: 597.6 ms, System: 93.4 ms]
  Range (min … max):   688.0 ms … 710.2 ms    10 runs
```

振れ幅はあると思いますが、先ほどよりは遅くなったのがわかると思います。これは、**モナド変換子を積み重ねたことによって、逐次実行 `>>=` のコストが増えた**と解釈できます。

しかし、`mkSieve` という処理は `ReaderT` やら `StateT` やら `WriterT` やらには依存せず、本質的には `ST s` にしか依存しません。重いループは `ST s` というシンプルなモナドで記述してやって、それを複雑なモナドに持ち上げてやれば速度を低下させずに済むのではないでしょうか？

`ST s` で記述されたアクションを `PrimMonad m` に埋め込むには、`stToPrim` という関数を使います：

```haskell
module Control.Monad.Primitive where

stToPrim :: PrimMonad m => ST (PrimState m) a -> m a
```

使用例です：

```haskell
mkSieveSTToPrim :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSieveSTToPrim !n = stToPrim $ do
  !vec <- VUM.replicate (n + 1) True
  VUM.write vec 0 False
  VUM.write vec 1 False
  forM_ [2..n] $ \i -> do
    !b <- VUM.read vec i
    when b $ do
      forM_ [i*i,i*(i+1)..n] $ \j ->
        VUM.write vec j False
  pure vec
```

先ほどのように、変換子が積み重なったモナドで `mkSieveSTToPrim` を実行してみます：

```haskell
-- Main.hs

-- 真面目にやるなら RWST を使うべきだが、段数を重ねたいのであえてこうする
type M = ReaderT Int (StateT Int (WriterT String IO))

runM :: M a -> IO a
runM action = fst <$> runWriterT (evalStateT (runReaderT action 0) 0)

main :: IO ()
main = do
  let n = 10000000
  vec <- runM $ mkSieveSTToPrim n
  m <- VUM.foldl (\ !acc !b -> if b then acc + 1 else acc :: Int) 0 vec
  print m
```

GitHubに上げたサンプルコードでは、引数に `stacked-sttoprim` を指定することでこれを試せます。実行時間を測定してみましょう：

```
$ cabal exec -O2 hyperfine "count-prime stacked-sttoprim"
Benchmark 1: count-prime stacked-sttoprim
  Time (mean ± σ):     377.4 ms ±  13.3 ms    [User: 298.3 ms, System: 68.1 ms]
  Range (min … max):   360.8 ms … 407.8 ms    10 runs
```

最初の、全て `ST` で書いたものと同等の実行時間になりました。ここでは `INLINABLE` を使わなくても速度が出ています。

## 良いとこどりをする

`ST` で済む処理は `ST` で書くと効率的なコードになりました。一方、`IO` やその他のモナドで使うには、`PrimMonad` で一般化すると便利でした。

これらの良いとこどりをするなら、処理の主要な部分を `ST` で書いて、`stToPrim` を使ったラッパーを用意するという形になるでしょう。ラッパーは小さいので `INLINE` プラグマを指定します。

```haskell
mkSieveST :: Int -> ST s (VUM.MVector s Bool)
mkSieveST !n = ... -- 処理の主要な部分

{-# INLINE mkSieve #-}
mkSieve :: PrimMonad m => Int -> m (VUM.MVector (PrimState m) Bool)
mkSieve !n = stToPrim $ mkSieveST n
```

もちろん、これが通用するのは処理の主要な部分が `ST` で完結する場合であり、ユーザーから与えられた `a -> m b` のような（モナド `m` が具体的ではない）アクションを処理中に呼び出す必要がある場合はこの手は使えません。

## 特殊化されたか確認する方法

「特殊化」というキーワードが出てきましたが、Haskellコードをコンパイルする際に特殊化が起こっているかを確認する方法を簡単に説明します。

まず、GHCのコンパイルオプションに `-ddump-to-file -ddump-prep` を指定します。Cabalなら

```
    ghc-options: -Wall -ddump-to-file -ddump-prep
```

と指定すると良いでしょう。`-ddump-prep` は最適化後の中間言語Coreを出力するオプションで、`-ddump-to-file` でそれをファイルに書き出すようにします（デフォルトでは標準出力だったか標準エラー出力だったかに書き出されます）。

ビルドすると、`（モジュール名）.dump-prep` というようなファイルが生成されます。Cabalを使う場合は `dist-newstyle/` 以下のどこかになるので、頑張って探してください。

例えば、`Main` モジュールに書いた関数呼び出しが特殊化されているか見たい場合は `Main.dump-prep` を関数名で検索します。例えば、

```
MyLib.$wmkSievePM @GHC.Types.IO Control.Monad.Primitive.$fPrimMonadIO 10000000#
```

というコードが見つかったら、これは型クラス `PrimMonad` の辞書 `$fPrimMonadIO` が値として渡されているので、「特殊化されていない」という結論になります。

一方、特殊化された場合は例えば

```
Main.$w$s$wmkSievePMInlinable [InlPrag=INLINABLE[2]]
  :: GHC.Prim.Int#
     -> GHC.Prim.State# GHC.Prim.RealWorld
     -> (# GHC.Prim.State# GHC.Prim.RealWorld, GHC.Prim.Int#,
           GHC.Prim.Int#, GHC.Prim.MutableByteArray# GHC.Prim.RealWorld #)
```

といういかにも特殊化されたような関数が `.dump-prep` に書き出されます。

---

「[HaskellのPrimMonadとうまく付き合う その2：Brainfuckインタープリターを題材にして](haskell-primmonad-interpreter)」へ続く
