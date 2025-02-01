---
title: "HaskellのPrimMonadとうまく付き合う その2：Brainfuckインタープリターを題材にして"
emoji: "✨"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell, brainfuck]
published: true
---

前回の記事「[HaskellのPrimMonadとうまく付き合う その1](./haskell-primmonad)」では、Haskellで `PrimMonad` を活用する際の基本的な注意事項と対策を紹介しました。

前回題材として取り上げた処理は主要な部分が `ST` で完結するものでした。一方で、今回は処理の一部に（ユーザーから与えられた）任意のアクションを含む場合を取り上げます。

サンプルコードはGitHubの[haskell-primmonad-example/brainfuck](https://github.com/minoki/haskell-primmonad-example/tree/main/brainfuck)に上がっています。

## 題材：Brainfuckインタープリター

Brainfuckという、処理系が非常に作りやすいプログラミング言語があります。これのインタープリターをHaskellで書いてみましょう。

完全なコードはGitHubを見てもらうとして、重要な部分を抜き出します。

```haskell
data Instruction = Arith !Word8 -- +-
                 | Pointer !Int -- ><
                 | Input        -- ,
                 | Output       -- .
                 | BeginLoop [Instruction] -- 値が 0 だったら [Instruction] の実行に移る
                 | EndLoop [Instruction] -- 値が 0 以外だったら [Instruction] の実行に移る
                 deriving (Eq, Show)

parseAll :: String -> [Instruction]
parseAll = ... -- 省略

data State s = State { pointer :: !Int
                     , array   :: !(VUM.MVector s Word8)
                     }

newState :: PrimMonad m => Int -> m (State (PrimState m))
newState !size = do
  a <- VUM.replicate size 0
  pure $ State { pointer = 0, array = a }
```

インタープリターのメインループは次のように書けます：

```haskell
runIO :: [Instruction] -> State RealWorld -> IO (State RealWorld)
runIO insns State { pointer = initialPointer, array = array } = do
  let loop !pointer [] = pure pointer
      loop !pointer (insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loop pointer insns
          Pointer delta -> loop (pointer + delta) insns
          Output -> do
            !c <- VUM.read array pointer
            putChar (chr $ fromIntegral c)
            loop pointer insns
          Input -> do
            !c <- getChar `catch` \(_ :: IOError) -> pure '\xFF'
            VUM.write array pointer (fromIntegral $ ord c)
            loop pointer insns
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer alt
              else loop pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer insns
              else loop pointer alt
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }
```

さて、この実装では入出力を直接 `putChar` や `getChar` でベタ書きしています。しかし、時には入出力をファイルにリダイレクトしたり、GUIで読み書きしたりしたいこともあるでしょう。あるいは、`ContT` モナドを使って、Brainfuckプログラムを `String -> m String` のような関数に変換したいこともあるかもしれません。

そこで、`Output` と `Input` で実行するアクションを `Word8 -> m ()` や `m Word8` という型のアクションとして外部から注入できるようにします。配列の読み書きが必要なので、モナドは任意のものではなく、`PrimMonad` 制約を満たすものとします。一般化したコードは次のようになります：

```haskell
runPM :: PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
runPM put get insns State { pointer = initialPointer, array = array } = do
  let loop !pointer [] = pure pointer
      loop !pointer (insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loop pointer insns
          Pointer delta -> loop (pointer + delta) insns
          Output -> do
            !c <- VUM.read array pointer
            put c
            loop pointer insns
          Input -> do
            !c <- get
            VUM.write array pointer c
            loop pointer insns
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer alt
              else loop pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loop pointer insns
              else loop pointer alt
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }
```

前回と同じように、`INLINABLE` を指定したバージョンも用意します：

```haskell
{-# INLINABLE runPMInlinable #-}
runPMInlinable :: PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
```

## 脇道：`ContT` モナドモナド変換子の活用

脇道にはなりますが、「`ContT` モナドを使って、Brainfuckプログラムを `String -> m String` のような関数に変換したい」の部分を実現してみました。サンプルプログラムの `cps-bf` として試せます。

この場合、インタープリターの実行結果は

```haskell
data Answer s = NeedMoreInput String (String -> ST s (Answer s))
              | Done String
```

という型の値として取り出せて、実行が（入力を必要とせず）完了した場合は `Done （出力）` が、追加の入力が必要な場合は `NeedMoreInput （これまでの出力） （続行するための関数）` として返ってきます。

コードは次のようになります：

```haskell
type K s = ContT (Answer s) (ST s)
type M s = ReaderT (STRef s String, STRef s String, String -> K s String) (K s)

put :: Word8 -> M s ()
put c = do
  let c' = chr $ fromIntegral c
  (_, bufOut, _) <- ask
  lift $ lift $ modifySTRef' bufOut (c' :)

get :: M s Word8
get = do
  (bufIn, bufOut, needMoreInput) <- ask
  input <- lift $ lift $ readSTRef bufIn
  case input of
    c:cs -> do
      lift $ lift $ writeSTRef bufIn cs
      pure $ fromIntegral $ ord c
    [] -> do
      output <- fmap reverse $ lift $ lift $ readSTRef bufOut
      lift $ lift $ writeSTRef bufOut []
      newInput <- lift $ needMoreInput output
      case newInput of
        c:cs -> do
          lift $ lift $ writeSTRef bufIn cs
          pure $ fromIntegral $ ord c
        [] -> pure 0xFF

interpret :: M s (Brainfuck.State s) -> ST s (Answer s)
interpret run = do
  bufIn <- newSTRef []
  bufOut <- newSTRef []
  let action = callCC $ \k -> do
        let needMoreInput output = callCC $ \l -> k (NeedMoreInput output (\newInput -> evalContT (l newInput)))
        _finalState <- runReaderT run (bufIn, bufOut, needMoreInput)
        output <- fmap reverse $ lift $ readSTRef bufOut
        pure $ Done output
  evalContT action

main :: IO ()
main = do
  args <- getArgs
  ans0 <- case args of
    "pm":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      ans0 <- stToIO $ interpret $ Brainfuck.runPM put get program state
    "pmi":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      stToIO $ interpret $ Brainfuck.runPMInlinable put get program state
    "mixed":programName:_ -> do
      program <- Brainfuck.parseAll <$> readFile programName
      state <- Brainfuck.newState 67108864
      stToIO $ interpret $ Brainfuck.runPMMixed put get program state
    _ -> do hPutStrLn stderr "usage: cps-bf pm/pmi/mixed <program.bf>"
            exitFailure
  -- 使用例
  case ans0 of
    Done s -> putStrLn $ "output[0]: " ++ s
    NeedMoreInput s more -> do
      putStrLn $ "output[0]: " ++ s
      ans1 <- stToIO $ more "Hello!"
      case ans1 of
        Done s1 -> putStrLn $ "output[1]: " ++ s1
        NeedMoreInput s1 _ -> do
          putStrLn $ "output[1]: " ++ s1
```

まあこれはデモなので、「`ContT` はすごそう」ということだけ認識してください。

## 実行速度の確認

非自明なBrainfuckプログラムを実行して、時間を測ってみましょう。ここでは、15までのFizzBuzzを（手書きするのは大変なので、C言語で書いて）ELVMの[8cc.js](https://shinh.skr.jp/elvm/8cc.js.html)でコンパイルしたものを使います。できたBrainfuckプログラムはサンプルのGitリポジトリの `fizzbuzz15.bf` にあります。

```c
// ELVM+8cc.jsを使ってこれをBrainfuckにコンパイルする
#include <stdio.h>

// printfは遅いので自分で整数の文字列化を書く
void putint(int n)
{
    if (n == 0) {
        putchar('0');
        return;
    } else if (n < 0) {
        putchar('-');
        n = -n;
    }
    char buf[10]; // やっつけなので、オーバーフローしないことを祈る
    char *p = buf;
    while (n != 0) {
        *p++ = '0' + (n % 10);
        n = n / 10;
    }
    while (p != buf) {
        putchar(*--p);
    }
}

int main(void)
{
    for (int i = 1; i <= 15; i++) {
        if (i % 5) {
            if (i % 3) {
                // printf("%d\n", i);
                putint(i);
                putchar('\n');
            } else {
                puts("Fizz");
            }
        } else {
            puts("FizzBuzz" + i * i % 3 * 4);
        }
    }
    return 0;
}
```

私の環境（Apple M4 Pro）で、最初の `runIO` を使って実行すると、これは35秒で実行できました。

```
$ cabal exec -O2 time simple-bf io fizzbuzz15.bf
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
       35.62 real        35.28 user         0.30 sys
```

特殊化しない `runPM` を使うと遅くなったり、INLINABLEを指定した `runPMInlinable` を使うと特殊化できて速くなったりするのは前回の記事と同様です。ちなみに、私の環境では特殊化しない `runPM` は190秒かかりました。

```
$ cabal exec -O2 time simple-bf pm fizzbuzz15.bf 
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
      190.13 real       188.15 user         1.85 sys
```

## `ContT` 版の速度を改善する

一方、`ContT` を使った方のインタープリターでは、特殊化しても40秒かかりました。

```
$ cabal exec -O2 time cps-bf pmi fizzbuzz15.bf
output[0]: 1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz

       40.84 real        40.12 user         0.37 sys
```

これは `ContT` を使ったことによる避けられないコストなのでしょうか？`ContT` の強力さを享受しつつ、もうちょっと改善できないのでしょうか？

インタープリターの動作を考えてみると、算術命令やポインターの命令・ループなど（Brainfuckで言うと `+-><[]`）の実行が、入出力命令（Brainfuckで言うと `,.`）に比べて圧倒的に多いと思われます。そこで、**入出力命令以外を `ST` モナドで実行し、入出力命令だけを与えられたモナドで実行する**ことを考えます。コードで書くと次のようになります：

```haskell
runPMMixed :: forall m. PrimMonad m => (Word8 -> m ()) -> m Word8 -> [Instruction] -> State (PrimState m) -> m (State (PrimState m))
runPMMixed put get insns State { pointer = initialPointer, array = array } = do
      -- loopST では入出力以外の命令を ST モナドで実行する
      -- 入出力やプログラムの終端に遭遇したら、その時の実行状態（残りの命令列と、ポインターの位置）を返す
  let loopST :: Int -> [Instruction] -> ST (PrimState m) ([Instruction], Int)
      loopST !pointer [] = pure ([], pointer)
      loopST !pointer insns0@(insn : insns) = do
        case insn of
          Arith delta -> VUM.modify array (+ delta) pointer >> loopST pointer insns
          Pointer delta -> loopST (pointer + delta) insns
          Output -> pure (insns0, pointer)
          Input -> pure (insns0, pointer)
          BeginLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loopST pointer alt
              else loopST pointer insns
          EndLoop alt -> do
            !v <- VUM.read array pointer
            if v == 0
              then loopST pointer insns
              else loopST pointer alt
      -- loop では入出力を与えられた m モナドで実行する
  let loop !pointer [] = pure pointer
      loop !pointer (Output : insns) = do
        !c <- VUM.read array pointer
        put c
        continue pointer insns
      loop !pointer (Input : insns) = do
        !c <- get
        VUM.write array pointer c
        continue pointer insns
      loop !pointer insns = continue pointer insns
      continue pointer insns = do
        (insns', pointer') <- stToPrim $ loopST pointer insns
        loop pointer' insns'
  !pointer <- loop initialPointer insns
  pure $ State { pointer = pointer, array = array }
```

これを使って実行すると、直接 `IO` で書いたのと同程度の、30秒で実行が完了しました：

```
$ cabal exec -O2 time cps-bf mixed fizzbuzz15.bf
output[0]: 1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz

       30.90 real        30.61 user         0.25 sys
```

つまり、**ユーザーから与えられたアクションを実行しなければならない状況でも、`ST` で済む処理が大半であれば、その部分を `ST` で書くことにより抽象化によるコスト増を最小限に抑えられる**、ということです。

## JITコンパイルでの類例

JITコンパイル界隈では、JITコンパイルされたコードから何らかの事情（JITコンパイルの前提が崩れた、JITコンパイルできない処理にぶつかった、等）でインタープリターに制御を戻すことを、deoptimizationとかbailoutなどと呼ぶようです。本記事の `runPMMixed` 関数も、「`ST` モナドで書いた高速なインタープリターから何らかの事情（ユーザーに与えられたアクションを実行する必要がある）で抽象的な `m` についてのインタープリターに制御を戻す」と捉えられるので、deoptimizationとかbailoutの亜種と考えることができるかもしれません。

