---
title: "HaskellでEDSLを作る：StableName編 〜共有の回復〜"
emoji: "🌟"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell, DSL]
published: true
---

シリーズ：

* [HaskellでEDSLを作る：atomicModifyIORef編 〜自動微分を題材に〜](haskell-dsl-atomicmodifyioref)
* HaskellでEDSLを作る：StableName編 〜共有の回復〜（この記事）
* HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜（後日公開）
* HaskellでEDSLを作る：SIMD編（後日公開）

[HaskellでEDSLを作る：atomicModifyIORef編](haskell-dsl-atomicmodifyioref)では、`unsafePerformIO` と `atomicModifyIORef` を使って、純粋な計算の中で行われている計算をキャプチャーし、リバースモード自動微分を実装する例を見ました。そして、その手法では計算の共有を取り扱えることを述べました。

HaskellでEDSLを作る上で、計算の共有を観測し、利用する方法は他にもあります。それがここで紹介する `StableName` です。前回述べたように、計算の共有は純粋なコードからは観測できてはいけないので、`StableName` を使う場合でも `IO` が絡んできます。

サンプルコードは[haskell-dsl-example/stablename](https://github.com/minoki/haskell-dsl-example/tree/main/stablename)に載せています。

## 四則演算DSL

変数を一個含み、四則演算ができる式のデータ型を作ります。抽象構文木（AST）です。

```haskell:Simple.hs
data Exp = Const Double
         | Var
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         deriving Show
```

これに対して演算子オーバーロードを行えば、四則演算ができるDSLができます。

```haskell:Simple.hs
instance Num Exp where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Const . fromInteger
  abs = undefined
  signum = undefined

instance Fractional Exp where
  (/) = Div
  fromRational = Const . fromRational
```

評価する関数も用意します。

```haskell:Simple.hs
eval :: Exp -> Double -> Double
eval (Const k) _ = k
eval Var x = x
eval (Add a b) x = eval a x + eval b x
eval (Sub a b) x = eval a x - eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Div a b) x = eval a x / eval b x
```

使用例は次のようになります。

```haskell:Simple.hs
main :: IO ()
main = do
  let f x = (x + 1)^10
  print $ eval (f Var) 3.0 -- 1048576.0
```

普通に評価するだけだと面白みがないですが、`Exp` はデータなので、各種変換を適用できるのが強みです。例えば、前回例題として使った自動微分を適用したり、式を文字列として書き出したり、機械語にJITコンパイルすることもできるでしょう。

## 計算の共有の喪失

さて、さっきの使用例では、`eval` の際の `Double` の足し算 `+` は何回呼び出されているでしょうか？`Debug.Trace` を使って観察してみましょう。

```haskell
import Debug.Trace

...
eval (Add a b) x = trace "+" (eval a x + eval b x)
...
```

実行結果は次のようになります：

```
$ runghc Simple.hs
+
+
+
+
+
+
+
+
+
+
1048576.0
```

10回表示されました。つまり、ソース上は `x + 1` の一箇所だった足し算が、ASTを経由することにより10回も評価される羽目になってしまったのです！

`print $ f Var` により構築された構文木を表示してみると、当たり前ですが `Add Var (Const 1.0)` が10回出現します：

```haskell
Mul (Mul (Mul (Mul (Add Var (Const 1.0)) (Add Var (Const 1.0))) (Mul (Add Var (Const 1.0)) (Add Var (Const 1.0)))) (Mul (Mul (Add Var (Const 1.0)) (Add Var (Const 1.0))) (Mul (Add Var (Const 1.0)) (Add Var (Const 1.0))))) (Mul (Add Var (Const 1.0)) (Add Var (Const 1.0)))
```

普通のHaskellでは

```haskell
f x = (x + 1)^10
```

あるいは

```haskell
f x = let y = x + 1
          y2 = y * y
          y4 = y2 * y2
          y8 = y4 * y4
      in y8 * y2
```

と書いたら（アグレッシブな最適化や、マルチスレッドでの処理により増減はあるかもしれませんが）多くの場合は `x + 1` は1回しか評価されません。これは、`let` 構文や関数の引数への束縛により、**計算の共有**が実現できていることを意味します。

それがDSLを経由することにより失われてしまいました。DSLを構築する際に、計算の共有を回復することはできないのでしょうか？

## 計算の共有の回復：`StableName`

### 概要

Haskellで作ったデータ構造から計算の共有を回復する方法の一つとして、`StableName` というものがあります。これは [`System.Mem.StableName` モジュール](https://hackage.haskell.org/package/base/docs/System-Mem-StableName.html)で定義されています。

```haskell
module System.Mem.StableName

data StableName a

instance Eq (StableName a)

makeStableName :: a -> IO (StableName a)
hashStableName :: StableName a -> Int
eqStableName :: StableName a -> StableName b -> Bool
```

`StableName` は、メモリ上に確保されたオブジェクトとしての同一性を判定することができます（オブジェクト指向ではない言語でも、メモリ上に確保された一塊の領域を「オブジェクト」と呼ぶことがあります）。GHCではGCによってオブジェクトが移動することがありますが、オブジェクトに対して移動しても変わらない（安定な）「名前」を与えることから `StableName` というのだと思われます。

使い方としては、`makeStableName` によって `a` 型から `StableName a` 型の値を作ります。同じオブジェクトから作られた `StableName` は基本的に「等しい」と判断されます。`makeStableName x` は `IO` 計算なので注意が必要です。純粋に見せたい関数から使いたい時は、どこかで `unsafePerformIO` が必要になります。

注意点として、同じ値であっても評価前と評価後で異なる `StableName` が得られることがあります。試してみましょう。

```haskell
ghci> import System.Mem.StableName 
ghci> x :: Double; x = 1 + 1 -- 未評価の計算を作る
ghci> n1 <- makeStableName x
ghci> x -- x を評価する
2.0
ghci> n2 <- makeStableName x
ghci> n1 == n2 -- 同じ x に対する StableName であっても異なる
False
```

そのため、`makeStableName` に値を渡す前に少なくともWHNFまで評価しておくのが良いと思われます。

`StableName` はハッシュ値を計算できるので、ハッシュテーブルのキーにできます。一方、`Ord` のインスタンスにはなっていないので、順序を要求するデータ構造のキーには使えません。

### 実演

先ほどの `Exp` 型から計算の共有を回復してみましょう。まず、計算の共有を表現できる式の表現を用意します。ここでは、ネストした式を `let` の羅列にし、`let` の右辺にはネストしない式だけが来るようにします。

```haskell:ExpS.hs
type Id = Int -- 変数の識別子

data Value = ConstV Double
           | VarV Id
           deriving Show

-- letの右辺の式
data SimpleExp = AddS Value Value
               | SubS Value Value
               | MulS Value Value
               | DivS Value Value
               deriving Show

-- 共有を表現できる式
data ExpS = Let Id SimpleExp ExpS
          | Value Value
          deriving Show
```

`Exp` から共有を回復して `ExpS` を構築する関数は次のようになります。

```haskell
{- cabal:
build-depends: base, unordered-containers, transformers
-}
{-# LANGUAGE BangPatterns #-}
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HM
import           System.Mem.StableName

-- ...

type Name = StableName Exp

-- 状態：(次に使う識別子, これまでに定義した変数と式のリスト, StableNameから変数名への対応)
type M = StateT (Int, [(Id, SimpleExp)], HM.HashMap Name Id) IO

recoverSharing :: Exp -> IO ExpS
recoverSharing x = do
    (v, (_, revLets, _)) <- runStateT (go x) (1, [], HM.empty)
    pure $ foldl (\x (i, s) -> Let i s x) (Value v) revLets
  where
    makeSimpleExp :: Name -> SimpleExp -> M Value
    makeSimpleExp n s = do
      (!i, _, _) <- get
      modify $ \(_, acc, m) -> (i + 1, (i, s) : acc, HM.insert n i m)
      pure $ VarV i

    go :: Exp -> M Value
    go !x = do
      n <- lift $ makeStableName x
      (_, _, m) <- get
      case HM.lookup n m of
        Just i ->
          -- すでに出現した項であればそれを変数として参照する
          pure $ VarV i
        Nothing ->
          case x of
            Const k -> pure $ ConstV k
            Var -> pure $ VarV 0
            -- これまでに出現していない項であればletを作る
            Add y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ AddS y' z'
            Sub y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ SubS y' z'
            Mul y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ MulS y' z'
            Div y z -> do
              y' <- go y
              z' <- go z
              makeSimpleExp n $ DivS y' z'
```

試しに、`(x + 1)^10` の式から共有を回復してみましょう：

```haskell:ExpS.hs
main :: IO ()
main = do
  let f x = (x + 1)^10
  e <- recoverSharing (f Var)
  print e
```

実行結果：

```
$ cabal run ExpS.hs    
Let 1 (AddS (VarV 0) (ConstV 1.0)) (Let 2 (MulS (VarV 1) (VarV 1)) (Let 3 (MulS (VarV 2) (VarV 2)) (Let 4 (MulS (VarV 3) (VarV 3)) (Let 5 (MulS (VarV 4) (VarV 2)) (Value (VarV 5))))))
```

得られた式をHaskell風に書くと

```haskell
let v1 = v0 + 1.0 in
  let v2 = v1 * v1 in
    let v3 = v2 * v2 in
      let v4 = v3 * v3 in
        let v5 = v4 * v2 in
          v5
```

となるので、確かに共有を回復できていることがわかりました。あとは自動微分にかけるなりJITコンパイルするなり、好きにできます。

注意点として、得られる共有の形はコンパイラーの最適化によって変動することがあります。例えば、

```haskell
main = do
  let g x = (x + 1) * (x + 1)
  e <- recoverSharing (g Var)
  print e
```

という例は最適化が無効だと

```
Let 1 (AddS (VarV 0) (ConstV 1.0)) (Let 2 (AddS (VarV 0) (ConstV 1.0)) (Let 3 (MulS (VarV 1) (VarV 2)) (Value (VarV 3))))
```

つまり

```haskell
let v1 = v0 + 1.0 in
  let v2 = v0 + 1.0 in
    let v3 = v1 * v2 in
      v3
```

となり、最適化が有効だと

```
Let 1 (AddS (VarV 0) (ConstV 1.0)) (Let 2 (MulS (VarV 1) (VarV 1)) (Value (VarV 2)))
```

つまり

```haskell
let v1 = v0 + 1.0 in
  let v2 = v1 * v1 in
    v2
```

となるかもしれません。共有の観測結果が最適化によって違うということは、共有の観測がある種のランダムさを伴う操作であり、それゆえに `IO` が型に現れる、と解釈できるかもしれません。

共有を回復する処理を `unsafePerformIO` により純粋な関数 `Exp -> ExpS` に見せかける場合は、回復後の構文木の違いが純粋なコードから観測できないようにした方が良いかもしれません。つまり、`eval :: ExpS -> Double -> Double` はよくても、文字列化する関数は `ExpS -> IO String` という風に `IO` をかませる、という具合です。

## 実用例と文献

`StableName` を使ってHaskellのEDSLから共有を回復している実例として、GPUプログラミングで有名な[Accelerate](https://www.acceleratehs.org/)があります。論文としては、[Optimising Purely Functional GPU Programs](https://raw.githubusercontent.com/AccelerateHS/acceleratehs.github.io/master/papers/acc-optim-icfp2013.pdf)に説明があります。

この記事での例は値の型を `Double` に絞ったのでややこしさが減っていますが、Accelerateのような本格的なEDSLだと型情報もあるので大変そうです。

この辺のキーワードとしては “observable sharing” や “sharing recovery” が該当するようです。他の文献も探してみてください。
