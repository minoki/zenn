---
title: "HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜"
emoji: "😸"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell, dsl, llvm]
published: false
---

シリーズ：

* [HaskellでEDSLを作る：atomicModifyIORef編 〜自動微分を題材に〜](haskell-dsl-atomicmodifyioref)
* [HaskellでEDSLを作る：StableName編 〜共有の回復〜](haskell-dsl-stablename)
* HaskellでEDSLを作る：LLVM編 〜JITコンパイル〜（この記事）
* HaskellでEDSLを作る：SIMD編（後日公開）

[HaskellでEDSLを作る：StableName編](haskell-dsl-stablename)では、`StableName` を使って計算の共有を回復する方法を見ました。

今回は、作った四則演算DSLをLLVMでJITコンパイルする方法を見てみます。サンプルコードは[haskell-dsl-example/llvm](https://github.com/minoki/haskell-dsl-example/tree/main/llvm)に載せています。

## 概要

### LLVMの呼び出し方

HaskellからLLVMを呼び出すやり方はいくつかあります。

まず、LLVM IRをファイルとして書き出し、LLVMをコマンドとして呼び出すというやり方があります。例えば、GHCはLLVM IRを `.ll` ファイルに書き出し、LLVMの `opt` コマンドと `llc` コマンドを使ってオブジェクトファイルを得ています。この方法では、コンパイラーはテキスト処理とファイルの読み書きさえできればよく、面倒なFFIを考える必要はありません。

次に、LLVMをC/C++のライブラリーとして呼び出すという方法です。Haskellから呼び出すにはFFIが必要となります。この方法は、JITコンパイルに適しています。

この記事では、後者、つまりFFIを使ってLLVMを呼び出す方法を取ります。ただし、FFIの部分を自前で書くのは大変なので、既にあるバインディングを利用します。

### LLVMへのバインディング

HaskellからLLVMを呼び出すためのバインディングはいくつか存在します。ここでは、[llvm-hs](https://github.com/llvm-hs)のファミリーを利用します。

llvm-hsファミリーのパッケージの構成は以下のようになります：

* llvm-hs-pure: C++の部分には依存しない純Haskellの部分。LLVM IRを構築できる。
* llvm-hs: C++で実装されたLLVMへのバインディング（FFI）。
* [llvm-hs-pretty](https://github.com/llvm-hs/llvm-hs-pretty): 純Haskellのpretty printerだが、メンテされていない。pretty print自体は（C++で書かれたLLVMに依存する）llvm-hsパッケージでできる。

llvm-hsはHackageには古いやつしか上がっていないので、GitHubにあるものを利用します。執筆時点では、llvm-15ブランチにあるLLVM 15対応のものが最新で、これを使います。ただ、執筆時点のものはGHC 9.8以降に対応していないようです（`LLVM.Prelude` でimportしている `unzip` が衝突する）。また、Cabalも新しいものはダメで3.10以下にしないとダメそうです。なので、この記事の内容はGHC 9.6.6/Cabal 3.10で動作確認しています。ちなみに、llvm-hsのGitHubのPRを見ると新しめのGHCやCabalに対応させたものがあるようです。

Cabalのプロジェクトで、依存パッケージをGitから取ってくるには、`cabal.project` に次のように `source-repository-package` を記述します：

```:cabal.project
packages: ./*.cabal

source-repository-package
    type: git
    location: https://github.com/llvm-hs/llvm-hs.git
    tag: 5bca2c1a2a3aa98ecfb19181e7a5ebbf3e212b76
    subdir: llvm-hs-pure

source-repository-package
    type: git
    location: https://github.com/llvm-hs/llvm-hs.git
    tag: 5bca2c1a2a3aa98ecfb19181e7a5ebbf3e212b76
    subdir: llvm-hs
```

llvm-hsのビルドにはLLVMが必要となります。具体的には、configure時に `llvm-config-15` コマンドあるいは `llvm-config` コマンドが見えている必要があります。Homebrew等を利用しているとデフォルトではこれらへのPATHが通っていないので、`cabal.project.local` に以下のように記述してllvm-hsが `llvm-config` を見つけられるようにします：

```:cabal.project.local
package llvm-hs
    extra-prog-path: /opt/homebrew/opt/llvm@15/bin
```

Homebrewの場合は、具体的な場所は `echo $(brew --prefix llvm@15)/bin` でわかります。

llvm-hsのllvm-15ブランチに対するHaddockはHackage等では見られないので、ドキュメントが見たい方は自分で `git clone` してきて `cabal haddock` を実行してください。

### 参考にできるコード

[llvm-hs-examples](https://github.com/llvm-hs/llvm-hs-examples)に幾つかサンプルがありますが、masterブランチはLLVM 9向けで、他にはllvm-12ブランチがあるくらいなので、若干古いです。

我々の四則演算DSLに近いのは[llvm-hs-examples/arith/Arith.hs](https://github.com/llvm-hs/llvm-hs-examples/blob/llvm-12/arith/Arith.hs)です。

ここでは `arith/Arith.hs` を参考に、llvm-15対応のコードを提示します。

### コンパイルした関数の呼び出し

JITコンパイルして `FunPtr` 型の関数ポインターが得られたとします。これをHaskellから呼び出すには、`foreign import ccall "dynamic"` で次のような関数を定義します：

```haskell
foreign import ccall unsafe "dynamic"
  mkDoubleFn :: FunPtr (Double -> Double) -> (Double -> Double)
```

こうやって定義した `mkDoubleFn` 関数で、関数ポインターをHaskellの関数へ変換できます。

unsafe FFIはオーバーヘッドが少ないですが、短時間で終わる処理を想定しています。時間がかかる処理はsafe FFIにした方が良いかもしれません。FFIの種類の話は昔描いた「[【低レベルHaskell】Haskell (GHC) でもインラインアセンブリに肉薄したい！ #assembly - Qiita](https://qiita.com/mod_poppo/items/793fdb08e62591d6f3fb)」で触れました。

この方式では、関数の型がコードを書く時点で確定している必要があります。任意個数の引数に対応する関数をJITコンパイルしたい場合は、Haskellとの界面を構造体か配列へのポインターにして `Ptr SomeStruct -> IO ()` という型の関数にするか、何らかのlibffiバインディングを使用します。

## 実践

### DSLへの機能追加

前回作ったDSLは本当に四則演算だけでした。しかし、LLVMのデモとして関数呼び出しの例をやりたいので、ここではいくつかの数学関数を追加します。また、変数の型が `Double` であるとわかるように型パラメーターを持たせます。

```haskell:src/Exp.hs
data UnaryOp = Negate
             | Abs
             | Exp
             | Log
             | Sin
             | Cos
             deriving (Eq, Show)

data Exp a where
  Const :: a -> Exp a
  Var :: Exp Double
  Unary :: UnaryOp -> Exp Double -> Exp Double
  Add :: Exp Double -> Exp Double -> Exp Double
  Sub :: Exp Double -> Exp Double -> Exp Double
  Mul :: Exp Double -> Exp Double -> Exp Double
  Div :: Exp Double -> Exp Double -> Exp Double

deriving instance Show a => Show (Exp a)

instance Num (Exp Double) where
  (+) = Add
  (-) = Sub
  (*) = Mul
  fromInteger = Const . fromInteger
  negate = Unary Negate
  abs = Unary Abs
  signum = undefined

instance Fractional (Exp Double) where
  (/) = Div
  fromRational = Const . fromRational

instance Floating (Exp Double) where
  pi = Const pi
  exp = Unary Exp
  log = Unary Log
  sin = Unary Sin
  cos = Unary Cos
  asin = undefined
  acos = undefined
  atan = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  acosh = undefined
  atanh = undefined

eval :: Exp a -> Double -> a
eval (Const k) _ = k
eval Var x = x
eval (Unary Negate a) x = - eval a x
eval (Unary Abs a) x = abs (eval a x)
eval (Unary Exp a) x = exp (eval a x)
eval (Unary Log a) x = log (eval a x)
eval (Unary Sin a) x = sin (eval a x)
eval (Unary Cos a) x = cos (eval a x)
eval (Add a b) x = eval a x + eval b x
eval (Sub a b) x = eval a x - eval b x
eval (Mul a b) x = eval a x * eval b x
eval (Div a b) x = eval a x / eval b x
```

共有を回復する部分は次のようになります：

```haskell:src/ExpS.hs
type Id = Int -- 変数の識別子

data Value a = ConstV a
             | VarV Id
             deriving Show

-- letの右辺の式
data SimpleExp a where
  UnaryS :: UnaryOp -> Value Double -> SimpleExp Double
  AddS :: Value Double -> Value Double -> SimpleExp Double
  SubS :: Value Double -> Value Double -> SimpleExp Double
  MulS :: Value Double -> Value Double -> SimpleExp Double
  DivS :: Value Double -> Value Double -> SimpleExp Double

deriving instance Show (SimpleExp a)

-- 共有を表現できる式
data ExpS a = Let Id (SimpleExp Double) (ExpS a)
            | Value (Value a)
            deriving Show

-- 状態：(次に使う識別子, これまでに定義した変数と式のリスト, StableNameから変数名への対応)
type M = StateT (Int, [(Id, SimpleExp Double)], HM.HashMap Name Id) IO

recoverSharing :: Exp Double -> IO (ExpS Double)
recoverSharing expr = do
  -- 省略
```

完全なソースコードは[サンプルコードのリポジトリー](https://github.com/minoki/haskell-dsl-example/tree/main/llvm)を参照してください。

### llvm-hs-pureでのコード生成

LLVM IRを生成する部分は、純Haskellのライブラリーであるllvm-hs-pureでできます。先にコードを載せておきます：

```haskell:src/Codegen.hs
import qualified LLVM.AST as AST (Module, Operand(ConstantOperand))
import qualified LLVM.AST.Constant as AST (Constant(Float))
import qualified LLVM.AST.Float as AST (SomeFloat(Double))
import qualified LLVM.AST.Type as AST (Type(FunctionType, resultType, argumentTypes, isVarArg), double)
import qualified LLVM.IRBuilder.Instruction as IR (fneg, call, fadd, fsub, fmul, fdiv, ret)
import qualified LLVM.IRBuilder.Module as IR (MonadModuleBuilder, ParameterName(ParameterName), buildModule, extern, function)
import qualified LLVM.IRBuilder.Monad as IR (MonadIRBuilder, named)

doubleFnType :: AST.Type
doubleFnType = AST.FunctionType
  { AST.resultType = AST.double
  , AST.argumentTypes = [AST.double]
  , AST.isVarArg = False
  }

codegen :: ExpS Double -> AST.Module
codegen expr = IR.buildModule "dsl.ll" $ do
  absFn <- IR.extern "llvm.fabs.f64" [AST.double] AST.double
  expFn <- IR.extern "llvm.exp.f64" [AST.double] AST.double
  logFn <- IR.extern "llvm.log.f64" [AST.double] AST.double
  sinFn <- IR.extern "llvm.sin.f64" [AST.double] AST.double
  cosFn <- IR.extern "llvm.cos.f64" [AST.double] AST.double

  let goValue :: IntMap.IntMap AST.Operand -> Value Double -> AST.Operand
      goValue _ (ConstV x) = AST.ConstantOperand $ AST.Float $ AST.Double x
      goValue env (VarV i) = env IntMap.! i
      goSimpleExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> SimpleExp Double -> m AST.Operand
      -- goSimpleExp :: IntMap.IntMap AST.Operand -> SimpleExp Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goSimpleExp env (UnaryS Negate x) = IR.fneg (goValue env x)
      goSimpleExp env (UnaryS Abs x) = IR.call doubleFnType absFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Exp x) = IR.call doubleFnType expFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Log x) = IR.call doubleFnType logFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Sin x) = IR.call doubleFnType sinFn [(goValue env x, [])]
      goSimpleExp env (UnaryS Cos x) = IR.call doubleFnType cosFn [(goValue env x, [])]
      goSimpleExp env (AddS x y) = IR.fadd (goValue env x) (goValue env y)
      goSimpleExp env (SubS x y) = IR.fsub (goValue env x) (goValue env y)
      goSimpleExp env (MulS x y) = IR.fmul (goValue env x) (goValue env y)
      goSimpleExp env (DivS x y) = IR.fdiv (goValue env x) (goValue env y)
      goExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> ExpS Double -> m AST.Operand
      -- goExp :: IntMap.IntMap AST.Operand -> ExpS Double -> LLVM.IRBuilder.Module.IRBuilderT LLVM.IRBuilder.Module.ModuleBuilder AST.Operand
      goExp env (Let i x body) = do
        v <- goSimpleExp env x `IR.named` SBS.toShort (BS.Char8.pack ("x" ++ show i))
        goExp (IntMap.insert i v env) body
      goExp env (Value v) = pure $ goValue env v

  let xparam :: IR.ParameterName
      xparam = IR.ParameterName "x"
  _ <- IR.function "f" [(AST.double, xparam)] AST.double $ \[arg] -> do
    result <- goExp (IntMap.singleton 0 arg) expr
    IR.ret result
  pure ()
```

`codegen` 関数が、作ったDSLの構文木を受け取って、LLVM IRを含むモジュールを作成する関数です。モジュールには関数の定義と、外部の関数の宣言が含まれます。

llvm-hs-pureはLLVM IRを構築するためのEDSL（モナド）を提供しています。ここでの例は単純すぎてDSLという感じがしませんが、後述の自動ベクトル化の例を見るとそれがよりわかりやすいかと思います。

LLVM IRのオペランド（変数や即値）は `LLVM.AST.Operand` 型を持ちます。変数は、IRの命令に対応する関数の返り値として得られます。

`LLVM.IRBuilder.Monad.named` 関数を使うことで、定義される変数やラベルに名前をつけることができます。実際には番号などがついて、`%x_0` という風になります。

### LLVM IRのpretty print

生成したLLVM IRをテキスト形式で表示してみましょう。llvm-hs-prettyパッケージは使えないので、llvm-hsパッケージでやります。`LLVM.Module.moduleLLVMAssembly` 関数を使います。

```haskell:pp/Main.hs
import qualified Data.ByteString as BS
import qualified LLVM.Context
import qualified LLVM.Module

main :: IO ()
main = do
  let f x = (x + 1)^10
  expr <- recoverSharing (f Var)
  let code = codegen expr
  LLVM.Context.withContext $ \context ->
    LLVM.Module.withModuleFromAST context code $ \mod' -> do
      asm <- LLVM.Module.moduleLLVMAssembly mod'
      BS.putStr asm
```

出力例は次のようになります：

```
; ModuleID = 'dsl.ll'
source_filename = "<string>"

declare double @llvm.fabs.f64(double)

declare double @llvm.exp.f64(double)

declare double @llvm.log.f64(double)

declare double @llvm.sin.f64(double)

declare double @llvm.cos.f64(double)

define double @f(double %x_0) {
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x4_0, %x2_0
  ret double %x5_0
}
```

それっぽくなっていますね。

### JITコンパイル

JITコンパイルは次のようになります：

```haskell:app/Main.hs
import qualified LLVM.CodeGenOpt
import qualified LLVM.CodeModel
import qualified LLVM.Context
import qualified LLVM.Linking
import qualified LLVM.Module
import qualified LLVM.OrcJIT as JIT
import qualified LLVM.Relocation
import qualified LLVM.Passes
import qualified LLVM.Target

foreign import ccall unsafe "dynamic"
  mkDoubleFun :: FunPtr (Double -> Double) -> (Double -> Double)

withSimpleJIT :: NFData a => ExpS Double -> ((Double -> Double) -> a) -> IO (Maybe a)
withSimpleJIT expr doFun = do
  LLVM.Context.withContext $ \context -> do
    _ <- LLVM.Linking.loadLibraryPermanently Nothing
    LLVM.Module.withModuleFromAST context (codegen expr) $ \mod' -> do
      LLVM.Target.withHostTargetMachine LLVM.Relocation.PIC LLVM.CodeModel.JITDefault LLVM.CodeGenOpt.Default $ \targetMachine -> do
        asm <- LLVM.Module.moduleLLVMAssembly mod'
        putStrLn "*** Before optimization ***"
        BS.putStr asm
        putStrLn "***************************"

        let passSetSpec = LLVM.Passes.PassSetSpec
                          { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                          , LLVM.Passes.targetMachine = Nothing -- Just targetMachine
                          }
        LLVM.Passes.runPasses passSetSpec mod'

        asm' <- LLVM.Module.moduleLLVMAssembly mod'
        putStrLn "*** After optimization ***"
        BS.putStr asm'
        putStrLn "**************************"

        tasm <- LLVM.Module.moduleTargetAssembly targetMachine mod'
        putStrLn "*** Target assembly ***"
        BS.putStr tasm
        putStrLn "***********************"

        JIT.withExecutionSession $ \executionSession -> do
          dylib <- JIT.createJITDylib executionSession "myDylib"
          JIT.withClonedThreadSafeModule mod' $ \threadSafeModule -> do
            objectLayer <- JIT.createRTDyldObjectLinkingLayer executionSession
            compileLayer <- JIT.createIRCompileLayer executionSession objectLayer targetMachine
            JIT.addDynamicLibrarySearchGeneratorForCurrentProcess compileLayer dylib
            JIT.addModule threadSafeModule dylib compileLayer

            sym <- JIT.lookupSymbol executionSession compileLayer dylib "f"
            case sym of
              Left (JIT.JITSymbolError err) -> do
                print err
                pure Nothing
              Right (JIT.JITSymbol fnAddr _jitSymbolFlags) -> do
                let fn = mkDoubleFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                Just <$> evaluate (force $ doFun fn)
```

まあ私もllvm-hs-examples/arithからよく理解しないまま写経しただけなので、詳しい説明はできません。それでも、わかる範囲で何点か説明しておきます。

`LLVM.Module.withModuleFromAST` にさっき作った `codegen` 関数の返り値（モジュール `LLVM.AST.Module`）を渡しています。

さっきもやりましたが、作ったLLVM IRのテキスト表現は `LLVM.Module.moduleLLVMAssembly` 関数で取れます。ここでは最適化の前後で表示しています。

生成されたマシン依存なアセンブリコードは `LLVM.Module.moduleTargetAssembly` 関数で取れます。

`LLVM.Target.withHostTargetMachine` の引数にllvm-hs-examplesの例だと `CodeModel.Default` を使っていますが、ここは `JITDefault` にしないとAArch64で外部の関数呼び出し（`exp` や `sin`）がコケます。

最適化を実行するのが `LLVM.Passes.runPasses` です。この関数は引数のモジュールを破壊的に更新します。これはJITコンパイルに入る前に実行しておかないと生成される機械語に反映されないので注意してください。私はこれをミスったせいで後述の自動ベクトル化が効かなくて（性能の向上が見られなくて）悩みました。

JITコンパイルでできた関数のポインターは `fnAddr :: WordPtr` として取得できます。これを `FunPtr` に変換して、`foreign import ccall "dynamic"` の関数に渡し、Haskellの関数に変換しています。

作った関数はスコープから抜けると消えてしまいます。作った関数はコールバック関数 `doFun` で消費されるわけですが、その評価がスコープを抜ける前に終わるように、`Control.DeepSeq` の `force` を使っています。

### 利用

利用側は次のようになります：

```haskell:app/Main.hs
main :: IO ()
main = do
  let f x = (x + 1)^10 * (x + 1) + cos x
  expr <- recoverSharing (f Var)
  result <- withSimpleJIT expr (\f' -> f' 1.0)
  case result of
    Nothing -> pure ()
    Just result' -> print result'
```

最適化の効果がわかるように、わざと `x + 1` を2回出現させました。と言ってもGHCの最適化が有効だと共通部分式削除でまとめられてしまうので、GHCの最適化は切っておきます。

```
$ cabal run -O0 example-run
*** Before optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

declare double @llvm.fabs.f64(double)

declare double @llvm.exp.f64(double)

declare double @llvm.log.f64(double)

declare double @llvm.sin.f64(double)

declare double @llvm.cos.f64(double)

define double @f(double %x_0) {
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x4_0, %x2_0
  %x6_0 = fadd double %x_0, 1.000000e+00
  %x7_0 = fmul double %x5_0, %x6_0
  %x8_0 = call double @llvm.cos.f64(double %x_0)
  %x9_0 = fadd double %x7_0, %x8_0
  ret double %x9_0
}
***************************
*** After optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

declare double @llvm.cos.f64(double)

define double @f(double %x_0) local_unnamed_addr {
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x2_0, %x4_0
  %x7_0 = fmul double %x1_0, %x5_0
  %x8_0 = tail call double @llvm.cos.f64(double %x_0)
  %x9_0 = fadd double %x7_0, %x8_0
  ret double %x9_0
}
**************************
```

最適化前は `fadd double %x_0, 1.000000e+00` が2箇所にあったのが、最適化後は1箇所に纏められたのがわかります。

生成されたアセンブリーと実行結果の値（x=1）は次のようになります：

```
*** Target assembly ***
	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_f
	.p2align	2
_f:
	.cfi_startproc
	stp	d9, d8, [sp, #-32]!
	.cfi_def_cfa_offset 32
	stp	x29, x30, [sp, #16]
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset b8, -24
	.cfi_offset b9, -32
	fmov	d1, #1.00000000
	fadd	d1, d0, d1
	fmul	d2, d1, d1
	fmul	d3, d2, d2
	fmul	d3, d3, d3
	fmul	d2, d2, d3
	fmul	d8, d1, d2
Lloh0:
	adrp	x8, _cos@GOTPAGE
Lloh1:
	ldr	x8, [x8, _cos@GOTPAGEOFF]
	blr	x8
	fadd	d0, d8, d0
	ldp	x29, x30, [sp, #16]
	ldp	d9, d8, [sp], #32
	ret
	.loh AdrpLdrGot	Lloh0, Lloh1
	.cfi_endproc

.subsections_via_symbols
***********************
2048.540302305868
```

ここではAArch64 macOSで実行したので、AArch64のアセンブリーソースが出てきて、関数名の先頭にアンダースコアがついています。他の環境だと違う結果になると思います。

### 自動ベクトル化を利用する

LLVMによって共通部分式削除が行われるのはわかりましたが、他の最適化もやってみましょう。例えば、**自動ベクトル化**は通常のHaskellからは縁遠い機能ですが、今回のようにLLVM IRを自前で生成すればやりやすそうではないでしょうか？

具体的には、次のような関数を定義します：

```c
// C言語による疑似コード
void f(int size, double * restrict resultArray, const double *inputArray)
{
    for (int i = 0; i < size; ++i) {
        double x = inputArray[i];
        resultArray[i] = /* x を使った計算 */;
    }
}
```

すると、LLVMは自動ベクトル化によってループをSIMD命令を使うように変換してくれるであろう、という寸法です。関数の型はHaskell的には `Int32 -> Ptr Double -> Ptr Double -> IO ()` となりますが、storable vector (`Data.Vector.Storable`) で `VS.Vector Double -> VS.Vector Double` として使えるようにラップするのが良いでしょう。`Data.Vector` にはいくつかバリエーションがありますが、アドレスを取ってFFIで使う場合は基本的にstorable vectorを使います。primitive vectorやそれを使うunboxed vectorはGCによってアドレスが変わりうるのでFFIでは使いづらいです（確保時にピン留めしたりFFI時にGCを止めて良いのならFFIでも使えますが、上級者向けです）。

ループのLLVM IRを生成する部分は次のようになります：

```haskell:src/LoopCodegen.hs
{-# LANGUAGE RecursiveDo #-}

codegen :: ExpS Double -> AST.Module
codegen expr = IR.buildModule "dsl.ll" $ do
  absFn <- IR.extern "llvm.fabs.f64" [AST.double] AST.double
  expFn <- IR.extern "llvm.exp.f64" [AST.double] AST.double
  logFn <- IR.extern "llvm.log.f64" [AST.double] AST.double
  sinFn <- IR.extern "llvm.sin.f64" [AST.double] AST.double
  cosFn <- IR.extern "llvm.cos.f64" [AST.double] AST.double

  let goValue :: IntMap.IntMap AST.Operand -> Value Double -> AST.Operand
      -- 省略
      goSimpleExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> SimpleExp Double -> m AST.Operand
      -- 省略
      goExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> ExpS Double -> m AST.Operand
      -- 省略

  let sizeName :: IR.ParameterName
      sizeName = IR.ParameterName "size"
  let resultArrayName :: IR.ParameterName
      resultArrayName = IR.ParameterName "resultArray"
  let inputArrayName :: IR.ParameterName
      inputArrayName = IR.ParameterName "inputArray"
  _ <- IR.function "f" [(AST.i32, sizeName), (AST.ptr, resultArrayName), (AST.ptr, inputArrayName)] AST.void $ \[size, resultArray, inputArray] -> mdo
    prologue <- IR.block
    IR.br loop

    loop <- IR.block `IR.named` "loop"
    counter <- IR.phi [(IR.int32 0, prologue), (nextCounter, loopBody)] `IR.named` "counter"
    lt <- IR.icmp AST.SLT counter size `IR.named` "lt"
    IR.condBr lt loopBody epilogue

    loopBody <- IR.block `IR.named` "loopBody"
    xPtr <- IR.gep (AST.ArrayType 0 AST.double) inputArray [IR.int32 0, counter] `IR.named` "xPtr"
    x <- IR.load AST.double xPtr 0 `IR.named` "x"
    result <- goExp (IntMap.singleton 0 x) expr `IR.named` "result"
    resultPtr <- IR.gep (AST.ArrayType 0 AST.double) resultArray [IR.int32 0, counter] `IR.named` "resultPtr"
    IR.store resultPtr 0 result
    nextCounter <- IR.add counter (IR.int32 1) `IR.named` "nextCounter"
    IR.br loop

    epilogue <- IR.block `IR.named` "epilogue"
    IR.retVoid

  pure ()
```

ここでは詳しい説明はしません。[LLVM Language Reference Manual](https://llvm.org/docs/LangRef.html)とにらめっこしてください。何点か補足しておきます：

* llvm-hs-pureのLLVM IRの命令を出力する関数は大体想像がつくと思います。ただ、LLVMの `getelementptr` は `gep` と省略されます。
* `RecursiveDo` 拡張の `mdo` 構文を使うことによって、後ろの方で定義されるラベルを参照できています。
* `load` と `store` に渡している `0` はアラインメントで、`0` を指定してやると型のデフォルトのものが利用されるようです。

JITコンパイルする部分は大枠はさっきと同じですが、いくつか注意があります。

```haskell:loop/Main.hs
foreign import ccall unsafe "dynamic"
  mkDoubleArrayFun :: FunPtr (Int32 -> Ptr Double -> Ptr Double -> IO ()) -> (Int32 -> Ptr Double -> Ptr Double -> IO ())

withArrayJIT :: NFData a => ExpS Double -> ((VS.Vector Double -> VS.Vector Double) -> IO a) -> IO (Maybe a)
withArrayJIT expr doFun = do
  -- 略
        let passSetSpec = LLVM.Passes.PassSetSpec
                          { LLVM.Passes.passes = [LLVM.Passes.CuratedPassSet 2]
                          , LLVM.Passes.targetMachine = Just targetMachine
                          }
        LLVM.Passes.runPasses passSetSpec mod'

            -- 略

            sym <- JIT.lookupSymbol executionSession compileLayer dylib "f"
            case sym of
              Left (JIT.JITSymbolError err) -> do
                print err
                pure Nothing
              Right (JIT.JITSymbol fnAddr _jitSymbolFlags) -> do
                let ptrFn = mkDoubleArrayFun . castPtrToFunPtr $ wordPtrToPtr fnAddr
                    vecFn !inputVec = unsafePerformIO $ do
                      let !n = VS.length inputVec
                      resultVec <- VSM.unsafeNew n
                      VS.unsafeWith inputVec $ \inputPtr ->
                        VSM.unsafeWith resultVec $ \resultPtr ->
                          ptrFn (fromIntegral n) resultPtr inputPtr
                      VS.unsafeFreeze resultVec
                result <- doFun vecFn
                Just <$> evaluate (force result)
```

まず、関数の型が変わります。ここでは任意のIOアクションを実行できるようにしました。これはいいですね。

次に、`passSetSpec` で `targetMachine` を指定するようにします。これがないと、LLVMはプラットフォーム非依存の最適化しかしてくれないので、ベクトル化も行われません。

あとは与えられた関数を `VS.Vector Double -> VS.Vector Double` に見せるようにゴニョゴニョします（`vecFn` 関数）。

利用側は次のようになります：

```haskell:loop/Main.hs
main :: IO ()
main = do
  let f x = (x + 1)^10 * (x + 1)
  expr <- recoverSharing (f Var)
  _ <- withArrayJIT expr $ \vf -> do
    print $ vf (VS.fromList [1..20])
  pure ()
```

AArch64 macOSでの実行結果も載せておきます：

```
*** Before optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

declare double @llvm.fabs.f64(double)

declare double @llvm.exp.f64(double)

declare double @llvm.log.f64(double)

declare double @llvm.sin.f64(double)

declare double @llvm.cos.f64(double)

define void @f(i32 %size_0, ptr %resultArray_0, ptr %inputArray_0) {
  br label %loop_0

loop_0:                                           ; preds = %loopBody_0, %0
  %counter_0 = phi i32 [ 0, %0 ], [ %nextCounter_0, %loopBody_0 ]
  %lt_0 = icmp slt i32 %counter_0, %size_0
  br i1 %lt_0, label %loopBody_0, label %epilogue_0

loopBody_0:                                       ; preds = %loop_0
  %xPtr_0 = getelementptr [0 x double], ptr %inputArray_0, i32 0, i32 %counter_0
  %x_0 = load double, ptr %xPtr_0, align 8
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x4_0, %x2_0
  %x6_0 = fmul double %x5_0, %x1_0
  %resultPtr_0 = getelementptr [0 x double], ptr %resultArray_0, i32 0, i32 %counter_0
  store double %x6_0, ptr %resultPtr_0, align 8
  %nextCounter_0 = add i32 %counter_0, 1
  br label %loop_0

epilogue_0:                                       ; preds = %loop_0
  ret void
}
***************************
*** After optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

; Function Attrs: argmemonly nofree norecurse nosync nounwind
define void @f(i32 %size_0, ptr nocapture writeonly %resultArray_0, ptr nocapture readonly %inputArray_0) local_unnamed_addr #0 {
  %lt_01 = icmp sgt i32 %size_0, 0
  br i1 %lt_01, label %loopBody_0.preheader, label %epilogue_0

loopBody_0.preheader:                             ; preds = %0
  %resultArray_03 = ptrtoint ptr %resultArray_0 to i64
  %inputArray_04 = ptrtoint ptr %inputArray_0 to i64
  %min.iters.check = icmp ult i32 %size_0, 4
  %1 = sub i64 %resultArray_03, %inputArray_04
  %diff.check = icmp ult i64 %1, 32
  %or.cond = select i1 %min.iters.check, i1 true, i1 %diff.check
  br i1 %or.cond, label %loopBody_0.preheader6, label %vector.ph

vector.ph:                                        ; preds = %loopBody_0.preheader
  %n.vec = and i32 %size_0, -4
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph
  %index = phi i32 [ 0, %vector.ph ], [ %index.next, %vector.body ]
  %2 = zext i32 %index to i64
  %3 = getelementptr [0 x double], ptr %inputArray_0, i64 0, i64 %2
  %wide.load = load <2 x double>, ptr %3, align 8
  %4 = getelementptr double, ptr %3, i64 2
  %wide.load5 = load <2 x double>, ptr %4, align 8
  %5 = fadd <2 x double> %wide.load, <double 1.000000e+00, double 1.000000e+00>
  %6 = fadd <2 x double> %wide.load5, <double 1.000000e+00, double 1.000000e+00>
  %7 = fmul <2 x double> %5, %5
  %8 = fmul <2 x double> %6, %6
  %9 = fmul <2 x double> %7, %7
  %10 = fmul <2 x double> %8, %8
  %11 = fmul <2 x double> %9, %9
  %12 = fmul <2 x double> %10, %10
  %13 = fmul <2 x double> %7, %11
  %14 = fmul <2 x double> %8, %12
  %15 = fmul <2 x double> %5, %13
  %16 = fmul <2 x double> %6, %14
  %17 = getelementptr [0 x double], ptr %resultArray_0, i64 0, i64 %2
  store <2 x double> %15, ptr %17, align 8
  %18 = getelementptr double, ptr %17, i64 2
  store <2 x double> %16, ptr %18, align 8
  %index.next = add nuw i32 %index, 4
  %19 = icmp eq i32 %index.next, %n.vec
  br i1 %19, label %middle.block, label %vector.body, !llvm.loop !0

middle.block:                                     ; preds = %vector.body
  %cmp.n = icmp eq i32 %n.vec, %size_0
  br i1 %cmp.n, label %epilogue_0, label %loopBody_0.preheader6

loopBody_0.preheader6:                            ; preds = %loopBody_0.preheader, %middle.block
  %counter_02.ph = phi i32 [ 0, %loopBody_0.preheader ], [ %n.vec, %middle.block ]
  br label %loopBody_0

loopBody_0:                                       ; preds = %loopBody_0.preheader6, %loopBody_0
  %counter_02 = phi i32 [ %nextCounter_0, %loopBody_0 ], [ %counter_02.ph, %loopBody_0.preheader6 ]
  %20 = zext i32 %counter_02 to i64
  %xPtr_0 = getelementptr [0 x double], ptr %inputArray_0, i64 0, i64 %20
  %x_0 = load double, ptr %xPtr_0, align 8
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x2_0, %x4_0
  %x6_0 = fmul double %x1_0, %x5_0
  %resultPtr_0 = getelementptr [0 x double], ptr %resultArray_0, i64 0, i64 %20
  store double %x6_0, ptr %resultPtr_0, align 8
  %nextCounter_0 = add nuw nsw i32 %counter_02, 1
  %lt_0 = icmp slt i32 %nextCounter_0, %size_0
  br i1 %lt_0, label %loopBody_0, label %epilogue_0, !llvm.loop !2

epilogue_0:                                       ; preds = %loopBody_0, %middle.block, %0
  ret void
}

attributes #0 = { argmemonly nofree norecurse nosync nounwind }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.isvectorized", i32 1}
!2 = distinct !{!2, !1}
**************************
*** Target assembly ***
	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_f
	.p2align	2
_f:
	cmp	w0, #1
	b.lt	LBB0_8
	mov	w8, #0
	cmp	w0, #4
	b.lo	LBB0_6
	sub	x9, x1, x2
	cmp	x9, #32
	b.lo	LBB0_6
	and	w8, w0, #0xfffffffc
	add	x9, x1, #16
	add	x10, x2, #16
	fmov.2d	v0, #1.00000000
	mov	x11, x8
LBB0_4:
	ldp	q1, q2, [x10, #-16]
	fadd.2d	v1, v1, v0
	fadd.2d	v2, v2, v0
	fmul.2d	v3, v1, v1
	fmul.2d	v4, v2, v2
	fmul.2d	v5, v3, v3
	fmul.2d	v6, v4, v4
	fmul.2d	v5, v5, v5
	fmul.2d	v6, v6, v6
	fmul.2d	v3, v3, v5
	fmul.2d	v4, v4, v6
	fmul.2d	v1, v1, v3
	fmul.2d	v2, v2, v4
	stp	q1, q2, [x9, #-16]
	add	x9, x9, #32
	add	x10, x10, #32
	subs	w11, w11, #4
	b.ne	LBB0_4
	cmp	w8, w0
	b.eq	LBB0_8
LBB0_6:
	mov	w8, w8
	fmov	d0, #1.00000000
LBB0_7:
	lsl	x9, x8, #3
	ldr	d1, [x2, x9]
	fadd	d1, d1, d0
	fmul	d2, d1, d1
	fmul	d3, d2, d2
	fmul	d3, d3, d3
	fmul	d2, d2, d3
	fmul	d1, d1, d2
	str	d1, [x1, x9]
	add	x8, x8, #1
	cmp	w8, w0
	b.lt	LBB0_7
LBB0_8:
	ret

.subsections_via_symbols
***********************
[2048.0,177147.0,4194304.0,4.8828125e7,3.62797056e8,1.977326743e9,8.589934592e9,3.1381059609e10,1.0e11,2.85311670611e11,7.43008370688e11,1.792160394037e12,4.049565169664e12,8.649755859375e12,1.7592186044416e13,3.4271896307633e13,6.4268410079232e13,1.16490258898219e14,2.048e14,3.50277500542221e14]
```

私のCPU（Apple M4）で使えるSIMDはNEONの128ビット幅なので、`Double` 2個を一度に処理できます。更に、ループ2回分を展開しているので、一回のループで `Double` 4個を処理していることになります。

x86系だとAVXとかAVX-512でもっと違う要素数になるかもしれません。

### noalias属性の活用

先ほど提示したC言語の疑似コードでは `restrict` を使っていましたが、先ほど生成したLLVM IRではそれに相当するものを使っていません。

LLVM的には関数の引数に `noalias` 属性（llvm-hs-pure的には `LLVM.AST.ParameterAttribute.NoAlias`）をつければ良いのですが、`LLVM.IRBuilder.Module.function` では引数に属性を指定できません。仕方がないので、関数を定義する部分を自分で書きます。

```haskell:src/LoopCodegen.hs
codegenNoAlias :: ExpS Double -> AST.Module
codegenNoAlias expr = IR.buildModule "dsl.ll" $ do
  absFn <- IR.extern "llvm.fabs.f64" [AST.double] AST.double
  -- 略
  cosFn <- IR.extern "llvm.cos.f64" [AST.double] AST.double

  let goValue :: IntMap.IntMap AST.Operand -> Value Double -> AST.Operand
      -- 略
      goSimpleExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> SimpleExp Double -> m AST.Operand
      -- 略
      goExp :: (IR.MonadIRBuilder m, IR.MonadModuleBuilder m) => IntMap.IntMap AST.Operand -> ExpS Double -> m AST.Operand
      -- 略

  let functionBody size resultArray inputArray = mdo
        prologue <- IR.block
        -- 略
        IR.retVoid

  ((sizeName, resultArrayName, inputArrayName), blocks) <- IR.runIRBuilderT IR.emptyIRBuilder $ do
    sizeName <- IR.fresh `IR.named` "size"
    resultArrayName <- IR.fresh `IR.named` "resultArray"
    inputArrayName <- IR.fresh `IR.named` "inputArray"
    functionBody (AST.LocalReference AST.i32 sizeName) (AST.LocalReference AST.ptr resultArrayName) (AST.LocalReference AST.ptr inputArrayName)
    pure (sizeName, resultArrayName, inputArrayName)

  let def = AST.GlobalDefinition AST.functionDefaults
            { AST.name = "f"
            , AST.parameters = ([AST.Parameter AST.i32 sizeName [], AST.Parameter AST.ptr resultArrayName [AST.NoAlias], AST.Parameter AST.ptr inputArrayName []], False)
            , AST.returnType = AST.void
            , AST.basicBlocks = blocks
            }
  IR.emitDefn def

  pure ()
```

これを使った実行結果は次のようになります：

```
*** Before optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

declare double @llvm.fabs.f64(double)

declare double @llvm.exp.f64(double)

declare double @llvm.log.f64(double)

declare double @llvm.sin.f64(double)

declare double @llvm.cos.f64(double)

define void @f(i32 %size_0, ptr noalias %resultArray_0, ptr %inputArray_0) {
  br label %loop_0

loop_0:                                           ; preds = %loopBody_0, %0
  %counter_0 = phi i32 [ 0, %0 ], [ %nextCounter_0, %loopBody_0 ]
  %lt_0 = icmp slt i32 %counter_0, %size_0
  br i1 %lt_0, label %loopBody_0, label %epilogue_0

loopBody_0:                                       ; preds = %loop_0
  %xPtr_0 = getelementptr [0 x double], ptr %inputArray_0, i32 0, i32 %counter_0
  %x_0 = load double, ptr %xPtr_0, align 8
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x4_0, %x2_0
  %x6_0 = fmul double %x5_0, %x1_0
  %resultPtr_0 = getelementptr [0 x double], ptr %resultArray_0, i32 0, i32 %counter_0
  store double %x6_0, ptr %resultPtr_0, align 8
  %nextCounter_0 = add i32 %counter_0, 1
  br label %loop_0

epilogue_0:                                       ; preds = %loop_0
  ret void
}
***************************
*** After optimization ***
; ModuleID = 'dsl.ll'
source_filename = "<string>"

; Function Attrs: argmemonly nofree norecurse nosync nounwind
define void @f(i32 %size_0, ptr noalias nocapture writeonly %resultArray_0, ptr nocapture readonly %inputArray_0) local_unnamed_addr #0 {
  %lt_01 = icmp sgt i32 %size_0, 0
  br i1 %lt_01, label %loopBody_0.preheader, label %epilogue_0

loopBody_0.preheader:                             ; preds = %0
  %min.iters.check = icmp ult i32 %size_0, 4
  br i1 %min.iters.check, label %loopBody_0.preheader4, label %vector.ph

vector.ph:                                        ; preds = %loopBody_0.preheader
  %n.vec = and i32 %size_0, -4
  br label %vector.body

vector.body:                                      ; preds = %vector.body, %vector.ph
  %index = phi i32 [ 0, %vector.ph ], [ %index.next, %vector.body ]
  %1 = zext i32 %index to i64
  %2 = getelementptr [0 x double], ptr %inputArray_0, i64 0, i64 %1
  %wide.load = load <2 x double>, ptr %2, align 8
  %3 = getelementptr double, ptr %2, i64 2
  %wide.load3 = load <2 x double>, ptr %3, align 8
  %4 = fadd <2 x double> %wide.load, <double 1.000000e+00, double 1.000000e+00>
  %5 = fadd <2 x double> %wide.load3, <double 1.000000e+00, double 1.000000e+00>
  %6 = fmul <2 x double> %4, %4
  %7 = fmul <2 x double> %5, %5
  %8 = fmul <2 x double> %6, %6
  %9 = fmul <2 x double> %7, %7
  %10 = fmul <2 x double> %8, %8
  %11 = fmul <2 x double> %9, %9
  %12 = fmul <2 x double> %6, %10
  %13 = fmul <2 x double> %7, %11
  %14 = fmul <2 x double> %4, %12
  %15 = fmul <2 x double> %5, %13
  %16 = getelementptr [0 x double], ptr %resultArray_0, i64 0, i64 %1
  store <2 x double> %14, ptr %16, align 8
  %17 = getelementptr double, ptr %16, i64 2
  store <2 x double> %15, ptr %17, align 8
  %index.next = add nuw i32 %index, 4
  %18 = icmp eq i32 %index.next, %n.vec
  br i1 %18, label %middle.block, label %vector.body, !llvm.loop !0

middle.block:                                     ; preds = %vector.body
  %cmp.n = icmp eq i32 %n.vec, %size_0
  br i1 %cmp.n, label %epilogue_0, label %loopBody_0.preheader4

loopBody_0.preheader4:                            ; preds = %loopBody_0.preheader, %middle.block
  %counter_02.ph = phi i32 [ 0, %loopBody_0.preheader ], [ %n.vec, %middle.block ]
  br label %loopBody_0

loopBody_0:                                       ; preds = %loopBody_0.preheader4, %loopBody_0
  %counter_02 = phi i32 [ %nextCounter_0, %loopBody_0 ], [ %counter_02.ph, %loopBody_0.preheader4 ]
  %19 = zext i32 %counter_02 to i64
  %xPtr_0 = getelementptr [0 x double], ptr %inputArray_0, i64 0, i64 %19
  %x_0 = load double, ptr %xPtr_0, align 8
  %x1_0 = fadd double %x_0, 1.000000e+00
  %x2_0 = fmul double %x1_0, %x1_0
  %x3_0 = fmul double %x2_0, %x2_0
  %x4_0 = fmul double %x3_0, %x3_0
  %x5_0 = fmul double %x2_0, %x4_0
  %x6_0 = fmul double %x1_0, %x5_0
  %resultPtr_0 = getelementptr [0 x double], ptr %resultArray_0, i64 0, i64 %19
  store double %x6_0, ptr %resultPtr_0, align 8
  %nextCounter_0 = add nuw nsw i32 %counter_02, 1
  %lt_0 = icmp slt i32 %nextCounter_0, %size_0
  br i1 %lt_0, label %loopBody_0, label %epilogue_0, !llvm.loop !2

epilogue_0:                                       ; preds = %loopBody_0, %middle.block, %0
  ret void
}

attributes #0 = { argmemonly nofree norecurse nosync nounwind }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.isvectorized", i32 1}
!2 = distinct !{!2, !3, !1}
!3 = !{!"llvm.loop.unroll.runtime.disable"}
**************************
*** Target assembly ***
	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0
	.globl	_f
	.p2align	2
_f:
	cmp	w0, #1
	b.lt	LBB0_8
	cmp	w0, #4
	b.hs	LBB0_3
	mov	w8, #0
	b	LBB0_6
LBB0_3:
	and	w8, w0, #0xfffffffc
	add	x9, x1, #16
	add	x10, x2, #16
	fmov.2d	v0, #1.00000000
	mov	x11, x8
LBB0_4:
	ldp	q1, q2, [x10, #-16]
	fadd.2d	v1, v1, v0
	fadd.2d	v2, v2, v0
	fmul.2d	v3, v1, v1
	fmul.2d	v4, v2, v2
	fmul.2d	v5, v3, v3
	fmul.2d	v6, v4, v4
	fmul.2d	v5, v5, v5
	fmul.2d	v6, v6, v6
	fmul.2d	v3, v3, v5
	fmul.2d	v4, v4, v6
	fmul.2d	v1, v1, v3
	fmul.2d	v2, v2, v4
	stp	q1, q2, [x9, #-16]
	add	x9, x9, #32
	add	x10, x10, #32
	subs	w11, w11, #4
	b.ne	LBB0_4
	cmp	w8, w0
	b.eq	LBB0_8
LBB0_6:
	mov	w8, w8
	fmov	d0, #1.00000000
LBB0_7:
	lsl	x9, x8, #3
	ldr	d1, [x2, x9]
	fadd	d1, d1, d0
	fmul	d2, d1, d1
	fmul	d3, d2, d2
	fmul	d3, d3, d3
	fmul	d2, d2, d3
	fmul	d1, d1, d2
	str	d1, [x1, x9]
	add	x8, x8, #1
	cmp	w8, w0
	b.lt	LBB0_7
LBB0_8:
	ret

.subsections_via_symbols
***********************
[2048.0,177147.0,4194304.0,4.8828125e7,3.62797056e8,1.977326743e9,8.589934592e9,3.1381059609e10,1.0e11,2.85311670611e11,7.43008370688e11,1.792160394037e12,4.049565169664e12,8.649755859375e12,1.7592186044416e13,3.4271896307633e13,6.4268410079232e13,1.16490258898219e14,2.048e14,3.50277500542221e14]
```

よく見ると引数に `noalias` がついて、関数のプロローグの分岐が一つ減ったのがわかると思います。

### ベンチマーク

純Haskellで書いた処理と、LLVMに生成させたコードで速度を比較してみましょう。

純Haskellの方は、`(x + 1)^(10 :: Int)` を普通に計算するのと、`^10` を展開したものを用意します。storable vectorの `map` で `Double -> Double` の関数を回すのと、配列ごとFFIで投げるものを用意します。

```haskell:benchmark/Main.hs
import           Criterion.Main

f :: Num a => a -> a
f x = (x + 1)^(10 :: Int)
{-# SPECIALIZE f :: Double -> Double #-}

g :: Num a => a -> a
g x = let y1 = x + 1
          y2 = y1 * y1
          y4 = y2 * y2
          y8 = y4 * y4
      in y8 * y2
{-# SPECIALIZE g :: Double -> Double #-}

main :: IO ()
main = do
  expr <- recoverSharing (f Var)
  let !input = VS.fromList [0..10000]
  _ <- withSimpleJIT expr $ \simpleF ->
    withArrayJIT expr $ \arrayF ->
      defaultMain
        [ bench "Haskell/vector" $ whnf (VS.map f) input
        , bench "Haskell unrolled/vector" $ whnf (VS.map g) input
        , bench "JIT/vector" $ whnf (VS.map simpleF) input
        , bench "JIT/array" $ whnf arrayF input
        ]
  pure ()
```

#### Apple M4 Proでの結果

まず、GHCのNCGバックエンドでの結果を載せます：

```
$ cabal-3.10.3.0 bench -w ghc-9.6.6 -O2 --builddir=dist-ncg
benchmarking Haskell/map
time                 26.91 μs   (26.88 μs .. 26.93 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.90 μs   (26.85 μs .. 26.93 μs)
std dev              138.9 ns   (111.8 ns .. 185.2 ns)

benchmarking Haskell unrolled/map
time                 7.239 μs   (7.228 μs .. 7.251 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.241 μs   (7.229 μs .. 7.254 μs)
std dev              41.28 ns   (33.48 ns .. 51.65 ns)

benchmarking JIT/map
time                 20.97 μs   (20.93 μs .. 21.00 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 20.96 μs   (20.93 μs .. 20.99 μs)
std dev              102.1 ns   (79.48 ns .. 131.4 ns)

benchmarking JIT/array
time                 1.632 μs   (1.631 μs .. 1.634 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.633 μs   (1.630 μs .. 1.635 μs)
std dev              7.347 ns   (5.794 ns .. 9.874 ns)
```

GHCのLLVMバックエンドでの結果も載せます：

```
$ cabal-3.10.3.0 bench -w ghc-9.6.6 -O2 --ghc-options=-fllvm --builddir=dist-llvm
benchmarking Haskell/map
time                 3.181 μs   (3.175 μs .. 3.187 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.180 μs   (3.173 μs .. 3.191 μs)
std dev              27.62 ns   (17.49 ns .. 48.06 ns)

benchmarking Haskell unrolled/map
time                 3.212 μs   (3.208 μs .. 3.217 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.214 μs   (3.211 μs .. 3.217 μs)
std dev              11.45 ns   (9.470 ns .. 15.02 ns)

benchmarking JIT/map
time                 7.238 μs   (7.223 μs .. 7.252 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 7.227 μs   (7.211 μs .. 7.250 μs)
std dev              61.64 ns   (40.19 ns .. 98.23 ns)

benchmarking JIT/array
time                 1.653 μs   (1.650 μs .. 1.658 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 1.649 μs   (1.647 μs .. 1.653 μs)
std dev              8.994 ns   (6.586 ns .. 12.49 ns)
```

JITしたコードを要素ごとに呼び出す方（JIT/map）は関数呼び出しかどこかでオーバーヘッドがかかるのか、純Haskellの `^10` を展開した方（Haskell unrolled/vector）に敵わないという結果になりました。

JITしたコードに配列ごと処理させる方（JIT/array）は、良い成績です。純HaskellでGHCのLLVMバックエンドを使って `^10` を展開した方（Haskell unrolled/map）の半分くらいの所要時間です。SIMDで2要素同時に処理しているので、こんなものでしょうか。自前でコード生成してLLVMを呼び出した甲斐がありましたね。

#### Ryzen 9 7940HSでの結果

AVX-512が使えるRyzen 9 7940HS（Zen 4）での結果も載せておきます。OSはWSL2上のUbuntu 22.04です。

`(x + 1)^10 * (x + 1)` に対して生成されるアセンブリコードは次のようになりました：

```
*** Target assembly ***
        .text
        .file   "<string>"
        .section        .rodata.cst8,"aM",@progbits,8
        .p2align        3
.LCPI0_0:
        .quad   0x3ff0000000000000
        .text
        .globl  f
        .p2align        4, 0x90
        .type   f,@function
f:
        pushq   %rbp
        pushq   %r14
        pushq   %rbx
.L0$pb:
        leaq    .L0$pb(%rip), %rax
        movabsq $_GLOBAL_OFFSET_TABLE_-.L0$pb, %r14
        addq    %rax, %r14
        testl   %edi, %edi
        jle     .LBB0_11
        movabsq $.LCPI0_0@GOTOFF, %r8
        xorl    %r11d, %r11d
        cmpl    $32, %edi
        jb      .LBB0_9
        movl    %edi, %r11d
        andl    $-32, %r11d
        xorl    %ecx, %ecx
        leal    -32(%r11), %eax
        movl    %eax, %r9d
        shrl    $5, %r9d
        leal    1(%r9), %r10d
        cmpl    $224, %eax
        jb      .LBB0_5
        vbroadcastsd    (%r14,%r8), %zmm0
        movl    %r10d, %ebp
        andl    $-8, %ebp
        leaq    1984(%rsi), %rax
        leaq    1984(%rdx), %rbx
        xorl    %ecx, %ecx
        .p2align        4, 0x90
.LBB0_4:
        vaddpd  -1984(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -1920(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -1856(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -1792(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -1984(%rax,%rcx,8)
        vmovupd %zmm2, -1920(%rax,%rcx,8)
        vmovupd %zmm3, -1856(%rax,%rcx,8)
        vmovupd %zmm4, -1792(%rax,%rcx,8)
        vaddpd  -1728(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -1664(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -1600(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -1536(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -1728(%rax,%rcx,8)
        vmovupd %zmm2, -1664(%rax,%rcx,8)
        vmovupd %zmm3, -1600(%rax,%rcx,8)
        vmovupd %zmm4, -1536(%rax,%rcx,8)
        vaddpd  -1472(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -1408(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -1344(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -1280(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -1472(%rax,%rcx,8)
        vmovupd %zmm2, -1408(%rax,%rcx,8)
        vmovupd %zmm3, -1344(%rax,%rcx,8)
        vmovupd %zmm4, -1280(%rax,%rcx,8)
        vaddpd  -1216(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -1152(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -1088(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -1024(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -1216(%rax,%rcx,8)
        vmovupd %zmm2, -1152(%rax,%rcx,8)
        vmovupd %zmm3, -1088(%rax,%rcx,8)
        vmovupd %zmm4, -1024(%rax,%rcx,8)
        vaddpd  -960(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -896(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -832(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -768(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -960(%rax,%rcx,8)
        vmovupd %zmm2, -896(%rax,%rcx,8)
        vmovupd %zmm3, -832(%rax,%rcx,8)
        vmovupd %zmm4, -768(%rax,%rcx,8)
        vaddpd  -704(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -640(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -576(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -512(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -704(%rax,%rcx,8)
        vmovupd %zmm2, -640(%rax,%rcx,8)
        vmovupd %zmm3, -576(%rax,%rcx,8)
        vmovupd %zmm4, -512(%rax,%rcx,8)
        vaddpd  -448(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -384(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -320(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  -256(%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -448(%rax,%rcx,8)
        vmovupd %zmm2, -384(%rax,%rcx,8)
        vmovupd %zmm3, -320(%rax,%rcx,8)
        vmovupd %zmm4, -256(%rax,%rcx,8)
        vaddpd  -192(%rbx,%rcx,8), %zmm0, %zmm1
        vaddpd  -128(%rbx,%rcx,8), %zmm0, %zmm2
        vaddpd  -64(%rbx,%rcx,8), %zmm0, %zmm3
        vaddpd  (%rbx,%rcx,8), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -192(%rax,%rcx,8)
        vmovupd %zmm2, -128(%rax,%rcx,8)
        vmovupd %zmm3, -64(%rax,%rcx,8)
        vmovupd %zmm4, (%rax,%rcx,8)
        addq    $256, %rcx
        addl    $-8, %ebp
        jne     .LBB0_4
.LBB0_5:
        testb   $7, %r10b
        je      .LBB0_8
        vbroadcastsd    (%r14,%r8), %zmm0
        incb    %r9b
        movl    %ecx, %ecx
        leaq    192(%rsi,%rcx,8), %rax
        leaq    192(%rdx,%rcx,8), %rcx
        xorl    %ebx, %ebx
        movzbl  %r9b, %ebp
        andl    $7, %ebp
        shlq    $8, %rbp
        .p2align        4, 0x90
.LBB0_7:
        vaddpd  -192(%rcx,%rbx), %zmm0, %zmm1
        vaddpd  -128(%rcx,%rbx), %zmm0, %zmm2
        vaddpd  -64(%rcx,%rbx), %zmm0, %zmm3
        vaddpd  (%rcx,%rbx), %zmm0, %zmm4
        vmulpd  %zmm1, %zmm1, %zmm5
        vmulpd  %zmm2, %zmm2, %zmm6
        vmulpd  %zmm3, %zmm3, %zmm7
        vmulpd  %zmm4, %zmm4, %zmm8
        vmulpd  %zmm5, %zmm5, %zmm9
        vmulpd  %zmm6, %zmm6, %zmm10
        vmulpd  %zmm7, %zmm7, %zmm11
        vmulpd  %zmm8, %zmm8, %zmm12
        vmulpd  %zmm9, %zmm9, %zmm9
        vmulpd  %zmm10, %zmm10, %zmm10
        vmulpd  %zmm11, %zmm11, %zmm11
        vmulpd  %zmm12, %zmm12, %zmm12
        vmulpd  %zmm9, %zmm5, %zmm5
        vmulpd  %zmm10, %zmm6, %zmm6
        vmulpd  %zmm11, %zmm7, %zmm7
        vmulpd  %zmm12, %zmm8, %zmm8
        vmulpd  %zmm5, %zmm1, %zmm1
        vmulpd  %zmm6, %zmm2, %zmm2
        vmulpd  %zmm7, %zmm3, %zmm3
        vmulpd  %zmm8, %zmm4, %zmm4
        vmovupd %zmm1, -192(%rax,%rbx)
        vmovupd %zmm2, -128(%rax,%rbx)
        vmovupd %zmm3, -64(%rax,%rbx)
        vmovupd %zmm4, (%rax,%rbx)
        addq    $256, %rbx
        cmpl    %ebx, %ebp
        jne     .LBB0_7
.LBB0_8:
        cmpl    %edi, %r11d
        je      .LBB0_11
.LBB0_9:
        vmovsd  (%r14,%r8), %xmm0
        movl    %r11d, %eax
        .p2align        4, 0x90
.LBB0_10:
        vaddsd  (%rdx,%rax,8), %xmm0, %xmm1
        vmulsd  %xmm1, %xmm1, %xmm2
        vmulsd  %xmm2, %xmm2, %xmm3
        vmulsd  %xmm3, %xmm3, %xmm3
        vmulsd  %xmm3, %xmm2, %xmm2
        vmulsd  %xmm2, %xmm1, %xmm1
        vmovsd  %xmm1, (%rsi,%rax,8)
        incq    %rax
        cmpl    %edi, %eax
        jl      .LBB0_10
.LBB0_11:
        popq    %rbx
        popq    %r14
        popq    %rbp
        vzeroupper
        retq
.Lfunc_end0:
        .size   f, .Lfunc_end0-f

        .section        ".note.GNU-stack","",@progbits
***********************
```

SIMDの利用だけではなく、ループの展開がすごいことになっていますね。ジャンプと分岐に恨みでもあるのか？

ベンチマークの結果は次の通りです：

```
$ cabal-3.10.3.0 bench -w ghc-9.6.6 -O2 --builddir=dist-ncg
benchmarking Haskell/map
time                 38.65 μs   (38.35 μs .. 38.92 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 38.38 μs   (37.96 μs .. 39.03 μs)
std dev              1.766 μs   (1.215 μs .. 3.018 μs)
variance introduced by outliers: 52% (severely inflated)

benchmarking Haskell unrolled/map
time                 13.79 μs   (13.70 μs .. 13.89 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 13.70 μs   (13.60 μs .. 13.84 μs)
std dev              390.5 ns   (300.6 ns .. 520.6 ns)
variance introduced by outliers: 32% (moderately inflated)

benchmarking JIT/map
time                 17.89 μs   (17.75 μs .. 18.06 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 17.92 μs   (17.81 μs .. 18.10 μs)
std dev              474.0 ns   (349.9 ns .. 644.7 ns)
variance introduced by outliers: 28% (moderately inflated)

benchmarking JIT/array
time                 1.485 μs   (1.467 μs .. 1.506 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.487 μs   (1.473 μs .. 1.511 μs)
std dev              56.94 ns   (41.96 ns .. 85.39 ns)
variance introduced by outliers: 52% (severely inflated)
```

```
$ cabal-3.10.3.0 bench -w ghc-9.6.6 -O2 --ghc-options=-fllvm --builddir=dist-llvm
benchmarking Haskell/map
time                 4.237 μs   (4.207 μs .. 4.272 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.227 μs   (4.195 μs .. 4.271 μs)
std dev              128.6 ns   (93.79 ns .. 171.3 ns)
variance introduced by outliers: 38% (moderately inflated)

benchmarking Haskell unrolled/map
time                 4.202 μs   (4.171 μs .. 4.238 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.249 μs   (4.213 μs .. 4.309 μs)
std dev              153.8 ns   (110.9 ns .. 229.4 ns)
variance introduced by outliers: 47% (moderately inflated)

benchmarking JIT/map
time                 17.79 μs   (17.64 μs .. 17.96 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 17.87 μs   (17.73 μs .. 18.20 μs)
std dev              671.3 ns   (342.2 ns .. 1.204 μs)
variance introduced by outliers: 44% (moderately inflated)

benchmarking JIT/array
time                 1.492 μs   (1.475 μs .. 1.514 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 1.492 μs   (1.478 μs .. 1.511 μs)
std dev              53.86 ns   (37.95 ns .. 72.97 ns)
variance introduced by outliers: 49% (moderately inflated)
```

GHCのLLVMバックエンドのHaskell unrolled/mapと、自動ベクトル化+JITコンパイルしたもの（JIT/array）を比較すると、4.202/1.492≈2.82なので自動ベクトル化によって2.8倍速になったことがわかります。AVX-512なので `Double` なら8並列を期待してしまいますが、内部処理が512ビット幅になるのはZen 5以降という話もありますし、実質4並列と考えて2.8倍……。こんなものなのでしょうか。

Apple M4 Proと比較すると、スカラーのコードはApple M4 Proに負けています（いずれもミニPCのもので、CPUはモバイル向けです）。しかし、自動ベクトル化でAVX-512を使った場合はApple M4 Proに勝っています。

## おわりに

ここで紹介したテクニックは[accelerate-llvm](https://github.com/AccelerateHS/accelerate-llvm)で使われていると思います（ちゃんと見てない）。AccelerateといえばGPUな印象ですが、CPUでも使えます。LLVMでCPU向けコード生成とGPU向けのコード生成の両方ができます。llvm-hsはAccelerateの人がメンテナンスに関わっているようです。

真面目にCPUを使い倒すには、自動ベクトル化によるSIMDの利用だけじゃなくて、マルチコアの活用も必要になってきます。が、その辺はやればできるんじゃないでしょうか。Haskellならrepaやmassivを見るといいかもしれません。

記事の執筆のためにllvm-hsを使ったわけですが、結構ハマりどころがありました。皆さんがAccelerateを打倒したくなったら私の屍を越えていってください。
