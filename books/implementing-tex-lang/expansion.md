---
title: "展開"
---

展開器の役目は、入力を消費し、展開不能なトークン（展開不能な値を持つトークン）を生み出すことです。Haskellの関数として書けば次のようになるでしょう：

```haskell
-- M は展開器の状態を含むモナド
nextExpandedToken :: M (Maybe (Token, Unexpandable))
```

すなわち、展開器の状態を変更し、何らかの展開不能なトークンを生み出すことです。入力が枯渇したらその旨を通知します（`Maybe` 型）。

とはいえ、トークンは展開不能なものばかりではありません。展開可能なトークン（展開可能な値を持つトークン）に遭遇したら展開器は何をするべきでしょうか？もちろん、展開ですね。展開とは具体的には、必要に応じて入力を消費し、何らかのトークン列を生み出すことです。

展開を行う関数の型は次のように書けます：

```haskell
expand :: Expandable  -- 展開したい値
       -> Token       -- 展開しようとしているトークン
       -> M [EToken]
```

引数は展開したい値と、展開しようとしているトークンで、返り値は、展開後のトークン列です。一部の展開可能プリミティブは、展開後の結果に展開しようとしたトークン自身が現れることがあるので第2引数が必要となります。

展開後のトークンの型が通常と異なることに注意してください。これは次のように定義されるデータ型です：

```haskell
data EToken = EToken { depth :: Int, token :: Token, noexpand :: Bool }
            deriving Show
```

`depth` は本書独自のフィールドで、無限ループを抑止するために使います。通常のTeX処理系は無限ループ（無限展開）ができて、このようなフィールドはありません。

`token` フィールドは展開後のトークンです。

`noexpand` フィールドは `\noexpand` の展開結果として現れるトークンを区別する目的のフラグです。

```haskell
data LocalState = LocalState { scopeType     :: ScopeType
                             , controlSeqMap :: Map.Map TS.ShortText Value
                             , activeCharMap :: Map.Map Char Value -- use IntMap?
                             , inputEnv      :: I.Env -- catcodeMap and endlinechar
                             -- lccodeMap, uccodeMap, mathcodeMap, delcodeMap, sfcodeMap
                             , escapechar    :: Int
                             , countReg      :: IntMap.IntMap Int32
                             -- dimenReg, skipReg, muskipReg, toksReg, box registers
                             -- thinmuskip, medmuskip, thickmuskip
                             }

data ConditionalKind = CondTruthy
                     | CondFalsy
                     | CondCase
                     | CondTest

data State = State { inputState       :: I.State
                   , pendingTokens    :: [EToken]
                   , localStates      :: NE.NonEmpty LocalState
                   , conditionalStack :: [ConditionalKind]
                   }
```
