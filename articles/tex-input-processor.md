---
title: "TeXの字句解析器の動作について"
emoji: "🐷"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["tex"]
published: true
---

TeX言語の字句解析の仕組みについて疑似コードで説明します。

# 定義

まず、字句解析器で使用するいくつかのデータ型を定義します。

## 文字

原初のTeXからpdfTeXまでは、字句解析器が扱う「文字」はASCIIコードを拡張した8ビット整数のことです。一方、Unicodeにネイティブ対応したLuaTeXやXeTeXでは、字句解析器はUnicodeを扱い、「文字」と言ったらUnicodeコードポイントのことです。

## カテゴリーコード

**カテゴリーコード** (category code) はTeXの字句解析器にとっての文字の役割を表す16通りの値です。疑似コードで定義すれば次のようになるでしょう：

```haskell
data CatCode = CCEscape -- 0, escape character
             | CCBeginGroup -- 1, beginning of group
             | CCEndGroup -- 2, end of group
             | CCMathShift -- 3, math shift
             | CCAlignmentTab -- 4, alignment tab
             | CCEndLine -- 5, end of line
             | CCParam -- 6, parameter
             | CCSup -- 7, superscript
             | CCSub -- 8, subscript
             | CCIgnored -- 9, ignored character
             | CCSpace -- 10, space
             | CCLetter -- 11, letter
             | CCOther -- 12, other character
             | CCActive -- 13, active character
             | CCComment -- 14, comment character
             | CCInvalid -- 15, invalid character
             deriving (Eq, Show, Enum, Bounded)
```

TeXの字句解析器の際には入力中の文字についていずれかのカテゴリーコードを割り当てるわけですが、割り当て方はフォーマットごと、あるいは文書の処理中に動的に変化する可能性があります。

ここでは、デフォルトのカテゴリーコードの割り当てを見ていきます。

まず、何もフォーマットをロードしない状態 (IniTeX) では次のようになります：

| 文字 | カテゴリーコード |
|-|-|
| `<return>` (U+000D; carriage return) | 5 (endline) |
| `<space>` (U+0020) | 10 (space) |
| `<null>` (U+0000) | 9 (ignored) |
| `<delete>` (U+007F; delete) | 15 (invalid) |
| `A`..`Z`, `a`..`z` | 11 (letter) |
| `%` | 14 (comment) |
| `\` | 0 (escape) |
| それ以外 | 12 (other) |

ここに書いたのは8ビットTeXの場合ですが、Unicodeをネイティブに扱えるTeX処理系の場合は他のUnicode文字にもカテゴリーコード11が割り当たっているかもしれません。

初期状態では `{` や `}` などのおなじみの特殊文字のカテゴリーコードは12で、特殊文字の機能を持ちません。

plain TeXではおなじみの特殊文字におなじみのカテゴリーコードが割り当てられます：

| 文字 | カテゴリーコード |
|-|-|
| `{` | 1 (begin group) |
| `}` | 2 (end group) |
| `$` | 3 (math shift) |
| `&` | 4 (alignment tab) |
| `#` | 6 (parameter) |
| `^`, `^^K` (U+000B; line tabulation) | 7 (superscript) |
| `_`, `^^A` (U+0001; start of heading) | 8 (subscript) |
| `^^I` (U+0009; character tabulation) | 10 (space) |
| `~`, `^^L` (U+000C; form feed) | 13 (active) |

LaTeXはだいたいplain TeXを踏襲していますが、制御文字のカテゴリーコードが異なるようです：

| 文字 | カテゴリーコード |
|-|-|
| `{` | 1 (begin group) |
| `}` | 2 (end group) |
| `$` | 3 (math shift) |
| `&` | 4 (alignment tab) |
| `#` | 6 (parameter) |.
| `^` | 7 (superscript) |.
| `_` | 8 (subscript) |
| `^^I` (U+0009; character tabulation) | 10 (space) |
| `~` | 13 (active) |
| `<null>` (U+0000) | 15 (invalid) |
| `^^J` (U+000C; form feed) | 12 (other) |
| `<null>`, `^^I`, `^^J`, `<return>`, `<delete>` を除く全ての制御文字 | 13 (active) |

## コマンド名

**制御綴** (control sequence) や**アクティブ文字** (active character) には、ユーザーが独自に意味を割り当てることができます。これらを表すデータ型があると便利です。ここではこれらをまとめて**コマンド名**と呼ぶことにします。

```haskell
data CommandName = ControlSeq Text
                 | ActiveChar Char
                 | FrozenRelax
                 deriving (Eq, Show)
```

`FrozenRelax` は `\ifodd1\fi` を展開すると出てくるやつです。字句解析の段階では気にしなくて結構です。エラー回復を実装するなら他にもfrozenトークンを用意する必要があります。

## トークン

字句解析の結果生成される**トークン**は、コマンド名または文字（アクティブ文字を除く）のいずれかです。いくつかのカテゴリーコードは字句解析の段階で特殊な役割を果たすため、文字トークンとしては現れません。

```haskell
data Token = TCommandName CommandName
           | TCharacter Char CatCode -- one of CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab, CCParam, CCSup, CCSub, CCSpace, CCLetter, CCOther
           deriving Show
```

## パラメーター

TeXではプログラム的にパラメーターを変更することで字句解析器に介入できます。字句解析器に影響するパラメーターは、 `\catcode` と `\endlinechar` の2つです。

`\catcode` は文字からカテゴリーコードへの写像です。`\endlinechar` は整数値で、典型的には何らかの文字コードを保持しますが、負の整数などの文字コードとしては無効な値を保持することもできます。

```haskell
data Env = Env { catcodeMap :: Map.Map Char CatCode
               , endlinechar :: Int
               }
```

`\endlinechar` の初期値は13で、これはキャリッジリターンの文字コードに相当します。

## 状態

字句解析器の状態は「この先待ち受けている行の列」と「行中の位置に関する状態」の2つです。

```haskell
data LineState = NewLine -- State 'N'
               | SkipSpaces -- State 'S'
               | MiddleOfLine -- State 'M'
               deriving (Eq, Show)

data State = State { lines :: [Text] -- 末尾の半角空白は除去済みと仮定する
                   , lineState :: LineState
                   }
```

# 前処理

TeXの字句解析器は入力を行単位で処理します。それぞれの行に対して行われる前処理は次の2ステップです：

1. 行の末尾の半角空白 (U+0020) を（全て）削除する。カテゴリーコードに関係なく、です。
2. `\endlinechar` が有効な文字コードを指していれば、それを文字に変換して末尾に付加する。
    * 典型的には `\endlinechar` の値は13なので、キャリッジリターンが付加されます。
    * `\endlinechar` が無効な値（`-1` など）を指している場合は、何も付加されません。

`\endlinechar` の付加はその行を最初に処理しようとした時に行われるので、行の途中で `\endlinechar` の値を変更してもその行末には反映されません。

# 文字の取得（`^^` 記法の処理）

TeXでは制御文字をソースコード上に書くために `^^` から始まる記法を用意しています。便宜的に `^^` と書きますが、正確には「カテゴリーコード7の文字（1文字目）と、1文字目と同じ文字コードの文字」の並びです。

`^^` の後に小文字の十六進数2桁が並べば、それは8ビットの文字コードとして解釈されます。それに該当しない場合は、 `^^` の後の文字の文字コードと64のxorを取った値の文字コードを持つ文字として解釈されます。

`^^` 記法の処理で得られた文字のカテゴリーコードが7であれば、再び `^^` 記法の処理が試されます。例えば、

```tex
\catcode`!=7 \message{^^a!8}
```

を実行すると、まず `^^a` の処理で `!` が生み出されます（`a` の文字コードは0x61、`!` の文字コードは0x21）その後に続く文字も `!` なので、`8` の文字コード（0x38）と64のxorを取って、文字コード0x78を持つ文字、すなわち `x` が生成されます。よって、`x` が標準出力に出てきます。

`^^` 記法の2文字目以降は `^^` 記法で生成することはできません。``\catcode`!=7 \message{^^a^^a8}`` は `x` ではなく `!X` を出力します。

この処理をコードで実装すると次のようになります：

```haskell
nextChar :: Env -- 現在の環境（カテゴリーコードの表が必要）
         -> Text -- 入力行
         -> Maybe (Char, CatCode, Text) -- 文字と、カテゴリーコードと、行の残りを返す
nextChar (Env { catcodeMap }) s = do
  (c0, rest0) <- T.uncons s -- 1文字取得してみる（行が空であればこの時点で Nothing を返す）
  let cc0 = getCatcode catcodeMap c0 -- カテゴリーコードを取得する
  -- ^^記法の処理を試みる：
  let doubleSuperscript = do guard (cc0 == CCSup) -- 1文字のカテゴリーコードは7でなくてはならない
                             tryDoubleSuperscript c0 rest0
  -- ^^記法の処理に成功したらそれで得られた文字を、失敗したら元の文字を返す
  doubleSuperscript <|> Just (c0, cc0, rest0)
  where
    tryDoubleSuperscript c0 rest0 = do
      (c1, rest1) <- T.uncons rest0 -- もう1文字取得してみる（行が終わってしまったら^^記法の処理は失敗とする）
      guard (c0 == c1) -- 最初の文字と今取得した文字の文字コードは同じである必要がある
      (c2, rest2) <- T.uncons rest1 -- もう1文字取得してみる（行が終わってしまったら^^記法の処理は失敗とする）
      -- 十六進小文字2桁の記法の処理を試す：
      let twoHexDigits = do guard (isLowerHexDigit c2)
                            (c3, rest3) <- T.uncons rest2
                            guard (isLowerHexDigit c3)
                            let c = chr (digitToInt c2 * 16 + digitToInt c3)
                            Just (c, getCatcode catcodeMap c, rest3)
      -- それが失敗した場合は、文字コードと64のxorを取る：
      let xor64 = do let c = chr (ord c2 `xor` 64)
                     Just (c, getCatcode catcodeMap c, rest2)
      result <- twoHexDigits <|> xor64
      -- 得られた文字のカテゴリーコードを調べ、それが7であれば再び^^記法の処理を試す
      case result of
        (c, CCSup, rest) -> tryDoubleSuperscript c rest <|> Just result
        _                -> Just result
```

`^^` で入力できるのは文字コード256未満の文字で、Unicode文字を表すには不足です。そこで、LuaTeXやXeTeXはさらに `^^^^` および `^^^^^^` 記法を提供しています。これらは十六進2桁の記法の拡張で、それぞれ後続の十六進小文字4桁または十六進小文字6桁を文字コードとして解釈します。

LuaTeXは `^^^^^^` が入力に現れた場合はそれ以降に続くのは十六進小文字6桁でなければなりません。また、 `^^^^` が入力に現れた場合はそれ以降に続くのは（`^^^^^^` 記法でなければ）十六進小文字4桁でなければなりません。`^^^^` が現れたらバックトラックはしないと言うことです。

`^^^^` 記法や `^^^^^^` 記法の1文字目は `^^`/`^^^^`/`^^^^^^` 記法で生成されたものでも構いません。また、2文字目以降は「1文字目と同じ文字コードの文字の並び」です。

# トークンの生成

いよいよ字句解析の本番、トークン生成です。

字句解析の状態を再掲しておきます：

```haskell
data LineState = NewLine -- State 'N'
               | SkipSpaces -- State 'S'
               | MiddleOfLine -- State 'M'
               deriving (Eq, Show)

data State = State { lines :: [Text] -- 末尾の半角空白は除去済みと仮定する
                   , lineState :: LineState
                   }
```

`LineState` の状態の意味は以下です：

状態*N* (`NewLine`) は、現在の位置が行頭、あるいは行頭からここまで遭遇した文字が全て空白（カテゴリーコード10の文字）だったことを表します。

状態*S* (`SkipSpaces`) は、直前の文字が空白（カテゴリーコード10の文字）だった、もしくは直前のトークンが制御語だったことを表します。

状態*M* (`MiddleOfLine`) はそれ以外です。

なお、カテゴリーコード9 (ignored) の文字に遭遇しても `LineState` は変わりません。

字句解析の処理は次のようになります：

```haskell
parToken, spaceToken :: Token
parToken = TCommandName (ControlSeq "par")
spaceToken = TCharacter ' ' CCSpace

nextToken :: Env -> State -> Maybe (Token, State)
nextToken _env (State { lines = [] }) = Nothing -- 入力の末尾に到達して、入力行がない場合：生成するトークンはない
nextToken env@(Env { endlinechar }) (State { lines = currentLine : rest, lineState })
  = let -- 次の行に進む場合の状態（前処理を行う）
        nextStateWithNewLine = State { lines = case rest of
                                                 l : ls -> appendEndlinechar l endlinechar : ls
                                                 [] -> []
                                     , lineState = NewLine
                                     }
    in case nextChar env currentLine of
         Nothing -> nextToken env nextStateWithNewLine -- 行の終端に到達した：次の行へ
         Just (c, cc, restOfLine) ->
           case cc of
             CCEscape ->
               case nextChar env restOfLine of
                 -- 行の末尾に到達した場合、空の制御綴を生成する。
                 -- 生成された命令によって \endlinechar が変更される可能性があるので、この時点では次の行の準備は行わない。
                 Nothing -> Just (TCommandName (ControlSeq ""), State { lines = T.empty : rest, lineState = SkipSpaces })

                 -- 制御語 (control word)
                 Just (c1, CCLetter, restOfLine1) ->
                   let go l acc = case nextChar env l of
                                    Nothing ->
                                      let nextState = State { lines = T.empty : rest, lineState = SkipSpaces }
                                      in Just (TCommandName (ControlSeq (T.pack (reverse acc))), nextState)
                                    Just (c', CCLetter, restOfLine') -> go restOfLine' (c' : acc)
                                    Just (c', _, restOfLine') ->
                                      let -- 読み取った文字はストリームに戻す。
                                          -- このことは、制御語の直後の^^記法は制御語を実行する前のカテゴリーコードで処理されることを意味する。
                                          nextState = State { lines = T.cons c' restOfLine' : rest, lineState = SkipSpaces }
                                      in Just (TCommandName (ControlSeq (T.pack (reverse acc))), nextState)
                   in go restOfLine1 [c1]

                 -- control symbol / control space
                 Just (c1, cc1, restOfLine1) ->
                   let nextState = State { lines = restOfLine1 : rest, lineState = if cc1 == CCSpace then SkipSpaces else MiddleOfLine }
                   in Just (TCommandName (ControlSeq (T.singleton c1)), nextState)

             -- 行末文字に遭遇した場合
             CCEndLine -> case lineState of
               NewLine -> Just (parToken, State { lines = T.empty : rest, lineState = SkipSpaces }) -- その行が空または空白しかなかった場合は \par を挿入する
               MiddleOfLine -> Just (spaceToken, State { lines = T.empty : rest, lineState = SkipSpaces }) -- その行に内容があって、直前の文字が空白ではなかった場合は半角空白を挿入する（常に文字コード U+0020 の空白が挿入される）
               SkipSpaces -> nextToken env nextStateWithNewLine -- 直前の文字が空白だった場合は、トークンを生成せずに次の行へ移る

             CCIgnored -> nextToken env (State { lines = restOfLine : rest, lineState = lineState })
             CCSpace -> case lineState of
               MiddleOfLine -> Just (spaceToken, State { lines = restOfLine : rest, lineState = SkipSpaces })
               _ -> nextToken env (State { lines = restOfLine : rest, lineState = lineState })
             CCActive -> Just (TCommandName (ActiveChar c), State { lines = restOfLine : rest, lineState = MiddleOfLine })
             CCComment -> nextToken env nextStateWithNewLine
             CCInvalid -> error "invalid character"

             -- CCBeginGroup, CCEndGroup, CCMathShift, CCAlignmentTab, CCParam, CCSup, CCSub, CCLetter, CCOther
             _ -> Just (TCharacter c cc, State { lines = restOfLine : rest, lineState = MiddleOfLine })
```

TeXは命令の実行によって字句解析器のパラメーター（`\catcode` と `\endlinechar`）が変化する可能性があるので、字句解析を進めるタイミングは重要です。注意点を何点か挙げておきます：

1. 制御語の直後の空白が読み飛ばされるのは、その制御語が（実行によって）消費された後、次のトークンが必要になったタイミングです。
2. 空の制御語や行末文字など、「トークンを生成してその行を終える」パターンがありますが、その次の行に付与される `\endlinechar` の値は、生成されたトークンが（実行によって）消費された後、次のトークンが必要になったタイミングです。
3. 制御語の直後に `^^` 記法が来る場合、superscriptの判定に使われるカテゴリーコードは制御語が字句解析されるタイミングのものであって、制御語が実行された後のものではありません。

それぞれサンプルコードを挙げておきます。

```tex
{\catcode`\ =\active
\gdef {\message{SPACE}}
}
\def\foo{\catcode`\ =\active}
{\foo }% → \fooによって空白がアクティブ化される。\fooの直後の空白は\fooの実行後に字句解析されるので、\message{SPACE}が実行される。
```

```tex
{\def~{\message{HELLO}}\endlinechar=126

}% → \endlinechar=126の直後の<return>（これはこの行を読み始めた時に付加されたもの）が空白文字を生み出すことによって\endlinecharの実行が完了する。
% その次の空行に付加される\endlinecharは1行目が実行された後のものなので、空行には文字コード126すなわち~が付加され、\message{HELLO}が実行される。

{\def\par{\endlinechar=126 \message{GOOD}}\def~{\message{BYE}}


}% →最初の空行からは\parが生成され、\message{GOOD}の実行と共に\endlinecharが126へ変更される。
% 次の空行は\parの実行が終わった後に\endlinecharが付加されるので、~が付加されて\message{BYE}が実行される。

{\expandafter\def\csname\endcsname{\endlinechar=126 }\def~{\message{HELLO}}\endlinechar=-1 %
\

}% → \ のみからなる行の処理開始時には\endlinechar=-1となっているので、その行からは空の制御綴が生成される。
% 空の制御綴の実行によって\endlinechar=126となるので、その次の行には~が付与される。
% ~が展開され、\message{HELLO}が実行される。
```

```tex
\def\foo{\catcode`!=7 \catcode`^=12 }% → !のカテゴリーコードをsuperscript, ^のカテゴリーコードをotherに変更する命令
{$\foo^^a!8$}% → \fooの直後の^^記法は\fooの字句解析のタイミングで処理され、!が生成される。\fooの字句解析のタイミングでは!のカテゴリーコードはotherなので制御語\fooが確定する。\fooの実行後の字句解析で!は改めてカテゴリーコード7が付与される。
{$\foo ^^a!8$}% → ^^は\fooの実行後に解釈されるが、その時点では^のカテゴリーコードはotherに変更されているので何も起こらない（文字^が印字される）。一方、!のカテゴリーコードはsuperscriptになっているので、aの右肩に8が乗る形になる。
```

なお、制御綴の先頭のescape文字を `^^` で表記したり、制御語の途中に `^^` で表現されたletterを含めることも可能です。

```tex
^^5cmes^^73age{HELLO}
% →^^5cはバックスラッシュに変換され、制御綴の解釈が始まる。^^73は小文字のsとして解釈される。
% 従って\message{HELLO}が実行される。
```

ちなみに、TeX Live 2022以降のpdfTeXや互換エンジン（e-(u)pTeX, XeTeX, LuaTeXを含む）では `\partokenname` によって空行に挿入されるトークン（通常は `\par`）をカスタマイズできるらしいです。

# 日本語TeX

TeXの字句解析の原則は上記の通りです。その原則に従うと、次のコード

```tex
本日は
晴天なり
```

は

1. 文字の列 `本日は`
2. 半角空白（行末文字から生成される）
3. 文字の列 `晴天なり`

と字句解析されます。和文文字の直後で改行すると半角空白が挿入されるわけです。

ですが、この挙動は皆さんお馴染みの日本語TeXの挙動とは異なりますよね？日本語TeXでは日本語文の途中で改行しても半角空白が挿入されるようなことはないと思います。

実は、pTeXは字句解析器を拡張することによって和文文字直後の改行による半角空白の生成を抑制しています。詳しくはpTeXのマニュアルを参照してください。

LuaTeX-jaでも、行の前処理に介入することで似たような動作を実現しています。詳しくはLuaTeX-jaのマニュアルを参照してください。

逆に、pTeX/upTeXおよびLuaTeX-ja以外のTeXエンジン・パッケージ（XeTeXや、Web向けのTeX互換処理系）では和文文字直後の改行で半角空白が挿入されてしまいます。その場合は手動で行末にコメント記号 `%` を配置しましょう。

# クイズ

問1． plain TeX/LaTeX（どちらでも良いです）のデフォルトのカテゴリーコード・`\endlinechar` の状態で

```tex
\message{^^
}
```

を実行したら `\message` 命令は何を出力するでしょうか？

問2．「空の制御綴」は `\csname\endcsname` の展開で得ることが多いかと思いますが、字句解析器に空の制御綴を生成させるにはどうしたらいいでしょうか？（ヒント：この記事を隅々まで読んでください）

# 字句解析の先へ

字句解析器は展開器によって駆動され、展開器は実行器によって駆動されるものです。展開とか実行とかの話もそのうち疑似コードと共に書けたらなあと思っています。

一応記事としては以前

* [TeX言語のトークンと値、字句解析から展開と実行まで](https://blog.miz-ar.info/2018/06/tex-token-and-value/)

というものを書きましたが、まだちょっと怪しいところがあります。

# 付録：Haskellコードとして動かすための追加コード

この記事に書いた疑似コードは、以下のコードを先頭に追加することで実際のHaskellコードとして動かせます。出来上がるプログラムでは、標準入力で与えられた文字列をひたすら字句解析してトークン列として出力します（実行はしません）。

このサンプルコードでの「文字」はUnicodeスカラー値としています。

```haskell
{- cabal:
build-depends: base, text, containers
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Bits (xor)
import           Data.Char (chr, digitToInt, isAlpha, ord)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T (getContents)
import           Data.Text (Text)
import           Prelude hiding (lines)

isUnicodeScalarValue :: Int -> Bool
isUnicodeScalarValue x = 0 <= x && x <= 0x10FFFF && not (0xD800 <= x && x <= 0xDFFF)

defaultCatcodeMap :: Map.Map Char CatCode
defaultCatcodeMap = Map.fromList [('\\', CCEscape) -- IniTeX
                                 ,('{', CCBeginGroup)
                                 ,('}', CCEndGroup)
                                 ,('$', CCMathShift)
                                 ,('&', CCAlignmentTab)
                                 ,('\r', CCEndLine) -- IniTeX
                                 ,('#', CCParam)
                                 ,('^', CCSup)
                                 ,('_', CCSub)
                                 ,('\NUL', CCIgnored) -- IniTeX
                                 ,(' ', CCSpace) -- IniTeX
                                 ,('~', CCActive)
                                 ,('%', CCComment) -- IniTeX
                                 ,('\DEL', CCInvalid) -- IniTeX
                                 ]

initialEnv :: Env
initialEnv = Env { catcodeMap = defaultCatcodeMap
                 , endlinechar = 13 -- '\r'
                 }

appendEndlinechar :: Text -> Int -> Text
appendEndlinechar t i | isUnicodeScalarValue i = T.snoc t (chr i)
                      | otherwise = t

getCatcode :: Map.Map Char CatCode -> Char -> CatCode
getCatcode m c = case Map.lookup c m of
                   Just cc -> cc
                   Nothing -> if isAlpha c then
                                CCLetter
                              else
                                CCOther

isLowerHexDigit :: Char -> Bool
isLowerHexDigit c = ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')

newState :: Env -> [Text] -> State
newState (Env { endlinechar }) lines
  = State { lines = case lines of
                      l : ls -> appendEndlinechar l endlinechar : ls
                      [] -> []
          , lineState = NewLine
          }

main :: IO ()
main = do
  c <- T.getContents
  let env = initialEnv
      initialState = newState env $ map (T.dropWhileEnd (== ' ')) $ T.lines c
      go state = case nextToken env state of
                   Nothing -> pure ()
                   Just (t, state') -> print t >> go state'
  go initialState
```
