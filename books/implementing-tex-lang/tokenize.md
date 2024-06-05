---
title: "字句解析"
---

まずは字句解析です。

## 文字

字句解析器は「文字」の列を「トークン」の列に変換します。そこで、「文字」とは何かを考えます。

TeX3からpdfTeXまでは、「文字」といえばASCIIを拡張した8ビットコードのことでした。一方、Unicodeにネイティブ対応したLuaTeXやXeTeXでは、字句解析器はUnicodeを扱い、「文字」と言ったらUnicodeコードポイント（0以上0x10FFFF以下の整数）のことです。

ここでは、「文字」はUnicodeスカラー値のこととします。つまり、0以上0x10FFFF以下の整数であって、0xD800以上0xDFFF以下の範囲にあるものを除いたものです。

Haskellの `Char` はUnicodeコードポイントを表しますが、本書ではもっぱらUnicodeスカラー値のために使います。

## カテゴリーコード

TeXの字句解析器は、文字に割り当たった**カテゴリーコード** (category code) に従って字句解析を行います。カテゴリーコードは0以上15以下の整数で表現されますが、ここでは列挙型で表しましょう：

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

## コマンド名

TeX文書で使う `\left` はこの5文字で一つのトークンを成します。このようにバックスラッシュで始まるトークンを**制御綴** (control sequence) と呼びます。制御綴には、バックスラッシュの後に1文字以上のアルファベットが続くもの（**制御語** (control word)）と、`\!` のように記号1文字が続くもの（**制御記号** (control symbol)）、空白が続くもの `\ `（**制御空白** (control space)）、それから1文字も続かないもの（`\csname\endcsname` で生成される）などがあります。

`\def` や `\let` などを使うと、制御綴に意味を代入することができます。

制御綴の他に、**アクティブ文字**にも意味を代入することができます。アクティブ文字はカテゴリーコード13が割り当たった文字です。

制御綴とアクティブ文字をひっくるめて、本書では**コマンド名** (command name) と呼ぶことにします。「コマンド名」は筆者が独自に使っている用語なので、The TeXbookやTeX by Topicを見ても見つかりません。

コマンド名をHaskellのデータ型として定義すると次のようになります：

```haskell
data CommandName = ControlSeq Text
                 | ActiveChar Char
                 deriving (Eq, Show)
```

## コマンド名とトークン

TeXのトークンは、大きく分けると（カテゴリーコードのついた）文字と、コマンド名に大別されます。文字のカテゴリーコードとして現れうるのは、1 (beginning of group), 2 (end of group), 3 (math shift), 4 (alignment tab), 6 (parameter), 7 (superscript), 8 (subscript), 10 (space), 11 (letter), 12 (other) です。アクティブ文字はコマンド名扱いとします。

条件分岐の展開では、この他に特殊なトークンとしてfrozen \relaxというものが現れます。`\ifodd1\fi` というトークン列を展開してみましょう。

```
**\message{\ifodd1\fi}
\relax 
```

`\relax` が現れました（詳しい仕組みは条件分岐のところで説明します）。しかし、この `\relax` は通常の `\relax` とは異なります。

```
*\edef\x{\ifodd1\fi} % \fi によって挿入される \relax

*\def\y{\relax} % 通常の \relax

*\ifx\x\y \message{Yes}\else\message{No}\fi
No
```

この特殊な `\relax` トークンのことをfrozen \relaxと呼びます。frozen \relaxの意味は常にプリミティブ `\relax` であり、`\let` などで意味を上書きすることはできません。従って、本書ではfrozen \relaxはコマンド名ではないことにします。

まとめると、TeXのトークンの定義は次のようになります：

```haskell
data Token = TCommandName CommandName
           | TCharacter Char CatCode
           | TFrozenRelax
           deriving (Eq, Show)
```

実際のTeXには、エラー回復の関係でfrozen \relax以外の「frozen」トークンがあるようです。

なお、TeX by Topicの2.4ではparameter tokensなるものも挙げられていますが、本書ではparameter tokenはマクロ定義で登場するものとし、字句解析の段階では考慮しません。

<!-- [TeXの字句解析器の動作について](https://zenn.dev/mod_poppo/articles/tex-input-processor) -->
