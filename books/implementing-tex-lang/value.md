---
title: "値と環境"
---

## 値

`\let` 命令を使うと、コマンド名の指すもの（意味）を別のものに変更できます。例えば、`\let\a=b` とすると、`\a` の指すものが文字トークン `b` と等価になります。これはマクロ定義 `\def\a{b}` とは異なります。

また、`\ifx` 命令を使うと、トークンの意味が等しいかどうか判定できます。

`\let` で「コマンド名の意味」を変更でき、`\ifx` で「トークンの意味」を比較できるということは、「トークンの意味」を表すデータ型が定義できて然るべきだということです。この「トークンの意味」を本書では**値** (value) と呼ぶことにします。トークンの意味を「値」と呼ぶのも筆者が独自に使っている用語なので、The TeXbookやTeX by Topicを見ても見つからないと思います。

値は、展開可能なものと、展開不能なものに大別されます。プリミティブかマクロか（あるいはそれ以外か）という分類方法もあります。

展開可能な値は、展開可能なプリミティブか、マクロ、それにコマンド名が未定義だったことを表す特殊な値のいずれかです。

「コマンド名が未定義であること」を値として取り扱えることは、次のようにわかります。まず、未定義のコマンド名同士は `\ifx` で「等しい」と判定されます：

```
**\show\aaa 
> \aaa=undefined.
<*> \show\aaa
             
?    

*\show\bbb
> \bbb=undefined.
<*> \show\bbb
             
? 

*\ifx\aaa\bbb \message{Yes}\else\message{No}\fi
Yes
```

そして、意味が定まったコマンド名に `\let` で「未定義のコマンド名」を代入してやると、e-TeXの `\ifdefined` で「未定義」と判定されます：

```
$ etex
**\show\bar
entering extended mode
> \bar=macro:
->\mathaccent "7016 .
<*> \show\bar
             
? 

*\let\bar=\aaa \ifdefined\bar \message{Yes}\else\message{No}\fi
No
```

従って、「未定義」という状態は `\ifx` で比較でき、`\let` で代入できるので、値として取り扱うのが妥当だということになります。未定義のコマンド名の `\edef` で展開しようとするとエラーが発生するので、展開可能な値として取り扱うのが筋です。

```
*\edef\a{\aaa}
! Undefined control sequence.
<*> \edef\a{\aaa
                }
? 

```

展開不能な値としては、展開不能なプリミティブか、カテゴリーコードのついた文字、`\chardef` で代入される値、`\countdef` で代入される値、他の `\...def` で代入される値などがあります。

トークンの意味が展開可能かどうかは、`\edef` の結果で判別できます。「未定義」という値の場合はエラーが発生するので、展開可能な値だと考えます。一方で、`\endcsname` はエラーも起きず、書いた内容がそのまま定義されたマクロに含まれるので、展開不能な値だと考えます（TeX by Topicでは `\endcsname` を「expandable command」に分類していますが、それは誤りだと考えます）。

```
*\edef\a{\aaa}
! Undefined control sequence.
<*> \edef\a{\aaa
                }
? 

*\edef\a{\endcsname}

*\show\a
> \a=macro:
->\endcsname .
<*> \show\a
           
? 

```

TeXの値をHaskellのデータ型として定義すると、次のようになるでしょう：

```haskell
-- 展開可能な値
data Expandable = Eundefined
                | Ecsname
                | Enoexpand
                | Eexpandafter
                | Enumber
                | Eif
                | Eifcat
                | Eifx
                | Eifcase
                | Eiftrue
                | Eiffalse
                | Eifodd
                | Eelse
                | Efi
                | Eor
                | ...
                | Macro { long :: Bool
                        , outer :: Bool
                        , protected :: Bool
                        , delimiterBeforeFirstParam :: [Token] -- empty if undelimited
                        , paramSpec :: [ParamSpec]
                        , replacement :: [Replacement]
                        }
                deriving (Eq, Show)

-- 展開不能な値
data Unexpandable = Character Char CatCode
                  | DefinedCharacter Char -- defined by \chardef
                  | Urelax { noexpand :: Bool }
                  | Uendcsname
                  | Ulet
                  | Udef
                  | Umessage
                  | ...
                  deriving (Eq, Show)

-- 値
data Value = Expandable Expandable
           | Unexpandable Unexpandable
           deriving (Eq, Show)
```

`\ifx` で等価と判断されるものとHaskellの `==` で等価と判断されるものが一致するように `Value` 型を定義しています。

多くのプリミティブは内部的なフィールドを持ちませんが、`\relax` に関しては「`\noexpand` に由来するかどうか」のフィールドを持ちます。`\noexpand` に由来する値（`\let` で代入されるもの）が通常の `\relax` と異なることは、次の実行例で確認できます：

```
**\expandafter\let\expandafter\a\noexpand\foo

*\ifx\a\relax\message{Yes}\else\message{No}\fi
No
```

## 環境

TODO: 執筆する

<!-- トークンの値 -->
