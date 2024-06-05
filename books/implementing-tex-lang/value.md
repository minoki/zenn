---
title: "値と環境"
---

`\let` 命令を使うと、コマンド名の指すものを別のものに変更できます。例えば、`\let\a=b` とすると、`\a` の指すものが文字トークン `b` と等価になります。これはマクロ定義 `\def\a{b}` とは異なります。

`\let` で「トークンの指すもの」を変更できるということは、「トークンの指すもの」を表すデータ型が定義できて然るべきだということです。この「トークンの指すもの」を本書では**値** (value) と呼ぶことにします。

値は、展開可能なものと、展開不能なものに大別されます。プリミティブかマクロかという分類方法もあります。

展開可能な値は、展開可能なプリミティブか、マクロ、それにコマンド名が未定義だったことを表す特殊な値のいずれかです。

展開不能な値としては、展開不能なプリミティブか、カテゴリーコードのついた文字、`\chardef` で代入される値、`\countdef` で代入される値、他の `\...def` で代入される値などがあります。

Haskellのデータ型として定義すると、次のようになるでしょう：

```haskell
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

data Unexpandable = Character Char CatCode
                  | DefinedCharacter Char -- defined by \chardef
                  | Urelax { noexpand :: Bool }
                  | Uendcsname
                  | Ulet
                  | Udef
                  | Umessage
                  | ...
                  deriving (Eq, Show)

data Value = Expandable Expandable
           | Unexpandable Unexpandable
           deriving (Eq, Show)
```

値は `\ifx` で比較することができます。`\ifx` で等価と判断されるものとHaskellの `==` で等価と判断されるものが一致するようにデータ型を定義しています。

多くのプリミティブは内部的なフィールドを持ちませんが、`\relax` に関しては「`\noexpand` に由来するかどうか」のフィールドを持ちます。`\noexpand` に由来する値（`\let` で代入されるもの）が通常の `\relax` と異なることは、次の実行例で確認できます：

```
**\expandafter\let\expandafter\a\noexpand\foo

*\ifx\a\relax\message{Yes}\else\message{No}\fi
No
```

<!-- 環境 -->

<!-- トークンの値 -->
