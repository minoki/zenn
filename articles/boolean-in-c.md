---
title: "C言語のbool型とその名前について 〜もう_Boolは嫌だ〜"
emoji: "💭"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

# 先史時代

昔のC言語には標準的なbool型はありませんでした。比較演算子は `int` を返しますし、 `isspace` みたいな述語関数も `int` を返します。

環境（プラットフォーム・ライブラリ）によっては独自のbool型を定義していることがありました。Windowsでの `BOOL` 型[^windows-bool]とか、Objective-Cの `BOOL` [^objc-bool]とか。

[^windows-bool]: Windowsの `BOOL` は `int` のtypedefです。

[^objc-bool]: Objective-Cの `BOOL` は古い環境（例：x86\_64 macOS）では `signed char`, 新しい環境（例：AArch64 macOS）では `_Bool` です。

このようなオレオレbool型の欠点は、0/1以外の値を取れることです。また、下手にインクリメントとかするとオーバーフローします。

# C99でのbool

C99では標準的なbool型が導入されました。しかし、互換性に配慮して `bool` ではなく `_Bool` という奇妙な名前で導入され、 `bool` という名前や `true`, `false` などの名前つき定数を使うには `<stdbool.h>` を `#include` する必要があります。

```c
// stdbool.h
#define __bool_true_false_are_defined 1
#define bool _Bool
#define true 1
#define false 0
```

ちなみに、 `bool`, `true`, `false` の各マクロはプログラム中で `#undef` や再 `#define` 可能です。

# C23ではどうなるか

`bool`, `true`, `false` を使うためにわざわざ `#include <stdbool.h>` するのは面倒です。幸い、C99から20年以上経過しており、アンダースコアなしの名前をキーワードにしても互換性の問題は少なくなっているでしょう。

ということで、C23では何も `#include` しなくてもアンダースコアなしの `bool`, `true`, `false` が使えるようになる見込みです。

処理系はアンダースコアなしの名前を定義済みマクロとして提供しても良いし、組み込みのキーワードとして提供しても良いことになっています。`bool` というトークンを文字列化マクロに通した結果はC標準では規定しないということです。

C17準拠の処理系が最小限の変更でC23に対応するなら、次のような定義済みマクロを追加することになるでしょう：

```c
#define bool _Bool
#define false ((bool)+0)
#define true ((bool)+1)
```

N2934では `_Bool` の他に `_Alignas`, `_Alignof`, `_Static_assert`, `_Thread_local` がこのような取り扱いをされることになっています。

# 参考文献

* [N2934 Jens Gustedt, Revise spelling of keywords v7](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2934.pdf)
* [N2935 Jens Gustedt, Make false and true first-class language features v8](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2935.pdf)
