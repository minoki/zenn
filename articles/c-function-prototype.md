---
title: "C言語において引数リストが空の関数定義はプロトタイプを与えるか"
emoji: "📌"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

C17までのC言語の関数**宣言**においては、空の引数リスト `()` は「引数に関する規定がない」ことを表します。したがって、以下のコードはコンパイルが通ります：

```c
void foo();
int main(void) { foo(1, 2, 3); }
```

もちろん、実際に与えた引数と、`foo` の定義における引数が合致していなかったら未定義動作となります。

なので、引数を取らない関数の宣言では、`(void)` と書きましょう。

関数**定義**ではどうでしょうか。次のコードを考えます。

```c
#include <stdio.h>
void bar() { puts("Hello!"); }
int main(void) { bar(1, 2, 3); }
```

2行目の `bar` の定義を見ると、`bar` は引数を受け取らないことがわかります。この定義は `bar` のプロトタイプを与えるでしょうか？つまり、コンパイラーは3行目の `bar(1, 2, 3)` に対して警告またはエラーを出すべきでしょうか？

C17の6.7.6.3の段落14には次のように書かれています：

> An identifier list declares only the identifiers of the parameters of the function. An empty list in a function declarator that is part of a definition of that function specifies that the function has no parameters. The empty list in a function declarator that is not part of a definition of that function specifies that no information about the number or types of the parameters is supplied.

`bar` の定義は、“An empty list in a function declarator that is ... specifies that the function has no parameters.” に該当するので、 `bar(void)` と等価になりそうに私には思えました。

しかし、検索してみると上の `bar` の定義はプロトタイプを与えないという解釈が多そうです。

* [c - Difference between int main() and int main(void)? - Stack Overflow](https://stackoverflow.com/questions/12225171/difference-between-int-main-and-int-mainvoid)
* [language lawyer - Is int main() { } (without "void") valid and portable in ISO C? - Stack Overflow](https://stackoverflow.com/questions/29190986/is-int-main-without-void-valid-and-portable-in-iso-c/29190987#comment110690114_29190987)

こういうのもあります：

* [Defect report #317](https://www.open-std.org/jtc1/sc22/wg14/www/docs/dr_317.htm)

そして実用上大事なのは、実際のコンパイラーの挙動です。GCCは `-Wall` でも上記の `bar` のコードに警告を出さないのに対し、Clangは警告を出します。GCCは `bar` の定義はプロトタイプを与えないと解釈し、Clangは関数の引数について何らかの情報（メッセージからするとプロトタイプとも違いそう？）を与えると解釈したようです。

```
$ gcc-14 -Wall proto.c
$ clang-18 -Wall proto.c
proto.c:3:29: warning: too many arguments in call to 'bar'
    3 | int main(void) { bar(1, 2, 3); }
      |                  ~~~        ^
proto.c:3:21: warning: passing arguments to 'bar' without a prototype is deprecated in all versions of C and is not supported in C23 [-Wdeprecated-non-prototype]
    3 | int main(void) { bar(1, 2, 3); }
      |                     ^
2 warnings generated.
```

というわけで、引数を取らない関数定義においては `void` を明示した方が良さそうです。

ちなみに、GCCでは `-Wstrict-prototypes` オプションでプロトタイプなしの関数に警告を出せます：[Warning Options (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html#index-Wstrict-prototypes)

なお、C23ではプロトタイプなしの関数が廃止され、`()` と `(void)` が同じ意味になる予定です。

---

私が執筆に参加した「[Binary Hacks Rebooted](https://www.oreilly.co.jp/books/9784814400850/)」ではサンプルコードの多くをC言語で書いており、執筆時点ではC11/C17がメジャーであると考えられました。そのため、引数を取らない関数に対しては定義であっても `(void)` を明示することにしました。本にはなるべく模範的なコードを載せたいですからね。

ただ、「Binary Hacks Rebooted」では意図的にプロトタイプなしの関数を使っている箇所があります。興味があれば探してみてください。
