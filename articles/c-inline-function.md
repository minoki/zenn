---
title: "C言語のinline関数について"
emoji: "🐷"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: false
---

C99以降のC言語には**インライン関数**という機能があります。機能自体は有名かと思いますが、重箱の隅をつつきに行くと意外と知られていないことがあるのではないかと思います。この記事では、インライン関数を深掘りします。あくまで**C言語を対象とし、C++は対象としません**。

この記事では特に、**`static` じゃない `inline` 関数**について解説します。

## 関数定義とコンパイル・リンクについての基本

まずは、複数のファイルからなるプログラムと関数の定義についておさらいします。

次のように、`foo1.c` と `main1.c` からなるプログラムを書いてみましょう：

```c:foo1.c
#include <stdio.h>

int add(int a, int b)
{
    return a + b;
}

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main1.c
#include <stdio.h>

extern int add(int a, int b);
extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

```
$ cc -o exe1 foo1.c main1.c
$ ./exe1
main: 0x102433ec0, 8
foo: 0x102433ec0, 8
```

関数のアドレスは環境によって、あるいは実行ごとに変わる可能性がありますが、他は普通ですね。

C言語の用語で言うと、`main1.c` と `foo1.c` は異なる**翻訳単位** (translation unit; C17 5.1.1.1) になります。`add` と `foo` は**外部結合**または**外部リンケージ** (external linkage; C17 6.2.2) を持つため、異なる翻訳単位であっても同じ実体を参照します。`add` のアドレスが同一であることがその証拠です。

今度は、`add` の定義を `static` 付きのものに変えてみましょう：

```c:foo2.c
#include <stdio.h>

static int add(int a, int b)
{
    return a + b;
}

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main2.c
#include <stdio.h>

extern int add(int a, int b);
extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

これをコンパイル・リンクすると次のようにエラーが出ます：

```
$ cc -o exe2 foo2.c main2.c
Undefined symbols for architecture arm64:
  "_add", referenced from:
      _main in main2-846628.o
      _main in main2-846628.o
ld: symbol(s) not found for architecture arm64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```

C言語の用語で言うと、`foo2.c` での `add` の定義は `static` がついたことにより**内部結合**あるいは**内部リンケージ** (internal linkage) を持つようになりました。一方で `main2.c` での `add` は外部リンケージのままです。外部リンケージを持つ `add` が使用されたにもかかわらず、その（外部リンケージを持つ `add` の）定義がプログラム中に存在しないので、エラーとなります（C17 6.9 段落5）。

今度は、`main3.c` で `add` を `static` 付きで定義してみましょう：

```c:foo3.c
#include <stdio.h>

static int add(int a, int b)
{
    return a + b;
}

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main3.c
#include <stdio.h>

static int add(int a, int b)
{
    return a + b;
}

extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

```
$ cc -o exe3 foo3.c main3.c
$ ./exe3
main: 0x104157f50, 8
foo: 0x104157ee0, 8
```

今度はコンパイルが通りましたが、`main` で表示した `add` のアドレスと `foo` で表示した `add` のアドレスが異なります。

C言語の用語で言うと、`foo3.c` の `add` も `main3.c` の `add` もそれぞれ内部リンケージを持つため、翻訳単位ごとに別の実体を持つことを許容されたということになります。ただ、この場合は関数の内容も同じなので、リンク時の最適化によって同じ実体にまとめられる可能性があるかもしれません（自信なし）。

最後に、2つのソースファイルで同じ関数を `static` なしで定義してみましょう。

```c:foo4.c
#include <stdio.h>

int add(int a, int b)
{
    return a + b;
}

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main4.c
#include <stdio.h>

int add(int a, int b)
{
    return a + b;
}

extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

```
$ cc -o exe4 foo4.c main4.c
duplicate symbol '_add' in:
    /private/var/folders/yg/36z4_5q142d6s6sn2y53gsd00000gn/T/main4-74bb37.o
    /private/var/folders/yg/36z4_5q142d6s6sn2y53gsd00000gn/T/foo4-3ec783.o
ld: 1 duplicate symbols
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```

リンクエラーが出ました。

C言語の用語で言うと、`add` は外部リンケージを持ちますが、**外部定義** (external definition) がプログラム中に複数存在するのでエラーになるということになります。外部リンケージを持つ一つの識別子に対する外部定義は高々1つしか存在してはいけません（その識別子が実際に使用される場合は、ちょうど1つ）。

一般論はCの規格を見てもらうとして、関数が外部リンケージ（JISの訳語では「外部結合」）を持つとは、ざっくりいうと `static` がついていない関数のことです。

また、関数定義が外部定義であるとは、インライン定義ではないもののことです。同じ識別子の外部定義が一つのプログラム中に複数存在するとエラーです（C17 6.9 段落5）。

## インライン関数とは

関数に `inline` 関数指定子をつけると、その関数はインライン関数になり、関数呼び出しが高速になる可能性があります。典型的にはインライン展開によって高速化が実現されますが、実際にインライン展開されるとは限らないので注意してください。詳しいことはCの規格では処理系定義 (implementation-defined) だったり未規定 (unspecified) だったりします。

もう少し詳しく説明します。

まず、内部リンケージを持つ関数（`static` がついた関数）はインライン指定できます。例えば

```c
static inline int add(int a, int b)
{
    return a + b;
}
```

みたいなやつですね。内部リンケージであることがわかっていれば良いので、

```c
static int add(int a, int b); // 内部リンケージとして宣言する

int main(void)
{
    printf("%d\n", add(3, 5));
}

inline int add(int a, int b)
{
    return a + b;
}
```

という書き方もできます。もちろん、実際にインライン化されるかは処理系次第です。

外部リンケージを持つ関数をインライン指定する場合は、`extern` の有無によって挙動が変わります。C17 6.7.4 段落7によると、外部リンケージを持つ関数について

* `inline` 関数指定子付きで宣言された関数は、同じ翻訳単位内に定義を持たなければならない。
* ある関数のファイルスコープの宣言が全て「`inline` 関数指定子付きで、`extern` なし」であれば、その翻訳単位内での定義は**インライン定義** (inline definition) となる。
    * インライン定義は外部定義を与えない。
    * ある翻訳単位にある識別子のインライン定義があっても、他の翻訳単位にその識別子の外部定義があることは阻害されない。
    * 関数呼び出しでは、インライン定義が外部定義の代わりに使用されることがある（実際どうなるかは未規定）。

とのことです。

例えば、次のコードはエラーになります：

```c:main5.c
#include <stdio.h>

inline int add(int a, int b)
{
    return a + b;
}

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
}
```

```
$ cc -oexe5 -std=c17 main5.c
Undefined symbols for architecture arm64:
  "_add", referenced from:
      _main in main5-9ea94f.o
      _main in main5-9ea94f.o
ld: symbol(s) not found for architecture arm64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
```

理由としては、`main5.c` での `add` は外部リンケージを持ち、`inline` 付きで `extern` なしなので、外部定義を与えない、ということになります。

一方で、次のコードはコンパイルが通ります：

```c:main6.c
#include <stdio.h>

extern inline int add(int a, int b)
{
    return a + b;
}

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
}
```

```
$ cc -oexe6 -std=c17 main6.c
$ ./exe6
main: 0x1047ebf18, 8
```

この場合、`add` はインライン定義には該当しないため、Cの規格ではインライン化が起こるかどうかについては何も言っていないことになります。しかし、外部リンケージを持つ識別子の外部定義はプログラム中に高々一つというルールがあるので、処理系がインライン化等の最適化を行うことに支障はないでしょう。

翻訳単位中に `extern` 付きの宣言が一つでもあれば、インライン定義には該当しなくなり、外部定義を与えるようになります：

```c:main7.c
#include <stdio.h>

inline int add(int a, int b)
{
    return a + b;
}

extern int add(int a, int b);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
}
```

```
$ cc -oexe7 -std=c17 main7.c
$ ./exe7
main: 0x100c47f18, 8
```

## ヘッダーとソースファイルを組み合わせる場合はどうすればいいのか

C言語でライブラリーを書くとき、ある関数のインライン定義を提供しつつ、インライン化できない場合（例えば、アドレスを取りたい場合）用の外部定義も用意しておくと良さそうです。そういう場合の書き方を考えてみます。

まず、ヘッダーでは次のように `static` も `extern` もない `inline` 関数として定義します：

```c:add.h
#if !defined(ADD_H)
#define ADD_H

inline int add(int a, int b)
{
    return a + b;
}

#endif
```

まあ翻訳単位ごとに別の実体が生成されても良く、なおかつ外部リンケージである必要がないのであれば `static` をつけても構いませんが、ここでは外部リンケージが欲しいとします（例えば、C言語以外の言語からFFIで呼ばれる関数を用意するには外部リンケージが必要です）。

インライン定義だけでは外部定義が生成されないので、どこかのソースファイル（翻訳単位）で `extern` 付きの `add` の宣言も用意します（定義ではダメです）：

```c:add.c
#include "add.h"

extern int add(int a, int b);
/* ここで
extern int add(int a, int b)
{
    return a + b;
}
と定義を書いてしまうと「外部定義が高々一つ」に反する。
*/
/*
ここで
inline int add(int a, int b);
と書いてしまうと外部定義が生成されない。
 */
/*
externなしの
int add(int a, int b);
でも良い。
 */
/*
何らかの事情でインライン定義とそうでない定義で別の定義を採用したければ、#include "add.h" せずに
int add(int a, int b)
{
    ...
}
と書く。
 */
```

利用する側は、普通に `#include "add.h"` します：

```c:foo8.c
#include <stdio.h>
#include "add.h"

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main8.c
#include <stdio.h>
#include "add.h"

/* ここで
extern int add(int a, int b);
と書いてしまうと外部定義が生成され、add.c と重複するのでダメ。
 */

extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

```
$ cc -oexe8 -std=c17 main8.c foo8.c add.c
$ ./exe8
main: 0x102917f58, 8
foo: 0x102917f58, 8
```

無事に外部定義が一個だけ生成され、うまくコンパイルできました。

この形がベストプラクティスだと思うのですが、あまり見かけたことがない気がします。みんな `static inline` を使っているのでは。`static inline` だとヘッダーで完結しますしね。

## `__attribute__((gnu_inline))` について

GCCはC89の時代から独自拡張としてインライン関数を実装していました。その仕様はC99のものとは微妙に違うので注意が必要です。GCCに `-std=gnu89` オプションまたは `-fgnu89-inline` オプションを渡すか、または関数に `gnu_inline` 属性をつけると古い仕様が有効になります。ここでは古い仕様を「GNU 89モード」と呼ぶことにします。

参考：「[Inline (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Inline.html)」

GNU 89モードでは、`static` も `extern` もつかないインライン関数は、外部定義を与えます。

GNU 89モードでは、`extern inline` がついた関数はインライン展開のみに使用され、外部定義を与えません。C標準の「インライン定義」に相当すると考えて良さそうです。

例えば、次のコードは、`foo` の方で外部定義が与えられ、`main` と `foo` の両方で `add` は同じ実体を指します：

```c:foo-gnu89.c
#include <stdio.h>

__attribute__((gnu_inline))
inline int add(int a, int b)
{
    return a + b;
}

void foo(void)
{
    printf("foo: %p, %d\n", add, add(3, 5));
}
```

```c:main-gnu89.c
#include <stdio.h>

extern int add(int a, int b);
extern void foo(void);

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
    foo();
}
```

```
$ gcc -oexe-gnu89 main-gnu89.c foo-gnu89.c
$ ./exe-gnu89
main: 0x5650472dd18c, 8
foo: 0x5650472dd18c, 8
```

一方、次の例は `add` の外部定義がないのでコンパイルエラーになります：

```c:main-gnu89-bad.c
#include <stdio.h>

__attribute__((gnu_inline))
extern inline int add(int a, int b)
{
    return a + b;
}

int main(void)
{
    printf("main: %p, %d\n", add, add(3, 5));
}
```

```
$ gcc main-gnu89-bad.c
/usr/bin/ld: /tmp/ccd9D6OW.o: in function `main':
main-gnu89-bad.c:(.text+0x13): undefined reference to `add'
/usr/bin/ld: main-gnu89-bad.c:(.text+0x1c): undefined reference to `add'
collect2: error: ld returned 1 exit status
```

現在どちらのモード（GNU 89 vs C99）でコンパイルされているかは、`__GNUC_STDC_INLINE__` マクロと `__GNUC_GNU_INLINE__` マクロで判別できるようです（参考：[Common Predefined Macros (The C Preprocessor)](https://gcc.gnu.org/onlinedocs/cpp/Common-Predefined-Macros.html)）。

```c
#include <stdio.h>
int main()
{
#if defined(__GNUC_STDC_INLINE__)
    puts("__GNUC_STDC_INLINE__");
#endif
#if defined(__GNUC_GNU_INLINE__)
    puts("__GNUC_GNU_INLINE__");
#endif
}
```

## always_inline, forceinline

C標準の `inline` はあくまでコンパイラーに対するヒントで、実際にインライン化が起こるとは限りません。コンパイラーによっては、もっと強制力の強い属性を提供していることがあります。

GCCや互換コンパイラー（Clangなど）は `always_inline` 属性により、関数呼び出しがインライン化されることを強制できます：[Common Function Attributes (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html#index-always_005finline-function-attribute)

MSVCは `__forceinline` というキーワードを提供しています：[Inline Functions (C++) | Microsoft Learn](https://learn.microsoft.com/en-us/cpp/cpp/inline-functions-cpp?view=msvc-170)

詳しくは、各コンパイラーのマニュアルを参照してください。

## 用語について

関連する用語の、C17における定義の場所をメモしておきます。セクション番号、段落番号はC17のものを指します。訳語はJIS X 3010を参考にしています。

* 翻訳単位 (translation unit) 5.1.1.1
* 結合 (linkage) 6.2.2
    * 外部結合 (external linkage)
    * 内部結合 (internal linkage)
    * 無結合 (no linkage)
* 宣言 (declaration) 6.7
* 定義 (definition) 6.7 段落5
* 記憶域クラス指定子 (storage-class specifier) 6.7.1
    * `typedef`, `extern`, `static`, `_Thread_local`, `auto`, `register`
* 関数指定子 (function specifier) 6.7.4
    * `inline`, `_Noreturn`
    * インライン定義 (inline definition) 6.7.4
* 外部宣言 (external declaration) 6.9
* 外部定義 (external definition) 6.9 段落5

## 参考

* GCC: [Inline (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc/Inline.html)
* Clang: [Language Compatibility -- C99 inline functions](https://clang.llvm.org/compatibility.html#inline)
* [Inline Functions In C](https://www.greenend.org.uk/rjk/tech/inline.html)
