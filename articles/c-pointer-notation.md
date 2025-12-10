---
title: "アスタリスクはもう古い！？モダンC言語でのポインター型の記法"
emoji: "🌟"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

この記事は「[C Advent Calendar 2025](https://qiita.com/advent-calendar/2025/crzlang)」の11日目の記事です。

この記事は半分ジョークで半分本気です。

## C言語のポインター型の表記の問題

C言語でのポインター型の表記にはアスタリスク `*` が使われます。例えば、`int` へのポインターなら `int *` という具合です。しかし、C言語のポインター型の記法には2つの問題点があります：

問題点その1：複数の変数を宣言したときに非直感的な結果になる。

アスタリスクは型ではなく変数につくので、複数の変数を宣言した時に非直感的な結果になります。次のコードを実行してみましょう：

```c
#include <stdio.h>

#define reveal_type(x) \
  printf("%s: %s\n", #x, _Generic((x), int: "int", int *: "int *", int **: "int **"))

int main()
{
    int * a, b, c;
    reveal_type(a);
    reveal_type(b);
    reveal_type(c);
}
```

```
$ clang -std=c23 ptr1.c
$ ./a.out
a: int *
b: int
c: int
```

ポインター型になったのは `a` だけで、`b` と `c` は普通の `int` 型になりました。

問題点その2：関数ポインター型や配列へのポインター型が難解になる。

これはポインター型の問題ではありませんが、C言語の関数型も難解な記法なので、組み合わせである関数ポインター型も難解になります。

C言語の関数の型は

```
<返り値の型> <名前>(<引数>)
```

の形をしています。名前（変数名）の後に引数が来ていることに注意してください。例えば関数ポインター `int (*)(int, int)` を返す関数は

```c
int (*get_fn_ptr())(int, int);
```

となります。このくらいならまだ読めるかもしれませんが、C標準の `signal` 関数の型はどうでしょうか：

```c
void (*signal(int sig, void (*func)(int)))(int);
```

C言語に慣れていない方には難解と感じられるのではないでしょうか。

典型的な回避策は、`typedef` を使うことです。上記の `signal` 関数と（`typedef` が多いことを除いて）同等の定義は次のように書けます：

```c
typedef void (*sighandler_t)(int);
sighandler_t signal(int sig, sighandler_t func);
```

配列型も似たような問題を抱えています：

```c
int (*foo[5])[3];
```

## typeofは救世主となるか

C23の新機能の一つが、`typeof` 演算子です。`typeof` 演算子は式または型名を受け取り、型を返します。

典型例は、マクロでの利用でしょう。例えば変数の値を入れ替えるマクロは次のように書けます：

```c
#define swap(a, b) \
  do { \
    typeof(a) tmp = a; \
    a = b; \
    b = tmp; \
  } while(0)
```

`typeof` 演算子は、GCCやClangでは以前からGNU拡張として利用可能でした。

`typeof` の典型的な用途は、上記のように「式の型を取得する」ことかと思いますが、 **`typeof` 演算子には型名を与えることもできます。**

すると、`typedef` することなく「ポインター型」を表す型名を取得できます。次のコードを見てみましょう：

```c
#include <stdio.h>

#define reveal_type(x) \
  printf("%s: %s\n", #x, _Generic((x), int: "int", int *: "int *", int **: "int **"))

int main()
{
    typeof(int *) a, b, c;
    reveal_type(a);
    reveal_type(b);
    reveal_type(c);
}
```

プログラムをC23としてコンパイルしてみます。`-std=c23` オプションはGCC 14 / Clang 18以降で利用できます。以前のバージョンでも `-std=c2x` でC23の一部の機能を有効化できる場合があります。

```
$ clang -std=c23 ptr2.c
$ ./a.out
a: int *
b: int *
c: int *
```

全ての変数がポインター型になりました。素晴らしいですね。

`signal` 関数はどうなるでしょうか。

```c
typeof(void (int)) *signal(int sig, typeof(void (int)) *func);
```

よさそう……？いやどうかな……。人によっては「可読性が向上した」と言うかもしれませんし、あまり向上していないと言う人もいるかもしれません。

ともかく、**`typeof` 演算子はC言語の型の表記の問題を解決する可能性を秘めている**とは思いませんか？

## マクロにしよう

この記事で提案する、「ポインター型の新しい記法」を紹介しましょう。まず、次のようなマクロを定義します：

```c
#define Ptr(T) typeof(typeof(T) *)
```

簡単ですね。では使ってみましょう。まずは複数の変数から。

```c
#include <stdio.h>

#define Ptr(T) typeof(typeof(T) *)

#define reveal_type(x) \
  printf("%s: %s\n", #x, _Generic((x), int: "int", int *: "int *", int **: "int **"))

int main()
{
    Ptr(int) a, b, c;
    reveal_type(a);
    reveal_type(b);
    reveal_type(c);
}
```

```
$ clang -std=c23 -Wall ptr3.c 
$ ./a.out
a: int *
b: int *
c: int *
```

よさそうですね。`signal` 関数がどう書けるかも見てみましょう：

```c
Ptr(void (int)) signal(int, Ptr(void (int)) func);
```

`typeof` を直で使うよりは若干文字数が減って読みやすくなったのではないでしょうか。

## 真面目な話

`Ptr` マクロを実際のコードで使うか？というと、筆者の場合は否定的です。記事の冒頭に「半分ジョーク」と書いたのはこのことで、こういうC言語の基本的な構文を置き換えるためのオレオレマクロが正当化される場面は少ないと思います。

一方、「半分本気」と書いたのは、**C言語において複雑な型を書く必要が出てきた時に `typeof` を使って可読性を改善できる可能性がある**、というのは頭の片隅に入れておいて損のない知識だと思うからです。まあ、`typedef` を使った場合と比べてどうか、ということになるとちょっと旗色が悪いかもしれませんが。

## 関連

C23に `typeof` を導入した提案N2927（最終的なC23ではもう一つの演算子は `typeof_unqual` になったことに留意）と、`signal` などの関数を `typeof` を使って読みやすく（？）する提案N3450を紹介しておきます。

* [N2927: Not-so-magic - typeof for C](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2927.htm)
* [N3450: Function pointers are more readable with typeof()](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3450.txt)

C23の他の新機能については「[次期C標準 (C23) の内容が固まったらしい](https://zenn.dev/mod_poppo/articles/next-c-language)」を参照してください。

【追記】`typeof` からこの記事の機能（型名をいい感じにする）だけを取り出したものを `_Typeas` としよう、という提案がC2yに向けて出ているようです：[N3759: Add operator _Typeas](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3759.txt)
