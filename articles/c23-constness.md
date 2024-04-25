---
title: "C23の改善点：文字列検索関数でのconst性の維持"
emoji: "🦔"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

## 文字列検索関数とconst性

C言語には文字列等の検索関数がいくつか定義されています。これらの関数がポインターを返す場合、元の配列が書き込み可能であれば返ってくるポインターも書き込み可能であって欲しいです。つまり、次のコードはコンパイルが通って欲しいです：

```c
#include <stdio.h>
#include <string.h>
int main(void) {
    char s[] = "Hello world!";
    char *p = strchr(s, 'o');
    *p = ' '; // 検索して得られたポインターに対して書き込む
    printf("%s\n", s); // => Hell  world!
}
```

一方で、検索関数は読み取り専用の配列に対しても使えて欲しいです。つまり、次のコードはコンパイルが通って欲しいです：

```c
#include <stdio.h>
#include <string.h>
int main(void) {
    const char *s = "Hello world!";
    const char *p = strchr(s, 'o');
    printf("%td\n", p - s); // => 4
}
```

さて、文字列検索関数 `strchr` の型はどうなっているべきでしょうか？仮に引数の型にも返り値の型にも `const` がつかない

```c
char *strchr(char *s, int c);
```

だとすると、最初の要求は満たせますが、2番目の要求は満たせません。一方、引数の型にも返り値の型にも `const` がつく

```c
const char *strchr(const char *s, int c);
```

だとすると、2番目の要求は満たせますが、最初の要求を満たせません。

C++の場合は、関数オーバーロードがあるので、`strchr` は

```cpp
char *strchr(char *s, int c);
const char *strchr(const char *s, int c);
```

と定義できます（参考：[std::strchr - cppreference.com](https://en.cppreference.com/w/cpp/string/byte/strchr)）。

一方、C17までのC言語では苦肉の策として

```c
char *strchr(const char *s, int c);
```

と定義されていました。つまり、引数には `const` がつきますが、返り値には `const` がつきません。これで一応は先ほど挙げた両方の要求を満たせます。

しかし、C17での定義では、次のコードが何の警告もなくコンパイルできてしまいます：

```c
#include <stdio.h>
#include <string.h>
int main(void) {
    const char *s = "Hello world!";
    char *p = strchr(s, 'o');
    *p = ' '; // 文字列リテラルを変更しようとしてしまう！
    printf("%s\n", s);
}
```

つまり、キャストもなくポインターの `const` を外せてしまうのです。これは望ましくない事態です。

この「`const` が暗黙に外れてしまう」問題は `strchr` 関数に限ったものではなく、以下の12個の関数に存在します：

* `bsearch`
* `bsearch_s`
* `memchr`
* `strchr`
* `strpbrk`
* `strrchr`
* `strstr`
* `wcschr`
* `wcspbrk`
* `wcsrchr`
* `wcsstr`
* `wmemchr`

## C23での解決

C23では、これらの関数が `const` 性を維持するようになりました。つまり、`const` な配列に対して呼び出せば `const` なポインターが返ってきて、非 `const` な配列に対して呼び出せば非 `const` なポインターが返ってきます。

C言語には関数オーバーロードの仕組みはないため、これらの関数は典型的には関数マクロとして提供されます（実装例は後述）。ただし、マクロ展開が何らかの方法で抑制された場合は、従来の（C17までの）型が露出します。この旧来の型はobsolescent feature（時代遅れになりつつある機能）扱いとなります。

ドキュメント上では、従来 `(const) char *` だった部分が `QChar *` になります。`(const) void *` は `QVoid *` に、`(const) wchar_t *` は `QWchar_t *` になります。

```c
#include <stdlib.h>
QVoid *bsearch(const void *key, QVoid *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
/* C++風に書けば
void *bsearch(const void *key, void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
const void *bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
となる。以下同様
 */

// Annex K
QVoid *bsearch_s(const void *key, QVoid *base, rsize_t nmemb, rsize_t size, int (*compar)(const void *k, const void *y, void *context), void *context);
```

```c
#include <string.h>
QVoid *memchr(QVoid *s, int c, size_t n);
QChar *strchr(QChar *s, int c);
QChar *strpbrk(QChar *s1, const char *s2);
QChar *strrchr(QChar *s, int c);
QChar *strstr(QChar *s1, const char *s2);
```

```c
#include <wchar.h>
QWchar_t *wcschr(QWchar_t *s, wchar_t c);
QWchar_t *wcspbrk(QWchar_t *s1, const wchar_t *s2);
QWchar_t *wcsrchr(QWchar_t *s, wchar_t c);
QWchar_t *wcsstr(QWchar_t *s1, const wchar_t *s2);
QWchar_t *wmemchr(QWchar_t *s, wchar_t c, size_t n);
```

ライブラリーで実装する場合は入力の型によって分岐を行う必要がありますが、C11の `_Generic` を使えば実装は難しくありません。例えば、`strchr` は次のように定義できるでしょう：

```c
char *strchr(const char *s, int c); // 従来の定義（マクロ展開が抑制された場合に露出する）

#define strchr(s, c) \
    _Generic((s), \
             char *: strchr((s), (c)), \
             const char *: (const char *)strchr((s), (c)))
```

## 参考文献

* [N3020: Qualifier-preserving standard library functions, v4](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3020.pdf)（2022年6月13日）

この記事を執筆している時点でC23はまだ出版されていませんが、C23が確定する前の最後の公開ドラフトN3096と、C23の次の標準に向けて作業が始まった最初のドラフトN3220はそれぞれ次で参照できます。

* [N3096](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3096.pdf)（2023年4月2日）
* [N3220](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf)（2024年2月22日）

## C23の他の新機能

C23についての他の話題は

* [次期C標準 (C23) の内容が固まったらしい](next-c-language)

を参照してください。
