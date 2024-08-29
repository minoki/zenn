---
title: "C言語のざんねんなしよう事典"
emoji: "🗂"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: false
---

モダンなプログラミング言語が多数登場した現代においても、C言語は重要な言語です。具体的に言うと、多くのプログラミング言語（あるいは言語ランタイム）がC言語で実装されていたり、OSのAPIはC言語のインターフェース（ABI）で提供されている場合が多かったり、異なるプログラミング言語間で関数呼び出しをしようとするとC言語のABIがベースとなったりします。

そんなC言語ですが、登場時から現代まで、少しずつですが進化を遂げています。その過程では、「この機能は良くなかった」ということが判明し、非推奨になったり削除された機能もあります。この記事では、C言語のそのような「良くなかった」機能を紹介します。

## `gets` 関数

C言語には、誤って使うとバッファオーバーフローなどの問題を引き起こす関数が多数用意されています。例えば、

```c
char buf[16];
scanf("%s", buf);
```

というコードは容易にバッファオーバーフローを引き起こします（先日発売された「Binary Hacks Rebooted」ではこのような `scanf` 呼び出しを「脆弱性を含むコードの例」として使いました）。

しかし、プログラマーが十分な注意を払えば、そのような関数であっても安全に使うことができる場合があります。例えば、`scanf` であれば

```c
char buf[16];
scanf("%15s", buf);
```

と書けば安全に使うことができます。つまり、「使い方を間違えれば危険だが、使い方次第では安全である」ということです。

ところが、かつてのC言語には「安全な使い方がそもそも存在しない」関数がありました。知っている方も多いと思いますが、`gets` 関数です。`gets` 関数は次のような型を持ちました：

```c
#include <stdio.h>
char *gets(char *s);
```

ここで、受け取るバッファーの長さを指定する引数がないことに注意してください。読み込み元は `stdin` で、これは一般にはプログラマーが制御できるものではないので、`gets` を使った時点でそのプログラムはバッファオーバーフローを起こすことが約束されてしまいます。

「自分の足を撃ち抜くことができる」C言語であってもこれは看過できないと思われたのか、C11では `gets` 関数は廃止されてしまいました。「非推奨」じゃなくて「廃止」です。

代替としては、`fgets` 関数があります。`gets` 関数は改行コードをバッファーに書き込まないのに対し、`fgets` 関数は改行コードも書き込むという違いはあります。

別の代替としては、Annex Kの `gets_s` 関数があります。`gets_s` 関数は次のような型を持ちます：

```c
#define __STDC_WANT_LIB_EXT1__ 1
#include <stdio.h>
char *gets_s(char *s, rsize_t n);
```

これは第2引数にバッファーの大きさを指定できます。また、`gets` 関数と同様に、改行コードを書き込みません。

まあAnnex Kの関数も割と「ざんねんな」機能になってないか、という懸念はあります。

## `_Noreturn` と `<stdnoreturn.h>`

C言語のいくつかの関数は、呼び出し元に制御を返しません。`exit` や `longjmp` がその例です。ユーザーが定義した関数も、必ず `exit` 等を呼び出すのであれば「制御を返さない関数」になりえます。例を載せます：

```c
void my_error(const char *message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}
```

この `my_error` を呼び出す別の関数を考えましょう。

```c
double safe_sqrt(double x)
{
    if (x < 0.0) {
        my_error("safe_sqrt: negative input");
    } else {
        return sqrt(x);
    }
}
```

`my_error` 関数の呼び出しは制御を返さないため、`safe_sqrt` の `x < 0.0` のパスには `return` 文がなくても問題ないはずです。しかしコンパイラーはそのことを知らないため、「値を返すべき関数が値を返さない可能性がある」旨の警告を発します。

この問題を解決するためにC11に導入されたのが、関数指定子 (function specifier) `_Noreturn` です。

先ほどの `my_error` の定義を

```c
_Noreturn void my_error(const char *message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}
```

に変えると、コンパイラーは警告を出さなくなります。

このキーワードが単に `noreturn` ではなくアンダースコアで始まるのは、既存のコードとの互換性に配慮したからでしょう。しかし、既存のコードとの互換性に配慮する必要のないコード用には単に `noreturn` と書けた方が良いと思われたのか、C11は `#define noreturn _Noreturn` するヘッダー `<stdnoreturn.h>` を用意しました。

```c
#include <stdnoreturn.h>
#define noreturn _Noreturn
```

`<stdnoreturn.h>` の役割はこれだけです。インクルードガードを除けばマジで1行で終わってしまいます。

さて、読者の中にはC++を知っている人もいるでしょう。C++11は「制御を返さない関数」を表す方法として `[[noreturn]]` 属性を用意しました。同じ機能なのに、C言語（C11）とC++で書き方が違うのです！制御を返さない関数をヘッダーで宣言する場合は、わざわざ `#ifdef __cplusplus` しなければなりません：

```c
#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
[[noreturn]]
#else
_Noreturn
#endif
void my_error(const char *message);

#ifdef __cplusplus
}
#endif
```

あるいはC++では `#define _Noreturn [[noreturn]]` とすれば行数を削減できるかもしれませんが、いずれにせよ面倒ですね！

幸いなことに、C23ではC++と同様の `[[]]` による属性が導入されました。そして、C++と同様の `[[noreturn]]` 属性も使えるようになります。C11の時点で `[[]]` を導入しておいてくれって感じですね（C11の時点でも検討はされていたようです）。

というわけでC11で導入された `_Noreturn` キーワードと `<stdnoreturn.h>` ヘッダーはC23では早くも「時代遅れになりつつある機能 (obsolescent feature)」となったのでした。C言語の世界では導入から12年で非推奨になるのはかなり早い方です。タイムアタックでもやってんのか。

ところで、C23でも `<stdnoreturn.h>` の機能はそのままです。突然廃止したらC11時代のコードが困りますからね。`<stdnoreturn.h>` と `[[noreturn]]` が同じ翻訳単位に混在したらどうなるでしょうか？つまり、次のようなヘッダー `myheader.h` を

```c
// myheader.h
// C23を念頭に書かれている
[[noreturn]] void my_error(const char *message);
```

次のようなソースファイル `mysource.c` が `#include` したらどうなるでしょうか？

```c
// mysource.c
// C11を念頭に書かれている

#include <stdnoreturn.h>
#include "myheader.h"
```

そう、プリプロセッサーによって `[[noreturn]]` が `[[_Noreturn]]` に変換されてしまいますね。

この問題に対処するため、C23では、`[[_Noreturn]]` 属性も `[[noreturn]]` のエイリアスとして認めるようになっています。互換性を保つのは大変ですね。

（ところでobsolescentってどう訳すのが定番なんですかね。「非推奨」でいいのか？）

* [N1478: Supporting the 'noreturn' property in C1x](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1478.htm)
* [N2764: The noreturn attribute (updates N2700)](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2764.pdf)

## `intmax_t` 型

C99では、任意の（符号あり／符号なし）整数型の任意の値を表現できる能力を持つ（符号あり／符号なし）整数型、あるいは「幅が最大の整数型」として `intmax_t`（符号あり）と `uintmax_t`（符号なし）が規定されました。

```c
#include <stdint.h>
typedef ... intmax_t;
typedef ... uintmax_t;
```

これらの型はプリプロセッサーの算術の規定で言及されたりします。また、C11の時点では `(u)intmax_t` に言及するライブラリー関数には以下があります：

```c
#include <inttypes.h>
intmax_t imaxabs(intmax_t j);
imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom);
intmax_t strtoimax(const char * restrict nptr, char ** restrict endptr, int base);
uintmax_t strtoumax(const char * restrict nptr, char ** restrict endptr, int base);
intmax_t wcstoimax(const wchar_t * restrict nptr, wchar_t ** restrict endptr, int base);
uintmax_t wcstoumax(const wchar_t * restrict nptr, wchar_t ** restrict endptr, int base);
```

このほか、`printf`, `scanf` 系の `j` 修飾子は `(u)intmax_t` を指します。

昔のC処理系では最大の幅を持つ整数型は64ビットの `long long` とかだったので、`intmax_t` は典型的な環境では64ビットです。

時は流れて、GCCなどのコンパイラーは128ビット整数型を拡張機能として提供するようになりました。`__int128` とか `unsigned __int128` ですね。この時、`intmax_t` 型の定義は128ビット整数型に変更されるべきでしょうか？それとも、64ビットに据え置かれるべきでしょうか？

もしも `intmax_t` の定義が128ビットに変更されてしまうと、従来のCコンパイラーでコンパイルしたバイナリーと新しいCコンパイラーでコンパイルしたバイナリーで辻褄（ABI）が合わなくなってしまいます。例えば、システムのlibcが `intmax_t = int64_t` を仮定しているのに、新たにコンパイルしたプログラムが `intmax_t = __int128` を仮定すると、`imaxabs` などの関数の呼び出しで不整合が生じたり、最悪の場合はクラッシュしてしまいます。

一方で、128ビット整数の導入後も `intmax_t` を64ビットに据え置くとすると、、「名が体を表さない」状態になってしまいます。

C23を策定する過程で、この `intmax_t` の問題も議論されたようです。そして、結論としては後者、つまり「64ビットに据え置く」ことができるように、文面を変えることになりました。最初に書いたようにC言語のABIは重要なので、ABIを壊す変更は受け入れられなかったってことですかね。

C23では、C言語の整数型は以下のように分類されることになりました：

* 標準の整数型 (standard integer types): `(signed|unsigned) char`, `(unsigned) short int`, `(unsigned) int`, `(unsigned) long int`, `(unsigned) long long int`, `bool`
* ビット幅が正確な整数型 (bit-precise integer types): `(unsigned) _BitInt(N)`
* 拡張整数型 (extended integer types): 処理系定義

そして、C23では `(u)intmax_t` の定義は「いくつかの例外を除いた整数型の値を表現できる整数型」となりました。ここで「いくつかの例外」としては、ビット幅が正確な整数型と、`long long` よりも幅の広い拡張整数型であって `(u)intN_t` として言及されているもの、が認められています。

つまり、C23ではコンパイラーが独自に定義した `__int128` を `int128_t` として提供しつつ、`intmax_t` を64ビットに据え置くことが認められるようになったのです！

* [intmax_t, again](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2498.pdf)

## `qsort_s` 関数と `bsearch_s` 関数

MSVC 2005は、「セキュアな関数」とか言って名前が `_s` で終わる関数を標準Cライブラリーに大量に追加しました。すでに紹介した `gets_s` もその仲間ですね。追加しただけならいいのですが、デフォルトで従来の関数に警告が出るようになったので一般Cユーザーは大迷惑でした（記憶）。その話は置いておきます。

Microsoftは `qsort` 関数と `bsearch` 関数にも「セキュアな」版を用意しました。これらの型は、MSVCでは次のようになっています：

```c
void qsort_s(
    void *base,
    size_t num,
    size_t width,
    int (*compare)(void *context, const void *, const void *),
    void *context
);
void *bsearch_s(
    const void *key,
    const void *base,
    size_t number,
    size_t width,
    int (*compare)(void *context, const void *key, const void *datum),
    void *context
);
```

* [qsort_s | Microsoft Learn](https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/qsort-s?view=msvc-170)
* [bsearch_s | Microsoft Learn](https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/bsearch-s?view=msvc-170)

`_s` じゃない方と比べると、比較関数が追加の引数 `void *context` を取れるようになっているのがクールですね！

さて、どういう経緯があったのか筆者はよく知りませんが、C11ではMSVCのセキュアな関数によく似たものをAnnex Kとして標準化することになりました。対応する処理系では、ヘッダーを `#include` する前にソースで `__STDC_WANT_LIB_EXT1__` を `1` と定義しておくと、`_s` で終わる関数が提供されるようになります。

ここで、C11の `qsort_s` と `bsearch_s` を見てみましょう：

```c
#define __STDC_WANT_LIB_EXT1__ 1
#include <stdlib.h>
errno_t qsort_s(
    void *base,
    rsize_t nmemb,
    rsize_t size,
    int (*compar)(const void *x, const void *y, void *context),
    void *context
)
void *bsearch_s(
    const void *key
    const void *base,
    rsize_t nmemb,
    rsize_t size,
    int (*compar)(const void *k, const void *y, void *context),
    void *context
);
```

MSVCとの違いに気づいたでしょうか？そう、比較関数の `context` 引数の位置が違います！

C言語は型が弱いので、`void *` と `const void *` を取り違えてもコンパイルが通ってしまうでしょう。すると、「C11の比較関数のつもりで `qsort_s` に引数を渡したが、実装はMSVCだった」場合（あるいはその逆）に引数の不整合が起こり、プログラムがクラッシュしてしまう可能性があります！

どうしてこんなことになったのか筆者はマジで知りませんが、もうちょっとこう、なんとかならなかったのかと思います。Microsoftと標準化委員会の間に何があったのか、それとも何もなかったが故にこうなったのか……。

* [N1031: Specification for secure C Library functions](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1031.pdf)

## `_Imaginary` 型

## スタック上の可変長配列
