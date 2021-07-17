---
title: "最近のC言語と、次期C標準(C23)"
emoji: "👏"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c言語"]
published: true
---

C言語といえば古い言語なイメージですが、その重要性はまだまだ落ちていません（多分）。重要な言語だからこそ、今もひっそりと進化を続けています。この記事では、そんなC言語の最近の動向を紹介します。

まずはC言語の前世紀の標準であるC99、現行の標準であるC11/C17を振り返り、その後に未来の標準であるC23に触れます。

# C99

C99では色々追加されました。ここでは一部のみの紹介とします。

* `_Bool`
* `_Complex`
    * C++の `std::complex` とメモリ上での互換性がある（C++11以降）。
* 可変長配列（VLA）
* 可変長引数マクロ
* 浮動小数点数の強化
    * 十六進表記
        * 筆者による関連記事：[浮動小数点数の16進表記](https://qiita.com/mod_poppo/items/3fa4cdc35f9bfb352ad5)
    * `fma`
        * 筆者による関連記事：[FMA (fused multiply-add) の話](https://qiita.com/mod_poppo/items/e6577df362f44a3ef8dd)
    * `#pragma STDC FENV_ACCESS`, `#pragma STDC CX_LIMITED_RANGE`, `#pragma STDC FP_CONTRACT`
* `restrict`
* ブロックの先頭以外での変数宣言
* `intmax_t` というのもC99で導入されましたがABI互換の問題がありC23に向けて議論中です。

メジャーなCコンパイラーであるGCCやClangは長いこと `#pragma STDC FENV_ACCESS` に対応していなかったのですが、Clang 12がついに `#pragma STDC FENV_ACCESS` に対応しました。
…が、全てのターゲットに対応しているわけではないようで、、手元のarm64-darwinでは

> '#pragma FENV_ACCESS' is not supported on this target

と言われました。

# C11 / C17

C11でも色々地味に追加されています。以下のページに変更点がまとまっています：

* [c - difference between c99 and c11 - Stack Overflow](https://stackoverflow.com/questions/38405260/difference-between-c99-and-c11)
* [C11 (C standard revision) - Wikipedia](https://en.wikipedia.org/wiki/C11_%28C_standard_revision%29)

ここでも変更点の一部を紹介すると、

* `_Alignof`, `_Alignas`, `aligned_alloc`
* `char16_t`, `char32_t`, `u""`, `U""`, `u8""`, `u''`, `U''`
    * C++でおなじみ。
    * `char16_t` と `char32_t` は `<uchar.h>` で定義される。C++のそれらは固有の型だが、C言語ではそれぞれ `uint_least16_t` と `uint_least32_t` のtypedefである。
    * C23で `u8""` の型は `char8_t` の配列に変更されるかも。
* `_Static_assert`
    * `<assert.h>` で `#define static_assert _Static_assert` される。
    * C++での対応物: `static_assert`
* `_Generic`
    * `<tgmath.h>`のマクロを自分で書けるようになる。
    * C++の型によるオーバーロードっぽいことができるようになる。
* `_Noreturn`
    * C++での対応物: `[[noreturn]]`
    * `<stdnoreturn.h>` で `#define noreturn _Noreturn` される。C++との互換性がない！
    * C23でC++互換な `[[noreturn]]` が導入され、 `_Noreturn` は非推奨になる予定。
* threads (optional)
    * `_Thread_local` キーワード
    * `<threads.h>`
    * 関連して、 `quick_exit` と `at_quick_exit`
* atomics (optional)
    * `_Atomic` キーワード
    * `<stdatomic.h>`
* C99で必須だった複素数とVLAはoptionalな機能に格下げされる。
* 浮動小数点数周りの強化
    * `CMPLX` マクロ
    * `(FLT|DBL|LDBL)_HAS_SUBNORM`
    * `(FLT|DBL|LDBL)_DECIMAL_DIG`
    * `(FLT|DBL|LDBL)_TRUE_MIN`
* Annex K: Bounds-checking interfaces
    * `_s` で終わる諸々の関数のこと。
* Annex L: Analyzability
* `gets` の廃止
* 無名struct, union
* `fopen` の `"...x"`
* `timespec_get`

という感じです。

ところで、長らく「主要なCコンパイラー」と言えばGCCとClangの2強\[要出典\]でしたが、2020年、MSVCがついにC11/C17準拠を目指し始めました。

* [C11 and C17 Standard Support Arriving in MSVC | C++ Team Blog](https://devblogs.microsoft.com/cppblog/c11-and-c17-standard-support-arriving-in-msvc/)

かつてのMSは「C++のサブセットではないCの機能は実装しない」という態度を取っていたこと（参考：2012年のHerb Sutter氏の記事

* [Reader Q&A: What about VC++ and C99? – Sutter’s Mill](https://herbsutter.com/2012/05/03/reader-qa-what-about-vc-and-c99/)  (2012年)

）を考えると、これは大きな方針転換です。

なお、C17はC11とほぼ同じです。

# 浮動小数点数に関するTechnical Specification

C11の時点では浮動小数点数の規格としてIEEE 754-1985相当を参照していました。しかし浮動小数点数の規格は2008年と2019年にそれぞれ新しいものが出ています。C言語でもこれに追従しようということで、2014年から2016年にかけてTS 18661が出版されました。

* TS 18661-1: IEEE 754-2008に対応させるやつ。IEEE 754-2008で必須とされている関数とか。
* TS 18661-2: 十進浮動小数点数。`_Decimal64` 型とか
* TS 18661-3: 二進交換形式と拡張形式。`_Float64` 型とか
* TS 18661-4: 追加の関数。IEEE 754-2008でオプショナルなもの。
* TS 18661-5: 追加の属性。`#pragma STDC FENV_` なやつを大量に追加する。

これらの一部はC23に取り込まれる見込みです。また、C標準以外ではARM C Language ExtensionがTS 18661-3で規定される `_Float16` 型を参照しています。

Technical SpecificationやTechnical Reportは他にもいくつか出ていて、固定小数点数へのサポートが含まれるTR 18037: Embedded Cというのもあります。

# C23 (C2x)

いよいよ次期C標準、C23です。少し前までは「C2x」と呼ばれていましたが、2020年11月ごろにスケジュールが固まって（？）、それ以降はC23と呼ばれています。

2021年7月時点で最新のドラフトは [N2596](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2596.pdf) (2020-12-12) です。

## C23 Charter

最新のC23 Charterは [N2611](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2611.htm) (2020-11-09) です。

C23に際しては「APIはなるべく自己記述的であること」が追加されました：

> 15. Application Programming Interfaces (APIs) should be self-documenting when possible.

具体的には、配列のサイズを表す引数は配列よりも前に現れることが挙げられています。コード例は次の通りです：

```c
double sum1(size_t n, const double *arr); // 好ましい
double sum2(const double *arr, size_t n); // 好ましくない
```

これはなぜかというと、可変長配列(VLA)を使って自己記述的な関数にしようとすると配列のサイズが先に来なければならないからです：

```c
double sum1(size_t n, const double arr[n]); // 自己記述的なAPI; n が先にくる必要がある
```

ちなみに、「長さの引数が後の場合でもVLAを書けるようにしよう」つまり引数の前方宣言をできるようにしよう、という提案もあります([N2780](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2780.pdf))。この機能はGCC拡張としてすでにGCCに実装されています。

## C23の新機能

Working Draftにすでに取り込まれたものを中心に挙げていきます。

* POSIXの機能の取り込み: `strdup`, `strndup`, `memccpy`
* C++の機能の取り込み:
    * `[[]]` による属性：N2596の時点では `[[nodiscard]]`, `[[maybe_unused]]`, `[[deprecated]]`, `[[fallthrough]]` の4つ。このほか `[[noreturn]]` も入りそう。
    * `u8` 文字リテラル（注：u8文字**列**リテラルはC11ですでに導入されている）
    * 1引数の `_Static_assert`
* 浮動小数点数
    * IEEE 754-2008またはIEEE 754-2019への対応。これまではIEEE 754-1985を参照していた。
    * TS 18661-1, TS 18661-2およびTS 18661-4の一部は本文に取り込まれ、TS 18661-3（`_FloatN` 型など）はAnnexとして取り込まれる見込み。
* K&amp;Rスタイルの関数定義の廃止
    * ただし、定義が伴わない関数宣言の仮引数リストが空の場合は相変わらず「引数の規定がない」ものとして扱われ、「関数が引数を取らない」ことを明示するには `int f(void);` という風に `void` を使う必要があります。
* 2の補数表現が必須となる

他にも色々提案中の機能があります

* `bool`, `true`, `false` をfirst-classにする
* `#elifdef`, `#elifndef`
* C++ライクな `auto` とかlambda（無名関数）とか
* `typeof`

個々の機能についてここで詳しい説明をするといくら時間があっても足りないので割愛します。いくつかについては、yohhoy氏の記事があるので紹介しておきます：

* [C2x標準の属性(attribute) - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20200505/p1)
* [#elifdefと#elifndef - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210604/p1)
* [2進数リテラル in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210228/p1)

## GCCの対応状況

最近のGCCでは一部の機能が `-std=c2x` で使えるようになっています。

* [GCC 10 Release Series — Changes, New Features, and Fixes - GNU Project - Free Software Foundation (FSF)](https://gcc.gnu.org/gcc-10/changes.html#c)
* [GCC 11 Release Series — Changes, New Features, and Fixes - GNU Project - Free Software Foundation (FSF)](https://gcc.gnu.org/gcc-11/changes.html#c)

GCC 10では

* `[[]]` による属性：標準の `[[deprecated]]`, `[[fallthrough]]`, `[[maybe_unused]]` と、 `[[gnu::ﾎﾆｬﾗﾗ]]`
* UTF-8文字リテラル：`u8''`

などが、GCC11では

* `<stdbool.h>` で定義される `true`, `false` が `bool` 型を持つようになった
* `[[nodiscard]]` 属性
* `__has_c_attribute`

などが実装されています。

## Clangの対応状況

Clangもバージョン9以降で `-std=c2x` によりC23の機能の一部が使えるようになっているようです。

* [Clang - C Programming Language Status - C2x implementation status](https://clang.llvm.org/c_status.html#c2x)

---

なんとなく中途半端な記事になってしまいました。

WG14の文書を読み漁って定期的に日本語でまとめてくれる人がいるといいなあ……という気がします。言い出しっぺがやれよと言われそうですが、筆者にはそこまでC言語に時間を費やす動機がないので……。
