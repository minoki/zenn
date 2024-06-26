---
title: "次期C標準 (C23) の内容が固まったらしい"
emoji: "😸"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c言語"]
published: true
---

C23については[最近のC言語と、次期C標準(C23)](modern-c-language)でも軽く紹介しました。

今回、C23入りする内容が大体固まったようなので改めて紹介します。

この記事を書いている時点での最新の公開されたWorking Draftは ~~[N2912](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2912.pdf)~~ ~~[N3047](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3047.pdf)~~ ~~[N3054](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3054.pdf)~~ [N3096](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3096.pdf)です。ただし、C2y向けの最初のドラフト[N3220](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf)もあり、そちらの方が実際の内容に近いかもしれません。

内容については会議参加者の投稿も参考にしています：

* https://twitter.com/rcs/status/1550526425211584512
* [C23 now finalized! : C_Programming](https://www.reddit.com/r/C_Programming/comments/w5hl80/c23_now_finalized/)

というわけで、C23に入る主な機能はこちらです：

# C23に入る主な機能

* POSIXの機能の取り込み: `strdup`, `strndup`, `memccpy`, `gmtime_r`, `localtime_r`
* C++の機能の取り込み:
    * `[[]]` による属性：標準では `[[nodiscard]]`, `[[maybe_unused]]`, `[[deprecated]]`, `[[fallthrough]]`, `[[noreturn]]`, `[[_Noreturn]]`, `[[reproducible]]`, `[[unsequenced]]` の8つ（実質7つ）。そのほかベンダー独自のもの `[[vendor::attr]]` も処理系次第で使える。
    * `char8_t` （ただしC++とは異なり、 `unsigned char` のtypedef）
    * `u8` 文字リテラル（注：u8文字**列**リテラルはC11ですでに導入されている）
    * 定義済みの `bool`, `true`, `false`, `static_assert`, `alignof`, `alignas`, `thread_local`
    * 1引数の `static_assert`
    * `auto` ([N3007](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3007.htm))
        * GCC等ではすでに `__auto_type` というキーワードでC++の `auto` みたいなやつが使えるようになっていました。マクロとかで便利なようです。[Typeof (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-12.1.0/gcc/Typeof.html#Typeof)
    * `constexpr` による定数。関数は不可。 ([N3018](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3018.htm))
    * 基礎となる型を指定した `enum` ([N3021](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3021.htm))
    * `nullptr` ([N3019](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3019.htm))
    * digit separator
    * 二進数リテラル
    * `= {}` による初期化 ([N2900](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2900.htm))
* 浮動小数点数
    * IEEE 754-2019への対応。これまではIEEE 754-1985を参照していた。
    * TS 18661-1, TS 18661-2およびTS 18661-4の一部は本文に取り込まれ、TS 18661-3（`_FloatN` 型など）はAnnex Hとして取り込まれた（以前のAnnex HはLanguage Independent Arithmeticだった）。
    * 追加される `#pragma` は `#pragma STDC FENV_ROUND`, `#pragma STDC FENV_DEC_ROUND` などです。
* K&amp;Rスタイルの関数定義の廃止
    * また、定義が伴わない関数宣言の仮引数リストが空の場合は「関数が引数を取らない」ことを表すようになります（[N2841](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2841.htm)）。
* 必須の引数を持たない可変長引数関数を定義できるようになる（[N2975](http://www.open-std.org/jtc1/sc22/wg14/www/docs/n2975.pdf)）
    * `va_start` の二番目の引数はoptionalとなる。
    * C++では以前から「必須の引数を持たない可変長引数関数の定義」ができ、SFINAEとかで活用されていましたが、 `va_start` を使えないため引数にアクセスすることはできませんでした。
* 2の補数表現が必須となる
* `#elifdef`, `#elifndef`
* `#embed` ([N3017](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3017.htm))
    * ファイルの埋め込みができます。
* `#warning`
* `typeof` ([N2927](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2927.htm))
    * C++の `decltype` は参照型に依存するため、C言語には持ってこれませんでした。
    * キーワードにアンダースコアとかがつかないのは、各種コンパイラーが既に `typeof` キーワードを提供しているし変数名に使っているやつはいないだろ、というアレがあるんでしょう（適当）。
    * ~~N2927では `typeof` の他に `remove_quals` というやつが入ります。一方で `remove_quals` ではなく `unqual_typeof` みたいな名前にしようぜ、という提案（[N2930](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2930.pdf)）もあり、結局どっちになったの？~~ `typeof` の他に `typeof_unqual` も入ります。
* `unreachable()` ([N2826](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2826.pdf))
    * GCC/Clangの `__builtin_unreachable()` とかMSVCの `__assume(false)` とかC++23の `std::unreachable()` みたいなやつです。ある種の最適化に役立ちます。
* `_BitInt`
* `memset_explicit`
    * 以前から似たようなやつにAnnex Kの `memset_s` がありましたが、引数がちょっと違います。
* スタックに確保しない可変長配列（ポインターとか引数とか）は必須の機能に昇格されます。
* オーバーフローを検査する整数演算：`ckd_add`, `ckd_sub`, `ckd_mul` in `<stdckdint.h>` ([N2683](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2683.pdf))
* `__has_include`
* popcountやclz, ctzなどのビット演算 `<stdbit.h>`: `stdc_count_ones`, `stdc_leading_zeros`, `stdc_trailing_zeros` など（[N3022](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3022.htm)の一部）

個々の機能についてここで詳しい説明をするといくら時間があっても足りません。いくつかについては私の書いた記事があります。

* [C言語のbool型とその名前について 〜もう_Boolは嫌だ〜](boolean-in-c)
* [C言語でのオーバーフロー検査](c-checked-int)（`<stdckdint.h>`）
* [C23の新機能：高度なビット操作<stdbit.h>の紹介](c23-stdbit-h)
* [C23の改善点：文字列検索関数でのconst性の維持](c23-constness)

他のいくつかについては、yohhoy氏の記事があるので紹介しておきます：

* [C2x標準の属性(attribute) - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20200505/p1)
* [#elifdefと#elifndef - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210604/p1)
* [2進数リテラル in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210228/p1)
* [realloc(ptr, 0)は廃止予定 - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210909/p1)
* [2進数フォーマット出力 in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20211028/p1)
* [nullptr定数 in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220906/p1)
* [reproducible/unsequenced属性 - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220909/p1)
* [typeof演算子 in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220912/p1)
* [#embedディレクティブ - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220915/p1)
* [R.I.P. "= {0}" - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220917/p1)

標準化委員会の中の人による記事も参考になります：

* [finally. #embed | The Pasture](https://thephd.dev/finally-embed-in-c23)
* [C23 is Finished: Here is What is on the Menu | The Pasture](https://thephd.dev/c23-is-coming-here-is-what-is-on-the-menu)

機能リストについてはcppreferenceの方にもまとまっているようです。

* [C23 - cppreference.com](https://en.cppreference.com/w/c/23)

# GCCの対応状況

最近のGCCでは一部の機能が `-std=c2x` で（GCC 14以降では `-std=c23` でも）使えるようになっています。

* [GCC 10 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-10/changes.html#c)
* [GCC 11 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-11/changes.html#c)
* [GCC 12 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-12/changes.html#c)
* [GCC 13 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-13/changes.html#c)
* [GCC 14 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-14/changes.html#c)

GCC 10では

* `[[]]` による属性：標準の `[[deprecated]]`, `[[fallthrough]]`, `[[maybe_unused]]` と、 `[[gnu::ﾎﾆｬﾗﾗ]]`
* UTF-8文字リテラル：`u8''`

などが、GCC11では

* `<stdbool.h>` で定義される `true`, `false` が `bool` 型を持つようになった
* `[[nodiscard]]` 属性
* `__has_c_attribute`

などが、GCC12では

* digit separators
* `#elifdef`, `#elifndef`

などが、GCC13では

* `nullptr`
* `enum` の拡張
* `auto`
* `constexpr`
* `typeof`, `typeof_unqual`
* `alignas`, `alignof`, `bool`, `false`, `static_assert`, `thread_local`, `true` のキーワード化
* `[[noreturn]]` 属性
* `= {}`
* `unreachable()`
* プロトタイプなし関数の削除

などが、GCC14では

* `_BitInt`
* 内容が同じstruct, union, enumを複数回定義できる
* `<stdckdint.h>`

などが実装されています。

# Clangの対応状況

Clangもバージョン9以降で `-std=c2x` によりC23の機能の一部が使えるようになっているようです。

* [Clang - C Programming Language Status - C2x implementation status](https://clang.llvm.org/c_status.html#c2x)

---

この記事は随時アップデートしています。変更履歴は[GitHub](https://github.com/minoki/zenn/blob/master/articles/next-c-language.md)を見てください。
