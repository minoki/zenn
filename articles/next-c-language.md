---
title: "次期C標準 (C23) の内容が固まったらしい"
emoji: "😸"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c言語"]
published: true
---

C23については[最近のC言語と、次期C標準(C23)](modern-c-language)でも軽く紹介しました。

今回、C23入りする内容が大体固まったようなので改めて紹介します。

この記事を書いている時点での最新のWorking Draftは[N2912](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2912.pdf)です。

直近の会議の議事録はまだ出ていないようなので、内容については会議参加者の投稿を参考にしています：

* https://twitter.com/rcs/status/1550526425211584512
* [C23 now finalized! : C_Programming](https://www.reddit.com/r/C_Programming/comments/w5hl80/c23_now_finalized/)

というわけで、C23に入る主な機能はこちらです：

* POSIXの機能の取り込み: `strdup`, `strndup`, `memccpy`
* C++の機能の取り込み:
    * `[[]]` による属性：標準では `[[nodiscard]]`, `[[maybe_unused]]`, `[[deprecated]]`, `[[fallthrough]]`, `[[noreturn]]`, `[[_Noreturn]]` の6つ。そのほかベンダー独自のもの `[[vendor::attr]]` も処理系次第で使える。
    * `u8` 文字リテラル（注：u8文字**列**リテラルはC11ですでに導入されている）
    * 定義済みの `bool`, `true`, `false`, `static_assert`, `alignof`, `alignas`, `thread_local`
        * [C言語のbool型とその名前について 〜もう_Boolは嫌だ〜](boolean-in-c)も参照してください。
    * 1引数の `static_assert`
    * `auto`
    * `constexpr` による定数。関数は不可。
    * 基礎となる型を指定した `enum`
    * `nullptr`
    * digit separator
    * 二進数リテラル
* 浮動小数点数
    * IEEE 754-2019への対応。これまではIEEE 754-1985を参照していた。
    * TS 18661-1, TS 18661-2およびTS 18661-4の一部は本文に取り込まれ、TS 18661-3（`_FloatN` 型など）はAnnex Hとして取り込まれた（以前のAnnex HはLanguage Independent Arithmeticだった）。
* K&amp;Rスタイルの関数定義の廃止
    * また、定義が伴わない関数宣言の仮引数リストが空の場合は「関数が引数を取らない」ことを表すようになります（N2841）。
* 2の補数表現が必須となる
* `#elifdef`, `#elifndef`
* `#embed`
    * ファイルの埋め込みができます。
    * [finally. #embed | The Pasture](https://thephd.dev/finally-embed-in-c23)
* `typeof`
    * C++の `decltype` は参照型に依存するため、C言語には持ってこれませんでした。
    * キーワードにアンダースコアとかがつかないのは、各種コンパイラーが既に `typeof` キーワードを提供しているし変数名に使っているやつはいないだろ、というアレがあるんでしょう（適当）。
    * Working Draftのやつでは `typeof` の他に `remove_quals` というやつが入ります。一方で `remove_quals` ではなく `unqual_typeof` みたいな名前にしようぜ、という提案（N2930）もあり、結局どっちになったの？
* `unreachable()`
* `_BitInt`
* `memset_explicit`
    * 以前から似たようなやつにAnnex Kの `memset_s` がありましたが、引数がちょっと違います。
* スタックに確保しない可変長配列（ポインターとか引数とか）は必須の機能に昇格されます。
* オーバーフローを検査する整数演算：`ckd_add`, `ckd_sub`, `ckd_mul`

個々の機能についてここで詳しい説明をするといくら時間があっても足りません。いくつかについては、yohhoy氏の記事があるので紹介しておきます：

* [C2x標準の属性(attribute) - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20200505/p1)
* [#elifdefと#elifndef - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210604/p1)
* [2進数リテラル in 標準C - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20210228/p1)

## GCCの対応状況

最近のGCCでは一部の機能が `-std=c2x` で使えるようになっています。

* [GCC 10 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-10/changes.html#c)
* [GCC 11 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-11/changes.html#c)
* [GCC 12 Release Series — Changes, New Features, and Fixes - GNU Project](https://gcc.gnu.org/gcc-12/changes.html#c)

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

などが実装されています。

## Clangの対応状況

Clangもバージョン9以降で `-std=c2x` によりC23の機能の一部が使えるようになっているようです。

* [Clang - C Programming Language Status - C2x implementation status](https://clang.llvm.org/c_status.html#c2x)

---

見落としていた重要な機能に気付いたり、議事録が出たりしたら適宜この記事をアップデートしていきたいと思います。
