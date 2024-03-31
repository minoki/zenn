---
title: "C23の新機能：高度なビット操作<stdbit.h>の紹介"
emoji: "😊"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

今年（2024年）出版される予定のC言語の新標準C23には、高度なビット操作関数を含む `<stdbit.h>` が入ります。この記事ではそれを紹介します。

C23についての他の話題は[次期C標準 (C23) の内容が固まったらしい](next-c-language)を参照してください。

## エンディアン

`<stdbit.h>` ではエンディアンを判別するためのマクロが規定されています。定義されるマクロは次の3つで、それぞれ整数定数です：

```c
__STDC_ENDIAN_LITTLE__
__STDC_ENDIAN_BIG__
__STDC_ENDIAN_NATIVE__
```

使い方は次のようになります：

```c
#if __STDC_ENDIAN_NATIVE__ == __STDC_ENDIAN_LITTLE__
// リトルエンディアン
#elif __STDC_ENDIAN_NATIVE__ == __STDC_ENDIAN_BIG__
// ビッグエンディアン
#else
// その他
#endif
```

エンディアン変換関数はC23には入りませんでした。

## Count Leading Zeros

上位ビットの連続する0の個数を数えます。

```c
unsigned int stdc_leading_zeros_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_leading_zeros_us(unsigned short value) [[unsequenced]];
unsigned int stdc_leading_zeros_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_leading_zeros_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_leading_zeros_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_leading_zeros(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_leading_zeros`）は、以下の型について適用できます：

* `bool` 以外の標準の符号なし整数型 (standard unsigned integer type)：`unsigned char`, `unsigned short`, `unsigned int`, `unsigned long`, `unsigned long long`
* 処理系定義の符号なし整数型 (extended unsigned integer type)
* `unsigned _BitInt(N)`

`<generic-return-type>` は結果を格納するのに十分な大きさの符号なし整数型です。

GCCで言うところの `__builtin_clz` 相当ですが、`__builtin_clz` は入力が0の場合にUBなのに対して `stdc_leading_zeros`　はそういう規定はなさそうです。

## Count Leading Ones

上位ビットの連続する1の個数を数えます。

```c
unsigned int stdc_leading_ones_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_leading_ones_us(unsigned short value) [[unsequenced]];
unsigned int stdc_leading_ones_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_leading_ones_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_leading_ones_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_leading_ones(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_leading_ones`）については、Count Leading Zerosと同様です。

## Count Trailing Zeros

下位ビットの連続する0の個数を数えます。

```c
unsigned int stdc_trailing_zeros_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_trailing_zeros_us(unsigned short value) [[unsequenced]];
unsigned int stdc_trailing_zeros_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_trailing_zeros_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_trailing_zeros_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_trailing_zeros(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_trailing_zeros`）については、Count Leading Zerosと同様です。

GCCで言うところの `__builtin_ctz` 相当ですが、`__builtin_ctz` は入力が0の場合にUBなのに対して `stdc_trailing_zeros`　はそういう規定はなさそうです。

## Count Trailing Ones

下位ビットの連続する1の個数を数えます。

```c
unsigned int stdc_trailing_ones_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_trailing_ones_us(unsigned short value) [[unsequenced]];
unsigned int stdc_trailing_ones_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_trailing_ones_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_trailing_ones_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_trailing_ones(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_trailing_ones`）については、Count Leading Zerosと同様です。

## First Leading Zero

上位ビットから数えて最初の0のインデックスに1を加えたものを返します。そのような0が見つからなければ0を返します。

```c
unsigned int stdc_first_leading_zero_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_first_leading_zero_us(unsigned short value) [[unsequenced]];
unsigned int stdc_first_leading_zero_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_first_leading_zero_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_first_leading_zero_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_first_leading_zero(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_first_leading_zero`）については、Count Leading Zerosと同様です。

## First Leading One

上位ビットから数えて最初の1のインデックスに1を加えたものを返します。そのような1が見つからなければ0を返します。

```c
unsigned int stdc_first_leading_one_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_first_leading_one_us(unsigned short value) [[unsequenced]];
unsigned int stdc_first_leading_one_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_first_leading_one_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_first_leading_one_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_first_leading_one(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_first_leading_one`）については、Count Leading Zerosと同様です。

## First Trailing Zero

下位ビットから数えて最初の0のインデックスに1を加えたものを返します。そのような0が見つからなければ0を返します。

```c
unsigned int stdc_first_trailing_zero_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_first_trailing_zero_us(unsigned short value) [[unsequenced]];
unsigned int stdc_first_trailing_zero_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_first_trailing_zero_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_first_trailing_zero_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_first_trailing_zero(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_first_trailing_zero`）については、Count Leading Zerosと同様です。

## First Trailing One

下位ビットから数えて最初の1のインデックスに1を加えたものを返します。そのような1が見つからなければ0を返します。

```c
unsigned int stdc_first_trailing_one_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_first_trailing_one_us(unsigned short value) [[unsequenced]];
unsigned int stdc_first_trailing_one_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_first_trailing_one_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_first_trailing_one_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_first_trailing_one(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_first_trailing_one`）については、Count Leading Zerosと同様です。

GCCで言うところの `__builtin_ffs` 相当です。

## Count Zeros

0の個数を数えます。

```c
unsigned int stdc_count_zeros_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_count_zeros_us(unsigned short value) [[unsequenced]];
unsigned int stdc_count_zeros_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_count_zeros_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_count_zeros_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_count_zeros(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_count_zeros`）については、Count Leading Zerosと同様です。

## Count Ones

1の個数を数えます。いわゆるpopulation countです。

```c
unsigned int stdc_count_ones_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_count_ones_us(unsigned short value) [[unsequenced]];
unsigned int stdc_count_ones_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_count_ones_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_count_ones_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_count_ones(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_count_ones`）については、Count Leading Zerosと同様です。

GCCで言うところの `__builtin_popcount` 相当です。

## Single-bit Check

立っている1の個数がちょうど1個のとき、かつその時に限り `true` を返します。

```c
bool stdc_has_single_bit_uc(unsigned char value) [[unsequenced]];
bool stdc_has_single_bit_us(unsigned short value) [[unsequenced]];
bool stdc_has_single_bit_ui(unsigned int value) [[unsequenced]];
bool stdc_has_single_bit_ul(unsigned long value) [[unsequenced]];
bool stdc_has_single_bit_ull(unsigned long long value) [[unsequenced]];
bool stdc_has_single_bit(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_has_single_bit`）の入力については、Count Leading Zerosと同様です。

## Bit Width

与えられた整数を表現するのに必要な最小のビット数を返します。入力が0の場合は0、そうでない場合は$1+\lfloor\log_2(\mathtt{value})\rfloor$です。

```c
unsigned int stdc_bit_width_uc(unsigned char value) [[unsequenced]];
unsigned int stdc_bit_width_us(unsigned short value) [[unsequenced]];
unsigned int stdc_bit_width_ui(unsigned int value) [[unsequenced]];
unsigned int stdc_bit_width_ul(unsigned long value) [[unsequenced]];
unsigned int stdc_bit_width_ull(unsigned long long value) [[unsequenced]];
<generic-return-type> stdc_bit_width(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_bit_width`）については、Count Leading Zerosと同様です。

## Bit Floor

与えられた整数を超えない最大の$2^n$を返します。ただし、入力が0の場合は0を返します。

```c
unsigned char stdc_bit_floor_uc(unsigned char value) [[unsequenced]];
unsigned short stdc_bit_floor_us(unsigned short value) [[unsequenced]];
unsigned int stdc_bit_floor_ui(unsigned int value) [[unsequenced]];
unsigned long stdc_bit_floor_ul(unsigned long value) [[unsequenced]];
unsigned long long stdc_bit_floor_ull(unsigned long long value) [[unsequenced]];
<generic-value-type> stdc_bit_floor(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_bit_floor`）の入力の型については、Count Leading Zerosと同様です。

## Bit Ceiling

与えられた整数以上の最小の$2^n$を返します。ただし、それを表現できない場合は0を返します。

```c
unsigned char stdc_bit_ceil_uc(unsigned char value) [[unsequenced]];
unsigned short stdc_bit_ceil_us(unsigned short value) [[unsequenced]];
unsigned int stdc_bit_ceil_ui(unsigned int value) [[unsequenced]];
unsigned long stdc_bit_ceil_ul(unsigned long value) [[unsequenced]];
unsigned long long stdc_bit_ceil_ull(unsigned long long value) [[unsequenced]];
<generic-value-type> stdc_bit_ceil(<generic-value-type> value) [[unsequenced]];
```

ジェネリックなやつ（`stdc_bit_ceil`）の入力の型については、Count Leading Zerosと同様です。

## その他

`__STDC_VERSION_STDBIT_H__` が `202311L` と等価な整数定数として定義されます。

`<stdbit.h>` によって `size_t`, `uintN_t`, `intN_t`, `uint_leastN_t`, `int_leastN_t` が使えるようになります。

## 注意事項

ジェネリックな関数はマクロとして定義されるかもしれませんし、それ以外の方法で定義されるかもしれません。マクロ展開が抑制された場合や、ジェネリックな関数と同じ名前の外部リンケージを持つ識別子が定義された場合の挙動は未規定 (unspecified) です。

`[[unsequenced]]` 属性はいわゆる「純粋関数」っぽいことを表現します。詳しくは

* [reproducible/unsequenced属性 - yohhoyの日記](https://yohhoy.hatenadiary.jp/entry/20220909/p1)
* [C attribute: unsequenced, reproducible (since C23) - cppreference.com](https://en.cppreference.com/w/c/language/attributes/unsequenced)

などを参照してください。

## libcの対応

glibc 2.39で `<stdbit.h>` が使えるようになったというニュースがありました。

* [GNU C Library 2.39 Released With New Tunables, stdbit.h For ISO C2X - Phoronix](https://www.phoronix.com/news/GNU-C-Library-glibc-2.39)
* [The GNU C Library version 2.39 is now available](https://lists.gnu.org/archive/html/info-gnu/2024-01/msg00017.html)

## 参考文献

`<stdbit.h>` は以下の提案

* [N3022: Modern Bit Utilities](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3022.htm)

がベースとなっています。C23には入りませんが、さらなるビット演算の提案が

* [N3104: More Modern Bit Utilities](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3104.htm)

でされています。

この記事を執筆している時点でC23はまだ出版されていませんが、C23が確定する前の最後の公開ドラフトN3096と、C23の次の標準に向けて作業が始まった最初のドラフトN3220はそれぞれ次で参照できます。N3096とN3220では諸々の関数に `[[unsequenced]]` がついているかどうかの違いがありますが、実際のC23に近いのはN3220の方だと考え、この記事では `[[unsequenced]]` をつけました。

* [N3096](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3096.pdf)（2023年4月2日）
* [N3220](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf)（2024年2月22日）
