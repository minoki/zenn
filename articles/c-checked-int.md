---
title: "C言語でのオーバーフロー検査"
emoji: "😽"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c言語]
published: true
---

C言語では符号付き整数のオーバーフローは未定義動作です。ですが、未定義動作とか言ってないでオーバーフローが起きたことをなんらかの方法で知りたい場合もあります。次期C標準であるC23では（符号付きまたは符号なし）整数のオーバーフローを検出する機能が提供される見込みです。

# GCC拡張

GCC/Clangは拡張機能として、オーバーフローの有無を教えてくれるビルトイン関数を提供しています。

* [Integer Overflow Builtins (Using the GNU Compiler Collection (GCC))](https://gcc.gnu.org/onlinedocs/gcc-12.2.0/gcc/Integer-Overflow-Builtins.html)
* [Checked Arithmetic Builtins - Clang Language Extensions](https://clang.llvm.org/docs/LanguageExtensions.html#checked-arithmetic-builtins)

```c
bool __builtin_add_overflow(type1 a, type2 b, type3 *res);
bool __builtin_sadd_overflow(int a, int b, int *res); // long, long long版もある
bool __builtin_uadd_overflow(unsigned int a, unsigned int b, unsigned int *res); // unsigned long, unsigned long long版もある
// sub, mulについても同様
```

これらのビルトイン関数は、第1、第2引数を正確な（無限精度の）整数とみなして演算を行い、第3引数の指す型にキャストし、そこに格納します。キャストの結果が正確であれば `false` が返り、キャストの結果が不正確であれば（つまり、オーバーフローが発生すれば） `true` が返ります。

最初のやつ（`type1`, `type2`, `type3` を取るやつ）は `bool` 型、enum型には適用できません。

2の補数表現で表される符号付き整数の場合は除算もオーバーフローを起こす可能性がありますが、GCCはそれに対する検査は提供されていません。`a == INT_MIN && b == -1` で十分だからでしょう。

# ライブラリー

既存のライブラリーもいくつかあるので紹介しておきます（N2683の受け売りですが）。

Windowsには `intsafe.h` というのがあるらしいです。

* [Intsafe.h Functions - Win32 apps | Microsoft Learn](https://learn.microsoft.com/en-us/windows/win32/shell/intsafe-h-functions-bumper)

SafeIntというライブラリーもあります。

* [dcleblanc/SafeInt: SafeInt is a class library for C++ that manages integer overflows.](https://github.com/dcleblanc/SafeInt)

BoostにはSafe Numericsというライブラリーが含まれています。

* [Safe Numerics - develop](https://www.boost.org/doc/libs/develop/libs/safe_numerics/doc/html/index.html)

# C23での方法

C23に向けて [N2683: Towards Integer Safety](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2683.pdf) で「安全な整数演算」について色々提案されました。その中から、以下の3つがC23に取り込まれます：

```c
#include <stdckdint.h>
bool ckd_add(type1 *result, type2 a, type3 b);
bool ckd_sub(type1 *result, type2 a, type3 b);
bool ckd_mul(type1 *result, type2 a, type3 b);
```

GCC拡張との違いは引数の順序が変わったことと、除外される整数型として `bool`, enumの他に（符号が不定な） `char` と `_BitInt` が加わったことくらいです。

つまり、 `a` と `b` の演算結果を `*result` に格納し、オーバーフローが発生した場合は `true` を返します。

C言語にはオーバーロードはないので、これらの3つはマクロとして規定されています。

---

C23の他の機能については

* [次期C標準 (C23) の内容が固まったらしい](next-c-language)

を参照してください。
