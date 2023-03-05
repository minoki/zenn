---
title: "C言語の符号付き整数はモジュラー演算ではない"
emoji: "📚"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["c言語"]
published: true
---

C言語の符号付き整数の足し算、引き算、掛け算は「$2^n$を法とするモジュラー演算である」と説明されることがありますが、これは間違いです。正しい説明は、「C言語の符号付き整数のオーバーフローは未定義動作である」となります。

この違いが観測できる例を2つ紹介します。

まず、単項マイナスについて。以下のプログラムを考えます：

```c
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
bool f(int x)
{
    return x == -x;
}
int main()
{
    printf("%s\n", f(INT_MIN) ? "true" : "false");
}
```

`int` 型の単項マイナスが$2^n$を法とするモジュラー計算で行われるのであれば、`g` が `true` を返す `x` は `0` と `INT_MIN` の2つのはずです。ですが、実際には `int` の単項マイナスのオーバーフローは未定義動作なので、コンパイラーは `x = INT_MIN` の場合を考慮せずに最適化を行うことができます。

実際、Clangでは最適化の有無で上記のプログラムの実行結果が変わります：

```
$ clang -o test1 test1.c
$ ./test1
true
$ clang -o test1 -O2 test1.c
$ ./test1
false
```

（`x == -x` の挙動がコンパイラーの最適化で変わることは、私がLuaJITにissueを立てた際にLuaJITの作者に教えていただきました：[Undefined behavior with negation of INT*_MIN · Issue #928 · LuaJIT/LuaJIT](https://github.com/LuaJIT/LuaJIT/issues/928)）

別の例として、乗算に関する以下のプログラムを考えます：

```c
#include <stdbool.h>
#include <stdio.h>
bool g(int x)
{
    return x * x >= 0;
}
int main()
{
    printf("%s\n", g(65535) ? "true" : "false");
}
```

`int` の幅は32ビットと仮定します。`int` 型の乗算が$2^{32}$を法とするモジュラー計算で行われるのであれば、`x = 65535` の時 `x * x` は$65536^2=4294836225$でこれは$2^{31}=2147483648$以上$2^{32}=4294967296$未満なのでwrap aroundして `-131071` となるはずです。ですが、実際には `int` の乗算のオーバーフローは未定義動作なので、コンパイラーはオーバーフローを考慮せずに最適化を行うことができます。

実際、Clangでは最適化の有無で上記のプログラムの実行結果が変わります：

```
$ clang -o test2 test2.c
$ ./test2
false
$ clang -o test2 -O2 test2.c
$ ./test2
true
```

なお、C言語の符号なし整数に関しては、足し算、引き算、掛け算では$2^n$を法とするモジュラー演算が行われることが保証されています。また、C23で追加されるオーバーフロー検査付きの演算は符号付き整数であってもwrap aroundした結果を得ることができます：

* [C言語での整数のオーバーフロー検査](https://zenn.dev/mod_poppo/articles/c-checked-int)

最後に、私が動作確認に使ったコンパイラーのバージョンは以下の通りです：

```
$ clang --version
Apple clang version 14.0.0 (clang-1400.0.29.202)
Target: arm64-apple-darwin21.6.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
```
