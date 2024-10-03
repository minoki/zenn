---
title: "C言語において浮動小数点数の足し算は可換か：FMAがある場合"
emoji: "😺"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [c, c言語, 浮動小数点数]
published: true
---

以前こういう記事を書きました：

* [浮動小数点数の足し算と掛け算は可換か](https://qiita.com/mod_poppo/items/46d2d3c8eef1d8de1534)

要約すると「浮動小数点数の足し算と掛け算はNaNのビットパターンを考慮しない限り可換だよ」という内容です。

プリミティブな足し算としてはそうなのですが、C言語は `a * b + t` の形の式をFMA `fma(a, b, t)` に変換することを許容しています（この話は「[浮動小数点演算の結果が環境依存なのはどんなときか](./floating-point-portability)」でちらっと触れました）。このような変換が存在しても足し算は「可換である」と言えるでしょうか？

つまり、`a * b + c * d` という式は `fma(a, b, c * d)` にコンパイルされる可能性もあれば、`fma(c, d, a * b)` にコンパイルされる可能性もあるため、ソースコード上で左右を入れ替えると違う結果が出るのではないか？という話です。

## 実証コード

この挙動を確認するコードを以下に用意します：

```c
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// GCCはデフォルトでアグレッシブにFMAへの変換を行うのでこの#pragmaは不要（というか対応してない）
// 以前のClangはFMAへの変換には積極的ではなかったので、#pragmaをつけておく
#pragma STDC FP_CONTRACT ON

__attribute__((noinline))
double f(double a, double b, double c, double d)
{
    return a * b + c * d;
}

__attribute__((noinline))
double g(double a, double b, double c, double d)
{
    return c * d + a * b;
}

int main(int argc, char *argv[])
{
    if (argc >= 5) {
        double a = strtod(argv[1], NULL);
        double b = strtod(argv[2], NULL);
        double c = strtod(argv[3], NULL);
        double d = strtod(argv[4], NULL);
        double r = f(a, b, c, d);
        double s = g(a, b, c, d);
        printf("a * b + c * d = %.17g (%a)\n", r, r);
        printf("c * d + a * b = %.17g (%a)\n", s, s);
    } else {
        fprintf(stderr, "Usage: %s a b c d\n", argv[0]);
    }
}
```

`f` と `g` は、数式だけ見れば等価な関数であるように見えます。しかし実は違う可能性がある、というのがこの記事のテーマです。

このコードをコンパイルして「非可換性」を観測するためには、「FMA命令が利用可能であること」「コンパイラーが `a * b + t` を積極的にFMA命令へ変換すること」の2点が必要です。

x86ではFMA命令はデフォルトでは仮定できないため、当然「積極的なFMA命令への変換」も行われません。x86でGCCやClangなどのコンパイラーに「FMA命令を仮定して良い」ことを伝えるには `-mfma` オプションを使います。

AArch64では最初からFMA命令が利用可能なため、`-mfma` のようなオプションは必要ありません。

「FMA命令が利用できる状況でコンパイラーが積和をFMA命令に変換するか」はコンパイラーの方針によります。GCCは昔から「積極的にFMAに変換する」という方針でした。Clangは昔はそういう変換に積極的ではありませんでしたが、Clang 14以降は積極的に変換するようになったようです（[リリースノート](https://releases.llvm.org/14.0.0/tools/clang/docs/ReleaseNotes.html#floating-point-support-in-clang)）。

こういう実験をする上での工夫として、コンパイラーの最適化を阻害することが有益だったりします。まず、定数畳み込みを避けるために、入力となる数値はリテラルではなくてコマンドライン引数で与えることにしました。文字列からの変換というステップが挟まるのでlibcを信用できない場合はアレですが、今回実験に使う環境では大丈夫だと信じましょう。

次に、インライン化が走るとコンパイラーが「`a * b + c * d` と `c * d + a * b` は等価だ」と思って共通部分式削除の最適化が走る可能性があります。なので、関数に `noinline` 属性をつけてインライン化を阻害します。GNU拡張を使いたくない場合はファイルを分割すると良いでしょう。

では実験です。

## 実験

では実験してみましょう。入力としては、`a = 0.1, b = 3.0, c = 3.0, d = 0.4` を与えます。

x86_64の場合：

```
$ uname -m
x86_64
$ gcc -O2 -mfma -o add-and-fma add-and-fma.c
$ ./add-and-fma 0.1 3.0 3.0 0.4
a * b + c * d = 1.5000000000000002 (0x1.8000000000001p+0)
c * d + a * b = 1.5 (0x1.8p+0)
```

AArch64の場合：

```
$ uname -m
aarch64
$ gcc -O2 -o add-and-fma add-and-fma.c
$ ./add-and-fma 0.1 3.0 3.0 0.4
a * b + c * d = 1.5000000000000002 (0x1.8000000000001p+0)
c * d + a * b = 1.5 (0x1.8p+0)
```

確かに、足し算の左右を入れ替えると計算結果が変わることを確認できました。

気になる方はコンパイラーが出力したアセンブリコードも読んでみてください。

## おまけ：実験に使うパラメーターを生成する方法

「浮動小数点数の演算について法則が成り立たない」（結合法則など）という状況はよくあります。この記事では、`fma(a, b, c * d) == fma(c, d, a * b)` という式が成り立たないことを利用しました。しかし、式が成り立たないことは薄々わかっても、反例を作るのは意外と面倒だったりします。

面倒なことはプログラムにやらせましょう。ここでは、HaskellのQuickCheckというproperty-based testingのライブラリーを使います。これは本来は「性質が成り立つ」ことを検証する目的で使いますが、性質に反例が見つかった場合はそれを表示してくれます。しかも、人間にとって複雑すぎない形に「縮小」してくれます。

テストにはFMAを使いますが、Haskellの標準ライブラリーにFMAの関数はないので、ここでは私が作っているfp-ieeeというパッケージを使います。

こういう書き捨てのHaskellコードにプロジェクトを作るのは大袈裟なので、「[プロジェクトを作らずにHaskellをやる](./haskell-without-project)」で紹介したcabal scriptを使います。

```haskell
{- cabal:
build-depends: base, QuickCheck, fp-ieee
-}
import Test.QuickCheck
import Numeric.Floating.IEEE

prop :: Double -> Double -> Double -> Double -> Property
prop a b c d = fusedMultiplyAdd a b (c * d) === fusedMultiplyAdd c d (a * b)

main :: IO ()
main = quickCheck prop
```

実行例：

```
$ cabal run FMACommute.hs
*** Failed! Falsified (after 15 tests and 12 shrinks):    
0.1
1.0
3.0
0.3
0.9999999999999999 /= 1.0
```

無事に反例を作ってくれました。

QuickCheckで浮動小数点数の反例を作る話はまた別に記事を書くかもしれません。
