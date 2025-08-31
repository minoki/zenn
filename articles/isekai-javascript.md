---
title: "異世界JavaScript"
emoji: "🔖"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [javascript]
published: true
---

最近は異世界転生も珍しくなくなりました。もしも異世界にJavaScriptがあったら、それは我々の知るJavaScriptとどう違うでしょうか？

この記事では、**ECMAScriptまたはその亜種に実装された機能で、標準化に至らなかったもの**を取り上げます。モダンなECMAScriptしか知らない方には、きっと異世界を見るように新鮮に感じていただけることと思います。

## 異世界の `typeof` 演算子

「[13.5.3 The typeof Operator - ECMAScript® 2025 Language Specification](https://262.ecma-international.org/16.0/#sec-typeof-operator)」によると、`typeof` 演算子の返しうる値（文字列）は

* `undefined`
* `object`
* `string`
* `symbol`
* `boolean`
* `number`
* `bigint`
* `function`

のちょうど8通りです。しかし、**異世界のJavaScriptでは `typeof` がこれら以外の文字列を返すかもしれません**。

Microsoftが開発していたJScriptという言語があります。Windowsで使えるECMAScript 3準拠の言語です。以下の内容を、`test.js` というファイル名で保存してみます。

```js
var x = new ActiveXObject("ADODB.Stream");
x.Type = 1;
x.Open();
x.LoadFromFile("test.js");
var b = x.Read(1);
x.Close();
WScript.Echo("typeof b = " + typeof b);
```

Windows Script HostのCScriptで実行してみましょう。`typeof b` は何を返すでしょうか？

```
> CScript /nologo test.js
typeof b = unknown
```

ご覧のように、`typeof b` は `"unknown"` という文字列を返しました。

実は、昔のECMAScriptの仕様では、ホストにより定義されたオブジェクトは `typeof` で処理系依存の値を返すことが認められていたのです。JScriptが準拠しているのはせいぜいECMAScript 3ぐらいなので「[ECMA-262, 3rd edition](https://www.ecma-international.org/wp-content/uploads/ECMA-262_3rd_edition_december_1999.pdf)」を見ると、11.4.3 The `typeof` Operatorのセクションに

> | Type | Result |
> |:-|:-|
> | ... | ... |
> | Object (host) | Implementation-dependent |

という行があります。

## 参照を返す関数

引き続きJScriptネタです。

C++などの関数は参照を返すことができ、関数呼び出しが代入の左辺に来ることができます。

```cpp
std::get<0>(t) = v;
```

一方、JavaScriptでは参照を返す関数を書くことはできません。

```js
var obj = {};
function foo() {
    return obj.member;
}
foo() = "foo"; // できない
```

しかし、「JavaScriptでは書くことはできない」は、「外界（ホスト）で定義された関数も参照を返せない」ことを意味しません。次のJScriptコードをWSHで実行してみましょう：

```js
var dict = new ActiveXObject("Scripting.Dictionary");
dict.Item("foo") = "bar";
WScript.Echo(dict.Item("foo"));
```

```
> CScript /nologo dict.js
bar
```

このコードにおいて、Dictionaryオブジェクトの `Item` メソッドは**参照を返しており**、JScript側でそれに代入できています。

昔のECMAScriptには、「ホストオブジェクトが呼び出し時に参照を返せるかは処理系定義」という規定がありました。「[11.2.3 Function Calls - ECMAScript Language Specification - ECMA-262 Edition 5.1](https://262.ecma-international.org/5.1/#sec-11.2.3)」より引用します：

> NOTE The returned result will never be of type Reference if func is a native ECMAScript object. Whether calling a host object can return a value of type Reference is implementation-dependent. If a value of type Reference is returned, it must be a non-strict Property Reference.

この規定は、ECMAScript 2015で削除されました。

## catch if

JavaScriptで特定の種類のエラーを捕捉したい場合、catch節にif文を書くことが多いと思います。

```js
try {
    f();
} catch (e) {
    if (e instanceof RangeError) {
        console.log("RangeError");
    } else {
        throw e;
    }
}
```

こういう場合に、`catch` 節に直接条件を書けたら便利ではないでしょうか？

```js
try {
    f();
} catch (e if e instanceof RangeError) {
    console.log("RangeError");
}
```

同じことを考えた人はいたようで、昔のSpiderMonkeyのJavaScriptでは `catch` に直接条件を書くことができました。

* [try...catch - JavaScript | MDN](https://web.archive.org/web/20190829045039/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)（MDNのWeb Archive）

まあ、標準化されなかった機能なので、実用していたのはせいぜいMozilla界隈（Firefox拡張のコミュニティーを含む）くらいだったのではないでしょうか。

2025年現在、この機能はSpiderMonkeyからも削除されています（[1228841 - Remove support for conditional catch expressions](https://bugzilla.mozilla.org/show_bug.cgi?id=1228841)）。現代においてこの機能を試したい場合は、**Rhino**（Netscape/Mozillaによる、Java製のJavaScript処理系）を使うのが一番簡単でしょう。

```js
$ cat test.js
try {
    throw new RangeError("test");
} catch (e if e instanceof RangeError) {
    print(e.toString());
}
$ rhino test.js
RangeError: test
```

## for each

オブジェクトのキーと値を走査する場合、モダンなJavaScriptでは `Object.entries` を使うと思います。値だけが必要なら、`Object.values` です。

MozillaのJavaScriptには、後述するE4Xの一環として、オブジェクトの値を走査するための別の構文がありました。その名もfor eachです。

* [for each...in - JavaScript | MDN](https://web.archive.org/web/20190829045118/https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for_each...in)

これもSpiderMonkeyからは削除されているので、Rhinoで試すのがお手軽でしょう。

```js
$ cat foreach.js
var obj = {
    a: "Alpha",
    b: "Bravo",
    c: "Charlie",
};
for each (var v in obj) {
    print(v);
}
$ rhino foreach.js
Alpha
Bravo
Charlie
```

## let式

複雑な式を書いている時、式の中で変数を導入できたら便利ではないでしょうか？例えば、2点間の距離を計算するときに、

```js
Math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0))
```

と書くのはだるい（※）ので、`dx = x1 - x0`, `dy = y1 - y0` という変数を一時的に導入できると便利です。

※JavaScriptにはこの計算をやってくれる `Math.hypot` 関数があるので、これは人工的な例です。

関数型言語にはlet式があり、式の中で変数を導入できます。JavaScriptにもlet式があると便利ではないでしょうか？

Mozilla界隈の人もそう考えたのか、2006年にFirefox 2に実装された**JavaScript 1.7**にはlet文と並んでlet式が導入されました。「JavaScript 1.7」というのはJavaScriptのバージョンであり、ECMAScriptのバージョンとは無関係なので注意してください。

* [New in JavaScript 1.7 - JavaScript | MDN](https://web.archive.org/web/20140801203447/https://developer.mozilla.org/en-US/docs/Web/JavaScript/New_in_JavaScript/1.7#let_expressions)（MDNのWeb Archive）

let式の構文は `let(変数1 = 式1, ..., 変数n = 式n) 式` です。

これもSpiderMonkeyからは削除されているので、Rhinoで試すのが手軽でしょう。

```js
$ cat letexp.js
var x0 = 100, y0 = 200, x1 = 150, y1 = 300;
var distance = let(dx = x1 - x0, dy = y1 - y0)
  Math.sqrt(dx * dx + dy * dy);
print(distance);
print(Math.hypot(x1 - x0, y1 - y0));
$ rhino letexp.js
111.80339887498948
111.80339887498948
```

## E4X: XMLとの統合

JavaScriptソース中にXMLっぽいものを書きたい！という場合に、現代で使われるのはFacebook（現Meta）によるJSXでしょう。実は、JavaScript中にXMLを書けるようにする取り組みはゼロ年代にもありました。それがE4Xです。

この記事で取り上げている他の機能とは違い、E4Xは標準化自体はされています。

* [ECMAScript for XML - Wikipedia](https://en.wikipedia.org/wiki/ECMAScript_for_XML)
* [ECMA-357 - Ecma International](https://ecma-international.org/publications-and-standards/standards/ecma-357/)

E4XにはXMLリテラルと、要素・属性にアクセスするための仕様が含まれていました。例によってRhinoで試してみましょう：

```js
$ cat e4x.js 
var xml = <foo><bar baz="qux" /></foo>;
print(typeof xml);
print(xml.bar.@baz);
$ rhino e4x.js
xml
qux
```

`typeof` が `"xml"` を返すこと、要素に `.` でアクセスできること、属性にアクセスできる `@` という記法があることなどがわかります。

## SIMD

モダンなCPUはSIMD (Single Instruction, Multiple Data) という、一命令で複数のデータを扱う仕組みを備えています。JavaScriptからもSIMD命令にアクセスできると、なんかこう、強そうではないでしょうか？

というわけで、JavaScriptからSIMD命令にアクセスできるようにする提案がありました。活発だったのは2013年〜2015年あたりですね。

* [tc39/ecmascript_simd: SIMD numeric type for EcmaScript](https://github.com/tc39/ecmascript_simd)

JavaScriptの性質をよく知っている方は、JavaScriptからSIMDというのはいくら何でも低レベル過ぎると思うかもしれません。JavaScriptはGCを前提とし、型なしのオブジェクトがそこらを駆け回る言語で、SIMDを活用する前にローレベルなメモリアクセスをできるようにしろ、と言いたくなるかもしれません。

実際のところ、JavaScriptでネイティブコードに近いメモリアクセスを実現することは（TypedArrayを活用して）可能です。そして、それをフル活用したのがEmscriptenであり、仕様化したのが2013年ごろに登場したasm.jsです。そして、asm.jsのような状況であればJavaScriptにSIMDを入れる意義がある、ということになります。

asm.jsについては昔書いた記事「[asm.js: 仕様と実装の今](https://qiita.com/mod_poppo/items/de5c6f2f4604b84b1bc1)」で紹介しました。そこでは、SpiderMonkeyがasm.jsからSIMDを使えるようにする対応を進めていたことにも触れました。

JavaScriptのSIMD拡張では、ベクトルに対応するプリミティブ型が追加され、`typeof v` は `"floatx4"` や `"uint16x8"` のような文字列を返すことになっていたようです。

まあ、現代人からすると「そういう低レベルな話はWebAssemblyでやれ」って思いますよね。実際そんな感じで、JavaScriptにSIMDを入れる話は終息したようです（WebAssemblyが発表されたのは2015年）。

## JScript.NET

JScript.NETはMicrosoftが開発した言語で、JScriptの.NET版みたいなやつです。.NETの黎明期に登場しました。コンパイル型です。詳しくはWikipediaとかを見てください：

* [JScript .NET - Wikipedia](https://en.wikipedia.org/wiki/JScript_.NET)

公式ドキュメントはこの辺になるのでしょうか：

* [JScript | Microsoft Learn](https://learn.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/72bd815a%28v=vs.100%29)

以下のページにHello worldが載っています。

* [特集：.NET Framework SDKで始める.NETプログラミング（前編）　６．Hello World展覧会（3）－ VB.NET,JScript.NET－ - ＠IT](https://atmarkit.itmedia.co.jp/fdotnet/special/dotnet_sdk/dotnetsdk07.html)

Hello worldだけではつまらないので、ここではクラスや型注釈を使ってみます。

```js
import System;
Console.WriteLine("Hello world!");

class Foo {
    var name: String;
    function Foo(myname: String) {
        // constructor
        this.name = myname;
    }
    function greet(name: String) {
        Console.WriteLine("Hello, " + name + "-san! I'm " + this.name + ".");
    }
}
var foo = new Foo("John Doe");
foo.greet("Nanashi Gonbe");
```

実行環境ですが、Windowsを持っている方は `C:\Windows\Microsoft.NET` 以下のどこかに `jsc.exe` というプログラムがあると思うので探してください。`jsc.exe` がJScript.NETのコンパイラーで、`.js` ファイルを実行ファイル `.exe` に変換できます。

```
> C:\Windows\Microsoft.NET\Framework64\v4.0.30319\jsc dotnet.js
Microsoft(R) JScript Compiler version 14.00.9032
for Microsoft(R) .NET Framework version 4.0.30319
Copyright (C) Microsoft Corporation. All rights reserved.

> .\dotnet.exe
Hello world!
Hello, Nanashi Gonbe-san! I'm John Doe.
```

雰囲気がわかりましたか？クラス構文、型注釈などはその後登場したECMAScript 2015やTypeScriptを彷彿とさせますが、よく見ると違いますね。この場合は「ECMAScript 4」の名前を出すのがより適切なのだと思いますが。

JScript.NETは、正直言ってあまり使われていないと思います。C系の文法を持つ言語を.NETで使いたかったらC#でいいじゃん、って思いますよね。でも、知名度の低いプログラミング言語がWindowsの片隅にひっそりと眠っているというのは、ロマンを感じませんか？

## その他

ActionScriptもECMAScriptベースなので、割と「異世界のJavaScript」感があるのではないかと思っていますが、筆者はActionScriptの経験がないので紹介できません。

## 裏テーマ：typeofの結果が増えないと仮定することは安全か

数日前に「esbuildが `typeof x === "undefined"` を `typeof x>"u"` にminifyする」というツイートを見ました。

https://x.com/uhyo_/status/1961272105183711278

ツイート主が言っているように、この変換は `typeof` の結果としてありうる文字列が増えた場合に安全ではありません。果たして、`typeof` の結果としてuvwxyzなどから始まる文字列が増えることはあり得るでしょうか？

この記事を読んだあなたなら、「少なくとも、過去にはあった（あるいは提案されていた）」と答えられるでしょう。それも3つも。

* JScript: `typeof x` が `"unknown"` を返す可能性がある。
* E4X: `typeof x` が `"xml"` を返す可能性がある。
* SIMD.js: `typeof x` が `"uint16x8"` を返す可能性がある。

このように、過去にあったのだから、将来再びそういう提案がなされても不思議ではありません。

したがって、私としては「`typeof x === "undefined"` を `typeof x>"u"` に変換するのは、**JavaScriptの過去や未来の可能性を想定しない、近視眼的な発想である**」と評価します。まあ、`typeof` が新しい結果を返すようになるのはJavaScriptの新機能を使うときで、その場合にはesbuildも改修されているだろうから、実際にはそこまで問題にはならないかもしれません。どうかな。私は不安です。
