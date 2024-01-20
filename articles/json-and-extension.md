---
title: "JSONの小ネタと、JSONに対する拡張"
emoji: "🌊"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [json]
published: true
---

JSONは最も普及したデータ形式の一つでしょう。JSONの仕様はECMA-404やRFC 8259として標準化されています。

* [ECMA-404 - Ecma International](https://ecma-international.org/publications-and-standards/standards/ecma-404/)
* [RFC 8259 - The JavaScript Object Notation (JSON) Data Interchange Format](https://datatracker.ietf.org/doc/html/rfc8259)

## 細かいネタ

### 文字コード

RFC 8259では、外部と交換するJSONテキストはUTF-8でエンコードされなければならないということになっています。BOMは禁止です。昔のRFCではUTF-16やUTF-32も許容されていました。

ただし、キーや文字列の中身としては単独のサロゲート（例：`"\uD800"`）は禁止されていません。JSONのキーや文字列はUnicodeスカラー値の列とは限らないのです。

UTF-8を前提とする実装にとっては、単独のサロゲート（不正なUTF-16）は悩ましい問題です。[WTF-8](https://simonsapin.github.io/wtf-8/)のような形で無理やりエンコードするか、エラーを出すか、となるでしょう。

Swiftの `JSONDecoder` はサロゲートの片割れに対してエラーを出すようです。

RubyのJSONモジュールは割と一貫性のない動作をするようです：

```ruby
$ irb3.3
irb(main):001> require 'json'
=> true
irb(main):002> JSON.parse("\"\\uD800\"")
/opt/local/lib/ruby3.3/3.3.0/json/common.rb:219:in `parse': incomplete surrogate pair at '\uD800"' (JSON::ParserError)
	from /opt/local/lib/ruby3.3/3.3.0/json/common.rb:219:in `parse'
	from (irb):2:in `<main>'
	from <internal:kernel>:187:in `loop'
	from /opt/local/lib/ruby3.3/gems/3.3.0/gems/irb-1.11.0/exe/irb:9:in `<top (required)>'
	from /opt/local/bin/irb3.3:25:in `load'
	from /opt/local/bin/irb3.3:25:in `<main>'
irb(main):003> JSON.parse("\"\\uD800xxxxxx\"")
=> "?xxxxx"
irb(main):004> JSON.parse("\"\\uDFFF\"")
=> "\xED\xBF\xBF"
```

### スラッシュのエスケープ

JSONの文字列で使えるエスケープは次の9種類です：

* `\"`
* `\\`
* `\/`
* `\b`
* `\f`
* `\n`
* `\r`
* `\t`
* `\uXXXX`

フォワードスラッシュ `/` がエスケープできるようになっているのは、文字列中に `</script>` が現れるのを回避できるようにするためのようです。実際、PHPの `json_encode` はデフォルトでフォワードスラッシュをエスケープするようです。

というわけで、JSONエンコーダーを作る場合はフォワードスラッシュをエスケープするオプションを作ると良いのかもしれません。

### ゼロの符号

JavaScriptは普通の整数も小数も浮動小数点数で表現しますが、他の言語のJSON実装では整数と浮動小数点数が区別されていることもあるでしょう。そういう場合、「小数点か指数部が含まれていれば浮動小数点数、いずれも含まれなければ整数」とするのが普通だと思います。

ここで注意したいのが負のゼロ `-0` です。これは小数点も指数部も含まないので整数として扱われるべきでしょうか？それとも整数ではゼロの符号は扱えないので、情報のロスを避けるために浮動小数点数として扱われるべきでしょうか？

いくつかの実装を試した感じでは、`-0` は `0` と同じ値にデコードされるケースが見受けられました。

```python
$ python3
Python 3.11.7 (main, Dec 18 2023, 10:52:21) [Clang 15.0.0 (clang-1500.1.0.2.5)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import json
>>> json.loads("[0,-0,0.0,-0.0]")
[0, 0, 0.0, -0.0]
```

```ruby
$ irb3.3
irb(main):001> require 'json'
=> true
irb(main):002> JSON.parse("[0,-0,0.0,-0.0]")
=> [0, 0, 0.0, -0.0]
irb(main):003> 
```

実装する際の方針としては、浮動小数点数の負のゼロをエンコードする際は必ず小数点を含めるようにして、デコード時は `-0` は整数とみなす、あたりが良さそうでしょうか。

## 拡張の例

素のJSONは最低限という感じがします。拡張するならどういう記法があると便利でしょうか。

### 特殊な数：`NaN`, `Infinity`, `-Infinity`

JSONではNaNや無限大などの特殊な数を表現できません。これはデータ交換に不便なので、いくつかの実装は `NaN`, `Infinity`, `-Infinity` を許容しています。

例えば、Pythonの `json` モジュールはデフォルトでNaNや無限大を出力します。

```python
$ python3
Python 3.11.7 (main, Dec 18 2023, 10:52:21) [Clang 15.0.0 (clang-1500.1.0.2.5)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> import json
>>> json.dumps(float('nan'))
'NaN'
>>> json.dumps(float('inf'))
'Infinity'
>>> json.dumps(float('-inf'))
'-Infinity'
```

Rubyはデフォルトではエラーを出しますが、`allow_nan` オプションを指定するとNaNや無限大を出力します。

```ruby
$ irb3.3
irb(main):001> require 'json'
=> true
irb(main):002> JSON.generate(Float::NAN)
/opt/local/lib/ruby3.3/3.3.0/json/common.rb:305:in `generate': NaN not allowed in JSON (JSON::GeneratorError)
	from /opt/local/lib/ruby3.3/3.3.0/json/common.rb:305:in `generate'
	from (irb):2:in `<main>'
	from <internal:kernel>:187:in `loop'
	from /opt/local/lib/ruby3.3/gems/3.3.0/gems/irb-1.11.0/exe/irb:9:in `<top (required)>'
	from /opt/local/bin/irb3.3:25:in `load'
	from /opt/local/bin/irb3.3:25:in `<main>'
irb(main):003> JSON.generate(Float::NAN, allow_nan: true)
=> "NaN"
irb(main):004> JSON.generate(Float::INFINITY, allow_nan: true)
=> "Infinity"
irb(main):005> JSON.generate(-Float::INFINITY, allow_nan: true)
=> "-Infinity"
```

.NETの `System.Text.Json.Serialization` ではオプション次第でNaNや無限大を扱えるようです。

* [JsonNumberHandling Enum (System.Text.Json.Serialization) | Microsoft Learn](https://learn.microsoft.com/en-us/dotnet/api/system.text.json.serialization.jsonnumberhandling?view=net-8.0)

一方、JavaScriptの `JSON` はNaNや無限大には対応しておらず、黙って `null` を出力します。

```js
$ node
Welcome to Node.js v21.5.0.
Type ".help" for more information.
> JSON.stringify(NaN)
'null'
> JSON.stringify(Infinity)
'null'
```

### コメント

JSONを手書きする場合、コメントを書けると便利です。

拡張として対応する場合、JavaScriptと同様の行コメント `//` と ブロックコメント `/* */` を採用するのが自然でしょう。

### 末尾カンマ

データを複数行にまたがって書く場合、

```
[
    1,
    2,
    3,
]
```

のように末尾にカンマを書けると便利そうです。

## JSON5

JSONのスーパーセットは[Wikipedia](https://en.wikipedia.org/wiki/JSON#Supersets)にもいくつか書かれていますが、その中ではJSON5というやつが有力そうです。

* [JSON5 – JSON for Humans | JSON5](https://json5.org/)

拡張の概要は以下の通りです：

* 末尾カンマ（オブジェクトと配列）
* キーはES 5.1のIdentifierNameであればクォート不要
* 文字列
    * シングルクォートによる文字列
    * 行末の `\` による複数行文字列
    * 任意の文字をエスケープできる
* 数値
    * 整数の十六進表記
    * 小数点から始まる数値、小数点で終わる数値
    * NaNと無限大
    * プラスから始まる数値
* コメント
* 追加の空白文字

JSONと同様、ECMAScript（ES2019以降）のサブセットとなっています。

JSON5にない記法の例ですが、`\u{}` の形のエスケープシーケンスはなさそうです。

拡張子は `.json` か `.json5` っぽいです。MIMEタイプは `application/json5` が一部で使われているようです。

* [mime type · Issue #40 · json5/json5](https://github.com/json5/json5/issues/40)
* [Clarify Mime type. · Issue #80 · json5/json5](https://github.com/json5/json5/issues/80)

Swiftの `JSONDecoder` はオプションでJSON5に対応しています。

* [allowsJSON5 | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/jsondecoder/3766916-allowsjson5)

```swift
import Foundation

struct Foo: Codable {
    let a: Double
    let b: Double
    let c: [Double]
}

let decoder = JSONDecoder()
decoder.allowsJSON5 = true
do {
    let json = "{a: NaN, b: -Infinity, c: [1,2,3,]}".data(using: .utf8)!
    let result = try decoder.decode(Foo.self, from: json)
    print(result) // Foo(a: nan, b: -inf, c: [1.0, 2.0, 3.0])
} catch {
    print(error)
}
```

最近のSQLiteのJSON処理関数もJSON5を処理できるようです。IdentifierNameの検証は真面目にやると大変なので、ASCII外の文字はホワイトスペースを除きすべてアルファベットとして扱うようです。

* [JSON Functions And Operators](https://www.sqlite.org/json1.html)

独自にJSON5のパーサーを実装する場合、IdentifierNameの検証にUnicodeデータベースが必要というのが課題となるでしょう。標準ライブラリーにUnicodeデータベースがある言語の場合は良いのですが。IdentifierName以外にも、ホワイトスペースとしてUnicodeのZsカテゴリーの文字が出現して良いことになっていますが、こちらは該当する文字数が少ないので問題なさそうです。
