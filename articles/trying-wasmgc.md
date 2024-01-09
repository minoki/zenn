---
title: "WasmGCを試す"
emoji: "🎉"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [WebAssembly, Wasm]
published: true
---

去る2023年は、WebAssemblyにGCを入れる仕様、WasmGCがChromeとFirefoxに実装された記念すべき年でした。WasmGCはコンパイラ作成者の長年の悲願であり（筆者の脳内調べ）、GCに依存する言語をWebAssembly上で動かすことを容易にします。2024年は、WasmGCがより広い環境で使えるようになることでしょう。まさにWasmGC元年と言っても過言ではありません（元年はなんぼあってもいいですからね）。

この記事は、2024年1月時点でのWasmGCの現状を、コンパイラ作成者の観点から調査したものです。筆者自身はWebAssemblyについては素人で、調べながら書いています。

## 概要と仕様

WasmGCの概要はChrome/V8界隈の人が書いた次の記事が参考になるでしょう：

* [A new way to bring garbage collected programming languages efficiently to WebAssembly · V8](https://v8.dev/blog/wasm-gc-porting)
    * kripken氏の記事
* [WebAssembly Garbage Collection (WasmGC) now enabled by default in Chrome  |  Blog  |  Chrome for Developers](https://developer.chrome.com/blog/wasmgc?hl=en)
    * 一般向けの記事っぽい（概要のみ）

従来のWasmは「Assembly」と言うだけあってローレベルな代物でしたが、WasmGCは完全に（JVMとかから連想される）ハイレベルなVMの領域に踏み込んでいる感じがします。もはや「Assembly」じゃないのでは？

日本語でもいくつか記事が出ています：

* [WasmGCについて予習する](https://zenn.dev/askua/articles/afe3a3b43b82cb)
* [WasmGCで導入される型や命令のお勉強](https://zenn.dev/tanishiking/articles/learn-wasm-gc)
* [Kotlin/Wasmが生成するWasmGCコードを眺める](https://zenn.dev/tanishiking/articles/2023-12-kotlin-wasm-mapping)
    * 実際のコード片はためになる。

もちろん、仕様が一番大事です。仕様はこの辺で見れます：

* [WebAssembly Specifications](https://webassembly.github.io/gc/)
* [gc/proposals/gc/MVP.md at main · WebAssembly/gc](https://github.com/WebAssembly/gc/blob/main/proposals/gc/MVP.md)
    * 同じディレクトリにあるOverview.mdはやや古そう。

## 対応状況

各種ランタイムのWebAssemblyの拡張機能の対応状況は

* [Feature Extensions - WebAssembly](https://webassembly.org/features/)

にまとまっています。この記事を書いている時点で、筆者が調べた限りの対応状況を以下に示します：

* Chrome 119以降
* Firefox 120以降
* Safari 未対応
* Wasmtime 未対応
    * [Tiers of support - Wasmtime](https://docs.wasmtime.dev/stability-tiers.html)
* Wasmer 未対応
    * [gc support · Issue #357 · wasmerio/wasmer](https://github.com/wasmerio/wasmer/issues/357)
* Node.js v21.5.0 `--experimental-wasm-gc` フラグ付き
* Deno v1.39.1/v8 12.0.267.8 対応？
* Bun v1.0.21 未対応

Node.jsはフラグをつけると使えることになっていますが、実際に試すとバリデーションかどこかでエラーが出ました。Denoは対応は明示されていないようですが、実際に試すと動きました。

WasmtimeとWasmerが未対応だと「WasmGCはJavaScript処理系をすでに持ってるやつしか実装しないのでは？」という疑念が育ってきます。独立系（？）WasmランタイムもGC対応する日が来ると良いですね。

## ツールチェイン

コンパイラを作る上では、コンパイラが直接機械語を出力することは少なく、そういうのはアセンブラに任せることが多いです（JITコンパイラは例外）。同様に、WebAssemblyをターゲットとする場合もテキスト形式（WAT）からバイナリ形式に変換する部分は外注したいかもしれません。そうでなくても、デバッグの際にはバイナリ形式からテキスト形式に変換するツールがあると便利でしょう。あるいは、あれこれ実験する際はWATを書いて変換して実行という形になるでしょう。

というわけで、その辺をやってくれそうなツールをいくつか探しました。

* [WebAssembly/wabt: The WebAssembly Binary Toolkit](https://github.com/WebAssembly/wabt)
    * GC proposalにちゃんと対応してない感じがする。というかwasm2cしか息してない？
* [WebAssembly/binaryen: Optimizer and compiler/toolchain library for WebAssembly](https://github.com/WebAssembly/binaryen)
    * 良さそう。`wasm-opt`, `wasm-as`, `wasm-dis` など。
    * `wasm-as` の入力はWATのサブセットっぽい。
* [WebAssembly/gc: Branch of the spec repo scoped to discussion of GC integration in WebAssembly](https://github.com/WebAssembly/gc)
    * リファレンス実装。WATからWasmへの変換ができそう。
    * OCamlで書かれている。コンパイラ開発者ならOCamlぐらい入ってるよね？

## 簡単な例：i32のボックス化

まずは `i32` 一個からなるstructを作ってみましょう。パラメーター多相を持つ言語の実装の方式の一つに、こういう風にプリミティブをボックス化するやつがありますね。

```
;; test.wat
;; コメントはセミコロン2つから始める
(module
  (type $boxed-i32 (struct (field i32)))

  (func (export "make")
    (param $i i32)
    (result (ref $boxed-i32))
    (struct.new $boxed-i32 (local.get $i))
    ;; wasm-asは
    ;;   local.get $i
    ;;   struct.new $boxed-i32
    ;; みたいな書き方は受け付けないっぽい
  )
  (func (export "get")
    (param $o (ref $boxed-i32))
    (result i32)
    (struct.get $boxed-i32 0 (local.get $o))
  )
)
```

これを `test.wat` として保存して、アセンブルします。

```sh-session
$ # リファレンス実装を使う場合
$ wasm -d test.wat -o test.wasm
$ # binaryenを使う場合
$ wasm-as --enable-gc --enable-reference-types test.wat
```

それを呼び出すJavaScriptコードを次のように用意します。

```js
// run.mjs
import { readFileSync } from "node:fs";
const wasmBuffer = readFileSync("test.wasm");
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const { make, get } = wasmModule.instance.exports;
const o = make(42);
console.log(o, get(o));
```

Node.jsで `node --experimental-wasm-gc --experimental-wasm-stringref run.mjs` ではうまくいかなかったので、Denoで実行します。

```sh-session
$ deno --version
deno 1.39.1 (release, aarch64-apple-darwin)
v8 12.0.267.8
typescript 5.3.3
$ deno run --allow-read run.mjs
[Object: null prototype] {} 42
```

良さそうですね。

## クロージャーの例

高級言語に必要な言語機能と言えば何でしょうか？そう、クロージャーですね。

クロージャーの実装は、Cっぽい言語で書けば次のようになります。

```c
// int -> int みたいな型に対応する実装
typedef int (*CODE)(struct Closure *c, int);
struct Closure {
    CODE code;
    void *payload[]; // 仮に void * としたが、実際には関数の自由変数に応じた型と個数が並ぶ
};
```

つまり、関数ポインタと環境の組ですね。呼び出す時は第一引数としてクロージャー自身を渡します。

ここでは `int -> int` っぽい関数を考えます。Wasmでの型の定義と、呼び出す処理は次のように書けます：

```
(module
  (rec
    (type $code (func (param (ref $closure) i32) (result i32)))
    ;; (sub ...) はsubtypeを作れるようにするマーカー。これがないとfinal扱いとなる
    (type $closure (sub (struct (field (ref $code)))))
  )

  (func (export "call")
    (param $f (ref $closure))
    (param $x i32)
    (result i32)
    (call_ref $code
      (local.get $f)
      (local.get $x)
      (struct.get $closure 0 ;; get f.code
        (local.get $f)
      )
    )
  )
  ;; 続く
```

クロージャーを返す関数も作ってみましょう。ここでは、足し算をカリー化したやつ（TypeScriptで言う `x => y => x + y`）を実装してみます。

```
  ;; 続き

  ;; subtypeを作る際はsupertypeのフィールドも再び列挙する
  (type $closure-with-i32 (sub final $closure (struct (field (ref $code)) (field i32))))

  (func $adder-impl
    (type $code)
    (param $closure (ref $closure))
    (param $y i32)
    (result i32)
    (i32.add
      (struct.get $closure-with-i32 1 ;; get env.payload
        (ref.cast (ref $closure-with-i32)
          (local.get $closure)
        )
      )
      (local.get $y)
    )
  )

  ;; 関数への参照を取れるようにするおまじない（よくわかってない）
  (elem (ref null $code) (ref.func $adder-impl))

  (func (export "adder")
    (param $x i32)
    (result (ref $closure-with-i32))
    (struct.new $closure-with-i32
      (ref.func $adder-impl)
      (local.get $x)
    )
  )
)
```

JavaScriptから呼び出すコードも用意します。

```js
import { readFileSync } from "node:fs";
const wasmBuffer = readFileSync("closure.wasm");
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const { call, adder } = wasmModule.instance.exports;
const a = adder(42);
console.log(a, call(a, 37));
```

```sh-session
$ wasm-as --enable-gc --enable-reference-types closure.wat         
$ deno run --allow-read run-closure.mjs
[Object: null prototype] {} 79
```

動きましたね。

## 直和型

高級言語に必要な言語機能と言えば何でしょうか？そう、直和型ですね。

まず、option型

```sml
datatype 'a option = NONE | SOME of 'a
```

はこんな感じになるでしょうか：

```
(type $option (sub (struct (field $tag i32))))
(type $option.NONE (sub final $option (struct (field $tag i32))))
(type $option.SOME (sub final $option (struct (field $tag i32) (field $payload anyref))))
```

あるいはnullを利用する手もあるかもしれません。

汎用的な直和型

```sml
datatype ('a,'b) either = LEFT of 'a | RIGHT of 'b
```

はこんな感じでしょうか：

```
(type $either (sub (struct (field $tag i32))))
(type $either.LEFT (sub final $either (struct (field $tag i32) (field $payload anyref))))
(type $either.RIGHT (sub final $either (struct (field $tag i32) (field $payload anyref))))
```

## 雑感

WasmGCはWasm MVPと違ってそれなりの型システムがあるので、コンパイラ作成者から見たらだいぶ違うターゲットに感じます。

現状、WasmGCに対応している本格的なランタイムは、JavaScript処理系だけのようです。すると、JavaScriptをコンパイルターゲットとする場合に比べて、WasmGCに対応するメリットはどのくらいあるのか気になるところです。パースが短時間で済むとか、JITコンパイルの立ち上がりが早くなるとか、`i64` をカジュアルに使えるとか、SIMDとか、そういうのになるんでしょうか。

WasmGCで初めてWasmに興味が湧いてきたコンパイラ作成者のためのチュートリアルが欲しいです。

私が作っているStandard ML処理系「LunarML」はこれまでにLuaとJavaScriptバックエンドを実装しましたが、WasmGCへの対応も検討しています。データ型のコンパイルで問題になるのは構造的レコードかと思っています。WasmGCの制約の中でなんとかしなければなりません。

* [minoki/LunarML: The Standard ML compiler that produces Lua/JavaScript](https://github.com/minoki/LunarML)
* [LunarMLのWasmGCバックエンド検討メモ](https://zenn.dev/mod_poppo/scraps/504bfff75d8821)
