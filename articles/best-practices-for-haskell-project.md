---
title: "Haskellプロジェクトのベストプラクティス"
emoji: "🐈"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: true
---

Haskellプロジェクトの「良い習慣」と考えられるやつをまとめてみます。あくまで私の個人的な意見です。

## プロジェクト固有のPrelude

`Prelude` に相当するモジュールをプロジェクト独自に持っておくと便利ではないか、という話をします。代替Preludeの話ではありません。

### プロジェクト固有のPreludeがあると便利な理由

理由の一つは、標準 `Prelude` の変化です。直近では次のような変化がありました：

* GHC 9.4: `~` 型演算子が追加（これまでは構文だった）
* GHC 9.6: `liftA2` が追加
* GHC 9.10（見込み）: `foldl'` が追加

もっと昔に遡ると、`Semigroup((<>))` が増えるやつなどがありました。

この帰結として、

* 新しいGHCで名前の衝突が起きやすくなる
* 新しいGHCで「冗長なインポート」の警告が出やすくなる

ことが言えます。これらの問題を避ける（避けやすくする）ために、標準の `Prelude` をラップしたプロジェクト固有の `Prelude` を用意することが考えられます。

別の理由は、デバッグの都合です。`Prelude` には部分関数が多く含まれます。`head` や `tail` はあまりにも有名ですが、そのほかにも `(^)` みたいなやつもあります。

汎用的な関数でエラーが起きた際に原因箇所を特定するのに役立つのが `HasCallStack` です。これは自分で用意した関数に対しては使えますが、標準ライブラリーの関数には使えません。`head` と `tail` にはGHC 9.4 (base-4.17) で `HasCallStack` がついたので良いのですが、 `(^)` にはついていないままです。

プロジェクト固有の `Prelude` があると、デバッグの際に独自 `Prelude` で一時的に `HasCallStack` 適用済みの定義に差し替えることができて便利ではないでしょうか。

あとは、頻繁に使う名前をプロジェクト固有の `Prelude` に追加しておくと便利かもしれません。

最近、`Int8` とか `Word64` みたいなサイズ指定整数型を `Prelude` からエクスポートしようという提案がありました（却下されましたが）。

* [Expose sized integer types from Prelude · Issue #156 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/156)

また、この記事を執筆している時点では、 `Data.Kind.Type` を `Prelude` に追加しようという議論が行われています。

* [Expose Type from Prelude · Issue #193 · haskell/core-libraries-committee](https://github.com/haskell/core-libraries-committee/issues/193)

議論の結果がどう転ぶとしても、サイズ指定整数型や `Type` カインドを毎回 `import` するのは面倒だというプロジェクトはそれなりにありそうなので、そういうプロジェクトであれば独自 `Prelude` を用意してそれらの名前をデフォルトで使えるようにするのはアリでしょう。

### プロジェクト固有のPreludeの用意の仕方

最小の独自 `Prelude` は次の通りです：

```haskell
-- MyPrelude.hs
module MyPrelude (module Prelude) where
import Prelude
```

使用する側は、愚直にやるなら次のようになるでしょう：

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
import MyPrelude
```

`NoImplicitPrelude` は `.cabal` の `default-extensions` に書けばモジュールごとに書く必要はなくなります。

`import MyPrelude` を書くのすら面倒だという場合は、独自 `Prelude` を `Prelude` というモジュール名で用意すると良いでしょう。その場合、GHCが独自 `Prelude` を勝手に `import` してくれます。独自 `Prelude` では `PackageImports` 拡張を使って次のように書く必要があります：

```haskell
-- Prelude.hs
{-# LANGUAGE PackageImports #-}
module Prelude (module Prelude) where
import "base" Prelude
```

参考：

* [6.2.10.1. Custom Prelude modules named Prelude](https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/exts/rebindable_syntax.html#custom-prelude-modules-named-prelude)

### 実例

「プロジェクト固有の `Prelude`」を実践している例としては、llvm-hs-pure/llvm-hsパッケージがあります。これはApplicative-MonadとかSemigroup-Monoidの変化の前後で一貫した `Prelude` を使いたい、というモチベーションから実践しているようです。

* [LLVM.Prelude](https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-Prelude.html)

これは `NoImplicitPrelude` 拡張を `default-extensions` で有効にして、他のモジュールでは `import LLVM.Prelude` を書いているようです。llvm-hs-pureパッケージで定義した独自 `Prelude` をllvm-hsパッケージでも使うために、`LLVM.Prelude` は公開モジュールとなっています。

拙作fp-ieeeパッケージでも独自 `Prelude` を用意しています。とは言っても、標準の `Prelude` を再エクスポートしているだけですが。

* [MyPrelude.hs](https://github.com/minoki/haskell-floating-point/blob/c07327376316b5a0ba852451b10d0d6047a1deb4/fp-ieee/src/MyPrelude.hs)

## default-extensionsでGHC2021を模倣する

`GHC2021`、使っていますか？個人的には `PolyKinds` を除けば `GHC2021` は良いものだと思います。

`GHC2021` はGHC 9.2以降の比較的新しい機能なので、古いGHCも気にする汎用的なライブラリーでは使いにくいかもしれません。

そういう場合でも、`.cabal` の `default-extensions` をガンガン使って `GHC2021` に近い機能をパッケージ内ではデフォルトで使えるようにすると良いかもしれません。`default-extensions` に拡張を追加する基準として `GHC2021` を使うわけですね。

これを実際にやっているパッケージとして、Google ResearchのDex言語があります：

* [dex.cabal](https://github.com/google-research/dex-lang/blob/3cbde4c396a029873d7fd0a8b6aa0a65465b96d1/dex.cabal)

## Stackユーザー向け：`.cabal` ファイルをgitで管理する

Stackの特徴の一つが、`package.yaml` でプロジェクトを記述できることです（正確には、そういう機能を持ったHpackというツールを統合していることです）。

Stackはビルド時に自動で `package.yaml` から `.cabal` ファイルを生成するわけですが、生成された `.cabal` ファイルはgitリポジトリに追加するべきでしょうか？

昔はともかく、今は「追加するべき」というのが公式の見解です。

* [Should I check-in automatically generated Cabal files?](https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/#should-i-check-in-automatically-generated-cabal-files)

Cabalユーザー的には、Hackageにリリースされていないgitのバージョンを `source-repository-package` で参照して使うには `.cabal` がGitに入っている必要がある、という動機もあります。

## 開発時・CI時に有効にすると良いかもしれないオプション

開発時やCIを回すときは、なるべくバグを発見しやすくなるようなビルドオプションを指定すると良いでしょう。そういうオプションをいくつか挙げます：

* `-dlint` (GHC 9.4以降): `-dcore-lint -dstg-lint -dcmm-lint -dasm-lint -fllvm-fill-undef-with-garbage -debug`
* `-fcheck-prim-bounds` (GHC 9.2以降)
* `-fno-ignore-asserts`

`-dlint` は従来の各種lintオプションをまとめたものです。`-fcheck-prim-bounds` はプリミティブな配列の範囲外アクセスを検知してくれるやつです。いずれも説明は

* [5.13.4. Checking for consistency](https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/debugging.html#id15)

を参照してください。

`-fno-ignore-asserts` は最適化オプション `-O` が有効な状況でも `assert` を残すやつです。詳しくは

* [6.19.4. Assertions](https://downloads.haskell.org/ghc/9.6.2/docs/users_guide/exts/assert.html)

を参照してください。

これらのオプションを有効にするには、 `cabal.project` に以下のように書くと良いでしょう：

```
-- いつもの
packages: .

-- 開発用設定
program-options
  ghc-options: -dcore-lint -dstg-lint -dcmm-lint -dasm-lint -fllvm-fill-undef-with-garbage -debug -fno-ignore-asserts

-- この書き方はCabal 3.10以降が必要そうなので注意
if(impl(ghc >= 9.2))
  program-options
    ghc-options: -fcheck-prim-bounds
```

（ドキュメントは「トップレベルの `ghc-options` はローカルパッケージに適用される」と読めますが、実際は `program-options` の中に書く必要があるようです：[Using ghc-options at the top-level in project encouraged by docs but unavailable. · Issue #8781 · haskell/cabal](https://github.com/haskell/cabal/issues/8781)）

あるいは、CIでcabalを呼び出す際に `--ghc-options="-dlint -fcheck-prim-bounds -fno-ignore-asserts"` などのオプションを指定すると良いでしょう。

## その他

最小限のメンテナンスコストで長く使われるパッケージを作りたいなら、 `-Werror` を `.cabal` に書くのはやめた方が良いでしょう。書くなら `cabal.project` とかCIの設定にしましょう。

`.cabal` ファイルを綺麗にしたいならcabal-fmtを使うといいかもしれません（と言いつつ私は導入できていない）。

* [phadej/cabal-fmt: An experiment of formatting .cabal files](https://github.com/phadej/cabal-fmt)
