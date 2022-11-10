---
title: "Haskellでちょっとしたスクリプトを書く"
emoji: "😊"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: ["haskell"]
published: false
---

Haskellで本格的に開発する際はcabalなりstackなりでプロジェクトを作るわけですが、ファイル一つで済むような（書き捨て）スクリプトをHaskellで書きたい場合があります。

依存関係のないスクリプトであれば、普通にファイルを作って `runghc`/`runhaskell` すれば良いでしょう。しかし、Haskellには標準ライブラリー（`base`）以外にも「準標準」と呼べる外部のライブラリーが多数あり（例：`bytestring`, `text`, `vector`）、それらに明示的に依存することは `runghc` ではできません。

そこで使えるのが、cabal scriptやstack scriptと呼ばれる機能です。

# cabal script

`cabal run` コマンドを使うと、ファイル中に `{- cabal:` 形式の特殊なコメントを含むHaskellコードをその場でビルドして実行することができます。

* [5.2.9. cabal run — 5.2. Commands — Cabal 3.8.1.0 User's Guide](https://cabal.readthedocs.io/en/3.8/cabal-commands.html#cabal-run)

例：

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring ^>= 0.11.3.1
-}
import qualified Data.ByteString.Char8 as BS
main = BS.putStrLn (BS.pack "Hello world!")
```

実行例（cabal-install 3.8の場合）：

```sh
$ cabal run hellocabal.hs
Hello world!
$ chmod +x hellocabal.hs
$ ./hellocabal.hs
Hello world!
```

`{- cabal:` はその行に単独で存在する必要があります。

shebangは、 `cabal v2-run` 経由で実行する場合は必要ありません。

cabal scriptはcabal-install 3.8で色々強化されました。ビルド時のメッセージが表示されなくなったり、ビルド結果のキャッシュが実装されたり、 `{- project:` が実装されたり、といった具合です。

`{- project:` を使うと `cabal.project` に相当する内容を書けます：

```haskell
#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring ^>= 0.11.3.1
-}
{- project:
with-compiler: ghc-9.4.3
-}
import qualified Data.ByteString.Char8 as BS
main = BS.putStrLn (BS.pack "Hello world!")
```

# stack script

stackでもスクリプトを実行できます。むしろstackの方が元ネタで、cabal scriptが後発です。

* [The script interpreter and stack script command — User's guide (introductory) - The Haskell Tool Stack](https://docs.haskellstack.org/en/v2.9.1/GUIDE/#the-script-interpreter-and-stack-script-command)
* [script interpreter + stack script でスクリプティング！](https://haskell.e-bigmoon.com/stack/tips/script-interpreter.html)

例：

```haskell
#!/usr/bin/env stack
-- stack script --resolver nightly-2022-11-08 --package bytestring --package vector
import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
main = do BS.putStrLn (BS.pack "Hello world!")
          print (V.sum (V.fromList [1..100]))
```

実行例：

```sh
$ stack hellostack.hs
Hello world!
5050
$ chmod +x hellostack.hs
$ ./hellostack.hs 
Hello world!
5050
```

stackへのオプションを指定する行はshebangを除いた最初の行（shebangがあれば2行目、shebangがなければ1行目）から始まる必要があります。`{- -}` 形式のコメントを使うことで複数行にわたって記述できます。

`--package` を全く指定しなかった場合は、`import` の内容から推測してくれます。

shebangは、`stack` 経由で実行する場合は必要ありません。

# 利用例

## ちょっとしたスクリプトとして使う

もちろん、プロジェクトを作るまでもないちょっとしたスクリプトを書くのに使えます。

例：

* <https://github.com/minoki/icfpc2006/blob/master/extract-CBV.hs>
    * bytestringとJuicyPixelsに依存するcabal scriptです。

## コードを貼るときに依存関係を明示するのに使う

GHCのバグ報告の際は報告文の中に再現コードを貼り付けることが多いです。その際、cabal script形式のコメントで依存関係を明示することができます。

例：

* [Misleading GHC error when function is not in scope (#16491) · Issues · Glasgow Haskell Compiler / GHC · GitLab](https://gitlab.haskell.org/ghc/ghc/-/issues/16491)

# 歴史

歴史的にはstack scriptの方が古く、cabal scriptは2018年ごろに実装された比較的新しい機能です。

stackのscript interpreterは[0.1.2.0](https://docs.haskellstack.org/en/stable/ChangeLog/#0120), `stack script` は[1.4.0](https://docs.haskellstack.org/en/stable/ChangeLog/#140)からの機能らしいです。

参考：

* [RFC: Add support for "#! /usr/bin/env cabal · Issue #3843 · haskell/cabal](https://github.com/haskell/cabal/issues/3843)
* [Add cabal scripting support by typedrat · Pull Request #5483 · haskell/cabal](https://github.com/haskell/cabal/pull/5483)
