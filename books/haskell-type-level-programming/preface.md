---
title: "はじめに"
---

Haskellの特徴の一つは、強力な型システムです。どのくらい強力かというと、型レベルで関数を書いたり、複雑な制約を記述したりできます（型レベルプログラミング）。

Haskellで型レベルプログラミングを使用する場面をいくつか挙げてみます。

* リストやVectorの長さ、行列のサイズを型で管理したい (sized, vector-sized, hmatrix)
* Web APIの仕様を型で記述してサーバー・クライアントを型安全に書きたい (servant)
* コマンドライン引数の仕様を型で記述したい (optparse-declarative)
* 拡張可能レコードを提供するライブラリーは大抵型レベルプログラミングの技法をふんだんに使っています。

この本では、Haskellでの型レベルプログラミングに入門することを目指します。読者はHaskellについてある程度慣れていることを前提としています。

取り扱いたいと思っているトピックは以下の通りです：

* 型とカインド
* 幽霊型とProxy
* 発展：カインド多相
* データ型の昇格
* 型レベル関数と型族、型演算子
* 型レベル計算の結果を実行時に利用する：型クラス
* GADTと型の等価性
* シングルトン型と依存型の模倣
* 定理証明
* 応用：Constraintカインド
* 応用：GHCの型レベル自然数（Natカインド）
* 応用：GHCの型レベル文字列（Symbolカインド）
* 応用：GHC.Generics
* 応用：実行時型情報（Typeable）

本文やサンプルコードの記述は、**GHC 9.2以降**を前提とします。

以下のGHC拡張は、断りなく使う場合があります：

* DataKinds
* GADTs
* KindSignatures ★
* NoStarIsType
* ScopedTypeVariables ★
* StandaloneKindSignatures ★
* TypeFamilies
* TypeOperators ★
* UndecidableInstances

なお、★マークをつけた拡張は、GHC 9.2以降のデフォルトの言語オプション「GHC2021」で自動的に有効になります。
