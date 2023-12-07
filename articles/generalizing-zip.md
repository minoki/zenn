---
title: "Haskellのzip関数を一般化すると何になるか"
emoji: "📚"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [haskell]
published: true
---

この記事は [Haskell Advent Calendar 2023](https://qiita.com/advent-calendar/2023/haskell) の7日目の記事です。

## zip関数について

Haskellには `zip` 関数というものがあります。この関数は、2つのリストを受け取って、それぞれから取り出した要素を組にしたリストを返します。

```haskell
zip :: [a] -> [b] -> [(a, b)]
```

実行例は次の通りです：

```haskell
ghci> zip [1,2,3] ["Alpha","Bravo","Charlie"]
[(1,"Alpha"),(2,"Bravo"),(3,"Charlie")]
```

リストの要素の個数が異なる場合は、短い方に合わせられます。`[1,2,3]` を無限等差数列 `[1..]` に置き換えてみましょう：

```haskell
ghci> zip [1..] ["Alpha","Bravo","Charlie"]
[(1,"Alpha"),(2,"Bravo"),(3,"Charlie")]
```

短い方（3要素）に合わせられました。

3つのリストを3要素タプルのリストに変換する `zip3` 関数や、4つ以上のリストに対して似たようなことをする関数もあります。

```haskell
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

-- 以下は Data.List より
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)] 
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)] 
zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)] 
```

関連する関数として、`zipWith` というのもあります。これはタプルを作る代わりに、関数適用を行います。

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

-- Data.List には zipWith7 まである
```

`zip` を使うと `zipWith f xs ys` は `map (uncurry f) (zip xs ys)` と表現でき、逆に `zipWith` を使うと `zip` は `zipWith (,)` と表現できます。ですので、`zip` と `zipWith` は表現力的には同じものと思って良さそうです。

さて、Haskellのリストに関する関数は `Foldable` や `Traversable` に一般化できるものがちょいちょいあります。そして、`zip` 系の関数はリストだけではなく、`Vector` にも定義されています。`zip` 系の関数をリスト以外に一般化することはできないのでしょうか？言い換えると、以下が定義されるような `f` についての制約 `???` は何と呼ばれるべきでしょうか？

```haskell
generalizedZip :: ??? f => f a -> f b -> f (a, b)
generalizedZipWith :: ??? f => (a -> b -> c) -> f a -> f b -> f c
```

## 満たすべき性質

何かを一般化するときは、何らかの法則を満たすように一般化したいです。`zip` が満たす性質はいくつかありますが、ここでは2点（+α）選びました。

まず、結合法則です。以下の2つは同型であって欲しいです：

```haskell
zip xs (zip ys zs)
zip (zip xs ys) zs
```

実際の型は `[(a, (b, c))]` と `[((a, b), c)]` になるので同一ではないですが、内容としては同じです。

次に、交換法則です。以下の等式が成り立って欲しいです：

```haskell
map (\(x,y) -> (y,x)) (zip xs ys) == zip ys xs
```

リストの `zip` に限れば、「`zip xs (repeat ())` と `xs` が同型」みたいな性質もあります。`Vector` については `repeat` 関数がないのでこの性質もありません。

## Applicativeクラス

:::message alert
圏論注意報！圏論が苦手な方は下の「閑話休題」まで飛んでください。
:::

上記の `zip` の性質を圏論的に言うと、対称モノイダル関手 (symmetric monoidal functor, 対称モノイド関手) となります。

モノイダル関手。どこかで聞いたことのある方もいるかもしれません。そう、Applicative関手の圏論的な呼び名が（強）laxモノイダル関手 ((strong) lax monoidal functor) でした。

圏論的な話をもっと知りたい方のために、私が昔書いた記事を挙げておきます：

* [アプリカティブ関手ってなに？モノイド圏との関係は？調べてみました！](https://blog.miz-ar.info/2018/12/applicative-functor/)（2018年12月）

余談ですが、昔の記事では私はmonoidal categoryの訳についてこんなことを書いていました：

> モノイド圏 (monoidal category)：文献によっては「モノイダル圏」と訳されることもある。しかし、“abelian group” は普通は「アーベリアン群」ではなく「アーベル群」と訳すし、“homological algebra” は「ホモロジカル代数」ではなく「ホモロジー代数」と訳す。よってこの文書では “monoidal category” は「モノイダル圏」ではなく「モノイド圏」と訳す。（1 文字短いし…）

が、monoidal categoryの訳は「モノイダル圏」で定着してしまったように見受けられます。なのでこの記事ではmonoidalの訳は「モノイダル」にしました。

閑話休題。

要するに、**リスト型は `zip` によって `Applicative` のインスタンスとなる**ということです。実際、リスト型を `zip` によって `Applicative` のインスタンスとみなすnewtype wrapperが `Control.Applicative` で定義されています：

* [ZipList - Control.Applicative](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Applicative.html#t:ZipList)

```haskell
ghci> :m + Control.Applicative
ghci> (,) <$> ZipList [1,2,3] <*> ZipList ["Alpha","Bravo","Charlie"]
ZipList {getZipList = [(1,"Alpha"),(2,"Bravo"),(3,"Charlie")]}
```

リスト型のデフォルトの `Applicative` インスタンスは `zip` ではなく、モナドのインスタンスに基づいたものであることに注意してください。

```haskell
ghci> (,) <$> [1,2,3] <*> ["Alpha","Bravo","Charlie"]
[(1,"Alpha"),(1,"Bravo"),(1,"Charlie"),(2,"Alpha"),(2,"Bravo"),(2,"Charlie"),(3,"Alpha"),(3,"Bravo"),(3,"Charlie")]
```

さて、`Applicative` クラスには

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```

の一般化として

```haskell
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
```

という関数が含まれています（ちなみに、GHC 9.6以降では `liftA2` は `Prelude` からエクスポートされています）。よって、`zip`/`zipWith` の一般化は `Applicative` であると自信を持って言えるでしょう。……本当に？

`Applicative` クラスには `pure :: a -> f a` という関数が含まれています。リストの `zip` の場合（`ZipList` の場合）はこれは `repeat :: a -> [a]` です。ですが、`Vector` には `repeat` 関数はありません。なので、`Vector` を `zip` によって `Applicative` インスタンスとみなすことはできません（「長さが型レベル自然数で与えられた `Vector` 型」であれば `Applicative` のインスタンスにできます）。

結局、`Vector` も考慮した `zip`/`zipWith` の一般化は「`Applicative` クラスから `pure` を取り除いたもの」とするのが適切でしょう。何とも歯切れの悪い結論になりました。

ちなみに、Haskellには「`Applicative` クラスから `pure` を取り除いたもの」はありませんが、PureScriptにはそれが `Applicative` クラスのスーパークラスとして存在して、`Apply` クラスと呼ばれているようです。

* [Control.Apply - purescript-prelude - Pursuit](https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Control.Apply)

## おまけ：unzipの一般化

`zip` の逆として `unzip` という関数もあります。

```haskell
unzip :: [(a, b)] -> ([a], [b]) 
```

これはリストの部分を `Functor` に一般化できます。

```haskell
unzip :: Functor f => f (a, b) -> (f a, f b)
unzip x = (fmap fst x, fmap snd x)
```

そして、GHC 9.8ではこの `Functor` に関する `unzip` が `Data.Functor` からエクスポートされるようになりました。`Data.Functor` をunqualified importしつつ `unzip` を使っていた人は気をつけてください。対処法はqualified importするか、`()` でimportする名前を制限するか、となるでしょう。
