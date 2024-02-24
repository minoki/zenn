---
title: "インターフェースと型クラスの違いを考える"
emoji: "💭"
type: "tech" # tech: 技術記事 / idea: アイデア
topics: [interface, 型クラス]
published: true
---

この記事では、JavaライクなインターフェースとHaskellライクな型クラスを比較します。どちらも、複数の異なる型を統一的に扱うための仕組みです。

## インターフェース

インターフェースはご存知の方も多いと思いますが、コード例を載せておきます。

```java
// インターフェースの定義
interface Greetable
{
    String greet();
}

// 実装の例1
class Hello implements Greetable
{
    String message;
    Hello(String m)
    {
        this.message = m;
    }
    public String greet()
    {
        return "Hello " + message + "!";
    }
}

// 実装の例2
class Goodbye implements Greetable
{
    public String greet()
    {
        return "Goodbye world!";
    }
}

// Greetableは型制約として利用できる
<T extends Greetable> void printGreeting(T a)
{
    System.out.println(a.greet());
}

void main()
{
    // Greetableは型として利用できる
    Greetable[] a = {new Hello("world"), new Hello("interface"), new Goodbye()};
    for(Greetable g : a)
    {
        System.out.println(g.greet());
    }
    printGreeting(new Hello("generics"));
}
```

インターフェースの特徴をいくつか挙げます：

* それ自体が型である
* 型制約として使うこともできる（ジェネリクスのある言語の場合）
* `this`（あるいは `self`）に応じて処理の実体を切り替えられる

インターフェースを持っている言語の代表例（というか、ここで念頭に置いた言語）はJavaやC\#です。

## 型クラス

型クラスは、Haskellやその影響を受けた言語に搭載されている機能です。コード例を載せます。

```haskell
-- 型クラスの定義
class Greetable a where
  greet :: a -> String

-- インスタンスの定義
newtype Hello = Hello String
instance Greetable Hello where
  greet (Hello message) = "Hello " ++ message ++ "!"

-- 型制約として利用できる
printGreeting :: Greetable a => a -> IO ()
printGreeting x = putStrLn (greet x)

main = printGreeting (Hello "world")
```

インターフェースと大きく違う点（だと私が考えている特徴）は、二項演算や定数（ゼロ項演算）のような例を素直に表現できることです。

```haskell
class Eq a => Ord a where
  -- 二項演算：入力に型 a が複数現れる
  compare :: a -> a -> Ordering

class Additive a where
  -- 二項演算：入力に型 a が複数現れ、また、出力にも a が現れる
  add :: a -> a -> a

  -- 定数：入力に a は現れないが、出力には a は現れる
  zero :: a
```

型クラスの特徴を独断と偏見でまとめます：

* それ自体は型ではない
* 型制約として使う（推論させることもできる）
* 型推論によって判明した型に応じて処理の実体を切り替えられる
* 二項演算や定数を素直に表現できる

型クラスを持っている代表的な言語（ここで念頭に置いた言語）はHaskellです。

## MLモジュール

インターフェースや型クラスよりは知名度は劣るかもしれませんが、ML系言語のモジュールシステムもそれらに類する機能だと思うことができます。モジュールの仕様に相当するものはシグネチャーと呼ばれ、実装はストラクチャーと呼びます（Standard MLの場合）。モジュールに依存するモジュールも書くことができ、ファンクターと呼ばれています（圏論のやつではないので安心してください）。

コード例を載せます。

```sml
(* モジュールの仕様：シグネチャー *)
signature GREET = sig
  type t
  val greet : t -> string
end

(* モジュールの実装：ストラクチャー *)
structure Hello = struct
  datatype t = HELLO of string
  fun greet (HELLO message) = "Hello " ^ message ^ "!"
end

(* モジュールに依存した型と関数：ファンクター *)
functor Greet (G : GREET) : sig
  val printGreeting : G.t -> unit
end = struct
  fun printGreeting x = print (G.greet x ^ "\n")
end

structure G = Greet (Hello)
val () = G.printGreeting (Hello.HELLO "world")
```

```sml
(* 二項演算や定数も素直に表現できる *)
signature ADDITIVE = sig
  type t
  val zero : t
  val add : t * t -> t
end

signature ORD = sig
  type t
  val compare : t * t -> order
end

functor Set (O : ORD) :> sig
  type set
  type elem = O.t
  val empty : set
  val fromList : elem list -> set
  val member : set * elem -> bool
end = struct
  ...
end
```

MLモジュールの特徴を載せます：

* それ自体は型ではない
* モジュールは型を持てるが、型にはモジュールは紐づかない。型推論には関与しない
* ファンクターによってモジュールに依存した型や処理を書ける
    * 言語によってはモジュールに依存する関数を簡便に書ける（第一級モジュール）
* ファンクターの引数として与えるモジュールは明示的に与える
* 二項演算や定数を素直に表現できる

MLモジュールを持っている代表的な言語（ここで念頭に置いた言語）はStandard MLやOCamlです。

## インターフェースと型クラスの違いは何か

最近の言語は、インターフェースとか型クラスに類する機能を持っていることが多いです。それらは「インターフェースのようなもの」と呼ぶべきでしょうか、「型クラスのようなもの」と呼ぶべきでしょうか？

人によって意見は様々かもしれませんが、私が重視するのは「二項演算を素直に表現できるか」ということです。すなわち、型によって実装が選択され、二項演算を素直に表現できる機構を型クラス、そうでない機構をインターフェースと呼びたいです。

この基準で言えば、RustのtraitやSwiftのprotocolは「型クラスのようなもの」となります（`Self` で `self` と同じ型を参照できるので）。一方、Objective-Cのprotocolは（Swiftと同じ名前ではありますが）「インターフェースのようなもの」となります（`Self` に相当する機構がないので）。

「型クラスは既存の型に後付けできる」という観点もありますが、C\#のpartial classやObjective-Cのextensionなどがあるので、あまり本質的な違いだとは思いません。

インターフェースは型として振る舞えるという観点もありますが、型クラスも存在型を組み合わせれば似たようなことができます。

## インターフェースで型クラスみたいなやつを模倣する

「型クラスは二項演算を素直に表現できる」と書きましたが、インターフェースでも二項演算を表現する方法はあります。

まず、C\#でよく用いられているのは、「自身の型」によってパラメーター化されたインターフェースです。比較演算の例を載せます。

```csharp
// IComparableを実装するのは、比較対象のクラス
interface IComparable<T>
{
    int CompareTo(T other);
}

// 型制約は T : IComparable<T> の形になる
```

ただ、この方法ではゼロ項演算は表現できません（ダミーのインスタンスが必要になる）。型制約が `T : IComparable<T>` という再帰っぽい形になるのが奇妙です。

別のやり方もあります。演算対象を表すオブジェクトと、演算を表すオブジェクトを別に用意するのです。例を載せます。

```java
// Comparatorを実装するのは、比較対象とは別のクラス
interface Comparator<T>
{
    int compare(T a, T b);
}

// 利用側は Comparator<T> のインスタンスを受け取る
```

なんか型クラスの脱糖後、あるいはMLモジュールっぽいものを自分で書いたみたいな感じですね。

## 型クラスとMLモジュールの違いは何か

型クラスもMLモジュールも、二項演算を素直に表現できます。これらの違いは何でしょうか？

私が重視したいのは、「型推論によって暗黙に実装が選択されるかどうか」です。型クラスは型推論によって実装が選択され、MLモジュールはユーザーが書いたコードによって実装が選択されます。

たまに「TypeScriptで型クラスをやってみた」みたいなの（fp-tsとか）を見かけますが、私の語法としてはそれは型クラスというより（第一級の）MLモジュールに近いものです。型をパラメーターとして取るかメンバーとして持つかという違いはありますが。

まあこれはあくまで私の意見なので、異論は認めます。

## 明示的に実装するか、暗黙に適合するか

インターフェースに類するものを明示的に実装するか、要件さえ満たせば暗黙に適合するのか、という観点もあります。

JavaのインターフェースやHaskellの型クラスは明示的に実装する必要があります。一方で、MLモジュールは適合させるために特別な宣言は必要なく、使用する際に適合が検査されます（モジュールを定義する際に明示的に適合を表明することはできますが、必須ではありません）。

インターフェースの仲間でも、GoやTypeScriptは暗黙に適合するようです。

「暗黙に適合する型クラス」は、ユーザー定義のインスタンスを書けると色々破滅しそうな気がします。ただ、Standard MLの等価性（eqtype）はユーザー定義のインスタンスを書けない型クラスと解釈できそうで、これは型の構成要素が条件を満たせば自動で適合します。

## 曖昧な境界

最近のC\#にはGeneric mathとか言って二項演算やゼロ項演算を扱える環境が整備されているようです。static abstractって何？

OCamlはModular implicitsとか言って、型クラスみたいなことをやりたいような動きがあるようです。

この記事では「インターフェース」「型クラス」「MLモジュール」に関する私の気持ちを述べましたが、これらは排反ではなく、グラデーションがあると考えるのが適切なのかもしれません。

この記事では多重ディスパッチ（CLOSとかJuliaにあるやつ）については取り扱いませんでした。筆者が詳しくないので……。
