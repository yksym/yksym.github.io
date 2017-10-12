再帰と不動点と関手の話
=========================


この1年何してたの？
--------------------

* CSPよく分からんから処理系書いてみた
    * 簡単な機能だけ実装したがキレイなコードにはならなかった
    * プロセスの引数(状態)やチャネルの引数(ペイロード)を静的型付で扱うの辛い
    * いい感じのDSLを書く方法を探して tagless final のlecture note 読んだ

* オレンジ色のプログラム意味論を読む(2回目)
    * 操作的意味論と表示的意味論の対応付けの話はかろうじて分かった気になれた（何一つ分かってないし全て忘却した）
    * 6章あたりの領域方程式の逆極限法で完全に挫折
    * その代わり「圏論の歩き方」という本で単純型付ラムダ計算（λ → )のCCCに基づく表示的意味論のさらっとした紹介読んで満足した(ほぼ型についての言及)

* lensから圏論プログラミングの世界へ足突っ込む
    * 「lens沼にようこそ(2回目)」
    * 「lensは余状態余モナドのF余代数やで」
    * 「CPLというのがあってだな・・・」


一番印象に残ってるのは？
--------------------

* 再帰と不動点と関手(fmap)がほんとマジカル
    * プログラム意味論で再帰関数やループの意味を最小不動点を使って記述する話
    * F代数で再帰関数のクラスを関手の不動点を使って分類する話

ここら辺について話します...


参考文献
----------------

分かりやすいし発展内容も含む素晴らしい資料達

* [1] "CATEGORICAL PROGRAMMING WITH INDUCTIVE AND COINDUCTIVE TYPES" (2000) <- 基本はコレ
* [2] https://www.slideshare.net/sakai/introduction-to-categorical-programming-revised (2009)
* [3] http://titech-ssr.blog.jp/archives/1047835805.html (2015)
* [4] 関数プログラミングの楽しみの３章(英語版ならwebにある)

自然数
-----------------

* 自然数は以下で表現します(実は無限含んでるからCoNatだという話もあるそうですが・・・)
```haskell
data Nat = Z | S Nat
```

* Haskellの数値リテラルは実はリテラルじゃなくて型クラスNumのインスタンスならなんでも表せる
    * 上のNatも0,1,2,3とコード上で表現することが出来る(Z が 0、 S Z が 1で表現されます)
    * 要素が1つしかない環を実装すればお馴染みの 0 = 1 だって出来る


再帰関数にも色々あるそうです
--------------------------

* 自然数を使った再帰関数

```haskell

-- パターン1: f(n+1) = h(f(n)) + 
add :: Nat -> Nat -> Nat
add m (S n) = S (add m n)
add m  Z    = m

mul :: Nat -> Nat -> Nat
mul m (S n) = add m (mul m n)
mul m  Z    = Z

-- パターン2: f(n+1) = h(f(n),n) <- primitive recursion
fact :: Nat -> Nat
fact (S n) = mul (S n) (fact n)
fact  Z    = S Z

-- パターン3: f(n+1) = h(f(1)..f(n)) <- course-of-values recursion
fib :: Nat -> Nat
fib (S (S n)) = add (fib (S n)) (fib n)
fib (S  Z)    = S Z
fib  Z        = S Z

```

* リストを使った再帰関数の場合はこう（理由は後述)
    * パターン1: f(x:xs) = h(x, f(xs))
        * 注目している要素 x と直前の結果 f xs (残りの要素の処理結果) にのみ依存している
        * mapやfilterやconcat lengthがこの形で書ける
        * foldrはパターン1相当の処理が記述出来る
    * パターン2: f(x:xs) = h(x, f(xs), xs)
        * パターン1と異なり、残りの要素 xsが使える(最初に条件に合致したものは処理して後はそのまま流用とか出来る）
        * dropWhileや一部のinsertがこの形で書ける
    * パターン3: ????
        * 勉強中。さっぱり分からん


* F代数という分野ではパターン1、2、3はそれぞれ catamorphism、paramorphism、histomorphismとよぶそうです [1]
    * morphismは圏論用語で射の意味。ここでは関数と思っておけばOK。

* ちなみにWikipediaを眺めてみると
    * パターン3はパターン2に変換可能らしい
    * ackermannは高階(関数を引数にした)原始帰納的関数でかけるらしい
    * 計算可能性理論とかいう分野があって再帰関数の細かい分類や性質はそちらを参照



不動点にも色々あるそうです
---------------------

* 一般的に x = f(x) なるxを不動点と呼ぶそうな

* 値レベルの不動点
```haskell
ones :: [Int]
ones = 1:ones

fix :: (a -> a) -> a
fix f = f (fix f)

ones' :: [Int]
ones' = fix (1:)
```

* 値レベルの不動点は遅延評価と合わさって様々な魔技が生まれた
    * 再帰関数のメモ化: https://wiki.haskell.org/Memoization (遅延評価のない言語でも使えるらしい?)
    * 1-passで2分木が持つ最小値を求めて全ノードその値に差し替える話
    * 1行フィボナッチ
```haskell
fib = 1:1:zipWith (+) fib (tail fib)
```


* 型レベルの不動点
```haskell
data Nat = Z | S Nat
data List a = Nil | Cons a (List a)

newtype Fix f = Fix f (Fix f)

```
* NatやListと同じものを Fixを使って記述出来るというのが今回のお話
* 不動点に順序関係があれば最小不動点・最大不動点が存在する場合もあるが、[1]によるとHaskellだとコード上は区別つかないらしい(両者が一致するのかも私にはよく分からない)




関手とかいうのもあるらしい
----------------------

* 関手は圏論？とかいう分野(よく知らない)の用語で、定義はそちらにゆずります
    * 本資料ではHask圏とその上の自己関手しか出てきませんので、関手 は 型クラスFunctorのインスタンス(ListとかMaybeとか型変数を)だと考えてください

* 関手Fの特徴は以下2つです
    * 型変数aを与えたら F a という型が手に入る
    * fmap :: (a -> b) -> F a -> F b というFunctor型クラスのメソッドが使える
        * 文献やコード追っていて不可思議なことが起こったら大体このfmapの定義が鍵になっていたりする(個人の感想)
        * fmap f . fmap g = fmap (f.g) が成立していないインスタンスを作ることはコンパイラは許可していますが道徳的に禁じられています

* 以降では以下のような有向グラフを使います。は頂点ノードは型を、エッジは関数を表します
    * 関数 f に対して fmap f を F(f) と表示します
```
        f
A   --------->  B


       F(f)
F A ---------> F B

```

意味不明なコード
-----------------

```haskell
newtype Fix f = Fix (f (Fix f))

type Nat = Fix Maybe
```


※注意：演出です。実際にこんなコードかくような人はいません。


型の多項式
-----------------

* よく使う記号群（他にも0や冪とかもよく出てくるけど今回は使わない)
```
F  : 関手
X  : 型
~= : 同型の関係を表す演算子。ここでは型はただの集合として、要素間の1:1対応付が可能であることを意味する
     逆写像が存在する関数自体を同型と呼ぶ場合もある
1  : 1点集合。Haskellだと()と同型。そして a は 1 -> a と同型である点に注意
*  : 直積。A * B は Haskellだと (A,B)。data C = C A B と同型
+  : 直和。A + B は Haskellだと Either A B。data C = C1 A | C2 B と同型
```

* Nat は 関手 F(X) = 1 + X の最小不動点 Fix F と同型となる
```

ふるゆわイメージ1

data Nat = Z  | S  Nat
      X  = 1  +     X

ふるゆわイメージ2

Fix F = F(F(F(F(F(F(...
   = 1 + (1 + (1 + ...  <-- それぞれの1点集合を 0,1,2,3...とアサインすれば自然数と1:1対応付けが可能
```

* ちなみにMaybeは1+Xと同型
```
               1       +       X
Either  () X = Left () | Right X
Maybe      X = Nothing | Just  X
```
* リスト [a] は F(X) = 1 + a * X の最小不動点 Fix F と同型になる
* F(X)がXについての多項式だとμFは存在するらしい...(ω連続うんぬん。よく分からぬ)



射の合成いろいろ
-----------------

```haskell
(&&&) :: (a -> b) -> (a -> c) -> a -> (b * c)
(f &&& g) x     = (f x, g x)

(|||) :: (a -> b) -> (c -> b) -> (a + c) -> b  -- either という関数と同じです
(f ||| g) x = case x of
                Left  x' ->  f x'
                Right x' ->  g x'
```


catamorphism
-----------------

* 型 C、関数 phi:: F C -> C の組(C, phi)をF代数と呼び、このCをF代数のキャリアと呼ぶ
    * 実際にはphiに全ての情報がつまってるのでphiを始代数と呼ぶ場合もあるらしい
* 任意のF代数(C,phi)に対して、以下の図を可換にするfがただ１つに定まるF代数(μF, in)をF始代数と呼ぶ
    * 図のfをcatamorphismと呼び f = cata phi で表す
```
         in
F(μF) ---------> μF

 |               |
 |F(f)           | f
 |               |
 v               v
         phi
F(C)  ---------> C


可換： f . in = phi . F(f)

```

* この方程式からF(X)に対してfが求まる（詳細は付録参照)
    * F(X) = 1 + X の場合 phi = c ||| h とすると、f (n) = (h ^ n) c = h(h(h(...(c))) つまり、hをn回合成したものにcを適用するような関数
    * F(X) = 1 + A * X の場合 f は foldr になる
    * どちらの場合もコンストラクタの置換という見方が出来る

* Lambekの補題
    * in ^ (-1) = cata (F in)
    * つまり、μFがF始代数のキャリアならば μF ~= Fix F がいえる
    * F(X) = 1 + X の場合、in の逆射像は pred になる。つまり pred は cata で書ける!


paramorphism
--------------

* (μF, in)をF始代数とすると、任意のphi :: F(C*μF) -> C に対して以下の図を可換にするfがただ1つに定まる
    * 図のfをparamorphismと呼び、para phiで表す
    * para phi = fst . (cata (phi &&& (in . fmap snd))) となる
```
            in
   F(μF) ---------> μF

    |               |
    |F(f &&& id)    | f
    |               |
    v               v
            phi
 F(C*μF) ---------> C


可換： f . in = phi . F(f &&& id)

```

この方程式からF(X)に対してfが求まる

* F(X) = 1 + X の場合 phi = c ||| h とすると
```
f(0) = c
f(n+1) = h(f(n), n)
```

* F(X) = 1 + A * X の場合 phi = c ||| h とすると
```
f(nil)  = c
f(x:xs) = h(x, f(xs), xs)
```

* anamorphism,histomorphism,hylomorphism,futumorphism,dynamorphism, zygomorphism, ...まだまだ沢山あるらしい



まとめ
-------------------

* データや処理の表現方法について考えさせられた
    * 直感的には"違う"ハズなのに実は同型(オモシロイ)
    * 直感的には"同じ"ハズなのに実は同型じゃない(コワイ)

* 流行が終わった感のある今こそ圏論やるか（2回目)
    * CWM?Awodey?でも最近は高階な圏(2-cat)を使って説明している場合が多いらしい・・・
    * CPLの論文は読みたいなぁ・・・


付録
-----------------

* F(X) = 1 + X の場合 cataの方程式の解き方
```

in :: 1 + μF -> μF
in は in = zero ||| succ と表せる

phi :: 1 + C -> C
phi は phi = c ||| h と表せる

f :: μF -> C
zero :: 1 -> μF
succ :: μF -> μF
c :: 1 -> C
h :: C -> C

f . (zero ||| succ) = (c ||| h) . (fmap f)

が成立するので

両辺にLeft () を適用すると

f zero = c が成立する

両辺にRight n を適用すると任意のnに対して

f (succ n) = h (f n)

以上を高校数学likeな表記になおすと

f(n+1) = h(f(n))
f(0) = c

となるので f(n) = (h^n) c となる
```

* F(X) = 1 + A * X の場合 cataの方程式の解き方
```

in :: 1 + (A*μF) -> μF
in は in = nil ||| cons と表せる

phi :: 1 + (A*C) -> C
phi は phi = c ||| h と表せる

f :: μF -> C
nil :: 1 -> μF
cons :: A*μF -> μF
c :: 1 -> C
h :: A*C -> C

f . (nil ||| cons) = (c ||| h) . (fmap f)

が成立するので

両辺にLeft () を適用すると

c = f nil

両辺にRight (x,xs) を適用すると任意のx,xsに対して

f (cons (x, xs)) = h (x, (f xs))

consを:に、nilを[]として高校数学likeな表記になおすと

f (x0:x1:x2:...[]) = h x0 (h x1 (h x2 (... c)))

となるので、これはfoldrと同じ。
```

