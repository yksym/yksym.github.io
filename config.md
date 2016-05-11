
不変条件(会話,作業,etc...)
====================

* 焦らず、余裕を持ち、丁寧で、正直である
* 楽しめるように工夫や調整をする(時間的余裕は必須)
* 他人からの承認よりも自己の成長を目的とする

on頭を整理する
===============

* 読書後や説明前に要約とキーワードを言葉にする(大体こんな感じはNG)
* 文章で以下を再帰的にちゃんと書けるかどうか
** ナゼナゼ + ナニナニ
*** Xとは何ですか？
*** XとYとの関係は何ですか？

onMTG
=================

* 常に議事録:テーマをTreeで表現する
** 起点となる問題をrootにする
*** 重要度や許容範囲を考える(放置していいのか？7割くらいの効果でいいのか？)
*** 原因(C)、対策(M)、問題(P)、その他(O)のタグをつける
* 意欲があれば意見にして、意欲がなければ質問にする


on伝える
==================

* なるべく一言で応える(Yes,No. XXはYYです.)
* 頭の中で整理出来ていないことは伝えない
* 自然言語以外の表現も検討する


on要求する/される
================================================

* (1) 何がしたいか、何故したいかを単純明快にする(Goal)
    * 出力を使って次に誰が何をするのか？その次は？最後は？
    * 事前条件、事後条件(検証/review方法:SV、出力フォーマット)
    * 締切

* (2) 作業が発生する場合：ブレークダウンして不足する情報や資源・リスク洗い出し

* (3) お願い/質問する場合：本当に自分で(簡単に)解決出来ないか証明する


on論理検証
====================

* DAGで構造化する(node: 単文/単語、edge: 関係)
* 曖昧な用語・修飾子はないか?筋が通っているか?前提は妥当か(確度・過不足)?


on問題解決
=====================

* (1) 問題は何か？何故それが問題なのかを第三者に説明出来るようにする(目的/現象/原因)
* (2) どう解くかを考える
    * 最適解が満たす条件(モデル化・指標・制約)
    * 問題領域の外を見て、そもそもの目的や前提を崩す
      - そもそも解かなくて済む方法を考える
      - 厄介な前提/制約を消す方法を考える
    * どうあるべきか？から上手く形式化出来ない場合、現状で何が出来るかを形式化してどの程度目的を達成できるかを議論する方が良い
    * 複雑で混乱する場合は問題を限定して考える
      - モデルを簡単にする(要素を４つくらいに絞る)
      - 文章(ツリー)だけで整理しようとせず、モデル図、表、グラフを駆使する
      - 制約を追加/削除/緩める/強める(数値を上下させたり)
* (3) 網羅的に案をリストアップ
      - 技術的なネタならgoogle先生と論文と隣人に相談するのも良い(最低10分は自分で考えるべき)
      - 探しものならそれが存在しうる場所はどこか？代用品はないのか？
      - Pros/Cons


on食べる
=============

* 晩飯は7部目
* 多めに作らない・頼まない
* 残飯処理しない・無理せず残す


on預かる/返す/発注/受注
========================

 * 台帳にいつ、どこにしまったかor誰に渡したかを記述する(理由も)
 * callBackとポーリング用のtimerをセット

on動く
===========================

 * 移動先に求めるものがあるか？空きがあるか？
 * 物理シミュレーションして最適パスを計算する
 * 粘性・摩擦力や固定されているかどうかは見た目から分からないので慎重に扱う


作業記録
=====================

 * 作業に関係するアイデアは、採用・非採用に関わらず結果・理由のログを残す
 * 誰でも再現できるように環境と実行手順と日付の情報は残すこと

