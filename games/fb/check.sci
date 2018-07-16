// scilab-adv-cli
t=linspace(0,10,100);
//fprintfMat('hoge.txt',t',%5.2f');
s=poly(0,'s');          //←多項式の変数　s　を定義
//G=(10*s + 10)/(s^3+12*s^2+30*s+10);        //←伝達関数　G　を定義
G=(10*s+10)/(10+12*s+s^2)
sys=syslin('c',G);      //←連続時間線形システムへ伝達関数 G を登録
y=csim('step',t,sys);   //←伝達関数　G　へステップ入力を与えた場合の出力変化を　y　に得る
plot(t,y)               //←グラフ表示
xgrid()                 //←グリッドの表示
xtitle('Step Respons','Time(sec)','Amplitude')  //←タイトル関連の表示

