//exec("xxx.sci");
//r

clf;
t=linspace(0,20,1000);
s=%s;

//K = 1
//K = 2
//K = 3
//K = 4 // good
K = 10  // unstable
r = routh_t((s+1)^3 + K);

G = K / ((s+1)^3)

//SYSTEM = G / (1 + G);
//sys=syslin('c',SYSTEM);
//y=csim('step',t,sys);

DISTURBANCE = 1 / (1 + G);
d=syslin('c',DISTURBANCE);
y=csim('step',t,d);

plot(t,y)
xgrid()
xtitle('Step Respons','Time(sec)','Amplitude')

