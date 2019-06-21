s=poly(0,'s')
//h=syslin('c',(10*s + 1)/(s+10))
//h=syslin('c',1/(s+10))
h=syslin('c',1/(10*s + 1))
clf();bode(h,0.00001,10000);