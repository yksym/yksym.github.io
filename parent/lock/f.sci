
funcprot(0)
clf();
x=[-100:1:700]';

function [y]=th(x)
y = (exp(x) - exp(-x)) ./ (exp(x) + exp(-x))
endfunction

function [y]=sigm(x, h, u, a)
y= h * th((x-u) / (2 * a))
//y= h * tanh((x-u) / (2 * a))
endfunction

function [y]=g(x, u, s)
y=exp(- (x-u) .* (x-u) / (2*s*s))
endfunction

function [y]=f(x, h, r, offset)
y=h*g(x, offset, r/5)
endfunction

//plot2d(x,25 - (f(x, 10, 50, 100) + f(x, 30, 50, 200) + f(x, 20, 50, 300) + f(x, 50, 50, 400) + f(x, 50, 50, 500) + sigm(x, 25, 600, 10)));
plot2d(x,sigm(x, 25, 600, 10)));
//plot2d(x,f(x, 10, 50, 100))
