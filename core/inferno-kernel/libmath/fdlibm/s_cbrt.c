#include "fdlibm.h"
static const unsigned
B1 = 715094163,
B2 = 696219795;
static const double
C = 5.42857142857142815906e-01,
D = -7.05306122448979611050e-01,
E = 1.41428571428571436819e+00,
F = 1.60714285714285720630e+00,
G = 3.57142857142857150787e-01;
double cbrt(double x)
{
int hx;
double r,s,t=0.0,w;
unsigned sign;
hx = __HI(x);
sign=hx&0x80000000;
hx ^=sign;
if(hx>=0x7ff00000) return(x+x);
if((hx|__LO(x))==0)
return(x);
__HI(x) = hx;
if(hx<0x00100000)
{__HI(t)=0x43500000;
t*=x; __HI(t)=__HI(t)/3+B2;
}
else
__HI(t)=hx/3+B1;
r=t*t/x;
s=C+r*t;
t*=G+F/(s+E+D/s);
__LO(t)=0; __HI(t)+=0x00000001;
s=t*t;
r=x/s;
w=t+t;
r=(r-t)/(w+r);
t=t+t*r;
__HI(t) |= sign;
return(t);
}