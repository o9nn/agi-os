#include "fdlibm.h"
static const double one = 1.0, Huge = 1e300;
static double zero = 0.0;
double __ieee754_atanh(double x)
{
double t;
int hx,ix;
unsigned lx;
hx = __HI(x);
lx = __LO(x);
ix = hx&0x7fffffff;
if ((ix|((lx|(-lx))>>31))>0x3ff00000)
return (x-x)/(x-x);
if(ix==0x3ff00000)
return x/zero;
if(ix<0x3e300000&&(Huge+x)>zero) return x;
__HI(x) = ix;
if(ix<0x3fe00000) {
t = x+x;
t = 0.5*log1p(t+t*x/(one-x));
} else
t = 0.5*log1p((x+x)/(one-x));
if(hx>=0) return t; else return -t;
}