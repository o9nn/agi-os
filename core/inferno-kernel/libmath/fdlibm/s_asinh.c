#include "fdlibm.h"
static const double
one =  1.00000000000000000000e+00,
ln2 =  6.93147180559945286227e-01,
Huge=  1.00000000000000000000e+300;
double asinh(double x)
{
double t,w;
int hx,ix;
hx = __HI(x);
ix = hx&0x7fffffff;
if(ix>=0x7ff00000) return x+x;
if(ix< 0x3e300000) {
if(Huge+x>one) return x;
}
if(ix>0x41b00000) {
w = __ieee754_log(fabs(x))+ln2;
} else if (ix>0x40000000) {
t = fabs(x);
w = __ieee754_log(2.0*t+one/(sqrt(x*x+one)+t));
} else {
t = x*x;
w =log1p(fabs(x)+t/(one+sqrt(one+t)));
}
if(hx>0) return w; else return -w;
}