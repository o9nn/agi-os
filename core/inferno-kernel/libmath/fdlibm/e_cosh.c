#include "fdlibm.h"
static const double one = 1.0, half=0.5, Huge = 1.0e300;
double __ieee754_cosh(double x)
{
double t,w;
int ix;
unsigned lx;
ix = __HI(x);
ix &= 0x7fffffff;
if(ix>=0x7ff00000) return x*x;
if(ix<0x3fd62e43) {
t = expm1(fabs(x));
w = one+t;
if (ix<0x3c800000) return w;
return one+(t*t)/(w+w);
}
if (ix < 0x40360000) {
t = __ieee754_exp(fabs(x));
return half*t+half/t;
}
if (ix < 0x40862E42) return half*__ieee754_exp(fabs(x));
lx = *( (((*(unsigned*)&one)>>29)) + (unsigned*)&x);
if (ix<0x408633CE ||
(ix==0x408633ce)&&(lx<=(unsigned)0x8fb9f87d)) {
w = __ieee754_exp(half*fabs(x));
t = half*w;
return t*w;
}
return Huge*Huge;
}