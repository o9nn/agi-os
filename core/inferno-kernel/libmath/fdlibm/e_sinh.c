#include "fdlibm.h"
static const double one = 1.0, sHuge = 1.0e307;
double __ieee754_sinh(double x)
{
double t,w,h;
int ix,jx;
unsigned lx;
jx = __HI(x);
ix = jx&0x7fffffff;
if(ix>=0x7ff00000) return x+x;
h = 0.5;
if (jx<0) h = -h;
if (ix < 0x40360000) {
if (ix<0x3e300000)
if(sHuge+x>one) return x;
t = expm1(fabs(x));
if(ix<0x3ff00000) return h*(2.0*t-t*t/(t+one));
return h*(t+t/(t+one));
}
if (ix < 0x40862E42)  return h*__ieee754_exp(fabs(x));
lx = *( (((*(unsigned*)&one)>>29)) + (unsigned*)&x);
if (ix<0x408633CE || (ix==0x408633ce)&&(lx<=(unsigned)0x8fb9f87d)) {
w = __ieee754_exp(0.5*fabs(x));
t = h*w;
return t*w;
}
return x*sHuge;
}