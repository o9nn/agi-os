#include "fdlibm.h"
int isnan(double x)
{
int hx,lx;
hx = (__HI(x)&0x7fffffff);
lx = __LO(x);
hx |= (unsigned)(lx|(-lx))>>31;
hx = 0x7ff00000 - hx;
return ((unsigned)(hx))>>31;
}