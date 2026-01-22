#include "fdlibm.h"
int finite(double x)
{
int hx;
hx = __HI(x);
return (unsigned)((hx&0x7fffffff)-0x7ff00000)>>31;
}