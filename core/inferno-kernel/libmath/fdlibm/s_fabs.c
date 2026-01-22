#include "fdlibm.h"
double fabs(double x)
{
__HI(x) &= 0x7fffffff;
return x;
}