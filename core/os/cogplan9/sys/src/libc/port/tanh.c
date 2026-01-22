#include <u.h>
#include <libc.h>
double
tanh(double arg)
{
if(arg < 0) {
arg = -arg;
if(arg > 21)
return -1;
return -sinh(arg)/cosh(arg);
}
if(arg > 21)
return 1;
return sinh(arg)/cosh(arg);
}