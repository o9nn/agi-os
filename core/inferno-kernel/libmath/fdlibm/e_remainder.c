#include "fdlibm.h"
static const double zero = 0.0;
double __ieee754_remainder(double x, double p)
{
int hx,hp;
unsigned sx,lx,lp;
double p_half;
hx = __HI(x);
lx = __LO(x);
hp = __HI(p);
lp = __LO(p);
sx = hx&0x80000000;
hp &= 0x7fffffff;
hx &= 0x7fffffff;
if((hp|lp)==0) return (x*p)/(x*p);
if((hx>=0x7ff00000)||
((hp>=0x7ff00000)&&
(((hp-0x7ff00000)|lp)!=0)))
return (x*p)/(x*p);
if (hp<=0x7fdfffff) x = __ieee754_fmod(x,p+p);
if (((hx-hp)|(lx-lp))==0) return zero*x;
x  = fabs(x);
p  = fabs(p);
if (hp<0x00200000) {
if(x+x>p) {
x-=p;
if(x+x>=p) x -= p;
}
} else {
p_half = 0.5*p;
if(x>p_half) {
x-=p;
if(x>=p_half) x -= p;
}
}
__HI(x) ^= sx;
return x;
}