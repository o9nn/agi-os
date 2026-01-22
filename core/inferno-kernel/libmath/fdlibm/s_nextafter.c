#include "fdlibm.h"
double nextafter(double x, double y)
{
int	hx,hy,ix,iy;
unsigned lx,ly;
hx = __HI(x);
lx = __LO(x);
hy = __HI(y);
ly = __LO(y);
ix = hx&0x7fffffff;
iy = hy&0x7fffffff;
if(((ix>=0x7ff00000)&&((ix-0x7ff00000)|lx)!=0) ||
((iy>=0x7ff00000)&&((iy-0x7ff00000)|ly)!=0))
return x+y;
if(x==y) return x;
if((ix|lx)==0) {
__HI(x) = hy&0x80000000;
__LO(x) = 1;
y = x*x;
if(y==x) return y; else return x;
}
if(hx>=0) {
if(hx>hy||((hx==hy)&&(lx>ly))) {
if(lx==0) hx -= 1;
lx -= 1;
} else {
lx += 1;
if(lx==0) hx += 1;
}
} else {
if(hy>=0||hx>hy||((hx==hy)&&(lx>ly))){
if(lx==0) hx -= 1;
lx -= 1;
} else {
lx += 1;
if(lx==0) hx += 1;
}
}
hy = hx&0x7ff00000;
if(hy>=0x7ff00000) return x+x;
if(hy<0x00100000) {
y = x*x;
if(y!=x) {
__HI(y) = hx; __LO(y) = lx;
return y;
}
}
__HI(x) = hx; __LO(x) = lx;
return x;
}