#include "os.h"
#include <mp.h>
#include "dat.h"
mpint*
betomp(uchar *p, uint n, mpint *b)
{
int m, s;
mpdigit x;
if(b == nil){
b = mpnew(0);
setmalloctag(b, getcallerpc(&p));
}
while(*p == 0 && n > 1){
p++;
n--;
}
mpbits(b, n*8);
b->top = DIGITS(n*8);
m = b->top-1;
s = ((n-1)*8)%Dbits;
x = 0;
for(; n > 0; n--){
x |= ((mpdigit)(*p++)) << s;
s -= 8;
if(s < 0){
b->p[m--] = x;
s = Dbits-8;
x = 0;
}
}
return b;
}