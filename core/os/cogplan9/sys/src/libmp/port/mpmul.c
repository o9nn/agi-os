#include "os.h"
#include <mp.h>
#include "dat.h"
static void
mpkaratsuba(mpdigit *a, int alen, mpdigit *b, int blen, mpdigit *p)
{
mpdigit *t, *u0, *u1, *v0, *v1, *u0v0, *u1v1, *res, *diffprod;
int u0len, u1len, v0len, v1len, reslen;
int sign, n;
n = alen/2;
if(alen&1)
n++;
u0len = n;
u1len = alen-n;
if(blen > n){
v0len = n;
v1len = blen-n;
} else {
v0len = blen;
v1len = 0;
}
u0 = a;
u1 = a + u0len;
v0 = b;
v1 = b + v0len;
t = mallocz(Dbytes*5*(2*n+1), 1);
if(t == nil)
sysfatal("mpkaratsuba: %r");
u0v0 = t;
u1v1 = t + (2*n+1);
diffprod = t + 2*(2*n+1);
res = t + 3*(2*n+1);
reslen = 4*n+1;
sign = 1;
if(mpveccmp(u1, u1len, u0, u0len) < 0){
sign = -1;
mpvecsub(u0, u0len, u1, u1len, u0v0);
} else
mpvecsub(u1, u1len, u0, u1len, u0v0);
if(mpveccmp(v0, v0len, v1, v1len) < 0){
sign *= -1;
mpvecsub(v1, v1len, v0, v1len, u1v1);
} else
mpvecsub(v0, v0len, v1, v1len, u1v1);
mpvecmul(u0v0, u0len, u1v1, v0len, diffprod);
memset(t, 0, 2*(2*n+1)*Dbytes);
if(v1len > 0)
mpvecmul(u1, u1len, v1, v1len, u1v1);
mpvecmul(u0, u0len, v0, v0len, u0v0);
mpvecadd(res, reslen, u0v0, u0len+v0len, res);
mpvecadd(res+n, reslen-n, u0v0, u0len+v0len, res+n);
if(v1len > 0){
mpvecadd(res+n, reslen-n, u1v1, u1len+v1len, res+n);
mpvecadd(res+2*n, reslen-2*n, u1v1, u1len+v1len, res+2*n);
}
if(sign < 0)
mpvecsub(res+n, reslen-n, diffprod, u0len+v0len, res+n);
else
mpvecadd(res+n, reslen-n, diffprod, u0len+v0len, res+n);
memmove(p, res, (alen+blen)*Dbytes);
free(t);
}
#define KARATSUBAMIN 32
void
mpvecmul(mpdigit *a, int alen, mpdigit *b, int blen, mpdigit *p)
{
int i;
mpdigit d;
mpdigit *t;
if(alen < blen){
i = alen;
alen = blen;
blen = i;
t = a;
a = b;
b = t;
}
if(blen == 0){
memset(p, 0, Dbytes*(alen+blen));
return;
}
if(alen >= KARATSUBAMIN && blen > 1){
mpkaratsuba(a, alen, b, blen, p);
} else {
for(i = 0; i < blen; i++){
d = b[i];
if(d != 0)
mpvecdigmuladd(a, alen, d, &p[i]);
}
}
}
void
mpmul(mpint *b1, mpint *b2, mpint *prod)
{
mpint *oprod;
oprod = nil;
if(prod == b1 || prod == b2){
oprod = prod;
prod = mpnew(0);
}
prod->top = 0;
mpbits(prod, (b1->top+b2->top+1)*Dbits);
mpvecmul(b1->p, b1->top, b2->p, b2->top, prod->p);
prod->top = b1->top+b2->top+1;
prod->sign = b1->sign*b2->sign;
mpnorm(prod);
if(oprod != nil){
mpassign(prod, oprod);
mpfree(prod);
}
}