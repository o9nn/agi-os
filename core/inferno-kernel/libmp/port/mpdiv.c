#include "os.h"
#include <mp.h>
#include "dat.h"
void
mpdiv(mpint *dividend, mpint *divisor, mpint *quotient, mpint *remainder)
{
int j, s, vn, sign;
mpdigit qd, *up, *vp, *qp;
mpint *u, *v, *t;
if(divisor->top == 0)
sysfatal("mpdiv: divide by zero");
if(mpmagcmp(dividend, divisor) < 0){
if(remainder != nil)
mpassign(dividend, remainder);
if(quotient != nil)
mpassign(mpzero, quotient);
return;
}
qd = divisor->p[divisor->top-1];
for(s = 0; (qd & mpdighi) == 0; s++)
qd <<= 1;
u = mpnew((dividend->top+2)*Dbits + s);
if(s == 0 && divisor != quotient && divisor != remainder) {
mpassign(dividend, u);
v = divisor;
} else {
mpleft(dividend, s, u);
v = mpnew(divisor->top*Dbits);
mpleft(divisor, s, v);
}
up = u->p+u->top-1;
vp = v->p+v->top-1;
vn = v->top;
if(*up >= *vp){
*++up = 0;
u->top++;
}
t = mpnew(4*Dbits);
qp = nil;
if(quotient != nil){
mpbits(quotient, (u->top - v->top)*Dbits);
quotient->top = u->top - v->top;
qp = quotient->p+quotient->top-1;
}
for(j = u->top; j > vn; j--){
mpdigdiv(up-1, *vp, &qd);
if(vn > 1) for(;;){
memset(t->p, 0, 3*Dbytes);
mpvecdigmuladd(vp-1, 2, qd, t->p);
if(mpveccmp(t->p, 3, up-2, 3) > 0)
qd--;
else
break;
}
sign = mpvecdigmulsub(v->p, vn, qd, up-vn);
if(sign < 0){
mpvecadd(up-vn, vn+1, v->p, vn, up-vn);
qd--;
}
if(qp != nil)
*qp-- = qd;
u->top--;
*up-- = 0;
}
if(qp != nil){
mpnorm(quotient);
if(dividend->sign != divisor->sign)
quotient->sign = -1;
}
if(remainder != nil){
mpright(u, s, remainder);
remainder->sign = dividend->sign;
}
mpfree(t);
mpfree(u);
if(v != divisor)
mpfree(v);
}