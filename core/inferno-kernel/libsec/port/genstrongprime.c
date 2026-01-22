#include "os.h"
#include <mp.h>
#include <libsec.h>
void
genstrongprime(mpint *p, int n, int accuracy)
{
mpint *s, *t, *r, *i;
if(n < 64)
n = 64;
s = mpnew(n/2);
genprime(s, (n/2)-16, accuracy);
t = mpnew(n/2);
genprime(t, n-mpsignif(s)-32, accuracy);
i = mpnew(16);
r = mpnew(0);
itomp(0x8000, i);
mpleft(t, 1, t);
mpmul(i, t, r);
mpadd(r, mpone, r);
for(;;){
if(probably_prime(r, 18))
break;
mpadd(r, t, r);
}
itomp(2, p);
mpsub(r, p, p);
mpexp(s, p, r, p);
mpmul(s, p, p);
mpleft(p, 1, p);
mpsub(p, mpone, p);
itomp(0x8000, i);
mpleft(r, 1, r);
mpmul(r, s, r);
mpmul(r, i, i);
mpadd(p, i, p);
for(;;){
if(probably_prime(p, accuracy))
break;
mpadd(p, r, p);
}
mpfree(i);
mpfree(s);
mpfree(r);
mpfree(t);
}