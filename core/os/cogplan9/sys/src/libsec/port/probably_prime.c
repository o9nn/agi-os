#include "os.h"
#include <mp.h>
#include <libsec.h>
int
probably_prime(mpint *n, int nrep)
{
int j, k, rep, nbits, isprime;
mpint *nm1, *q, *x, *y, *r;
if(n->sign < 0)
sysfatal("negative prime candidate");
if(nrep <= 0)
nrep = 18;
k = mptoi(n);
if(k == 2)
return 1;
if(k < 2)
return 0;
if((n->p[0] & 1) == 0)
return 0;
if(smallprimetest(n) < 0)
return 0;
x = uitomp(2, nil);
y = mpnew(0);
mpexp(x, n, n, y);
k = mptoi(y);
if(k != 2){
mpfree(x);
mpfree(y);
return 0;
}
nbits = mpsignif(n);
nm1 = mpnew(nbits);
mpsub(n, mpone, nm1);
k = mplowbits0(nm1);
q = mpnew(0);
mpright(nm1, k, q);
for(rep = 0; rep < nrep; rep++){
for(;;){
r = mprand(nbits, prng, nil);
mpmod(r, nm1, x);
mpfree(r);
if(mpcmp(x, mpone) > 0)
break;
}
mpexp(x, q, n, y);
if(mpcmp(y, mpone) == 0 || mpcmp(y, nm1) == 0)
continue;
for(j = 1;; j++){
if(j >= k) {
isprime = 0;
goto done;
}
mpmul(y, y, x);
mpmod(x, n, y);
if(mpcmp(y, nm1) == 0)
break;
if(mpcmp(y, mpone) == 0){
isprime = 0;
goto done;
}
}
}
isprime = 1;
done:
mpfree(y);
mpfree(x);
mpfree(q);
mpfree(nm1);
return isprime;
}