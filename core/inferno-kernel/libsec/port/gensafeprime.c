#include "os.h"
#include <mp.h>
#include <libsec.h>
void
gensafeprime(mpint *p, mpint *alpha, int n, int accuracy)
{
mpint *q, *b;
q = mpnew(n-1);
while(1){
genprime(q, n-1, accuracy);
mpleft(q, 1, p);
mpadd(p, mpone, p);
if(probably_prime(p, accuracy))
break;
}
b = mpnew(0);
while(1){
mprand(n, genrandom, alpha);
mpmod(alpha, p, alpha);
mpmul(alpha, alpha, b);
mpmod(b, p, b);
if(mpcmp(b, mpone) == 0)
continue;
mpexp(alpha, q, p, b);
if(mpcmp(b, mpone) != 0)
break;
}
mpfree(b);
mpfree(q);
}