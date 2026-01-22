#include "os.h"
#include <mp.h>
#include <libsec.h>
RSApriv*
rsagen(int nlen, int elen, int rounds)
{
mpint *p, *q, *e, *d, *phi, *n, *t1, *t2, *kp, *kq, *c2;
RSApriv *rsa;
p = mpnew(nlen/2);
q = mpnew(nlen/2);
n = mpnew(nlen);
e = mpnew(elen);
d = mpnew(0);
phi = mpnew(nlen);
genprime(p, nlen/2, rounds);
genprime(q, nlen - mpsignif(p) + 1, rounds);
mpmul(p, q, n);
mpsub(p, mpone, e);
mpsub(q, mpone, d);
mpmul(e, d, phi);
t1 = mpnew(0);
t2 = mpnew(0);
mprand(elen, genrandom, e);
if(mpcmp(e,mptwo) <= 0)
itomp(3, e);
for(;;){
mpextendedgcd(e, phi, t1, d, t2);
if(mpcmp(t1, mpone) == 0)
break;
mpadd(mpone, e, e);
}
mpfree(t1);
mpfree(t2);
c2 = mpnew(0);
mpinvert(p, q, c2);
kq = mpnew(0);
kp = mpnew(0);
mpsub(p, mpone, phi);
mpmod(d, phi, kp);
mpsub(q, mpone, phi);
mpmod(d, phi, kq);
rsa = rsaprivalloc();
rsa->pub.ek = e;
rsa->pub.n = n;
rsa->dk = d;
rsa->kp = kp;
rsa->kq = kq;
rsa->p = p;
rsa->q = q;
rsa->c2 = c2;
mpfree(phi);
return rsa;
}