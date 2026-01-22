#include "os.h"
#include <mp.h>
#include <libsec.h>
int
dsaverify(DSApub *pub, DSAsig *sig, mpint *m)
{
int rv = -1;
mpint *u1, *u2, *v, *sinv;
if(sig->r->sign < 0 || mpcmp(sig->r, pub->q) >= 0)
return rv;
if(sig->s->sign < 0 || mpcmp(sig->s, pub->q) >= 0)
return rv;
u1 = mpnew(0);
u2 = mpnew(0);
v = mpnew(0);
sinv = mpnew(0);
mpextendedgcd(sig->s, pub->q, u1, sinv, v);
if(mpcmp(u1, mpone) != 0)
goto out;
mpmul(sinv, m, u1);
mpmod(u1, pub->q, u1);
mpmul(sig->r, sinv, u2);
mpmod(u2, pub->q, u2);
mpexp(pub->alpha, u1, pub->p, sinv);
mpexp(pub->key, u2, pub->p, v);
mpmul(sinv, v, v);
mpmod(v, pub->p, v);
mpmod(v, pub->q, v);
if(mpcmp(v, sig->r) == 0)
rv = 0;
out:
mpfree(v);
mpfree(u1);
mpfree(u2);
mpfree(sinv);
return rv;
}