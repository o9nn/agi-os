#include "os.h"
#include <mp.h>
#include <libsec.h>
mpint*
rsadecrypt(RSApriv *rsa, mpint *in, mpint *out)
{
mpint *v1, *v2;
if(out == nil)
out = mpnew(0);
v1 = mpnew(0);
mpmod(in, rsa->p, v1);
v2 = mpnew(0);
mpmod(in, rsa->q, v2);
mpexp(v1, rsa->kp, rsa->p, v1);
mpexp(v2, rsa->kq, rsa->q, v2);
mpsub(v2, v1, v2);
mpmul(v2, rsa->c2, v2);
mpmod(v2, rsa->q, v2);
mpmul(v2, rsa->p, out);
mpadd(v1, out, out);
mpfree(v1);
mpfree(v2);
return out;
}