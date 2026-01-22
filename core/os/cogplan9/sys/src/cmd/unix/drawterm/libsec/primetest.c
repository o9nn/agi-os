#include "os.h"
#include <mp.h>
#include <libsec.h>
void
main(void)
{
mpint *z = mpnew(0);
mpint *p = mpnew(0);
mpint *q = mpnew(0);
mpint *nine = mpnew(0);
fmtinstall('B', mpconv);
strtomp("2492491", nil, 16, z);
strtomp("15662C00E811", nil, 16, p);
uitomp(9, nine);
if(probably_prime(z, 5) == 1)
fprint(2, "tricked primality test\n");
if(probably_prime(nine, 5) == 1)
fprint(2, "9 passed primality test!\n");
if(probably_prime(p, 25) == 1)
fprint(2, "ok\n");
DSAprimes(q, p, nil);
print("q=%B\np=%B\n", q, p);
exits(0);
}