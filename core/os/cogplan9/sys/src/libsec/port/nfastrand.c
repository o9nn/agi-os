#include <u.h>
#include <libc.h>
#include <libsec.h>
#define Maxrand	((1UL<<31)-1)
ulong
nfastrand(ulong n)
{
ulong m, r;
if(n > Maxrand)
sysfatal("nfastrand: n too large");
m = Maxrand - Maxrand % n;
while((r = fastrand()) >= m)
;
return r%n;
}