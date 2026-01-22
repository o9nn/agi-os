#include <u.h>
#include <libc.h>
ulong
ntruerand(ulong n)
{
ulong m, r;
if(n > (1UL<<31))
m = n-1;
else
m = 0xFFFFFFFFUL - (2*((1UL<<31)%n))%n;
while((r = truerand()) > m)
;
return r%n;
}