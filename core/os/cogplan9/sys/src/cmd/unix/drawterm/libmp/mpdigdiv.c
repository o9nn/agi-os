#include "os.h"
#include <mp.h>
#include "dat.h"
void
mpdigdiv(mpdigit *dividend, mpdigit divisor, mpdigit *quotient)
{
mpdigit hi, lo, q, x, y;
int i;
hi = dividend[1];
lo = dividend[0];
if(hi >= divisor || divisor == 0){
divisor = 0;
*quotient = ~divisor;
return;
}
q = 0;
x = divisor;
for(i = Dbits-1; hi > 0 && i >= 0; i--){
x >>= 1;
if(x > hi)
continue;
y = divisor<<i;
if(x == hi && y > lo)
continue;
if(y > lo)
hi--;
lo -= y;
hi -= x;
q |= 1<<i;
}
q += lo/divisor;
*quotient = q;
}