#include <util/atoi.h>
int
mach_atoi(const u_char *cp, int *nump)
{
int number;
const u_char *original;
original = cp;
for (number = 0; ('0' <= *cp) && (*cp <= '9'); cp++)
number = (number * 10) + (*cp - '0');
if (original == cp)
*nump = MACH_ATOI_DEFAULT;
else
*nump = number;
return(cp - original);
}