#include <sys/types.h>
#include <sys/syscall.h>
#include "dat.h"
int
segflush(void *a, ulong n)
{
ulong *p;
for (p = (ulong *)((ulong)a & ~7UL); (char *)p < (char *)a + n; p++)
__asm__("dcbst	0,%0\n\t"
"icbi	0,%0\n\t"
:
: "ar" (p)
);
__asm__("sync\n\t"
:
:
);
__asm__("isync\n\t"
:
:
);
return 0;
}