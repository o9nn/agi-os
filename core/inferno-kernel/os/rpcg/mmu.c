#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
void
mmuinit(void)
{
}
int
segflush(void *a, ulong n)
{
dcflush(a, n);
icflush(a, n);
return 0;
}