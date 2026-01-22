#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#include "arm.h"
#define ISPOW2(i) (((i) & ((i) - 1)) == 0)
int
ispow2(uvlong uvl)
{
return ISPOW2(uvl);
}
static int
isulpow2(ulong ul)
{
return ISPOW2(ul);
}
int
log2(ulong n)
{
int i;
i = BI2BY*BY2WD - 1 - clz(n);
if (n == 0 || !ISPOW2(n))
i++;
return i;
}