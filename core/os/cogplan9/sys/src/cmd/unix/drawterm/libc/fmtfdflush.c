#include <inttypes.h>
#include <u.h>
#include <libc.h>
#include "fmtdef.h"
int
__fmtFdFlush(Fmt *f)
{
int n;
n = (char*)f->to - (char*)f->start;
if(n && write((uintptr_t)f->farg, f->start, n) != n)
return 0;
f->to = f->start;
return 1;
}