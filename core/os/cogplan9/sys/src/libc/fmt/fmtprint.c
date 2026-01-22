#include <u.h>
#include <libc.h>
#include "fmtdef.h"
int
fmtprint(Fmt *f, char *fmt, ...)
{
va_list va;
int n;
va_start(va, fmt);
n = fmtvprint(f, fmt, va);
va_end(va);
return n;
}