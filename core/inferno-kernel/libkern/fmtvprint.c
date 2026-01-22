#include "lib9.h"
#include "fmtdef.h"
int
fmtvprint(Fmt *f, char *fmt, va_list args)
{
va_list va;
int n;
f->flags = 0;
f->width = 0;
f->prec = 0;
va_copy(va, f->args);
va_end(f->args);
va_copy(f->args, args);
n = dofmt(f, fmt);
f->flags = 0;
f->width = 0;
f->prec = 0;
va_end(f->args);
va_copy(f->args, va);
va_end(va);
if(n >= 0)
return 0;
return n;
}