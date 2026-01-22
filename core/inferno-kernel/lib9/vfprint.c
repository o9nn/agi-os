#include "lib9.h"
#include "fmtdef.h"
int
_fmtFdFlush(Fmt *f)
{
int n;
n = (char*)f->to - (char*)f->start;
if(n && write((int)f->farg, f->start, n) != n)
return 0;
f->to = f->start;
return 1;
}
int
vfprint(int fd, char *fmt, va_list args)
{
Fmt f;
char buf[256];
int n;
fmtfdinit(&f, fd, buf, sizeof(buf));
va_copy(f.args, args);
n = dofmt(&f, fmt);
va_end(f.args);
if(n > 0 && _fmtFdFlush(&f) == 0)
return -1;
return n;
}