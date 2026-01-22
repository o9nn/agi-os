#include <inttypes.h>
#include <u.h>
#include <libc.h>
#include "fmtdef.h"
int
fmtfdflush(Fmt *f)
{
if(__fmtFdFlush(f) <= 0)
return -1;
return f->nfmt;
}
int
fmtfdinit(Fmt *f, int fd, char *buf, int size)
{
f->runes = 0;
f->start = buf;
f->to = buf;
f->stop = buf + size;
f->flush = __fmtFdFlush;
f->farg = (void*)(uintptr_t)fd;
f->nfmt = 0;
return 0;
}