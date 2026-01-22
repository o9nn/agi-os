#include "lib9.h"
#include "fmtdef.h"
int
fmtfdflush(Fmt *f)
{
if(_fmtFdFlush(f) <= 0)
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
f->flush = _fmtFdFlush;
f->farg = (void*)fd;
f->nfmt = 0;
return 0;
}