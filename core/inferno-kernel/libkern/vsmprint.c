#include "lib9.h"
#include "fmtdef.h"
static int
fmtStrFlush(Fmt *f)
{
char *s;
int n;
if(f->start == nil)
return 0;
n = (int)f->farg;
n += 256;
f->farg = (void*)n;
s = f->start;
f->start = realloc(s, n);
if(f->start == nil){
free(s);
f->to = nil;
f->stop = nil;
return 0;
}
f->to = (char*)f->start + ((char*)f->to - s);
f->stop = (char*)f->start + n - 1;
return 1;
}
int
fmtstrinit(Fmt *f)
{
int n;
memset(f, 0, sizeof(*f));
n = 32;
f->start = malloc(n);
if(f->start == nil)
return -1;
f->to = f->start;
f->stop = (char*)f->start + n - 1;
f->flush = fmtStrFlush;
f->farg = (void*)n;
f->nfmt = 0;
return 0;
}
char*
vsmprint(char *fmt, va_list args)
{
Fmt f;
int n;
if(fmtstrinit(&f) < 0)
return nil;
va_copy(f.args, args);
n = dofmt(&f, fmt);
va_end(f.args);
if(n < 0){
free(f.start);
f.start = nil;
return nil;
}
return fmtstrflush(&f);
}