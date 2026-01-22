#include <u.h>
#include <libc.h>
#include "fmtdef.h"
int
dorfmt(Fmt *f, const Rune *fmt)
{
Rune *rt, *rs;
int r;
char *t, *s;
int nfmt;
nfmt = f->nfmt;
for(;;){
if(f->runes){
rt = f->to;
rs = f->stop;
while((r = *fmt++) && r != '%'){
FMTRCHAR(f, rt, rs, r);
}
f->nfmt += rt - (Rune *)f->to;
f->to = rt;
if(!r)
return f->nfmt - nfmt;
f->stop = rs;
}else{
t = f->to;
s = f->stop;
while((r = *fmt++) && r != '%'){
FMTRUNE(f, t, f->stop, r);
}
f->nfmt += t - (char *)f->to;
f->to = t;
if(!r)
return f->nfmt - nfmt;
f->stop = s;
}
fmt = __fmtdispatch(f, (Rune*)fmt, 1);
if(fmt == nil)
return -1;
}
return 0;
}