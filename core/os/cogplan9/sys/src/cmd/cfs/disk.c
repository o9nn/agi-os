#include <u.h>
#include <libc.h>
#include "cformat.h"
#include "lru.h"
#include "bcache.h"
#include "disk.h"
int	icformat(Disk*, ulong);
int
dinit(Disk *d, int f, int psize, char *expname)
{
ulong	i;
uvlong	length;
char	buf[1024];
Bbuf	*b;
Dalloc	*ba;
Dir	*dir;
dir = dirfstat(f);
if(dir == nil){
perror("dinit: stat");
return -1;
}
length = dir->length;
free(dir);
if(seek(f, 0, 0) < 0){
perror("dinit: seek");
return -1;
}
if(read(f, buf, sizeof(buf)) != sizeof(buf)){
perror("dinit: read");
return -1;
}
ba = (Dalloc*)buf;
if(ba->bsize <= 0){
fprint(2, "dinit: bsize 0x%lux<= 0\n", ba->bsize);
return -1;
}
if((ba->bsize % psize) != 0){
fprint(2, "dinit: logical bsize (%lud) not multiple of physical (%ud)\n",
ba->bsize, psize);
return -1;
}
d->bsize = ba->bsize;
d->nb = length/d->bsize;
d->b2b = (d->bsize - sizeof(Dahdr))*8;
d->nab = (d->nb+d->b2b-1)/d->b2b;
d->p2b = d->bsize/sizeof(Dptr);
strncpy(d->name, ba->name, sizeof d->name);
if (expname != nil && strncmp(d->name, expname, sizeof d->name) != 0) {
fprint(2, "cfs: name mismatch\n");
return -1;
}
if(bcinit(d, f, d->bsize) < 0){
fprint(2, "dinit: couldn't init block cache\n");
return -1;
}
for(i = 0; i < d->nab; i++){
b = bcread(d, i);
if(b == 0){
perror("dinit: read");
return -1;
}
ba = (Dalloc*)b->data;
if(ba->magic != Amagic){
fprint(2, "dinit: bad magic in alloc block %uld\n", i);
return -1;
}
if(d->bsize != ba->bsize){
fprint(2, "dinit: bad bsize in alloc block %uld\n", i);
return -1;
}
if(d->nab != ba->nab){
fprint(2, "dinit: bad nab in alloc block %uld\n", i);
return -1;
}
if(strncmp(d->name, ba->name, sizeof(d->name))){
fprint(2, "dinit: bad name in alloc block %uld\n", i);
return -1;
}
}
return 0;
}
int
dformat(Disk *d, int f, char *name, ulong bsize, ulong psize)
{
int	i;
uvlong	length;
Bbuf	*b;
Dalloc	*ba;
Dir	*dir;
Dptr	dptr;
fprint(2, "formatting disk\n");
dir = dirfstat(f);
if(dir == nil)
return -1;
length = dir->length;
d->bsize = bsize;
if((d->bsize % psize) != 0){
fprint(2, "cfs: logical bsize not multiple of physical\n");
return -1;
}
d->nb = length/d->bsize;
d->b2b = (d->bsize - sizeof(Dahdr))*8;
d->nab = (d->nb+d->b2b-1)/d->b2b;
d->p2b = d->bsize/sizeof(Dptr);
if(bcinit(d, f, d->bsize) < 0)
return -1;
for(i = 0; i < d->nab; i++){
b = bcalloc(d, i);
if(b == 0){
perror("cfs: bcalloc");
return -1;
}
memset(b->data, 0, d->bsize);
ba = (Dalloc*)b->data;
ba->magic = Amagic;
ba->bsize = d->bsize;
ba->nab = d->nab;
strncpy(ba->name, name, sizeof(ba->name));
bcmark(d, b);
}
for(i = 0; i < d->nab; i++)
if(dalloc(d, &dptr) == Notabno){
fprint(2, "can't allocate allocation blocks\n");
return -1;
}
return bcsync(d);
}
static ulong
_balloc(Dalloc *ba, ulong max)
{
int len;
ulong i;
ulong m;
ulong v;
ulong *p, *e;
len = (max+BtoUL-1)/BtoUL;
for(p = ba->bits, e = p + len; p < e; p++)
if(*p != 0xFFFFFFFF)
break;
if(p == e)
return Notabno;
v = *p;
for(m = 1, i = 0; i < BtoUL; i++, m <<= 1)
if((m|v) != v)
break;
i += (p - ba->bits)*BtoUL;
if(i >= max)
return Notabno;
*p = v | m;
return i;
}
ulong
dalloc(Disk *d, Dptr *p)
{
ulong	bno, max, rv;
Bbuf	*b;
Dalloc	*ba;
max = d->nb;
for(bno = 0; bno < d->nab; bno++){
b = bcread(d, bno);
ba = (Dalloc*)b->data;
rv = _balloc(ba, max > d->b2b ? d->b2b : max);
if(rv != Notabno){
rv = bno*d->b2b + rv;
if(p){
p->start = p->end = 0;
p->bno = rv;
}
bcmark(d, b);
return rv;
}
max -= d->b2b;
}
if(p)
p->bno = Notabno;
return Notabno;
}
ulong
dpalloc(Disk *d, Dptr *p)
{
Bbuf *b;
Dptr *sp, *ep;
if(dalloc(d, p) == Notabno)
return Notabno;
b = bcalloc(d, p->bno);
if(b == 0)
return -1;
sp = (Dptr*)b->data;
for(ep = sp + d->p2b; sp < ep; sp++){
sp->bno = Notabno;
sp->start = sp->end = 0;
}
p->bno |= Indbno;
p->start = 0;
p->end = d->bsize;
bcmark(d, b);
return 0;
}
int
_bfree(Disk *d, ulong i)
{
ulong bno, m;
ulong *p;
Bbuf *b;
Dalloc *ba;
bno = i/d->b2b;
if(bno >= d->nab)
return -1;
b = bcread(d, bno);
if(b == 0)
return -1;
ba = (Dalloc*)b->data;
i -= bno*d->b2b;
p = ba->bits + (i/BtoUL);
m = 1<<(i%BtoUL);
*p &= ~m;
bcmark(d, b);
return 0;
}
int
dfree(Disk *d, Dptr *dp)
{
ulong bno;
Dptr *sp, *ep;
Bbuf *b;
bno = dp->bno;
dp->bno = Notabno;
if(bno == Notabno)
return 0;
if((bno & Indbno) == 0)
return _bfree(d, bno);
bno &= ~Indbno;
_bfree(d, bno);
b = bcread(d, bno);
if(b == 0)
return -1;
sp = (Dptr*)b->data;
for(ep = sp + d->p2b; sp < ep; sp++)
if(dfree(d, sp) < 0)
return -1;
return 0;
}