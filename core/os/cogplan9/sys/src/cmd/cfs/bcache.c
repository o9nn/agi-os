#include <u.h>
#include <libc.h>
#include "cformat.h"
#include "lru.h"
#include "bcache.h"
int
bcinit(Bcache *bc, int f, int bsize)
{
Bbuf *b;
bc->dfirst = 0;
bc->bsize = bsize;
bc->f = f;
lruinit(bc);
for(b = bc->bb; b < &bc->bb[Nbcache]; b++){
b->inuse = 0;
b->next = 0;
b->dirty = 0;
if(b->data == 0)
b->data = (char *)malloc(bc->bsize);
if(b->data == 0)
return -1;
lruadd(bc, b);
}
return 0;
}
Bbuf *
bcfind(Bcache *bc, ulong bno)
{
Bbuf *b;
if(bno == Notabno)
error("bcfind: Notabno");
bno &= ~Indbno;
for(b = bc->bb; b < &bc->bb[Nbcache]; b++)
if(b->inuse && b->bno==bno)
goto out;
b = (Bbuf*)bc->lnext;
out:
if(b->dirty)
if(bcwrite(bc, b) < 0)
warning("writing dirty page");
lruref(bc, b);
return b;
}
Bbuf *
bcalloc(Bcache *bc, ulong bno)
{
Bbuf *b;
b = bcfind(bc, bno);
bno &= ~Indbno;
b->bno = bno;
b->inuse = 1;
return b;
}
Bbuf *
bcread(Bcache *bc, ulong bno)
{
Bbuf *b;
b = bcfind(bc, bno);
bno &= ~Indbno;
if(b->bno!=bno || !b->inuse)
if(bread(bc, bno, b->data) < 0){
b->inuse = 0;
return 0;
}
b->bno = bno;
b->inuse = 1;
return b;
}
void
bcmark(Bcache *bc, Bbuf *b)
{
lruref(bc, b);
if(b->dirty){
bcwrite(bc, b);
return;
}
b->dirty = 1;
if(bc->dfirst)
bc->dlast->next = b;
else
bc->dfirst = b;
bc->dlast = b;
}
int
bcwrite(Bcache *bc, Bbuf *b)
{
Bbuf *nb;
while(nb = bc->dfirst){
if(bwrite(bc, nb->bno, nb->data) < 0)
return -1;
nb->dirty = 0;
bc->dfirst = nb->next;
nb->next = 0;
if(nb == b)
return 0;
}
if(bwrite(bc, b->bno, b->data) < 0)
return -1;
b->dirty = 0;
b->next = 0;
return 0;
}
int
bcsync(Bcache *bc)
{
if(bc->dfirst)
return bcwrite(bc, bc->dlast);
return 0;
}
int
bread(Bcache *bc, ulong bno, void *buf)
{
uvlong x = (uvlong)bno * bc->bsize;
if(pread(bc->f, buf, bc->bsize, x) != bc->bsize)
return -1;
return 0;
}
int
bwrite(Bcache *bc, ulong bno, void *buf)
{
uvlong x = (uvlong)bno * bc->bsize;
if(pwrite(bc->f, buf, bc->bsize, x) != bc->bsize)
return -1;
return 0;
}