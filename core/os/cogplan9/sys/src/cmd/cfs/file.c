#include <u.h>
#include <libc.h>
#include "cformat.h"
#include "lru.h"
#include "bcache.h"
#include "disk.h"
#include "inode.h"
#include "file.h"
void
fmerge(Dptr *p, char *to, char *from, int start, int len)
{
int end;
end = start + len;
memmove(to+start, from, end-start);
if(start>p->end || p->start>end){
p->start = start;
p->end = end;
} else {
if(start < p->start)
p->start = start;
if(end > p->end)
p->end = end;
}
}
int
fbwrite(Icache *ic, Ibuf *b, char *a, ulong off, int len)
{
int wrinode;
ulong fbno;
Bbuf *dbb;
Bbuf *ibb;
Dptr *p;
Dptr t;
fbno = off / ic->bsize;
p = &b->inode.ptr;
ibb = 0;
wrinode = 0;
if(p->bno == Notabno){
wrinode = 1;
goto dowrite;
}
if(p->bno & Indbno){
ibb = bcread(ic, p->bno);
if(ibb == 0)
return -1;
p = (Dptr*)ibb->data;
p += fbno % ic->p2b;
goto dowrite;
}
if((p->fbno%ic->p2b) != (fbno%ic->p2b)){
t = *p;
dpalloc(ic, p);
if(p->bno == Notabno){
*p = t;
return -1;
}
ibb = bcalloc(ic, p->bno);
if(ibb == 0){
*p = t;
return -1;
}
p = (Dptr*)ibb->data;
p += t.fbno % ic->p2b;
*p = t;
p = (Dptr*)ibb->data;
p += fbno % ic->p2b;
}
wrinode = 1;
dowrite:
if(p->bno == Notabno){
dalloc(ic, p);
if(p->bno == Notabno)
return -1;
dbb = bcalloc(ic, p->bno);
} else {
dbb = bcread(ic, p->bno);
}
if(dbb == 0)
return -1;
if(p->fbno != fbno){
p->start = p->end = 0;
p->fbno = fbno;
}
fmerge(p, dbb->data, a, off % ic->bsize, len);
bcmark(ic, dbb);
if(ibb)
bcmark(ic, ibb);
if(wrinode)
if(iwrite(ic, b) < 0)
return -1;
return len;
}
long
fwrite(Icache *ic, Ibuf *b, char *a, ulong off, long n)
{
int len;
long sofar;
for(sofar = 0; sofar < n; sofar += len){
len = ic->bsize - ((off+sofar)%ic->bsize);
if(len > n - sofar)
len = n - sofar;
if(fbwrite(ic, b, a+sofar, off+sofar, len) < 0)
return sofar;
}
return sofar;
}
Dptr *
fpget(Icache *ic, Ibuf *b, ulong off)
{
ulong fbno;
long doff;
Bbuf *ibb;
Dptr *p, *p0, *pf;
fbno = off / ic->bsize;
p = &b->inode.ptr;
if(p->bno == Notabno)
return 0;
if(!(p->bno & Indbno)){
if(p->fbno > fbno)
return p;
if(p->fbno < fbno)
return 0;
doff = off % ic->bsize;
if(doff>=p->start && doff<p->end)
return p;
else
return 0;
}
ibb = bcread(ic, p->bno);
if(ibb == 0)
return 0;
p0 = (Dptr*)ibb->data;
pf = p0 + (fbno % ic->p2b);
if(pf->bno!=Notabno && pf->fbno==fbno){
doff = off % ic->bsize;
if(doff<pf->end)
return pf;
}
for(p = pf+1; p < p0 + ic->p2b; p++){
fbno++;
if(p->fbno==fbno && p->bno!=Notabno && p->start<p->end)
return p;
}
for(p = p0; p < pf; p++){
fbno++;
if(p->fbno==fbno && p->bno!=Notabno && p->start<p->end)
return p;
}
return 0;
}
long
fread(Icache *ic, Ibuf *b, char *a, ulong off, long n)
{
int len, start;
long sofar, gap;
Dptr *p;
Bbuf *bb;
for(sofar = 0; sofar < n; sofar += len, off += len){
len = n - sofar;
p = fpget(ic, b, off);
if(p == 0)
return sofar;
gap = (ic->bsize*p->fbno + p->start) - off;
if(gap>0)
if(sofar == 0)
return -gap;
else
return sofar;
bb = bcread(ic, p->bno);
if(bb == 0)
return sofar;
start = p->start - gap;
if(p->end - start < len)
len = p->end - start;
memmove(a + sofar, bb->data + start, len);
}
return sofar;
}