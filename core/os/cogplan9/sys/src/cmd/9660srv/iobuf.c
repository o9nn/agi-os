#include <u.h>
#include <libc.h>
#include <auth.h>
#include <fcall.h>
#include "dat.h"
#include "fns.h"
#define	BUFPERCLUST	256
#define	NCLUST		16
int nclust = NCLUST;
static Ioclust*	iohead;
static Ioclust*	iotail;
static Ioclust*	getclust(Xdata*, vlong);
static void	putclust(Ioclust*);
static void	xread(Ioclust*);
void
iobuf_init(void)
{
int i, j, n;
Ioclust *c;
Iobuf *b;
uchar *mem;
n = nclust*sizeof(Ioclust) +
nclust*BUFPERCLUST*(sizeof(Iobuf)+Sectorsize);
mem = sbrk(n);
if(mem == (void*)-1)
panic(0, "iobuf_init");
memset(mem, 0, n);
for(i=0; i<nclust; i++){
c = (Ioclust*)mem;
mem += sizeof(Ioclust);
c->addr = -1;
c->prev = iotail;
if(iotail)
iotail->next = c;
iotail = c;
if(iohead == nil)
iohead = c;
c->buf = (Iobuf*)mem;
mem += BUFPERCLUST*sizeof(Iobuf);
c->iobuf = mem;
mem += BUFPERCLUST*Sectorsize;
for(j=0; j<BUFPERCLUST; j++){
b = &c->buf[j];
b->clust = c;
b->addr = -1;
b->iobuf = c->iobuf+j*Sectorsize;
}
}
}
void
purgebuf(Xdata *dev)
{
Ioclust *p;
for(p=iohead; p!=nil; p=p->next)
if(p->dev == dev){
p->addr = -1;
p->busy = 0;
}
}
static Ioclust*
getclust(Xdata *dev, vlong addr)
{
Ioclust *c, *f;
f = nil;
for(c=iohead; c; c=c->next){
if(!c->busy)
f = c;
if(c->addr == addr && c->dev == dev){
c->busy++;
return c;
}
}
if(f == nil)
panic(0, "out of buffers");
f->addr = addr;
f->dev = dev;
f->busy++;
if(waserror()){
f->addr = -1;
putclust(f);
nexterror();
}
xread(f);
poperror();
return f;
}
static void
putclust(Ioclust *c)
{
if(c->busy <= 0)
panic(0, "putbuf");
c->busy--;
if(c == iohead)
return;
c->prev->next = c->next;
if(c->next)
c->next->prev = c->prev;
else
iotail = c->prev;
c->prev = nil;
c->next = iohead;
iohead->prev = c;
iohead = c;
}
Iobuf*
getbuf(Xdata *dev, uvlong addr)
{
int off;
Ioclust *c;
off = addr%BUFPERCLUST;
c = getclust(dev, addr - off);
if(c->nbuf < off){
c->busy--;
error("short read or I/O error");
}
return &c->buf[off];
}
void
putbuf(Iobuf *b)
{
putclust(b->clust);
}
static void
xread(Ioclust *c)
{
int n;
Xdata *dev;
dev = c->dev;
seek(dev->dev, (vlong)c->addr * Sectorsize, 0);
n = readn(dev->dev, c->iobuf, BUFPERCLUST*Sectorsize);
if(n < Sectorsize)
error("short read or I/O error");
c->nbuf = n/Sectorsize;
}