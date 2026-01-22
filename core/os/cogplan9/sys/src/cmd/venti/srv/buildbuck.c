#include "stdinc.h"
#include "dat.h"
#include "fns.h"
struct IEStream
{
Part	*part;
u64int	off;
u64int	n;
u32int	size;
u8int	*buf;
u8int	*pos;
u8int	*epos;
};
IEStream*
initiestream(Part *part, u64int off, u64int clumps, u32int size)
{
IEStream *ies;
ies = MKZ(IEStream);
ies->buf = MKN(u8int, size);
ies->epos = ies->buf;
ies->pos = ies->epos;
ies->off = off;
ies->n = clumps;
ies->size = size;
ies->part = part;
return ies;
}
void
freeiestream(IEStream *ies)
{
if(ies == nil)
return;
free(ies->buf);
free(ies);
}
static u8int*
peekientry(IEStream *ies)
{
u32int n, nn;
n = ies->epos - ies->pos;
if(n < IEntrySize){
memmove(ies->buf, ies->pos, n);
ies->epos = &ies->buf[n];
ies->pos = ies->buf;
nn = ies->size;
if(nn > ies->n * IEntrySize)
nn = ies->n * IEntrySize;
nn -= n;
if(nn == 0)
return nil;
if(readpart(ies->part, ies->off, ies->epos, nn) < 0){
seterr(EOk, "can't read sorted index entries: %r");
return nil;
}
ies->epos += nn;
ies->off += nn;
}
return ies->pos;
}
static u32int
iebuck(Index *ix, u8int *b, IBucket *ib, IEStream *ies)
{
USED(ies);
USED(ib);
return hashbits(b, 32) / ix->div;
}
u32int
buildbucket(Index *ix, IEStream *ies, IBucket *ib, uint maxdata)
{
IEntry ie1, ie2;
u8int *b;
u32int buck;
buck = TWID32;
ib->n = 0;
while(ies->n){
b = peekientry(ies);
if(b == nil)
return TWID32;
if(ib->n == 0)
buck = iebuck(ix, b, ib, ies);
else{
if(buck != iebuck(ix, b, ib, ies))
break;
if(ientrycmp(&ib->data[(ib->n - 1)* IEntrySize], b) == 0){
unpackientry(&ie1, &ib->data[(ib->n - 1)* IEntrySize]);
unpackientry(&ie2, b);
seterr(EOk, "duplicate index entry for score=%V type=%d", ie1.score, ie1.ia.type);
ib->n--;
if(ie1.ia.addr > ie2.ia.addr)
memmove(b, &ib->data[ib->n * IEntrySize], IEntrySize);
}
}
if((ib->n+1)*IEntrySize > maxdata){
seterr(EOk, "bucket overflow");
return TWID32;
}
memmove(&ib->data[ib->n * IEntrySize], b, IEntrySize);
ib->n++;
ies->n--;
ies->pos += IEntrySize;
}
return buck;
}