#include "stdinc.h"
#include "dat.h"
#include "fns.h"
enum
{
MinBufSize = 64*1024,
MaxBufSize = 4*1024*1024,
};
int dumb;
int errors;
char **isect;
int nisect;
int bloom;
int zero;
u32int isectmem;
u64int totalbuckets;
u64int totalclumps;
Channel *arenadonechan;
Channel *isectdonechan;
Index *ix;
u64int arenaentries;
u64int skipentries;
u64int indexentries;
static int shouldprocess(ISect*);
static void isectproc(void*);
static void arenapartproc(void*);
void
usage(void)
{
fprint(2, "usage: buildindex [-b] [-i isect]... [-M imem] venti.conf\n");
threadexitsall("usage");
}
void
threadmain(int argc, char *argv[])
{
int fd, i, napart, nfinish, maxdisks;
u32int bcmem, imem;
Config conf;
Part *p;
maxdisks = 100000;
ventifmtinstall();
imem = 256*1024*1024;
ARGBEGIN{
case 'b':
bloom = 1;
break;
case 'd':
dumb = 1;
break;
case 'i':
isect = vtrealloc(isect, (nisect+1)*sizeof(isect[0]));
isect[nisect++] = EARGF(usage());
break;
case 'M':
imem = unittoull(EARGF(usage()));
break;
case 'm':
maxdisks = atoi(EARGF(usage()));
break;
default:
usage();
break;
}ARGEND
if(argc != 1)
usage();
if(initventi(argv[0], &conf) < 0)
sysfatal("can't init venti: %r");
ix = mainindex;
if(nisect == 0 && ix->bloom)
bloom = 1;
if(bloom && ix->bloom && resetbloom(ix->bloom) < 0)
sysfatal("loadbloom: %r");
if(bloom && !ix->bloom)
sysfatal("-b specified but no bloom filter");
if(!bloom)
ix->bloom = nil;
isectmem = imem/ix->nsects;
p = nil;
for(i=0; i<ix->narenas; i++){
if(ix->arenas[i]->part != p){
p = ix->arenas[i]->part;
if((fd = open(p->filename, OREAD)) < 0)
sysfatal("cannot reopen %s: %r", p->filename);
dup(fd, p->fd);
close(fd);
}
}
bcmem = maxblocksize * (mainindex->narenas + 16);
if(0) fprint(2, "initialize %d bytes of disk block cache\n", bcmem);
initdcache(bcmem);
totalclumps = 0;
for(i=0; i<ix->narenas; i++)
totalclumps += ix->arenas[i]->diskstats.clumps;
totalbuckets = 0;
for(i=0; i<ix->nsects; i++)
totalbuckets += ix->sects[i]->blocks;
fprint(2, "%,lld clumps, %,lld buckets\n", totalclumps, totalbuckets);
fprint(2, "%T read index\n");
isectdonechan = chancreate(sizeof(void*), 0);
for(i=0; i<ix->nsects; i++){
if(shouldprocess(ix->sects[i])){
ix->sects[i]->writechan = chancreate(sizeof(IEntry), 0);
vtproc(isectproc, ix->sects[i]);
}
}
for(i=0; i<nisect; i++)
if(isect[i])
fprint(2, "warning: did not find index section %s\n", isect[i]);
p = nil;
napart = 0;
nfinish = 0;
arenadonechan = chancreate(sizeof(void*), 0);
for(i=0; i<ix->narenas; i++){
if(ix->arenas[i]->part != p){
p = ix->arenas[i]->part;
vtproc(arenapartproc, p);
if(++napart >= maxdisks){
recvp(arenadonechan);
nfinish++;
}
}
}
for(nfinish=0; nfinish<napart; nfinish++)
recvp(arenadonechan);
for(i=0; i<ix->nsects; i++)
if(ix->sects[i]->writechan)
send(ix->sects[i]->writechan, nil);
for(i=0; i<ix->nsects; i++)
if(ix->sects[i]->writechan)
recvp(isectdonechan);
if(ix->bloom && writebloom(ix->bloom) < 0)
fprint(2, "writing bloom filter: %r\n");
fprint(2, "%T done arenaentries=%,lld indexed=%,lld (nskip=%,lld)\n",
arenaentries, indexentries, skipentries);
threadexitsall(nil);
}
static int
shouldprocess(ISect *is)
{
int i;
if(nisect == 0)
return 1;
for(i=0; i<nisect; i++)
if(isect[i] && strcmp(isect[i], is->name) == 0){
isect[i] = nil;
return 1;
}
return 0;
}
static void
add(u64int *a, u64int n)
{
static Lock l;
lock(&l);
*a += n;
unlock(&l);
}
enum
{
ClumpChunks = 32*1024,
};
static void
arenapartproc(void *v)
{
int i, j, n, nskip, x;
u32int clump;
u64int addr, tot;
Arena *a;
ClumpInfo *ci, *cis;
IEntry ie;
Part *p;
p = v;
threadsetname("arenaproc %s", p->name);
nskip = 0;
tot = 0;
cis = MKN(ClumpInfo, ClumpChunks);
for(i=0; i<ix->narenas; i++){
a = ix->arenas[i];
if(a->part != p)
continue;
if(a->memstats.clumps)
fprint(2, "%T arena %s: %d entries\n",
a->name, a->memstats.clumps);
addr = ix->amap[i].start + a->memstats.used;
for(clump=a->memstats.clumps; clump > 0; clump-=n){
n = ClumpChunks;
if(n > clump)
n = clump;
if(readclumpinfos(a, clump-n, cis, n) != n){
fprint(2, "%T arena %s: directory read: %r\n", a->name);
errors = 1;
break;
}
for(j=n-1; j>=0; j--){
ci = &cis[j];
ie.ia.type = ci->type;
ie.ia.size = ci->uncsize;
addr -= ci->size + ClumpSize;
ie.ia.addr = addr;
ie.ia.blocks = (ci->size + ClumpSize + (1<<ABlockLog)-1) >> ABlockLog;
scorecp(ie.score, ci->score);
if(ci->type == VtCorruptType)
nskip++;
else{
tot++;
x = indexsect(ix, ie.score);
assert(0 <= x && x < ix->nsects);
if(ix->sects[x]->writechan)
send(ix->sects[x]->writechan, &ie);
if(ix->bloom)
markbloomfilter(ix->bloom, ie.score);
}
}
}
if(addr != ix->amap[i].start)
fprint(2, "%T arena %s: clump miscalculation %lld != %lld\n", a->name, addr, ix->amap[i].start);
}
add(&arenaentries, tot);
add(&skipentries, nskip);
sendp(arenadonechan, p);
}
static u32int
score2bucket(ISect *is, uchar *score)
{
u32int b;
b = hashbits(score, 32)/ix->div;
if(b < is->start || b >= is->stop){
fprint(2, "score2bucket: score=%V div=%d b=%ud start=%ud stop=%ud\n",
score, ix->div, b, is->start, is->stop);
}
assert(is->start <= b && b < is->stop);
return b - is->start;
}
static u32int
offset2bucket(ISect *is, u64int offset)
{
u32int b;
assert(is->blockbase <= offset);
offset -= is->blockbase;
b = offset/is->blocksize;
assert(b < is->stop-is->start);
return b;
}
static u64int
bucket2offset(ISect *is, u32int b)
{
assert(b <= is->stop-is->start);
return is->blockbase + (u64int)b*is->blocksize;
}
typedef struct Buf Buf;
struct Buf
{
Part *part;
uchar *bp;
uchar *ep;
uchar *wp;
u64int boffset;
u64int woffset;
u64int eoffset;
u32int nentry;
};
static void
bflush(Buf *buf)
{
u32int bufsize;
if(buf->woffset >= buf->eoffset)
sysfatal("buf index chunk overflow - need bigger index");
bufsize = buf->ep - buf->bp;
if(writepart(buf->part, buf->woffset, buf->bp, bufsize) < 0){
fprint(2, "write %s: %r\n", buf->part->name);
errors = 1;
}
buf->woffset += bufsize;
memset(buf->bp, 0, bufsize);
buf->wp = buf->bp;
}
static void
bwrite(Buf *buf, IEntry *ie)
{
if(buf->wp+IEntrySize > buf->ep)
bflush(buf);
assert(buf->bp <= buf->wp && buf->wp < buf->ep);
packientry(ie, buf->wp);
buf->wp += IEntrySize;
assert(buf->bp <= buf->wp && buf->wp <= buf->ep);
buf->nentry++;
}
typedef struct Minibuf Minibuf;
struct Minibuf
{
u64int boffset;
u64int roffset;
u64int woffset;
u64int eoffset;
u32int nentry;
u32int nwentry;
};
typedef struct IEntryLink IEntryLink;
typedef struct IPool IPool;
struct IEntryLink
{
uchar ie[IEntrySize];
IEntryLink *next;
};
struct IPool
{
ISect *isect;
u32int buck0;
u32int mbufbuckets;
IEntryLink *entry;
u32int nentry;
IEntryLink *free;
u32int nfree;
Minibuf *mbuf;
u32int nmbuf;
IEntryLink **mlist;
u32int *mcount;
u32int bufsize;
uchar *rbuf;
uchar *wbuf;
u32int epbuf;
};
static IPool*
mkipool(ISect *isect, Minibuf *mbuf, u32int nmbuf,
u32int mbufbuckets, u32int bufsize)
{
u32int i, nentry;
uchar *data;
IPool *p;
IEntryLink *l;
nentry = (nmbuf+1)*bufsize / IEntrySize;
p = ezmalloc(sizeof(IPool)
+nentry*sizeof(IEntry)
+nmbuf*sizeof(IEntryLink*)
+nmbuf*sizeof(u32int)
+3*bufsize);
p->isect = isect;
p->mbufbuckets = mbufbuckets;
p->bufsize = bufsize;
p->entry = (IEntryLink*)(p+1);
p->nentry = nentry;
p->mlist = (IEntryLink**)(p->entry+nentry);
p->mcount = (u32int*)(p->mlist+nmbuf);
p->nmbuf = nmbuf;
p->mbuf = mbuf;
data = (uchar*)(p->mcount+nmbuf);
data += bufsize - (uintptr)data%bufsize;
p->rbuf = data;
p->wbuf = data+bufsize;
p->epbuf = bufsize/IEntrySize;
for(i=0; i<p->nentry; i++){
l = &p->entry[i];
l->next = p->free;
p->free = l;
p->nfree++;
}
return p;
}
static void
ipoolinsert(IPool *p, uchar *ie)
{
u32int buck, x;
IEntryLink *l;
assert(p->free != nil);
buck = score2bucket(p->isect, ie);
x = (buck-p->buck0) / p->mbufbuckets;
if(x >= p->nmbuf){
fprint(2, "buck=%ud mbufbucket=%ud x=%ud\n",
buck, p->mbufbuckets, x);
}
assert(x < p->nmbuf);
l = p->free;
p->free = l->next;
p->nfree--;
memmove(l->ie, ie, IEntrySize);
l->next = p->mlist[x];
p->mlist[x] = l;
p->mcount[x]++;
}
static u32int
ipoolgetbuf(IPool *p, u32int x)
{
uchar *bp, *ep, *wp;
IEntryLink *l;
u32int n;
bp = p->wbuf;
ep = p->wbuf + p->bufsize;
n = 0;
assert(x < p->nmbuf);
for(wp=bp; wp+IEntrySize<=ep && p->mlist[x]; wp+=IEntrySize){
l = p->mlist[x];
p->mlist[x] = l->next;
p->mcount[x]--;
memmove(wp, l->ie, IEntrySize);
l->next = p->free;
p->free = l;
p->nfree++;
n++;
}
memset(wp, 0, ep-wp);
return n;
}
static void
ipoolloadblock(IPool *p, Minibuf *mb)
{
u32int i, n;
assert(mb->nentry > 0);
assert(mb->roffset >= mb->woffset);
assert(mb->roffset < mb->eoffset);
n = p->bufsize/IEntrySize;
if(n > mb->nentry)
n = mb->nentry;
if(readpart(p->isect->part, mb->roffset, p->rbuf, p->bufsize) < 0)
fprint(2, "readpart %s: %r\n", p->isect->part->name);
else{
for(i=0; i<n; i++)
ipoolinsert(p, p->rbuf+i*IEntrySize);
}
mb->nentry -= n;
mb->roffset += p->bufsize;
}
static void
ipoolflush0(IPool *pool, u32int x)
{
u32int bufsize;
Minibuf *mb;
mb = pool->mbuf+x;
bufsize = pool->bufsize;
mb->nwentry += ipoolgetbuf(pool, x);
if(mb->nentry > 0 && mb->roffset == mb->woffset){
assert(pool->nfree >= pool->bufsize/IEntrySize);
ipoolloadblock(pool, mb);
}
if(writepart(pool->isect->part, mb->woffset, pool->wbuf, bufsize) < 0)
fprint(2, "writepart %s: %r\n", pool->isect->part->name);
mb->woffset += bufsize;
}
static void
ipoolflush1(IPool *pool)
{
u32int i;
assert(pool->nfree <= pool->epbuf);
for(i=0; i<pool->nmbuf; i++){
if(pool->mcount[i] >= pool->epbuf){
ipoolflush0(pool, i);
return;
}
}
sysfatal("ipoolflush1");
}
static void
ipoolflush(IPool *pool)
{
u32int i;
for(i=0; i<pool->nmbuf; i++)
while(pool->mlist[i])
ipoolflush0(pool, i);
assert(pool->nfree == pool->nentry);
}
static int
ientrycmpaddr(const void *va, const void *vb)
{
int i;
uchar *a, *b;
a = (uchar*)va;
b = (uchar*)vb;
i = ientrycmp(a, b);
if(i)
return i;
return -memcmp(a+IEntryAddrOff, b+IEntryAddrOff, 8);
}
static void
zerorange(Part *p, u64int o, u64int e)
{
static uchar zero[MaxIoSize];
u32int n;
for(; o<e; o+=n){
n = sizeof zero;
if(o+n > e)
n = e-o;
if(writepart(p, o, zero, n) < 0)
fprint(2, "writepart %s: %r\n", p->name);
}
}
static void
sortminibuffer(ISect *is, Minibuf *mb, uchar *buf, u32int nbuf, u32int bufsize)
{
uchar *buckdata, *p, *q, *ep;
u32int b, lastb, memsize, n;
u64int o;
IBucket ib;
Part *part;
part = is->part;
buckdata = emalloc(is->blocksize);
if(mb->nwentry == 0)
return;
assert(mb->nwentry*IEntrySize <= mb->woffset-mb->boffset);
assert(mb->woffset-mb->boffset <= nbuf);
if(readpart(part, mb->boffset, buf, mb->woffset-mb->boffset) < 0){
fprint(2, "readpart %s: %r\n", part->name);
errors = 1;
return;
}
assert(*(uint*)buf != 0xa5a5a5a5);
memsize = (bufsize/IEntrySize)*IEntrySize;
for(o=mb->boffset, p=q=buf; o<mb->woffset; o+=bufsize){
memmove(p, q, memsize);
p += memsize;
q += bufsize;
}
ep = buf + mb->nwentry*IEntrySize;
assert(ep <= buf+nbuf);
qsort(buf, mb->nwentry, IEntrySize, ientrycmpaddr);
n = 0;
lastb = offset2bucket(is, mb->boffset);
for(p=buf; p<ep; p=q){
b = score2bucket(is, p);
for(q=p; q<ep && score2bucket(is, q)==b; q+=IEntrySize)
;
if(lastb+1 < b && zero)
zerorange(part, bucket2offset(is, lastb+1), bucket2offset(is, b));
if(IBucketSize+(q-p) > is->blocksize)
sysfatal("bucket overflow - make index bigger");
memmove(buckdata+IBucketSize, p, q-p);
ib.n = (q-p)/IEntrySize;
n += ib.n;
packibucket(&ib, buckdata, is->bucketmagic);
if(writepart(part, bucket2offset(is, b), buckdata, is->blocksize) < 0)
fprint(2, "write %s: %r\n", part->name);
lastb = b;
}
if(lastb+1 < is->stop-is->start && zero)
zerorange(part, bucket2offset(is, lastb+1), bucket2offset(is, is->stop - is->start));
if(n != mb->nwentry)
fprint(2, "sortminibuffer bug: n=%ud nwentry=%ud have=%ld\n", n, mb->nwentry, (ep-buf)/IEntrySize);
free(buckdata);
}
static void
isectproc(void *v)
{
u32int buck, bufbuckets, bufsize, epbuf, i, j;
u32int mbufbuckets, n, nbucket, nn, space;
u32int nbuf, nminibuf, xminiclump, prod;
u64int blocksize, offset, xclump;
uchar *data, *p;
Buf *buf;
IEntry ie;
IPool *ipool;
ISect *is;
Minibuf *mbuf, *mb;
is = v;
blocksize = is->blocksize;
nbucket = is->stop - is->start;
xclump = nbucket * (double)totalclumps/totalbuckets;
xminiclump = isectmem/2/IEntrySize;
prod = (xclump+xminiclump-1) / xminiclump;
if(!dumb && prod*MinBufSize < isectmem){
nbuf = prod;
nminibuf = 1;
}else{
for(nbuf=1; nbuf*nbuf<prod; nbuf++)
;
if(nbuf*MinBufSize > isectmem)
sysfatal("not enough memory");
nminibuf = nbuf;
}
if (nbuf == 0) {
fprint(2, "%s: brand-new index, no work to do\n", argv0);
threadexitsall(0);
}
bufsize = MinBufSize;
while(bufsize*2*nbuf <= isectmem && bufsize < MaxBufSize)
bufsize *= 2;
data = emalloc(nbuf*bufsize);
epbuf = bufsize/IEntrySize;
fprint(2, "%T %s: %,ud buckets, %,ud groups, %,ud minigroups, %,ud buffer\n",
is->part->name, nbucket, nbuf, nminibuf, bufsize);
buf = MKNZ(Buf, nbuf);
p = data;
offset = is->blockbase;
bufbuckets = (nbucket+nbuf-1)/nbuf;
for(i=0; i<nbuf; i++){
buf[i].part = is->part;
buf[i].bp = p;
buf[i].wp = p;
p += bufsize;
buf[i].ep = p;
buf[i].boffset = offset;
buf[i].woffset = offset;
if(i < nbuf-1){
offset += bufbuckets*blocksize;
buf[i].eoffset = offset;
}else{
offset = is->blockbase + nbucket*blocksize;
buf[i].eoffset = offset;
}
}
assert(p == data+nbuf*bufsize);
n = 0;
while(recv(is->writechan, &ie) == 1){
if(ie.ia.addr == 0)
break;
buck = score2bucket(is, ie.score);
i = buck/bufbuckets;
assert(i < nbuf);
bwrite(&buf[i], &ie);
n++;
}
add(&indexentries, n);
nn = 0;
for(i=0; i<nbuf; i++){
bflush(&buf[i]);
buf[i].bp = nil;
buf[i].ep = nil;
buf[i].wp = nil;
nn += buf[i].nentry;
}
if(n != nn)
fprint(2, "isectproc bug: n=%ud nn=%ud\n", n, nn);
free(data);
fprint(2, "%T %s: reordering\n", is->part->name);
mbuf = MKN(Minibuf, nminibuf);
mbufbuckets = (bufbuckets+nminibuf-1)/nminibuf;
while(mbufbuckets*blocksize % bufsize)
mbufbuckets++;
for(i=0; i<nbuf; i++){
n = buf[i].nentry;
nn = 0;
offset = buf[i].boffset;
memset(mbuf, 0, nminibuf*sizeof(mbuf[0]));
for(j=0; j<nminibuf; j++){
mb = &mbuf[j];
mb->boffset = offset;
offset += mbufbuckets*blocksize;
if(offset > buf[i].eoffset)
offset = buf[i].eoffset;
mb->eoffset = offset;
mb->roffset = mb->boffset;
mb->woffset = mb->boffset;
mb->nentry = epbuf * (mb->eoffset - mb->boffset)/bufsize;
if(mb->nentry > buf[i].nentry)
mb->nentry = buf[i].nentry;
buf[i].nentry -= mb->nentry;
nn += mb->nentry;
}
if(n != nn)
fprint(2, "isectproc bug2: n=%ud nn=%ud (i=%d)\n", n, nn, i);;
if(!dumb && nminibuf == 1){
mbuf[0].nwentry = mbuf[0].nentry;
mbuf[0].woffset = buf[i].woffset;
}else{
ipool = mkipool(is, mbuf, nminibuf, mbufbuckets, bufsize);
ipool->buck0 = bufbuckets*i;
for(j=0; j<nminibuf; j++){
mb = &mbuf[j];
while(mb->nentry > 0){
if(ipool->nfree < epbuf){
ipoolflush1(ipool);
continue;
}
assert(ipool->nfree >= epbuf);
ipoolloadblock(ipool, mb);
}
}
ipoolflush(ipool);
nn = 0;
for(j=0; j<nminibuf; j++)
nn += mbuf[j].nwentry;
if(n != nn)
fprint(2, "isectproc bug3: n=%ud nn=%ud (i=%d)\n", n, nn, i);
free(ipool);
}
space = 0;
for(j=0; j<nminibuf; j++)
if(space < mbuf[j].woffset - mbuf[j].boffset)
space = mbuf[j].woffset - mbuf[j].boffset;
data = emalloc(space);
for(j=0; j<nminibuf; j++){
mb = &mbuf[j];
sortminibuffer(is, mb, data, space, bufsize);
}
free(data);
}
sendp(isectdonechan, is);
}