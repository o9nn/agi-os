#include "stdinc.h"
#include "dat.h"
#include "fns.h"
typedef struct DCache DCache;
enum
{
HashLog = 9,
HashSize = 1<<HashLog,
HashMask = HashSize - 1,
};
struct DCache
{
QLock lock;
RWLock dirtylock;
Rendez full;
Round round;
DBlock *free;
u32int now;
int size;
DBlock **heads;
int nheap;
DBlock **heap;
int nblocks;
DBlock *blocks;
DBlock **write;
u8int *mem;
int ndirty;
int maxdirty;
};
typedef struct Ra Ra;
struct Ra
{
Part *part;
u64int addr;
};
static DCache dcache;
static int downheap(int i, DBlock *b);
static int upheap(int i, DBlock *b);
static DBlock *bumpdblock(void);
static void delheap(DBlock *db);
static void fixheap(int i, DBlock *b);
static void flushproc(void*);
static void writeproc(void*);
void
initdcache(u32int mem)
{
DBlock *b, *last;
u32int nblocks, blocksize;
int i;
u8int *p;
if(mem < maxblocksize * 2)
sysfatal("need at least %d bytes for the disk cache", maxblocksize * 2);
if(maxblocksize == 0)
sysfatal("no max. block size given for disk cache");
blocksize = maxblocksize;
nblocks = mem / blocksize;
dcache.full.l = &dcache.lock;
dcache.nblocks = nblocks;
dcache.maxdirty = (nblocks * 2) / 3;
trace(TraceProc, "initialize disk cache with %d blocks of %d bytes, maximum %d dirty blocks\n",
nblocks, blocksize, dcache.maxdirty);
dcache.size = blocksize;
dcache.heads = MKNZ(DBlock*, HashSize);
dcache.heap = MKNZ(DBlock*, nblocks);
dcache.blocks = MKNZ(DBlock, nblocks);
dcache.write = MKNZ(DBlock*, nblocks);
dcache.mem = MKNZ(u8int, (nblocks+1+128) * blocksize);
last = nil;
p = (u8int*)(((uintptr)dcache.mem+blocksize-1)&~(uintptr)(blocksize-1));
for(i = 0; i < nblocks; i++){
b = &dcache.blocks[i];
b->data = &p[i * blocksize];
b->heap = TWID32;
b->writedonechan = chancreate(sizeof(void*), 1);
b->next = last;
last = b;
}
dcache.free = last;
dcache.nheap = 0;
setstat(StatDcacheSize, nblocks);
initround(&dcache.round, "dcache", 120*1000);
vtproc(flushproc, nil);
vtproc(delaykickroundproc, &dcache.round);
}
static u32int
pbhash(u64int addr)
{
u32int h;
#define hashit(c) ((((c) * 0x6b43a9b5) >> (32 - HashLog)) & HashMask)
h = (addr >> 32) ^ addr;
return hashit(h);
}
DBlock*
getdblock(Part *part, u64int addr, int mode)
{
DBlock *b;
b = _getdblock(part, addr, mode, 1);
if(mode == OREAD || mode == ORDWR)
addstat(StatDcacheRead, 1);
if(mode == OWRITE || mode == ORDWR)
addstat(StatDcacheWrite, 1);
return b;
}
DBlock*
_getdblock(Part *part, u64int addr, int mode, int load)
{
DBlock *b;
u32int h, size, ms;
ms = 0;
trace(TraceBlock, "getdblock enter %s 0x%llux", part->name, addr);
size = part->blocksize;
if(size > dcache.size){
seterr(EAdmin, "block size %d too big for cache with size %d", size, dcache.size);
if(load)
addstat(StatDcacheLookup, 1);
return nil;
}
h = pbhash(addr);
qlock(&dcache.lock);
again:
for(b = dcache.heads[h]; b != nil; b = b->next){
if(b->part == part && b->addr == addr){
if(load)
addstat2(StatDcacheHit, 1, StatDcacheLookup, 1);
goto found;
}
}
if(!load){
qunlock(&dcache.lock);
return nil;
}
ms = msec();
addstat2(StatDcacheLookup, 1, StatDcacheMiss, 1);
b = bumpdblock();
if(b == nil){
trace(TraceBlock, "all disk cache blocks in use");
addstat(StatDcacheStall, 1);
rsleep(&dcache.full);
addstat(StatDcacheStall, -1);
goto again;
}
assert(!b->dirty);
b->used = (b->used2 + dcache.now) / 2;
b->next = dcache.heads[h];
dcache.heads[h] = b;
if(b->next != nil)
b->next->prev = b;
b->prev = nil;
b->addr = addr;
b->part = part;
b->size = 0;
found:
b->ref++;
b->used2 = b->used;
b->used = dcache.now++;
if(b->heap != TWID32)
fixheap(b->heap, b);
if((mode == ORDWR || mode == OWRITE) && part->writechan == nil){
trace(TraceBlock, "getdblock allocwriteproc %s", part->name);
part->writechan = chancreate(sizeof(DBlock*), dcache.nblocks);
vtproc(writeproc, part);
}
qunlock(&dcache.lock);
trace(TraceBlock, "getdblock lock");
addstat(StatDblockStall, 1);
if(mode == OREAD)
rlock(&b->lock);
else
wlock(&b->lock);
addstat(StatDblockStall, -1);
trace(TraceBlock, "getdblock locked");
if(b->size != size){
if(mode == OREAD){
addstat(StatDblockStall, 1);
runlock(&b->lock);
wlock(&b->lock);
addstat(StatDblockStall, -1);
}
if(b->size < size){
if(mode == OWRITE)
memset(&b->data[b->size], 0, size - b->size);
else{
trace(TraceBlock, "getdblock readpart %s 0x%llux", part->name, addr);
diskaccess(0);
if(readpart(part, addr + b->size, &b->data[b->size], size - b->size) < 0){
b->mode = ORDWR;
putdblock(b);
return nil;
}
trace(TraceBlock, "getdblock readpartdone");
addstat(StatApartRead, 1);
addstat(StatApartReadBytes, size-b->size);
}
}
b->size = size;
if(mode == OREAD){
addstat(StatDblockStall, 1);
wunlock(&b->lock);
rlock(&b->lock);
addstat(StatDblockStall, -1);
}
}
b->mode = mode;
trace(TraceBlock, "getdblock exit");
if(ms)
addstat(StatDcacheLookupTime, msec() - ms);
return b;
}
void
putdblock(DBlock *b)
{
if(b == nil)
return;
trace(TraceBlock, "putdblock %s 0x%llux", b->part->name, b->addr);
if(b->mode == OREAD)
runlock(&b->lock);
else
wunlock(&b->lock);
qlock(&dcache.lock);
if(--b->ref == 0 && !b->dirty){
if(b->heap == TWID32)
upheap(dcache.nheap++, b);
rwakeupall(&dcache.full);
}
qunlock(&dcache.lock);
}
void
dirtydblock(DBlock *b, int dirty)
{
int odirty;
trace(TraceBlock, "dirtydblock enter %s 0x%llux %d from 0x%lux",
b->part->name, b->addr, dirty, getcallerpc(&b));
assert(b->ref != 0);
assert(b->mode==ORDWR || b->mode==OWRITE);
odirty = b->dirty;
if(b->dirty)
assert(b->dirty == dirty);
else
b->dirty = dirty;
qlock(&dcache.lock);
if(!odirty){
dcache.ndirty++;
setstat(StatDcacheDirty, dcache.ndirty);
if(dcache.ndirty >= dcache.maxdirty)
kickround(&dcache.round, 0);
else
delaykickround(&dcache.round);
}
qunlock(&dcache.lock);
}
static void
unchain(DBlock *b)
{
ulong h;
if(b->prev == nil){
h = pbhash(b->addr);
if(dcache.heads[h] != b)
sysfatal("bad hash chains in disk cache");
dcache.heads[h] = b->next;
}else
b->prev->next = b->next;
if(b->next != nil)
b->next->prev = b->prev;
}
static DBlock*
bumpdblock(void)
{
DBlock *b;
trace(TraceBlock, "bumpdblock enter");
b = dcache.free;
if(b != nil){
dcache.free = b->next;
return b;
}
if(dcache.ndirty >= dcache.maxdirty)
kickdcache();
for(;;){
if(dcache.nheap == 0){
kickdcache();
trace(TraceBlock, "bumpdblock gotnothing");
return nil;
}
b = dcache.heap[0];
delheap(b);
if(!b->ref && !b->dirty)
break;
}
trace(TraceBlock, "bumpdblock bumping %s 0x%llux", b->part->name, b->addr);
unchain(b);
return b;
}
void
emptydcache(void)
{
DBlock *b;
qlock(&dcache.lock);
while(dcache.nheap > 0){
b = dcache.heap[0];
delheap(b);
if(!b->ref && !b->dirty){
unchain(b);
b->next = dcache.free;
dcache.free = b;
}
}
qunlock(&dcache.lock);
}
static void
delheap(DBlock *db)
{
if(db->heap == TWID32)
return;
fixheap(db->heap, dcache.heap[--dcache.nheap]);
db->heap = TWID32;
}
static void
fixheap(int i, DBlock *b)
{
if(upheap(i, b) == i)
downheap(i, b);
}
static int
upheap(int i, DBlock *b)
{
DBlock *bb;
u32int now;
int p;
now = dcache.now;
for(; i != 0; i = p){
p = (i - 1) >> 1;
bb = dcache.heap[p];
if(b->used2 - now >= bb->used2 - now)
break;
dcache.heap[i] = bb;
bb->heap = i;
}
dcache.heap[i] = b;
b->heap = i;
return i;
}
static int
downheap(int i, DBlock *b)
{
DBlock *bb;
u32int now;
int k;
now = dcache.now;
for(; ; i = k){
k = (i << 1) + 1;
if(k >= dcache.nheap)
break;
if(k + 1 < dcache.nheap && dcache.heap[k]->used2 - now > dcache.heap[k + 1]->used2 - now)
k++;
bb = dcache.heap[k];
if(b->used2 - now <= bb->used2 - now)
break;
dcache.heap[i] = bb;
bb->heap = i;
}
dcache.heap[i] = b;
b->heap = i;
return i;
}
static void
findblock(DBlock *bb)
{
DBlock *b, *last;
int h;
last = nil;
h = pbhash(bb->addr);
for(b = dcache.heads[h]; b != nil; b = b->next){
if(last != b->prev)
sysfatal("bad prev link");
if(b == bb)
return;
last = b;
}
sysfatal("block missing from hash table");
}
void
checkdcache(void)
{
DBlock *b;
u32int size, now;
int i, k, refed, nfree;
qlock(&dcache.lock);
size = dcache.size;
now = dcache.now;
for(i = 0; i < dcache.nheap; i++){
if(dcache.heap[i]->heap != i)
sysfatal("dc: mis-heaped at %d: %d", i, dcache.heap[i]->heap);
if(i > 0 && dcache.heap[(i - 1) >> 1]->used2 - now > dcache.heap[i]->used2 - now)
sysfatal("dc: bad heap ordering");
k = (i << 1) + 1;
if(k < dcache.nheap && dcache.heap[i]->used2 - now > dcache.heap[k]->used2 - now)
sysfatal("dc: bad heap ordering");
k++;
if(k < dcache.nheap && dcache.heap[i]->used2 - now > dcache.heap[k]->used2 - now)
sysfatal("dc: bad heap ordering");
}
refed = 0;
for(i = 0; i < dcache.nblocks; i++){
b = &dcache.blocks[i];
if(b->data != &dcache.mem[i * size])
sysfatal("dc: mis-blocked at %d", i);
if(b->ref && b->heap == TWID32)
refed++;
if(b->addr)
findblock(b);
if(b->heap != TWID32
&& dcache.heap[b->heap] != b)
sysfatal("dc: spurious heap value");
}
nfree = 0;
for(b = dcache.free; b != nil; b = b->next){
if(b->addr != 0 || b->heap != TWID32)
sysfatal("dc: bad free list");
nfree++;
}
if(dcache.nheap + nfree + refed != dcache.nblocks)
sysfatal("dc: missing blocks: %d %d %d", dcache.nheap, refed, dcache.nblocks);
qunlock(&dcache.lock);
}
void
flushdcache(void)
{
trace(TraceProc, "flushdcache enter");
kickround(&dcache.round, 1);
trace(TraceProc, "flushdcache exit");
}
void
kickdcache(void)
{
kickround(&dcache.round, 0);
}
static int
parallelwrites(DBlock **b, DBlock **eb, int dirty)
{
DBlock **p, **q;
Part *part;
for(p=b; p<eb && (*p)->dirty == dirty; p++){
assert(b<=p && p<eb);
sendp((*p)->part->writechan, *p);
}
q = p;
for(p=b; p<q; p++){
assert(b<=p && p<eb);
recvp((*p)->writedonechan);
}
part = nil;
for(p=b; p<q; p++){
if(part != (*p)->part){
part = (*p)->part;
flushpart(part);
}
}
return p-b;
}
static int
writeblockcmp(const void *va, const void *vb)
{
DBlock *a, *b;
a = *(DBlock**)va;
b = *(DBlock**)vb;
if(a->dirty != b->dirty)
return a->dirty - b->dirty;
if(a->part != b->part){
if(a->part < b->part)
return -1;
if(a->part > b->part)
return 1;
}
if(a->addr < b->addr)
return -1;
return 1;
}
static void
flushproc(void *v)
{
int i, j, n;
ulong t0;
DBlock *b, **write;
USED(v);
threadsetname("flushproc");
for(;;){
waitforkick(&dcache.round);
trace(TraceWork, "start");
t0 = nsec()/1000;
trace(TraceProc, "build t=%lud", (ulong)(nsec()/1000)-t0);
write = dcache.write;
n = 0;
for(i=0; i<dcache.nblocks; i++){
b = &dcache.blocks[i];
if(b->dirty)
write[n++] = b;
}
qsort(write, n, sizeof(write[0]), writeblockcmp);
trace(TraceProc, "writeblocks t=%lud", (ulong)(nsec()/1000)-t0);
i = 0;
for(j=1; j<DirtyMax; j++){
trace(TraceProc, "writeblocks.%d t=%lud",
j, (ulong)(nsec()/1000)-t0);
i += parallelwrites(write+i, write+n, j);
}
if(i != n){
fprint(2, "in flushproc i=%d n=%d\n", i, n);
for(i=0; i<n; i++)
fprint(2, "\tblock %d: dirty=%d\n",
i, write[i]->dirty);
abort();
}
trace(TraceProc, "undirty.%d t=%lud", j, (ulong)(nsec()/1000)-t0);
qlock(&dcache.lock);
for(i=0; i<n; i++){
b = write[i];
--dcache.ndirty;
if(b->ref == 0 && b->heap == TWID32){
upheap(dcache.nheap++, b);
rwakeupall(&dcache.full);
}
}
setstat(StatDcacheDirty, dcache.ndirty);
qunlock(&dcache.lock);
addstat(StatDcacheFlush, 1);
trace(TraceWork, "finish");
}
}
static void
writeproc(void *v)
{
DBlock *b;
Part *p;
p = v;
threadsetname("writeproc:%s", p->name);
for(;;){
b = recvp(p->writechan);
trace(TraceWork, "start");
assert(b->part == p);
trace(TraceProc, "wlock %s 0x%llux", p->name, b->addr);
wlock(&b->lock);
trace(TraceProc, "writepart %s 0x%llux", p->name, b->addr);
diskaccess(0);
if(writepart(p, b->addr, b->data, b->size) < 0)
fprint(2, "%s: writeproc: part %s addr 0x%llux: write error: %r\n",
argv0, p->name, b->addr);
addstat(StatApartWrite, 1);
addstat(StatApartWriteBytes, b->size);
b->dirty = 0;
wunlock(&b->lock);
trace(TraceProc, "finish %s 0x%llux", p->name, b->addr);
trace(TraceWork, "finish");
sendp(b->writedonechan, b);
}
}