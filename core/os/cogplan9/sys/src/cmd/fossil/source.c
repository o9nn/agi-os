#include "stdinc.h"
#include "dat.h"
#include "fns.h"
#include "error.h"
#include "9.h"
static int sizeToDepth(uvlong s, int psize, int dsize);
static u32int tagGen(void);
static Block *sourceLoad(Source *r, Entry *e);
static int sourceShrinkDepth(Source*, Block*, Entry*, int);
static int sourceShrinkSize(Source*, Entry*, uvlong);
static int sourceGrowDepth(Source*, Block*, Entry*, int);
#define sourceIsLocked(r) ((r)->b != nil)
static Source *
sourceAlloc(Fs *fs, Block *b, Source *p, u32int offset, int mode, int issnapshot)
{
int epb;
u32int epoch;
char *pname = nil;
Source *r;
Entry e;
assert(p==nil || sourceIsLocked(p));
if(p == nil){
assert(offset == 0);
epb = 1;
}else
epb = p->dsize / VtEntrySize;
if(b->l.type != BtDir)
goto Bad;
if(!entryUnpack(&e, b->data, offset % epb)){
pname = sourceName(p);
consPrint("%s: %s %V: sourceAlloc: entryUnpack failed\n",
fs->name, pname, b->score);
goto Bad;
}
if(!(e.flags & VtEntryActive)){
pname = sourceName(p);
if(0) consPrint("%s: %s %V: sourceAlloc: not active\n",
fs->name, pname, e.score);
goto Bad;
}
if(e.psize < 256 || e.dsize < 256){
pname = sourceName(p);
consPrint("%s: %s %V: sourceAlloc: psize %ud or dsize %ud < 256\n",
fs->name, pname, e.score, e.psize, e.dsize);
goto Bad;
}
if(e.depth < sizeToDepth(e.size, e.psize, e.dsize)){
pname = sourceName(p);
consPrint("%s: %s %V: sourceAlloc: depth %ud size %llud "
"psize %ud dsize %ud\n", fs->name, pname,
e.score, e.depth, e.size, e.psize, e.dsize);
goto Bad;
}
if((e.flags & VtEntryLocal) && e.tag == 0){
pname = sourceName(p);
consPrint("%s: %s %V: sourceAlloc: flags %#ux tag %#ux\n",
fs->name, pname, e.score, e.flags, e.tag);
goto Bad;
}
if(e.dsize > fs->blockSize || e.psize > fs->blockSize){
pname = sourceName(p);
consPrint("%s: %s %V: sourceAlloc: psize %ud or dsize %ud "
"> blocksize %ud\n", fs->name, pname, e.score,
e.psize, e.dsize, fs->blockSize);
goto Bad;
}
epoch = b->l.epoch;
if(mode == OReadWrite){
if(e.snap != 0){
vtSetError(ESnapRO);
return nil;
}
}else if(e.snap != 0){
if(e.snap < fs->elo){
vtSetError(ESnapOld);
return nil;
}
if(e.snap >= fs->ehi)
goto Bad;
epoch = e.snap;
}
r = vtMemAllocZ(sizeof(Source));
r->fs = fs;
r->mode = mode;
r->issnapshot = issnapshot;
r->dsize = e.dsize;
r->gen = e.gen;
r->dir = (e.flags & VtEntryDir) != 0;
r->lk = vtLockAlloc();
r->ref = 1;
r->parent = p;
if(p){
vtLock(p->lk);
assert(mode == OReadOnly || p->mode == OReadWrite);
p->ref++;
vtUnlock(p->lk);
}
r->epoch = epoch;
memmove(r->score, b->score, VtScoreSize);
r->scoreEpoch = b->l.epoch;
r->offset = offset;
r->epb = epb;
r->tag = b->l.tag;
return r;
Bad:
free(pname);
vtSetError(EBadEntry);
return nil;
}
Source *
sourceRoot(Fs *fs, u32int addr, int mode)
{
Source *r;
Block *b;
b = cacheLocalData(fs->cache, addr, BtDir, RootTag, mode, 0);
if(b == nil)
return nil;
if(mode == OReadWrite && b->l.epoch != fs->ehi){
consPrint("sourceRoot: fs->ehi = %ud, b->l = %L\n",
fs->ehi, &b->l);
blockPut(b);
vtSetError(EBadRoot);
return nil;
}
r = sourceAlloc(fs, b, nil, 0, mode, 0);
blockPut(b);
return r;
}
Source *
sourceOpen(Source *r, ulong offset, int mode, int issnapshot)
{
ulong bn;
Block *b;
assert(sourceIsLocked(r));
if(r->mode == OReadWrite)
assert(r->epoch == r->b->l.epoch);
if(!r->dir){
vtSetError(ENotDir);
return nil;
}
bn = offset/(r->dsize/VtEntrySize);
b = sourceBlock(r, bn, mode);
if(b == nil)
return nil;
r = sourceAlloc(r->fs, b, r, offset, mode, issnapshot);
blockPut(b);
return r;
}
Source *
sourceCreate(Source *r, int dsize, int dir, u32int offset)
{
int i, epb, psize;
u32int bn, size;
Block *b;
Entry e;
Source *rr;
assert(sourceIsLocked(r));
if(!r->dir){
vtSetError(ENotDir);
return nil;
}
epb = r->dsize/VtEntrySize;
psize = (dsize/VtScoreSize)*VtScoreSize;
size = sourceGetDirSize(r);
if(offset == 0){
offset = lnrand(size+1);
offset -= offset % epb;
}
for(;;){
bn = offset/epb;
b = sourceBlock(r, bn, OReadWrite);
if(b == nil)
return nil;
for(i=offset%r->epb; i<epb; i++){
entryUnpack(&e, b->data, i);
if((e.flags&VtEntryActive) == 0 && e.gen != ~0)
goto Found;
}
blockPut(b);
if(offset == size){
fprint(2, "sourceCreate: cannot happen\n");
vtSetError("sourceCreate: cannot happen");
return nil;
}
offset = size;
}
Found:
e.psize = psize;
e.dsize = dsize;
assert(psize && dsize);
e.flags = VtEntryActive;
if(dir)
e.flags |= VtEntryDir;
e.depth = 0;
e.size = 0;
memmove(e.score, vtZeroScore, VtScoreSize);
e.tag = 0;
e.snap = 0;
e.archive = 0;
entryPack(&e, b->data, i);
blockDirty(b);
offset = bn*epb + i;
if(offset+1 > size){
if(!sourceSetDirSize(r, offset+1)){
blockPut(b);
return nil;
}
}
rr = sourceAlloc(r->fs, b, r, offset, OReadWrite, 0);
blockPut(b);
return rr;
}
static int
sourceKill(Source *r, int doremove)
{
Entry e;
Block *b;
u32int addr;
u32int tag;
int type;
assert(sourceIsLocked(r));
b = sourceLoad(r, &e);
if(b == nil)
return 0;
assert(b->l.epoch == r->fs->ehi);
if(doremove==0 && e.size == 0){
blockPut(b);
return 1;
}
addr = globalToLocal(e.score);
type = entryType(&e);
tag = e.tag;
if(doremove){
if(e.gen != ~0)
e.gen++;
e.dsize = 0;
e.psize = 0;
e.flags = 0;
}else{
e.flags &= ~VtEntryLocal;
}
e.depth = 0;
e.size = 0;
e.tag = 0;
memmove(e.score, vtZeroScore, VtScoreSize);
entryPack(&e, b->data, r->offset % r->epb);
blockDirty(b);
if(addr != NilBlock)
blockRemoveLink(b, addr, type, tag, 1);
blockPut(b);
if(doremove){
sourceUnlock(r);
sourceClose(r);
}
return 1;
}
int
sourceRemove(Source *r)
{
return sourceKill(r, 1);
}
int
sourceTruncate(Source *r)
{
return sourceKill(r, 0);
}
uvlong
sourceGetSize(Source *r)
{
Entry e;
Block *b;
assert(sourceIsLocked(r));
b = sourceLoad(r, &e);
if(b == nil)
return 0;
blockPut(b);
return e.size;
}
static int
sourceShrinkSize(Source *r, Entry *e, uvlong size)
{
int i, type, ppb;
uvlong ptrsz;
u32int addr;
uchar score[VtScoreSize];
Block *b;
type = entryType(e);
b = cacheGlobal(r->fs->cache, e->score, type, e->tag, OReadWrite);
if(b == nil)
return 0;
ptrsz = e->dsize;
ppb = e->psize/VtScoreSize;
for(i=0; i+1<e->depth; i++)
ptrsz *= ppb;
while(type&BtLevelMask){
if(b->addr == NilBlock || b->l.epoch != r->fs->ehi){
blockPut(b);
return 0;
}
i = (size+ptrsz-1)/ptrsz;
for(; i<ppb; i++){
addr = globalToLocal(b->data+i*VtScoreSize);
memmove(b->data+i*VtScoreSize, vtZeroScore, VtScoreSize);
blockDirty(b);
if(addr != NilBlock)
blockRemoveLink(b, addr, type-1, e->tag, 1);
}
i = size/ptrsz;
size = size%ptrsz;
if(size == 0){
blockPut(b);
return 1;
}
ptrsz /= ppb;
type--;
memmove(score, b->data+i*VtScoreSize, VtScoreSize);
blockPut(b);
b = cacheGlobal(r->fs->cache, score, type, e->tag, OReadWrite);
if(b == nil)
return 0;
}
if(b->addr == NilBlock || b->l.epoch != r->fs->ehi){
blockPut(b);
return 0;
}
if(type == BtData && e->dsize > size){
memset(b->data+size, 0, e->dsize-size);
blockDirty(b);
}
blockPut(b);
return 1;
}
int
sourceSetSize(Source *r, uvlong size)
{
int depth;
Entry e;
Block *b;
assert(sourceIsLocked(r));
if(size == 0)
return sourceTruncate(r);
if(size > VtMaxFileSize || size > ((uvlong)MaxBlock)*r->dsize){
vtSetError(ETooBig);
return 0;
}
b = sourceLoad(r, &e);
if(b == nil)
return 0;
if(e.size == size){
blockPut(b);
return 1;
}
depth = sizeToDepth(size, e.psize, e.dsize);
if(depth < e.depth){
if(!sourceShrinkDepth(r, b, &e, depth)){
blockPut(b);
return 0;
}
}else if(depth > e.depth){
if(!sourceGrowDepth(r, b, &e, depth)){
blockPut(b);
return 0;
}
}
if(size < e.size)
sourceShrinkSize(r, &e, size);
e.size = size;
entryPack(&e, b->data, r->offset % r->epb);
blockDirty(b);
blockPut(b);
return 1;
}
int
sourceSetDirSize(Source *r, ulong ds)
{
uvlong size;
int epb;
assert(sourceIsLocked(r));
epb = r->dsize/VtEntrySize;
size = (uvlong)r->dsize*(ds/epb);
size += VtEntrySize*(ds%epb);
return sourceSetSize(r, size);
}
ulong
sourceGetDirSize(Source *r)
{
ulong ds;
uvlong size;
int epb;
assert(sourceIsLocked(r));
epb = r->dsize/VtEntrySize;
size = sourceGetSize(r);
ds = epb*(size/r->dsize);
ds += (size%r->dsize)/VtEntrySize;
return ds;
}
int
sourceGetEntry(Source *r, Entry *e)
{
Block *b;
assert(sourceIsLocked(r));
b = sourceLoad(r, e);
if(b == nil)
return 0;
blockPut(b);
return 1;
}
int
sourceSetEntry(Source *r, Entry *e)
{
Block *b;
Entry oe;
assert(sourceIsLocked(r));
b = sourceLoad(r, &oe);
if(b == nil)
return 0;
entryPack(e, b->data, r->offset%r->epb);
blockDirty(b);
blockPut(b);
return 1;
}
static Block *
blockWalk(Block *p, int index, int mode, Fs *fs, Entry *e)
{
Block *b;
Cache *c;
u32int addr;
int type;
uchar oscore[VtScoreSize], score[VtScoreSize];
Entry oe;
c = fs->cache;
if((p->l.type & BtLevelMask) == 0){
assert(p->l.type == BtDir);
type = entryType(e);
b = cacheGlobal(c, e->score, type, e->tag, mode);
}else{
type = p->l.type - 1;
b = cacheGlobal(c, p->data + index*VtScoreSize, type, e->tag, mode);
}
if(b)
b->pc = getcallerpc(&p);
if(b == nil || mode == OReadOnly)
return b;
if(p->l.epoch != fs->ehi){
fprint(2, "blockWalk: parent not writable\n");
abort();
}
if(b->l.epoch == fs->ehi)
return b;
oe = *e;
if(e->tag == 0){
assert(p->l.type == BtDir);
e->tag = tagGen();
e->flags |= VtEntryLocal;
}
addr = b->addr;
b = blockCopy(b, e->tag, fs->ehi, fs->elo);
if(b == nil)
return nil;
b->pc = getcallerpc(&p);
assert(b->l.epoch == fs->ehi);
blockDirty(b);
memmove(score, b->score, VtScoreSize);
if(p->l.type == BtDir){
memmove(e->score, b->score, VtScoreSize);
entryPack(e, p->data, index);
blockDependency(p, b, index, nil, &oe);
}else{
memmove(oscore, p->data+index*VtScoreSize, VtScoreSize);
memmove(p->data+index*VtScoreSize, b->score, VtScoreSize);
blockDependency(p, b, index, oscore, nil);
}
blockDirty(p);
if(addr != NilBlock)
blockRemoveLink(p, addr, type, e->tag, 0);
return b;
}
static int
sourceGrowDepth(Source *r, Block *p, Entry *e, int depth)
{
Block *b, *bb;
u32int tag;
int type;
Entry oe;
assert(sourceIsLocked(r));
assert(depth <= VtPointerDepth);
type = entryType(e);
b = cacheGlobal(r->fs->cache, e->score, type, e->tag, OReadWrite);
if(b == nil)
return 0;
tag = e->tag;
if(tag == 0)
tag = tagGen();
oe = *e;
while(e->depth < depth){
bb = cacheAllocBlock(r->fs->cache, type+1, tag, r->fs->ehi, r->fs->elo);
if(bb == nil)
break;
memmove(bb->data, b->score, VtScoreSize);
memmove(e->score, bb->score, VtScoreSize);
e->depth++;
type++;
e->tag = tag;
e->flags |= VtEntryLocal;
blockDependency(bb, b, 0, vtZeroScore, nil);
blockPut(b);
b = bb;
blockDirty(b);
}
entryPack(e, p->data, r->offset % r->epb);
blockDependency(p, b, r->offset % r->epb, nil, &oe);
blockPut(b);
blockDirty(p);
return e->depth == depth;
}
static int
sourceShrinkDepth(Source *r, Block *p, Entry *e, int depth)
{
Block *b, *nb, *ob, *rb;
u32int tag;
int type, d;
Entry oe;
assert(sourceIsLocked(r));
assert(depth <= VtPointerDepth);
type = entryType(e);
rb = cacheGlobal(r->fs->cache, e->score, type, e->tag, OReadWrite);
if(rb == nil)
return 0;
tag = e->tag;
if(tag == 0)
tag = tagGen();
oe = *e;
ob = nil;
b = rb;
for(d=e->depth; d > depth; d--, type++){
nb = cacheGlobal(r->fs->cache, b->data, type-1, tag, OReadWrite);
if(nb == nil)
break;
if(ob!=nil && ob!=rb)
blockPut(ob);
ob = b;
b = nb;
}
if(b == rb){
blockPut(rb);
return 0;
}
e->depth = d;
if(globalToLocal(b->score) == NilBlock)
e->flags &= ~VtEntryLocal;
memmove(e->score, b->score, VtScoreSize);
entryPack(e, p->data, r->offset % r->epb);
blockDependency(p, b, r->offset % r->epb, nil, &oe);
blockDirty(p);
memmove(ob->data, vtZeroScore, VtScoreSize);
blockDependency(ob, p, 0, b->score, nil);
blockDirty(ob);
if(rb->addr != NilBlock)
blockRemoveLink(p, rb->addr, rb->l.type, rb->l.tag, 1);
blockPut(rb);
if(ob!=nil && ob!=rb)
blockPut(ob);
blockPut(b);
return d == depth;
}
Block *
_sourceBlock(Source *r, ulong bn, int mode, int early, ulong tag)
{
Block *b, *bb;
int index[VtPointerDepth+1];
Entry e;
int i, np;
int m;
assert(sourceIsLocked(r));
assert(bn != NilBlock);
m = mode;
if(m == OOverWrite)
m = OReadWrite;
b = sourceLoad(r, &e);
if(b == nil)
return nil;
if(r->issnapshot && (e.flags & VtEntryNoArchive)){
blockPut(b);
vtSetError(ENotArchived);
return nil;
}
if(tag){
if(e.tag == 0)
e.tag = tag;
else if(e.tag != tag){
fprint(2, "tag mismatch\n");
vtSetError("tag mismatch");
goto Err;
}
}
np = e.psize/VtScoreSize;
memset(index, 0, sizeof(index));
for(i=0; bn > 0; i++){
if(i >= VtPointerDepth){
vtSetError(EBadAddr);
goto Err;
}
index[i] = bn % np;
bn /= np;
}
if(i > e.depth){
if(mode == OReadOnly){
vtSetError(EBadAddr);
goto Err;
}
if(!sourceGrowDepth(r, b, &e, i))
goto Err;
}
index[e.depth] = r->offset % r->epb;
for(i=e.depth; i>=early; i--){
bb = blockWalk(b, index[i], m, r->fs, &e);
if(bb == nil)
goto Err;
blockPut(b);
b = bb;
}
b->pc = getcallerpc(&r);
return b;
Err:
blockPut(b);
return nil;
}
Block*
sourceBlock(Source *r, ulong bn, int mode)
{
Block *b;
b = _sourceBlock(r, bn, mode, 0, 0);
if(b)
b->pc = getcallerpc(&r);
return b;
}
void
sourceClose(Source *r)
{
if(r == nil)
return;
vtLock(r->lk);
r->ref--;
if(r->ref){
vtUnlock(r->lk);
return;
}
assert(r->ref == 0);
vtUnlock(r->lk);
if(r->parent)
sourceClose(r->parent);
vtLockFree(r->lk);
memset(r, ~0, sizeof(*r));
vtMemFree(r);
}
static Block*
sourceLoadBlock(Source *r, int mode)
{
u32int addr;
Block *b;
switch(r->mode){
default:
assert(0);
case OReadWrite:
assert(r->mode == OReadWrite);
if(r->epoch == r->fs->ehi){
b = cacheGlobal(r->fs->cache, r->score, BtDir, r->tag, OReadWrite);
if(b == nil)
return nil;
assert(r->epoch == b->l.epoch);
return b;
}
assert(r->parent != nil);
if(!sourceLock(r->parent, OReadWrite))
return nil;
b = sourceBlock(r->parent, r->offset/r->epb, OReadWrite);
sourceUnlock(r->parent);
if(b == nil)
return nil;
assert(b->l.epoch == r->fs->ehi);
memmove(r->score, b->score, VtScoreSize);
r->scoreEpoch = b->l.epoch;
r->tag = b->l.tag;
r->epoch = r->fs->ehi;
return b;
case OReadOnly:
addr = globalToLocal(r->score);
if(addr == NilBlock)
return cacheGlobal(r->fs->cache, r->score, BtDir, r->tag, mode);
b = cacheLocalData(r->fs->cache, addr, BtDir, r->tag, mode, r->scoreEpoch);
if(b)
return b;
if(strcmp(vtGetError(), ELabelMismatch) == 0){
if(!sourceLock(r->parent, OReadOnly))
return nil;
b = sourceBlock(r->parent, r->offset/r->epb, OReadOnly);
sourceUnlock(r->parent);
if(b){
fprint(2, "sourceAlloc: lost %V found %V\n",
r->score, b->score);
memmove(r->score, b->score, VtScoreSize);
r->scoreEpoch = b->l.epoch;
return b;
}
}
return nil;
}
}
int
sourceLock(Source *r, int mode)
{
Block *b;
if(mode == -1)
mode = r->mode;
b = sourceLoadBlock(r, mode);
if(b == nil)
return 0;
assert(r->b == nil);
r->b = b;
if(r->mode == OReadWrite)
assert(r->epoch == r->b->l.epoch);
return 1;
}
int
sourceLock2(Source *r, Source *rr, int mode)
{
Block *b, *bb;
if(rr == nil)
return sourceLock(r, mode);
if(mode == -1)
mode = r->mode;
if(r->parent==rr->parent && r->offset/r->epb == rr->offset/rr->epb){
b = sourceLoadBlock(r, mode);
if(b == nil)
return 0;
if(memcmp(r->score, rr->score, VtScoreSize) != 0){
memmove(rr->score, b->score, VtScoreSize);
rr->scoreEpoch = b->l.epoch;
rr->tag = b->l.tag;
rr->epoch = rr->fs->ehi;
}
blockDupLock(b);
bb = b;
}else if(r->parent==rr->parent || r->offset > rr->offset){
bb = sourceLoadBlock(rr, mode);
b = sourceLoadBlock(r, mode);
}else{
b = sourceLoadBlock(r, mode);
bb = sourceLoadBlock(rr, mode);
}
if(b == nil || bb == nil){
if(b)
blockPut(b);
if(bb)
blockPut(bb);
return 0;
}
r->b = b;
rr->b = bb;
return 1;
}
void
sourceUnlock(Source *r)
{
Block *b;
if(r->b == nil){
fprint(2, "sourceUnlock: already unlocked\n");
abort();
}
b = r->b;
r->b = nil;
blockPut(b);
}
static Block*
sourceLoad(Source *r, Entry *e)
{
Block *b;
assert(sourceIsLocked(r));
b = r->b;
if(!entryUnpack(e, b->data, r->offset % r->epb))
return nil;
if(e->gen != r->gen){
vtSetError(ERemoved);
return nil;
}
blockDupLock(b);
return b;
}
static int
sizeToDepth(uvlong s, int psize, int dsize)
{
int np;
int d;
np = psize/VtScoreSize;
s = (s + dsize - 1)/dsize;
for(d = 0; s > 1; d++)
s = (s + np - 1)/np;
return d;
}
static u32int
tagGen(void)
{
u32int tag;
for(;;){
tag = lrand();
if(tag >= UserTag)
break;
}
return tag;
}
char *
sourceName(Source *s)
{
return fileName(s->file);
}