#include "logfsos.h"
#include "logfs.h"
#include "local.h"
typedef struct AllocState AllocState;
struct AllocState {
long oldblock;
int markbad;
};
Pageset
logfsdatapagemask(int pages, int base)
{
if(pages == BITSPERSET)
return ~(Pageset)0;
return (((Pageset)1 << pages) - 1) << (BITSPERSET - base - pages);
}
static Pageset
fastgap(Pageset w, uint n)
{
Pageset s;
if(w == 0 || n < 1 || n > BITSPERSET)
return 0;
s = n >> 1;
w &= w << s;
n -= s;
s = n >> 1;
w &= w << s;
n -= s;
s = n >> 1;
w &= w << s;
n -= s;
s = n >> 1;
w &= w << s;
n -= s;
s = n >> 1;
if(BITSPERSET == 64){
w &= w << s;
n -= s;
s = n >> 1;
}
return w & (w << s);
}
static int
nlz(Pageset x)
{
int n, c;
if(x == 0)
return BITSPERSET;
if(x & PAGETOP)
return 0;
n = BITSPERSET;
c = BITSPERSET/2;
do {
Pageset y;
y = x >> c;
if(y != 0) {
n -= c;
x = y;
}
} while((c >>= 1) != 0);
return n - x;
}
static Pageset
findgap(Pageset w, uint n)
{
Pageset m;
do {
m = fastgap(w, n);
if(m)
break;
n--;
} while(n);
if(n == 0)
return 0;
return logfsdatapagemask(n, nlz(m));
}
static int
bitcount(Pageset mask)
{
Pageset m;
int rv;
rv = 0;
for(m = PAGETOP; m != 0; m >>= 1)
if(mask & m)
rv++;
return rv;
}
static char *
allocdatapages(LogfsServer *server, u32int count, int *countp, long *blockindexp, int *pagep, u32int *flashaddr, AllocState *state)
{
LogfsLowLevel *ll = server->ll;
long b, blockindex;
DataBlock *db;
int pagebase;
u32int pages = (count + (1 << ll->l2pagesize) - 1) >> ll->l2pagesize;
u32int gapmask;
long bestfreeblockindex;
int bestfree;
int pagesperblock = 1 << ll->l2pagesperblock;
int apages;
char *errmsg;
int didsomething;
state->oldblock = -1;
state->markbad = 0;
if(pages > pagesperblock)
pages = pagesperblock;
bestfreeblockindex = -1;
bestfree = 0;
for(blockindex = 0; blockindex < server->ndatablocks; blockindex++) {
db = server->datablock + blockindex;
if(db->block < 0)
continue;
gapmask = findgap(db->free & ~db->dirty, pages);
if(gapmask != 0) {
b = db->block;
USED(b);
goto done;
}
else {
int free = bitcount(db->free & logfsdatapagemask(pagesperblock, 0));
if(free > 0 && (bestfreeblockindex < 0 || free > bestfree)) {
bestfreeblockindex = blockindex;
bestfree = free;
}
}
}
if(bestfreeblockindex >= 0) {
b = logfsfindfreeblock(ll, AllocReasonTransfer);
while(b >= 0) {
char *errmsg;
LogfsLowLevelReadResult llrr;
long oldblock;
int markedbad;
db = server->datablock + bestfreeblockindex;
oldblock = db->block;
errmsg = logfsservercopyactivedata(server, b, bestfreeblockindex, 0, &llrr, &markedbad);
if(errmsg) {
if(!markedbad)
return errmsg;
b = logfsfindfreeblock(ll, AllocReasonTransfer);
}
else {
Pageset available;
blockindex = bestfreeblockindex;
state->oldblock = oldblock;
state->markbad = llrr != LogfsLowLevelReadResultOk;
available = db->free & ~db->dirty;
if(available & PAGETOP)
available = logfsdatapagemask(nlz(~available), 0);
gapmask = findgap(available, pages);
goto done;
}
}
}
b = logfsfindfreeblock(ll, AllocReasonDataExtend);
if(b >= 0) {
useerased:
for(blockindex = 0, db = server->datablock; blockindex < server->ndatablocks; blockindex++, db++)
if(db->block < 0)
break;
if(blockindex == server->ndatablocks)
server->ndatablocks++;
db->path = mkdatapath(blockindex, 0);
db->block = b;
(*ll->setblocktag)(ll, b, LogfsTdata);
(*ll->setblockpath)(ll, b, db->path);
db->free = logfsdatapagemask(pagesperblock, 0);
db->dirty = 0;
gapmask = db->free;
goto done;
}
errmsg = logfsserverlogsweep(server, 0, &didsomething);
if(errmsg)
return errmsg;
if(didsomething) {
b = logfsfindfreeblock(ll, AllocReasonDataExtend);
if(b >= 0) {
goto useerased;
}
}
*countp = 0;
return nil;
done:
apages = bitcount(gapmask);
pagebase = nlz(gapmask);
if(apages > pages)
apages = pages;
gapmask = logfsdatapagemask(apages, pagebase);
if(server->trace > 1)
print("allocdatapages: block %ld(%ld) pages %d mask 0x%.8ux pagebase %d apages %d\n",
blockindex, db->block, pages, gapmask, pagebase, apages);
db->free &= ~gapmask;
db->dirty |= gapmask;
*pagep = pagebase;
*blockindexp = blockindex;
*flashaddr = logfsspo2flashaddr(server, blockindex, pagebase, 0);
*countp = apages << ll->l2pagesize;
if(*countp > count)
*countp = count;
return nil;
}
typedef struct Page {
u32int pageaddr;
int ref;
} Page;
typedef struct DataStructure {
LogfsServer *server;
int nentries;
int maxentries;
Page *array;
} DataStructure;
static int
deltapage(DataStructure *ds, u32int pageaddr, int add, int delta)
{
int i;
for(i = 0; i < ds->nentries; i++)
if(ds->array[i].pageaddr == pageaddr) {
ds->array[i].ref += delta;
return 1;
}
if(!add)
return 1;
if(ds->maxentries == 0) {
ds->array = logfsrealloc(nil, sizeof(Page) * 100);
if(ds->array == nil)
return 0;
ds->maxentries = 100;
}
else if(ds->nentries >= ds->maxentries) {
void *a = logfsrealloc(ds->array, ds->maxentries * 2 * sizeof(Page));
if(a == nil)
return 0;
ds->array = a;
ds->maxentries *= 2;
}
ds->array[ds->nentries].pageaddr = pageaddr;
ds->array[ds->nentries++].ref = delta;
return 1;
}
static int
deltapages(DataStructure *ds, LogfsLowLevel *ll, u32int baseflashaddr, int range, int add, int delta)
{
long seq;
int page, offset;
int pages;
u32int pageaddr;
int x;
logfsflashaddr2spo(ds->server, baseflashaddr, &seq, &page, &offset);
pages = (offset + range + (1 << ll->l2pagesize) - 1) >> ll->l2pagesize;
pageaddr = (seq << ll->l2pagesperblock) + page;
for(x = 0; x < pages; x++, pageaddr++)
if(!deltapage(ds, pageaddr, add, delta))
return 0;
return 1;
}
static int
findpageset(void *magic, u32int baseoffset, u32int limitoffset, Extent *e, u32int extentoffset)
{
DataStructure *ds = magic;
LogfsLowLevel *ll;
u32int flashaddr;
u32int range;
u32int residue;
if(e == nil || (e->flashaddr & LogAddr) != 0)
return 1;
ll = ds->server->ll;
flashaddr = e->flashaddr;
if(extentoffset)
if(!deltapages(ds, ll, flashaddr, extentoffset, 1, 1))
return -1;
flashaddr += extentoffset;
range = limitoffset - baseoffset;
if(!deltapages(ds, ll, flashaddr, range, 1, -1))
return -1;
flashaddr += range;
residue = e->max - e->min - (extentoffset + range);
if(residue)
if(!deltapages(ds, ll, flashaddr, residue, 1, 1))
return -1;
return 1;
}
static int
addpagereferences(void *magic, Extent *e, int hole)
{
DataStructure *ds = magic;
if(hole || (e->flashaddr & LogAddr) != 0)
return 1;
return deltapages(ds, ds->server->ll, e->flashaddr, e->max - e->min, 0, 1) ? 1 : -1;
}
static char *
zappages(LogfsServer *server, Entry *e, u32int min, u32int max)
{
DataStructure ds;
long seq;
int x, rv, page;
Page *p;
if(min >= e->u.file.length)
return nil;
if(min == 0 && max >= e->u.file.length) {
logfsextentlistwalk(e->u.file.extent, logfsunconditionallymarkfreeanddirty, server);
return nil;
}
ds.server = server;
ds.nentries = 0;
ds.maxentries = 0;
ds.array = nil;
rv = logfsextentlistwalkrange(e->u.file.extent, findpageset, &ds, min, max);
if(rv < 0 || ds.nentries == 0)
goto Out;
if(server->trace > 1){
print("pass 1\n");
for(x = 0; x < ds.nentries; x++){
p = &ds.array[x];
seq = p->pageaddr >> server->ll->l2pagesperblock;
page = p->pageaddr & ((1 << server->ll->l2pagesperblock) - 1);
print("block %lud page %ud ref %d\n", seq, page, p->ref);
}
print("pass 2\n");
}
rv = logfsextentlistwalk(e->u.file.extent, addpagereferences, &ds);
if(rv >= 0){
for(x = 0; x < ds.nentries; x++){
p = &ds.array[x];
seq = p->pageaddr >> server->ll->l2pagesperblock;
page = p->pageaddr & ((1 << server->ll->l2pagesperblock) - 1);
if(server->trace > 1)
print("block %lud page %ud ref %d\n", seq, page, p->ref);
if(p->ref == 0)
logfsfreedatapages(server, seq, logfsdatapagemask(1, page));
}
}
Out:
logfsfreemem(ds.array);
return rv < 0 ? Enomem : nil;
}
static void
disposeofoldblock(LogfsServer *server, AllocState *state)
{
if(state->oldblock >= 0) {
if(server->testflags & LogfsTestDontFettleDataBlock) {
(*server->ll->setblocktag)(server->ll, state->oldblock, LogfsTworse);
server->testflags &= ~LogfsTestDontFettleDataBlock;
}
else {
if(state->markbad)
(*server->ll->markblockbad)(server->ll, state->oldblock);
else
logfsbootfettleblock(server->lb, state->oldblock, LogfsTnone, ~0, nil);
}
state->oldblock = -1;
}
}
char *
logfsserverwrite(LogfsServer *server, u32int fid, u32int offset, u32int count, uchar *buf, u32int *rcount)
{
Fid *f;
Entry *e;
u32int now;
char *muid;
int muidlen;
LogfsLowLevel *ll = server->ll;
if(server->trace > 1)
print("logfsserverwrite(%ud, %ud, %ud)\n", fid, offset, count);
f = logfsfidmapfindentry(server->fidmap, fid);
if(f == nil)
return logfsebadfid;
if(f->openmode < 0)
return logfsefidnotopen;
if((f->openmode & 3) == OREAD)
return logfseaccess;
e = f->entry;
if(e->deadandgone)
return Eio;
if(e->qid.type & QTDIR)
return Eperm;
if(e->perm & DMAPPEND)
offset = e->u.file.length;
now = logfsnow();
*rcount = count;
muid = logfsisfindidfromname(server->is, f->uname);
muidlen = strlen(muid);
while(count) {
Extent extent;
int thistime;
char *errmsg;
thistime = lognicesizeforwrite(server, 1, count, muidlen);
if(thistime == 0) {
int p;
u32int n;
long blockindex;
int pagebase;
AllocState state;
int pagesize = 1 << ll->l2pagesize;
reallocate:
errmsg = allocdatapages(server, count, &thistime, &blockindex, &pagebase, &extent.flashaddr, &state);
if(errmsg)
return errmsg;
if(thistime == 0)
return logfselogfull;
for(p = pagebase, n = 0; n < thistime; p++, n += pagesize) {
u32int mask;
DataBlock *db = server->datablock + blockindex;
errmsg = (*ll->writepage)(ll, buf + n, db->block, p);
if(errmsg) {
if(strcmp(errmsg, Eio) != 0) {
if(p != 0) {
disposeofoldblock(server, &state);
mask = logfsdatapagemask(p - pagebase - 1, pagebase);
db->free |= mask;
db->dirty |= mask;
return errmsg;
}
if(state.oldblock < 0) {
(*ll->markblockbad)(ll, db->block);
db->block = -1;
if(blockindex == server->ndatablocks - 1)
server->ndatablocks--;
return errmsg;
}
(*ll->markblockbad)(ll, db->block);
db->block = state.oldblock;
return errmsg;
}
if(state.oldblock >= 0) {
(*ll->markblockbad)(ll, db->block);
db->block = state.oldblock;
}
else {
errmsg = logfsserverreplacedatablock(server, blockindex);
if(errmsg)
return errmsg;
}
goto reallocate;
}
mask = logfsdatapagemask(1, p);
db->free &= ~mask;
db->dirty |= mask;
}
errmsg = logfslogwrite(server, 1, e->qid.path, offset, thistime, now, e->u.file.cvers,
muid, nil, &extent.flashaddr);
disposeofoldblock(server, &state);
}
else {
if(thistime > count)
thistime = count;
errmsg = logfslogwrite(server, 1, e->qid.path, offset, thistime, now, e->u.file.cvers,
muid, buf, &extent.flashaddr);
}
if(errmsg)
return errmsg;
if(server->trace > 1)
print("logfsserverwrite: %d bytes at flashaddr 0x%.8ux\n", thistime, extent.flashaddr);
extent.min = offset;
extent.max = offset + thistime;
errmsg = zappages(server, e, extent.min, extent.max);
if(errmsg)
return errmsg;
errmsg = logfsextentlistinsert(e->u.file.extent, &extent, nil);
if(errmsg)
return errmsg;
e->muid = muid;
e->mtime = now;
offset += thistime;
if(e->u.file.length < offset)
e->u.file.length = offset;
count -= thistime;
buf += thistime;
e->qid.vers++;
}
return nil;
}