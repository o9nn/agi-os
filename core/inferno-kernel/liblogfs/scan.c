#include "logfsos.h"
#include "logfs.h"
#include "local.h"
#include "fcall.h"
typedef struct PathEnt {
ulong path;
long block;
} PathEnt;
typedef struct GenInfo {
long start;
long end;
int gaps;
} GenInfo;
static int
dataorder(ulong p1, ulong p2)
{
int o;
o = dataseqof(p1) - dataseqof(p2);
if(o != 0)
return o;
return copygenof(p1) - copygenof(p2);
}
static int
logorder(ulong p1, ulong p2)
{
int o;
o = loggenof(p1) - loggenof(p2);
if(o != 0)
return o;
o = logseqof(p1) - logseqof(p2);
if(o != 0)
return o;
return copygenof(p1) - copygenof(p2);
}
static void
insert(PathEnt *pathmap, long entries, ulong path, long block, int (*order)(ulong p1, ulong p2))
{
long i;
for(i = 0; i < entries; i++)
if((*order)(path, pathmap[i].path) < 0)
break;
memmove(&pathmap[i + 1], &pathmap[i], (entries - i) * sizeof(PathEnt));
pathmap[i].path = path;
pathmap[i].block = block;
}
static void
populate(LogSegment *seg, int gen, long unsweptblockindex, long curblockindex, PathEnt *pathent)
{
long i;
seg->gen = gen;
seg->unsweptblockindex = unsweptblockindex;
seg->curblockindex = curblockindex;
for(i = unsweptblockindex; i <= curblockindex; i++) {
seg->blockmap[i] = pathent->block;
pathent++;
}
}
static int
dataduplicate(PathEnt *p1, PathEnt *p2)
{
return dataseqof(p2->path) == dataseqof(p1->path)
&& copygenof(p2->path) == copygensucc(copygenof(p1->path));
}
static char *
eliminateduplicates(LogfsServer *server, char *name, PathEnt *map, long *entriesp)
{
long i;
long k = *entriesp;
for(i = 0; i < k;) {
PathEnt *prev = &map[i - 1];
PathEnt *this = &map[i];
if(i > 0 && dataduplicate(prev, this)) {
print("%s duplicate detected\n", name);
if(i + 1 < k && dataduplicate(this, &map[i + 1]))
return "three or more copies of same block";
if(copygensucc(copygenof(this->path)) == copygenof(prev->path)) {
PathEnt m;
m = *this;
*this = *prev;
*prev = m;
}
else if(copygensucc(copygenof(prev->path)) != copygenof(this->path))
return "duplicate blocks but copy generations not sequential";
logfsbootfettleblock(server->lb, prev->block, LogfsTnone, ~0, nil);
memmove(prev, this, sizeof(PathEnt) * (k - i));
k--;
continue;
}
i++;
}
*entriesp = k;
return nil;
}
char *
logfsscan(LogfsServer *server)
{
LogfsLowLevel *ll = server->ll;
long b;
long i;
long logfound = 0;
long datafound = 0;
PathEnt *logpathmap, *datapathmap;
GenInfo geninfo[1 << L2LogSweeps];
int gensfound, lastgenfound;
int g0, g1;
char *errmsg;
logpathmap = logfsrealloc(nil, sizeof(PathEnt) * server->ll->blocks);
datapathmap = logfsrealloc(nil, sizeof(PathEnt) * server->ll->blocks);
if(logpathmap == nil || datapathmap == nil)
return Enomem;
for(b = 0; b < ll->blocks; b++) {
short tag = (*ll->getblocktag)(ll, b);
ulong path = (*ll->getblockpath)(ll, b);
switch(tag) {
case LogfsTlog:
insert(logpathmap, logfound++, path, b, logorder);
break;
case LogfsTdata:
insert(datapathmap, datafound++, path, b, dataorder);
break;
}
}
if(server->trace > 1) {
for(i = 0; i < logfound; i++)
print("log gen %lud seq %lud copygen %lud block %ld\n",
loggenof(logpathmap[i].path), logseqof(logpathmap[i].path), copygenof(datapathmap[i].path), logpathmap[i].block);
for(i = 0; i < datafound; i++)
print("data seq %lud copygen %lud block %ld\n",
dataseqof(datapathmap[i].path), copygenof(datapathmap[i].path), datapathmap[i].block);
}
errmsg = eliminateduplicates(server, "data", datapathmap, &datafound);
if(errmsg)
goto fail;
if(datafound)
server->ndatablocks = dataseqof(datapathmap[datafound - 1].path) + 1;
else
server->ndatablocks = 0;
for(i = 0; i < server->ndatablocks; i++)
server->datablock[i].block = -1;
for(i = 0; i < datafound; i++) {
long j;
j = dataseqof(datapathmap[i].path);
server->datablock[j].path = datapathmap[i].path;
server->datablock[j].block = datapathmap[i].block;
server->datablock[j].dirty = server->datablock[j].free = logfsdatapagemask(1 << ll->l2pagesperblock, 0);
}
errmsg = eliminateduplicates(server, "log", logpathmap, &logfound);
if(errmsg)
goto fail;
gensfound = 0;
lastgenfound = -1;
for(i = 0; i < nelem(geninfo); i++)
geninfo[i].start = -1;
for(i = 0; i < logfound; i++) {
int gen;
gen = loggenof(logpathmap[i].path);
if(geninfo[gen].start < 0) {
if(lastgenfound >= 0)
geninfo[lastgenfound].end = i;
geninfo[gen].start = i;
lastgenfound = gen;
geninfo[gen].gaps = 0;
gensfound++;
}
else if(!geninfo[lastgenfound].gaps && logseqof(logpathmap[i - 1].path) + 1 != logseqof(logpathmap[i].path)) {
geninfo[lastgenfound].gaps = 1;
print("generation %d has gaps (%lud, %lud)\n", lastgenfound,
logseqof(logpathmap[i - 1].path), logseqof(logpathmap[i].path));
}
}
if(lastgenfound >= 0)
geninfo[lastgenfound].end = i;
if(server->trace > 1) {
for(i = 0; i < nelem(geninfo); i++)
print("geninfo: %ld: start %ld end %ld gaps %d\n", i, geninfo[i].start, geninfo[i].end, geninfo[i].gaps);
}
switch(gensfound) {
case 0:
break;
case 1:
for(g0 = 0; g0 < nelem(geninfo); g0++)
if(geninfo[g0].start >= 0)
break;
if(geninfo[g0].gaps || geninfo[g0].start != 0) {
errmsg = "missing log blocks";
goto fail;
}
populate(server->activelog, g0, 0, geninfo[g0].end - geninfo[g0].start - 1, logpathmap + geninfo[g0].start);
break;
case 2:
g0 = -1;
for(g1 = 0; g1 < nelem(geninfo); g1++)
if(geninfo[g1].start >= 0) {
if(g0 < 0)
g0 = g1;
else
break;
}
if(geninfo[g0].gaps || geninfo[g1].gaps) {
errmsg = "missing log blocks";
goto fail;
}
if(g0 == loggensucc(g1)) {
int tmp = g0;
g0 = g1;
g1 = tmp;
}
else if(g1 != loggensucc(g0)) {
errmsg = "nonsequential generations in log";
goto fail;
}
if(logseqof(logpathmap[geninfo[g1].start].path) != 0) {
errmsg = "swept log does not start at 0";
goto fail;
}
if(logseqof(logpathmap[geninfo[g0].start].path) == logseqof(logpathmap[geninfo[g1].end - 1].path)) {
logfsbootfettleblock(server->lb, logpathmap[geninfo[g1].end - 1].block, LogfsTnone, ~0, nil);
geninfo[g1].end--;
}
if(logseqof(logpathmap[geninfo[g0].start].path) < logseqof(logpathmap[geninfo[g1].end - 1].path)) {
errmsg = "active log overlaps end of swept log";
goto fail;
}
populate(server->activelog, g0, logseqof(logpathmap[geninfo[g0].start].path),
logseqof(logpathmap[geninfo[g0].end - 1].path), logpathmap + geninfo[g0].start);
if(server->sweptlog == nil) {
errmsg = logfslogsegmentnew(server, g1, &server->sweptlog);
if(errmsg)
goto fail;
}
populate(server->sweptlog, g1, logseqof(logpathmap[geninfo[g1].start].path),
logseqof(logpathmap[geninfo[g1].end - 1].path), logpathmap + geninfo[g1].start);
break;
default:
errmsg = "more than two generations in log";
goto fail;
}
goto ok;
fail:
logfslogsegmentfree(&server->sweptlog);
ok:
logfsfreemem(logpathmap);
logfsfreemem(datapathmap);
return errmsg;
}