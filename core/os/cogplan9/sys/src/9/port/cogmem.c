#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
typedef struct CogMemBlock CogMemBlock;
struct CogMemBlock {
void	*addr;
ulong	size;
short	sti;
short	lti;
int	type;
ulong	lastaccess;
CogMemBlock *next;
Lock;
};
enum {
MemAtom = 1,
MemLink,
MemPattern,
MemInference,
MemAttention,
MemGeneral,
};
typedef struct CogMemPool CogMemPool;
struct CogMemPool {
CogMemBlock	*blocks;
int		nblocks;
ulong		totalmem;
ulong		maxmem;
short		totalsti;
short		totallti;
Lock;
};
static CogMemPool cogmempool;
void
cogmeminit(void)
{
cogmempool.blocks = nil;
cogmempool.nblocks = 0;
cogmempool.totalmem = 0;
cogmempool.maxmem = 1024*1024*1024;
cogmempool.totalsti = 10000;
cogmempool.totallti = 5000;
}
void*
cogalloc(ulong size, int type, short sti, short lti)
{
CogMemBlock *block;
void *addr;
lock(&cogmempool);
if(cogmempool.totalmem + size > cogmempool.maxmem) {
cogreclaim(size);
if(cogmempool.totalmem + size > cogmempool.maxmem) {
unlock(&cogmempool);
return nil;
}
}
addr = malloc(size);
if(addr == nil) {
unlock(&cogmempool);
return nil;
}
block = malloc(sizeof(CogMemBlock));
if(block == nil) {
free(addr);
unlock(&cogmempool);
return nil;
}
block->addr = addr;
block->size = size;
block->sti = sti;
block->lti = lti;
block->type = type;
block->lastaccess = m->ticks;
block->next = cogmempool.blocks;
cogmempool.blocks = block;
cogmempool.nblocks++;
cogmempool.totalmem += size;
unlock(&cogmempool);
return addr;
}
void
cogfree(void *addr)
{
CogMemBlock *block, *prev;
if(addr == nil)
return;
lock(&cogmempool);
prev = nil;
for(block = cogmempool.blocks; block != nil; block = block->next) {
if(block->addr == addr) {
if(prev == nil)
cogmempool.blocks = block->next;
else
prev->next = block->next;
cogmempool.totalmem -= block->size;
cogmempool.nblocks--;
free(addr);
free(block);
unlock(&cogmempool);
return;
}
prev = block;
}
unlock(&cogmempool);
}
void
cogmemupdate(void *addr, short sti, short lti)
{
CogMemBlock *block;
lock(&cogmempool);
for(block = cogmempool.blocks; block != nil; block = block->next) {
if(block->addr == addr) {
lock(block);
block->sti += sti;
block->lti += lti;
block->lastaccess = m->ticks;
unlock(block);
break;
}
}
unlock(&cogmempool);
}
int
cogreclaim(ulong needed)
{
CogMemBlock *block, *victim, *prev, *vprev;
int minscore, score;
ulong reclaimed;
reclaimed = 0;
while(reclaimed < needed) {
victim = nil;
vprev = nil;
minscore = 10000;
prev = nil;
for(block = cogmempool.blocks; block != nil; block = block->next) {
lock(block);
score = block->sti + (block->lti / 10);
if(m->ticks - block->lastaccess > HZ*60)
score /= 2;
unlock(block);
if(score < minscore) {
minscore = score;
victim = block;
vprev = prev;
}
prev = block;
}
if(victim == nil)
break;
if(vprev == nil)
cogmempool.blocks = victim->next;
else
vprev->next = victim->next;
reclaimed += victim->size;
cogmempool.totalmem -= victim->size;
cogmempool.nblocks--;
free(victim->addr);
free(victim);
}
return reclaimed >= needed ? 0 : -1;
}
void
cogmemdecay(void)
{
CogMemBlock *block;
lock(&cogmempool);
for(block = cogmempool.blocks; block != nil; block = block->next) {
lock(block);
if(block->sti > 0)
block->sti--;
if(block->lti > 0 && m->ticks % (HZ*10) == 0)
block->lti--;
unlock(block);
}
unlock(&cogmempool);
}
void
cogmemstats(ulong *total, ulong *max, int *nblocks)
{
lock(&cogmempool);
*total = cogmempool.totalmem;
*max = cogmempool.maxmem;
*nblocks = cogmempool.nblocks;
unlock(&cogmempool);
}
CogMemBlock*
cogmemfind(int type)
{
CogMemBlock *block;
lock(&cogmempool);
for(block = cogmempool.blocks; block != nil; block = block->next) {
if(block->type == type) {
unlock(&cogmempool);
return block;
}
}
unlock(&cogmempool);
return nil;
}
void*
cogallocatom(ulong size, short sti)
{
return cogalloc(size, MemAtom, sti, sti/2);
}
void*
cogalloclink(ulong size, short sti)
{
return cogalloc(size, MemLink, sti, sti/2);
}
void*
cogallocpattern(ulong size)
{
return cogalloc(size, MemPattern, 50, 100);
}
void*
cogallocinfer(ulong size)
{
return cogalloc(size, MemInference, 100, 20);
}
void
cogmempromote(void *addr)
{
cogmemupdate(addr, 10, 5);
}
void
cogmemdemote(void *addr)
{
cogmemupdate(addr, -10, -5);
}
void
cogmemtouch(void *addr)
{
CogMemBlock *block;
lock(&cogmempool);
for(block = cogmempool.blocks; block != nil; block = block->next) {
if(block->addr == addr) {
lock(block);
block->lastaccess = m->ticks;
unlock(block);
break;
}
}
unlock(&cogmempool);
}
int
coggc(void)
{
CogMemBlock *block, *next, *prev;
int collected;
ulong now;
collected = 0;
now = m->ticks;
lock(&cogmempool);
prev = nil;
block = cogmempool.blocks;
while(block != nil) {
next = block->next;
lock(block);
if(block->sti <= 0 && block->lti <= 0 &&
now - block->lastaccess > HZ*60) {
unlock(block);
if(prev == nil)
cogmempool.blocks = next;
else
prev->next = next;
cogmempool.totalmem -= block->size;
cogmempool.nblocks--;
free(block->addr);
free(block);
collected++;
block = next;
continue;
}
unlock(block);
prev = block;
block = next;
}
unlock(&cogmempool);
return collected;
}