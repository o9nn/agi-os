#include <u.h>
#include <libc.h>
#include <pool.h>
typedef struct Alloc Alloc;
typedef struct Arena Arena;
typedef struct Bhdr Bhdr;
typedef struct Btail Btail;
typedef struct Free Free;
struct Bhdr {
ulong magic;
ulong size;
};
enum {
NOT_MAGIC = 0xdeadfa11,
DEAD_MAGIC = 0xdeaddead,
};
#define B2NB(b) ((Bhdr*)((uchar*)(b)+(b)->size))
#define SHORT(x) (((x)[0] << 8) | (x)[1])
#define PSHORT(p, x) \
(((uchar*)(p))[0] = ((x)>>8)&0xFF, \
((uchar*)(p))[1] = (x)&0xFF)
enum {
TAIL_MAGIC0 = 0xBE,
TAIL_MAGIC1 = 0xEF
};
struct Btail {
uchar magic0;
uchar datasize[2];
uchar magic1;
ulong size;
};
#define B2T(b) ((Btail*)((uchar*)(b)+(b)->size-sizeof(Btail)))
#define B2PT(b) ((Btail*)((uchar*)(b)-sizeof(Btail)))
#define T2HDR(t) ((Bhdr*)((uchar*)(t)+sizeof(Btail)-(t)->size))
struct Free {
Bhdr;
Free* left;
Free* right;
Free* next;
Free* prev;
};
enum {
FREE_MAGIC = 0xBA5EBA11,
};
struct Alloc {
Bhdr;
};
enum {
ALLOC_MAGIC = 0x0A110C09,
UNALLOC_MAGIC = 0xCAB00D1E+1,
};
struct Arena {
Bhdr;
Arena* aup;
Arena* down;
ulong asize;
ulong pad;
};
enum {
ARENA_MAGIC = 0xC0A1E5CE+1,
ARENATAIL_MAGIC = 0xEC5E1A0C+1,
};
#define A2TB(a) ((Bhdr*)((uchar*)(a)+(a)->asize-sizeof(Bhdr)))
#define A2B(a) B2NB(a)
enum {
ALIGN_MAGIC = 0xA1F1D1C1,
};
enum {
MINBLOCKSIZE = sizeof(Free)+sizeof(Btail)
};
static uchar datamagic[] = { 0xFE, 0xF1, 0xF0, 0xFA };
#define Poison (void*)0xCafeBabe
#define _B2D(a) ((void*)((uchar*)a+sizeof(Bhdr)))
#define _D2B(v) ((Alloc*)((uchar*)v-sizeof(Bhdr)))
static void* B2D(Pool*, Alloc*);
static Alloc* D2B(Pool*, void*);
static Arena* arenamerge(Pool*, Arena*, Arena*);
static void blockcheck(Pool*, Bhdr*);
static Alloc* blockmerge(Pool*, Bhdr*, Bhdr*);
static Alloc* blocksetdsize(Pool*, Alloc*, ulong);
static Bhdr* blocksetsize(Bhdr*, ulong);
static ulong bsize2asize(Pool*, ulong);
static ulong dsize2bsize(Pool*, ulong);
static ulong getdsize(Alloc*);
static Alloc* trim(Pool*, Alloc*, ulong);
static Free* listadd(Free*, Free*);
static void logstack(Pool*);
static Free** ltreewalk(Free**, ulong);
static void memmark(void*, int, ulong);
static Free* pooladd(Pool*, Alloc*);
static void* poolallocl(Pool*, ulong);
static void poolcheckl(Pool*);
static void poolcheckarena(Pool*, Arena*);
static int poolcompactl(Pool*);
static Alloc* pooldel(Pool*, Free*);
static void pooldumpl(Pool*);
static void pooldumparena(Pool*, Arena*);
static void poolfreel(Pool*, void*);
static void poolnewarena(Pool*, ulong);
static void* poolreallocl(Pool*, void*, ulong);
static Free* treedelete(Free*, Free*);
static Free* treeinsert(Free*, Free*);
static Free* treelookup(Free*, ulong);
static Free* treelookupgt(Free*, ulong);
#define antagonism if(!(p->flags & POOL_ANTAGONISM)){}else
#define paranoia if(!(p->flags & POOL_PARANOIA)){}else
#define verbosity if(!(p->flags & POOL_VERBOSITY)){}else
#define DPRINT if(!(p->flags & POOL_DEBUGGING)){}else p->print
#define LOG if(!(p->flags & POOL_LOGGING)){}else p->print
static void
checklist(Free *t)
{
Free *q;
for(q=t->next; q!=t; q=q->next){
assert(q->size == t->size);
assert(q->next==nil || q->next->prev==q);
assert(q->prev==nil || q->prev->next==q);
assert(q->magic==FREE_MAGIC);
}
}
static void
checktree(Free *t, int a, int b)
{
assert(t->magic==FREE_MAGIC);
assert(a < t->size && t->size < b);
assert(t->next==nil || t->next->prev==t);
assert(t->prev==nil || t->prev->next==t);
checklist(t);
if(t->left)
checktree(t->left, a, t->size);
if(t->right)
checktree(t->right, t->size, b);
}
static Free**
ltreewalk(Free **t, ulong size)
{
assert(t != nil );
for(;;) {
if(*t == nil)
return t;
assert((*t)->magic == FREE_MAGIC);
if(size == (*t)->size)
return t;
if(size < (*t)->size)
t = &(*t)->left;
else
t = &(*t)->right;
}
}
static Free*
treelookup(Free *t, ulong size)
{
return *ltreewalk(&t, size);
}
static Free*
treeinsert(Free *tree, Free *node)
{
Free **loc, *repl;
assert(node != nil );
loc = ltreewalk(&tree, node->size);
if(*loc == nil) {
node->left = nil;
node->right = nil;
} else {
repl = *loc;
node->left = repl->left;
node->right = repl->right;
}
*loc = node;
return tree;
}
static Free*
treedelete(Free *tree, Free *node)
{
Free **loc, **lsucc, *succ;
assert(node != nil );
loc = ltreewalk(&tree, node->size);
assert(*loc == node);
if(node->left == nil)
*loc = node->right;
else if(node->right == nil)
*loc = node->left;
else {
for(lsucc = &node->right; (*lsucc)->left; lsucc = &(*lsucc)->left)
;
succ = *lsucc;
*lsucc = succ->right;
succ->left = node->left;
succ->right = node->right;
*loc = succ;
}
node->left = node->right = Poison;
return tree;
}
static Free*
treelookupgt(Free *t, ulong size)
{
Free *lastgood;
lastgood = nil;
for(;;) {
if(t == nil)
return lastgood;
if(size == t->size)
return t;
if(size < t->size) {
lastgood = t;
t = t->left;
} else
t = t->right;
}
}
static Free*
listadd(Free *list, Free *node)
{
if(list == nil) {
node->next = node;
node->prev = node;
return node;
}
node->prev = list->prev;
node->next = list;
node->prev->next = node;
node->next->prev = node;
return list;
}
static Free*
listdelete(Pool *p, Free *list, Free *node)
{
if(node->next == node) {
node->prev = node->next = Poison;
return nil;
}
if(node->next == nil)
p->panic(p, "pool->next");
if(node->prev == nil)
p->panic(p, "pool->prev");
node->next->prev = node->prev;
node->prev->next = node->next;
if(list == node)
list = node->next;
node->prev = node->next = Poison;
return list;
}
static Free*
pooladd(Pool *p, Alloc *anode)
{
Free *lst, *olst;
Free *node;
Free **parent;
antagonism {
memmark(_B2D(anode), 0xF7, anode->size-sizeof(Bhdr)-sizeof(Btail));
}
node = (Free*)anode;
node->magic = FREE_MAGIC;
parent = ltreewalk(&p->freeroot, node->size);
olst = *parent;
lst = listadd(olst, node);
if(olst != lst)
*parent = treeinsert(*parent, lst);
p->curfree += node->size;
return node;
}
static Alloc*
pooldel(Pool *p, Free *node)
{
Free *lst, *olst;
Free **parent;
parent = ltreewalk(&p->freeroot, node->size);
olst = *parent;
assert(olst != nil );
lst = listdelete(p, olst, node);
if(lst == nil)
*parent = treedelete(*parent, olst);
else if(lst != olst)
*parent = treeinsert(*parent, lst);
node->left = node->right = Poison;
p->curfree -= node->size;
antagonism {
memmark(_B2D(node), 0xF9, node->size-sizeof(Bhdr)-sizeof(Btail));
}
node->magic = UNALLOC_MAGIC;
return (Alloc*)node;
}
static ulong
dsize2bsize(Pool *p, ulong sz)
{
sz += sizeof(Bhdr)+sizeof(Btail);
if(sz < p->minblock)
sz = p->minblock;
if(sz < MINBLOCKSIZE)
sz = MINBLOCKSIZE;
sz = (sz+p->quantum-1)&~(p->quantum-1);
return sz;
}
static ulong
bsize2asize(Pool *p, ulong sz)
{
sz += sizeof(Arena)+sizeof(Btail);
if(sz < p->minarena)
sz = p->minarena;
sz = (sz+p->quantum)&~(p->quantum-1);
return sz;
}
static Alloc*
blockmerge(Pool *pool, Bhdr *a, Bhdr *b)
{
Btail *t;
assert(B2NB(a) == b);
if(a->magic == FREE_MAGIC)
pooldel(pool, (Free*)a);
if(b->magic == FREE_MAGIC)
pooldel(pool, (Free*)b);
t = B2T(a);
t->size = (ulong)Poison;
t->magic0 = NOT_MAGIC;
t->magic1 = NOT_MAGIC;
PSHORT(t->datasize, NOT_MAGIC);
a->size += b->size;
t = B2T(a);
t->size = a->size;
PSHORT(t->datasize, 0xFFFF);
b->size = NOT_MAGIC;
b->magic = NOT_MAGIC;
a->magic = UNALLOC_MAGIC;
return (Alloc*)a;
}
static Bhdr*
blocksetsize(Bhdr *b, ulong bsize)
{
Btail *t;
assert(b->magic != FREE_MAGIC );
b->size = bsize;
t = B2T(b);
t->size = b->size;
t->magic0 = TAIL_MAGIC0;
t->magic1 = TAIL_MAGIC1;
return b;
}
static ulong
getdsize(Alloc *b)
{
Btail *t;
t = B2T(b);
return b->size - SHORT(t->datasize);
}
static Alloc*
blocksetdsize(Pool *p, Alloc *b, ulong dsize)
{
Btail *t;
uchar *q, *eq;
assert(b->size >= dsize2bsize(p, dsize));
assert(b->size - dsize < 0x10000);
t = B2T(b);
PSHORT(t->datasize, b->size - dsize);
q=(uchar*)_B2D(b)+dsize;
eq = (uchar*)t;
if(eq > q+4)
eq = q+4;
for(; q<eq; q++)
*q = datamagic[((ulong)(uintptr)q)%nelem(datamagic)];
return b;
}
static Alloc*
trim(Pool *p, Alloc *b, ulong dsize)
{
ulong extra, bsize;
Alloc *frag;
bsize = dsize2bsize(p, dsize);
extra = b->size - bsize;
if(b->size - dsize >= 0x10000 ||
(extra >= bsize>>2 && extra >= MINBLOCKSIZE && extra >= p->minblock)) {
blocksetsize(b, bsize);
frag = (Alloc*) B2NB(b);
antagonism {
memmark(frag, 0xF1, extra);
}
frag->magic = UNALLOC_MAGIC;
blocksetsize(frag, extra);
pooladd(p, frag);
}
b->magic = ALLOC_MAGIC;
blocksetdsize(p, b, dsize);
return b;
}
static Alloc*
freefromfront(Pool *p, Alloc *b, ulong skip)
{
Alloc *bb;
skip = skip&~(p->quantum-1);
if(skip >= 0x1000 || (skip >= b->size>>2 && skip >= MINBLOCKSIZE && skip >= p->minblock)){
bb = (Alloc*)((uchar*)b+skip);
blocksetsize(bb, b->size-skip);
bb->magic = UNALLOC_MAGIC;
blocksetsize(b, skip);
b->magic = UNALLOC_MAGIC;
pooladd(p, b);
return bb;
}
return b;
}
static void
arenasetsize(Arena *a, ulong asize)
{
Bhdr *atail;
a->asize = asize;
atail = A2TB(a);
atail->magic = ARENATAIL_MAGIC;
atail->size = 0;
}
static void
poolnewarena(Pool *p, ulong asize)
{
Arena *a;
Arena *ap, *lastap;
Alloc *b;
LOG(p, "newarena %lud\n", asize);
if(p->cursize+asize > p->maxsize) {
if(poolcompactl(p) == 0){
LOG(p, "pool too big: %lud+%lud > %lud\n",
p->cursize, asize, p->maxsize);
werrstr("memory pool too large");
}
return;
}
if((a = p->alloc(asize)) == nil) {
return;
}
p->cursize += asize;
a->magic = ARENA_MAGIC;
blocksetsize(a, sizeof(Arena));
arenasetsize(a, asize);
blockcheck(p, a);
b = (Alloc*)A2B(a);
b->magic = UNALLOC_MAGIC;
blocksetsize(b, (uchar*)A2TB(a)-(uchar*)b);
blockcheck(p, b);
pooladd(p, b);
blockcheck(p, b);
for(lastap=nil, ap=p->arenalist; ap > a; lastap=ap, ap=ap->down)
;
if(a->down = ap)
a->down->aup = a;
if(a->aup = lastap)
a->aup->down = a;
else
p->arenalist = a;
if(a->aup)
arenamerge(p, a, a->aup);
if(a->down)
arenamerge(p, a->down, a);
}
static void
blockgrow(Pool *p, Bhdr *b, ulong nsize)
{
if(b->magic == FREE_MAGIC) {
Alloc *a;
Bhdr *bnxt;
a = pooldel(p, (Free*)b);
blockcheck(p, a);
blocksetsize(a, nsize);
blockcheck(p, a);
bnxt = B2NB(a);
if(bnxt->magic == FREE_MAGIC)
a = blockmerge(p, a, bnxt);
blockcheck(p, a);
pooladd(p, a);
} else {
Alloc *a;
ulong dsize;
a = (Alloc*)b;
dsize = getdsize(a);
blocksetsize(a, nsize);
trim(p, a, dsize);
}
}
static Arena*
arenamerge(Pool *p, Arena *bot, Arena *top)
{
Bhdr *bbot, *btop;
Btail *t;
blockcheck(p, bot);
blockcheck(p, top);
assert(bot->aup == top && top > bot);
if(p->merge == nil || p->merge(bot, top) == 0)
return nil;
if(bot->aup = top->aup)
bot->aup->down = bot;
else
p->arenalist = bot;
t = B2PT(A2TB(bot));
bbot = T2HDR(t);
btop = A2B(top);
blockcheck(p, bbot);
blockcheck(p, btop);
arenasetsize(bot, top->asize + ((uchar*)top - (uchar*)bot));
blockgrow(p, bbot, (uchar*)btop-(uchar*)bbot);
blockcheck(p, bbot);
return bot;
}
static void
dumpblock(Pool *p, Bhdr *b)
{
ulong *dp;
ulong dsize;
uchar *cp;
dp = (ulong*)b;
p->print(p, "pool %s block %p\nhdr %.8lux %.8lux %.8lux %.8lux %.8lux %.8lux\n",
p->name, b, dp[0], dp[1], dp[2], dp[3], dp[4], dp[5], dp[6]);
dp = (ulong*)B2T(b);
p->print(p, "tail %.8lux %.8lux %.8lux %.8lux %.8lux %.8lux | %.8lux %.8lux\n",
dp[-6], dp[-5], dp[-4], dp[-3], dp[-2], dp[-1], dp[0], dp[1]);
if(b->magic == ALLOC_MAGIC){
dsize = getdsize((Alloc*)b);
if(dsize >= b->size)
return;
cp = (uchar*)_B2D(b)+dsize;
p->print(p, "user data ");
p->print(p, "%.2ux %.2ux %.2ux %.2ux  %.2ux %.2ux %.2ux %.2ux",
cp[-8], cp[-7], cp[-6], cp[-5], cp[-4], cp[-3], cp[-2], cp[-1]);
p->print(p, " | %.2ux %.2ux %.2ux %.2ux  %.2ux %.2ux %.2ux %.2ux\n",
cp[0], cp[1], cp[2], cp[3], cp[4], cp[5], cp[6], cp[7]);
}
}
static void
printblock(Pool *p, Bhdr *b, char *msg)
{
p->print(p, "%s\n", msg);
dumpblock(p, b);
}
static void
panicblock(Pool *p, Bhdr *b, char *msg)
{
p->print(p, "%s\n", msg);
dumpblock(p, b);
p->panic(p, "pool panic");
}
static void
blockcheck(Pool *p, Bhdr *b)
{
Alloc *a;
Btail *t;
int i, n;
uchar *q, *bq, *eq;
ulong dsize;
switch(b->magic) {
default:
panicblock(p, b, "bad magic");
case FREE_MAGIC:
case UNALLOC_MAGIC:
t = B2T(b);
if(t->magic0 != TAIL_MAGIC0 || t->magic1 != TAIL_MAGIC1)
panicblock(p, b, "corrupt tail magic");
if(T2HDR(t) != b)
panicblock(p, b, "corrupt tail ptr");
break;
case DEAD_MAGIC:
t = B2T(b);
if(t->magic0 != TAIL_MAGIC0 || t->magic1 != TAIL_MAGIC1)
panicblock(p, b, "corrupt tail magic");
if(T2HDR(t) != b)
panicblock(p, b, "corrupt tail ptr");
n = getdsize((Alloc*)b);
q = _B2D(b);
q += 8;
for(i=8; i<n; i++)
if(*q++ != 0xDA)
panicblock(p, b, "dangling pointer write");
break;
case ARENA_MAGIC:
b = A2TB((Arena*)b);
if(b->magic != ARENATAIL_MAGIC)
panicblock(p, b, "bad arena size");
case ARENATAIL_MAGIC:
if(b->size != 0)
panicblock(p, b, "bad arena tail size");
break;
case ALLOC_MAGIC:
a = (Alloc*)b;
t = B2T(b);
dsize = getdsize(a);
bq = (uchar*)_B2D(a)+dsize;
eq = (uchar*)t;
if(t->magic0 != TAIL_MAGIC0){
if((p->flags & POOL_TOLERANCE) && bq == eq && t->magic0 == 0)
printblock(p, b, "mem user overflow (magic0)");
else
panicblock(p, b, "corrupt tail magic0");
}
if(t->magic1 != TAIL_MAGIC1)
panicblock(p, b, "corrupt tail magic1");
if(T2HDR(t) != b)
panicblock(p, b, "corrupt tail ptr");
if(dsize2bsize(p, dsize) > a->size)
panicblock(p, b, "too much block data");
if(eq > bq+4)
eq = bq+4;
for(q=bq; q<eq; q++){
if(*q != datamagic[((uintptr)q)%nelem(datamagic)]){
if(q == bq && *q == 0 && (p->flags & POOL_TOLERANCE)){
printblock(p, b, "mem user overflow");
continue;
}
panicblock(p, b, "mem user overflow");
}
}
break;
}
}
enum {
FLOATING_MAGIC = 0xCBCBCBCB,
};
static int
arenacompact(Pool *p, Arena *a)
{
Bhdr *b, *wb, *eb, *nxt;
int compacted;
if(p->move == nil)
p->panic(p, "don't call me when pool->move is nil\n");
poolcheckarena(p, a);
eb = A2TB(a);
compacted = 0;
for(b=wb=A2B(a); b && b < eb; b=nxt) {
nxt = B2NB(b);
switch(b->magic) {
case FREE_MAGIC:
pooldel(p, (Free*)b);
b->magic = FLOATING_MAGIC;
break;
case ALLOC_MAGIC:
if(wb != b) {
memmove(wb, b, b->size);
p->move(_B2D(b), _B2D(wb));
compacted = 1;
}
wb = B2NB(wb);
break;
}
}
if(wb < eb) {
wb->magic = UNALLOC_MAGIC;
blocksetsize(wb, (uchar*)eb-(uchar*)wb);
pooladd(p, (Alloc*)wb);
}
return compacted;
}
static int
poolcompactl(Pool *pool)
{
Arena *a;
int compacted;
if(pool->move == nil || pool->lastcompact == pool->nfree)
return 0;
pool->lastcompact = pool->nfree;
compacted = 0;
for(a=pool->arenalist; a; a=a->down)
compacted |= arenacompact(pool, a);
return compacted;
}
static void*
B2D(Pool *p, Alloc *a)
{
if(a->magic != ALLOC_MAGIC)
p->panic(p, "B2D called on unworthy block");
return _B2D(a);
}
static Alloc*
D2B(Pool *p, void *v)
{
Alloc *a;
ulong *u;
if((uintptr)v&(sizeof(ulong)-1))
v = (char*)v - ((uintptr)v&(sizeof(ulong)-1));
u = v;
while(u[-1] == ALIGN_MAGIC)
u--;
a = _D2B(u);
if(a->magic != ALLOC_MAGIC)
p->panic(p, "D2B called on non-block %p (double-free?)", v);
return a;
}
static void*
poolallocl(Pool *p, ulong dsize)
{
ulong bsize;
Free *fb;
Alloc *ab;
if(dsize >= 0x80000000UL){
werrstr("invalid allocation size");
return nil;
}
bsize = dsize2bsize(p, dsize);
fb = treelookupgt(p->freeroot, bsize);
if(fb == nil) {
poolnewarena(p, bsize2asize(p, bsize));
if((fb = treelookupgt(p->freeroot, bsize)) == nil) {
return nil;
}
}
ab = trim(p, pooldel(p, fb), dsize);
p->curalloc += ab->size;
antagonism {
memset(B2D(p, ab), 0xDF, dsize);
}
return B2D(p, ab);
}
static void*
poolreallocl(Pool *p, void *v, ulong ndsize)
{
Alloc *a;
Bhdr *left, *right, *newb;
Btail *t;
ulong nbsize;
ulong odsize;
ulong obsize;
void *nv;
if(v == nil)
return poolallocl(p, ndsize);
if(ndsize == 0) {
poolfreel(p, v);
return nil;
}
a = D2B(p, v);
blockcheck(p, a);
odsize = getdsize(a);
obsize = a->size;
nbsize = dsize2bsize(p, ndsize);
if(nbsize <= a->size) {
Returnblock:
if(v != _B2D(a))
memmove(_B2D(a), v, odsize);
a = trim(p, a, ndsize);
p->curalloc -= obsize;
p->curalloc += a->size;
v = B2D(p, a);
return v;
}
right = B2NB(a);
if(right->magic == FREE_MAGIC && a->size+right->size >= nbsize) {
a = blockmerge(p, a, right);
goto Returnblock;
}
t = B2PT(a);
left = T2HDR(t);
if(left->magic == FREE_MAGIC && left->size+a->size >= nbsize) {
a = blockmerge(p, left, a);
goto Returnblock;
}
if(left->magic == FREE_MAGIC && right->magic == FREE_MAGIC
&& left->size+a->size+right->size >= nbsize) {
a = blockmerge(p, blockmerge(p, left, a), right);
goto Returnblock;
}
if((nv = poolallocl(p, ndsize)) == nil)
return nil;
left = T2HDR(B2PT(a));
right = B2NB(a);
newb = D2B(p, nv);
if(left == newb || right == newb) {
if(left == newb || left->magic == FREE_MAGIC)
a = blockmerge(p, left, a);
if(right == newb || right->magic == FREE_MAGIC)
a = blockmerge(p, a, right);
assert(a->size >= nbsize);
goto Returnblock;
}
memmove(nv, v, odsize);
antagonism {
memset((char*)nv+odsize, 0xDE, ndsize-odsize);
}
poolfreel(p, v);
return nv;
}
static void*
alignptr(void *v, ulong align, long offset)
{
char *c;
ulong off;
c = v;
if(align){
off = (uintptr)c%align;
if(off != offset){
c += offset - off;
if(off > offset)
c += align;
}
}
return c;
}
static void*
poolallocalignl(Pool *p, ulong dsize, ulong align, long offset, ulong span)
{
ulong asize;
void *v;
char *c;
ulong *u;
int skip;
Alloc *b;
if(align){
if(offset < 0)
offset = align - ((-offset)%align);
else
offset %= align;
}
asize = dsize+align;
v = poolallocl(p, asize);
if(v == nil)
return nil;
if(span && (uintptr)v/span != ((uintptr)v+asize)/span){
poolfreel(p, v);
v = poolallocl(p, 2*asize);
if(v == nil)
return nil;
}
c = alignptr(v, align, offset);
if(span && (uintptr)c/span != (uintptr)(c+dsize-1)/span){
c += span - (uintptr)c%span;
c = alignptr(c, align, offset);
if((uintptr)c/span != (uintptr)(c+dsize-1)/span){
poolfreel(p, v);
werrstr("cannot satisfy dsize %lud span %lud with align %lud+%ld", dsize, span, align, offset);
return nil;
}
}
skip = c - (char*)v;
b = _D2B(v);
b = freefromfront(p, b, skip);
v = _B2D(b);
skip = c - (char*)v;
if(c > (char*)v){
u = v;
while(c >= (char*)u+sizeof(ulong))
*u++ = ALIGN_MAGIC;
}
trim(p, b, skip+dsize);
assert(D2B(p, c) == b);
antagonism {
memset(c, 0xDD, dsize);
}
return c;
}
static void
poolfreel(Pool *p, void *v)
{
Alloc *ab;
Bhdr *back, *fwd;
if(v == nil)
return;
ab = D2B(p, v);
blockcheck(p, ab);
if(p->flags&POOL_NOREUSE){
int n;
ab->magic = DEAD_MAGIC;
n = getdsize(ab)-8;
if(n > 0)
memset((uchar*)v+8, 0xDA, n);
return;
}
p->nfree++;
p->curalloc -= ab->size;
back = T2HDR(B2PT(ab));
if(back->magic == FREE_MAGIC)
ab = blockmerge(p, back, ab);
fwd = B2NB(ab);
if(fwd->magic == FREE_MAGIC)
ab = blockmerge(p, ab, fwd);
pooladd(p, ab);
}
void*
poolalloc(Pool *p, ulong n)
{
void *v;
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
v = poolallocl(p, n);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(p->logstack && (p->flags & POOL_LOGGING)) p->logstack(p);
LOG(p, "poolalloc %p %lud = %p\n", p, n, v);
p->unlock(p);
return v;
}
void*
poolallocalign(Pool *p, ulong n, ulong align, long offset, ulong span)
{
void *v;
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
v = poolallocalignl(p, n, align, offset, span);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(p->logstack && (p->flags & POOL_LOGGING)) p->logstack(p);
LOG(p, "poolalignspanalloc %p %lud %lud %lud %ld = %p\n", p, n, align, span, offset, v);
p->unlock(p);
return v;
}
int
poolcompact(Pool *p)
{
int rv;
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
rv = poolcompactl(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
LOG(p, "poolcompact %p\n", p);
p->unlock(p);
return rv;
}
void*
poolrealloc(Pool *p, void *v, ulong n)
{
void *nv;
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
nv = poolreallocl(p, v, n);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(p->logstack && (p->flags & POOL_LOGGING)) p->logstack(p);
LOG(p, "poolrealloc %p %p %ld = %p\n", p, v, n, nv);
p->unlock(p);
return nv;
}
void
poolfree(Pool *p, void *v)
{
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
poolfreel(p, v);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(p->logstack && (p->flags & POOL_LOGGING)) p->logstack(p);
LOG(p, "poolfree %p %p\n", p, v);
p->unlock(p);
}
ulong
poolmsize(Pool *p, void *v)
{
Alloc *b;
ulong dsize;
p->lock(p);
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(v == nil)
dsize = 0;
else {
b = D2B(p, v);
dsize = (b->size&~(p->quantum-1)) - sizeof(Bhdr) - sizeof(Btail);
assert(dsize >= getdsize(b));
blocksetdsize(p, b, dsize);
}
paranoia {
poolcheckl(p);
}
verbosity {
pooldumpl(p);
}
if(p->logstack && (p->flags & POOL_LOGGING)) p->logstack(p);
LOG(p, "poolmsize %p %p = %ld\n", p, v, dsize);
p->unlock(p);
return dsize;
}
static void
poolcheckarena(Pool *p, Arena *a)
{
Bhdr *b;
Bhdr *atail;
atail = A2TB(a);
for(b=a; b->magic != ARENATAIL_MAGIC && b<atail; b=B2NB(b))
blockcheck(p, b);
blockcheck(p, b);
if(b != atail)
p->panic(p, "found wrong tail");
}
static void
poolcheckl(Pool *p)
{
Arena *a;
for(a=p->arenalist; a; a=a->down)
poolcheckarena(p, a);
if(p->freeroot)
checktree(p->freeroot, 0, 1<<30);
}
void
poolcheck(Pool *p)
{
p->lock(p);
poolcheckl(p);
p->unlock(p);
}
void
poolblockcheck(Pool *p, void *v)
{
if(v == nil)
return;
p->lock(p);
blockcheck(p, D2B(p, v));
p->unlock(p);
}
static void
pooldumpl(Pool *p)
{
Arena *a;
p->print(p, "pool %p %s\n", p, p->name);
for(a=p->arenalist; a; a=a->down)
pooldumparena(p, a);
}
void
pooldump(Pool *p)
{
p->lock(p);
pooldumpl(p);
p->unlock(p);
}
static void
pooldumparena(Pool *p, Arena *a)
{
Bhdr *b;
for(b=a; b->magic != ARENATAIL_MAGIC; b=B2NB(b))
p->print(p, "(%p %.8lux %lud)", b, b->magic, b->size);
p->print(p, "\n");
}
static void
memmark(void *v, int sig, ulong size)
{
uchar *p, *ep;
ulong *lp, *elp;
lp = v;
elp = lp+size/4;
while(lp < elp)
*lp++ = (sig<<24) ^ ((uintptr)lp-(uintptr)v);
p = (uchar*)lp;
ep = (uchar*)v+size;
while(p<ep)
*p++ = sig;
}