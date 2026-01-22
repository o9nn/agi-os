#include "sh.h"
# if DEBUG_ALLOC
void acheck ARGS((Area *ap));
# define ACHECK(ap) acheck(ap)
# else
# define ACHECK(ap)
# endif
#define ICELLS 200
typedef union Cell Cell;
typedef struct Block Block;
#define NOBJECT_FIELDS 2
union Cell {
size_t size;
Cell *next;
Block *block;
struct {int _;} junk;
double djunk;
};
struct Block {
Block *next;
Block *prev;
Cell *freelist;
Cell *last;
Cell cell [1];
};
static Block aempty = {&aempty, &aempty, aempty.cell, aempty.cell};
static void ablockfree ARGS((Block *bp, Area *ap));
static void *asplit ARGS((Area *ap, Block *bp, Cell *fp, Cell *fpp, int cells));
Area *
ainit(ap)
register Area *ap;
{
ap->freelist = &aempty;
ACHECK(ap);
return ap;
}
void
afreeall(ap)
register Area *ap;
{
register Block *bp;
register Block *tmp;
ACHECK(ap);
bp = ap->freelist;
if (bp != NULL && bp != &aempty) {
do {
tmp = bp;
bp = bp->next;
free((void*)tmp);
} while (bp != ap->freelist);
ap->freelist = &aempty;
}
ACHECK(ap);
}
void *
alloc(size, ap)
size_t size;
register Area *ap;
{
int cells, acells;
Block *bp = 0;
Cell *fp = 0, *fpp = 0;
ACHECK(ap);
if (size <= 0)
aerror(ap, "allocate bad size");
cells = (unsigned)(size + sizeof(Cell) - 1) / sizeof(Cell);
acells = cells + NOBJECT_FIELDS;
if (cells <= ICELLS) {
for (bp = ap->freelist; ; bp = bp->next) {
for (fpp = NULL, fp = bp->freelist;
fp != bp->last; fpp = fp, fp = fp->next)
{
if ((fp-1)->size >= cells)
goto Found;
}
if (bp->next == ap->freelist) {
bp = 0;
break;
}
}
acells += ICELLS;
}
if (bp == 0) {
bp = (Block*) malloc(offsetof(Block, cell[acells]));
if (bp == NULL)
aerror(ap, "cannot allocate");
if (ap->freelist == &aempty) {
ap->freelist = bp->next = bp->prev = bp;
} else {
bp->next = ap->freelist->next;
ap->freelist->next->prev = bp;
ap->freelist->next = bp;
bp->prev = ap->freelist;
}
bp->last = bp->cell + acells;
fp = bp->freelist = bp->cell + NOBJECT_FIELDS;
(fp-1)->size = acells - NOBJECT_FIELDS;
(fp-2)->block = bp;
fp->next = bp->last;
fpp = NULL;
}
Found:
return asplit(ap, bp, fp, fpp, cells);
}
static void *
asplit(ap, bp, fp, fpp, cells)
Area *ap;
Block *bp;
Cell *fp;
Cell *fpp;
int cells;
{
Cell *dp = fp;
int split = (fp-1)->size - cells;
ACHECK(ap);
if (split < 0)
aerror(ap, "allocated object too small");
if (split <= NOBJECT_FIELDS) {
fp = fp->next;
} else {
Cell *next = fp->next;
ap->freelist = bp;
(fp-1)->size = cells;
fp += cells + NOBJECT_FIELDS;
(fp-1)->size = split - NOBJECT_FIELDS;
(fp-2)->block = bp;
fp->next = next;
}
if (fpp == NULL)
bp->freelist = fp;
else
fpp->next = fp;
ACHECK(ap);
return (void*) dp;
}
void *
aresize(ptr, size, ap)
register void *ptr;
size_t size;
Area *ap;
{
int cells;
Cell *dp = (Cell*) ptr;
int oldcells = dp ? (dp-1)->size : 0;
ACHECK(ap);
if (size <= 0)
aerror(ap, "allocate bad size");
cells = (unsigned)(size - 1) / sizeof(Cell) + 1;
if (oldcells > ICELLS && cells > ICELLS) {
Block *bp = (dp-2)->block;
Block *nbp;
Block *next = bp->next, *prev = bp->prev;
if (bp->freelist != bp->last)
aerror(ap, "allocation resizing free pointer");
nbp = realloc((void *) bp,
offsetof(Block, cell[cells + NOBJECT_FIELDS]));
if (!nbp) {
if (next == bp)
ap->freelist = &aempty;
else {
next->prev = prev;
prev->next = next;
if (ap->freelist == bp)
ap->freelist = next;
}
aerror(ap, "cannot re-allocate");
}
if (nbp != bp) {
if (next == bp)
nbp->next = nbp->prev = nbp;
else {
next->prev = nbp;
prev->next = nbp;
}
if (ap->freelist == bp)
ap->freelist = nbp;
dp = nbp->cell + NOBJECT_FIELDS;
(dp-2)->block = nbp;
}
(dp-1)->size = cells;
nbp->last = nbp->cell + cells + NOBJECT_FIELDS;
nbp->freelist = nbp->last;
ACHECK(ap);
return (void*) dp;
}
if (dp && cells > oldcells && cells <= ICELLS) {
Cell *fp, *fpp;
Block *bp = (dp-2)->block;
int need = cells - oldcells - NOBJECT_FIELDS;
for (fpp = NULL, fp = bp->freelist;
fp != bp->last
&& dp + oldcells + NOBJECT_FIELDS <= fp
; fpp = fp, fp = fp->next)
{
if (dp + oldcells + NOBJECT_FIELDS == fp
&& (fp-1)->size >= need)
{
Cell *np = asplit(ap, bp, fp, fpp, need);
(dp-1)->size += (np-1)->size + NOBJECT_FIELDS;
ACHECK(ap);
return ptr;
}
}
}
if (dp && cells <= oldcells && oldcells <= ICELLS) {
int split;
split = oldcells - cells;
if (split <= NOBJECT_FIELDS)
;
else {
Block *bp = (dp-2)->block;
(dp-1)->size = cells;
dp += cells + NOBJECT_FIELDS;
(dp-1)->size = split - NOBJECT_FIELDS;
(dp-2)->block = bp;
afree((void*)dp, ap);
}
return ptr;
}
ptr = alloc(size, ap);
if (dp != NULL) {
size_t s = (dp-1)->size * sizeof(Cell);
if (s > size)
s = size;
memcpy(ptr, dp, s);
afree((void *) dp, ap);
}
return ptr;
}
void
afree(ptr, ap)
void *ptr;
register Area *ap;
{
register Block *bp;
register Cell *fp, *fpp;
register Cell *dp = (Cell*)ptr;
ACHECK(ap);
if (ptr == 0)
aerror(ap, "freeing null pointer");
bp = (dp-2)->block;
if ((dp-1)->size > ICELLS) {
ablockfree(bp, ap);
ACHECK(ap);
return;
}
if (dp < &bp->cell[NOBJECT_FIELDS] || dp >= bp->last)
aerror(ap, "freeing memory outside of block (corrupted?)");
for (fpp = NULL, fp = bp->freelist; fp < dp; fpp = fp, fp = fp->next)
;
if (fp == dp)
aerror(ap, "freeing free object");
if (dp + (dp-1)->size == fp-NOBJECT_FIELDS) {
(dp-1)->size += (fp-1)->size + NOBJECT_FIELDS;
dp->next = fp->next;
} else
dp->next = fp;
if (fpp == NULL)
bp->freelist = dp;
else if (fpp + (fpp-1)->size == dp-NOBJECT_FIELDS) {
(fpp-1)->size += (dp-1)->size + NOBJECT_FIELDS;
fpp->next = dp->next;
} else
fpp->next = dp;
if (bp->next != bp && bp->freelist == bp->cell + NOBJECT_FIELDS
&& bp->freelist + (bp->freelist-1)->size == bp->last
)
ablockfree(bp, ap);
ACHECK(ap);
}
static void
ablockfree(bp, ap)
Block *bp;
Area *ap;
{
if (bp->next == bp)
ap->freelist = &aempty;
else {
bp->next->prev = bp->prev;
bp->prev->next = bp->next;
if (ap->freelist == bp)
ap->freelist = bp->next;
}
free((void*) bp);
}
# if DEBUG_ALLOC
void
acheck(ap)
Area *ap;
{
Block *bp, *bpp;
Cell *dp, *dptmp, *fp;
int ok = 1;
int isfree;
static int disabled;
if (disabled)
return;
if (!ap) {
disabled = 1;
aerror(ap, "acheck: null area pointer");
}
bp = ap->freelist;
if (!bp) {
disabled = 1;
aerror(ap, "acheck: null area freelist");
}
if (bp == &aempty)
return;
bpp = ap->freelist->prev;
while (1) {
if (bp->prev != bpp) {
shellf("acheck: bp->prev != previous\n");
ok = 0;
}
fp = bp->freelist;
for (dp = &bp->cell[NOBJECT_FIELDS]; dp != bp->last; ) {
if ((dp-2)->block != bp) {
shellf("acheck: fragment's block is wrong\n");
ok = 0;
}
isfree = dp == fp;
if ((dp-1)->size == 0 && isfree) {
shellf("acheck: 0 size frag\n");
ok = 0;
}
if ((dp-1)->size > ICELLS
&& !isfree
&& (dp != &bp->cell[NOBJECT_FIELDS]
|| dp + (dp-1)->size != bp->last))
{
shellf("acheck: big cell doesn't make up whole block\n");
ok = 0;
}
if (isfree) {
if (dp->next <= dp) {
shellf("acheck: free fragment's next <= self\n");
ok = 0;
}
if (dp->next > bp->last) {
shellf("acheck: free fragment's next > last\n");
ok = 0;
}
fp = dp->next;
}
dptmp = dp + (dp-1)->size;
if (dptmp > bp->last) {
shellf("acheck: next frag out of range\n");
ok = 0;
break;
} else if (dptmp != bp->last) {
dptmp += NOBJECT_FIELDS;
if (dptmp > bp->last) {
shellf("acheck: next frag just out of range\n");
ok = 0;
break;
}
}
if (isfree && dptmp == fp && dptmp != bp->last) {
shellf("acheck: adjacent free frags\n");
ok = 0;
} else if (dptmp > fp) {
shellf("acheck: free frag list messed up\n");
ok = 0;
}
dp = dptmp;
}
bpp = bp;
bp = bp->next;
if (bp == ap->freelist)
break;
}
if (!ok) {
disabled = 1;
aerror(ap, "acheck failed");
}
}
void
aprint(ap, ptr, size)
register Area *ap;
void *ptr;
size_t size;
{
Block *bp;
if (!ap)
shellf("aprint: null area pointer\n");
else if (!(bp = ap->freelist))
shellf("aprint: null area freelist\n");
else if (bp == &aempty)
shellf("aprint: area is empty\n");
else {
int i;
Cell *dp, *fp;
Block *bpp;
bpp = ap->freelist->prev;
for (i = 0; ; i++) {
if (ptr) {
void *eptr = (void *) (((char *) ptr) + size);
if (!((ptr >= (void *) bp
&& ptr <= (void *) bp->last)
|| (eptr >= (void *) bp
&& eptr <= (void *) bp->last)))
continue;
shellf("aprint: overlap of 0x%p .. 0x%p\n",
ptr, eptr);
}
if (bp->prev != bpp || bp->next->prev != bp)
shellf(
"aprint: BAD prev pointer: bp %p, bp->prev %p, bp->next %p, bpp=%p\n",
bp, bp->prev, bp->next, bpp);
shellf("aprint: block %2d (p=%p,%p,n=%p): 0x%p .. 0x%p (%ld)\n", i,
bp->prev, bp, bp->next,
bp->cell, bp->last,
(long) ((char *) bp->last - (char *) bp->cell));
fp = bp->freelist;
if (bp->last <= bp->cell + NOBJECT_FIELDS)
shellf(
"aprint: BAD bp->last too small: %p <= %p\n",
bp->last, bp->cell + NOBJECT_FIELDS);
if (bp->freelist < bp->cell + NOBJECT_FIELDS
|| bp->freelist > bp->last)
shellf(
"aprint: BAD bp->freelist %p out of range: %p .. %p\n",
bp->freelist,
bp->cell + NOBJECT_FIELDS, bp->last);
for (dp = bp->cell; dp != bp->last ; ) {
dp += NOBJECT_FIELDS;
shellf(
"aprint:   0x%p .. 0x%p (%ld) %s\n",
(dp-NOBJECT_FIELDS),
(dp-NOBJECT_FIELDS) + (dp-1)->size
+ NOBJECT_FIELDS,
(long) ((dp-1)->size + NOBJECT_FIELDS)
* sizeof(Cell),
dp == fp ? "free" : "allocated");
if ((dp-2)->block != bp)
shellf(
"aprint: BAD dp->block %p != bp %p\n",
(dp-2)->block, bp);
if (dp > bp->last)
shellf(
"aprint: BAD dp gone past block: %p > %p\n",
dp, bp->last);
if (dp > fp)
shellf(
"aprint: BAD dp gone past free: %p > %p\n",
dp, fp);
if (dp == fp) {
fp = fp->next;
if (fp < dp || fp > bp->last)
shellf(
"aprint: BAD free object %p out of range: %p .. %p\n",
fp,
dp, bp->last);
}
dp += (dp-1)->size;
}
bpp = bp;
bp = bp->next;
if (bp == ap->freelist)
break;
}
}
}
#endif