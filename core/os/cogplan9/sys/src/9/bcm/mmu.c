#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "arm.h"
#define L1X(va) FEXT((va), 20, 12)
#define L2X(va) FEXT((va), 12, 8)
enum {
L1lo = UZERO/MiB,
L1hi = (USTKTOP+MiB-1)/MiB,
};
void
mmuinit(void)
{
PTE *l1, *l2;
uintptr pa, va;
l1 = (PTE*)PADDR(L1);
l2 = (PTE*)PADDR(L2);
va = KZERO;
for(pa = PHYSDRAM; pa < PHYSDRAM+DRAMSIZE; pa += MiB){
l1[L1X(va)] = pa|Dom0|L1AP(Krw)|Section|Cached|Buffered;
va += MiB;
}
l1[L1X(PHYSDRAM)] = PHYSDRAM|Dom0|L1AP(Krw)|Section|Cached|Buffered;
va = VIRTIO;
for(pa = PHYSIO; pa < PHYSIO+IOSIZE; pa += MiB){
l1[L1X(va)] = pa|Dom0|L1AP(Krw)|Section;
va += MiB;
}
va = HVECTORS;
l1[L1X(va)] = (uintptr)l2|Dom0|Coarse;
l2[L2X(va)] = PHYSDRAM|L2AP(Krw)|Small;
}
void
mmuinit1(void)
{
PTE *l1;
l1 = (PTE*)L1;
m->mmul1 = l1;
l1[L1X(PHYSDRAM)] = 0;
cachedwbse(&l1[L1X(PHYSDRAM)], sizeof(PTE));
mmuinvalidate();
}
static void
mmul2empty(Proc* proc, int clear)
{
PTE *l1;
Page **l2, *page;
l1 = m->mmul1;
l2 = &proc->mmul2;
for(page = *l2; page != nil; page = page->next){
if(clear)
memset(UINT2PTR(page->va), 0, BY2PG);
l1[page->daddr] = Fault;
l2 = &page->next;
}
*l2 = proc->mmul2cache;
proc->mmul2cache = proc->mmul2;
proc->mmul2 = nil;
}
static void
mmul1empty(void)
{
#ifdef notdef
PTE *l1;
if(m->mmul1lo > L1lo){
if(m->mmul1lo == 1)
m->mmul1[L1lo] = Fault;
else
memset(&m->mmul1[L1lo], 0, m->mmul1lo*sizeof(PTE));
m->mmul1lo = L1lo;
}
if(m->mmul1hi < L1hi){
l1 = &m->mmul1[m->mmul1hi];
if((L1hi - m->mmul1hi) == 1)
*l1 = Fault;
else
memset(l1, 0, (L1hi - m->mmul1hi)*sizeof(PTE));
m->mmul1hi = L1hi;
}
#else
memset(&m->mmul1[L1lo], 0, (L1hi - L1lo)*sizeof(PTE));
#endif
}
void
mmuswitch(Proc* proc)
{
int x;
PTE *l1;
Page *page;
if(m->mmupid == proc->pid && !proc->newtlb)
return;
m->mmupid = proc->pid;
cacheuwbinv();
if(proc->newtlb){
mmul2empty(proc, 1);
proc->newtlb = 0;
}
mmul1empty();
l1 = m->mmul1;
for(page = proc->mmul2; page != nil; page = page->next){
x = page->daddr;
l1[x] = PPN(page->pa)|Dom0|Coarse;
if(x+1 - m->mmul1lo < m->mmul1hi - x)
m->mmul1lo = x+1;
else
m->mmul1hi = x;
}
cachedwbse(&l1[L1X(UZERO)], (L1hi - L1lo)*sizeof(PTE));
mmuinvalidate();
}
void
flushmmu(void)
{
int s;
s = splhi();
up->newtlb = 1;
mmuswitch(up);
splx(s);
}
void
mmurelease(Proc* proc)
{
Page *page, *next;
cacheuwbinv();
mmul2empty(proc, 0);
for(page = proc->mmul2cache; page != nil; page = next){
next = page->next;
if(--page->ref)
panic("mmurelease: page->ref %d", page->ref);
pagechainhead(page);
}
if(proc->mmul2cache && palloc.r.p)
wakeup(&palloc.r);
proc->mmul2cache = nil;
mmul1empty();
cachedwbse(&m->mmul1[L1X(UZERO)], (L1hi - L1lo)*sizeof(PTE));
mmuinvalidate();
}
void
putmmu(uintptr va, uintptr pa, Page* page)
{
int x;
Page *pg;
PTE *l1, *pte;
x = L1X(va);
l1 = &m->mmul1[x];
if(*l1 == Fault){
if(up->mmul2cache == nil){
pg = newpage(1, 0, 0);
pg->va = VA(kmap(pg));
}
else{
pg = up->mmul2cache;
up->mmul2cache = pg->next;
memset(UINT2PTR(pg->va), 0, BY2PG);
}
pg->daddr = x;
pg->next = up->mmul2;
up->mmul2 = pg;
cachedwbse((void *)pg->va, BY2PG);
*l1 = PPN(pg->pa)|Dom0|Coarse;
cachedwbse(l1, sizeof *l1);
if(x >= m->mmul1lo && x < m->mmul1hi){
if(x+1 - m->mmul1lo < m->mmul1hi - x)
m->mmul1lo = x+1;
else
m->mmul1hi = x;
}
}
pte = UINT2PTR(KADDR(PPN(*l1)));
x = Small;
if(!(pa & PTEUNCACHED))
x |= Cached|Buffered;
if(pa & PTEWRITE)
x |= L2AP(Urw);
else
x |= L2AP(Uro);
pte[L2X(va)] = PPN(pa)|x;
cachedwbse(&pte[L2X(va)], sizeof pte[0]);
mmuinvalidateaddr(PPN(va));
cachedwbinv();
if(page->cachectl[0] == PG_TXTFLUSH){
cacheiinv();
page->cachectl[0] = PG_NOFLUSH;
}
checkmmu(va, PPN(pa));
}
uintptr
cankaddr(uintptr pa)
{
if(pa < PHYSDRAM + memsize)
return PHYSDRAM + memsize - pa;
return 0;
}
uintptr
mmukmap(uintptr va, uintptr pa, usize size)
{
int o;
usize n;
PTE *pte, *pte0;
assert((va & (MiB-1)) == 0);
o = pa & (MiB-1);
pa -= o;
size += o;
pte = pte0 = &m->mmul1[L1X(va)];
for(n = 0; n < size; n += MiB)
if(*pte++ != Fault)
return 0;
pte = pte0;
for(n = 0; n < size; n += MiB){
*pte++ = (pa+n)|Dom0|L1AP(Krw)|Section;
mmuinvalidateaddr(va+n);
}
cachedwbse(pte0, pte - pte0);
return va + o;
}
void
checkmmu(uintptr va, uintptr pa)
{
USED(va);
USED(pa);
}