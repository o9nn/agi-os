#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "arm.h"
#define L1X(va)		FEXT((va), 20, 12)
#define L2X(va)		FEXT((va), 12, 8)
enum {
L1lo		= UZERO/MiB,
L1hi		= (USTKTOP+MiB-1)/MiB,
};
#define ISHOLE(pte)	((pte) == 0)
void
mmudump(PTE *l1)
{
int i, type, rngtype;
uintptr pa, startva, startpa;
uvlong va, endva;
PTE pte;
print("\n");
endva = startva = startpa = 0;
rngtype = 0;
for (va = i = 0; i < 4096; i++) {
pte = l1[i];
pa = pte & ~(MB - 1);
type = pte & (Fine|Section|Coarse);
if (ISHOLE(pte)) {
if (endva != 0) {
print("l1 maps va (%#lux-%#llux) -> pa %#lux type %#ux\n",
startva, endva-1, startpa, rngtype);
endva = 0;
}
} else {
if (endva == 0) {
startva = va;
startpa = pa;
rngtype = type;
}
endva = va + MB;
}
va += MB;
}
if (endva != 0)
print("l1 maps va (%#lux-%#llux) -> pa %#lux type %#ux\n",
startva, endva-1, startpa, rngtype);
}
static void
idmap(PTE *l1, ulong va)
{
va &= ~(MB-1);
l1[L1X(va)] = va | Dom0 | L1AP(Krw) | Section;
}
void
mmumap(uintptr virt, uintptr phys, int mbs)
{
uint off;
PTE *l1;
phys &= ~(MB-1);
virt &= ~(MB-1);
l1 = KADDR(ttbget());
for (off = 0; mbs-- > 0; off += MB)
l1[L1X(virt + off)] = (phys + off) | Dom0 | L1AP(Krw) | Section;
cacheuwbinv();
l2cacheuwbinv();
mmuinvalidate();
}
void
mmuidmap(uintptr phys, int mbs)
{
mmumap(phys, phys, mbs);
}
void
mmuinit(void)
{
uintptr pa;
PTE *l1, *l2;
pa = ttbget();
l1 = KADDR(pa);
l1[L1X(VIRTIO)] = PHYSIO|Dom0|L1AP(Krw)|Section;
idmap(l1, PHYSETHER);
idmap(l1, PHYSL4PROT);
idmap(l1, PHYSL3);
idmap(l1, PHYSSMS);
idmap(l1, PHYSDRC);
idmap(l1, PHYSGPMC);
pa -= MACHSIZE+2*1024;
l2 = KADDR(pa);
memset(l2, 0, 1024);
l2[L2X(HVECTORS)] = PHYSDRAM|L2AP(Krw)|Small;
l1[L1X(HVECTORS)] = pa|Dom0|Coarse;
coherence();
cacheuwbinv();
l2cacheuwbinv();
mmuinvalidate();
m->mmul1 = l1;
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
}
void*
mmuuncache(void* v, usize size)
{
int x;
PTE *pte;
uintptr va;
va = PTR2UINT(v);
assert(!(va & (1*MiB-1)) && size == 1*MiB);
x = L1X(va);
pte = &m->mmul1[x];
if((*pte & (Fine|Section|Coarse)) != Section)
return nil;
*pte &= ~(Cached|Buffered);
mmuinvalidateaddr(va);
cachedwbinvse(pte, 4);
return v;
}
uintptr
mmukmap(uintptr va, uintptr pa, usize size)
{
int x;
PTE *pte;
assert(!(va & (1*MiB-1)) && !(pa & (1*MiB-1)) && size == 1*MiB);
x = L1X(va);
pte = &m->mmul1[x];
if(*pte != Fault)
return 0;
*pte = pa|Dom0|L1AP(Krw)|Section;
mmuinvalidateaddr(va);
cachedwbinvse(pte, 4);
return va;
}
uintptr
mmukunmap(uintptr va, uintptr pa, usize size)
{
int x;
PTE *pte;
assert(!(va & (1*MiB-1)) && !(pa & (1*MiB-1)) && size == 1*MiB);
x = L1X(va);
pte = &m->mmul1[x];
if(*pte != (pa|Dom0|L1AP(Krw)|Section))
return 0;
*pte = Fault;
mmuinvalidateaddr(va);
cachedwbinvse(pte, 4);
return va;
}
uintptr
cankaddr(uintptr pa)
{
if(pa >= PHYSDRAM && pa < PHYSDRAM+memsize)
return PHYSDRAM+memsize - pa;
return 0;
}
void*
vmap(uintptr pa, usize size)
{
uintptr pae, va;
usize o, osize;
if(pa+size < 4*MiB)
return UINT2PTR(kseg0|pa);
osize = size;
o = pa & (BY2PG-1);
pa -= o;
size += o;
size = ROUNDUP(size, BY2PG);
va = kseg0|pa;
pae = mmukmap(va, pa, size);
if(pae == 0 || pae-size != pa)
panic("vmap(%#p, %ld) called from %#p: mmukmap fails %#p",
pa+o, osize, getcallerpc(&pa), pae);
return UINT2PTR(va+o);
}
void
vunmap(void* v, usize size)
{
USED(v, size);
}