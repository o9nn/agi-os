#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#define DATASEGM(p) { 0xFFFF, SEGG|SEGB|(0xF<<16)|SEGP|SEGPL(p)|SEGDATA|SEGW }
#define EXECSEGM(p) { 0xFFFF, SEGG|SEGD|(0xF<<16)|SEGP|SEGPL(p)|SEGEXEC|SEGR }
#define EXEC16SEGM(p) { 0xFFFF, SEGG|(0xF<<16)|SEGP|SEGPL(p)|SEGEXEC|SEGR }
#define TSSSEGM(b,p) { ((b)<<16)|sizeof(Tss),\
((b)&0xFF000000)|(((b)>>16)&0xFF)|SEGTSS|SEGPL(p)|SEGP }
void realmodeintrinst(void);
void _stop32pg(void);
Segdesc gdt[NGDT] =
{
[NULLSEG] { 0, 0},
[KDSEG] DATASEGM(0),
[KESEG] EXECSEGM(0),
[UDSEG] DATASEGM(3),
[UESEG] EXECSEGM(3),
[TSSSEG] TSSSEGM(0,0),
[KESEG16] EXEC16SEGM(0),
};
static int didmmuinit;
static void taskswitch(ulong, ulong);
static void memglobal(void);
#define vpt ((ulong*)VPT)
#define VPTX(va) (((ulong)(va))>>12)
#define vpd (vpt+VPTX(VPT))
void
mmuinit0(void)
{
memmove(m->gdt, gdt, sizeof gdt);
}
void
mmuinit(void)
{
ulong x, *p;
ushort ptr[3];
didmmuinit = 1;
if(0) print("vpt=%#.8ux vpd=%#p kmap=%#.8ux\n",
VPT, vpd, KMAP);
memglobal();
m->pdb[PDX(VPT)] = PADDR(m->pdb)|PTEWRITE|PTEVALID;
m->tss = malloc(sizeof(Tss));
if(m->tss == nil)
panic("mmuinit: no memory");
memset(m->tss, 0, sizeof(Tss));
m->tss->iomap = 0xDFFF<<16;
memmove(m->gdt, gdt, sizeof gdt);
x = (ulong)m->tss;
m->gdt[TSSSEG].d0 = (x<<16)|sizeof(Tss);
m->gdt[TSSSEG].d1 = (x&0xFF000000)|((x>>16)&0xFF)|SEGTSS|SEGPL(0)|SEGP;
ptr[0] = sizeof(gdt)-1;
x = (ulong)m->gdt;
ptr[1] = x & 0xFFFF;
ptr[2] = (x>>16) & 0xFFFF;
lgdt(ptr);
ptr[0] = sizeof(Segdesc)*256-1;
x = IDTADDR;
ptr[1] = x & 0xFFFF;
ptr[2] = (x>>16) & 0xFFFF;
lidt(ptr);
if(0) for(x = PGROUND((ulong)_stop32pg); x < (ulong)etext; x += BY2PG){
if (x == (ulong)realmodeintrinst & ~(BY2PG-1))
continue;
p = mmuwalk(m->pdb, x, 2, 0);
if(p == nil)
panic("mmuinit");
*p &= ~PTEWRITE;
}
taskswitch(PADDR(m->pdb), (ulong)m + MACHSIZE);
ltr(TSSSEL);
}
static void
memglobal(void)
{
int i, j;
ulong *pde, *pte;
if(m->machno != 0)
return;
if(!m->havepge)
return;
pde = m->pdb;
for(i=PDX(KZERO); i<1024; i++){
if(pde[i] & PTEVALID){
pde[i] |= PTEGLOBAL;
if(!(pde[i] & PTESIZE)){
pte = KADDR(pde[i]&~(BY2PG-1));
for(j=0; j<1024; j++)
if(pte[j] & PTEVALID)
pte[j] |= PTEGLOBAL;
}
}
}
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
flushpg(ulong va)
{
if(X86FAMILY(m->cpuidax) >= 4)
invlpg(va);
else
putcr3(getcr3());
}
static Page*
mmupdballoc(void)
{
int s;
Page *page;
ulong *pdb;
s = splhi();
m->pdballoc++;
if(m->pdbpool == 0){
spllo();
page = newpage(0, 0, 0);
page->va = (ulong)vpd;
splhi();
pdb = tmpmap(page);
memmove(pdb, m->pdb, BY2PG);
pdb[PDX(VPT)] = page->pa|PTEWRITE|PTEVALID;
tmpunmap(pdb);
}else{
page = m->pdbpool;
m->pdbpool = page->next;
m->pdbcnt--;
}
splx(s);
return page;
}
static void
mmupdbfree(Proc *proc, Page *p)
{
if(islo())
panic("mmupdbfree: islo");
m->pdbfree++;
if(m->pdbcnt >= 10){
p->next = proc->mmufree;
proc->mmufree = p;
}else{
p->next = m->pdbpool;
m->pdbpool = p;
m->pdbcnt++;
}
}
static void
mmuptefree(Proc* proc)
{
int s;
ulong *pdb;
Page **last, *page;
if(proc->mmupdb == nil || proc->mmuused == nil)
return;
s = splhi();
pdb = tmpmap(proc->mmupdb);
last = &proc->mmuused;
for(page = *last; page; page = page->next){
pdb[page->daddr] = 0;
last = &page->next;
}
tmpunmap(pdb);
splx(s);
*last = proc->mmufree;
proc->mmufree = proc->mmuused;
proc->mmuused = 0;
}
static void
taskswitch(ulong pdb, ulong stack)
{
Tss *tss;
tss = m->tss;
tss->ss0 = KDSEL;
tss->esp0 = stack;
tss->ss1 = KDSEL;
tss->esp1 = stack;
tss->ss2 = KDSEL;
tss->esp2 = stack;
putcr3(pdb);
}
void
mmuswitch(Proc* proc)
{
ulong *pdb;
if(proc->newtlb){
mmuptefree(proc);
proc->newtlb = 0;
}
if(proc->mmupdb){
pdb = tmpmap(proc->mmupdb);
pdb[PDX(MACHADDR)] = m->pdb[PDX(MACHADDR)];
tmpunmap(pdb);
taskswitch(proc->mmupdb->pa, (ulong)(proc->kstack+KSTACK));
}else
taskswitch(PADDR(m->pdb), (ulong)(proc->kstack+KSTACK));
}
void
mmurelease(Proc* proc)
{
Page *page, *next;
ulong *pdb;
if(islo())
panic("mmurelease: islo");
taskswitch(PADDR(m->pdb), (ulong)m + BY2PG);
if(proc->kmaptable){
if(proc->mmupdb == nil)
panic("mmurelease: no mmupdb");
if(--proc->kmaptable->ref)
panic("mmurelease: kmap ref %d", proc->kmaptable->ref);
if(proc->nkmap)
panic("mmurelease: nkmap %d", proc->nkmap);
pdb = tmpmap(proc->mmupdb);
if(PPN(pdb[PDX(KMAP)]) != proc->kmaptable->pa)
panic("mmurelease: bad kmap pde %#.8lux kmap %#.8lux",
pdb[PDX(KMAP)], proc->kmaptable->pa);
pdb[PDX(KMAP)] = 0;
tmpunmap(pdb);
pagechainhead(proc->kmaptable);
proc->kmaptable = 0;
}
if(proc->mmupdb){
mmuptefree(proc);
mmupdbfree(proc, proc->mmupdb);
proc->mmupdb = 0;
}
for(page = proc->mmufree; page; page = next){
next = page->next;
if(--page->ref)
panic("mmurelease: page->ref %d", page->ref);
pagechainhead(page);
}
if(proc->mmufree && palloc.r.p)
wakeup(&palloc.r);
proc->mmufree = 0;
}
static void
upallocpdb(void)
{
int s;
ulong *pdb;
Page *page;
if(up->mmupdb != nil)
return;
page = mmupdballoc();
s = splhi();
if(up->mmupdb != nil){
mmupdbfree(up, page);
splx(s);
return;
}
pdb = tmpmap(page);
pdb[PDX(MACHADDR)] = m->pdb[PDX(MACHADDR)];
tmpunmap(pdb);
up->mmupdb = page;
putcr3(up->mmupdb->pa);
splx(s);
}
void
putmmu(ulong va, ulong pa, Page*)
{
int old, s;
Page *page;
if(up->mmupdb == nil)
upallocpdb();
s = splhi();
if(!(vpd[PDX(va)]&PTEVALID)){
if(up->mmufree == 0){
spllo();
page = newpage(0, 0, 0);
splhi();
}
else{
page = up->mmufree;
up->mmufree = page->next;
}
vpd[PDX(va)] = PPN(page->pa)|PTEUSER|PTEWRITE|PTEVALID;
memset((void*)(VPT+PDX(va)*BY2PG), 0, BY2PG);
page->daddr = PDX(va);
page->next = up->mmuused;
up->mmuused = page;
}
old = vpt[VPTX(va)];
vpt[VPTX(va)] = pa|PTEUSER|PTEVALID;
if(old&PTEVALID)
flushpg(va);
if(getcr3() != up->mmupdb->pa)
print("bad cr3 %#.8lux %#.8lux\n", getcr3(), up->mmupdb->pa);
splx(s);
}
void
checkmmu(ulong va, ulong pa)
{
if(up->mmupdb == 0)
return;
if(!(vpd[PDX(va)]&PTEVALID) || !(vpt[VPTX(va)]&PTEVALID))
return;
if(PPN(vpt[VPTX(va)]) != pa)
print("%ld %s: va=%#08lux pa=%#08lux pte=%#08lux\n",
up->pid, up->text,
va, pa, vpt[VPTX(va)]);
}
ulong*
mmuwalk(ulong* pdb, ulong va, int level, int create)
{
ulong *table;
void *map;
table = &pdb[PDX(va)];
if(!(*table & PTEVALID) && create == 0)
return 0;
switch(level){
default:
return 0;
case 1:
return table;
case 2:
if(*table & PTESIZE)
panic("mmuwalk2: va %luX entry %luX", va, *table);
if(!(*table & PTEVALID)){
if(didmmuinit)
map = xspanalloc(BY2PG, BY2PG, 0);
else
map = rampage();
if(map == nil)
panic("mmuwalk xspanalloc failed");
*table = PADDR(map)|PTEWRITE|PTEVALID;
}
table = KADDR(PPN(*table));
return &table[PTX(va)];
}
}
static Lock vmaplock;
static int findhole(ulong *a, int n, int count);
static ulong vmapalloc(ulong size);
static void pdbunmap(ulong*, ulong, int);
void*
vmap(ulong pa, int size)
{
int osize;
ulong o, va;
osize = size;
o = pa & (BY2PG-1);
pa -= o;
size += o;
size = ROUND(size, BY2PG);
if(pa == 0){
print("vmap pa=0 pc=%#p\n", getcallerpc(&pa));
return nil;
}
ilock(&vmaplock);
if((va = vmapalloc(size)) == 0
|| pdbmap(MACHP(0)->pdb, pa|PTEUNCACHED|PTEWRITE, va, size) < 0){
iunlock(&vmaplock);
return 0;
}
iunlock(&vmaplock);
USED(osize);
return (void*)(va + o);
}
static int
findhole(ulong *a, int n, int count)
{
int have, i;
have = 0;
for(i=0; i<n; i++){
if(a[i] == 0)
have++;
else
have = 0;
if(have >= count)
return i+1 - have;
}
return -1;
}
static ulong
vmapalloc(ulong size)
{
int i, n, o;
ulong *vpdb;
int vpdbsize;
vpdb = &MACHP(0)->pdb[PDX(VMAP)];
vpdbsize = VMAPSIZE/(4*MB);
if(size >= 4*MB){
n = (size+4*MB-1) / (4*MB);
if((o = findhole(vpdb, vpdbsize, n)) != -1)
return VMAP + o*4*MB;
return 0;
}
n = (size+BY2PG-1) / BY2PG;
for(i=0; i<vpdbsize; i++)
if((vpdb[i]&PTEVALID) && !(vpdb[i]&PTESIZE))
if((o = findhole(KADDR(PPN(vpdb[i])), WD2PG, n)) != -1)
return VMAP + i*4*MB + o*BY2PG;
if((o = findhole(vpdb, vpdbsize, 1)) != -1)
return VMAP + o*4*MB;
return 0;
}
void
vunmap(void *v, int size)
{
int i;
ulong va, o;
Mach *nm;
Proc *p;
va = (ulong)v;
o = va&(BY2PG-1);
va -= o;
size += o;
size = ROUND(size, BY2PG);
if(size < 0 || va < VMAP || va+size > VMAP+VMAPSIZE)
panic("vunmap va=%#.8lux size=%#x pc=%#.8lux",
va, size, getcallerpc(&v));
pdbunmap(MACHP(0)->pdb, va, size);
if(!active.thunderbirdsarego){
if(MACHP(0)->pdb == 0)
panic("vunmap: nil m->pdb pc=%#p", getcallerpc(&v));
if(PADDR(MACHP(0)->pdb) == 0)
panic("vunmap: nil PADDR(m->pdb)");
putcr3(PADDR(MACHP(0)->pdb));
return;
}
for(i=0; i<conf.nproc; i++){
p = proctab(i);
if(p->state == Dead)
continue;
if(p != up)
p->newtlb = 1;
}
for(i=0; i<conf.nmach; i++){
nm = MACHP(i);
if(nm != m)
nm->flushmmu = 1;
}
flushmmu();
for(i=0; i<conf.nmach; i++){
nm = MACHP(i);
if(nm != m)
while((active.machs&(1<<nm->machno)) && nm->flushmmu)
;
}
}
int
pdbmap(ulong *pdb, ulong pa, ulong va, int size)
{
int pse;
ulong pgsz, *pte, *table;
ulong flag, off;
flag = pa&0xFFF;
pa &= ~0xFFF;
if((MACHP(0)->cpuiddx & 0x08) && (getcr4() & 0x10))
pse = 1;
else
pse = 0;
for(off=0; off<size; off+=pgsz){
table = &pdb[PDX(va+off)];
if((*table&PTEVALID) && (*table&PTESIZE))
panic("vmap: pdb pte valid and big page: "
"va=%#.8lux pa=%#.8lux pde=%#.8lux",
va+off, pa+off, *table);
if(pse && (pa+off)%(4*MB) == 0 && (va+off)%(4*MB) == 0 &&
(size-off) >= 4*MB){
*table = (pa+off)|flag|PTESIZE|PTEVALID;
pgsz = 4*MB;
}else{
pte = mmuwalk(pdb, va+off, 2, 1);
if(*pte&PTEVALID)
panic("vmap: va=%#.8lux pa=%#.8lux pte=%#.8lux",
va+off, pa+off, *pte);
*pte = (pa+off)|flag|PTEVALID;
pgsz = BY2PG;
}
}
return 0;
}
static void
pdbunmap(ulong *pdb, ulong va, int size)
{
ulong vae;
ulong *table;
vae = va+size;
while(va < vae){
table = &pdb[PDX(va)];
if(!(*table & PTEVALID)){
panic("vunmap: not mapped");
}
if(*table & PTESIZE){
*table = 0;
va = (va+4*MB-1) & ~(4*MB-1);
continue;
}
table = KADDR(PPN(*table));
if(!(table[PTX(va)] & PTEVALID))
panic("vunmap: not mapped");
table[PTX(va)] = 0;
va += BY2PG;
}
}
int
vmapsync(ulong va)
{
ulong entry, *table;
if(va < VMAP || va >= VMAP+VMAPSIZE)
return 0;
entry = MACHP(0)->pdb[PDX(va)];
if(!(entry&PTEVALID))
return 0;
if(!(entry&PTESIZE)){
table = KADDR(PPN(entry));
if(!(table[PTX(va)]&PTEVALID))
return 0;
}
vpd[PDX(va)] = entry;
return 1;
}
#define kpt (vpt+VPTX(KMAP))
#define NKPT (KMAPSIZE/BY2PG)
KMap*
kmap(Page *page)
{
int i, o, s;
if(up == nil)
panic("kmap: up=0 pc=%#.8lux", getcallerpc(&page));
if(up->mmupdb == nil)
upallocpdb();
if(up->nkmap < 0)
panic("kmap %lud %s: nkmap=%d", up->pid, up->text, up->nkmap);
s = splhi();
up->nkmap++;
if(!(vpd[PDX(KMAP)]&PTEVALID)){
if(KMAPSIZE > BY2XPG)
panic("bad kmapsize");
if(up->kmaptable != nil)
panic("kmaptable");
spllo();
up->kmaptable = newpage(0, 0, 0);
splhi();
vpd[PDX(KMAP)] = up->kmaptable->pa|PTEWRITE|PTEVALID;
flushpg((ulong)kpt);
memset(kpt, 0, BY2PG);
kpt[0] = page->pa|PTEWRITE|PTEVALID;
up->lastkmap = 0;
splx(s);
return (KMap*)KMAP;
}
if(up->kmaptable == nil)
panic("no kmaptable");
o = up->lastkmap+1;
for(i=0; i<NKPT; i++){
if(kpt[(i+o)%NKPT] == 0){
o = (i+o)%NKPT;
kpt[o] = page->pa|PTEWRITE|PTEVALID;
up->lastkmap = o;
splx(s);
return (KMap*)(KMAP+o*BY2PG);
}
}
panic("out of kmap");
return nil;
}
void
kunmap(KMap *k)
{
ulong va;
va = (ulong)k;
if(up->mmupdb == nil || !(vpd[PDX(KMAP)]&PTEVALID))
panic("kunmap: no kmaps");
if(va < KMAP || va >= KMAP+KMAPSIZE)
panic("kunmap: bad address %#.8lux pc=%#p", va, getcallerpc(&k));
if(!(vpt[VPTX(va)]&PTEVALID))
panic("kunmap: not mapped %#.8lux pc=%#p", va, getcallerpc(&k));
up->nkmap--;
if(up->nkmap < 0)
panic("kunmap %lud %s: nkmap=%d", up->pid, up->text, up->nkmap);
vpt[VPTX(va)] = 0;
flushpg(va);
}
#define fasttmp 1
void*
tmpmap(Page *p)
{
ulong i;
ulong *entry;
if(islo())
panic("tmpaddr: islo");
if(fasttmp && p->pa < -KZERO)
return KADDR(p->pa);
entry = &vpt[VPTX(TMPADDR)];
if(!(*entry&PTEVALID)){
for(i=KZERO; i<=CPU0MACH; i+=BY2PG)
print("%#p: *%#p=%#p (vpt=%#p index=%#p)\n", i, &vpt[VPTX(i)], vpt[VPTX(i)], vpt, VPTX(i));
panic("tmpmap: no entry");
}
if(PPN(*entry) != PPN(TMPADDR-KZERO))
panic("tmpmap: already mapped entry=%#.8lux", *entry);
*entry = p->pa|PTEWRITE|PTEVALID;
flushpg(TMPADDR);
return (void*)TMPADDR;
}
void
tmpunmap(void *v)
{
ulong *entry;
if(islo())
panic("tmpaddr: islo");
if(fasttmp && (ulong)v >= KZERO && v != (void*)TMPADDR)
return;
if(v != (void*)TMPADDR)
panic("tmpunmap: bad address");
entry = &vpt[VPTX(TMPADDR)];
if(!(*entry&PTEVALID) || PPN(*entry) == PPN(PADDR(TMPADDR)))
panic("tmpmap: not mapped entry=%#.8lux", *entry);
*entry = PPN(TMPADDR-KZERO)|PTEWRITE|PTEVALID;
flushpg(TMPADDR);
}
void*
kaddr(ulong pa)
{
if(pa > (ulong)-KZERO)
panic("kaddr: pa=%#.8lux > -KZERO pc=%#p", pa, getcallerpc(&pa));
return (void*)(pa | KZERO);
}
ulong
paddr(void *v)
{
ulong va;
va = (ulong)v;
if(va < KZERO)
panic("paddr: va=%#.8lux < KZERO pc=%#p", va, getcallerpc(&v));
return va & ~KSEGM;
}
void
countpagerefs(ulong *ref, int print)
{
USED(ref, print);
}
void
checkfault(ulong, ulong)
{
}
ulong
cankaddr(ulong pa)
{
if(pa >= -KZERO)
return 0;
return -KZERO - pa;
}