#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#define MEMDEBUG	0
enum {
MemUPA		= 0,
MemRAM		= 1,
MemUMB		= 2,
MemReserved	= 3,
NMemType	= 4,
KB		= 1024,
};
typedef struct Map Map;
struct Map {
ulong	size;
ulong	addr;
};
typedef struct RMap RMap;
struct RMap {
char*	name;
Map*	map;
Map*	mapend;
Lock;
};
static Map mapupa[16];
static RMap rmapupa = {
"unallocated unbacked physical addresses",
mapupa,
&mapupa[nelem(mapupa)-1],
};
static Map mapram[16];
static RMap rmapram = {
"physical memory",
mapram,
&mapram[nelem(mapram)-1],
};
static Map mapumb[64];
static RMap rmapumb = {
"upper memory block",
mapumb,
&mapumb[nelem(mapumb)-1],
};
static Map mapumbrw[16];
static RMap rmapumbrw = {
"UMB device memory",
mapumbrw,
&mapumbrw[nelem(mapumbrw)-1],
};
static void map(ulong base, ulong len, int type);
void
mapprint(RMap *rmap)
{
Map *mp;
print("%s\n", rmap->name);
for(mp = rmap->map; mp->size; mp++)
print("\t%8.8luX %8.8luX (%lud)\n", mp->addr, mp->addr+mp->size, mp->size);
}
void
memdebug(void)
{
ulong maxpa, maxpa1, maxpa2;
maxpa = (nvramread(0x18)<<8)|nvramread(0x17);
maxpa1 = (nvramread(0x31)<<8)|nvramread(0x30);
maxpa2 = (nvramread(0x16)<<8)|nvramread(0x15);
print("maxpa = %luX -> %luX, maxpa1 = %luX maxpa2 = %luX\n",
maxpa, MB+maxpa*KB, maxpa1, maxpa2);
mapprint(&rmapram);
mapprint(&rmapumb);
mapprint(&rmapumbrw);
mapprint(&rmapupa);
}
void
mapfree(RMap* rmap, ulong addr, ulong size)
{
Map *mp;
ulong t;
if(size <= 0)
return;
lock(rmap);
for(mp = rmap->map; mp->addr <= addr && mp->size; mp++)
;
if(mp > rmap->map && (mp-1)->addr+(mp-1)->size == addr){
(mp-1)->size += size;
if(addr+size == mp->addr){
(mp-1)->size += mp->size;
while(mp->size){
mp++;
(mp-1)->addr = mp->addr;
(mp-1)->size = mp->size;
}
}
}
else{
if(addr+size == mp->addr && mp->size){
mp->addr -= size;
mp->size += size;
}
else do{
if(mp >= rmap->mapend){
print("mapfree: %s: losing %#luX, %ld\n",
rmap->name, addr, size);
break;
}
t = mp->addr;
mp->addr = addr;
addr = t;
t = mp->size;
mp->size = size;
mp++;
}while(size = t);
}
unlock(rmap);
}
ulong
mapalloc(RMap* rmap, ulong addr, int size, int align)
{
Map *mp;
ulong maddr, oaddr;
lock(rmap);
for(mp = rmap->map; mp->size; mp++){
maddr = mp->addr;
if(addr){
if(maddr > addr)
break;
if(mp->size < addr - maddr)
continue;
if(addr - maddr > mp->size - size)
break;
maddr = addr;
}
if(align > 0)
maddr = ((maddr+align-1)/align)*align;
if(mp->addr+mp->size-maddr < size)
continue;
oaddr = mp->addr;
mp->addr = maddr+size;
mp->size -= maddr-oaddr+size;
if(mp->size == 0){
do{
mp++;
(mp-1)->addr = mp->addr;
}while((mp-1)->size = mp->size);
}
unlock(rmap);
if(oaddr != maddr)
mapfree(rmap, oaddr, maddr-oaddr);
return maddr;
}
unlock(rmap);
return 0;
}
void*
rampage(void)
{
ulong m;
m = mapalloc(&rmapram, 0, BY2PG, BY2PG);
if(m == 0)
return nil;
return KADDR(m);
}
static void
umbexclude(void)
{
int size;
ulong addr;
char *op, *p, *rptr;
if((p = getconf("umbexclude")) == nil)
return;
while(p && *p != '\0' && *p != '\n'){
op = p;
addr = strtoul(p, &rptr, 0);
if(rptr == nil || rptr == p || *rptr != '-'){
print("umbexclude: invalid argument <%s>\n", op);
break;
}
p = rptr+1;
size = strtoul(p, &rptr, 0) - addr + 1;
if(size <= 0){
print("umbexclude: bad range <%s>\n", op);
break;
}
if(rptr != nil && *rptr == ',')
*rptr++ = '\0';
p = rptr;
mapalloc(&rmapumb, addr, size, 0);
}
}
static void
umbscan(void)
{
uchar o[2], *p;
p = KADDR(0xD0000);
while(p < (uchar*)KADDR(0xE0000)){
if(p[0] == 0x55 && p[1] == 0xAA){
p += p[2]*512;
continue;
}
o[0] = p[0];
p[0] = 0xCC;
o[1] = p[2*KB-1];
p[2*KB-1] = 0xCC;
if(p[0] == 0xCC && p[2*KB-1] == 0xCC){
p[0] = o[0];
p[2*KB-1] = o[1];
mapfree(&rmapumbrw, PADDR(p), 2*KB);
}
else if(p[0] == 0xFF && p[1] == 0xFF)
mapfree(&rmapumb, PADDR(p), 2*KB);
p += 2*KB;
}
p = KADDR(0xE0000);
if(p[0] != 0x55 || p[1] != 0xAA){
p[0] = 0xCC;
p[64*KB-1] = 0xCC;
if(p[0] != 0xCC && p[64*KB-1] != 0xCC)
mapfree(&rmapumb, PADDR(p), 64*KB);
}
umbexclude();
}
enum {
Pteflags = (1<<12) - 1,
};
void
dumppdb(ulong *pdb)
{
ulong *pp;
pdb = (ulong *)((uintptr)pdb & ~Pteflags);
iprint("pdb at phys %#8.8p:\n", PADDR(pdb));
for (pp = pdb; pp < pdb + 1024; pp++)
if (*pp)
iprint("pdb[%3ld]: %#8.8lux\n", pp - pdb, *pp);
}
void
dumppte(ulong *pdb, int sub, int first)
{
ulong *pp, *pte;
pte = KADDR(pdb[sub]);
pte = (ulong *)((uintptr)pte & ~Pteflags);
if (PADDR(pte) == 0) {
iprint("pdb[%d] unmapped\n", sub);
return;
}
iprint("pdb[%d] pte at phys %#8.8p:\n", sub, PADDR(pte));
for (pp = pte; pp < pte + first; pp++)
if (*pp)
iprint("pte[%3ld]: %#8.8lux\n", pp - pte, *pp);
iprint("...\n");
}
uintptr
mapping(uintptr va)
{
ulong *pte;
pte = KADDR(m->pdb[PDX(va)] & ~Pteflags);
return pte[PTX(va)] & ~Pteflags;
}
static void
lowraminit(void)
{
mapalloc(&rmapram, BY2PG, Mallocbase - BY2PG, 0);
map(Mallocbase, MemMax - Mallocbase, MemRAM);
map(MemMax, KZERO-MemMax, MemUPA);
mmuflushtlb(PADDR(m->pdb));
}
static void
map(ulong base, ulong len, int type)
{
ulong n, flags, maxkpa;
if(base < MemMin && len > MemMin-base){
n = MemMin - base;
map(base, n, type);
map(MemMin, len-n, type);
return;
}
switch(type){
case MemRAM:
mapfree(&rmapram, base, len);
flags = PTEWRITE|PTEVALID;
break;
case MemUMB:
mapfree(&rmapumb, base, len);
flags = PTEWRITE|PTEUNCACHED|PTEVALID;
break;
case MemUPA:
mapfree(&rmapupa, base, len);
flags = 0;
break;
default:
case MemReserved:
flags = 0;
break;
}
if(flags){
maxkpa = -KZERO;
if(base >= maxkpa)
return;
if(len > maxkpa-base)
len = maxkpa - base;
pdbmap(m->pdb, base|flags, base+KZERO, len);
}
}
void
meminit(void)
{
int i, kzsub;
Map *mp;
Confmem *cm;
ulong pa, *pte;
ulong lost, physpte;
pte = m->pdb + BY2PG/BY2WD;
kzsub = ((uintptr)KZERO >> (2*PGSHIFT - 4)) / sizeof(ulong);
physpte = (uintptr)PADDR(pte);
for (i = 0; i < LOWPTEPAGES; i++)
m->pdb[kzsub + i] = m->pdb[i] =
PTEVALID | PTEKERNEL | PTEWRITE | (physpte + i * BY2PG);
for(pa = 0xA0000; pa < 0xC0000; pa += BY2PG){
pte = mmuwalk(m->pdb, (ulong)KADDR(pa), 2, 0);
*pte |= PTEWT;
}
for(pa = 0xC0000; pa < 0x100000; pa += BY2PG){
pte = mmuwalk(m->pdb, (ulong)KADDR(pa), 2, 0);
*pte |= PTEUNCACHED;
}
mmuflushtlb(PADDR(m->pdb));
umbscan();
lowraminit();
for(i=0; i<nelem(mapram) && i<nelem(conf.mem); i++){
mp = &rmapram.map[i];
cm = &conf.mem[i];
cm->base = mp->addr;
cm->npage = mp->size/BY2PG;
if (i == 0 && cm->npage == 0)
panic("meminit: no memory in conf.mem");
}
lost = 0;
for(; i<nelem(mapram); i++)
lost += rmapram.map[i].size;
if(lost)
print("meminit - lost %lud bytes\n", lost);
if(MEMDEBUG)
memdebug();
}
ulong
umbmalloc(ulong addr, int size, int align)
{
ulong a;
if(a = mapalloc(&rmapumb, addr, size, align))
return (ulong)KADDR(a);
return 0;
}
void
umbfree(ulong addr, int size)
{
mapfree(&rmapumb, PADDR(addr), size);
}
ulong
umbrwmalloc(ulong addr, int size, int align)
{
ulong a;
uchar o[2], *p;
if(a = mapalloc(&rmapumbrw, addr, size, align))
return(ulong)KADDR(a);
if((a = umbmalloc(addr, size, align)) == 0)
return 0;
p = (uchar*)a;
o[0] = p[0];
p[0] = 0xCC;
o[1] = p[size-1];
p[size-1] = 0xCC;
if(p[0] == 0xCC && p[size-1] == 0xCC){
p[0] = o[0];
p[size-1] = o[1];
return a;
}
umbfree(a, size);
return 0;
}
void
umbrwfree(ulong addr, int size)
{
mapfree(&rmapumbrw, PADDR(addr), size);
}
ulong
upaalloc(int size, int align)
{
ulong a;
a = mapalloc(&rmapupa, 0, size, align);
if(a == 0){
print("out of physical address space allocating %d\n", size);
mapprint(&rmapupa);
}
return a;
}
void
upafree(ulong pa, int size)
{
mapfree(&rmapupa, pa, size);
}
void
upareserve(ulong pa, int size)
{
ulong a;
a = mapalloc(&rmapupa, pa, size, 0);
if(a != pa){
if(a != 0)
mapfree(&rmapupa, a, size);
}
}
void
memorysummary(void)
{
memdebug();
}