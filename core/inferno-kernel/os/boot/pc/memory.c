#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#define MEMDEBUG 0
#define PDX(va) ((((ulong)(va))>>22) & 0x03FF)
#define PTX(va) ((((ulong)(va))>>12) & 0x03FF)
enum {
MemUPA = 0,
MemRAM = 1,
MemUMB = 2,
NMemType = 3,
KB = 1024,
MemMinMB = 4,
MemMaxMB = 768,
NMemBase = 10,
};
typedef struct {
int size;
ulong addr;
} Map;
typedef struct {
char* name;
Map* map;
Map* mapend;
Lock;
} RMap;
static Map mapupa[8];
static RMap rmapupa = {
"unallocated unbacked physical memory",
mapupa,
&mapupa[7],
};
static Map xmapupa[8];
static RMap xrmapupa = {
"unbacked physical memory",
xmapupa,
&xmapupa[7],
};
static Map mapram[8];
static RMap rmapram = {
"physical memory",
mapram,
&mapram[7],
};
static Map mapumb[64];
static RMap rmapumb = {
"upper memory block",
mapumb,
&mapumb[63],
};
static Map mapumbrw[8];
static RMap rmapumbrw = {
"UMB device memory",
mapumbrw,
&mapumbrw[7],
};
void
memdebug(void)
{
Map *mp;
ulong maxpa, maxpa1, maxpa2;
if(MEMDEBUG == 0)
return;
maxpa = (nvramread(0x18)<<8)|nvramread(0x17);
maxpa1 = (nvramread(0x31)<<8)|nvramread(0x30);
maxpa2 = (nvramread(0x16)<<8)|nvramread(0x15);
print("maxpa = %luX -> %luX, maxpa1 = %luX maxpa2 = %luX\n",
maxpa, MB+maxpa*KB, maxpa1, maxpa2);
for(mp = rmapram.map; mp->size; mp++)
print("%8.8luX %8.8luX %8.8luX\n", mp->addr, (ulong)mp->size, mp->addr+mp->size);
for(mp = rmapumb.map; mp->size; mp++)
print("%8.8luX %8.8luX %8.8luX\n", mp->addr, (ulong)mp->size, mp->addr+mp->size);
for(mp = rmapumbrw.map; mp->size; mp++)
print("%8.8luX %8.8luX %8.8luX\n", mp->addr, (ulong)mp->size, mp->addr+mp->size);
for(mp = rmapupa.map; mp->size; mp++)
print("%8.8luX %8.8luX %8.8luX\n", mp->addr, (ulong)mp->size, mp->addr+mp->size);
}
void
mapfree(RMap* rmap, ulong addr, ulong size)
{
Map *mp;
ulong t;
if(size == 0)
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
print("mapfree: %s: losing 0x%luX, %lud\n",
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
static void
umbscan(void)
{
uchar *p;
p = KADDR(0xD0000);
while(p < (uchar*)KADDR(0xE0000)){
if (p[0] == 0x55 && p[1] == 0xAA) {
p += p[2] * 512;
continue;
}
p[0] = 0xCC;
p[2*KB-1] = 0xCC;
if(p[0] != 0xCC || p[2*KB-1] != 0xCC){
p[0] = 0x55;
p[1] = 0xAA;
p[2] = 4;
if(p[0] == 0x55 && p[1] == 0xAA){
p += p[2]*512;
continue;
}
if(p[0] == 0xFF && p[1] == 0xFF)
mapfree(&rmapumb, PADDR(p), 2*KB);
}
else
mapfree(&rmapumbrw, PADDR(p), 2*KB);
p += 2*KB;
}
p = KADDR(0xE0000);
if(p[0] != 0x55 || p[1] != 0xAA){
p[0] = 0xCC;
p[64*KB-1] = 0xCC;
if(p[0] != 0xCC && p[64*KB-1] != 0xCC)
mapfree(&rmapumb, PADDR(p), 64*KB);
}
}
void
meminit(ulong)
{
ulong maxmem = 0x40000000;
umbscan();
mapfree(&rmapupa, maxmem, 0x00000000-maxmem);
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
uchar *p;
if(a = mapalloc(&rmapumbrw, addr, size, align))
return(ulong)KADDR(a);
if((a = umbmalloc(addr, size, align)) == 0)
return 0;
p = (uchar*)a;
p[0] = 0xCC;
p[size-1] = 0xCC;
if(p[0] == 0xCC && p[size-1] == 0xCC)
return a;
umbfree(a, size);
return 0;
}
void
umbrwfree(ulong addr, int size)
{
mapfree(&rmapumbrw, PADDR(addr), size);
}
ulong*
mmuwalk(ulong* pdb, ulong va, int level, int create)
{
ulong pa, *table;
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
panic("mmuwalk2: va 0x%ux entry 0x%ux\n", va, *table);
if(!(*table & PTEVALID)){
pa = PADDR(ialloc(BY2PG, BY2PG));
*table = pa|PTEWRITE|PTEVALID;
}
table = KADDR(PPN(*table));
return &table[PTX(va)];
}
}
static Lock mmukmaplock;
ulong
mmukmap(ulong pa, ulong va, int size)
{
ulong pae, *table, *pdb, pgsz, *pte, x;
int pse, sync;
extern int cpuidax, cpuiddx;
pdb = KADDR(getcr3());
if((cpuiddx & 0x08) && (getcr4() & 0x10))
pse = 1;
else
pse = 0;
sync = 0;
pa = PPN(pa);
if(va == 0)
va = (ulong)KADDR(pa);
else
va = PPN(va);
pae = pa + size;
lock(&mmukmaplock);
while(pa < pae){
table = &pdb[PDX(va)];
if(*table & PTEVALID){
if(*table & PTESIZE){
x = PPN(*table);
if(x != pa)
panic("mmukmap1: pa 0x%ux  entry 0x%ux\n",
pa, *table);
x += 4*MB;
if(pae <= x){
pa = pae;
break;
}
pgsz = x - pa;
pa += pgsz;
va += pgsz;
continue;
}
else{
pte = mmuwalk(pdb, va, 2, 0);
if(pte && *pte & PTEVALID){
x = PPN(*pte);
if(x != pa)
panic("mmukmap2: pa 0x%ux entry 0x%ux\n",
pa, *pte);
pgsz = BY2PG;
pa += pgsz;
va += pgsz;
sync++;
continue;
}
}
}
if(pse && (pa % (4*MB)) == 0 && (pae >= pa+4*MB)){
*table = pa|PTESIZE|PTEWRITE|PTEUNCACHED|PTEVALID;
pgsz = 4*MB;
}
else{
pte = mmuwalk(pdb, va, 2, 1);
*pte = pa|PTEWRITE|PTEUNCACHED|PTEVALID;
pgsz = BY2PG;
}
pa += pgsz;
va += pgsz;
sync++;
}
unlock(&mmukmaplock);
if(sync)
putcr3(PADDR(pdb));
return pa;
}
ulong
upamalloc(ulong addr, int size, int align)
{
ulong ae, a;
USED(align);
if((a = mapalloc(&rmapupa, addr, size, align)) == 0){
memdebug();
return 0;
}
ae = mmukmap(a, 0, size);
USED(ae);
return a;
}
void
upafree(ulong pa, int size)
{
USED(pa, size);
}