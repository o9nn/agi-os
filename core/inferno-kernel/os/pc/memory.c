#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#define MEMDEBUG 0
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
static Map mapupa[16];
static RMap rmapupa = {
"unallocated unbacked physical memory",
mapupa,
&mapupa[nelem(mapupa)-1],
};
static Map xmapupa[16];
static RMap xrmapupa = {
"unbacked physical memory",
xmapupa,
&xmapupa[nelem(xmapupa)-1],
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
void
mapprint(RMap *rmap)
{
Map *mp;
print("%s\n", rmap->name);
for(mp = rmap->map; mp->size; mp++)
print("\t%8.8luX %8.8uX %8.8luX\n", mp->addr, mp->size, mp->addr+mp->size);
}
void
memdebug(void)
{
ulong maxpa, maxpa1, maxpa2;
if(MEMDEBUG == 0)
return;
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
print("mapfree: %s: losing 0x%luX, %ld\n",
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
if(p[0] == 0x55 && p[1] == 0xAA){
p += p[2]*512;
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
static void
ramscan(ulong maxmem)
{
ulong *k0, kzero, map, maxpa, pa, *pte, *table, *va, x, n;
int nvalid[NMemType];
uchar *bda;
x = PADDR(CPU0MACH+BY2PG);
bda = (uchar*)KADDR(0x400);
n = ((bda[0x14]<<8)|bda[0x13])*KB-x;
mapfree(&rmapram, x, n);
x = PADDR(PGROUND((ulong)end));
pa = MemMinMB*MB;
mapfree(&rmapram, x, pa-x);
if(maxmem == 0){
x = (nvramread(0x18)<<8)|nvramread(0x17);
if(x == 0 || x >= (63*KB))
maxpa = MemMaxMB*MB;
else
maxpa = MB+x*KB;
if(maxpa < 24*MB)
maxpa = 24*MB;
maxmem = MemMaxMB*MB;
}
else
maxpa = maxmem;
k0 = (ulong*)KADDR(0);
kzero = *k0;
map = 0;
x = 0x12345678;
memset(nvalid, 0, sizeof(nvalid));
while(pa < maxpa){
va = KADDR(pa);
table = &m->pdb[PDX(va)];
if(*table == 0){
if(map == 0 && (map = mapalloc(&rmapram, 0, BY2PG, BY2PG)) == 0)
break;
memset(KADDR(map), 0, BY2PG);
*table = map|PTEWRITE|PTEVALID;
memset(nvalid, 0, sizeof(nvalid));
}
table = KADDR(PPN(*table));
pte = &table[PTX(va)];
*pte = pa|PTEWRITE|PTEUNCACHED|PTEVALID;
mmuflushtlb(PADDR(m->pdb));
*va = x;
*k0 = ~x;
if(*va == x){
nvalid[MemRAM] += MB/BY2PG;
mapfree(&rmapram, pa, MB);
do{
*pte++ = pa|PTEWRITE|PTEVALID;
pa += BY2PG;
}while(pa % MB);
mmuflushtlb(PADDR(m->pdb));
}
else if(pa < 16*MB){
nvalid[MemUMB] += MB/BY2PG;
mapfree(&rmapumb, pa, MB);
do{
*pte++ = pa|PTEWRITE|PTEUNCACHED|PTEVALID;
pa += BY2PG;
}while(pa % MB);
}
else{
nvalid[MemUPA] += MB/BY2PG;
mapfree(&rmapupa, pa, MB);
*pte = 0;
pa += MB;
}
if((pa % (4*MB)) == 0){
table = &m->pdb[PDX(va)];
if(nvalid[MemUPA] == (4*MB)/BY2PG)
*table = 0;
else if(nvalid[MemRAM] == (4*MB)/BY2PG && (m->cpuiddx & 0x08))
*table = (pa - 4*MB)|PTESIZE|PTEWRITE|PTEVALID;
else if(nvalid[MemUMB] == (4*MB)/BY2PG && (m->cpuiddx & 0x08))
*table = (pa - 4*MB)|PTESIZE|PTEWRITE|PTEUNCACHED|PTEVALID;
else
map = 0;
}
mmuflushtlb(PADDR(m->pdb));
x += 0x3141526;
}
if(pa % (4*MB))
map = 0;
if(map)
mapfree(&rmapram, map, BY2PG);
if(pa < maxmem)
mapfree(&rmapupa, pa, maxmem-pa);
if(maxmem < 0xFFE00000)
mapfree(&rmapupa, maxmem, 0x00000000-maxmem);
if(MEMDEBUG)
print("maxmem %luX %luX\n", maxmem, 0x00000000-maxmem);
*k0 = kzero;
}
void
meminit(ulong maxmem)
{
Map *mp, *xmp;
ulong pa, *pte;
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
ramscan(maxmem);
mp = rmapram.map;
conf.base0 = mp->addr;
conf.npage0 = mp->size/BY2PG;
mp++;
for(xmp = 0; mp->size; mp++){
if(xmp == 0 || mp->size > xmp->size)
xmp = mp;
}
if(xmp){
conf.base1 = xmp->addr;
conf.npage1 = xmp->size/BY2PG;
}
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
ulong
upamalloc(ulong pa, int size, int align)
{
ulong a, ae;
if(a = mapalloc(&xrmapupa, pa, size, align))
return a;
if((a = mapalloc(&rmapupa, pa, size, align)) == 0){
memdebug();
return 0;
}
ae = mmukmap(a, a, size);
USED(ae);
return a;
}
void
upafree(ulong pa, int size)
{
mapfree(&xrmapupa, pa, size);
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