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
MemMin		= 8*MB,
MemMax		= (3*1024+768)*MB,
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
"unallocated unbacked physical memory",
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
static void*
sigscan(uchar* addr, int len, char* signature)
{
int sl;
uchar *e, *p;
e = addr+len;
sl = strlen(signature);
for(p = addr; p+sl < e; p += 16)
if(memcmp(p, signature, sl) == 0)
return p;
return nil;
}
void*
sigsearch(char* signature)
{
uintptr p;
uchar *bda;
void *r;
bda = BIOSSEG(0x40);
if(memcmp(KADDR(0xfffd9), "EISA", 4) == 0){
if((p = (bda[0x0f]<<8)|bda[0x0e]) != 0){
if((r = sigscan(BIOSSEG(p), 1024, signature)) != nil)
return r;
}
}
if((p = ((bda[0x14]<<8)|bda[0x13])*1024) != 0){
if((r = sigscan(KADDR(p-1024), 1024, signature)) != nil)
return r;
}
if((r = sigscan(KADDR(0xa0000-1024), 1024, signature)) != nil)
return r;
return sigscan(BIOSSEG(0xe000), 0x20000, signature);
}
static void
lowraminit(void)
{
ulong n, pa, x;
uchar *bda;
x = PADDR(CPU0END);
bda = (uchar*)KADDR(0x400);
n = ((bda[0x14]<<8)|bda[0x13])*KB-x;
mapfree(&rmapram, x, n);
memset(KADDR(x), 0, n);
x = PADDR(PGROUND((ulong)end));
pa = MemMin;
if(x > pa)
panic("kernel too big");
mapfree(&rmapram, x, pa-x);
memset(KADDR(x), 0, pa-x);
}
static void
ramscan(ulong maxmem)
{
ulong *k0, kzero, map, maxkpa, maxpa, pa, *pte, *table, *va, vbase, x;
int nvalid[NMemType];
pa = MemMin;
if(maxmem == 0){
x = (nvramread(0x18)<<8)|nvramread(0x17);
if(x == 0 || x >= (63*KB))
maxpa = MemMax;
else
maxpa = MB+x*KB;
if(maxpa < 24*MB)
maxpa = 24*MB;
}else
maxpa = maxmem;
maxkpa = (u32int)-KZERO;
k0 = (ulong*)KADDR(0);
kzero = *k0;
map = 0;
x = 0x12345678;
memset(nvalid, 0, sizeof(nvalid));
vbase = MemMin;
while(pa < maxpa){
va = (void*)(vbase + pa%(4*MB));
table = &m->pdb[PDX(va)];
if(pa%(4*MB) == 0){
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
if(pa%(4*MB) == 0 && pa >= 32*MB && nvalid[MemUPA] == (4*MB)/BY2PG){
break;
}
if(pa <= maxkpa && pa%(4*MB) == 0){
table = &m->pdb[PDX(KADDR(pa - 4*MB))];
if(nvalid[MemUPA] == (4*MB)/BY2PG)
*table = 0;
else if(nvalid[MemRAM] == (4*MB)/BY2PG && (m->cpuiddx & 0x08))
*table = (pa - 4*MB)|PTESIZE|PTEWRITE|PTEVALID;
else if(nvalid[MemUMB] == (4*MB)/BY2PG && (m->cpuiddx & 0x08))
*table = (pa - 4*MB)|PTESIZE|PTEWRITE|PTEUNCACHED|PTEVALID;
else{
*table = map|PTEWRITE|PTEVALID;
map = 0;
}
}
mmuflushtlb(PADDR(m->pdb));
x += 0x3141526;
}
if(pa % (4*MB) && pa <= maxkpa){
m->pdb[PDX(KADDR(pa))] = map|PTEWRITE|PTEVALID;
map = 0;
}
if(map)
mapfree(&rmapram, map, BY2PG);
m->pdb[PDX(vbase)] = 0;
mmuflushtlb(PADDR(m->pdb));
mapfree(&rmapupa, pa, (u32int)-pa);
*k0 = kzero;
}
enum
{
SMAP = ('S'<<24)|('M'<<16)|('A'<<8)|'P',
Ememory = 1,
Ereserved = 2,
Carry = 1,
};
typedef struct Emap Emap;
struct Emap
{
uvlong base;
uvlong len;
ulong type;
};
static Emap emap[16];
int nemap;
static char *etypes[] =
{
"type=0",
"memory",
"reserved",
"acpi reclaim",
"acpi nvs",
};
static int
emapcmp(const void *va, const void *vb)
{
Emap *a, *b;
a = (Emap*)va;
b = (Emap*)vb;
if(a->base < b->base)
return -1;
if(a->base > b->base)
return 1;
if(a->len < b->len)
return -1;
if(a->len > b->len)
return 1;
return a->type - b->type;
}
static void
map(ulong base, ulong len, int type)
{
ulong e, n;
ulong *table, flags, maxkpa;
if(base < MemMin && len > MemMin-base){
n = MemMin - base;
map(base, n, type);
map(MemMin, len-n, type);
}
if(base < MemMin)
return;
if(type == MemUPA && base < 16*MB && base+len > 16*MB){
map(base, 16*MB-base, MemUMB);
map(16*MB, len-(16*MB-base), MemUPA);
return;
}
if(base < PADDR(CPU0END)){
n = PADDR(CPU0END) - base;
if(len <= n)
return;
map(PADDR(CPU0END), len-n, type);
return;
}
if(base < PADDR(KTZERO) && base+len > PADDR(KTZERO)){
map(base, PADDR(KTZERO)-base, type);
return;
}
if(PADDR(KTZERO) < base && base < PADDR(PGROUND((ulong)end))){
n = PADDR(PGROUND((ulong)end));
if(len <= n)
return;
map(PADDR(PGROUND((ulong)end)), len-n, type);
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
if(base < MemMin){
table = KADDR(PPN(m->pdb[PDX(base)]));
e = base+len;
base = PPN(base);
for(; base<e; base+=BY2PG)
table[PTX(base)] |= flags;
return;
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
static int
e820scan(void)
{
int i;
Ureg u;
ulong cont, base, len;
uvlong last;
Emap *e;
if(getconf("*norealmode") || getconf("*noe820scan"))
return -1;
cont = 0;
for(i=0; i<nelem(emap); i++){
memset(&u, 0, sizeof u);
u.ax = 0xE820;
u.bx = cont;
u.cx = 20;
u.dx = SMAP;
u.es = (PADDR(RMBUF)>>4)&0xF000;
u.di = PADDR(RMBUF)&0xFFFF;
u.trap = 0x15;
realmode(&u);
cont = u.bx;
if((u.flags&Carry) || u.ax != SMAP || u.cx != 20)
break;
e = &emap[nemap++];
*e = *(Emap*)RMBUF;
if(u.bx == 0)
break;
}
if(nemap == 0)
return -1;
qsort(emap, nemap, sizeof emap[0], emapcmp);
if(getconf("*noe820print") == nil){
for(i=0; i<nemap; i++){
e = &emap[i];
print("E820: %.8llux %.8llux ", e->base, e->base+e->len);
if(e->type < nelem(etypes))
print("%s\n", etypes[e->type]);
else
print("type=%lud\n", e->type);
}
}
last = 0;
for(i=0; i<nemap; i++){
e = &emap[i];
if(e->base >= (1LL<<32))
break;
base = e->base;
if(base+e->len > (1LL<<32))
len = -base;
else
len = e->len;
if(last < e->base)
map(last, e->base-last, MemUPA);
last = base+len;
if(e->type == Ememory)
map(base, len, MemRAM);
else
map(base, len, MemReserved);
}
if(last < (1LL<<32))
map(last, (u32int)-last, MemUPA);
return 0;
}
void
meminit(void)
{
int i;
Map *mp;
Confmem *cm;
ulong pa, *pte;
ulong maxmem, lost;
char *p;
if(p = getconf("*maxmem"))
maxmem = strtoul(p, 0, 0);
else
maxmem = 0;
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
if(e820scan() < 0)
ramscan(maxmem);
for(i=0; i<nelem(mapram) && i<nelem(conf.mem); i++){
mp = &rmapram.map[i];
cm = &conf.mem[i];
cm->base = mp->addr;
cm->npage = mp->size/BY2PG;
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