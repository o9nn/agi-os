#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#define TTB(pa) ((pa) & ~0x3FFF)
#define L1x(pa) (((pa)>>20) & 0xFFF)
#define PTBA(pa) ((pa) & ~0x3FF)
#define L2x(pa) (((pa)>>12) & 0xFF)
#define PBA(pa) ((pa) & ~0xFFF)
#define SBA(pa) ((pa) & ~0xFFFFF)
enum {
Section= 1<<20,
LargePage= 1<<16,
SmallPage= 1<<12,
EsmallPage= 1<<10,
SectionPages = Section/SmallPage,
PtAlign = 1<<10,
L1type= 3<<0,
L1page= 1<<0,
L1section= 2<<0,
L1fpage= 3<<0,
L1buffered= 1<<2,
L1cached= 1<<3,
L1mbo= 1<<4,
L2type= 3<<0,
L2invalid= 0<<0,
L2large= 1<<0,
L2small= 2<<0,
L2esmall= 3<<0,
L2buffered= 1<<2,
L2cached= 1<<3,
L2smallX= 1<<6,
L2largeX= 1<<12,
Dnone= 0,
Dclient= 1,
Dmanager= 3,
APsro= 0,
APsrw= 1,
APuro= 2,
APurw= 3,
};
#define L1dom(d) (((d) & 0xF)<<5)
#define AP(i, v) ((v)<<(((i)*2)+4))
#define L1AP(v) AP(3, (v))
#define L2AP(v) AP(3, (v))|AP(2, (v))|AP(1, (v))|AP(0, (v))
#define L1krw (L1AP(APsrw) | L1dom(0))
ulong
va2pa(void *v)
{
int idx;
ulong pte, ste, *ttb;
idx = L1x((ulong)v);
ttb = (ulong*)KTTB;
ste = ttb[idx];
switch(ste & L1type) {
case L1section:
return SBA(ste)|((ulong)v & 0x000fffff);
case L1page:
pte = ((ulong *)PTBA(ste))[L2x((ulong)v)];
switch(pte & 3) {
case L2large:
return (pte & 0xffff0000)|((ulong)v & 0x0000ffff);
case L2small:
return (pte & 0xfffff000)|((ulong)v & 0x00000fff);
}
}
return 0;
}
void
prs(char *s)
{
serialputs(s, strlen(s));
}
void
pr16(ulong n)
{
int i, c;
for(i=28; i>=0; i-=4){
c = (n>>i) & 0xF;
if(c >= 0 && c <= 9)
c += '0';
else
c += 'A'-10;
serialputc(c);
}
}
void
xdelay(int n)
{
int j;
for(j=0; j<1000000/4; j++)
n++;
USED(n);
}
void*
mmuphysmap(void *va, ulong pa, ulong nbytes)
{
ulong *ttb;
ulong p, o;
if(va == nil)
va = KADDR(pa);
p = (ulong)va;
if((pa|p) & (Section-1))
panic("kmapphys");
ttb = (ulong*)KTTB;
nbytes = (nbytes+Section-1)&~(Section-1);
for(o = 0; o < nbytes; o += Section)
ttb[L1x(p+o)] = (pa+o) | (1<<4) | L1krw | L1section;
return va;
}
void*
mmukaddr(ulong pa)
{
if(pa >= PHYSDRAM0 && pa < conf.topofmem)
return (void*)(KZERO+(pa-PHYSDRAM0));
return (void*)pa;
}
void
mmuinit(void)
{
int i;
ulong *ttb, *ptable;
ttb = (ulong*)KTTB;
memset(ttb, 0, 16384);
for(i = L1x(PHYSFLASH0); i < 0x1000; i++)
ttb[i] = (i<<20) | L1krw | (1<<4) | L1section;
for(i = 0; i < 32*MB; i += MB)
ttb[L1x(KZERO+i)] = (PHYSDRAM0+i) | (1<<4) | L1krw | L1section | L1cached | L1buffered;
for(i = 0; i < 64*MB; i += MB)
ttb[L1x(UCDRAMZERO+i)] = (PHYSDRAM0+i) | L1krw | (1<<4) | L1section;
for(i=0; i<8*MB; i+=MB)
ttb[L1x(FLASHMEM+i)] = (PHYSFLASH0+i) | L1krw | (1<<4) | L1section;
ptable = xspanalloc(SectionPages*sizeof(*ptable), PtAlign, 0);
ptable[L2x(AIVECADDR)] = PADDR(page0) | L2AP(APsrw) | L2cached | L2buffered | L2small;
ttb[L1x(AIVECADDR)] = PADDR(ptable) | (1<<4) | L1page;
mmuputttb(KTTB & ~KZERO);
mmuputdac(Dclient);
mmuputctl(mmugetctl() | CpCaltivec | CpCIcache | CpCsystem | CpCwpd | CpCDcache | CpCmmu);
tlbinvalidateall();
}
int
segflush(void *a, ulong n)
{
dcflush(a, n);
icflush(a, n);
return 0;
}
void*
mmucacheinhib(void *a, ulong nb)
{
ulong p;
if(a == nil)
return nil;
p = PADDR(a);
if(p & (CACHELINESZ-1))
panic("mmucacheinhib");
dcflush(a, nb);
return (void*)(UCDRAMZERO|PADDR(a));
}