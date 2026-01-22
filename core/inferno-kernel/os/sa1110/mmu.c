#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
ulong
va2pa(void *v)
{
int idx;
ulong pte, ste, *ttb;
idx = MmuL1x((ulong)v);
ttb = (ulong*)KTTB;
ste = ttb[idx];
switch(ste & MmuL1type) {
case MmuL1section:
return MmuSBA(ste)|((ulong)v & 0x000fffff);
case MmuL1page:
pte = ((ulong *)MmuPTBA(ste))[MmuL2x((ulong)v)];
switch(pte & 3) {
case MmuL2large:
return (pte & 0xffff0000)|((ulong)v & 0x0000ffff);
case MmuL2small:
return (pte & 0xfffff000)|((ulong)v & 0x00000fff);
}
}
return 0;
}
enum {
SectionPages = MmuSection/MmuSmallPage,
PtAlign = 1<<10,
MINICACHED = 0x10000000,
};
void
prs(char *s)
{
for(; *s; s++)
uartputc(*s);
}
void
pr16(ulong n)
{
int i;
for(i=28; i>=0; i-=4)
uartputc("0123456789ABCDEF"[(n>>i)&0xF]);
}
void*
mmuphysmap(ulong phys, ulong)
{
ulong *ttb;
void *va;
ttb = (ulong*)KTTB;
va = KADDR(phys);
ttb[MmuL1x((ulong)va)] = phys | 0xC10 | MmuL1section;
return va;
}
void
mmuinit(void)
{
int i;
ulong *ttb, *ptable, va;
ttb = (ulong*)KTTB;
for(i=0; i<MmuL1x(0x10000000); i++)
ttb[i] = 0;
for(; i < 0x1000; i++)
ttb[i] = (i<<20) | 0xC10 | MmuL1section;
for(va = KZERO; va < KZERO+64*MB; va += MB)
ttb[MmuL1x(va)] |= MmuWB | MmuIDC;
for(i = 0; i < 64*MB; i += MB)
ttb[MmuL1x(UCDRAMZERO+i)] = (PHYSMEM0+i) | 0xC10 | MmuL1section;
for(va = KZERO; va < KZERO+64*MB; va += MB)
ttb[MmuL1x(va|MINICACHED)] = va | 0xC10 | MmuIDC | MmuL1section;
ttb[MmuL1x(DCFADDR)] |= MmuIDC | MmuWB;
ttb[MmuL1x(MCFADDR)] |= MmuIDC;
for(i=0; i<32*MB; i+=MB)
ttb[MmuL1x(FLASHMEM+i)] = (PHYSFLASH0+i) | 0xC10 | MmuL1section;
ptable = xspanalloc(SectionPages*sizeof(*ptable), PtAlign, 0);
ptable[MmuL2x(AIVECADDR)] = PADDR(page0) | MmuL2AP(MmuAPsrw) | MmuWB | MmuIDC | MmuL2small;
ttb[MmuL1x(AIVECADDR)] = PADDR(ptable) | MmuL1page;
mmuputttb(KTTB);
mmuputdac(1);
mmuenable(CpCaltivec | CpCIcache | CpCsystem | (1<<6) | CpCd32 | CpCi32 | CpCwb | CpCDcache | CpCmmu);
}
int
segflush(void *a, ulong n)
{
dcflush(a, n);
icflushall();
return 0;
}
void *
minicached(void *a)
{
if(conf.useminicache == 0)
return a;
dcflushall();
minidcflush();
dcinval();
return (void*)((ulong)a | MINICACHED);
}