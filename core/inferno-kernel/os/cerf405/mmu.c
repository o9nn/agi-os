#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
extern	ulong	tlbtab[], tlbtabe[];
static	int	tlbx;
enum
{
Isarc=	0x018,
Iscntl=	0x019,
Isen=		1<<31,
Dsarc=	0x01A,
Dscntl=	0x01B,
Dsen=		1<<31,
Dof=		1<<30,
};
void
mmuinit(void)
{
int i;
tlbx = -1;
for(i = 0; i < 64; i++)
if((tlbrehi(i) & TLBVALID) == 0){
if(tlbx < 0)
tlbx = i;
tlbwelo(i, 0);
tlbwehi(i, 0);
}
iprint("ccr0=%8.8lux\n", getccr0());
putdcr(Isarc, OCMZERO);
putdcr(Dsarc, OCMZERO);
putdcr(Iscntl, Isen);
putdcr(Iscntl, Dsen|Dof);
tlbwelo(tlbx, OCMZERO|TLBZONE(0)|TLBWR|TLBEX|TLBI);
tlbwehi(tlbx, OCMZERO|TLB4K|TLBVALID);
tlbx++;
}
int
segflush(void *a, ulong n)
{
dcflush(a, n);
icflush(a, n);
return 0;
}
ulong
mmumapsize(ulong n)
{
ulong size;
int i;
size = 1024;
for(i = 0; i < 8 && size < n; i++)
size <<= 2;
return size;
}
void*
kmapphys(void *va, ulong pa, ulong nb, ulong attr, ulong le)
{
int s, i;
ulong size;
if(va == nil)
va = (void*)pa;
size = 1024;
for(i = 0; i < 8 && size < nb; i++)
size <<= 2;
if(i >= 8)
return nil;
s = splhi();
tlbwelo(tlbx, pa | TLBZONE(0) | attr);
tlbwehi(tlbx, (ulong)va | (i<<7) | TLBVALID | le);
tlbx++;
splx(s);
return va;
}
void*
mmucacheinhib(void *a, ulong nb)
{
ulong p;
if(a == nil)
return nil;
dcflush(a, nb);
p = PADDR(a);
return kmapphys((void*)(KSEG1|p), p, nb, TLBWR | TLBI | TLBG, 0);
}