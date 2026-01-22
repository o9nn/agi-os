#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
enum {
Nirq	= 4,
Baseirq	= 12,
Nchan	= 32,
};
typedef struct Regs Regs;
typedef struct Dchan Dchan;
struct Regs {
uchar	_pad0[8];
ulong	irqsts[Nirq];
ulong	irqen[Nirq];
ulong	syssts;
ulong	syscfg;
uchar	_pad1[0x64 - 0x30];
ulong	caps[5];
ulong	gcr;
ulong	_pad2;
struct Dchan {
ulong	ccr;
ulong	clnkctrl;
ulong	cicr;
ulong	csr;
ulong	csdp;
ulong	cen;
ulong	cfn;
ulong	cssa;
ulong	cdsa;
ulong	csei;
ulong	csfi;
ulong	cdei;
ulong	cdfi;
ulong	csac;
ulong	cdac;
ulong	ccen;
ulong	ccfn;
ulong	color;
uchar	_pad3[24];
} chan[Nchan];
};
enum {
Blocki	= 1 << 5,
Enable	= 1 << 7,
};
typedef struct Xfer Xfer;
static struct Xfer {
Rendez	*rend;
int	*done;
} xfer[Nirq];
int
isdmadone(int irq)
{
Dchan *cp;
Regs *regs = (Regs *)PHYSSDMA;
cp = regs->chan + irq;
return cp->csr & Blocki;
}
static void
dmaintr(Ureg *, void *a)
{
int i = (int)a;
Dchan *cp;
Regs *regs = (Regs *)PHYSSDMA;
assert(i >= 0 && i < Nirq);
*xfer[i].done = 1;
assert(xfer[i].rend != nil);
wakeup(xfer[i].rend);
cp = regs->chan + i;
if(!(cp->csr & Blocki))
iprint("dmaintr: req %d: Blocki not set; csr %#lux\n",
i, cp->csr);
cp->csr |= cp->csr;
coherence();
regs->irqsts[i] = regs->irqsts[i];
coherence();
regs->irqen[i] &= ~(1 << i);
coherence();
xfer[i].rend = nil;
coherence();
}
void
zerowds(ulong *wdp, int cnt)
{
while (cnt-- > 0)
*wdp++ = 0;
}
static int
istestdmadone(void *arg)
{
return *(int *)arg;
}
void
dmainit(void)
{
int n;
char name[16];
Dchan *cp;
Regs *regs = (Regs *)PHYSSDMA;
if (probeaddr((uintptr)&regs->syssts) < 0)
panic("dmainit: no syssts reg");
regs->syssts = 0;
coherence();
regs->syscfg |= 1<<1;
coherence();
while(!(regs->syssts & (1<<0)))
;
for (n = 0; n < Nchan; n++) {
cp = regs->chan + n;
cp->ccr = 0;
cp->clnkctrl = 0;
cp->cicr = 0;
cp->csr = 0;
cp->csdp = 0;
cp->cen = cp->cfn = 0;
cp->cssa = cp->cdsa = 0;
cp->csei = cp->csfi = 0;
cp->cdei = cp->cdfi = 0;
cp->ccen = cp->ccfn = 0;
cp->color = 0;
}
zerowds((void *)regs->irqsts, sizeof regs->irqsts / sizeof(ulong));
zerowds((void *)regs->irqen,  sizeof regs->irqen / sizeof(ulong));
coherence();
regs->gcr = 65;
coherence();
for (n = 0; n < Nirq; n++) {
snprint(name, sizeof name, "dma%d", n);
intrenable(Baseirq + n, dmaintr, (void *)n, nil, name);
}
}
enum {
Testbyte	= 0252,
Testsize	= 256,
Scratch		= MB,
};
void
dmatest(void)
{
int n, done;
uchar *bp;
static ulong pat = 0x87654321;
static Rendez trendez;
if (up == nil)
panic("dmatest: up not set yet");
bp = (uchar *)KADDR(PHYSDRAM + 128*MB);
memset(bp, Testbyte, Scratch);
done = 0;
dmastart((void *)PADDR(bp), Postincr, (void *)PADDR(&pat), Const,
Testsize, &trendez, &done);
sleep(&trendez, istestdmadone, &done);
cachedinvse(bp, Scratch);
if (((ulong *)bp)[0] != pat)
panic("dmainit: copied incorrect data %#lux != %#lux",
((ulong *)bp)[0], pat);
for (n = Testsize; n < Scratch && bp[n] != Testbyte; n++)
;
if (n >= Scratch)
panic("dmainit: ran wild over memory, clobbered ≥%,d bytes", n);
if (bp[n] == Testbyte && n != Testsize)
iprint("dma: %d-byte dma stopped after %d bytes!\n",
Testsize, n);
}
int
dmastart(void *to, int tmode, void *from, int fmode, uint len, Rendez *rend,
int *done)
{
int irq, chan;
uint ruplen;
Dchan *cp;
Regs *regs = (Regs *)PHYSSDMA;
static Lock alloclck;
ilock(&alloclck);
for (irq = 0; irq < Nirq && xfer[irq].rend != nil; irq++)
;
if (irq >= Nirq)
panic("dmastart: no available irqs; too many concurrent dmas");
chan = irq;
xfer[irq].rend = rend;
xfer[irq].done = done;
*done = 0;
iunlock(&alloclck);
ruplen = ROUNDUP(len, sizeof(ulong));
assert(to != from);
cp = regs->chan + chan;
cp->ccr &= ~Enable;
cp->cicr = 0;
regs->irqen[irq] &= ~(1 << chan);
coherence();
cp->csdp = 2;
cp->cssa = (uintptr)from;
cp->cdsa = (uintptr)to;
cp->ccr = tmode << 14 | fmode << 12;
cp->csei = cp->csfi = cp->cdei = cp->cdfi = 1;
cp->cen = ruplen / sizeof(ulong);
cp->cfn = 1;
cp->cicr = Blocki;
regs->irqen[irq] |= 1 << chan;
coherence();
cp->ccr |= Enable;
coherence();
return irq;
}