#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
enum {
DEBUG = 0,
};
void
chandevpower(int up)
{
int i;
if(up){
for(i=0; devtab[i] != nil; i++)
if(devtab[i]->power != nil)
devtab[i]->power(1);
}else{
for(i=0; devtab[i] != nil; i++)
;
while(--i >= 0)
if(devtab[i]->power != nil)
devtab[i]->power(0);
}
}
static void
dumpitall(void)
{
iprint("intr: icip %lux iclr %lux iccr %lux icmr %lux\n",
INTRREG->icip,
INTRREG->iclr, INTRREG->iccr, INTRREG->icmr );
iprint("gpio: lvl %lux dir %lux, re %lux, fe %lux sts %lux alt %lux\n",
GPIOREG->gplr,
GPIOREG->gpdr, GPIOREG->grer, GPIOREG->gfer,
GPIOREG->gpsr, GPIOREG->gafr);
iprint("uart1: %lux %lux %lux\nuart3: %lux %lux %lux\n",
UARTREG(1)->utcr0, UARTREG(1)->utsr0, UARTREG(1)->utsr1,
UARTREG(3)->utcr0, UARTREG(3)->utsr0, UARTREG(3)->utsr1);
iprint("tmr: osmr %lux %lux %lux %lux oscr %lux ossr %lux oier %lux\n",
OSTMRREG->osmr[0], OSTMRREG->osmr[1],
OSTMRREG->osmr[2], OSTMRREG->osmr[3],
OSTMRREG->oscr, OSTMRREG->ossr, OSTMRREG->oier);
iprint("dram: mdcnfg %lux mdrefr %lux cas %lux %lux %lux %lux %lux %lux\n",
MEMCFGREG->mdcnfg, MEMCFGREG->mdrefr,
MEMCFGREG->mdcas0[0], MEMCFGREG->mdcas0[1],MEMCFGREG->mdcas0[2],
MEMCFGREG->mdcas2[0], MEMCFGREG->mdcas2[1],MEMCFGREG->mdcas2[2]);
iprint("dram: mdcnfg msc %lux %lux %lux mecr %lux\n",
MEMCFGREG->msc0, MEMCFGREG->msc1,MEMCFGREG->msc2,
MEMCFGREG->mecr);
}
static ulong *coreregs[] = {
&MEMCFGREG->mecr,
&MEMCFGREG->msc0,
&MEMCFGREG->msc1,
&MEMCFGREG->msc2,
&PPCREG->ppdr,
&PPCREG->ppsr,
&PPCREG->ppar,
&PPCREG->psdr,
&GPIOREG->grer,
&GPIOREG->gfer,
&GPIOREG->gafr,
&GPIOREG->gpdr,
&GPCLKREG->gpclkr1,
&GPCLKREG->gpclkr2,
&GPCLKREG->gpclkr0,
&OSTMRREG->osmr[0],
&OSTMRREG->osmr[1],
&OSTMRREG->osmr[2],
&OSTMRREG->osmr[3],
&OSTMRREG->oscr,
&OSTMRREG->oier,
&INTRREG->iclr,
&INTRREG->iccr,
&INTRREG->icmr,
nil,
};
static ulong corestate[nelem(coreregs)];
void
powersuspend(void)
{
extern void suspenditall(void);
GpioReg *g;
ulong back = 0x43219990;
ulong pwer, gplr;
ulong *rp;
int i, s;
s = splfhi();
archpowerdown();
if(DEBUG)
dumpitall();
blankscreen(1);
chandevpower(0);
gplr = GPIOREG->gplr;
for(i=0; (rp = coreregs[i]) != nil; i++)
corestate[i] = *rp;
pwer = PMGRREG->pwer;
if(pwer == 0)
pwer = 1<<0;
g = GPIOREG;
g->grer &= pwer;
g->gfer &= pwer;
g->gedr = g->gedr;
RESETREG->rcsr = 0xF;
minidcflush();
if(DEBUG)
iprint("suspenditall...\n");
suspenditall();
PMGRREG->pspr = 0;
archpowerup();
trapstacks();
GPIOREG->gpsr = gplr;
GPIOREG->gpcr = ~gplr;
for(i=0; (rp = coreregs[i]) != nil; i++)
*rp = corestate[i];
GPIOREG->gedr = GPIOREG->gedr;
PMGRREG->pssr = PSSR_ph;
chandevpower(1);
if(back != 0x43219990){
iprint("back %8.8lux\n", back);
panic("powersuspend");
}
blankscreen(0);
if(DEBUG)
dumpitall();
splx(s);
}