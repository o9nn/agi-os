#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"io.h"
#include	"draw.h"
#include	<memdraw.h>
#include "../port/netif.h"
#include "etherif.h"
#include	"../port/flashif.h"
enum {
LED0 = 1<<0,
LED1 = 1<<1,
LED2 = 1<<2,
LED3 = 1<<3,
CFBVD1 = 1<<20,
CFBVD2 = 1<<19,
CFReset = 1<<21,
CFRdypin = 22,
CFRdy = 1<<CFRdypin,
CFnCDxpin = 23,
CFnCDx = 1<<CFnCDxpin,
EnableRS232In = 1<<24,
EnableRS232Out = 1<<25,
};
void
archreset(void)
{
GpioReg *g = GPIOREG;
g->grer = 0;
g->gfer = 0;
g->gedr = g->gedr;
g->gpdr = 0;
g->gpdr = EnableRS232In | EnableRS232Out | CFReset;
g->gpsr = EnableRS232In | EnableRS232Out;
GPCLKREG->gpclkr0 |= 1;
}
void
archconfinit(void)
{
int w;
conf.topofmem = 0xC0000000+32*MB;
w = PMGRREG->ppcr & 0x1f;
m->cpuhz = CLOCKFREQ*(w*4+16);
conf.useminicache = 1;
conf.portrait = 1;
}
void
archconsole(void)
{
uartspecial(0, 38400, 'n', &kbdq, &printq, kbdcr2nl);
}
void
archuartpower(int, int)
{
}
void
kbdinit(void)
{
}
void
archreboot(void)
{
dcflushall();
GPIOREG->gedr = 1<<0;
mmuputctl(mmugetctl() & ~CpCaltivec);
RESETREG->rsrr = 1;
for(;;)
spllo();
}
void
archflashwp(Flash*, int)
{
}
int
archflashreset(int bank, Flash *f)
{
if(bank != 0)
return -1;
f->type = "cfi16";
f->addr = KADDR(FLASHMEM);
f->size = 0;
f->width = 2;
return 0;
}
int
pcmpowered(int slotno)
{
if(slotno)
return 0;
return 3;
}
void
pcmpower(int slotno, int on)
{
USED(slotno, on);
}
void
pcmreset(int slot)
{
if(slot != 0)
return;
GPIOREG->gpsr = CFReset;
delay(100);
GPIOREG->gpcr = CFReset;
}
int
pcmpin(int slot, int type)
{
if(slot)
return -1;
switch(type){
case PCMready:
return CFRdypin;
case PCMeject:
return CFnCDxpin;
case PCMstschng:
return -1;
}
}
void
pcmsetvpp(int slot, int vpp)
{
USED(slot, vpp);
}
int
archether(int ctlno, Ether *ether)
{
if(ctlno > 0)
return -1;
sprint(ether->type, "CS8900");
ether->nopt = 0;
ether->irq = 26;
ether->itype = BusGPIOrising;
return 1;
}