#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	<draw.h>
#include	<memdraw.h>
#include	<cursor.h>
#include	"screen.h"
#include "../port/netif.h"
#include "../mpc/etherif.h"
#include "../port/flashif.h"
#include	"archrpcg.h"
enum {
COM3=	IBIT(1)|IBIT(2),
TBS =	IBIT(6),
RTSEL = IBIT(8),
RTDIV = IBIT(7),
CRQEN = IBIT(9),
PRQEN = IBIT(10),
EDBF2 = IBIT(14),
CSRC = IBIT(21),
};
void
archinit(void)
{
IMM *io;
int mf, i;
m->bcsr = KADDR(PHYSBCSR);
m->bcsr[0] &= ~EnableEnet;
io = m->iomem;
m->clockgen = 8000000;
m->oscclk = m->clockgen/MHz;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr &= ~CSRC;
mf = (io->plprcr >> 20)+1;
m->cpuhz = m->clockgen*mf;
m->speed = m->cpuhz/MHz;
io->plprcrk = ~KEEP_ALIVE_KEY;
io->sccrk = KEEP_ALIVE_KEY;
io->sccr |= COM3 | TBS | CRQEN | PRQEN;
io->sccrk = ~KEEP_ALIVE_KEY;
if(0){
io->pgcr[1] = 1<<7;
io->per = 0;
io->pscr = ~0;
for(i=0; i<8; i++)
io->pcmr[i].base = io->pcmr[i].option = 0;
}
}
static ulong
banksize(int x, ulong *pa)
{
IMM *io;
io = m->iomem;
if((io->memc[x].base & 1) == 0)
return 0;
*pa = io->memc[x].base & ~0x7FFF;
return -(io->memc[x].option&~0x7FFF);
}
void
archconfinit(void)
{
ulong nbytes, pa, ktop;
conf.nscc = 2;
conf.nocts2 = 1;
conf.npage0 = 0;
nbytes = banksize(DRAM1CS, &pa);
if(nbytes){
conf.npage0 = nbytes/BY2PG;
conf.base0 = pa;
}
conf.npage1 = 0;
ktop = PGROUND((ulong)end);
ktop = PADDR(ktop) - conf.base0;
conf.npage0 -= ktop/BY2PG;
conf.base0 += ktop;
if(m->bcsr[0] & NVRAMBattGood){
conf.nvramsize = banksize(NVRAMCS, &pa);
conf.nvrambase = KADDR(pa);
}
}
void
cpuidprint(void)
{
ulong v;
int i;
print("PVR: ");
switch(m->cputype){
case 0x01:	print("MPC601"); break;
case 0x03:	print("MPC603"); break;
case 0x04:	print("MPC604"); break;
case 0x06:	print("MPC603e"); break;
case 0x07:	print("MPC603e-v7"); break;
case 0x50:	print("MPC8xx"); break;
default:	print("PowerPC version #%x", m->cputype); break;
}
print(", revision #%lux\n", getpvr()&0xffff);
print("IMMR: ");
v = getimmr() & 0xFFFF;
switch(v>>8){
case 0x00:	print("MPC860/821"); break;
case 0x20:	print("MPC823"); break;
case 0x21:	print("MPC823A"); break;
default:	print("Type #%lux", v>>8); break;
}
print(", mask #%lux\n", v&0xFF);
print("plprcr=%8.8lux sccr=%8.8lux bcsr=%8.8lux\n", m->iomem->plprcr, m->iomem->sccr, m->bcsr[0]);
print("%lud MHz system\n", m->cpuhz/MHz);
print("%lud pages\n", (conf.npage0-conf.base0)/BY2PG);
print("%ludK NVRAM\n", conf.nvramsize/1024);
print("\n");
for(i=0; i<nelem(m->iomem->pcmr); i++)
print("%d: %8.8lux %8.8lux\n", i, m->iomem->memc[i].base, m->iomem->memc[i].option);
}
int
archoptionsw(void)
{
return (m->bcsr[0]&DipSwitchMask)>>4;
}
static void
twinkle(void)
{
if(m->ticks%MS2TK(1000) == 0)
m->bcsr[0] ^= LedOff;
}
void	(*archclocktick)(void) = twinkle;
void
clockcheck(void)
{
}
int
archflashreset(int bank, Flash *f)
{
if(bank != 0)
return -1;
f->type = "AMD29F0x0";
f->addr = KADDR(PHYSFLASH);
f->size = 4*1024*1024;
f->width = 4;
f->interleave = 1;
return 0;
}
int
archether(int ctlrno, Ether *ether)
{
if(isaconfig("ether", ctlrno, ether) == 0)
return -1;
return 1;
}
int
archetherenable(int cpmid, int *rcs, int *tcs, int mbps, int fullduplex)
{
IMM *io;
if(cpmid != CPscc2)
return -1;
USED(mbps);
USED(fullduplex);
io = ioplock();
m->bcsr[0] = (m->bcsr[0] & ~(EnableXcrLB|DisableColTest)) | EnableEnet;
eieio();
io->papar |= SIBIT(6)|SIBIT(4);
io->padir &= ~(SIBIT(6)|SIBIT(4));
iopunlock();
*rcs = CLK4;
*tcs = CLK2;
return 0;
}
void
archenableuart(int id, int irda)
{
USED(id, irda);
}
void
archdisableuart(int id)
{
USED(id);
}
void
archenableusb(int highspeed, int master)
{
ioplock();
if(master)
m->bcsr[0] |= EnableUSBPwr;
else
m->bcsr[0] &= ~EnableUSBPwr;
m->bcsr[0] &= ~DisableUSB;
if(highspeed)
m->bcsr[0] |= HighSpdUSB;
else
m->bcsr[0] &= ~HighSpdUSB;
iopunlock();
}
void
archdisableusb(void)
{
ioplock();
m->bcsr[0] |= DisableUSB;
m->bcsr[0] &= ~EnableUSBPwr;
iopunlock();
}
void
archsetirxcvr(int highspeed)
{
USED(highspeed);
}
void
archreboot(void)
{
IMM *io;
io = m->iomem;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr |= 1<<7;
io->plprcrk = ~KEEP_ALIVE_KEY;
eieio();
io->sdcr = 1;
eieio();
io->lccr = 0;
eieio();
firmware(0);
}
int
pcmslotavail(int slotno)
{
return slotno == 1;
}
void
pcmenable(void)
{
ioplock();
m->bcsr[0] = m->bcsr[0] & ~(VPPMask|VCCMask);
eieio();
m->bcsr[0] |= VCC5V | VPPVCC;
eieio();
m->iomem->pgcr[1] = 0;
iopunlock();
iprint("B=%8.8lux\n", m->bcsr[0]);
}
int
pcmpowered(int)
{
ulong r;
r = m->bcsr[0]&VCCMask;
if(r == VCC5V)
return 5;
if(r == VCC3V)
return 3;
return 0;
}
void
pcmsetvcc(int, int v)
{
if(v == 5)
v = VCC5V;
else if(v == 3)
v = VCC3V;
else
v = VCC0V;
ioplock();
m->bcsr[0] = (m->bcsr[0] & ~VCCMask) | v;
iopunlock();
}
void
pcmsetvpp(int, int v)
{
if(v == 5 || v == 3)
v = VPPVCC;
else if(v == 12)
v = VPP12V;
else if(v == 0)
v = VPP0V;
else
v = VPPHiZ;
ioplock();
m->bcsr[0] = (m->bcsr[0] & ~VPPMask) | v;
iopunlock();
}
void
pcmpower(int slotno, int on)
{
if(!on){
pcmsetvcc(slotno, 0);
pcmsetvpp(slotno, -1);
}else
pcmsetvcc(slotno, 5);
}
void
archbacklight(int on)
{
USED(on);
}
int
archlcdmode(Mode *m)
{
m->x = 640;
m->y = 480;
m->d = 3;
m->lcd.freq = 25000000;
m->lcd.ac = 0;
m->lcd.vpw = 1;
m->lcd.wbf = 33;
m->lcd.wbl = 228;
m->lcd.flags = IsColour | IsTFT | OELow | VsyncLow | ClockLow;
return -1;
}
void
archkbdinit(void)
{
}
void
archflashwp(Flash*, int)
{
}