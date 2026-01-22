#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/netif.h"
#include "../mpc/etherif.h"
#include "../port/flashif.h"
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
#include "archfads.h"
enum {
BOOTCS = 0,
BCSRCS = 1,
DRAM1 = 2,
DRAM2 = 3,
SDRAM = 4,
RTSEL = IBIT(8),
RTDIV = IBIT(7),
CRQEN = IBIT(9),
PRQEN = IBIT(10),
CSRC = IBIT(21),
};
void
archinit(void)
{
IMM *io;
int mf;
m->bcsr = KADDR(PHYSBCSR);
m->bcsr[1] |= DisableRS232a | DisableIR | DisableEther | DisablePCMCIA | DisableRS232b;
m->bcsr[1] &= ~(DisableDRAM|DisableFlash);
m->bcsr[1] &= ~EnableSDRAM;
m->bcsr[4] &= ~EnableVideoClock;
m->bcsr[4] |= DisableVideoLamp;
io = m->iomem;
if(1 || (io->sccr & RTDIV) != 0){
if((m->bcsr[2]>>19)&(1<<2))
m->clockgen = 5*MHz;
else
m->clockgen = 4*MHz;
} else
m->clockgen = 32768;
m->oscclk = m->clockgen/MHz;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr &= ~CSRC;
mf = (io->plprcr >> 20)+1;
io->plprcrk = ~KEEP_ALIVE_KEY;
io->sccrk = KEEP_ALIVE_KEY;
io->sccr |= CRQEN | PRQEN;
io->sccr |= RTSEL;
io->sccrk = ~KEEP_ALIVE_KEY;
m->cpuhz = m->clockgen*mf;
m->speed = m->cpuhz/MHz;
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
if((m->bcsr[1] & DisableDRAM) == 0){
nbytes = banksize(DRAM1, &pa);
if(nbytes){
conf.npage0 = nbytes/BY2PG;
conf.base0 = pa;
}
}
conf.npage1 = 0;
if(m->bcsr[1] & EnableSDRAM){
nbytes = banksize(SDRAM, &pa);
if(nbytes){
conf.npage1 = nbytes/BY2PG;
conf.base1 = pa;
}
}
ktop = PGROUND((ulong)end);
ktop = PADDR(ktop) - conf.base0;
conf.npage0 -= ktop/BY2PG;
conf.base0 += ktop;
}
static void
archidprint(void)
{
int f, i;
ulong v;
print("IMMR: ");
v = getimmr() & 0xFFFF;
switch(v>>8){
case 0x00: print("MPC860/821"); break;
case 0x20: print("MPC823"); break;
case 0x21: print("MPC823A"); break;
default: print("Type #%lux", v>>8); break;
}
print(", mask #%lux\n", v&0xFF);
v = m->bcsr[3]>>16;
print("MPC8xxFADS rev %lud, DB: ", ((v>>4)&8)|((v>>1)&4)|(v&3));
f = (v>>8)&0x3F;
switch(f){
default: print("ID#%x", f); break;
case 0x00: print("MPC860/821"); break;
case 0x01: print("MPC813"); break;
case 0x02: print("MPC821"); break;
case 0x03: print("MPC823"); break;
case 0x20: print("MPC801"); break;
case 0x21: print("MPC850"); break;
case 0x22: print("MPC860"); break;
case 0x23: print("MPC860SAR"); break;
case 0x24: print("MPC860T"); break;
}
print("ADS, rev #%lux\n", (m->bcsr[2]>>16)&7);
for(i=0; i<=4; i++)
print("BCSR%d: %8.8lux\n", i, m->bcsr[i]);
v = m->bcsr[2];
f = (v>>28)&0xF;
switch(f){
default: print("Unknown"); break;
case 4: print("SM732A2000/SM73228 - 8M SIMM"); break;
case 5: print("SM732A1000A/SM73218 - 4M SIMM"); break;
case 6: print("MCM29080 - 8M SIMM"); break;
case 7: print("MCM29040 - 4M SIMM"); break;
case 8: print("MCM29020 - 2M SIMM"); break;
}
switch((m->bcsr[3]>>20)&7){
default: i = 0; break;
case 1: i = 150; break;
case 2: i = 120; break;
case 3: i = 90; break;
}
print(" flash, %dns\n", i);
f = (v>>23)&0xF;
switch(f&3){
case 0: i = 4; break;
case 1: i = 32; break;
case 2: i = 16; break;
case 3: i = 8; break;
}
print("%dM SIMM, ", i);
switch(f>>2){
default: i = 0; break;
case 2: i = 70; break;
case 3: i = 60; break;
}
print("%dns\n", i);
print("options: #%lux\n", (m->bcsr[2]>>19)&0xF);
print("plprcr=%8.8lux sccr=%8.8lux\n", m->iomem->plprcr, m->iomem->sccr);
}
void
cpuidprint(void)
{
print("PVR: ");
switch(m->cputype){
case 0x01: print("MPC601"); break;
case 0x03: print("MPC603"); break;
case 0x04: print("MPC604"); break;
case 0x06: print("MPC603e"); break;
case 0x07: print("MPC603e-v7"); break;
case 0x50: print("MPC8xx"); break;
default: print("PowerPC version #%x", m->cputype); break;
}
print(", revision #%lux\n", getpvr()&0xffff);
archidprint();
print("%lud MHz system\n", m->cpuhz/MHz);
print("\n");
}
int
archoptionsw(void)
{
return (m->bcsr[2]>>19)&0xF;
}
static void
twinkle(void)
{
if(m->ticks%MS2TK(1000) == 0)
m->bcsr[4] ^= DisableLamp;
}
void (*archclocktick)(void) = twinkle;
void
clockcheck(void)
{
}
int
archflashreset(int bank, Flash *f)
{
char *t;
int mbyte;
if(bank != 0)
return -1;
switch((m->bcsr[2]>>28)&0xF){
default: return -1;
case 4: mbyte=8; t = "SM732x8"; break;
case 5: mbyte=4; t = "SM732x8"; break;
case 6: mbyte=8; t = "AMD29F0x0"; break;
case 7: mbyte=4; t = "AMD29F0x0"; break;
case 8: mbyte=2; t = "AMD29F0x0"; break;
}
f->type = t;
f->addr = KADDR(PHYSFLASH);
f->size = mbyte*1024*1024;
f->width = 4;
f->interleave = 3;
return 0;
}
void
archflashwp(Flash*, int)
{
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
USED(mbps, fullduplex);
switch(cpmid){
default:
return -1;
case CPscc2:
io = ioplock();
m->bcsr[1] |= DisableIR|DisableRS232b;
m->bcsr[1] &= ~DisableEther;
io->papar |= SIBIT(6)|SIBIT(5);
io->padir &= ~(SIBIT(6)|SIBIT(5));
iopunlock();
*rcs = CLK2;
*tcs = CLK3;
break;
case CPscc1:
io = ioplock();
m->bcsr[1] |= DisableIR|DisableRS232b;
m->bcsr[1] &= ~DisableEther;
io->papar |= SIBIT(6)|SIBIT(7);
io->padir &= ~(SIBIT(6)|SIBIT(7));
io->pcpar &= ~(SIBIT(4)|SIBIT(5)|SIBIT(6));
io->pcdir |= SIBIT(4)|SIBIT(5)|SIBIT(6);
io->pcdat &= ~SIBIT(4);
io->pcdat |= SIBIT(5)|SIBIT(6);
iopunlock();
*rcs = CLK2;
*tcs = CLK1;
break;
}
return 0;
}
void
archenableuart(int id, int irda)
{
switch(id){
case CPsmc1:
m->bcsr[1] &= ~DisableRS232a;
break;
case CPscc2:
m->bcsr[1] |= DisableEther|DisableIR|DisableRS232b;
if(irda)
m->bcsr[1] &= ~DisableIR;
else
m->bcsr[1] &= ~DisableRS232b;
break;
default:
break;
}
}
void
archdisableuart(int id)
{
switch(id){
case CPsmc1:
m->bcsr[1] |= DisableRS232a;
break;
case CPscc2:
m->bcsr[1] |= DisableIR|DisableRS232b;
break;
default:
break;
}
}
void
archenableusb(int highspeed, int master)
{
if(highspeed)
m->bcsr[4] |= USBFullSpeed;
else
m->bcsr[4] &= ~USBFullSpeed;
if(master)
m->bcsr[4] &= ~DisableUSBVcc;
else
m->bcsr[4] |= DisableUSBVcc;
eieio();
m->bcsr[4] &= ~DisableUSB;
}
void
archdisableusb(void)
{
m->bcsr[4] |= DisableUSBVcc | DisableUSB;
}
void
archsetirxcvr(int highspeed)
{
if(!highspeed){
m->bcsr[1] |= DisableIR;
microdelay(2);
}
m->bcsr[1] &= ~DisableIR;
}
void
archreboot(void)
{
IMM *io;
io = m->iomem;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr |= 1<<7;
io->plprcrk = ~KEEP_ALIVE_KEY;
m->iomem->padat &= ~SIBIT(4);
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
m->bcsr[1] = (m->bcsr[1] | PCCVPPHiZ) & ~PCCVPP5V;
m->bcsr[1] |= PCCVCC0V;
m->bcsr[1] &= ~DisablePCMCIA;
m->bcsr[1] &= ~PCCVCC5V;
iopunlock();
}
int
pcmpowered(int)
{
ulong r;
r = ~m->bcsr[1]&PCCVCCMask;
if(r == PCCVCC5V)
return 5;
if(r == PCCVCC3V)
return 3;
return 0;
}
void
pcmsetvcc(int, int v)
{
if(v == 5)
v = PCCVCC5V;
else if(v == 3)
v = PCCVCC3V;
else
v = 0;
ioplock();
m->bcsr[1] = (m->bcsr[1] | PCCVCCMask) & ~v;
iopunlock();
}
void
pcmsetvpp(int, int v)
{
if(v == 5)
v = PCCVPP5V;
else if(v == 12)
v = PCCVPP12V;
else if(v == 0)
v = PCCVPP0V;
else
v = 0;
ioplock();
m->bcsr[1] = (m->bcsr[1] | PCCVPPHiZ) & ~v;
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
IMM *io;
delay(2);
io = ioplock();
io->papar &= ~SIBIT(4);
io->padir |= SIBIT(4);
if(on)
io->padat |= SIBIT(4);
else
io->padat &= ~SIBIT(4);
iopunlock();
}
int
archlcdmode(Mode *m)
{
m->x = 640;
m->y = 480;
m->d = 3;
m->lcd.freq = 25000000;
m->lcd.ac = 0;
m->lcd.vpw = 2;
m->lcd.wbf = 34;
m->lcd.wbl = 106;
m->lcd.flags = IsColour | IsTFT | OELow | HsyncLow | VsyncLow;
m->lcd.notpdpar = SIBIT(6);
return 0;
}
void
archresetvideo(void)
{
ioplock();
m->bcsr[4] &= ~DisableVideoLamp;
m->bcsr[4] |= EnableVideoPort;
eieio();
m->bcsr[4] &= ~EnableVideoPort;
iopunlock();
delay(6);
ioplock();
m->bcsr[4] |= EnableVideoPort;
iopunlock();
delay(6);
}
void
archenablevideo(void)
{
ioplock();
m->bcsr[4] |= EnableVideoClock|EnableVideoPort;
iopunlock();
}
void
archdisablevideo(void)
{
ioplock();
m->bcsr[4] &= ~(EnableVideoClock|EnableVideoPort);
m->bcsr[4] |= DisableVideoLamp;
iopunlock();
}
uchar*
archvideobuffer(long nbytes)
{
if((m->bcsr[1] & EnableSDRAM) == 0){
m->bcsr[1] |= EnableSDRAM;
return KADDR(PHYSSDRAM);
}
return xspanalloc(nbytes, 16, 0);
}
void
archkbdinit(void)
{
}