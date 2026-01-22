#include	"u.h"
#include	"lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"archfads.h"
enum {
USESDRAM = 0,
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
int mf, isfads, sysmhz, t;
ulong v;
v = getimmr() & 0xFFFF;
isfads = 0;
sysmhz = 40;
switch(v>>8){
case 0x00:	t = 0x86000; break;
case 0x20:	t = 0x82300; isfads = 1; break;
case 0x21:	t = 0x823a0; isfads = 1; break;
default:	t = 0; break;
}
m->cputype = t;
m->bcsr = KADDR(BCSRMEM);
m->bcsr[1] |= DisableRS232a | DisableIR | DisableEther | DisablePCMCIA | DisableRS232b;
m->bcsr[1] &= ~(DisableDRAM|DisableFlash);
if(isfads){
sysmhz = 50;
m->bcsr[1] &= ~EnableSDRAM;
m->bcsr[4] &= ~(EnableVideoClock|EnableVideoPort);
m->bcsr[4] |= DisableVideoLamp;
}
io = m->iomem;
if(1 || io->sccr & IBIT(7)){
if((m->bcsr[2]>>19)&(1<<2))
m->clockgen = 5*MHz;
else
m->clockgen = 4*MHz;
} else
m->clockgen = 32768;
mf = (sysmhz*MHz)/m->clockgen;
m->cpuhz = m->clockgen*mf;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr &= ~IBIT(21);
io->plprcr = (io->plprcr & ((1<<20)-1)) | ((mf-1)<<20);
io->mptpr = 0x0400;
io->plprcrk = ~KEEP_ALIVE_KEY;
if(isfads){
m->bcsr[1] |= EnableSDRAM;
sdraminit(SDRAMMEM);
if(!USESDRAM)
m->bcsr[1] &= ~EnableSDRAM;
}
}
static void
archidprint(void)
{
int f, i;
ulong v;
print("IMMR: ");
v = getimmr() & 0xFFFF;
switch(v>>8){
case 0x00:	print("MPC860/821"); break;
case 0x20:	print("MPC823"); break;
case 0x21:	print("MPC823A"); break;
default:	print("Type #%lux", v>>8); break;
}
print(", mask #%lux\n", v&0xFF);
v = m->bcsr[3]>>16;
print("MPC8xxFADS rev %lud, DB: ", ((v>>4)&8)|((v>>1)&4)|(v&3));
f = (v>>8)&0x3F;
switch(f){
default:	print("ID#%x", f); break;
case 0x00:	print("MPC860/821"); break;
case 0x01:	print("MPC813"); break;
case 0x02:	print("MPC821"); break;
case 0x03:	print("MPC823"); break;
case 0x20:	print("MPC801"); break;
case 0x21:	print("MPC850"); break;
case 0x22:	print("MPC860"); break;
case 0x23:	print("MPC860SAR"); break;
case 0x24:	print("MPC860T"); break;
}
print("ADS, rev #%lux\n", (m->bcsr[2]>>16)&7);
for(i=0; i<=4; i++)
print("BCSR%d: %8.8lux\n", i, m->bcsr[i]);
v = m->bcsr[2];
f = (v>>28)&0xF;
switch(f){
default:	print("Unknown"); break;
case 4:	print("SM732A2000/SM73228 - 8M SIMM"); break;
case 5:	print("SM732A1000A/SM73218 - 4M SIMM"); break;
case 6:	print("MCM29080 - 8M SIMM"); break;
case 7:	print("MCM29040 - 4M SIMM"); break;
case 8:	print("MCM29020 - 2M SIMM"); break;
}
switch((m->bcsr[3]>>20)&7){
default:	i = 0; break;
case 1:	i = 150; break;
case 2:	i = 120; break;
case 3:	i = 90; break;
}
print(" flash, %dns\n", i);
f = (v>>23)&0xF;
switch(f&3){
case 0:	i = 4; break;
case 1:	i = 32; break;
case 2:	i = 16; break;
case 3:	i = 8; break;
}
print("%dM SIMM, ", i);
switch(f>>2){
default: 	i = 0; break;
case 2:	i = 70; break;
case 3:	i = 60; break;
}
print("%dns\n", i);
print("options: #%lux\n", (m->bcsr[2]>>19)&0xF);
print("plprcr=%8.8lux sccr=%8.8lux\n", m->iomem->plprcr, m->iomem->sccr);
}
void
cpuidprint(void)
{
int t;
print("PVR: ");
t = getpvr()>>16;
switch(t){
case 0x01:	print("MPC601"); break;
case 0x03:	print("MPC603"); break;
case 0x04:	print("MPC604"); break;
case 0x06:	print("MPC603e"); break;
case 0x07:	print("MPC603e-v7"); break;
case 0x50:	print("MPC8xx"); break;
default:	print("PowerPC version #%x", t); break;
}
print(", revision #%lux\n", getpvr()&0xffff);
archidprint();
print("%lud MHz system\n", m->cpuhz/MHz);
print("\n");
}
static	char*	defplan9ini[2] = {
"ether0=type=SCC port=1 ea=00108bf12900\r\n"
"vgasize=640x480x8\r\n"
"kernelpercent=40\r\n"
"console=0 lcd\r\nbaud=9600\r\n",
"ether0=type=SCC port=2 ea=00108bf12900\r\n"
"vgasize=640x480x8\r\n"
"kernelpercent=40\r\n"
"console=0 lcd\r\nbaud=9600\r\n",
};
char *
archconfig(void)
{
print("Using default configuration\n");
return defplan9ini[MPCMODEL(m->cputype) == 0x823];
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
void	(*archclocktick)(void) = twinkle;
int
archflashreset(char *type, void **addr, long *length)
{
char *t;
int mbyte;
if((m->iomem->memc[0].base & 1) == 0)
return -1;
switch((m->bcsr[2]>>28)&0xF){
default:	return -1;
case 4:	mbyte=8; t = "SM732x8"; break;
case 5:	mbyte=4; t = "SM732x8"; break;
case 6:	mbyte=8; t = "AMD29F0x0"; break;
case 7:	mbyte=4; t = "AMD29F0x0"; break;
case 8:	mbyte=2; t = "AMD29F0x0"; break;
}
strcpy(type, t);
*addr = KADDR(FLASHMEM);
*length = mbyte*1024*1024;
return 0;
}
int
archetherenable(int cpmid, int *rcs, int *tcs)
{
IMM *io;
switch(cpmid){
default:
return -1;
case SCC2ID:
io = ioplock();
m->bcsr[1] |= DisableIR|DisableRS232b;
m->bcsr[1] &= ~DisableEther;
io->papar |= SIBIT(6)|SIBIT(5);
io->padir &= ~(SIBIT(6)|SIBIT(5));
*rcs = CLK2;
*tcs = CLK3;
iopunlock();
break;
case SCC1ID:
io = ioplock();
m->bcsr[1] |= DisableIR|DisableRS232b;
m->bcsr[1] &= ~DisableEther;
io->papar |= SIBIT(6)|SIBIT(7);
io->padir &= ~(SIBIT(6)|SIBIT(7));
io->pcpar &= ~(SIBIT(4)|SIBIT(5)|SIBIT(6));
io->pcdir |= SIBIT(4)|SIBIT(5)|SIBIT(6);
io->pcdat &= ~SIBIT(4);
io->pcdat |= SIBIT(5)|SIBIT(6);
*rcs = CLK2;
*tcs = CLK1;
iopunlock();
break;
}
return 0;
}
void
archetherdisable(int id)
{
USED(id);
m->bcsr[1] |= DisableEther|DisableIR|DisableRS232b;
}
void
archenableuart(int id, int irda)
{
switch(id){
case SMC1ID:
m->bcsr[1] &= ~DisableRS232a;
break;
case SCC2ID:
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
case SMC1ID:
m->bcsr[1] |= DisableRS232a;
break;
case SCC2ID:
m->bcsr[1] |= DisableIR|DisableRS232b;
break;
default:
break;
}
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