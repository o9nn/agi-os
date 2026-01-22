#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include <draw.h>
#include <memdraw.h>
#include "screen.h"
#include "../port/netif.h"
#include "etherif.h"
#include "../port/flashif.h"
#include	"archipe.h"
enum {
COM3=	IBIT(1)|IBIT(2),
TBS =	IBIT(6),
RTSEL = IBIT(8),
RTDIV = IBIT(7),
CRQEN = IBIT(9),
PRQEN = IBIT(10),
CSRC = IBIT(21),
Showports = 0,
};
static ulong ports[4*4];
void
archinit(void)
{
IMM *io;
int mf;
io = m->iomem;
m->clockgen = 4000000;
m->oscclk = m->clockgen/MHz;
io->plprcrk = KEEP_ALIVE_KEY;
io->plprcr &= ~CSRC;
mf = (io->plprcr >> 20)+1;
io->plprcrk = ~KEEP_ALIVE_KEY;
io->sccrk = KEEP_ALIVE_KEY;
io->sccr |= CRQEN | PRQEN | RTDIV | COM3;
io->sccrk = ~KEEP_ALIVE_KEY;
m->cpuhz = m->clockgen*mf;
m->speed = m->cpuhz/MHz;
if((io->memc[CLOCKCS].base & 1) == 0){
io->memc[CLOCKCS].option = 0xFFFF0F24;
io->memc[CLOCKCS].base = 0xFF020001;
}
if(Showports){
ports[0] = io->padat;
ports[1] = io->padir;
ports[2] = io->papar;
ports[3] = io->paodr;
ports[4] = io->pbdat;
ports[5] = io->pbdir;
ports[6] = io->pbpar;
ports[7] = io->pbodr;
ports[8] = io->pcdat;
ports[9] = io->pcdir;
ports[10] = io->pcpar;
ports[11] = io->pcso;
ports[12] = io->pddat;
ports[13] = io->pddir;
ports[14] = io->pdpar;
ports[15] = 0;
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
ulong pa, nbytes, ktop;
conf.nscc = 2;
conf.sccuarts = 0;
conf.smcuarts = (1<<0)|(1<<1);
nbytes = banksize(DRAMCS, &pa);
if(nbytes == 0){
nbytes = 16*1024*1024;
pa = 0;
}
conf.npage0 = nbytes/BY2PG;
conf.base0 = pa;
conf.npage1 = 0;
ktop = PGROUND((ulong)end);
ktop = PADDR(ktop) - conf.base0;
conf.npage0 -= ktop/BY2PG;
conf.base0 += ktop;
}
void
cpuidprint(void)
{
ulong v;
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
print("%lud MHz system\n", m->cpuhz/MHz);
print("\n");
if(Showports){
print("plprcr=%8.8lux sccr=%8.8lux\n", m->iomem->plprcr, m->iomem->sccr);
print("pipr=%8.8lux\n", m->iomem->pipr);
print("ports:\n");
for(v=0;v<nelem(ports);v++)
print("%2ld %8.8lux\n", v, ports[v]);
for(v=0; v<nelem(m->iomem->memc); v++)
if(m->iomem->memc[v].base & 1)
print("%ld %8.8lux %8.8lux\n", v, m->iomem->memc[v].base, m->iomem->memc[v].option);
}
}
static int
envnameok(char *s)
{
if(*s == '*')
s++;
if(*s >= '0' && *s <= '9' || *s == 0)
return 0;
for(; *s; s++)
if(*s >= '0' && *s <= '9' ||
*s >= 'a' && *s <= 'z' ||
*s >= 'A' && *s <= 'Z' ||
*s == '.' || *s == '_' || *s == '#'){
}else
return 0;
return 1;
}
int
archconfval(char **names, char **vals, int limit)
{
uchar *b, *e;
char *s;
int n, v, l, o;
static char bootargs[512];
b = KADDR(PHYSFLASH+0x4000);
if(*b & 1){
b += 0x2000;
if(*b & 1)
return 0;
}
v = (b[2]<<8)|b[3];
b += 4;
if(v >= 0x2000-4)
return 0;
n = 0;
o = 0;
e = b+v;
for(; b < e; b += v){
v = *b;
if(v == 0xFF || n >= limit)
break;
s = (char*)b+1;
if(v >= 0x80){
v = ((v&0x7F)<<8) | b[1];
s++;
}
if(envnameok(s)){
names[n] = s;
s += strlen(s)+1;
l = strlen(s)+1;
if(o+l > sizeof(bootargs))
break;
vals[n] = bootargs+o;
memmove(vals[n], s, l);
o += l;
n++;
}
}
return n;
}
void
toggleled(int b)
{
int s;
s = splhi();
m->iomem->pdpar &= ~(0x20<<b);
m->iomem->pddir |= 0x20<<b;
m->iomem->pddat ^= 0x020<<b;
splx(s);
}
int
archoptionsw(void)
{
return 0;
}
static void
twinkle(void)
{
if(m->ticks%MS2TK(1000) == 0 && m->iomem)
toggleled(0);
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
f->type = "Intel28F320B3B";
f->addr = KADDR(PHYSFLASH);
f->size = 4*1024*1024;
f->width = 2;
return 0;
}
void
archflashwp(Flash*, int)
{
}
int
archether(int ctlno, Ether *ether)
{
if(isaconfig("ether", ctlno, ether) == 0 && ctlno > 0)
return -1;
if(ctlno == 0){
ether->type = "SCC";
ether->port = 2;
}
memmove(ether->ea, KADDR(PHYSFLASH+0x3FFA), Eaddrlen);
return 1;
}
int
archetherenable(int cpmid, int *rcs, int *tcs, int mbps, int fd)
{
IMM *io;
if(cpmid != CPscc2)
return -1;
USED(mbps);
io = ioplock();
io->pcpar &= ~EnetLoopback;
io->pcso &= ~EnetLoopback;
io->pcdir |= EnetLoopback;
io->pcdat &= ~EnetLoopback;
if(0){
io->pbpar &= ~EnetFullDuplex;
io->pbdir |= EnetFullDuplex;
if(fd)
io->pbdat |= EnetFullDuplex;
else
io->pbdat &= ~EnetFullDuplex;
}
io->pbpar &= ~EnableEnet;
io->pbdir |= EnableEnet;
io->pbdat |= EnableEnet;
io->papar |= SIBIT(7)|SIBIT(6);
io->padir &= ~(SIBIT(7)|SIBIT(6));
iopunlock();
*rcs = CLK2;
*tcs = CLK1;
return 0;
}
static	ulong	uartsactive;
void
archenableuart(int id, int irda)
{
IMM *io;
USED(id);
USED(irda);
io = ioplock();
if(uartsactive == 0){
io->pbpar &= ~EnableRS232;
io->pbdir |= EnableRS232;
io->pbdat |= EnableRS232;
}
uartsactive |= 1<<id;
iopunlock();
}
void
archdisableuart(int id)
{
IMM *io;
io = ioplock();
uartsactive &= ~(1<<id);
if(uartsactive == 0)
io->pbdat &= ~EnableRS232;
iopunlock();
}
void
archenableusb(int highspeed, int master)
{
IMM *io;
USED(master);
io = ioplock();
io->pcpar &= ~USBFullSpeed;
io->pcso &= ~USBFullSpeed;
if(highspeed)
io->pcdat |= USBFullSpeed;
else
io->pcdat &= ~USBFullSpeed;
io->pcdir |= USBFullSpeed;
iopunlock();
}
void
archdisableusb(void)
{
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