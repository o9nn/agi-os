#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"io.h"
#include	"fns.h"
#include	"m8260.h"
enum {
Pin4 = BIT(4),
};
static union {
struct {
ulong	hi;
ulong	lo;
};
uvlong		val;
} ticks;
struct {
ulong	hi;
ulong	lo;
} vec2mask[64]	= {
[0]	= {0,		0	},
[1]	= {0,		BIT(16)	},
[2]	= {0,		BIT(17)	},
[3]	= {0,		BIT(18)	},
[4]	= {0,		BIT(19)	},
[5]	= {0,		BIT(20)	},
[6]	= {0,		BIT(21)	},
[7]	= {0,		BIT(22)	},
[8]	= {0,		BIT(23)	},
[9]	= {0,		BIT(24)	},
[10]	= {0,		BIT(25)	},
[11]	= {0,		0	},
[12]	= {0,		BIT(27)	},
[13]	= {0,		BIT(28)	},
[14]	= {0,		BIT(29)	},
[15]	= {0,		BIT(30)	},
[16]	= {BIT(29),	0	},
[17]	= {BIT(30),	0	},
[18]	= {0,		0	},
[19]	= {BIT(17),	0	},
[20]	= {BIT(18),	0	},
[21]	= {BIT(19),	0	},
[22]	= {BIT(20),	0	},
[23]	= {BIT(21),	0	},
[24]	= {BIT(22),	0	},
[25]	= {BIT(23),	0	},
[26]	= {0,		0	},
[27]	= {0,		0	},
[28]	= {0,		0	},
[29]	= {0,		0	},
[30]	= {0,		0	},
[31]	= {0,		0	},
[32]	= {0,		BIT(0)	},
[33]	= {0,		BIT(1)	},
[34]	= {0,		BIT(2)	},
[35]	= {0,		0	},
[36]	= {0,		BIT(4)	},
[37]	= {0,		BIT(5)	},
[38]	= {0,		0	},
[39]	= {0,		0	},
[40]	= {0,		BIT(8)	},
[41]	= {0,		BIT(9)	},
[42]	= {0,		BIT(10)	},
[43]	= {0,		BIT(11)	},
[44]	= {0,		0	},
[45]	= {0,		0	},
[46]	= {0,		0	},
[47]	= {0,		0	},
[48]	= {BIT(15),	0	},
[49]	= {BIT(14),	0	},
[50]	= {BIT(13),	0	},
[51]	= {BIT(12),	0	},
[52]	= {BIT(11),	0	},
[53]	= {BIT(10),	0	},
[54]	= {BIT(9),	0	},
[55]	= {BIT(8),	0	},
[56]	= {BIT(7),	0	},
[57]	= {BIT(6),	0	},
[58]	= {BIT(5),	0	},
[59]	= {BIT(4),	0	},
[60]	= {BIT(3),	0	},
[61]	= {BIT(2),	0	},
[62]	= {BIT(1),	0	},
[63]	= {BIT(0),	0	},
};
IMM* iomem = (IMM*)IOMEM;
uchar etheraddr[6] = { 0x90, 0x85, 0x82, 0x32, 0x83, 0x00};
static Lock cpmlock;
void
machinit(void)
{
ulong scmr;
int pllmf;
extern char* plan9inistr;
memset(m, 0, sizeof(*m));
m->cputype = getpvr()>>16;
m->imap = (Imap*)INTMEM;
m->loopconst = 1096;
iomem->fcc[0].gfmr &= ~(BIT(27)|BIT(26));
iomem->fcc[1].gfmr &= ~(BIT(27)|BIT(26));
iomem->fcc[2].gfmr &= ~(BIT(27)|BIT(26));
iomem->bank[3].br = 0x04001001;
iomem->bank[6].br = 0xe0001801;
iomem->bank[6].or = 0xff000830;
scmr = iomem->scmr;
m->clkin = CLKIN;
pllmf = scmr & 0xfff;
m->vco_out = m->clkin * (pllmf + 1);
if (scmr & BIT(19))
m->vco_out >>= 1;
m->cpmhz = m->vco_out >> 1;
m->brghz = m->vco_out >> (2 * ((iomem->sccr & 0x3) + 1));
m->bushz = m->vco_out / (((scmr & 0x00f00000) >> 20) + 1);
switch(scmr >> (31-7)){
case 0x0a:
m->cpuhz = m->clkin * 2;
break;
case 0x0b:
m->cpuhz = (m->clkin >> 1) * 5;
break;
default:
case 0x0d:
m->cpuhz = m->clkin * 3;
break;
case 0x14:
m->cpuhz = (m->clkin >> 1) * 7;
break;
case 0x1c:
m->cpuhz = m->clkin * 4;
break;
}
m->cyclefreq = m->bushz / 4;
active.machs = 1;
active.exiting = 0;
putmsr(getmsr() | MSR_ME);
icacheenb();
dcacheenb();
kfpinit();
if (PLAN9INI == ~0 ||
(uchar)*(plan9inistr = (char*)(FLASHMEM+PLAN9INI)) == 0xff){
plan9inistr =
"console=0\n"
"ether0=type=fcc port=0 ea=00601d051dd8\n"
"flash0=mem=0xfe000000\n"
"fs=135.104.9.42\n"
"auth=135.104.9.7\n"
"authdom=cs.bell-labs.com\n"
"sys=blast\n"
"ntp=135.104.9.52\n";
}
}
void
fpgareset(void)
{
print("fpga reset\n");
ioplock();
iomem->port[1].pdat &= ~Pin4;
delay(100);
iomem->port[1].pdat |= Pin4;
iopunlock();
}
void
hwintrinit(void)
{
iomem->sicr = 2 << 8;
iomem->sipnr_h = ~7;
iomem->sipnr_h = ~1;
iomem->simr_h = 0;
iomem->simr_l = 0;
iomem->sypcr &= ~2;
iomem->port[1].pdir |= Pin4;
iomem->port[1].ppar &= ~Pin4;
iomem->port[1].pdat |= Pin4;
}
int
vectorenable(Vctl *v)
{
ulong hi, lo;
if (v->irq & ~0x3f){
print("m8260enable: interrupt vector %d out of range\n", v->irq);
return -1;
}
hi = vec2mask[v->irq].hi;
lo = vec2mask[v->irq].lo;
if (hi == 0 && lo == 0){
print("m8260enable: nonexistent vector %d\n", v->irq);
return -1;
}
ioplock();
iomem->sipnr_h |= hi;
iomem->sipnr_l |= lo;
iomem->simr_h |= hi;
iomem->simr_l |= lo;
iopunlock();
return v->irq;
}
void
vectordisable(Vctl *v)
{
ulong hi, lo;
if (v->irq & ~0x3f){
print("m8260disable: interrupt vector %d out of range\n", v->irq);
return;
}
hi = vec2mask[v->irq].hi;
lo = vec2mask[v->irq].lo;
if (hi == 0 && lo == 0){
print("m8260disable: nonexistent vector %d\n", v->irq);
return;
}
ioplock();
iomem->simr_h &= ~hi;
iomem->simr_l &= ~lo;
iopunlock();
}
int
intvec(void)
{
return iomem->sivec >> 26;
}
void
intend(int vno)
{
ioplock();
iomem->sipnr_h |= vec2mask[vno].hi;
iomem->sipnr_l |= vec2mask[vno].lo;
iopunlock();
}
int
m8260eoi(int)
{
return 0;
}
int
m8260isr(int)
{
return 0;
}
void
flashprogpower(int)
{
}
enum {
TgcrCas 			= 0x80,
TgcrGm 			= 0x08,
TgcrStp 			= 0x2,
TgcrRst 			= 0x1,
TmrIclkCasc		= 0x00<<1,
TmrIclkIntclock	= 0x01<<1,
TmrIclkIntclock16	= 0x02<<1,
TmrIclkTin		= 0x03<<1,
TmrCERising		= 0x1 << 6,
TmrCEFalling		= 0x2 << 6,
TmrCEAny		= 0x3 << 6,
TmrFrr			= SBIT(12),
TmrOri			= SBIT(11),
TerRef			= SBIT(14),
TerCap			= SBIT(15),
};
uvlong
fastticks(uvlong *hz)
{
ulong count;
static Lock fasttickslock;
if (hz)
*hz = m->clkin>>1;
ilock(&fasttickslock);
count = iomem->tcnl1;
if (count < ticks.lo)
ticks.hi += 1;
ticks.lo = count;
iunlock(&fasttickslock);
return ticks.val;
}
ulong multiplier;
ulong
µs(void)
{
uvlong x;
if(multiplier == 0){
multiplier = (uvlong)(1000000LL << 16) / m->cyclefreq;
print("µs: multiplier %ld, cyclefreq %lld, shifter %d\n", multiplier, m->cyclefreq, 16);
}
cycles(&x);
return (x*multiplier) >> 16;
}
void
timerset(Tval next)
{
long offset;
uvlong now;
static int cnt;
now = fastticks(nil);
offset = next - now;
if (offset < 2500)
next = now + 2500;
else if (offset > m->clkin / HZ){
print("too far in the future: offset %llux, now %llux\n", next, now);
next = now + m->clkin / HZ;
}
iomem->trrl1 = next;
}
void
m8260timerintr(Ureg *u, void*)
{
iomem->ter2 |= TerRef | TerCap;
timerintr(u, 0);
}
void
timerinit(void)
{
iomem->tgcr1 = TgcrCas | TgcrGm;
iomem->tcnl1 = 0;
iomem->trrl1 = m->clkin / HZ;
iomem->tmr1 = TmrIclkCasc;
iomem->tmr2 = TmrIclkIntclock | TmrOri;
intrenable(13, m8260timerintr, nil, "timer");
iomem->tgcr1 |= TgcrRst << 4;
}
static void
addseg(char *name, ulong start, ulong length)
{
Physseg segbuf;
memset(&segbuf, 0, sizeof(segbuf));
segbuf.attr = SG_PHYSICAL;
kstrdup(&segbuf.name, name);
segbuf.pa = start;
segbuf.size = length;
if (addphysseg(&segbuf) == -1) {
print("addphysseg: %s\n", name);
return;
}
}
void
sharedseginit(void)
{
int i, j;
ulong base, size;
char name[16], *a, *b, *s;
static char *segnames[] = {
"fpga",
"dsp",
};
for (j = 0; j < nelem(segnames); j++){
for (i = 0; i < 8; i++){
snprint(name, sizeof name, "%s%d", segnames[j], i);
if ((a = getconf(name)) == nil)
continue;
if ((b = strstr(a, "mem=")) == nil){
print("blastseginit: %s: no base\n", name);
continue;
}
b += 4;
base = strtoul(b, nil, 0);
if (base == 0){
print("blastseginit: %s: bad base: %s\n", name, b);
continue;
}
if ((s = strstr(a, "size=")) == nil){
print("blastseginit: %s: no size\n", name);
continue;
}
s += 5;
size = strtoul(s, nil, 0);
if (size == 0){
print("blastseginit: %s: bad size: %s\n", name, s);
continue;
}
addseg(name, base, size);
}
}
}
void
cpmop(int op, int dev, int mcn)
{
ioplock();
eieio();
while(iomem->cpcr & 0x10000)
eieio();
iomem->cpcr = dev<<(31-10) | mcn<<(31-25) | op | 0x10000;
eieio();
while(iomem->cpcr & 0x10000)
eieio();
iopunlock();
}
void
sccnmsi(int x, int rcs, int tcs)
{
ulong v;
int sh;
sh = (x-1)*8;
v = (((rcs&7)<<3) | (tcs&7)) << sh;
iomem->sicr = (iomem->sicr & ~(0xFF<<sh)) | v;
}
void
ioplock(void)
{
ilock(&cpmlock);
}
void
iopunlock(void)
{
eieio();
iunlock(&cpmlock);
}
BD*
bdalloc(int n)
{
static BD *palloc = ((Imap*)INTMEM)->bd;
BD *p;
p = palloc;
if (palloc > ((Imap*)INTMEM)->bd + nelem(((Imap*)INTMEM)->bd)){
print("bdalloc: out of BDs\n");
return nil;
}
palloc += n;
return p;
}
int
ioringinit(Ring* r, int nrdre, int ntdre, int bufsize)
{
int i, x;
static uchar *dpmallocaddr;
static uchar *dpmallocend;
if (dpmallocaddr == nil){
dpmallocaddr = m->imap->dpram1;
dpmallocend = dpmallocaddr + sizeof(m->imap->dpram1);
}
r->nrdre = nrdre;
if(r->rdr == nil)
r->rdr = xspanalloc(nrdre*sizeof(BD), 0, 8);
if(r->rdr == nil)
return -1;
if(r->rrb == nil && bufsize){
r->rrb = xspanalloc(nrdre*bufsize, 0, CACHELINESZ);
if(r->rrb == nil)
return -1;
}
x = bufsize ? PADDR(r->rrb) : 0;
for(i = 0; i < nrdre; i++){
r->rdr[i].length = 0;
r->rdr[i].addr = x;
r->rdr[i].status = BDEmpty|BDInt;
x += bufsize;
}
r->rdr[i-1].status |= BDWrap;
r->rdrx = 0;
r->ntdre = ntdre;
if(r->tdr == nil)
r->tdr = xspanalloc(ntdre*sizeof(BD), 0, 8);
if(r->txb == nil)
r->txb = xspanalloc(ntdre*sizeof(Block*), 0, CACHELINESZ);
if(r->tdr == nil || r->txb == nil)
return -1;
for(i = 0; i < ntdre; i++){
r->txb[i] = nil;
r->tdr[i].addr = 0;
r->tdr[i].length = 0;
r->tdr[i].status = 0;
}
r->tdr[i-1].status |= BDWrap;
r->tdrh = 0;
r->tdri = 0;
r->ntq = 0;
return 0;
}
void
trapinit(void)
{
int i;
for(i = 0x0; i < 0x2000; i += 0x100)
sethvec(i, trapvec);
setmvec(0x1000, imiss, tlbvec);
setmvec(0x1100, dmiss, tlbvec);
setmvec(0x1200, dmiss, tlbvec);
dcflush(KADDR(0), 0x2000);
icflush(KADDR(0), 0x2000);
putmsr(getmsr() & ~MSR_IP);
}
void
reboot(void*, void*, ulong)
{
ulong *p;
int x;
p = (ulong*)0x90000000;
x = splhi();
iomem->sypcr |= 0xc0;
print("iomem->sypcr = 0x%lux\n", iomem->sypcr);
*p = 0;
print("still alive\n");
splx(x);
}