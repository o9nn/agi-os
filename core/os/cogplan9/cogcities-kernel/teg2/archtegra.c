#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#include "arm.h"
#include "../port/netif.h"
#include "etherif.h"
#include "../port/flashif.h"
#include "../port/usb.h"
#include "../port/portusbehci.h"
#include "usbehci.h"
enum {
Maxcpus		= 4,
Maxflowcpus	= 2,
Debug	= 0,
};
typedef struct Clkrst Clkrst;
typedef struct Diag Diag;
typedef struct Flow Flow;
typedef struct Scu Scu;
typedef struct Power Power;
struct Clkrst {
ulong	rstsrc;
ulong	rstdevl;
ulong	rstdevh;
ulong	rstdevu;
ulong	clkoutl;
ulong	clkouth;
ulong	clkoutu;
uchar	_pad0[0x24-0x1c];
ulong	supcclkdiv;
ulong	_pad1;
ulong	supsclkdiv;
uchar	_pad4[0x4c-0x30];
ulong	clkcpu;
uchar	_pad1[0xe0-0x50];
ulong	pllxbase;
ulong	pllxmisc;
ulong	pllebase;
ulong	pllemisc;
uchar	_pad2[0x340-0xf0];
ulong	cpuset;
ulong	cpuclr;
};
enum {
Wdcpurst =	1<<0,
Wdcoprst =	1<<1,
Wdsysrst =	1<<2,
Wdsel =		1<<4,
Wdena =		1<<5,
Sysreset =	1<<2,
Cpu1stop =	1<<9,
Cpu0stop =	1<<8,
Cpu1dbgreset =	1<<13,
Cpu0dbgreset =	1<<12,
Cpu1wdreset =	1<<9,
Cpu0wdreset =	1<<8,
Cpu1dereset =	1<<5,
Cpu0dereset =	1<<4,
Cpu1reset =	1<<1,
Cpu0reset =	1<<0,
};
struct Power {
ulong	ctl;
ulong	secregdis;
ulong	swrst;
ulong	wakevmask;
ulong	waklvl;
ulong	waksts;
ulong	swwaksts;
ulong	dpdpadsovr;
ulong	dpdsample;
ulong	dpden;
ulong	gatetimroff;
ulong	gatetimron;
ulong	toggle;
ulong	unclamp;
ulong	gatests;
ulong	goodtmr;
ulong	blinktmr;
ulong	noiopwr;
ulong	detect;
ulong	detlatch;
ulong	scratch[24];
ulong	secscratch[6];
ulong	cpupwrgoodtmr;
ulong	cpupwrofftmr;
ulong	pgmask[2];
ulong	autowaklvl;
ulong	autowaklvlmask;
ulong	wakdelay;
ulong	detval;
ulong	ddr;
ulong	usbdebdel;
ulong	usbao;
ulong	cryptoop;
ulong	pllpwb0ovr;
ulong	scratch24[42-24+1];
ulong	boundoutmirr[3];
ulong	sys33ven;
ulong	boundoutmirracc;
ulong	gate;
};
enum {
Start	= 1<<8,
Partpcie= 3,
Partl2	= 4,
};
struct Scu {
ulong	ctl;
ulong	cfg;
ulong	cpupwrsts;
ulong	inval;
uchar	_pad0[0x40-0x10];
ulong	filtstart;
ulong	filtend;
uchar	_pad1[0x50-0x48];
ulong	accctl;
ulong	nsaccctl;
};
enum {
Scuenable =	1<<0,
Filter =	1<<1,
Scuparity =	1<<2,
Specfill =	1<<3,
Allport0 =	1<<4,
Standby =	1<<5,
Icstandby =	1<<6,
};
struct Flow {
ulong	haltcpu0;
ulong	haltcop;
ulong	cpu0;
ulong	cop;
ulong	xrq;
ulong	haltcpu1;
ulong	cpu1;
};
enum {
Stop =	2<<29,
Event =			1<<14,
Waitwfebitsshift =	4,
Waitwfebitsmask =	MASK(2),
Eventenable =		1<<1,
Cpuenable =		1<<0,
};
struct Diag {
Cacheline c0;
Lock;
long	cnt;
long	sync;
Cacheline c1;
};
extern ulong testmem;
int navailcpus;
Isolated l1ptstable;
Soc soc = {
.clkrst	= 0x60006000,
.power	= 0x7000e400,
.exceptvec = PHYSEVP,
.sema	= 0x60001000,
.l2cache= PHYSL2BAG,
.flow	= 0x60007000,
.scu	= 0x50040000,
.intr	= 0x50040100,
.glbtmr	= 0x50040200,
.loctmr	= 0x50040600,
.intrdist=0x50041000,
.uart	= { 0x70006000, 0x70006040,
0x70006200, 0x70006300, 0x70006400, },
.rtc	= 0x7000e000,
.tmr	= { 0x60005000, 0x60005008, 0x60005050, 0x60005058, },
.µs	= 0x60005010,
.pci	= 0x80000000,
.ether	= 0xa0024000,
.nand	= 0x70008000,
.nor	= 0x70009000,
.ehci	= P2VAHB(0xc5000000),
.ide	= P2VAHB(0xc3000000),
.gpio	= { 0x6000d000, 0x6000d080, 0x6000d100, 0x6000d180,
0x6000d200, 0x6000d280, 0x6000d300, },
.spi	= { 0x7000d400, 0x7000d600, 0x7000d800, 0x7000da00, },
.twsi	= 0x7000c000,
.mmc	= { P2VAHB(0xc8000000), P2VAHB(0xc8000200),
P2VAHB(0xc8000400), P2VAHB(0xc8000600), },
};
static volatile Diag diag;
static int missed;
void
dumpcpuclks(void)
{
Clkrst *clk = (Clkrst *)soc.clkrst;
iprint("pllx base %#lux misc %#lux\n", clk->pllxbase, clk->pllxmisc);
iprint("plle base %#lux misc %#lux\n", clk->pllebase, clk->pllemisc);
iprint("super cclk divider %#lux\n", clk->supcclkdiv);
iprint("super sclk divider %#lux\n", clk->supsclkdiv);
}
static char *
devidstr(ulong)
{
return "ARM Cortex-A9";
}
void
archtegralink(void)
{
}
char *
cputype2name(char *buf, int size)
{
ulong r;
r = cpidget();
assert((r >> 24) == 'A');
seprint(buf, buf + size, "Cortex-A9 r%ldp%ld",
(r >> 20) & MASK(4), r & MASK(4));
return buf;
}
static void
errata(void)
{
ulong reg, r, p;
r = cpidget();
assert((r >> 24) == 'A');
p = r & MASK(4);
r >>= 20;
r &= MASK(4);
reg = cprdsc(0, CpDTLB, 0, 1);
if (r < 2 || r == 2 && p <= 2)
reg |= 1<<4;
if (r == 2 && p <= 2)
reg |= 1<<6 | 1<<12 | 1<<22;
if (r < 3)
reg |= 1<<11;
cpwrsc(0, CpDTLB, 0, 1, reg);
}
void
archconfinit(void)
{
char *p;
ulong hz;
assert(m != nil);
m->cpuhz = 1000 * Mhz;
p = getconf("*cpumhz");
if (p) {
hz = atoi(p) * Mhz;
if (hz >= 100*Mhz && hz <= 3600UL*Mhz)
m->cpuhz = hz;
}
m->delayloop = m->cpuhz/2000;
errata();
}
int
archether(unsigned ctlrno, Ether *ether)
{
switch(ctlrno) {
case 0:
ether->type = "rtl8169";
ether->ctlrno = ctlrno;
ether->irq = Pcieirq;
ether->nopt = 0;
ether->mbps = 1000;
return 1;
}
return -1;
}
void
dumpscustate(void)
{
Scu *scu = (Scu *)soc.scu;
print("cpu%d scu: accctl %#lux\n", m->machno, scu->accctl);
print("cpu%d scu: smp cpu bit map %#lo for %ld cpus; ", m->machno,
(scu->cfg >> 4) & MASK(4), (scu->cfg & MASK(2)) + 1);
print("cpus' power %#lux\n", scu->cpupwrsts);
}
void
scuon(void)
{
Scu *scu = (Scu *)soc.scu;
if (scu->ctl & Scuenable)
return;
scu->inval = MASK(16);
coherence();
scu->ctl = Scuparity | Scuenable | Specfill;
coherence();
}
int
getncpus(void)
{
int n;
char *p;
Scu *scu;
if (navailcpus == 0) {
scu = (Scu *)soc.scu;
navailcpus = (scu->cfg & MASK(2)) + 1;
if (navailcpus > MAXMACH)
navailcpus = MAXMACH;
p = getconf("*ncpu");
if (p && *p) {
n = atoi(p);
if (n > 0 && n < navailcpus)
navailcpus = n;
}
}
return navailcpus;
}
void
cpuidprint(void)
{
char name[64];
cputype2name(name, sizeof name);
delay(50);
iprint("cpu%d: %lldMHz ARM %s %s-endian\n",
m->machno, m->cpuhz / Mhz, name,
getpsr() & PsrBigend? "big": "little");
}
static void
clockson(void)
{
Clkrst *clk = (Clkrst *)soc.clkrst;
clk->rstdevl = clk->rstdevh = clk->rstdevu = 0;
coherence();
clk->clkoutl = clk->clkouth = clk->clkoutu = ~0;
coherence();
clk->rstsrc = Wdcpurst | Wdcoprst | Wdsysrst | Wdena;
coherence();
}
void
stopcpu(uint cpu)
{
Flow *flow = (Flow *)soc.flow;
Clkrst *clk = (Clkrst *)soc.clkrst;
if (cpu == 0) {
iprint("stopcpu: may not stop cpu0\n");
return;
}
machoff(cpu);
lock(&active);
active.stopped |= 1 << cpu;
unlock(&active);
l1cache->wb();
flow->haltcop = Stop;
coherence();
flow->cop = 0;
coherence();
delay(10);
assert(cpu < Maxflowcpus);
*(cpu == 0? &flow->haltcpu0: &flow->haltcpu1) = Stop;
coherence();
*(cpu == 0? &flow->cpu0: &flow->cpu1) = 0;
coherence();
delay(10);
assert(cpu < Maxcpus);
clk->cpuset = (Cpu0reset | Cpu0dbgreset | Cpu0dereset) << cpu;
coherence();
delay(1);
l1cache->wb();
}
static void
synccpus(volatile long *cntp, int n)
{
ainc(cntp);
while (*cntp < n)
;
}
static void
pass1(int pass, volatile Diag *dp)
{
int i;
if(m->machno == 0)
iprint(" %d", pass);
for (i = 1000*1000; --i > 0; ) {
ainc(&dp->cnt);
adec(&dp->cnt);
}
synccpus(&dp->sync, navailcpus);
ilock(dp);
if(dp->cnt != 0)
panic("cpu%d: diag: failed w count %ld", m->machno, dp->cnt);
iunlock(dp);
synccpus(&dp->sync, 2 * navailcpus);
adec(&dp->sync);
adec(&dp->sync);
}
void
l1diag(void)
{
int pass;
volatile Diag *dp;
if (!Debug)
return;
l1cache->wb();
dp = &diag;
ilock(dp);
if (m->machno == 0)
iprint("l1: waiting for %d cpus... ", navailcpus);
iunlock(dp);
synccpus(&dp->sync, navailcpus);
ilock(dp);
if (m->machno == 0)
iprint("cache coherency pass");
iunlock(dp);
synccpus(&dp->sync, 2 * navailcpus);
adec(&dp->sync);
adec(&dp->sync);
for (pass = 0; pass < 3; pass++)
pass1(pass, dp);
synccpus(&dp->sync, navailcpus);
if(dp->sync < navailcpus || dp->sync >= 2 * navailcpus)
panic("cpu%d: diag: failed w dp->sync %ld", m->machno,
dp->sync);
if(dp->cnt != 0)
panic("cpu%d: diag: failed w dp->cnt %ld", m->machno,
dp->cnt);
ilock(dp);
iprint(" cpu%d ok", m->machno);
iunlock(dp);
synccpus(&dp->sync, 2 * navailcpus);
adec(&dp->sync);
adec(&dp->sync);
l1cache->wb();
ilock(dp);
if (m->machno == 0)
iprint("\n");
iunlock(dp);
}
static void
unfreeze(uint cpu)
{
Clkrst *clk = (Clkrst *)soc.clkrst;
Flow *flow = (Flow *)soc.flow;
assert(cpu < Maxcpus);
clk->clkcpu &= ~(Cpu0stop << cpu);
coherence();
clk->cpuclr = (Cpu0reset | Cpu0wdreset | Cpu0dbgreset | Cpu0dereset) <<
cpu;
coherence();
assert(cpu < Maxflowcpus);
*(cpu == 0? &flow->cpu0: &flow->cpu1) = 0;
coherence();
*(cpu == 0? &flow->haltcpu0: &flow->haltcpu1) = 0;
coherence();
}
int
startcpu(uint cpu)
{
int i, r;
ulong oldvec, rstaddr;
ulong *evp = (ulong *)soc.exceptvec;
r = 0;
if (getncpus() < 2 || cpu == m->machno ||
cpu >= MAXMACH || cpu >= navailcpus)
return -1;
oldvec = *evp;
l1cache->wb();
*evp = rstaddr = PADDR(_vrst);
coherence();
l1cache->wb();
unfreeze(cpu);
for (i = 2000; i > 0 && *evp == rstaddr; i--)
delay(1);
if (i <= 0 || *evp != cpu) {
iprint("cpu%d: didn't start!\n", cpu);
stopcpu(cpu);
r = -1;
}
*evp = oldvec;
return r;
}
static void
cksecure(void)
{
ulong db;
extern ulong getdebug(void);
if (getscr() & 1)
panic("cpu%d: running non-secure", m->machno);
db = getdebug();
if (db)
iprint("cpu%d: debug enable reg %#lux\n", m->machno, db);
}
ulong
smpon(void)
{
ulong aux;
aux = getauxctl();
putauxctl(aux | CpACsmp | CpACmaintbcast);
return aux;
}
void
cortexa9cachecfg(void)
{
putauxctl(getauxctl() | CpACparity | CpAClwr0line | CpACl2pref);
}
void
cpustart(void)
{
int ms;
ulong *evp;
Power *pwr;
up = nil;
if (active.machs & (1<<m->machno)) {
serialputc('?');
serialputc('r');
panic("cpu%d: resetting after start", m->machno);
}
assert(m->machno != 0);
errata();
cortexa9cachecfg();
memdiag(&testmem);
machinit();
machoff(m->machno);
clockshutdown();
trapinit();
clockinit();
timersinit();
cpuidprint();
evp = (ulong *)soc.exceptvec;
*evp = m->machno;
coherence();
l1diag();
pwr = (Power *)soc.power;
assert(pwr->gatests == MASK(7));
if (Debug)
iprint("cpu%d: waiting for 8169\n", m->machno);
for (ms = 0; !l1ptstable.word && ms < 5000; ms += 10) {
delay(10);
cachedinvse(&l1ptstable.word, sizeof l1ptstable.word);
}
if (!l1ptstable.word)
iprint("cpu%d: 8169 unreasonably slow; proceeding\n", m->machno);
mmuinit();
fpon();
machon(m->machno);
if (Debug)
iprint("cpu%d: scheding\n", m->machno);
schedinit();
panic("cpu%d: schedinit returned", m->machno);
}
void
sgintr(Ureg *ureg, void *)
{
iprint("cpu%d: got sgi\n", m->machno);
if (m->machno != 0)
clockprod(ureg);
}
void
archreset(void)
{
static int beenhere;
if (beenhere)
return;
beenhere = 1;
m->cpuhz = 1000 * Mhz;
m->delayloop = m->cpuhz/2000;
prcachecfg();
clockson();
archconfinit();
fpon();
if (irqtooearly)
panic("archreset: too early for irqenable");
irqenable(Cpu0irq, sgintr, nil, "cpu0");
irqenable(Cpu1irq, sgintr, nil, "cpu1");
}
void
archreboot(void)
{
Clkrst *clk = (Clkrst *)soc.clkrst;
assert(m->machno == 0);
iprint("archreboot: reset!\n");
delay(20);
clk->rstdevl |= Sysreset;
coherence();
delay(500);
splhi();
iprint("awaiting reset");
for(;;) {
delay(1000);
print(".");
}
}
void
kbdinit(void)
{
}
static void
missing(ulong addr, char *name)
{
static int firstmiss = 1;
if (addr == 0) {
iprint("address zero for %s\n", name);
return;
}
if (probeaddr(addr) >= 0)
return;
missed++;
if (firstmiss) {
iprint("missing:");
firstmiss = 0;
} else
iprint(",\n\t");
iprint(" %s at %#lux", name, addr);
}
void
chkmissing(void)
{
delay(10);
missing(KZERO, "dram");
missing(soc.intr, "intr ctlr");
missing(soc.intrdist, "intr distrib");
missing(soc.tmr[0], "tegra timer1");
missing(soc.uart[0], "console uart");
missing(soc.pci, "pcie");
missing(soc.ether, "ether8169");
missing(soc.µs, "µs counter");
if (missed)
iprint("\n");
delay(10);
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
panic("archflashreset: rewrite for nor & nand flash on ts");
f->type = "onenand";
f->addr = (void*)VIRTNOR;
f->size = 0;
f->width = 1;
f->interleave = 0;
return 0;
}