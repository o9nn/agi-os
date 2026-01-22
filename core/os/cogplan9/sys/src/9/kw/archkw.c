#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#include "../port/netif.h"
#include "etherif.h"
#include "../port/flashif.h"
#include "arm.h"
enum {
L2writeback = 1,
Debug = 0,
};
typedef struct GpioReg GpioReg;
struct GpioReg {
ulong dataout;
ulong dataoutena;
ulong blinkena;
ulong datainpol;
ulong datain;
ulong intrcause;
ulong intrmask;
ulong intrlevelmask;
};
typedef struct L2uncache L2uncache;
typedef struct L2win L2win;
struct L2uncache {
struct L2win {
ulong base;
ulong size;
} win[4];
};
enum {
L2enable = 1<<0,
};
typedef struct Dramctl Dramctl;
struct Dramctl {
ulong ctl;
ulong ddrctllo;
struct {
ulong lo;
ulong hi;
} time;
ulong addrctl;
ulong opagectl;
ulong oper;
ulong mode;
ulong extmode;
ulong ddrctlhi;
ulong ddr2timelo;
ulong operctl;
struct {
ulong lo;
ulong hi;
} mbusctl;
ulong mbustimeout;
ulong ddrtimehi;
ulong sdinitctl;
ulong extsdmode1;
ulong extsdmode2;
struct {
ulong lo;
ulong hi;
} odtctl;
ulong ddrodtctl;
ulong rbuffsel;
ulong accalib;
ulong dqcalib;
ulong dqscalib;
};
typedef struct SDramdReg SDramdReg;
struct SDramdReg {
struct {
ulong base;
ulong size;
} win[4];
};
typedef struct Addrmap Addrmap;
typedef struct Addrwin Addrwin;
struct Addrmap {
struct Addrwin {
ulong ctl;
ulong base;
ulong remaplo;
ulong remaphi;
} win[8];
ulong dirba;
};
Soc soc = {
.cpu = PHYSIO+0x20100,
.devid = PHYSIO+0x10034,
.l2cache = PHYSIO+0x20a00,
.sdramc = PHYSIO+0x01400,
.iocfg = PHYSIO+0x100e0,
.addrmap = PHYSIO+0x20000,
.intr = PHYSIO+0x20200,
.nand = PHYSIO+0x10418,
.cesa = PHYSIO+0x30000,
.ehci = PHYSIO+0x50000,
.spi = PHYSIO+0x10600,
.twsi = PHYSIO+0x11000,
.analog = PHYSIO+0x1007c,
.pci = PHYSIO+0x40000,
.pcibase = PHYSIO+0x41800,
.rtc = PHYSIO+0x10300,
.clock = PHYSIO+0x20300,
.ether = { PHYSIO+0x72000, PHYSIO+0x76000, },
.sata = { PHYSIO+0x80000,
PHYSIO+0x82000,
PHYSIO+0x84000,
},
.uart = { PHYSIO+0x12000, PHYSIO+0x12100, },
.gpio = { PHYSIO+0x10100, PHYSIO+0x10140, },
};
#define WINTARG(ctl) (((ctl) >> 4) & 017)
#define WINATTR(ctl) (((ctl) >> 8) & 0377)
#define WIN64KSIZE(ctl) (((ctl) >> 16) + 1)
static void
praddrwin(Addrwin *win, int i)
{
ulong ctl, targ, attr, size64k;
if (!Debug) {
USED(win, i);
return;
}
ctl = win->ctl;
targ = WINTARG(ctl);
attr = WINATTR(ctl);
size64k = WIN64KSIZE(ctl);
print("cpu addr map: %s window %d: targ %ld attr %#lux size %,ld addr %#lux",
ctl & Winenable? "enabled": "disabled", i, targ, attr,
size64k * 64*1024, win->base);
if (i < 4)
print(" remap addr %#llux", (uvlong)win->remaphi<<32 |
win->remaplo);
print("\n");
}
static void
fixaddrmap(void)
{
int i;
ulong ctl, targ, attr, size64k;
Addrmap *map;
Addrwin *win;
map = (Addrmap *)soc.addrmap;
for (i = 0; i < nelem(map->win); i++) {
win = &map->win[i];
ctl = win->ctl;
targ = WINTARG(ctl);
attr = WINATTR(ctl);
size64k = WIN64KSIZE(ctl);
USED(attr, size64k);
if (targ == Targcesasram) {
win->ctl |= Winenable;
win->base = PHYSCESASRAM;
coherence();
praddrwin(win, i);
}
}
if (map->dirba != PHYSIO)
panic("dirba not %#ux", PHYSIO);
}
static void
praddrmap(void)
{
int i;
Addrmap *map;
map = (Addrmap *)soc.addrmap;
for (i = 0; i < nelem(map->win); i++)
praddrwin(&map->win[i], i);
}
int
ispow2(uvlong ul)
{
return (ul & (ul - 1)) == 0;
}
int
log2(ulong n)
{
int i;
i = 31 - clz(n);
if (!ispow2(n) || n == 0)
i++;
return i;
}
void
cacheinfo(int level, int kind, Memcache *cp)
{
uint len, assoc, size;
ulong setsways;
setsways = cprdsc(0, CpID, CpIDidct, CpIDct);
cp->level = level;
cp->kind = kind;
if ((setsways & (1<<24)) == 0)
kind = Unified;
if (kind != Icache)
setsways >>= 12;
assoc = (setsways >> 3) & MASK(3);
cp->nways = 1 << assoc;
size = (setsways >> 6) & MASK(4);
cp->size = 1 << (size + 9);
len = setsways & MASK(2);
cp->log2linelen = len + 3;
cp->linelen = 1 << cp->log2linelen;
cp->setsways = setsways;
cp->nsets = 1 << (size + 6 - assoc - len);
cp->setsh = cp->log2linelen;
cp->waysh = 32 - log2(cp->nways);
}
static char *
wbtype(uint type)
{
static char *types[] = {
"write-through",
"read data block",
"reg 7 ops, no lock-down",
[06] "reg 7 ops, format A",
[07] "reg 7 ops, format B deprecated",
[016] "reg 7 ops, format C",
[05] "reg 7 ops, format D",
};
if (type >= nelem(types) || types[type] == nil)
return "GOK";
return types[type];
}
static void
prcache(Memcache *mcp)
{
int type;
char id;
if (mcp->kind == Unified)
id = 'U';
else if (mcp->kind == Icache)
id = 'I';
else if (mcp->kind == Dcache)
id = 'D';
else
id = '?';
print("l%d %c: %d bytes, %d ways %d sets %d bytes/line",
mcp->level, id, mcp->size, mcp->nways, mcp->nsets,
mcp->linelen);
if (mcp->linelen != CACHELINESZ)
print(" *should* be %d", CACHELINESZ);
type = (mcp->setsways >> 25) & MASK(4);
if (type == 0)
print("; write-through only");
else
print("; write-back type `%s' (%#o) possible",
wbtype(type), type);
if (mcp->setsways & (1<<11))
print("; page table mapping restrictions apply");
if (mcp->setsways & (1<<2))
print("; M bit is set in cache type reg");
print("\n");
}
static void
prcachecfg(void)
{
Memcache mc;
cacheinfo(1, Dcache, &mc);
prcache(&mc);
cacheinfo(1, Icache, &mc);
prcache(&mc);
}
void
l2cacheon(void)
{
ulong cfg;
CpucsReg *cpu;
L2uncache *l2p;
cacheuwbinv();
l2cacheuwbinv();
l1cachesoff();
cpwrsc(CpDef, CpCLD, 0, 0, 0);
cpu = (CpucsReg *)soc.cpu;
cfg = cpu->cpucfg | L2exists | L2ecc | Cfgiprefetch | Cfgdprefetch;
if (L2writeback)
cfg &= ~L2writethru;
else
cfg |= L2writethru;
cpu->l2cfg = cfg;
coherence();
cpu->l2tm1 = cpu->l2tm0 = 0x66666666;
coherence();
cpwrsc(CpL2, CpTESTCFG, CpTCl2waylck, CpTCl2waylock, 0);
cachedinv();
l2cacheuinv();
l2p = (L2uncache *)soc.l2cache;
memset(l2p, 0, sizeof *l2p);
l2p->win[0].base = 0x80000000 | L2enable;
l2p->win[0].size = (32*1024-1) << 16;
coherence();
l2cachecfgon();
l1cacheson();
print("l2 cache: 256K or 512K: 4 ways, 32-byte lines, write-%s, sdram only\n",
cpu->l2cfg & L2writethru? "through": "back");
}
void
archconfinit(void)
{
m->cpuhz = Frequency;
m->delayloop = m->cpuhz/2000;
fixaddrmap();
if (Debug)
praddrmap();
prcachecfg();
l2cacheon();
}
void
archkwlink(void)
{
}
int
archether(unsigned ctlno, Ether *ether)
{
if(ctlno >= 2)
return -1;
ether->type = "88e1116";
ether->port = ctlno;
return 1;
}
enum {
KWOEValHigh = 1<<(49-32),
KWOEValLow = 1<<29,
KWOELow = ~0,
KWOEHigh = ~0,
};
void
archreset(void)
{
ulong clocks;
CpucsReg *cpu;
Dramctl *dram;
GpioReg *gpio;
clockshutdown();
gpio = (GpioReg*)soc.gpio[0];
gpio->dataout = KWOEValLow;
coherence();
gpio->dataoutena = KWOELow;
gpio = (GpioReg*)soc.gpio[1];
gpio->dataout = KWOEValHigh;
coherence();
gpio->dataoutena = KWOEHigh;
coherence();
cpu = (CpucsReg *)soc.cpu;
cpu->mempm = 0;
coherence();
clocks = MASK(10);
clocks |= MASK(21) & ~MASK(14);
clocks &= ~(1<<18 | 1<<1);
cpu->clockgate |= clocks;
cpu->l2cfg |= L2exists;
coherence();
dram = (Dramctl *)soc.sdramc;
dram->ddrctllo &= ~(1<<6);
*(ulong *)soc.analog = 0x68;
coherence();
}
void
archreboot(void)
{
CpucsReg *cpu;
iprint("reset!\n");
delay(10);
cpu = (CpucsReg *)soc.cpu;
cpu->rstout = RstoutSoft;
cpu->softreset = ResetSystem;
coherence();
cpu->cpucsr = Reset;
coherence();
delay(500);
splhi();
iprint("waiting...");
for(;;)
idlehands();
}
void
archconsole(void)
{
}
void
archflashwp(Flash*, int)
{
}
int flashat(Flash *f, uintptr pa);
int
archflashreset(int bank, Flash *f)
{
if(bank != 0)
return -1;
f->type = "nand";
if (flashat(f, PHYSNAND1))
f->addr = (void*)PHYSNAND1;
else if (flashat(f, PHYSNAND2))
f->addr = (void*)PHYSNAND2;
else
f->addr = nil;
f->size = 0;
f->width = 1;
f->interleave = 0;
return 0;
}