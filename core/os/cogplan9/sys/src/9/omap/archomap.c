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
#define FREQSEL(x) ((x) << 4)
typedef struct Cm Cm;
typedef struct Cntrl Cntrl;
typedef struct Gen Gen;
typedef struct Gpio Gpio;
typedef struct L3agent L3agent;
typedef struct L3protreg L3protreg;
typedef struct L3regs L3regs;
typedef struct Prm Prm;
typedef struct Usbotg Usbotg;
typedef struct Usbtll Usbtll;
struct Usbotg {
uchar	faddr;
uchar	power;
ushort	intrtx;
ushort	intrrx;
ushort	intrtxe;
ushort	intrrxe;
uchar	intrusb;
uchar	intrusbe;
ushort	frame;
uchar	index;
uchar	testmode;
uchar	_pad0[0x400 - 0x10];
ulong	otgrev;
ulong	otgsyscfg;
ulong	otgsyssts;
ulong	otgifcsel;
uchar	_pad1[0x414 - 0x410];
ulong	otgforcestdby;
};
enum {
Hsen		= 1<<5,
Forcehost	= 1<<7,
Forcehs		= 1<<4,
Midle		= 1<<12,
Sidle		= 1<<3,
};
struct Usbtll {
ulong	revision;
uchar	_pad0[0x10-0x4];
ulong	sysconfig;
ulong	sysstatus;
ulong	irqstatus;
ulong	irqenable;
};
enum {
Softreset	= 1<<1,
Resetdone	= 1<<0,
Ehci_resetdone	= 1<<2,
Ohci_resetdone	= 1<<1,
};
struct L3protreg {
uvlong	req_info_perm;
uvlong	read_perm;
uvlong	write_perm;
uvlong	addr_match;
};
enum {
Permusbhost	= 1<<9,
Permusbotg	= 1<<4,
Permsysdma	= 1<<3,
Permmpu		= 1<<1,
};
struct L3agent {
uchar	_pad0[0x20];
uvlong	ctl;
uvlong	sts;
uchar	_pad1[0x58 - 0x30];
uvlong	errlog;
uvlong	errlogaddr;
};
struct L3regs {
L3protreg *base;
int	upper;
char	*name;
};
L3regs l3regs[] = {
(L3protreg *)(PHYSL3GPMCPM+0x48), 7, "gpmc",
(L3protreg *)(PHYSL3PMRT+0x48), 1, "rt",
(L3protreg *)(PHYSL3OCTRAM+0x48), 7, "ocm ram",
(L3protreg *)(PHYSL3OCTROM+0x48), 1, "ocm rom",
(L3protreg *)(PHYSL3MAD2D+0x48), 7, "mad2d",
(L3protreg *)(PHYSL3IVA+0x48), 3, "iva2.2",
};
struct Cm {
ulong	fclken;
ulong	fclken2;
ulong	fclken3;
uchar	_pad0[0x10 - 0xc];
ulong	iclken;
ulong	iclken2;
ulong	iclken3;
uchar	_pad1[0x20 - 0x1c];
ulong	idlest;
ulong	idlest2;
ulong	idlest3;
uchar	_pad2[0x30 - 0x2c];
ulong	autoidle;
ulong	autoidle2;
ulong	autoidle3;
uchar	_pad3[0x40 - 0x3c];
union {
ulong	clksel[5];
struct unused {
ulong	sleepdep;
ulong	clkstctrl;
ulong	clkstst;
};
uchar	_pad4[0x70 - 0x40];
};
ulong	clkoutctrl;
};
struct Prm {
uchar	_pad[0x50];
ulong	rstctrl;
};
struct Gpio {
ulong	_pad0[4];
ulong	sysconfig;
ulong	sysstatus;
ulong	irqsts1;
ulong	irqen1;
ulong	wkupen;
ulong	_pad1;
ulong	irqsts2;
ulong	irqen2;
ulong	ctrl;
ulong	oe;
ulong	datain;
ulong	dataout;
ulong	lvldet0;
ulong	lvldet1;
ulong	risingdet;
ulong	fallingdet;
ulong	deben;
ulong	debtime;
ulong	_pad2[2];
ulong	clrirqen1;
ulong	setirqen1;
ulong	_pad3[2];
ulong	clrirqen2;
ulong	setirqen2;
ulong	_pad4[2];
ulong	clrwkupen;
ulong	setwkupen;
ulong	_pad5[2];
ulong	clrdataout;
ulong	setdataout;
};
enum {
Wkusimocp	= 1 << 9,
Wkwdt2		= 1 << 5,
Wkgpio1		= 1 << 3,
Wkgpt1		= 1 << 0,
Dssl3l4		= 1 << 0,
Dsstv		= 1 << 2,
Dss2		= 1 << 1,
Dss1		= 1 << 0,
Pergpio6	= 1 << 17,
Pergpio5	= 1 << 16,
Pergpio4	= 1 << 15,
Pergpio3	= 1 << 14,
Pergpio2	= 1 << 13,
Perwdt3		= 1 << 12,
Peruart3	= 1 << 11,
Pergpt9		= 1 << 10,
Pergpt8		= 1 << 9,
Pergpt7		= 1 << 8,
Pergpt6		= 1 << 7,
Pergpt5		= 1 << 6,
Pergpt4		= 1 << 5,
Pergpt3		= 1 << 4,
Pergpt2		= 1 << 3,
Perenable	= Pergpio6 | Pergpio5 | Perwdt3 | Pergpt2 | Peruart3,
Usbhost2	= 1 << 1,
Usbhost1	= 1 << 0,
Usbhost		= Usbhost1,
Usbhostidle	= 1 << 1,
Usbhoststdby	= 1 << 0,
Coreusbhsotg	= 1 << 4,
Core3usbtll	= 1 << 2,
Coreusbhsotgidle = 1 << 5,
Coreusbhsotgstdby= 1 << 4,
Dplllock	= 7,
Dplllocked	= 1,
Dpllbypassed	= 0,
Gpio1idle	= 1 << 3,
Dssidle		= 1 << 1,
Gpio1vidmagic	= 1<<24 | 1<<8 | 1<<5,
};
enum {
Rstgs		= 1 << 1,
Fpsid		= 0,
Fpscr,
Mvfr1		= 6,
Mvfr0,
Fpexc,
};
enum {
Ethergpio	= 176,
Etherchanbit	= 1 << (Ethergpio % 32),
};
enum {
Cawt	= 1 << 31,
Cawb	= 1 << 30,
Cara	= 1 << 29,
Cawa	= 1 << 28,
};
struct Gen {
ulong	padconf_off;
ulong	devconf0;
uchar	_pad0[0x68 - 8];
ulong	devconf1;
};
struct Cntrl {
ulong	_pad0;
ulong	id;
ulong	_pad1;
ulong	skuid;
};
static char *
devidstr(ulong)
{
return "ARM Cortex-A8";
}
void
archomaplink(void)
{
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
if (n == 0 || !ispow2(n))
i++;
return i;
}
void
archconfinit(void)
{
char *p;
ulong mhz;
assert(m != nil);
m->cpuhz = 500 * Mhz;
p = getconf("*cpumhz");
if (p) {
mhz = atoi(p) * Mhz;
if (mhz >= 100*Mhz && mhz <= 3000UL*Mhz)
m->cpuhz = mhz;
}
m->delayloop = m->cpuhz/2000;
}
static void
prperm(uvlong perm)
{
if (perm == MASK(16))
print("all");
else
print("%#llux", perm);
}
static void
prl3region(L3protreg *pr, int r)
{
int level, size, addrspace;
uvlong am, base;
if (r == 0)
am = 0;
else
am = pr->addr_match;
size = (am >> 3) & MASK(5);
if (r > 0 && size == 0)
return;
print("  %d: perms req ", r);
prperm(pr->req_info_perm);
if (pr->read_perm == pr->write_perm && pr->read_perm == MASK(16))
print(" rw all");
else {
print(" read ");
prperm(pr->read_perm);
print(" write ");
prperm(pr->write_perm);
}
if (r == 0)
print(", all addrs level 0");
else {
size = 1 << size;
level = (am >> 9) & 1;
if (r == 1)
level = 3;
else
level++;
addrspace = am & 7;
base = am & ~MASK(10);
print(", base %#llux size %dKB level %d addrspace %d",
base, size, level, addrspace);
}
print("\n");
delay(100);
}
static void
dumpl3pr(void)
{
int r;
L3regs *reg;
L3protreg *pr;
for (reg = l3regs; reg < l3regs + nelem(l3regs); reg++) {
print("%#p (%s) enabled l3 regions:\n", reg->base, reg->name);
for (r = 0; r <= reg->upper; r++)
prl3region(reg->base + r, r);
}
if (0) {
reg = l3regs;
for (r = 0; r <= reg->upper; r++) {
pr = reg->base + r;
}
print("%#p (%s) modified l3 regions:\n", reg->base, reg->name);
for (r = 0; r <= reg->upper; r++)
prl3region(reg->base + r, r);
}
}
static void
p16(uchar *p, ulong v)
{
*p++ = v>>8;
*p   = v;
}
static void
p32(uchar *p, ulong v)
{
*p++ = v>>24;
*p++ = v>>16;
*p++ = v>>8;
*p   = v;
}
int
archether(unsigned ctlrno, Ether *ether)
{
switch(ctlrno) {
case 0:
ether->type = "9221";
ether->ctlrno = ctlrno;
ether->irq = 34;
ether->nopt = 0;
ether->mbps = 100;
return 1;
}
return -1;
}
static void
configmpu(void)
{
ulong clk, mhz, nmhz, maxmhz;
Cm *mpu = (Cm *)PHYSSCMMPU;
Cntrl *id = (Cntrl *)PHYSCNTRL;
if ((id->skuid & MASK(4)) == 8)
maxmhz = 720;
else
maxmhz = 600;
iprint("cpu capable of %ldMHz operation", maxmhz);
clk = mpu->clksel[0];
mhz = (clk >> 8) & MASK(11);
iprint("; at %ldMHz", mhz);
nmhz = m->cpuhz / Mhz;
if (mhz == nmhz) {
iprint("\n");
return;
}
mhz = nmhz;
if (mhz > maxmhz) {
mhz = maxmhz;
iprint("; limiting operation to %ldMHz", mhz);
}
mpu->fclken2 = mpu->fclken2 & ~MASK(3) | 5;
coherence();
while (mpu->idlest2 != Dpllbypassed)
;
mpu->clksel[0] = clk & ~(MASK(11) << 8) | mhz << 8;
coherence();
mpu->fclken2 = mpu->fclken2 & ~FREQSEL(MASK(4)) | FREQSEL(3);
coherence();
mpu->fclken2 = mpu->fclken2 & ~(MASK(2) << 8) | 3 << 8;
coherence();
mpu->fclken2 &= ~(1 << 3);
coherence();
mpu->fclken2 |= Dplllock;
coherence();
while (mpu->idlest2 != Dplllocked)
;
delay(200);
if (((mpu->clksel[0] >> 8) & MASK(11)) != mhz)
panic("mpu clock speed change didn't stick");
iprint("; now at %ldMHz\n", mhz);
}
static void
configpll(void)
{
int i;
Cm *pll = (Cm *)PHYSSCMPLL;
pll->clkoutctrl |= 1 << 7;
coherence();
delay(10);
if ((pll->idlest & 3) != 3) {
pll->fclken = pll->fclken & ~(MASK(3) << 16 | MASK(3)) |
1 << 16 | 5;
coherence();
while (pll->idlest & 3)
;
pll->fclken =  (FREQSEL(3) | Dplllock) << 16 |
FREQSEL(3) | Dplllock;
coherence();
while ((pll->idlest & 3) != 3)
;
}
if (!(pll->idlest2 & Dplllocked)) {
pll->fclken2 = 3 << 8 | FREQSEL(1) | 1;
coherence();
for (i = 0; pll->idlest2 & Dplllocked && i < 20; i++)
delay(50);
if (i >= 20)
iprint(" [dpll5 failed to stop]");
pll->clksel[4-1] = 120 << 8 | 12;
pll->clksel[5-1] = 1;
coherence();
pll->fclken2 = 3 << 8 | FREQSEL(1) | Dplllock;
coherence();
for (i = 0; !(pll->idlest2 & Dplllocked) && i < 20; i++)
delay(50);
if (i >= 20)
iprint(" [dpll5 failed to lock]");
}
if (!(pll->idlest2 & (1<<1)))
iprint(" [no 120MHz clock]");
if (!(pll->idlest2 & (1<<3)))
iprint(" [no dpll5 120MHz clock output]");
}
static void
configper(void)
{
Cm *per = (Cm *)PHYSSCMPER;
per->clksel[0] &= ~MASK(8);
per->iclken |= Perenable;
coherence();
per->fclken |= Perenable;
coherence();
while (per->idlest & Perenable)
;
per->autoidle = 0;
coherence();
}
static void
configwkup(void)
{
Cm *wkup = (Cm *)PHYSSCMWKUP;
wkup->clksel[0] &= ~1;
wkup->iclken |= Wkusimocp | Wkwdt2 | Wkgpt1;
coherence();
wkup->fclken |= Wkusimocp | Wkwdt2 | Wkgpt1;
coherence();
while (wkup->idlest & (Wkusimocp | Wkwdt2 | Wkgpt1))
;
}
static void
configusb(void)
{
int i;
Cm *usb = (Cm *)PHYSSCMUSB;
usb->iclken |= Usbhost;
coherence();
usb->fclken |= Usbhost1 | Usbhost2;
coherence();
for (i = 0; usb->idlest & Usbhostidle && i < 20; i++)
delay(50);
if (i >= 20)
iprint(" [usb inaccessible]");
}
static void
configcore(void)
{
Cm *core = (Cm *)PHYSSCMCORE;
core->iclken  |= Coreusbhsotg;
core->iclken3 |= Core3usbtll;
coherence();
core->fclken3 |= Core3usbtll;
coherence();
delay(100);
while (core->idlest & Coreusbhsotgidle)
;
if (core->idlest3 & Core3usbtll)
iprint(" [no usb tll]");
}
static void
configclks(void)
{
int s;
Gen *gen = (Gen *)PHYSSCMPCONF;
delay(20);
s = splhi();
configmpu();
iprint("clocks:");
iprint(" usb");
configusb();
iprint(", pll");
configpll();
iprint(", wakeup");
configwkup();
iprint(", per");
configper();
iprint(", core");
configcore();
iprint("\n");
gen->devconf0 |= 1 << 1 | 1 << 0;
gen->devconf1 |= 1 << 23 | 1 << 22 | 1 << 21 | 1 << 8 | 1 << 7;
coherence();
splx(s);
delay(20);
}
static void
resetwait(ulong *reg)
{
long bound;
for (bound = 400*Mhz; !(*reg & Resetdone) && bound > 0; bound--)
;
if (bound <= 0)
iprint("archomap: Resetdone didn't come ready\n");
}
static void
configgpio(void)
{
Gpio *gpio = (Gpio *)PHYSGPIO6;
gpio->sysconfig = Softreset;
coherence();
resetwait(&gpio->sysstatus);
gpio->ctrl = 1<<1 | 0;
gpio->oe |= Etherchanbit;
coherence();
gpio->irqen1 = Etherchanbit;
gpio->irqen2 = 0;
gpio->lvldet0 = Etherchanbit;
gpio->lvldet1 = 0;
gpio->risingdet = 0;
gpio->fallingdet = 0;
gpio->wkupen = 0;
gpio->deben = 0;
gpio->debtime = 0;
coherence();
gpio->irqsts1 = ~0;
gpio->irqsts2 = ~0;
coherence();
}
void
configscreengpio(void)
{
Cm *wkup = (Cm *)PHYSSCMWKUP;
Gpio *gpio = (Gpio *)PHYSGPIO1;
wkup->iclken |= Wkgpio1;
coherence();
wkup->fclken |= Wkgpio1;
coherence();
wkup->autoidle = 0;
coherence();
while (wkup->idlest & Gpio1idle)
;
gpio->oe &= ~Gpio1vidmagic;
coherence();
gpio->dataout |= Gpio1vidmagic;
coherence();
delay(50);
}
void
screenclockson(void)
{
Cm *dss = (Cm *)PHYSSCMDSS;
dss->iclken |= Dssl3l4;
coherence();
dss->fclken = Dsstv | Dss2 | Dss1;
coherence();
dss->clksel[0] = 1<<12 | 2;
coherence();
delay(50);
while (dss->idlest & Dssidle)
;
}
void
gpioirqclr(void)
{
Gpio *gpio = (Gpio *)PHYSGPIO6;
gpio->irqsts1 = gpio->irqsts1;
coherence();
}
static char *
l1iptype(uint type)
{
static char *types[] = {
"reserved",
"asid-tagged VIVT",
"VIPT",
"PIPT",
};
if (type >= nelem(types) || types[type] == nil)
return "GOK";
return types[type];
}
void
cacheinfo(int level, Memcache *cp)
{
ulong setsways;
cpwrsc(CpIDcssel, CpID, CpIDid, 0, (level - 1) << 1);
setsways = cprdsc(CpIDcsize, CpID, CpIDid, 0);
cp->l1ip = cprdsc(0, CpID, CpIDidct, CpIDct);
cp->level = level;
cp->nways = ((setsways >> 3)  & MASK(10)) + 1;
cp->nsets = ((setsways >> 13) & MASK(15)) + 1;
cp->log2linelen = (setsways & MASK(2)) + 2 + 2;
cp->linelen = 1 << cp->log2linelen;
cp->setsways = setsways;
cp->setsh = cp->log2linelen;
cp->waysh = 32 - log2(cp->nways);
}
static void
prcachecfg(void)
{
int cache;
Memcache mc;
for (cache = 1; cache <= 2; cache++) {
cacheinfo(cache, &mc);
iprint("l%d: %d ways %d sets %d bytes/line",
mc.level, mc.nways, mc.nsets, mc.linelen);
if (mc.linelen != CACHELINESZ)
iprint(" *should* be %d", CACHELINESZ);
if (mc.setsways & Cawt)
iprint("; can WT");
if (mc.setsways & Cawb)
iprint("; can WB");
#ifdef COMPULSIVE
if (mc.setsways & Cara)
iprint("; can read-allocate");
#endif
if (mc.setsways & Cawa)
iprint("; can write-allocate");
if (cache == 1)
iprint("; l1 I policy %s",
l1iptype((mc.l1ip >> 14) & MASK(2)));
iprint("\n");
}
}
static char *
subarch(int impl, uint sa)
{
static char *armarchs[] = {
"VFPv1 (pre-armv7)",
"VFPv2 (pre-armv7)",
"VFPv3+ with common VFP subarch v2",
"VFPv3+ with null subarch",
"VFPv3+ with common VFP subarch v3",
};
if (impl != 'A' || sa >= nelem(armarchs))
return "GOK";
else
return armarchs[sa];
}
enum {
Inena	= 1 << 8,
Indis	= 0 << 8,
Ptup	= 1 << 4,
Ptdown	= 0 << 4,
Ptena	= 1 << 3,
Ptdis	= 0 << 3,
Muxmode	= MASK(3),
GpmcA1		= 0x4800207A,
GpmcA2		= 0x4800207C,
GpmcA3		= 0x4800207E,
GpmcA4		= 0x48002080,
GpmcA5		= 0x48002082,
GpmcA6		= 0x48002084,
GpmcA7		= 0x48002086,
GpmcA8		= 0x48002088,
GpmcA9		= 0x4800208A,
GpmcA10		= 0x4800208C,
GpmcD0		= 0x4800208E,
GpmcD1		= 0x48002090,
GpmcD2		= 0x48002092,
GpmcD3		= 0x48002094,
GpmcD4		= 0x48002096,
GpmcD5		= 0x48002098,
GpmcD6		= 0x4800209A,
GpmcD7		= 0x4800209C,
GpmcD8		= 0x4800209E,
GpmcD9		= 0x480020A0,
GpmcD10		= 0x480020A2,
GpmcD11		= 0x480020A4,
GpmcD12		= 0x480020A6,
GpmcD13		= 0x480020A8,
GpmcD14		= 0x480020AA,
GpmcD15		= 0x480020AC,
GpmcNCS0	= 0x480020AE,
GpmcNCS1	= 0x480020B0,
GpmcNCS2	= 0x480020B2,
GpmcNCS3	= 0x480020B4,
GpmcNCS4	= 0x480020B6,
GpmcNCS5	= 0x480020B8,
GpmcNCS6	= 0x480020BA,
GpmcNCS7	= 0x480020BC,
GpmcCLK		= 0x480020BE,
GpmcNADV_ALE	= 0x480020C0,
GpmcNOE		= 0x480020C2,
GpmcNWE		= 0x480020C4,
GpmcNBE0_CLE	= 0x480020C6,
GpmcNBE1	= 0x480020C8,
GpmcNWP		= 0x480020CA,
GpmcWAIT0	= 0x480020CC,
GpmcWAIT1	= 0x480020CE,
GpmcWAIT2	= 0x480020D0,
GpmcWAIT3	= 0x480020D2,
};
void
setmuxmode(ulong addr, int shorts, int mode)
{
int omode;
ushort *ptr;
mode &= Muxmode;
for (ptr = (ushort *)addr; shorts-- > 0; ptr++) {
omode = *ptr & Muxmode;
if (omode != mode)
*ptr = *ptr & ~Muxmode | mode;
}
coherence();
}
static void
setpadmodes(void)
{
int off;
setmuxmode(0x48002166, 7, 5);
setmuxmode(0x48002180, 1, 5);
setmuxmode(0x48002184, 4, 5);
setmuxmode(0x480021a2, 12, 0);
setmuxmode(0x480021d4, 6, 2);
setmuxmode(0x480025d8, 18, 6);
setmuxmode(0x480020e4, 2, 5);
setmuxmode(0x4800219a, 4, 0);
setmuxmode(0x480021aa, 4, 2);
setmuxmode(0x48002240, 2, 3);
*(ushort *)0x480021d2 = Inena | Ptup | Ptena | 4;
*(ushort *)GpmcA1	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA2	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA3	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA4	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA5	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA6	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA7	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA8	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA9	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcA10	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcD0	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD1	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD2	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD3	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD4	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD5	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD6	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD7	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD8	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD9	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD10	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD11	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD12	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD13	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD14	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcD15	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcNCS0	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS1	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS2	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS3	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS4	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS5	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNCS6	= Indis | Ptup | Ptena | 0;
*(ushort *)GpmcNOE	= Indis | Ptdown | Ptdis | 0;
*(ushort *)GpmcNWE	= Indis | Ptdown | Ptdis | 0;
*(ushort *)GpmcWAIT2	= Inena | Ptup | Ptena | 4;
*(ushort *)GpmcNCS7	= Inena | Ptup | Ptena | 1;
*(ushort *)GpmcCLK	= Indis | Ptdown | Ptdis | 0;
*(ushort *)GpmcNBE1	= Inena | Ptdown | Ptdis | 0;
*(ushort *)GpmcNADV_ALE	= Indis | Ptdown | Ptdis | 0;
*(ushort *)GpmcNBE0_CLE	= Indis | Ptdown | Ptdis | 0;
*(ushort *)GpmcNWP	= Inena | Ptdown | Ptdis | 0;
*(ushort *)GpmcWAIT0	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcWAIT1	= Inena | Ptup | Ptena | 0;
*(ushort *)GpmcWAIT3	= Inena | Ptup | Ptena | 0;
for (off = 0xc0; off <= 0xc4; off += sizeof(short))
*((ushort *)(PHYSSCM + off)) |= 0xe00;
coherence();
}
static char *
implement(uchar impl)
{
if (impl == 'A')
return "arm";
else
return "unknown";
}
static void
fpon(void)
{
int gotfp, impl;
ulong acc, scr;
gotfp = 1 << CpFP | 1 << CpDFP;
cpwrsc(0, CpCONTROL, 0, CpCPaccess, MASK(28));
acc = cprdsc(0, CpCONTROL, 0, CpCPaccess);
if ((acc & (MASK(2) << (2*CpFP))) == 0) {
gotfp &= ~(1 << CpFP);
print("fpon: no single FP coprocessor\n");
}
if ((acc & (MASK(2) << (2*CpDFP))) == 0) {
gotfp &= ~(1 << CpDFP);
print("fpon: no double FP coprocessor\n");
}
if (!gotfp) {
print("fpon: no FP coprocessors\n");
return;
}
fpwr(Fpexc, fprd(Fpexc) | 1 << 30);
scr = fprd(Fpsid);
impl = scr >> 24;
print("fp: %s arch %s", implement(impl),
subarch(impl, (scr >> 16) & MASK(7)));
scr = fprd(Fpscr);
scr |= 1 << 9;
scr &= ~(MASK(2) << 20 | MASK(3) << 16);
fpwr(Fpscr, scr);
print("\n");
}
static void
resetusb(void)
{
int bound;
Uhh *uhh;
Usbotg *otg;
Usbtll *tll;
iprint("resetting usb: otg...");
otg = (Usbotg *)PHYSUSBOTG;
otg->otgsyscfg = Softreset;
coherence();
resetwait(&otg->otgsyssts);
otg->otgsyscfg |= Sidle | Midle;
coherence();
iprint("uhh...");
uhh = (Uhh *)PHYSUHH;
uhh->sysconfig |= Softreset;
coherence();
resetwait(&uhh->sysstatus);
for (bound = 400*Mhz; !(uhh->sysstatus & Resetdone) && bound > 0;
bound--)
;
uhh->sysconfig |= Sidle | Midle;
uhh->hostconfig &= ~P1ulpi_bypass;
coherence();
if (uhh->hostconfig & P1ulpi_bypass)
iprint("utmi (tll) mode...");
else
iprint("ulpi (phy) mode...");
tll = (Usbtll *)PHYSUSBTLL;
if (probeaddr(PHYSUSBTLL) >= 0) {
iprint("tll...");
tll->sysconfig |= Softreset;
coherence();
resetwait(&tll->sysstatus);
tll->sysconfig |= Sidle;
coherence();
} else
iprint("no tll...");
iprint("\n");
}
void
archreset(void)
{
static int beenhere;
if (beenhere)
return;
beenhere = 1;
m->cpuhz = 500 * Mhz;
m->delayloop = m->cpuhz/2000;
prcachecfg();
memset((void *)PHYSSWBOOTCFG, 0, 240);
coherence();
setpadmodes();
configclks();
configgpio();
archconfinit();
resetusb();
fpon();
}
void
archreboot(void)
{
Prm *prm = (Prm *)PHYSPRMGLBL;
iprint("archreboot: reset!\n");
delay(20);
prm->rstctrl |= Rstgs;
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
void
lastresortprint(char *buf, long bp)
{
iprint("%.*s", (int)bp, buf);
}
static void
scmdump(ulong addr, int shorts)
{
ushort reg;
ushort *ptr;
ptr = (ushort *)addr;
print("scm regs:\n");
while (shorts-- > 0) {
reg = *ptr++;
print("%#p: %#ux\tinputenable %d pulltypeselect %d "
"pulludenable %d muxmode %d\n",
ptr, reg, (reg>>8) & 1, (reg>>4) & 1, (reg>>3) & 1,
reg & 7);
}
}
char *cputype2name(char *buf, int size);
void
cpuidprint(void)
{
char name[64];
cputype2name(name, sizeof name);
delay(250);
iprint("cpu%d: %lldMHz ARM %s\n", m->machno, m->cpuhz / Mhz, name);
}
static void
missing(ulong addr, char *name)
{
static int firstmiss = 1;
if (probeaddr(addr) >= 0)
return;
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
delay(20);
missing(PHYSSCM, "scm");
missing(KZERO, "dram");
missing(PHYSL3, "l3 config");
missing(PHYSINTC, "intr ctlr");
missing(PHYSTIMER1, "timer1");
missing(PHYSCONS, "console uart2");
missing(PHYSUART0, "uart0");
missing(PHYSUART1, "uart1");
missing(PHYSETHER, "smc9221");
missing(PHYSUSBOTG, "usb otg");
missing(PHYSUHH, "usb uhh");
missing(PHYSOHCI, "usb ohci");
missing(PHYSEHCI, "usb ehci");
missing(PHYSSDMA, "dma");
missing(PHYSWDOG, "watchdog timer");
missing(PHYSUSBTLL, "usb tll");
iprint("\n");
delay(20);
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
f->type = "onenand";
f->addr = (void*)PHYSNAND;
f->size = 0;
f->width = 1;
f->interleave = 0;
return 0;
}