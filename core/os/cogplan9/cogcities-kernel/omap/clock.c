#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "arm.h"
enum {
Debug = 0,
Tn0 = PHYSTIMER1,
Tn1 = PHYSTIMER2,
Tn0irq = 37,
Freebase = 1,
Clockfreqbase = 32 * 1024,
Tcycles = Clockfreqbase / HZ,
MinPeriod = (Tcycles / 100 < 2? 2: Tcycles / 100),
MaxPeriod = Tcycles,
Dogtimeout = 20 * Clockfreqbase,
};
enum {
Noidle = 1<<3,
Softreset = 1<<1,
Resetdone = 1<<0,
Ovf_it = 1<<1,
Mat_it = 1<<0,
Wdovf_it = 1<<0,
Ar = 1<<1,
St = 1<<0,
};
typedef struct Timerregs Timerregs;
struct Timerregs {
uchar pad0[0x10];
ulong ticpcfg;
ulong tistat;
ulong tisr;
ulong tier;
ulong twer;
ulong tclr;
ulong tcrr;
ulong tldr;
ulong ttgr;
ulong twps;
ulong tmar;
ulong tcar1;
ulong tsicr;
ulong tcar2;
union {
ulong tpir;
ulong wspr;
};
ulong tnir;
ulong tcvr;
ulong tocr;
ulong towr;
};
static int ticks;
static Lock clklck;
static ulong rdcycles(void), rdbaseticks(void);
static void
wdogwrss(Timerregs *tn, ulong val)
{
while (tn->twps & (1 << 4))
;
tn->wspr = val;
coherence();
while (tn->twps & (1 << 4))
;
}
static void
resetwait(Timerregs *tn)
{
long bound;
for (bound = 400*Mhz; !(tn->tistat & Resetdone) && bound > 0; bound--)
;
if (bound <= 0)
iprint("clock reset didn't complete\n");
}
static void
wdogoff(Timerregs *tn)
{
resetwait(tn);
wdogwrss(tn, 0xaaaa);
wdogwrss(tn, 0x5555);
tn->tldr = 1;
coherence();
tn->tcrr = 1;
coherence();
}
static void wdogassure(void);
static void
wdogon(Timerregs *tn)
{
static int beenhere;
resetwait(tn);
tn->tldr = -Dogtimeout;
tn->tcrr = -Dogtimeout;
coherence();
wdogwrss(tn, 0xbbbb);
wdogwrss(tn, 0x4444);
if (!beenhere) {
beenhere = 1;
addclock0link(wdogassure, HZ);
}
}
static void
wdogassure(void)
{
Timerregs *tn;
tn = (Timerregs *)PHYSWDOG;
wdogoff(tn);
tn->tcrr = -Dogtimeout;
coherence();
wdogon(tn);
}
static void
clockintr(Ureg* ureg, void *arg)
{
Timerregs *tn;
static int nesting;
ticks++;
coherence();
if (nesting == 0) {
++nesting;
timerintr(ureg, 0);
--nesting;
}
tn = arg;
tn->tisr = Ovf_it;
coherence();
}
static void
clockreset(Timerregs *tn)
{
if (probeaddr((uintptr)&tn->ticpcfg) < 0)
panic("no clock at %#p", tn);
tn->ticpcfg = Softreset | Noidle;
coherence();
resetwait(tn);
tn->tier = tn->tclr = 0;
coherence();
}
void
clockshutdown(void)
{
clockreset((Timerregs *)PHYSWDT2);
wdogoff((Timerregs *)PHYSWDT2);
clockreset((Timerregs *)PHYSWDT3);
wdogoff((Timerregs *)PHYSWDT3);
clockreset((Timerregs *)Tn0);
clockreset((Timerregs *)Tn1);
}
enum {
Instrs = 10*Mhz,
};
static long
issue1loop(void)
{
register int i;
long st;
st = rdbaseticks();
i = Instrs;
do {
--i; --i; --i; --i; --i;
--i; --i; --i; --i;
} while(--i >= 0);
return rdbaseticks() - st;
}
static long
issue2loop(void)
{
register int i, j;
long st;
st = rdbaseticks();
i = Instrs / 2;
j = 0;
do {
--i; --j; --i; --j;
--i; --j; --i; --j;
--j;
} while(--i >= 0);
return rdbaseticks() - st;
}
static void
guessmips(long (*loop)(void), char *lab)
{
int s;
long tcks;
do {
s = splhi();
tcks = loop();
splx(s);
if (tcks < 0)
iprint("again...");
} while (tcks < 0);
s = ((vlong)Clockfreqbase * Instrs) / tcks / 1000000;
if (Debug)
iprint("%ud mips (%s-issue)", s, lab);
USED(s);
}
void
clockinit(void)
{
int i, s;
Timerregs *tn;
clockshutdown();
cpwrsc(0, CpCLD, CpCLDena, CpCLDenacyc, 1<<31);
cpwrsc(0, CpCLD, CpCLDena, CpCLDenapmnc, 1<<2 | 1);
cpwrsc(0, CpCLD, CpCLDena, CpCLDenapmnc, 1);
ilock(&clklck);
m->fastclock = 1;
m->ticks = ticks = 0;
tn = (Timerregs *)Tn0;
tn->tcrr = Freebase;
tn->tldr = Freebase;
coherence();
tn->tclr = Ar | St;
iunlock(&clklck);
tn = (Timerregs *)Tn1;
irqenable(Tn0irq+1, clockintr, tn, "clock");
ilock(&clklck);
tn->tcrr = -Tcycles;
tn->tldr = -Tcycles;
coherence();
tn->tclr = Ar | St;
coherence();
tn->tier = Ovf_it;
coherence();
iunlock(&clklck);
s = spllo();
for (i = 0; i < 5 && ticks == 0; i++) {
delay(10);
cachedwbinvse(&ticks, sizeof ticks);
}
splx(s);
if (ticks == 0) {
if (tn->tcrr == 0)
panic("clock not interrupting");
else if (tn->tcrr == tn->tldr)
panic("clock not ticking at all");
#ifdef PARANOID
else
panic("clock running very slowly");
#endif
}
guessmips(issue1loop, "single");
if (Debug)
iprint(", ");
guessmips(issue2loop, "dual");
if (Debug)
iprint("\n");
m->delayloop = m->cpuhz / (1000 * 2);
delay(m->machno*2);
}
void
watchdoginit(void)
{
wdogassure();
}
ulong
µs(void)
{
return fastticks2us(fastticks(nil));
}
void
timerset(Tval next)
{
long offset;
Timerregs *tn = (Timerregs *)Tn1;
static Lock setlck;
ilock(&setlck);
offset = next - fastticks(nil);
if(offset < MinPeriod)
offset = MinPeriod;
else if(offset > MaxPeriod)
offset = MaxPeriod;
tn->tcrr = -offset;
coherence();
iunlock(&setlck);
}
static ulong
rdcycles(void)
{
ulong v;
v = cprdsc(0, CpCLD, CpCLDcyc, 0);
return v == 0? 1: v;
}
static ulong
rdbaseticks(void)
{
ulong v;
v = ((Timerregs *)Tn0)->tcrr;
return v == 0? 1: v;
}
ulong
perfticks(void)
{
return rdcycles();
}
long
lcycles(void)
{
return perfticks();
}
typedef union Counter Counter;
union Counter {
uvlong uvl;
struct {
ulong low;
ulong high;
};
};
enum {
Fastvlongops = 0,
};
uvlong
fastticks(uvlong *hz)
{
Counter now, sclnow;
if(hz)
*hz = m->cpuhz;
ilock(&clklck);
if (m->ticks > HZ/10 && m->fastclock == 0)
panic("fastticks: zero m->fastclock; ticks %lud fastclock %#llux",
m->ticks, m->fastclock);
now.uvl = m->fastclock;
now.low = rdcycles();
if(now.uvl < m->fastclock)
now.high++;
m->fastclock = now.uvl;
coherence();
sclnow.uvl = now.uvl;
iunlock(&clklck);
return sclnow.uvl;
}
void
microdelay(int l)
{
int i;
l = l * (vlong)m->delayloop / 1000;
if(l <= 0)
l = 1;
for(i = 0; i < l; i++)
;
}
void
delay(int l)
{
ulong i, j;
j = m->delayloop;
while(l-- > 0)
for(i=0; i < j; i++)
;
}