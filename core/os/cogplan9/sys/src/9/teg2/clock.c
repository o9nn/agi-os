#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "arm.h"
enum {
Debug		= 0,
Basetickfreq	= Mhz,
Clockfreqbase	= 250*Mhz / 2,
Tcycles		= Clockfreqbase / HZ,
MinPeriod	= Tcycles / 100,
MaxPeriod	= Tcycles,
Dogtimeout	= Dogsectimeout * Clockfreqbase,
};
typedef struct Ltimer Ltimer;
typedef struct Pglbtmr Pglbtmr;
typedef struct Ploctmr Ploctmr;
struct Ltimer {
ulong	load;
ulong	cnt;
ulong	ctl;
ulong	isr;
ulong	wdrst;
ulong	wddis;
ulong	_pad0[2];
};
struct Ploctmr {
Ltimer	loc;
Ltimer	wd;
};
enum {
Tmrena	= 1<<0,
Wdogena = Tmrena,
Xreload	= 1<<1,
Tintena	= 1<<2,
Wdog	= 1<<3,
Xsclrshift = 8,
Xsclrmask = MASK(8),
Xisrclk	= 1<<0,
Wdrst	= 1<<0,
Wdon	= 1,
Wdoff1	= 0x12345678,
Wdoff2	= 0x87654321,
};
struct Pglbtmr {
ulong	cnt[2];
ulong	ctl;
ulong	isr;
ulong	cmp[2];
ulong	inc;
};
enum {
Gcmp	= 1<<1,
Gincr	= 1<<3,
};
typedef union Vlong Vlong;
union Vlong {
uvlong	uvl;
struct {
ulong	low;
ulong	high;
};
};
static int fired;
static int ticking[MAXMACH];
static void
setltimer(Ltimer *tn, ulong ticks)
{
int s;
assert(ticks <= Clockfreqbase);
s = splhi();
tn->load = ticks - 1;
coherence();
tn->ctl = Tmrena | Tintena | Xreload;
coherence();
splx(s);
}
static void
ckstuck(int cpu, long myticks, long histicks)
{
if (labs(histicks - myticks) > HZ) {
if (!ticking[cpu])
panic("cpu%d: clock not interrupting", cpu);
}
}
static void
mpclocksanity(void)
{
int cpu, mycpu;
long myticks, histicks;
if (conf.nmach <= 1 || active.exiting || navailcpus == 0)
return;
mycpu = m->machno;
myticks = m->ticks;
if (myticks == HZ)
ticking[mycpu] = 1;
if (myticks < 5*HZ)
return;
for (cpu = 0; cpu < navailcpus; cpu++) {
if (cpu == mycpu)
continue;
histicks = MACHP(cpu)->ticks;
if (myticks == 5*HZ || histicks > 1)
ckstuck(cpu, myticks, histicks);
}
}
static void
clockintr(Ureg* ureg, void *arg)
{
Ltimer *wd, *tn;
Ploctmr *lt;
lt = (Ploctmr *)arg;
tn = &lt->loc;
tn->isr = Xisrclk;
coherence();
timerintr(ureg, 0);
#ifdef watchdog_not_bloody_useless
wd = &lt->wd;
if (wd->cnt == 0 &&
(wd->ctl & (Wdog | Wdogena | Tintena)) == (Wdog | Wdogena))
panic("cpu%d: zero watchdog count but no system reset",
m->machno);
wd->load = Dogtimeout - 1;
coherence();
#endif
SET(wd); USED(wd);
tegclockintr();
mpclocksanity();
}
void
clockprod(Ureg *ureg)
{
Ltimer *tn;
timerintr(ureg, 0);
tegclockintr();
if (m->machno != 0) {
tn = &((Ploctmr *)soc.loctmr)->loc;
setltimer(tn, Tcycles);
}
}
static void
clockreset(Ltimer *tn)
{
if (probeaddr((uintptr)tn) < 0)
panic("no clock at %#p", tn);
tn->ctl = 0;
coherence();
}
void
watchdogoff(Ltimer *wd)
{
wd->ctl &= ~Wdogena;
coherence();
wd->wddis = Wdoff1;
coherence();
wd->wddis = Wdoff2;
coherence();
}
void
wdogclrintr(Ltimer *wd)
{
#ifdef watchdog_not_bloody_useless
wd->isr = Xisrclk;
coherence();
wd->wdrst = Wdrst;
coherence();
#endif
USED(wd);
}
void
clockshutdown(void)
{
Ploctmr *lt;
lt = (Ploctmr *)soc.loctmr;
clockreset(&lt->loc);
watchdogoff(&lt->wd);
tegclockshutdown();
}
enum {
Instrs		= 10*Mhz,
};
static long
issue1loop(void)
{
register int i;
long st;
i = Instrs;
st = perfticks();
do {
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i; --i;
--i; --i; --i; --i; --i; --i; --i; --i; --i;
} while(--i >= 0);
return perfticks() - st;
}
static long
issue2loop(void)
{
register int i, j;
long st;
i = Instrs / 2;
j = 0;
st = perfticks();
do {
--j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
--i; --j; --i; --j; --i; --j; --i; --j; --i; --j;
} while(--i >= 0);
return perfticks() - st;
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
s = (((vlong)Basetickfreq * Instrs) / tcks + 500000) / 1000000;
if (Debug)
iprint("%ud mips (%s-issue)", s, lab);
USED(s);
}
void
wdogintr(Ureg *, void *ltmr)
{
#ifdef watchdog_not_bloody_useless
Ltimer *wd;
wd = ltmr;
fired++;
wdogclrintr(wd);
#endif
USED(ltmr);
}
static void
ckcounting(Ltimer *lt)
{
ulong old;
old = lt->cnt;
if (old == lt->cnt)
delay(1);
if (old == lt->cnt)
panic("cpu%d: watchdog timer not counting down", m->machno);
}
static void
ckwatchdog(Ltimer *wd)
{
#ifdef watchdog_not_bloody_useless
int s;
fired = 0;
wd->load = Tcycles - 1;
coherence();
wd->ctl |= Wdogena | Tintena;
coherence();
ckcounting(wd);
s = spllo();
delay(2 * 1000/HZ);
splx(s);
if (!fired)
iprint("cpu%d: local watchdog failed to interrupt\n", m->machno);
wd->ctl &= ~Wdogena;
coherence();
#endif
USED(wd);
}
static void
startwatchdog(void)
{
#ifdef watchdog_not_bloody_useless
Ltimer *wd;
Ploctmr *lt;
lt = (Ploctmr *)soc.loctmr;
wd = &lt->wd;
watchdogoff(wd);
wdogclrintr(wd);
irqenable(Wdtmrirq, wdogintr, wd, "watchdog");
ckwatchdog(wd);
wd->ctl &= ~Tintena;
coherence();
wd->ctl |= Wdog;
coherence();
wd->load = Dogtimeout - 1;
coherence();
wd->ctl |= Wdogena;
coherence();
ckcounting(wd);
#endif
}
static void
clock0init(Ltimer *tn)
{
int s;
ulong old, fticks;
s = splhi();
tn->load = ~0ul >> 1;
coherence();
tn->ctl = Tmrena;
coherence();
old = perfticks();
fticks = tn->cnt;
delay(1);
fticks = abs(tn->cnt - fticks);
old = perfticks() - old;
splx(s);
if (Debug)
iprint("cpu%d: fastclock %ld/%ldµs = %ld fastticks/µs (MHz)\n",
m->machno, fticks, old, (fticks + old/2 - 1) / old);
USED(fticks, old);
if (Debug)
iprint("cpu%d: ", m->machno);
guessmips(issue1loop, "single");
if (Debug)
iprint(", ");
guessmips(issue2loop, "dual");
if (Debug)
iprint("\n");
m->delayloop = m->cpuhz / (1000 * 2);
tegclock0init();
}
void
clockinit(void)
{
ulong old;
Ltimer *tn;
Ploctmr *lt;
clockshutdown();
cpwrsc(0, CpCLD, CpCLDena, CpCLDenacyc, 1<<31);
cpwrsc(0, CpCLD, CpCLDena, CpCLDenapmnc, 1<<2 | 1);
cpwrsc(0, CpCLD, CpCLDuser, CpCLDenapmnc, 1);
tegclockinit();
lt = (Ploctmr *)soc.loctmr;
tn = &lt->loc;
if (m->machno == 0)
irqenable(Loctmrirq, clockintr, lt, "clock");
else
intcunmask(Loctmrirq);
tn->load = Clockfreqbase / 1000;
tn->isr = Xisrclk;
coherence();
tn->ctl = Tmrena;
coherence();
old = tn->cnt;
delay(5);
if (tn->cnt == old)
panic("cpu%d: clock not ticking at all", m->machno);
else if ((long)tn->cnt > 0)
panic("cpu%d: clock ticking slowly", m->machno);
if (m->machno == 0)
clock0init(tn);
startwatchdog();
delay(m->machno*2);
setltimer(tn, Tcycles);
}
ulong
µs(void)
{
return fastticks2us(fastticks(nil));
}
void
timerset(Tval next)
{
int s;
long offset;
Ltimer *tn;
tn = &((Ploctmr *)soc.loctmr)->loc;
s = splhi();
offset = fastticks2us(next - fastticks(nil));
offset *= Clockfreqbase / Mhz;
if(offset < MinPeriod)
offset = MinPeriod;
else if(offset > MaxPeriod)
offset = MaxPeriod;
setltimer(tn, offset);
splx(s);
}
static ulong
cpucycles(void)
{
ulong v;
v = getcyc();
return v == 0? 1: v;
}
long
lcycles(void)
{
return perfticks();
}
uvlong
fastticks(uvlong *hz)
{
int s;
ulong newticks;
Vlong *fcp;
if(hz)
*hz = Basetickfreq;
fcp = (Vlong *)&m->fastclock;
s = splhi();
newticks = perfticks();
if(newticks < fcp->low)
fcp->high++;
fcp->low = newticks;
splx(s);
if (fcp->low == 0 && fcp->high == 0 && m->ticks > HZ/10)
panic("fastticks: zero m->fastclock; ticks %lud fastclock %#llux",
m->ticks, m->fastclock);
return m->fastclock;
}
void
microdelay(int l)
{
for (l = l * (vlong)m->delayloop / 1000; --l >= 0; )
;
}
void
delay(int l)
{
int i, d;
d = m->delayloop;
while(--l >= 0)
for (i = d; --i >= 0; )
;
}