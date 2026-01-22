#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
enum {
Tcycles		= CLOCKFREQ / HZ,
Dogperiod	= 15 * CLOCKFREQ,
MaxPeriod	= Tcycles,
MinPeriod	= MaxPeriod / 100,
Tmr0enable	= 1<<0,
Tmr0reload	= 1<<1,
Tmr1enable	= 1<<2,
Tmr1reload	= 1<<3,
TmrWDenable	= 1<<4,
TmrWDreload	= 1<<5,
};
typedef struct TimerReg TimerReg;
struct TimerReg
{
ulong	ctl;
ulong	pad[3];
ulong	reload0;
ulong	timer0;
ulong	reload1;
ulong	timer1;
ulong	reloadwd;
ulong	timerwd;
};
static int ticks;
static void
clockintr(Ureg *ureg, void *arg)
{
TimerReg *tmr = arg;
static int nesting;
tmr->timerwd = Dogperiod;
ticks++;
coherence();
if (nesting == 0) {
++nesting;
timerintr(ureg, 0);
--nesting;
}
intrclear(Irqbridge, IRQcputimer0);
}
void
clockshutdown(void)
{
TimerReg *tmr = (TimerReg *)soc.clock;
tmr->ctl = 0;
coherence();
}
void
clockinit(void)
{
int i, s;
CpucsReg *cpu = (CpucsReg *)soc.cpu;
TimerReg *tmr = (TimerReg *)soc.clock;
clockshutdown();
intrenable(Irqbridge, IRQcputimer0, clockintr, tmr, "clock0");
s = spllo();
splx(s);
m->ticks = ticks = 0;
m->fastclock = 0;
tmr->timer0 = 1;
tmr->ctl = Tmr0enable;
coherence();
s = spllo();
for (i = 0; i < 10 && ticks == 0; i++) {
delay(1);
coherence();
}
splx(s);
if (ticks == 0) {
serialputc('?');
if (tmr->timer0 == 0)
panic("clock not interrupting");
else if (tmr->timer0 == tmr->reload0)
panic("clock not ticking");
else
panic("clock running very slowly");
}
clockshutdown();
tmr->reload0 = tmr->timer0 = Tcycles;
tmr->reload1 = tmr->timer1 = ~0;
tmr->timerwd = Dogperiod;
coherence();
tmr->ctl = Tmr0enable | Tmr0reload | Tmr1enable | Tmr1reload |
TmrWDenable;
cpu->rstout |= RstoutWatchdog;
coherence();
}
void
timerset(Tval next)
{
int offset;
TimerReg *tmr = (TimerReg *)soc.clock;
offset = next - fastticks(nil);
if(offset < MinPeriod)
offset = MinPeriod;
else if(offset > MaxPeriod)
offset = MaxPeriod;
tmr->timer0 = offset;
coherence();
}
uvlong
fastticks(uvlong *hz)
{
uvlong now;
int s;
if(hz)
*hz = CLOCKFREQ;
s = splhi();
now = (m->fastclock & ~(uvlong)~0ul) | perfticks();
if(now < m->fastclock)
now += 1ll << 32;
m->fastclock = now;
splx(s);
return now;
}
ulong
perfticks(void)
{
TimerReg *tmr = (TimerReg *)soc.clock;
return ~tmr->timer1;
}
long
lcycles(void)
{
return perfticks();
}
ulong
µs(void)
{
return fastticks2us(fastticks(nil));
}
void
microdelay(int l)
{
int i;
l *= m->delayloop;
l /= 1000;
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