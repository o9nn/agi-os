#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
enum {
Cyccntres	= 2,
Basetickfreq	= 680*Mhz / Cyccntres,
};
void (*kproftimer)(ulong);
void
silencewdog(void)
{
*Rstwdogtimer = Basetickfreq * 2 * 5;
}
void
sicwdog(void)
{
*Rstwdogtimer = Basetickfreq * 2;
*Rstwdogctl = Wdogreset;
}
void
wdogreset(void)
{
*Rstwdogtimer = Basetickfreq / 100;
*Rstwdogctl = Wdogreset;
coherence();
*Rstwdogtimer = Basetickfreq / 10000;
coherence();
}
void
stopwdog(void)
{
*Rstwdogtimer = ~0;
*Rstwdogctl = Wdognoaction;
}
void
clockshutdown(void)
{
stopwdog();
}
void
delay(int l)
{
while(l-- > 0)
microdelay(1000);
}
void
microdelay(int l)
{
int s;
ulong x, cyc, cnt, speed;
speed = m->speed;
if (speed == 0)
speed = Basetickfreq / Mhz * Cyccntres;
cyc = (ulong)l * (speed / Cyccntres);
s = splhi();
cnt = rdcount();
x = cnt + cyc;
if (x < cnt || x >= ~0ul - Basetickfreq) {
wrcount(0);
wrcompare(rdcompare() - cnt);
x = cyc;
}
while(rdcount() < x)
;
splx(s);
silencewdog();
}
void
clock(Ureg *ureg)
{
wrcompare(rdcount()+m->maxperiod);
silencewdog();
timerintr(ureg, 0);
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
--i; --i; --i; --i; --i;
i -= 1+3;
} while(--i >= 0);
return perfticks() - st;
}
static int
guessmips(long (*loop)(void), char *)
{
int s;
long cyc;
do {
s = splhi();
cyc = loop();
splx(s);
if (cyc < 0)
iprint("again...");
} while (cyc < 0);
return (((vlong)Basetickfreq * Instrs) / cyc + Mhz/2) / Mhz;
}
void
clockinit(void)
{
int mips;
silencewdog();
mips = guessmips(issue1loop, "single");
m->delayloop = mips*Mhz / (1000 * 2);
if(m->delayloop == 0)
m->delayloop = 1;
m->speed = mips;
m->hz = m->speed*Mhz;
m->maxperiod = Basetickfreq / HZ;
m->minperiod = Basetickfreq / (100*HZ);
wrcompare(rdcount()+m->maxperiod);
delay(m->machno*2);
syncclock();
intron(INTR7);
}
void
timerset(Tval next)
{
int x;
long period;
if(next == 0)
return;
x = splhi();
period = next - fastticks(nil);
if(period > m->maxperiod - m->minperiod)
period = m->maxperiod;
else if(period < m->minperiod)
period = m->minperiod;
wrcompare(rdcount()+period);
silencewdog();
splx(x);
}
uvlong
fastticks(uvlong *hz)
{
int x;
ulong delta, count;
if(hz)
*hz = Basetickfreq;
x = splhi();
count = rdcount();
if(rdcompare() - count > m->maxperiod)
wrcompare(count+m->maxperiod);
silencewdog();
if (count < m->lastcount)
delta = count + ((1ull<<32) - m->lastcount);
else
delta = count - m->lastcount;
m->lastcount = count;
m->fastticks += delta;
splx(x);
return m->fastticks;
}
ulong
µs(void)
{
return fastticks2us(fastticks(nil));
}
ulong
perfticks(void)
{
return rdcount();
}
long
lcycles(void)
{
return perfticks();
}
void
cycles(uvlong *cycp)
{
*cycp = fastticks(nil);
}
Lock mpsynclock;
void
syncclock(void)
{
uvlong x;
if(m->machno == 0){
m->lastcount = rdcount();
m->fastticks = 0;
m->ticks = 0;
wrcompare(rdcount()+m->maxperiod);
} else {
lock(&mpsynclock);
x = MACHP(0)->fastticks;
while(MACHP(0)->fastticks == x)
;
m->lastcount = rdcount();
m->fastticks = MACHP(0)->fastticks;
m->ticks = MACHP(0)->ticks;
wrcompare(rdcount()+m->maxperiod);
unlock(&mpsynclock);
}
}