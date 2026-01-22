#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
enum
{
T0cntr=	0x40,
T1cntr=	0x41,
T2cntr=	0x42,
Tmode=	0x43,
T2ctl=	0x61,
Latch0=	0x00,
Load0l=	0x10,
Load0m=	0x20,
Load0=	0x30,
Latch1=	0x40,
Load1l=	0x50,
Load1m=	0x60,
Load1=	0x70,
Latch2=	0x80,
Load2l=	0x90,
Load2m=	0xa0,
Load2=	0xb0,
Rdback=	0xc0,
Rdnstat=0x10,
Rdncnt=	0x20,
Rd0cntr=0x02,
Rd1cntr=0x04,
Rd2cntr=0x08,
ModeMsk=0xe,
Square=	0x6,
Trigger=0x0,
Sstrobe=0x8,
T2gate=	(1<<0),
T2spkr=	(1<<1),
T2out=	(1<<5),
Freq=	1193182,
Tickshift=8,
MaxPeriod=Freq/HZ,
MinPeriod=Freq/(100*HZ),
};
typedef struct I8253 I8253;
struct I8253
{
Lock;
ulong	period;
int	enabled;
uvlong	hz;
ushort	last;
uvlong	ticks;
ulong	periodset;
};
I8253 i8253;
void
i8253init(void)
{
int loops, x;
ioalloc(T0cntr, 4, 0, "i8253");
ioalloc(T2ctl, 1, 0, "i8253.cntr2ctl");
i8253.period = Freq/HZ;
outb(Tmode, Load0|Square);
outb(T0cntr, (Freq/HZ));
outb(T0cntr, (Freq/HZ)>>8);
outb(Tmode, Load2|Square);
outb(T2cntr, 0);
outb(T2cntr, 0);
x = inb(T2ctl);
x |= T2gate;
outb(T2ctl, x);
x = (Freq/HZ);
for(loops = 0; loops < 100000 && x >= (Freq/HZ); loops++){
outb(Tmode, Latch0);
x = inb(T0cntr);
x |= inb(T0cntr)<<8;
}
}
void
guesscpuhz(int aalcycles)
{
int loops, incr, x, y;
uvlong a, b, cpufreq;
incr = 16000000/(aalcycles*HZ*2);
x = 2000;
for(loops = incr; loops < 64*1024; loops += incr) {
outb(Tmode, Latch0);
cycles(&a);
x = inb(T0cntr);
x |= inb(T0cntr)<<8;
aamloop(loops);
outb(Tmode, Latch0);
cycles(&b);
y = inb(T0cntr);
y |= inb(T0cntr)<<8;
x -= y;
if(x < 0)
x += Freq/HZ;
if(x > Freq/(3*HZ))
break;
}
cpufreq = (vlong)loops*((aalcycles*2*Freq)/x);
m->loopconst = (cpufreq/1000)/aalcycles;
if(m->havetsc){
b = (b-a)<<1;
b *= Freq;
b /= x;
m->cpumhz = (b+500000)/1000000L;
m->cpuhz = b;
m->cyclefreq = b;
} else {
m->cpumhz = (cpufreq + cpufreq/200)/1000000;
m->cpuhz = cpufreq;
}
i8253.hz = Freq<<Tickshift;
}
void
i8253timerset(uvlong next)
{
long period;
ulong want;
ulong now;
period = MaxPeriod;
if(next != 0){
want = next>>Tickshift;
now = i8253.ticks;
period = want - now;
if(period < MinPeriod)
period = MinPeriod;
else if(period > MaxPeriod)
period = MaxPeriod;
}
if(i8253.period != period){
ilock(&i8253);
outb(Tmode, Load0|Square);
outb(T0cntr, period);
outb(T0cntr, period >> 8);
i8253.period = period;
i8253.periodset++;
iunlock(&i8253);
}
}
static void
i8253clock(Ureg* ureg, void*)
{
timerintr(ureg, 0);
}
void
i8253enable(void)
{
i8253.enabled = 1;
i8253.period = Freq/HZ;
intrenable(IrqCLOCK, i8253clock, 0, BUSUNKNOWN, "clock");
}
void
i8253link(void)
{
}
uvlong
i8253read(uvlong *hz)
{
ushort y, x;
uvlong ticks;
if(hz)
*hz = i8253.hz;
ilock(&i8253);
outb(Tmode, Latch2);
y = inb(T2cntr);
y |= inb(T2cntr)<<8;
if(y < i8253.last)
x = i8253.last - y;
else {
x = i8253.last + (0x10000 - y);
if (x > 3*MaxPeriod) {
outb(Tmode, Load2|Square);
outb(T2cntr, 0);
outb(T2cntr, 0);
y = 0xFFFF;
x = i8253.period;
}
}
i8253.last = y;
i8253.ticks += x>>1;
ticks = i8253.ticks;
iunlock(&i8253);
return ticks<<Tickshift;
}
void
delay(int millisecs)
{
millisecs *= m->loopconst;
if(millisecs <= 0)
millisecs = 1;
aamloop(millisecs);
}
void
microdelay(int microsecs)
{
microsecs *= m->loopconst;
microsecs /= 1000;
if(microsecs <= 0)
microsecs = 1;
aamloop(microsecs);
}
ulong
perfticks(void)
{
uvlong x;
if(m->havetsc)
cycles(&x);
else
x = 0;
return x;
}