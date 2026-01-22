#include "boot.h"
#define PIT_RW_COUNTER0  0x30
#define PIT_RW_COUNTER1  0x70
#define PIT_RW_COUNTER2  0xB0
#define PIT_COUNTERLATCH0	0x00
#define PIT_COUNTERLATCH1	0x40
#define PIT_COUNTERLATCH2	0x80
#define PIT_MODE_0	0
#define PIT_MODE_1	2
#define PIT_MODE_2	4
#define PIT_MODE_3	6
#define PIT_MODE_4	8
#define PIT_MODE_5	10
#undef inb
#undef outb
#define 	inb(port)			((*(uchar *)(port))&0xff)
#define 	outb(port, data)	(*(uchar *)(port) = (data))
enum
{
Cnt0=	0xf2000000,
Cnt1=	0xf2000004,
Cnt2=	0xf2000008,
Ctlw=	0xf200000c,
Latch0=	0x00,
Load0=	0x30,
Latch1=	0x40,
Load1=	0x70,
Square=	0x06,
RateGen=	0x04,
Freq=	3686400,
};
static int cpufreq = 233000000;
static int aalcycles = 14;
static void
clockintr(Ureg*, void*)
{
m->ticks++;
checkalarms();
}
void
delay(int l)
{
l *= m->delayloop;
if(l <= 0)
l = 1;
aamloop(l);
}
void
microdelay(int l)
{
l *= m->delayloop;
l /= 1000;
if(l <= 0)
l = 1;
aamloop(l);
}
void
clockinit(void)
{
int x, y;
int loops, incr;
setvec(V_TIMER0, clockintr, 0);
outb(Ctlw, Load0|Square);
outb(Cnt0, (Freq/HZ));
outb(Cnt0, (Freq/HZ)>>8);
incr = 16000000/(aalcycles*HZ*2);
x = 2000;
for(loops = incr; loops < 64*1024; loops += incr) {
outb(Ctlw, Latch0);
x = inb(Cnt0);
x |= inb(Cnt0)<<8;
aamloop(loops);
outb(Ctlw, Latch0);
y = inb(Cnt0);
y |= inb(Cnt0)<<8;
x -= y;
if(x < 0)
x += Freq/HZ;
if(x > Freq/(3*HZ))
break;
}
x >>= 1;
cpufreq = loops*((aalcycles*Freq)/x);
m->delayloop = (cpufreq/1000)/aalcycles;
m->speed = (cpufreq + cpufreq/500)/1000000;
}