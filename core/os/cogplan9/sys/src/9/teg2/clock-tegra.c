#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "arm.h"
typedef struct Shrdtmr Shrdtmr;
typedef struct µscnt µscnt;
struct Shrdtmr {
ulong	trigger;
ulong	prescnt;
};
enum {
Enable =	1u<<31,
Periodintr =	1<<30,
Countmask =	MASK(29),
Intrclr =	1<<30,
};
struct µscnt {
ulong	cntr;
ulong	cfg;
uchar	_pad0[0x3c - 0x8];
ulong	freeze;
};
enum {
Dividendshift =	8,
Dividendmask =	MASK(8),
Divisorshift =	0,
Divisormask =	MASK(8),
};
void
tegclockintr(void)
{
int junk;
Shrdtmr *tmr;
tmr = (Shrdtmr *)soc.tmr[0];
junk = tmr->trigger;
USED(junk);
}
void
tegclockshutdown(void)
{
Shrdtmr *tmr;
if (m->machno == 0) {
tmr = (Shrdtmr *)soc.tmr[0];
tmr->prescnt = tmr->trigger = 0;
coherence();
}
}
void
tegwdogintr(Ureg *, void *v)
{
int junk;
Shrdtmr *tmr;
tmr = (Shrdtmr *)v;
tmr->prescnt |= Intrclr;
coherence();
junk = tmr->trigger;
USED(junk);
}
void
tegclock0init(void)
{
Shrdtmr *tmr;
tmr = (Shrdtmr *)soc.tmr[0];
irqenable(Tn0irq, tegwdogintr, tmr, "tegra watchdog");
tmr->trigger = (Dogsectimeout * Mhz / 2 - 1) | Periodintr | Enable;
coherence();
}
void
tegclockinit(void)
{
ulong old;
µscnt *µs = (µscnt *)soc.µs;
assert(µs->cfg == 0xb);
old = µs->cntr;
delay(1);
assert(old != µs->cntr);
}
ulong
perfticks(void)
{
ulong v;
v = ((µscnt *)soc.µs)->cntr;
return v == 0? 1: v;
}