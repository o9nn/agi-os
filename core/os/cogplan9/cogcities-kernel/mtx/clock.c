#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
static	ulong	clkreload;
void
delayloopinit(void)
{
ulong v;
uvlong x;
m->loopconst = 5000;
v = getdec();
delay(1000);
v -= getdec();
x = m->loopconst;
x *= m->dechz;
x /= v;
m->loopconst = x;
}
void
clockinit(void)
{
m->cpuhz = 300000000;
m->bushz = 66666666;
m->dechz = m->bushz/4;
m->tbhz = m->dechz;
delayloopinit();
clkreload = m->dechz/HZ-1;
putdec(clkreload);
}
void
clockintr(Ureg *ureg)
{
long v;
v = -getdec();
if(v > clkreload/2){
if(v > clkreload)
m->ticks += v/clkreload;
v = 0;
}
putdec(clkreload-v);
timerintr(ureg, 0);
}
void
timerset(Tval)
{
}
void
delay(int l)
{
ulong i, j;
j = m->loopconst;
while(l-- > 0)
for(i=0; i < j; i++)
;
}
void
microdelay(int l)
{
ulong i;
l *= m->loopconst;
l += 500;
l /= 1000;
if(l <= 0)
l = 1;
for(i = 0; i < l; i++)
;
}
uvlong
fastticks(uvlong *hz)
{
if(hz)
*hz = HZ;
return m->ticks;
}
ulong
µs(void)
{
return fastticks2us(m->ticks);
}
ulong
perfticks(void)
{
return m->ticks;
}