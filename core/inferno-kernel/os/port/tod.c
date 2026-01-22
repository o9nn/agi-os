#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#define TODFREQ	1000000000ULL
struct {
int		init;
ulong	cnt;
Lock;
uvlong	multiplier;
uvlong	divider;
vlong	hz;
vlong	last;
vlong	off;
vlong	lasttime;
vlong	delta;
ulong	sstart;
ulong	send;
} tod;
void
todinit(void)
{
if(tod.init)
return;
ilock(&tod);
tod.last = fastticks((uvlong*)&tod.hz);
iunlock(&tod);
todsetfreq(tod.hz);
tod.init = 1;
addclock0link(todfix, 100);
}
void
todsetfreq(vlong f)
{
ilock(&tod);
tod.hz = f;
tod.multiplier = mk64fract(TODFREQ, f);
tod.divider = mk64fract(f, TODFREQ);
iunlock(&tod);
}
void
todset(vlong t, vlong delta, int n)
{
if(!tod.init)
todinit();
ilock(&tod);
if(t >= 0){
tod.off = t;
tod.last = fastticks(nil);
tod.lasttime = 0;
tod.delta = 0;
tod.sstart = tod.send;
} else {
if(n <= 0)
n = 1;
n *= HZ;
if(delta < 0 && n > -delta)
n = -delta;
if(delta > 0 && n > delta)
n = delta;
delta = delta/n;
tod.sstart = MACHP(0)->ticks;
tod.send = tod.sstart + n;
tod.delta = delta;
}
iunlock(&tod);
}
vlong
todget(vlong *ticksp)
{
uvlong x;
vlong ticks, diff;
ulong t;
if(!tod.init)
todinit();
ilock(&tod);
tod.cnt++;
ticks = fastticks(nil);
if(tod.sstart != tod.send){
t = MACHP(0)->ticks;
if(t >= tod.send)
t = tod.send;
tod.off = tod.off + tod.delta*(t - tod.sstart);
tod.sstart = t;
}
diff = ticks - tod.last;
if(diff < 0)
diff = 0;
mul64fract(&x, diff, tod.multiplier);
x += tod.off;
if(x < tod.lasttime)
x = tod.lasttime;
else
tod.lasttime = x;
iunlock(&tod);
if(ticksp != nil)
*ticksp = ticks;
return x;
}
uvlong
tod2fastticks(vlong ns)
{
uvlong x;
ilock(&tod);
mul64fract(&x, ns-tod.off, tod.divider);
x += tod.last;
iunlock(&tod);
return x;
}
void
todfix(void)
{
vlong ticks, diff;
uvlong x;
ticks = fastticks(nil);
diff = ticks - tod.last;
if(diff > tod.hz){
ilock(&tod);
mul64fract(&x, diff, tod.multiplier);
if(x > 30000000000ULL) print("todfix %llud\n", x);
x += tod.off;
tod.last = ticks;
tod.off = x;
iunlock(&tod);
}
}
long
tseconds(void)
{
vlong x;
int i;
x = todget(nil);
x = x/TODFREQ;
i = x;
return i;
}
uvlong
ms2fastticks(ulong ms)
{
if(!tod.init)
todinit();
return (tod.hz*ms)/1000ULL;
}
uvlong
ns2fastticks(uvlong ns)
{
uvlong res;
if(!tod.init)
todinit();
mul64fract(&res, ns, tod.divider);
return res;
}
uvlong
fastticks2ns(uvlong ticks)
{
uvlong res;
if(!tod.init)
todinit();
mul64fract(&res, ticks, tod.multiplier);
return res;
}
uvlong
mk64fract(uvlong to, uvlong from)
{
return (to<<32)/from;
}