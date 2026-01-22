#include "stdinc.h"
#include "dat.h"
#include "fns.h"
ulong lasttime[2];
int manualscheduling;
int l0quantum = 120;
int l1quantum = 120;
ulong lasticachechange;
void
disksched(void)
{
int p, nwrite, nflush, ndirty, tdirty, toflush;
ulong t;
vlong cflush;
Stats *prev;
t = time(0);
if(manualscheduling){
lasticachechange = t;
return;
}
if(t-lasttime[0] < l0quantum){
p = icachedirtyfrac();
if(p < IcacheFrac*5/10){
icachesleeptime = SleepForever;
lasticachechange = t;
}else if(p > IcacheFrac*9/10){
icachesleeptime = 0;
lasticachechange = t;
}else if(t-lasticachechange > 60){
prev = &stathist[(stattime-60+nstathist)%nstathist];
nwrite = stats.n[StatIcacheWrite] - prev->n[StatIcacheWrite];
ndirty = stats.n[StatIcacheDirty] - prev->n[StatIcacheDirty];
nflush = nwrite - ndirty;
tdirty = (vlong)stats.n[StatIcacheSize]*700/1000;
cflush = (vlong)nflush*(icachesleeptime+1);
toflush = nwrite + (stats.n[StatIcacheDirty] - tdirty);
if(toflush <= 0 || cflush/toflush > 100000)
icachesleeptime = SleepForever;
else
icachesleeptime = cflush/toflush;
}
arenasumsleeptime = SleepForever;
return;
}
if(t-lasttime[1] < l1quantum){
icachesleeptime = 0;
arenasumsleeptime = SleepForever;
return;
}
icachesleeptime = 0;
arenasumsleeptime = 0;
}
void
diskaccess(int level)
{
if(level < 0 || level >= nelem(lasttime)){
fprint(2, "bad level in diskaccess; caller=%#p\n",
getcallerpc(&level));
return;
}
lasttime[level] = time(0);
}