#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include <isa.h>
#include <interp.h>
typedef struct Clock0link Clock0link;
typedef struct Clock0link {
void (*clock)(void);
Clock0link* link;
} Clock0link;
static Clock0link *clock0link;
static Lock clock0lock;
ulong clkrelinq;
void (*kproftick)(ulong);
void (*archclocktick)(void);
Timer*
addclock0link(void (*clock)(void), int)
{
Clock0link *lp;
if((lp = malloc(sizeof(Clock0link))) == 0){
print("addclock0link: too many links\n");
return nil;
}
ilock(&clock0lock);
lp->clock = clock;
lp->link = clock0link;
clock0link = lp;
iunlock(&clock0lock);
return nil;
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
void
microdelay(int l)
{
ulong i;
l *= m->delayloop;
l /= 1000;
if(l <= 0)
l = 1;
for(i = 0; i < l; i++)
;
}
enum {
Timebase = 4,
};
static ulong clkreload;
void
clockinit(void)
{
long x;
m->delayloop = m->cpuhz/1000;
do {
x = gettbl();
delay(10);
x = gettbl() - x;
} while(x < 0);
m->delayloop = ((vlong)m->delayloop*(10*m->clockgen/1000))/(x*Timebase);
if(m->delayloop == 0)
m->delayloop = 1;
clkreload = (m->clockgen/Timebase)/HZ-1;
putdec(clkreload);
}
void
clockintr(Ureg *ur)
{
Clock0link *lp;
long v;
v = -getdec();
if(v > clkreload/2){
if(v > clkreload)
m->ticks += v/clkreload;
v = 0;
}
putdec(clkreload-v);
if(m->iomem->sypcr & (1<<2)){
m->iomem->swsr = 0x556c;
m->iomem->swsr = 0xaa39;
}
m->ticks++;
if(archclocktick != nil)
archclocktick();
if(up)
up->pc = ur->pc;
checkalarms();
if(m->machno == 0) {
if(kproftick != nil)
(*kproftick)(ur->pc);
if(canlock(&clock0lock)){
for(lp = clock0link; lp; lp = lp->link)
lp->clock();
unlock(&clock0lock);
}
}
if(up && up->state == Running){
if(cflag && up->type == Interp && tready(nil))
ur->cr |= 1;
}
}
uvlong
fastticks(uvlong *hz)
{
if(hz)
*hz = HZ;
return m->ticks;
}