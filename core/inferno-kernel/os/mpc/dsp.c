#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"io.h"
#include	"dsp.h"
enum {
Ndsp = 2,
NHOLES=	64
};
typedef struct DSPparam DSPparam;
struct DSPparam {
ulong	fdbase;
ulong	fd_ptr;
ulong	dstate;
ulong	resvd[2];
ushort	dstatus;
ushort	i;
ushort	tap;
ushort	cbase;
ushort	anon1;
ushort	xptr;
ushort	anon2;
ushort	yptr;
ushort	m;
ushort	anon3;
ushort	n;
ushort	anon4;
ushort	k;
ushort	anon5;
};
struct DSP {
Lock;
void	(*done)(void*);
void*	arg;
DSPparam*	par;
CPMdev*	cpm;
QLock;
int	busy;
Rendez	busyr;
};
static	DSP	dsps[Ndsp];
static	Lock	dsplock;
static	int	dspinit;
static struct {
QLock;
ulong	avail;
Rendez	wantr;
} dspalloc;
static	Map	fndmapv[NHOLES];
static	RMap	fndmap = {"DSP function descriptors"};
static void
dspinterrupt(Ureg*, void*)
{
int i;
ushort events;
DSP *dsp;
events = m->iomem->sdsr;
m->iomem->sdsr = events;
if(events & (1<<7))
panic("dsp: SDMA channel bus error sdar=#%lux", m->iomem->sdar);
for(i=0; i<Ndsp; i++)
if(events & (1<<i)){
dsp = &dsps[i];
if(dsp->busy){
dsp->busy = 0;
if(dsp->done)
dsp->done(dsp->arg);
else
wakeup(&dsp->busyr);
}else
print("dsp%d: empty interrupt\n", i);
}
}
void
dspinitialise(void)
{
CPMdev *d;
ilock(&dsplock);
if(dspinit == 0){
mapinit(&fndmap, fndmapv, sizeof(fndmapv));
d = cpmdev(CPdsp1);
dsps[0].cpm = d;
dsps[0].par = d->param;
d = cpmdev(CPdsp2);
dsps[1].cpm = d;
dsps[1].par = d->param;
intrenable(VectorCPIC+d->irq, dspinterrupt, nil, BUSUNKNOWN, "dsp");
dspalloc.avail = (1<<Ndsp)-1;
dspinit = 1;
}
iunlock(&dsplock);
}
static int
dspavail(void*)
{
return dspalloc.avail != 0;
}
DSP*
dspacquire(void (*done)(void*), void *arg)
{
DSP *dsp;
int i;
if(dspinit == 0)
dspinitialise();
qlock(&dspalloc);
if(waserror()){
qunlock(&dspalloc);
nexterror();
}
for(i=0;; i++){
if(i >= Ndsp){
sleep(&dspalloc.wantr, dspavail, nil);
i = 0;
}
if(dspalloc.avail & (1<<i))
break;
}
dsp = &dsps[i];
if(dsp->busy)
panic("dspacquire");
dsp->done = done;
dsp->arg = arg;
poperror();
qunlock(&dspalloc);
return dsp;
}
void
dsprelease(DSP *dsp)
{
ulong bit;
if(dsp == nil)
return;
bit = 1 << (dsp-dsps);
if(dspalloc.avail & bit)
panic("dsprelease");
dspalloc.avail |= bit;
wakeup(&dspalloc.wantr);
}
void
dspexec(DSP *dsp, FnD *f, ulong n)
{
dspsetfn(dsp, f, n);
dspstart(dsp);
}
void
dspsetfn(DSP *dsp, FnD *f, ulong n)
{
f[n-1].status |= FnWrap;
ilock(dsp);
dsp->par->fdbase = PADDR(f);
iunlock(dsp);
cpmop(dsp->cpm, InitDSP, 0);
}
void
dspstart(DSP *dsp)
{
ilock(dsp);
dsp->busy = 1;
iunlock(dsp);
cpmop(dsp->cpm, StartDSP, 0);
}
static int
dspdone(void *a)
{
return ((DSP*)a)->busy;
}
void
dspsleep(DSP *dsp)
{
sleep(&dsp->busyr, dspdone, dsp);
}
FnD*
fndalloc(ulong n)
{
ulong a, nb, pgn;
FnD *f;
if(n == 0)
return nil;
if(dspinit == 0)
dspinitialise();
nb = n*sizeof(FnD);
while((a = rmapalloc(&fndmap, 0, nb, sizeof(FnD))) != 0){
pgn = (nb+BY2PG-1)&~(BY2PG-1);
a = PADDR(xspanalloc(pgn, sizeof(FnD), 0));
if(a == 0)
return nil;
mapfree(&fndmap, a, pgn);
}
f = KADDR(a);
f[n-1].status = FnWrap;
return f;
}
void
fndfree(FnD *f, ulong n)
{
if(f != nil)
mapfree(&fndmap, PADDR(f), n*sizeof(FnD));
}
void*
dspmalloc(ulong n)
{
ulong i;
n = (n+3)&~4;
i = n;
if(n & (n-1)){
for(i=1; i < n; i <<= 1)
;
}
return cpmalloc(n, i);
}
void
dspfree(void *p, ulong n)
{
if(p != nil)
cpmfree(p, (n+3)&~4);
}