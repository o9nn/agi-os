#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include <tos.h>
#include "ureg.h"
#include "arm.h"
void
setkernur(Ureg* ureg, Proc* p)
{
ureg->pc = p->sched.pc;
ureg->sp = p->sched.sp+4;
ureg->r14 = PTR2UINT(sched);
}
void
validalign(uintptr addr, unsigned align)
{
if(align == sizeof(vlong))
align = sizeof(long);
if((align != 0 && !(align & (align-1))) && !(addr & (align-1)))
return;
postnote(up, 1, "sys: odd address", NDebug);
error(Ebadarg);
}
void
kexit(Ureg*)
{
uvlong t;
Tos *tos;
tos = (Tos*)(USTKTOP-sizeof(Tos));
t = fastticks(nil);
tos->kcycles += t - up->kentry;
tos->pcycles = up->pcycles;
tos->cyclefreq = Frequency;
tos->pid = up->pid;
cachedwbinvse(tos, sizeof *tos);
}
uintptr
userpc(void)
{
Ureg *ureg = up->dbgreg;
return ureg->pc;
}
void
setregisters(Ureg* ureg, char* pureg, char* uva, int n)
{
USED(ureg, pureg, uva, n);
}
static void
linkproc(void)
{
spllo();
up->kpfun(up->kparg);
pexit("kproc exiting", 0);
}
void
kprocchild(Proc *p, void (*func)(void*), void *arg)
{
p->sched.pc = PTR2UINT(linkproc);
p->sched.sp = PTR2UINT(p->kstack+KSTACK);
p->kpfun = func;
p->kparg = arg;
}
uintptr
dbgpc(Proc* p)
{
Ureg *ureg;
ureg = p->dbgreg;
if(ureg == 0)
return 0;
return ureg->pc;
}
void
procsetup(Proc* p)
{
fpusysprocsetup(p);
}
void
procsave(Proc* p)
{
uvlong t;
cycles(&t);
p->pcycles += t;
fpuprocsave(p);
}
void
procrestore(Proc* p)
{
uvlong t;
if(p->kp)
return;
t = lcycles();
p->pcycles -= t;
fpuprocrestore(p);
}
int
userureg(Ureg* ureg)
{
return (ureg->psr & PsrMask) == PsrMusr;
}
long
_xdec(long *p)
{
int s, v;
s = splhi();
v = --*p;
splx(s);
return v;
}
void
_xinc(long *p)
{
int s;
s = splhi();
++*p;
splx(s);
}
int
ainc(int *p)
{
int s, v;
s = splhi();
v = ++*p;
splx(s);
return v;
}
int
adec(int *p)
{
int s, v;
s = splhi();
v = --*p;
splx(s);
return v;
}
int
cas32(void* addr, u32int old, u32int new)
{
int r, s;
s = splhi();
if(r = (*(u32int*)addr == old))
*(u32int*)addr = new;
splx(s);
if (r)
coherence();
return r;
}