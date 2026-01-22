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
evenaddr(uintptr addr)
{
if(addr & 3){
postnote(up, 1, "sys: odd address", NDebug);
error(Ebadarg);
}
}
void
kexit(Ureg*)
{
uvlong t;
Tos *tos;
tos = (Tos*)(USTKTOP-sizeof(Tos));
cycles(&t);
tos->kcycles += t - up->kentry;
tos->pcycles = up->pcycles;
tos->cyclefreq = m->cpuhz;
tos->pid = up->pid;
l1cache->wbse(tos, sizeof *tos);
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
l1cache->wbse(p, sizeof *p);
l1cache->wb();
}
void
procrestore(Proc* p)
{
uvlong t;
if(p->kp)
return;
cycles(&t);
p->pcycles -= t;
wakewfi();
l1cache->wb();
}
int
userureg(Ureg* ureg)
{
return (ureg->psr & PsrMask) == PsrMusr;
}