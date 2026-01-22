#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "../port/error.h"
uchar BREAK = 0xcc;
static ulong skipflags;
extern int (*breakhandler)(Ureg *ur, Proc*);
static Bkpt *skip;
int breakmatch(BkptCond *cond, Ureg *ur, Proc *p);
void breaknotify(Bkpt *b, Proc *p);
void breakrestore(Bkpt *b);
Bkpt* breakclear(int id);
void
skiphandler(Ureg *ur, void*)
{
if (skip == 0)
panic("single step outside of skip");
breakrestore( skip );
skip = 0;
ur->flags = skipflags;
if (up != 0)
up->state = Running;
}
void
machbreakinit(void)
{
breakhandler = breakhit;
trapenable(VectorDBG, skiphandler, nil, "bkpt.skip");
}
Instr
machinstr(ulong addr)
{
if (addr < KTZERO)
error(Ebadarg);
return *(uchar*)addr;
}
void
machbreakset(ulong addr)
{
if (addr < KTZERO)
error(Ebadarg);
*(uchar*)addr = BREAK;
}
void
machbreakclear(ulong addr, Instr i)
{
if (addr < KTZERO)
error(Ebadarg);
*(uchar*)addr = i;
}
extern Bkpt *breakpoints;
int
breakhit(Ureg *ur, Proc *p)
{
Bkpt *b;
int nmatched;
ur->pc--;
nmatched = 0;
for(b = breakpoints; b != nil; b = b->next) {
if(breakmatch(b->conditions, ur, p)) {
breaknotify(b, p);
++nmatched;
}
}
if (nmatched)
return 1;
if (skip != nil)
panic("x86break: non-nil skip in breakhit\n");
for(b = breakpoints; b != (Bkpt*) nil; b = b->next) {
if(b->addr == ur->pc) {
if(breakclear(b->id) == 0)
panic("breakhit: breakclear() failed");
skip = b;
skipflags = ur->flags;
if (p != 0)
p->state = Stopped;
if (ur->flags & (1 << 9)) {
ur->flags &= ~(1<<9);
}
ur->flags |= (1 << 8);
}
}
return 1;
}
int
isvalid_va(void*)
{
return 1;
}