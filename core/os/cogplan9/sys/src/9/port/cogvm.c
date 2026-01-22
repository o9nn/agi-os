#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
enum {
COGnop = 0,
COGcreate,
COGlink,
COGquery,
COGinfer,
COGfocus,
COGspread,
COGpattern,
COGmine,
COGreason,
COGlearn,
};
typedef struct CogInstr CogInstr;
struct CogInstr {
int	op;
int	arg1;
int	arg2;
int	arg3;
void	*data;
};
typedef struct CogProgram CogProgram;
struct CogProgram {
CogInstr	*instrs;
int		ninstr;
int		pc;
Lock;
};
typedef struct CogProc CogProc;
struct CogProc {
int		cogpid;
Proc		*proc;
CogProgram	*program;
int		*regs;
int		nregs;
short		sti;
short		lti;
ulong		cycles;
Lock;
};
static struct {
CogProc		**procs;
int		nprocs;
int		maxprocs;
ulong		totalcycles;
ulong		totalinfer;
int		quantum;
Lock;
} cogvm;
void
cogvminit(void)
{
cogvm.maxprocs = 1024;
cogvm.procs = malloc(cogvm.maxprocs * sizeof(CogProc*));
if(cogvm.procs == nil)
panic("cogvminit: no memory");
cogvm.nprocs = 0;
cogvm.totalcycles = 0;
cogvm.totalinfer = 0;
cogvm.quantum = 10;
}
CogProc*
cogproccreate(Proc *p)
{
CogProc *cp;
lock(&cogvm);
if(cogvm.nprocs >= cogvm.maxprocs) {
unlock(&cogvm);
return nil;
}
cp = malloc(sizeof(CogProc));
if(cp == nil) {
unlock(&cogvm);
return nil;
}
cp->cogpid = cogvm.nprocs;
cp->proc = p;
cp->program = nil;
cp->nregs = 16;
cp->regs = malloc(cp->nregs * sizeof(int));
if(cp->regs == nil) {
free(cp);
unlock(&cogvm);
return nil;
}
cp->sti = 100;
cp->lti = 50;
cp->cycles = 0;
cogvm.procs[cogvm.nprocs++] = cp;
unlock(&cogvm);
return cp;
}
static int
cogvmexec(CogProc *cp, CogInstr *instr)
{
lock(cp);
switch(instr->op) {
case COGnop:
break;
case COGcreate:
break;
case COGlink:
break;
case COGquery:
instr->arg1 = 0;
break;
case COGinfer:
cogvm.totalinfer++;
break;
case COGfocus:
break;
case COGspread:
break;
case COGpattern:
break;
case COGmine:
break;
case COGreason:
cogvm.totalinfer++;
break;
case COGlearn:
break;
default:
unlock(cp);
return -1;
}
cp->cycles++;
cogvm.totalcycles++;
unlock(cp);
return 0;
}
int
cogvmrun(CogProc *cp)
{
CogProgram *prog;
int i, n;
if(cp == nil || cp->program == nil)
return -1;
prog = cp->program;
lock(prog);
n = cogvm.quantum;
for(i = 0; i < n && prog->pc < prog->ninstr; i++) {
if(cogvmexec(cp, &prog->instrs[prog->pc]) < 0) {
unlock(prog);
return -1;
}
prog->pc++;
}
unlock(prog);
return prog->pc < prog->ninstr ? 0 : 1;
}
CogProc*
cogschedule(void)
{
CogProc *best, *cp;
int i, maxpri;
lock(&cogvm);
best = nil;
maxpri = -1;
for(i = 0; i < cogvm.nprocs; i++) {
cp = cogvm.procs[i];
if(cp->sti > maxpri) {
maxpri = cp->sti;
best = cp;
}
}
unlock(&cogvm);
return best;
}
void
cogallocate(CogProc *cp, short sti, short lti)
{
lock(cp);
cp->sti += sti;
cp->lti += lti;
unlock(cp);
}
void
cogdecay(float rate)
{
int i;
CogProc *cp;
lock(&cogvm);
for(i = 0; i < cogvm.nprocs; i++) {
cp = cogvm.procs[i];
lock(cp);
cp->sti = (short)(cp->sti * rate);
unlock(cp);
}
unlock(&cogvm);
}
void
cogvmstats(ulong *cycles, ulong *inferences, int *nprocs)
{
lock(&cogvm);
*cycles = cogvm.totalcycles;
*inferences = cogvm.totalinfer;
*nprocs = cogvm.nprocs;
unlock(&cogvm);
}
CogProc*
cogfork(CogProc *parent)
{
CogProc *child;
int i;
child = cogproccreate(parent->proc);
if(child == nil)
return nil;
lock(parent);
child->sti = parent->sti / 2;
child->lti = parent->lti / 2;
for(i = 0; i < parent->nregs && i < child->nregs; i++)
child->regs[i] = parent->regs[i];
unlock(parent);
return child;
}
int
cogmerge(CogProc *cp1, CogProc *cp2)
{
lock(cp1);
lock(cp2);
cp1->sti += cp2->sti;
cp1->lti += cp2->lti;
cp1->cycles += cp2->cycles;
unlock(cp2);
unlock(cp1);
return 0;
}
void
cogintegrate(Proc *p)
{
CogProc *cp;
cp = cogproccreate(p);
if(cp == nil)
return;
}
int
syscogthink(int op, int arg1, int arg2, void *data)
{
CogInstr instr;
CogProc *cp;
lock(&cogvm);
cp = cogvm.nprocs > 0 ? cogvm.procs[0] : nil;
unlock(&cogvm);
if(cp == nil)
return -1;
instr.op = op;
instr.arg1 = arg1;
instr.arg2 = arg2;
instr.arg3 = 0;
instr.data = data;
return cogvmexec(cp, &instr);
}
int
syscogwait(void)
{
CogProc *cp;
cp = nil;
if(cp == nil)
return -1;
return 0;
}