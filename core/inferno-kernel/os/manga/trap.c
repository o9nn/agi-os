#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"../port/error.h"
#define waslo(sr) (!((sr) & (PsrDirq|PsrDfiq)))
enum
{
MaxVector=	32,
Maxhandler=	MaxVector+5
};
typedef struct Handler Handler;
struct Handler {
void	(*r)(Ureg*, void*);
void*	a;
char	name[KNAMELEN];
Handler*	next;
int	edge;
ulong	nintr;
ulong	ticks;
int	maxtick;
};
static Lock veclock;
static struct
{
Handler	*ivec[MaxVector];
Handler	h[Maxhandler];
int	free;
Handler*	freelist;
} halloc;
Instr BREAK = 0xE6BAD010;
int (*breakhandler)(Ureg*, Proc*);
int (*catchdbg)(Ureg *, uint);
extern void (*serwrite)(char *, int);
void
intrenable(int sort, int v, void (*r)(Ureg*, void*), void* a, char *name)
{
int o, x;
ulong f;
GpioReg *g;
Handler *h;
USED(sort);
f = v;
v &= IRQmask;
if(v >= nelem(halloc.ivec))
panic("intrenable(%d)", v);
ilock(&veclock);
if(v >= IRQext0 && v <= IRQtm1){
g = GPIOREG;
if(v <= IRQext3){
o = (v-IRQext0)*4;
g->iopc = (g->iopc & ~(7<<o)) | (1<<(o+3)) | ((f>>8)&7)<<o;
o = v - IRQext0;
if(f & IRQsoft)
g->iopm |= 1<<o;
else
g->iopm &= ~(1<<o);
}else
g->iopc |= 1<<(16+(v-IRQtm0));
if(0)
iprint("v=%d iopc=%8.8lux iopm=%8.8lux\n", v, g->iopc, g->iopm);
}
if((h = halloc.freelist) == nil){
if(halloc.free >= Maxhandler){
iunlock(&veclock);
panic("out of interrupt handlers");
}
h = &halloc.h[halloc.free++];
}else
halloc.freelist = h->next;
h->r = r;
h->a = a;
strncpy(h->name, name, KNAMELEN-1);
h->name[KNAMELEN-1] = 0;
h->next = halloc.ivec[v];
halloc.ivec[v] = h;
x = splfhi();
INTRREG->st = 1<<v;
INTRREG->en |= 1<<v;
splx(x);
iunlock(&veclock);
}
void
intrdisable(int sort, int v, void (*r)(Ureg*, void*), void* a, char *name)
{
int x, o;
GpioReg *g;
Handler *h, **hp;
USED(sort);
v &= IRQmask;
if(v >= nelem(halloc.ivec))
panic("intrdisable(%d)", v);
ilock(&veclock);
for(hp = &halloc.ivec[v]; (h = *hp) != nil; hp = &h->next)
if(h->r == r && h->a == a && strcmp(h->name, name) == 0){
*hp = h->next;
h->r = nil;
h->next = halloc.freelist;
halloc.freelist = h;
break;
}
if(halloc.ivec[v] == nil){
if(v >= IRQext0 && v <= IRQtm1){
g = GPIOREG;
if(v <= IRQext3){
o = (v-IRQext0)*4;
g->iopc &= ~(0xF<<o);
g->iopm &= ~(v-IRQext0);
}else
g->iopc &= ~(1<<(16+(v-IRQtm0)));
}
x = splfhi();
INTRREG->en &= ~(1<<v);
splx(x);
}
iunlock(&veclock);
}
static void
intrs(Ureg *ur, ulong ibits)
{
Handler *h;
int i, s;
for(i=0; i<nelem(halloc.ivec) && ibits; i++)
if(ibits & (1<<i)){
h = halloc.ivec[i];
for(; h != nil; h = h->next){
INTRREG->st = 1<<i;
h->r(ur, h->a);
ibits &= ~(1<<i);
}
}
if(ibits != 0){
iprint("spurious irq interrupt: %8.8lux\n", ibits);
s = splfhi();
INTRREG->en &= ~ibits;
splx(s);
}
}
void
trapstacks(void)
{
setr13(PsrMfiq, m->fiqstack+nelem(m->fiqstack));
setr13(PsrMirq, m->irqstack+nelem(m->irqstack));
setr13(PsrMabt, m->abtstack+nelem(m->abtstack));
setr13(PsrMund, m->undstack+nelem(m->undstack));
}
void
trapinit(void)
{
IntrReg *intr;
intr = INTRREG;
intr->mc = 0;
intr->en = 0;
intr->st = intr->st;
trapstacks();
memmove(page0->vectors, vectors, sizeof(page0->vectors));
memmove(page0->vtable, vtable, sizeof(page0->vtable));
dcflush(page0, sizeof(*page0));
icflushall();
}
static char *trapnames[PsrMask+1] = {
[ PsrMfiq ] "Fiq interrupt",
[ PsrMirq ] "Mirq interrupt",
[ PsrMsvc ] "SVC/SWI Exception",
[ PsrMabt ] "Prefetch Abort/Data Abort",
[ PsrMabt+1 ] "Data Abort",
[ PsrMund ] "Undefined instruction",
[ PsrMsys ] "Sys trap"
};
static char *
trapname(int psr)
{
char *s;
s = trapnames[psr & PsrMask];
if(s == nil)
s = "Undefined trap";
return s;
}
static void
sys_trap_error(int type)
{
char errbuf[ERRMAX];
sprint(errbuf, "sys: trap: %s\n", trapname(type));
error(errbuf);
}
static void
faultarm(Ureg *ureg, ulong far)
{
char buf[ERRMAX];
sprint(buf, "sys: trap: fault pc=%8.8lux addr=0x%lux", (ulong)ureg->pc, far);
if(1){
iprint("%s\n", buf);
dumpregs(ureg);
}
if(far == ~0)
disfault(ureg, "dereference of nil");
disfault(ureg, buf);
}
void
trap(Ureg* ureg)
{
ulong far, fsr;
int t, itype;
Proc *oup;
itype = ureg->type;
if(itype == PsrMabt+1)
ureg->pc -= 8;
else
ureg->pc -= 4;
ureg->sp = (ulong)(ureg+1);
if(itype == PsrMfiq){
oup = up;
up = nil;
intrs(ureg, INTRREG->ms & INTRREG->mc);
up = oup;
return;
}
if(up){
up->pc = ureg->pc;
up->dbgreg = ureg;
}
switch(itype) {
case PsrMirq:
t = m->ticks;
up = nil;
splflo();
intrs(ureg, INTRREG->ms & ~INTRREG->mc);
up = m->proc;
preemption(m->ticks - t);
break;
case PsrMund:
if(*(ulong*)ureg->pc == BREAK && breakhandler) {
int s;
Proc *p;
p = up;
s = breakhandler(ureg, p);
if(s == BrkSched) {
p->preempted = 0;
sched();
} else if(s == BrkNoSched) {
p->preempted = 1;
if(up)
up->dbgreg = 0;
return;
}
break;
}
if(up == nil)
goto faultpanic;
spllo();
if(waserror()) {
if(waslo(ureg->psr) && up->type == Interp)
disfault(ureg, up->env->errstr);
setpanic();
dumpregs(ureg);
panic("%s", up->env->errstr);
}
if(!fpiarm(ureg)) {
dumpregs(ureg);
sys_trap_error(ureg->type);
}
poperror();
break;
case PsrMsvc:
if(waslo(ureg->psr) && up && up->type == Interp) {
spllo();
dumpregs(ureg);
sys_trap_error(ureg->type);
}
setpanic();
dumpregs(ureg);
panic("SVC/SWI exception");
break;
case PsrMabt:
if(catchdbg && catchdbg(ureg, 0))
break;
case PsrMabt+1:
fsr = mmugetfsr();
far = mmugetfar();
if(fsr & (1<<9)) {
mmuputfsr(fsr & ~(1<<9));
if(catchdbg && catchdbg(ureg, fsr))
break;
print("Debug/");
}
if(waslo(ureg->psr) && up && up->type == Interp) {
spllo();
faultarm(ureg, far);
}
iprint("Data Abort: FSR %8.8luX FAR %8.8luX\n", fsr, far); xdelay(500);serialputs("\n", 1);
default:
faultpanic:
setpanic();
dumpregs(ureg);
panic("exception %uX %s\n", ureg->type, trapname(ureg->type));
break;
}
splhi();
if(up)
up->dbgreg = 0;
}
void
setpanic(void)
{
if(breakhandler != 0)
return;
splhi();
GPIOREG->iopd &= ~(1<<GPIO_status_orange_o);
consoleprint = 1;
serwrite = serialputs;
}
int
isvalid_va(void *v)
{
return (ulong)v >= KZERO && (ulong)v <= (ulong)KADDR(conf.topofmem-1);
}
void
dumplongs(char *msg, ulong *v, int n)
{
int i, l;
l = 0;
iprint("%s at %.8p: ", msg, v);
for(i=0; i<n; i++){
if(l >= 4){
iprint("\n    %.8p: ", v);
l = 0;
}
if(isvalid_va(v)){
iprint(" %.8lux", *v++);
l++;
}else{
iprint(" invalid");
break;
}
}
iprint("\n");
}
static void
_dumpstack(Ureg *ureg)
{
ulong v, *l, *estack;
int i;
l = (ulong*)(ureg+1);
if((ulong)l & 3){
iprint("invalid ureg/stack: %.8p\n", l);
return;
}
iprint("dumpstack\n");
print("ktrace /kernel/path %.8ux %.8ux %.8ux\n", ureg->pc, ureg->sp, ureg->r14);
if(up != nil && l >= (ulong*)up->kstack && l <= (ulong*)(up->kstack+KSTACK-4))
estack = (ulong*)(up->kstack+KSTACK);
else if(l >= (ulong*)m->stack && l <= (ulong*)((ulong)m+BY2PG-4))
estack = (ulong*)((ulong)m+BY2PG-4);
else{
iprint("unknown stack %8.8p\n", l);
return;
}
iprint("estackx %8.8p\n", estack);
i = 0;
for(; l<estack; l++) {
v = *l;
if(KTZERO < v && v < (ulong)etext){
iprint("%8.8p=%8.8lux ", l, v);
if(i++ == 4){
iprint("\n");
i = 0;
}
}
}
if(i)
print("\n");
}
void
dumpregs(Ureg* ureg)
{
print("TRAP: %s", trapname(ureg->type));
if((ureg->psr & PsrMask) != PsrMsvc)
print(" in %s", trapname(ureg->psr));
print("\n");
print("PSR %8.8uX type %2.2uX PC %8.8uX LINK %8.8uX\n",
ureg->psr, ureg->type, ureg->pc, ureg->link);
print("R14 %8.8uX R13 %8.8uX R12 %8.8uX R11 %8.8uX R10 %8.8uX\n",
ureg->r14, ureg->r13, ureg->r12, ureg->r11, ureg->r10);
print("R9  %8.8uX R8  %8.8uX R7  %8.8uX R6  %8.8uX R5  %8.8uX\n",
ureg->r9, ureg->r8, ureg->r7, ureg->r6, ureg->r5);
print("R4  %8.8uX R3  %8.8uX R2  %8.8uX R1  %8.8uX R0  %8.8uX\n",
ureg->r4, ureg->r3, ureg->r2, ureg->r1, ureg->r0);
print("Stack is at: %8.8luX\n", ureg);
print("PC %8.8lux LINK %8.8lux\n", (ulong)ureg->pc, (ulong)ureg->link);
if(up)
print("Process stack:  %8.8lux-%8.8lux\n",
up->kstack, up->kstack+KSTACK-4);
else
print("System stack: %8.8lux-%8.8lux\n",
(ulong)(m+1), (ulong)m+BY2PG-4);
dumplongs("stack", (ulong *)(ureg + 1), 16);
_dumpstack(ureg);
}
void
callwithureg(void (*fn)(Ureg*))
{
Ureg ureg;
ureg.pc = getcallerpc(&fn);
ureg.sp = (ulong)&fn;
ureg.r14 = 0;
fn(&ureg);
}
void
dumpstack(void)
{
return;
callwithureg(_dumpstack);
}
void
trapspecial(int (*f)(Ureg *, uint))
{
catchdbg = f;
}