#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "ureg.h"
#include "arm.h"
enum {
Nirqs = 96,
Nvec = 8,
Bi2long = BI2BY * sizeof(long),
};
extern int notify(Ureg*);
extern int ldrexvalid;
typedef struct Intrregs Intrregs;
struct Intrregs {
uchar	_pad0[4*4];
ulong	sysconfig;
ulong	sysstatus;
uchar	_pad1[0x40 - 0x18];
ulong	sir_irq;
ulong	sir_fiq;
ulong	control;
ulong	protection;
ulong	idle;
uchar	_pad2[0x60 - 0x54];
ulong	irq_priority;
ulong	fiq_priority;
ulong	threshold;
uchar	_pad3[0x80 - 0x6c];
struct Bits {
ulong	itr;
ulong	mir;
ulong	mir_clear;
ulong	mir_set;
ulong	isr_set;
ulong	isr_clear;
ulong	pending_irq;
ulong	pending_fiq;
} bits[3];
ulong	ilr[Nirqs];
};
enum {
Softreset	= 1<<1,
Resetdone	= 1<<0,
Activeirq	= MASK(7),
Newirqagr	= 1<<0,
Protection	= 1<<0,
Irqpriority	= MASK(6),
Prioritythreshold = MASK(8),
Priority	= MASK(8) - MASK(2),
};
typedef struct Vctl Vctl;
typedef struct Vctl {
Vctl*	next;
char	*name;
void	(*f)(Ureg*, void*);
void*	a;
} Vctl;
static Lock vctllock;
static Vctl* vctl[Nirqs];
typedef struct Vpage0 {
void	(*vectors[Nvec])(void);
u32int	vtable[Nvec];
} Vpage0;
static Vpage0 *vpage0;
uvlong ninterrupt;
uvlong ninterruptticks;
int irqtooearly = 1;
static volatile int probing, trapped;
static int
irqinuse(uint irq)
{
Intrregs *ip = (Intrregs *)PHYSINTC;
return (ip->bits[irq / Bi2long].mir & (1 << (irq % Bi2long))) == 0;
}
static void
intcmask(uint irq)
{
Intrregs *ip = (Intrregs *)PHYSINTC;
ip->bits[irq / Bi2long].mir_set = 1 << (irq % Bi2long);
coherence();
}
static void
intcunmask(uint irq)
{
Intrregs *ip = (Intrregs *)PHYSINTC;
ip->bits[irq / Bi2long].mir_clear = 1 << (irq % Bi2long);
coherence();
}
static void
intcmaskall(void)
{
int i;
Intrregs *ip = (Intrregs *)PHYSINTC;
for (i = 0; i < 3; i++)
ip->bits[i].mir_set = ~0;
coherence();
}
static void
intcunmaskall(void)
{
int i;
Intrregs *ip = (Intrregs *)PHYSINTC;
for (i = 0; i < 3; i++)
ip->bits[i].mir_clear = ~0;
coherence();
}
static void
intcinvertall(void)
{
int i, s;
ulong bits;
Intrregs *ip = (Intrregs *)PHYSINTC;
s = splhi();
for (i = 0; i < 3; i++) {
bits = ip->bits[i].mir;
ip->bits[i].mir_set = ~0;
coherence();
ip->bits[i].mir_clear = bits;
}
coherence();
splx(s);
}
static void
intrsave(ulong buf[3])
{
int i;
Intrregs *ip = (Intrregs *)PHYSINTC;
for (i = 0; i < nelem(buf); i++)
buf[i] = ip->bits[i].mir;
coherence();
}
static void
intrrestore(ulong buf[3])
{
int i, s;
Intrregs *ip = (Intrregs *)PHYSINTC;
s = splhi();
for (i = 0; i < nelem(buf); i++) {
ip->bits[i].mir_clear = ~0;
coherence();
ip->bits[i].mir_set = buf[i];
}
coherence();
splx(s);
}
void
trapinit(void)
{
int i;
Intrregs *ip = (Intrregs *)PHYSINTC;
vpage0 = (Vpage0*)HVECTORS;
memmove(vpage0->vectors, vectors, sizeof(vpage0->vectors));
memmove(vpage0->vtable, vtable, sizeof(vpage0->vtable));
cacheuwbinv();
l2cacheuwbinv();
setr13(PsrMfiq, m->sfiq);
setr13(PsrMirq, m->sirq);
setr13(PsrMabt, m->sabt);
setr13(PsrMund, m->sund);
#ifdef HIGH_SECURITY
setr13(PsrMmon, m->smon);
#endif
setr13(PsrMsys, m->ssys);
intcmaskall();
ip->control = 0;
ip->threshold = Prioritythreshold;
for (i = 0; i < Nirqs; i++)
ip->ilr[i] = 0<<2 | 0;
irqtooearly = 0;
coherence();
}
void
intrsoff(void)
{
Intrregs *ip = (Intrregs *)PHYSINTC;
intcmaskall();
ip->control = Newirqagr;
coherence();
}
int
irqenable(int irq, void (*f)(Ureg*, void*), void* a, char *name)
{
Vctl *v;
if(irq >= nelem(vctl) || irq < 0)
panic("irqenable irq %d", irq);
if (irqtooearly) {
iprint("irqenable for %d %s called too early\n", irq, name);
return -1;
}
if(irqinuse(irq))
print("irqenable: %s: irq %d already in use, chaining\n",
name, irq);
v = malloc(sizeof(Vctl));
if (v == nil)
panic("irqenable: malloc Vctl");
v->f = f;
v->a = a;
v->name = malloc(strlen(name)+1);
if (v->name == nil)
panic("irqenable: malloc name");
strcpy(v->name, name);
lock(&vctllock);
v->next = vctl[irq];
vctl[irq] = v;
intcunmask(irq);
unlock(&vctllock);
return 0;
}
int
irqdisable(int irq, void (*f)(Ureg*, void*), void* a, char *name)
{
Vctl **vp, *v;
if(irq >= nelem(vctl) || irq < 0)
panic("irqdisable irq %d", irq);
lock(&vctllock);
for(vp = &vctl[irq]; v = *vp; vp = &v->next)
if (v->f == f && v->a == a && strcmp(v->name, name) == 0){
print("irqdisable: remove %s\n", name);
*vp = v->next;
free(v);
break;
}
if(v == nil)
print("irqdisable: irq %d, name %s not enabled\n", irq, name);
if(vctl[irq] == nil){
print("irqdisable: clear icmr bit %d\n", irq);
intcmask(irq);
}
unlock(&vctllock);
return 0;
}
static void
faultarm(Ureg *ureg, uintptr va, int user, int read)
{
int n, insyscall;
char buf[ERRMAX];
if(up == nil) {
dumpregs(ureg);
panic("fault: nil up in faultarm, accessing %#p", va);
}
insyscall = up->insyscall;
up->insyscall = 1;
n = fault(va, read);
if(n < 0){
if(!user){
dumpregs(ureg);
panic("fault: kernel accessing %#p", va);
}
snprint(buf, sizeof buf, "sys: trap: fault %s va=%#p",
read? "read": "write", va);
postnote(up, 1, buf, NDebug);
}
up->insyscall = insyscall;
}
static int
irq(Ureg* ureg)
{
int clockintr;
uint irqno, handled, t, ticks = perfticks();
Intrregs *ip = (Intrregs *)PHYSINTC;
Vctl *v;
static int nesting, lastirq = -1;
handled = 0;
irqno = ip->sir_irq & Activeirq;
if (irqno >= 37 && irqno <= 47)
m->inclockintr++;
lastirq = irqno;
if (irqno >= nelem(vctl)) {
iprint("trap: irq %d >= # vectors (%d)\n", irqno, nelem(vctl));
ip->control = Newirqagr;
return 0;
}
++nesting;
for(v = vctl[irqno]; v != nil; v = v->next)
if (v->f) {
if (islo())
panic("trap: pl0 before trap handler for %s",
v->name);
v->f(ureg, v->a);
if (islo())
panic("trap: %s lowered pl", v->name);
handled++;
}
if(!handled) {
iprint("unexpected interrupt: irq %d", irqno);
switch (irqno) {
case 56:
case 57:
iprint(" (I⁲C)");
break;
case 83:
case 86:
case 94:
iprint(" (MMC)");
break;
}
if(irqno < nelem(vctl)) {
intcmask(irqno);
iprint(", now masked");
}
iprint("\n");
}
t = perfticks();
ninterrupt++;
if(t < ticks)
ninterruptticks += ticks-t;
else
ninterruptticks += t-ticks;
ip->control = Newirqagr;
coherence();
--nesting;
clockintr = m->inclockintr == 1;
if (irqno >= 37 && irqno <= 47)
m->inclockintr--;
return clockintr;
}
int
writetomem(ulong inst)
{
if((inst & 0x0FC00000) == 0x01000000)
return 1;
if(inst & (1<<20))
return 0;
return 1;
}
void	prgpmcerrs(void);
void
trap(Ureg *ureg)
{
int clockintr, user, x, rv, rem;
ulong inst, fsr;
uintptr va;
char buf[ERRMAX];
splhi();
if(up != nil)
rem = ((char*)ureg)-up->kstack;
else
rem = ((char*)ureg)-((char*)m+sizeof(Mach));
if(rem < 1024) {
iprint("trap: %d stack bytes left, up %#p ureg %#p at pc %#lux\n",
rem, up, ureg, ureg->pc);
delay(1000);
dumpstack();
panic("trap: %d stack bytes left, up %#p ureg %#p at pc %#lux",
rem, up, ureg, ureg->pc);
}
user = (ureg->psr & PsrMask) == PsrMusr;
if(user){
up->dbgreg = ureg;
cycles(&up->kentry);
}
if(ureg->type == (PsrMabt+1))
ureg->pc -= 8;
else
ureg->pc -= 4;
clockintr = 0;
switch(ureg->type){
default:
panic("unknown trap; type %#lux, psr mode %#lux", ureg->type,
ureg->psr & PsrMask);
break;
case PsrMirq:
ldrexvalid = 0;
clockintr = irq(ureg);
m->intr++;
break;
case PsrMabt:
ldrexvalid = 0;
x = ifsrget();
fsr = (x>>7) & 0x8 | x & 0x7;
switch(fsr){
case 0x02:
if(user){
snprint(buf, sizeof buf, "sys: breakpoint");
postnote(up, 1, buf, NDebug);
}else{
iprint("kernel bkpt: pc %#lux inst %#ux\n",
ureg->pc, *(u32int*)ureg->pc);
panic("kernel bkpt");
}
break;
default:
faultarm(ureg, ureg->pc, user, 1);
break;
}
break;
case PsrMabt+1:
ldrexvalid = 0;
va = farget();
inst = *(ulong*)(ureg->pc);
x = fsrget();
fsr = (x>>7) & 0x20 | (x>>6) & 0x10 | x & 0xf;
if (probing && !user) {
if (trapped++ > 0)
panic("trap: recursive probe %#lux", va);
ureg->pc += 4;
break;
}
switch(fsr){
default:
case 0xa:
panic("unknown data fault, 6b fsr %#lux", fsr);
break;
case 0x0:
panic("vector exception at %#lux", ureg->pc);
break;
case 0x1:
case 0x3:
if(user){
snprint(buf, sizeof buf,
"sys: alignment: pc %#lux va %#p\n",
ureg->pc, va);
postnote(up, 1, buf, NDebug);
} else
panic("kernel alignment: pc %#lux va %#p", ureg->pc, va);
break;
case 0x2:
panic("terminal exception at %#lux", ureg->pc);
break;
case 0x4:
case 0x6:
case 0x8:
case 0x28:
case 0xc:
case 0x2c:
case 0xe:
case 0x2e:
case 0x16:
case 0x36:
panic("external abort %#lux pc %#lux addr %#p",
fsr, ureg->pc, va);
break;
case 0x1c:
case 0x1e:
case 0x18:
panic("translation parity error %#lux pc %#lux addr %#p",
fsr, ureg->pc, va);
break;
case 0x5:
case 0x7:
faultarm(ureg, va, user, !writetomem(inst));
break;
case 0x9:
case 0xb:
if(user){
snprint(buf, sizeof buf,
"sys: access violation: pc %#lux va %#p\n",
ureg->pc, va);
postnote(up, 1, buf, NDebug);
} else
panic("kernel access violation: pc %#lux va %#p",
ureg->pc, va);
break;
case 0xd:
case 0xf:
faultarm(ureg, va, user, !writetomem(inst));
break;
}
break;
case PsrMund:
if(user){
if(seg(up, ureg->pc, 0) != nil &&
*(u32int*)ureg->pc == 0xD1200070){
snprint(buf, sizeof buf, "sys: breakpoint");
postnote(up, 1, buf, NDebug);
}else{
x = spllo();
rv = fpiarm(ureg);
splx(x);
if(rv == 0){
ldrexvalid = 0;
snprint(buf, sizeof buf,
"undefined instruction: pc %#lux\n",
ureg->pc);
postnote(up, 1, buf, NDebug);
}
}
}else{
if (ureg->pc & 3) {
iprint("rounding fault pc %#lux down to word\n",
ureg->pc);
ureg->pc &= ~3;
}
iprint("undefined instruction: pc %#lux inst %#ux\n",
ureg->pc, ((u32int*)ureg->pc)[-2]);
panic("undefined instruction");
}
break;
}
splhi();
if(up && up->delaysched && clockintr){
ldrexvalid = 0;
sched();
splhi();
}
if(user){
if(up->procctl || up->nnote)
notify(ureg);
kexit(ureg);
}
}
void
callwithureg(void (*fn)(Ureg*))
{
Ureg ureg;
ureg.pc = getcallerpc(&fn);
ureg.sp = PTR2UINT(&fn);
fn(&ureg);
}
static void
dumpstackwithureg(Ureg *ureg)
{
int x;
uintptr l, v, i, estack;
char *s;
dumpregs(ureg);
if((s = getconf("*nodumpstack")) != nil && strcmp(s, "0") != 0){
iprint("dumpstack disabled\n");
return;
}
iprint("dumpstack\n");
x = 0;
x += iprint("ktrace /kernel/path %#.8lux %#.8lux %#.8lux # pc, sp, link\n",
ureg->pc, ureg->sp, ureg->r14);
delay(20);
i = 0;
if(up
&& (uintptr)&l >= (uintptr)up->kstack
&& (uintptr)&l <= (uintptr)up->kstack+KSTACK)
estack = (uintptr)up->kstack+KSTACK;
else if((uintptr)&l >= (uintptr)m->stack
&& (uintptr)&l <= (uintptr)m+MACHSIZE)
estack = (uintptr)m+MACHSIZE;
else
return;
x += iprint("estackx %p\n", estack);
for(l = (uintptr)&l; l < estack; l += sizeof(uintptr)){
v = *(uintptr*)l;
if((KTZERO < v && v < (uintptr)etext) || estack-l < 32){
x += iprint("%.8p ", v);
delay(20);
i++;
}
if(i == 8){
i = 0;
x += iprint("\n");
delay(20);
}
}
if(i)
iprint("\n");
}
void
dumpstack(void)
{
callwithureg(dumpstackwithureg);
}
static void
dumpscr(void)
{
iprint("0:\t%#8.8ux id\n", cpidget());
iprint("\t%8.8#ux ct\n", cpctget());
iprint("1:\t%#8.8ux control\n", controlget());
iprint("2:\t%#8.8ux ttb\n", ttbget());
iprint("3:\t%#8.8ux dac\n", dacget());
iprint("4:\t(reserved)\n");
iprint("5:\t%#8.8ux fsr\n", fsrget());
iprint("6:\t%#8.8ux far\n", farget());
iprint("7:\twrite-only cache\n");
iprint("8:\twrite-only tlb\n");
iprint("13:\t%#8.8ux pid\n", pidget());
delay(10);
}
static void
dumpgpr(Ureg* ureg)
{
if(up != nil)
iprint("cpu%d: registers for %s %lud\n",
m->machno, up->text, up->pid);
else
iprint("cpu%d: registers for kernel\n", m->machno);
delay(20);
iprint("%#8.8lux\tr0\n", ureg->r0);
iprint("%#8.8lux\tr1\n", ureg->r1);
iprint("%#8.8lux\tr2\n", ureg->r2);
delay(20);
iprint("%#8.8lux\tr3\n", ureg->r3);
iprint("%#8.8lux\tr4\n", ureg->r4);
iprint("%#8.8lux\tr5\n", ureg->r5);
delay(20);
iprint("%#8.8lux\tr6\n", ureg->r6);
iprint("%#8.8lux\tr7\n", ureg->r7);
iprint("%#8.8lux\tr8\n", ureg->r8);
delay(20);
iprint("%#8.8lux\tr9 (up)\n", ureg->r9);
iprint("%#8.8lux\tr10 (m)\n", ureg->r10);
iprint("%#8.8lux\tr11 (loader temporary)\n", ureg->r11);
iprint("%#8.8lux\tr12 (SB)\n", ureg->r12);
delay(20);
iprint("%#8.8lux\tr13 (sp)\n", ureg->r13);
iprint("%#8.8lux\tr14 (link)\n", ureg->r14);
iprint("%#8.8lux\tr15 (pc)\n", ureg->pc);
delay(20);
iprint("%10.10lud\ttype\n", ureg->type);
iprint("%#8.8lux\tpsr\n", ureg->psr);
delay(20);
}
void
dumpregs(Ureg* ureg)
{
dumpgpr(ureg);
dumpscr();
}
vlong
probeaddr(uintptr addr)
{
vlong v;
static Lock fltlck;
ilock(&fltlck);
trapped = 0;
probing = 1;
coherence();
v = *(ulong *)addr;
USED(probing);
coherence();
probing = 0;
coherence();
if (trapped)
v = -1;
iunlock(&fltlck);
return v;
}