#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "ureg.h"
#include "arm.h"
#define ISSGI(irq)	((uint)(irq) < Nsgi)
enum {
Debug = 0,
Nvec = 8,
Bi2long = BI2BY * sizeof(long),
Nirqs = 1024,
Nsgi =	16,
Nppi =	32,
};
typedef struct Intrcpuregs Intrcpuregs;
typedef struct Intrdistregs Intrdistregs;
struct Intrdistregs {
ulong	ctl;
ulong	ctlrtype;
ulong	distid;
uchar	_pad0[0x80 - 0xc];
ulong	grp[32];
ulong	setena[32];
ulong	clrena[32];
ulong	setpend[32];
ulong	clrpend[32];
ulong	setact[32];
ulong	clract[32];
uchar	pri[1020];
ulong	_rsrvd1;
uchar	targ[1020];
ulong	_rsrvd2;
ulong	cfg[64];
ulong	_pad1[64];
ulong	nsac[64];
ulong	swgen;
uchar	_pad2[0xf10 - 0xf04];
uchar	clrsgipend[16];
uchar	setsgipend[16];
};
enum {
Forw2cpuif =	1,
Cpunoshft =	5,
Cpunomask =	MASK(3),
Intrlines =	MASK(5),
Level =		0<<1,
Edge =		1<<1,
Toall =		0<<0,
To1 =		1<<0,
Totargets =	0,
Tonotme =	1<<24,
Tome =		2<<24,
};
struct Intrcpuregs {
ulong	ctl;
ulong	primask;
ulong	binpt;
ulong	ack;
ulong	end;
ulong	runpri;
ulong	hipripend;
ulong	alibinpt;
ulong	aliack;
ulong	aliend;
ulong	alihipripend;
uchar	_pad0[0xd0 - 0x2c];
ulong	actpri[4];
ulong	nsactpri[4];
uchar	_pad0[0xfc - 0xf0];
ulong	ifid;
uchar	_pad0[0x1000 - 0x100];
ulong	deact;
};
enum {
Enable =	1,
Eoinodeact =	1<<9,
Intrmask =	MASK(10),
Cpuidshift =	10,
Cpuidmask =	MASK(3),
Archversshift =	16,
Archversmask =	MASK(4),
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
enum
{
Ntimevec = 20
};
ulong intrtimes[Nirqs][Ntimevec];
uvlong ninterrupt;
uvlong ninterruptticks;
int irqtooearly = 1;
static ulong shadena[32];
static Lock distlock, nintrlock;
extern int notify(Ureg*);
static void dumpstackwithureg(Ureg *ureg);
void
printrs(int base, ulong word)
{
int bit;
for (bit = 0; word; bit++, word >>= 1)
if (word & 1)
iprint(" %d", base + bit);
}
void
dumpintrs(char *what, ulong *bits)
{
int i, first, some;
ulong word;
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
first = 1;
some = 0;
USED(idp);
for (i = 0; i < nelem(idp->setpend); i++) {
word = bits[i];
if (word) {
if (first) {
first = 0;
iprint("%s", what);
}
some = 1;
printrs(i * Bi2long, word);
}
}
if (!some)
iprint("%s none", what);
iprint("\n");
}
void
dumpintrpend(void)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
iprint("\ncpu%d gic regs:\n", m->machno);
dumpintrs("group 1", idp->grp);
dumpintrs("enabled", idp->setena);
dumpintrs("pending", idp->setpend);
dumpintrs("active ", idp->setact);
}
void
intrtime(Mach*, int vno)
{
ulong diff;
ulong x;
x = perfticks();
diff = x - m->perf.intrts;
m->perf.intrts = x;
m->perf.inintr += diff;
if(up == nil && m->perf.inidle > diff)
m->perf.inidle -= diff;
if (m->cpumhz == 0)
return;
diff /= m->cpumhz*100;
if(diff >= Ntimevec)
diff = Ntimevec-1;
if ((uint)vno >= Nirqs)
vno = Nirqs-1;
intrtimes[vno][diff]++;
}
static ulong
intack(Intrcpuregs *icp)
{
return icp->ack & Intrmask;
}
static void
intdismiss(Intrcpuregs *icp, ulong ack)
{
icp->end = ack;
coherence();
}
static int
irqinuse(uint irq)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
return idp->setena[irq / Bi2long] & (1 << (irq % Bi2long));
}
void
intcunmask(uint irq)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
ilock(&distlock);
idp->setena[irq / Bi2long] = 1 << (irq % Bi2long);
iunlock(&distlock);
}
void
intcmask(uint irq)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
ilock(&distlock);
idp->clrena[irq / Bi2long] = 1 << (irq % Bi2long);
iunlock(&distlock);
}
static void
intcmaskall(Intrdistregs *idp)
{
int i;
for (i = 0; i < nelem(idp->setena); i++)
shadena[i] = idp->setena[i];
for (i = 0; i < nelem(idp->clrena); i++)
idp->clrena[i] = ~0;
coherence();
}
static void
intcunmaskall(Intrdistregs *idp)
{
int i;
for (i = 0; i < nelem(idp->setena); i++)
idp->setena[i] = shadena[i];
coherence();
}
static ulong
permintrs(Intrdistregs *idp, int base, int r)
{
ulong perms;
idp->clrena[r] = ~0;
coherence();
perms = idp->clrena[r];
if (perms) {
iprint("perm intrs:");
printrs(base, perms);
iprint("\n");
}
return perms;
}
static void
intrcfg(Intrdistregs *idp)
{
int i, cpumask;
ulong pat;
pat = 0;
for (i = 0; i < Bi2long; i += 2)
pat |= (Level | To1) << i;
if (m->machno == 0) {
for (i = 0; i < nelem(idp->grp); i++)
idp->grp[i] = 0;
for (i = 0; i < nelem(idp->pri); i++)
idp->pri[i] = 0;
for (i = 0; i < nelem(idp->cfg); i++)
idp->cfg[i] = pat;
cpumask = 1<<0;
navailcpus = getncpus();
for (i = Nppi; i < sizeof idp->targ; i++)
idp->targ[i] = cpumask;
coherence();
intcmaskall(idp);
for (i = 0; i < nelem(idp->clrena); i++) {
idp->clrpend[i] = idp->clract[i] = idp->clrena[i] = ~0;
}
} else {
idp->grp[0] = 0;
for (i = 0; i < 8; i++)
idp->pri[i] = 0;
for (i = 0; i < Nppi; i++)
idp->targ[i] = 1<<m->machno;
idp->cfg[1] = pat;
coherence();
idp->clrpend[0] = idp->clract[0] = idp->clrena[0] = ~0;
}
coherence();
}
void
intrto(int cpu, int irq)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
ilock(&distlock);
idp->targ[irq] = 1 << cpu;
iunlock(&distlock);
}
void
intrsto(int cpu)
{
int i;
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
for (i = Nppi; i < sizeof idp->targ; i++)
intrto(cpu, i);
USED(idp);
}
void
intrcpu(int cpu)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
ilock(&distlock);
idp->swgen = Totargets | 1 << (cpu + 16) | m->machno;
iunlock(&distlock);
}
void
trapinit(void)
{
int s;
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
Intrcpuregs *icp = (Intrcpuregs *)soc.intr;
Vpage0 *vpage0;
enum { Vecsize = sizeof vpage0->vectors + sizeof vpage0->vtable, };
if (m->machno == 0) {
vpage0 = (Vpage0*)HVECTORS;
memmove(vpage0->vectors, vectors, sizeof(vpage0->vectors));
memmove(vpage0->vtable, vtable, sizeof(vpage0->vtable));
vpage0 = (Vpage0*)KADDR(0);
memmove(vpage0->vectors, vectors, sizeof(vpage0->vectors));
memmove(vpage0->vtable, vtable, sizeof(vpage0->vtable));
allcache->wbse(vpage0, Vecsize);
cacheiinv();
}
s = splhi();
setr13(PsrMfiq, m->sfiq);
setr13(PsrMirq, m->sirq);
setr13(PsrMmon, m->smon);
setr13(PsrMabt, m->sabt);
setr13(PsrMund, m->sund);
setr13(PsrMsys, m->ssys);
splx(s);
assert((idp->distid & MASK(12)) == 0x43b);
assert((icp->ifid   & MASK(12)) == 0x43b);
ilock(&distlock);
idp->ctl = 0;
icp->ctl = 0;
coherence();
intrcfg(idp);
icp->ctl = Enable;
icp->primask = (uchar)~0;
coherence();
idp->ctl = Forw2cpuif;
iunlock(&distlock);
}
void
intrsoff(void)
{
ilock(&distlock);
intcmaskall((Intrdistregs *)soc.intrdist);
iunlock(&distlock);
}
void
intrcpushutdown(void)
{
Intrcpuregs *icp = (Intrcpuregs *)soc.intr;
icp->ctl = 0;
icp->primask = 0;
coherence();
}
void
intrshutdown(void)
{
Intrdistregs *idp = (Intrdistregs *)soc.intrdist;
intrsoff();
idp->ctl = 0;
intrcpushutdown();
}
int
irqenable(uint irq, void (*f)(Ureg*, void*), void* a, char *name)
{
Vctl *v;
if(irq >= nelem(vctl))
panic("irqenable irq %d", irq);
if (irqtooearly) {
iprint("irqenable for %d %s called too early\n", irq, name);
return -1;
}
if(!ISSGI(irq) && irqinuse(irq)) {
lock(&vctllock);
if (vctl[irq] == nil) {
dumpintrpend();
panic("non-sgi irq %d in use yet no Vctl allocated", irq);
}
unlock(&vctllock);
}
else if (vctl[irq] == nil) {
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
if (vctl[irq] != nil) {
free(v->name);
free(v);
} else {
v->next = vctl[irq];
vctl[irq] = v;
}
unlock(&vctllock);
}
intcunmask(irq);
return 0;
}
int
irqdisable(uint irq, void (*f)(Ureg*, void*), void* a, char *name)
{
Vctl **vp, *v;
if(irq >= nelem(vctl))
panic("irqdisable irq %d", irq);
lock(&vctllock);
for(vp = &vctl[irq]; v = *vp; vp = &v->next)
if (v->f == f && v->a == a && strcmp(v->name, name) == 0){
if(Debug)
print("irqdisable: remove %s\n", name);
*vp = v->next;
free(v->name);
free(v);
break;
}
if(v == nil)
print("irqdisable: irq %d, name %s not enabled\n", irq, name);
if(vctl[irq] == nil){
if(Debug)
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
if(up == nil) {
dumpstackwithureg(ureg);
panic("faultarm: cpu%d: nil up, %sing %#p at %#p",
m->machno, (read? "read": "writ"), va, ureg->pc);
}
insyscall = up->insyscall;
up->insyscall = 1;
n = fault(va, read);
splhi();
if(n < 0){
char buf[ERRMAX];
if(!user){
dumpstackwithureg(ureg);
panic("fault: cpu%d: kernel %sing %#p at %#p",
m->machno, read? "read": "writ", va, ureg->pc);
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
int clockintr, ack;
uint irqno, handled, t, ticks;
Intrcpuregs *icp = (Intrcpuregs *)soc.intr;
Vctl *v;
ticks = perfticks();
handled = 0;
ack = intack(icp);
irqno = ack & Intrmask;
if (irqno >= nelem(vctl)) {
iprint("trap: irq %d >= # vectors (%d)\n", irqno, nelem(vctl));
intdismiss(icp, ack);
return 0;
}
if (irqno == Loctmrirq)
m->inclockintr++;
if(m->machno && m->inclockintr > 1) {
iprint("cpu%d: nested clock intrs\n", m->machno);
m->inclockintr--;
intdismiss(icp, ack);
return 0;
}
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
if(!handled)
if (irqno >= 1022)
iprint("cpu%d: ignoring spurious interrupt\n", m->machno);
else {
intcmask(irqno);
iprint("cpu%d: unexpected interrupt %d, now masked\n",
m->machno, irqno);
}
t = perfticks();
if (0) {
ilock(&nintrlock);
ninterrupt++;
if(t < ticks)
ninterruptticks += ticks-t;
else
ninterruptticks += t-ticks;
iunlock(&nintrlock);
}
USED(t, ticks);
clockintr = m->inclockintr == 1;
if (irqno == Loctmrirq)
m->inclockintr--;
intdismiss(icp, ack);
intrtime(m, irqno);
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
static void
datafault(Ureg *ureg, int user)
{
int x;
ulong inst, fsr;
uintptr va;
va = farget();
if (m->probing && !user) {
if (m->trapped++ > 0) {
dumpstackwithureg(ureg);
panic("trap: recursive probe %#lux", va);
}
ureg->pc += 4;
return;
}
inst = *(ulong*)(ureg->pc);
x = fsrget();
fsr = (x>>7) & 0x20 | (x>>6) & 0x10 | x & 0xf;
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
char buf[ERRMAX];
snprint(buf, sizeof buf,
"sys: alignment: pc %#lux va %#p\n",
ureg->pc, va);
postnote(up, 1, buf, NDebug);
} else {
dumpstackwithureg(ureg);
panic("kernel alignment: pc %#lux va %#p", ureg->pc, va);
}
break;
case 0x2:
panic("terminal exception at %#lux", ureg->pc);
break;
case 0x4:
case 0x6:
case 0x8:
case 0x28:
case 0x16:
case 0x36:
panic("external non-translation abort %#lux pc %#lux addr %#p",
fsr, ureg->pc, va);
break;
case 0xc:
case 0x2c:
case 0xe:
case 0x2e:
panic("external translation abort %#lux pc %#lux addr %#p",
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
char buf[ERRMAX];
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
}
void
trap(Ureg *ureg)
{
int clockintr, user, rem;
uintptr va, ifar, ifsr;
splhi();
if(up != nil)
rem = ((char*)ureg)-up->kstack;
else
rem = ((char*)ureg)-((char*)m+sizeof(Mach));
if(rem < 1024) {
iprint("trap: %d stack bytes left, up %#p ureg %#p m %#p cpu%d at pc %#lux\n",
rem, up, ureg, m, m->machno, ureg->pc);
dumpstackwithureg(ureg);
panic("trap: %d stack bytes left, up %#p ureg %#p at pc %#lux",
rem, up, ureg, ureg->pc);
}
m->perf.intrts = perfticks();
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
m->intr++;
clockintr = irq(ureg);
if(0 && up && !clockintr)
preempted();
break;
case PsrMabt:
va = ureg->pc;
ifsr = cprdsc(0, CpFSR, 0, CpIFSR);
ifsr = (ifsr>>7) & 0x8 | ifsr & 0x7;
switch(ifsr){
case 0x02:
if(user)
postnote(up, 1, "sys: breakpoint", NDebug);
else{
iprint("kernel bkpt: pc %#lux inst %#ux\n",
va, *(u32int*)va);
panic("kernel bkpt");
}
break;
default:
ifar = cprdsc(0, CpFAR, 0, CpIFAR);
if (va != ifar)
iprint("trap: cpu%d: i-fault va %#p != ifar %#p\n",
m->machno, va, ifar);
faultarm(ureg, va, user, 1);
break;
}
break;
case PsrMabt+1:
datafault(ureg, user);
break;
case PsrMund:
if(!user) {
if (ureg->pc & 3) {
iprint("rounding fault pc %#lux down to word\n",
ureg->pc);
ureg->pc &= ~3;
}
if (Debug)
iprint("mathemu: cpu%d fpon %d instr %#8.8lux at %#p\n",
m->machno, m->fpon, *(ulong *)ureg->pc,
ureg->pc);
dumpstackwithureg(ureg);
panic("cpu%d: undefined instruction: pc %#lux inst %#ux",
m->machno, ureg->pc, ((u32int*)ureg->pc)[0]);
} else if(seg(up, ureg->pc, 0) != nil &&
*(u32int*)ureg->pc == 0xD1200070)
postnote(up, 1, "sys: breakpoint", NDebug);
else if(fpuemu(ureg) == 0){
char buf[ERRMAX];
snprint(buf, sizeof buf,
"undefined instruction: pc %#lux instr %#8.8lux\n",
ureg->pc, *(ulong *)ureg->pc);
postnote(up, 1, buf, NDebug);
}
break;
}
splhi();
if(up && up->delaysched && clockintr){
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
memset(&ureg, 0, sizeof ureg);
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
delay(1000);
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
delay(3000);
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
delay(500);
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
ilock(&m->probelock);
m->trapped = 0;
m->probing = 1;
coherence();
v = *(ulong *)addr;
coherence();
m->probing = 0;
if (m->trapped)
v = -1;
iunlock(&m->probelock);
return v;
}