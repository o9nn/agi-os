#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "ureg.h"
#include "io.h"
#include <tos.h>
#include "../port/error.h"
#define setstatus(v)
typedef struct Handler Handler;
struct Handler {
void (*handler)(void *);
void *arg;
Handler *next;
ulong intrs;
};
ulong offintrs;
ulong intrcauses[ILmax+1];
int intr(Ureg*);
void kernfault(Ureg*, int);
void noted(Ureg*, Ureg**, ulong);
void rfnote(Ureg**);
char *excname[] =
{
"trap: external interrupt",
"trap: TLB modification (store to unwritable)",
"trap: TLB miss (load or fetch)",
"trap: TLB miss (store)",
"trap: address error (load or fetch)",
"trap: address error (store)",
"trap: bus error (fetch)",
"trap: bus error (data load or store)",
"trap: system call",
"breakpoint",
"trap: reserved instruction",
"trap: coprocessor unusable",
"trap: arithmetic overflow",
"trap: TRAP exception",
"trap: VCE (instruction)",
"trap: floating-point exception",
"trap: coprocessor 2 implementation-specific",
"trap: corextend unusable",
"trap: precise coprocessor 2 exception",
"trap: TLB read-inhibit",
"trap: TLB execute-inhibit",
"trap: undefined 21",
"trap: undefined 22",
"trap: WATCH exception",
"trap: machine checkcore",
"trap: undefined 25",
"trap: undefined 26",
"trap: undefined 27",
"trap: undefined 28",
"trap: undefined 29",
"trap: cache error",
"trap: VCE (data)",
};
char *fpcause[] =
{
"inexact operation",
"underflow",
"overflow",
"division by zero",
"invalid operation",
};
char *fpexcname(Ureg*, ulong, char*, uint);
#define FPEXPMASK (0x3f<<12)
struct {
char *name;
uint off;
} regname[] = {
"STATUS", Ureg_status,
"PC", Ureg_pc,
"SP", Ureg_sp,
"CAUSE",Ureg_cause,
"BADADDR", Ureg_badvaddr,
"TLBVIRT", Ureg_tlbvirt,
"HI", Ureg_hi,
"LO", Ureg_lo,
"R31", Ureg_r31,
"R30", Ureg_r30,
"R28", Ureg_r28,
"R27", Ureg_r27,
"R26", Ureg_r26,
"R25", Ureg_r25,
"R24", Ureg_r24,
"R23", Ureg_r23,
"R22", Ureg_r22,
"R21", Ureg_r21,
"R20", Ureg_r20,
"R19", Ureg_r19,
"R18", Ureg_r18,
"R17", Ureg_r17,
"R16", Ureg_r16,
"R15", Ureg_r15,
"R14", Ureg_r14,
"R13", Ureg_r13,
"R12", Ureg_r12,
"R11", Ureg_r11,
"R10", Ureg_r10,
"R9", Ureg_r9,
"R8", Ureg_r8,
"R7", Ureg_r7,
"R6", Ureg_r6,
"R5", Ureg_r5,
"R4", Ureg_r4,
"R3", Ureg_r3,
"R2", Ureg_r2,
"R1", Ureg_r1,
};
static Lock intrlock;
static Handler handlers[ILmax+1];
static char *
ptlb(ulong phys)
{
static char buf[4][32];
static int k;
char *p;
k = (k+1)&3;
p = buf[k];
p += snprint(p, sizeof buf[k] - (p - buf[k]), "(%#lux %lud ",
(phys<<6) & ~(BY2PG-1), (phys>>3)&7);
if(phys & 4)
*p++ = 'd';
if(phys & 2)
*p++ = 'v';
if(phys & 1)
*p++ = 'g';
*p++ = ')';
*p = 0;
return buf[k];
}
static void
kpteprint(Ureg *ur)
{
ulong i, tlbstuff[3];
KMap *k;
i = (ur->badvaddr & ~(2*BY2PG-1)) | TLBPID(tlbvirt());
print("tlbvirt=%#lux\n", i);
i = gettlbp(i, tlbstuff);
print("i=%lud v=%#lux p0=%s p1=%s\n",
i, tlbstuff[0], ptlb(tlbstuff[1]), ptlb(tlbstuff[2]));
i = (ur->badvaddr & ~KMAPADDR)>>15;
if(i > KPTESIZE){
print("kpte index = %lud ?\n", i);
return;
}
k = &kpte[i];
print("i=%lud, &k=%#p, k={v=%#lux, p0=%s, p1=%s, pg=%#p}\n",
i, k, k->virt, ptlb(k->phys0), ptlb(k->phys1), k->pg);
print("pg={pa=%#lux, va=%#lux}\n", k->pg->pa, k->pg->va);
}
void
kvce(Ureg *ur, int ecode)
{
char c;
Pte **p;
Page **pg;
Segment *s;
ulong addr, soff;
c = 'D';
if(ecode == CVCEI)
c = 'I';
print("Trap: VCE%c: addr=%#lux\n", c, ur->badvaddr);
if((ur->badvaddr & KSEGM) == KSEG3) {
kpteprint(ur);
return;
}
if(up && !(ur->badvaddr & KSEGM)) {
addr = ur->badvaddr;
s = seg(up, addr, 0);
if(s == 0){
print("kvce: no seg for %#lux\n", addr);
for(;;)
;
}
addr &= ~(BY2PG-1);
soff = addr - s->base;
p = &s->map[soff/PTEMAPMEM];
if(*p){
pg = &(*p)->pages[(soff&(PTEMAPMEM-1))/BY2PG];
if(*pg)
print("kvce: pa=%#lux, va=%#lux\n",
(*pg)->pa, (*pg)->va);
else
print("kvce: no *pg\n");
}else
print("kvce: no *p\n");
}
}
void
kexit(Ureg*)
{
Tos *tos;
tos = (Tos*)(USTKTOP-sizeof(Tos));
tos->kcycles += fastticks(&tos->cyclefreq) - up->kentry;
tos->pcycles = up->pcycles;
tos->pid = up->pid;
}
void
trap(Ureg *ur)
{
int ecode, clockintr, user, cop, x, fpchk;
ulong fpfcr31;
char buf[2*ERRMAX], buf1[ERRMAX], *fpexcep;
static int dumps;
ecode = (ur->cause>>2)&EXCMASK;
user = ur->status&KUSER;
if (ur->cause & TS)
panic("trap: tlb shutdown");
fpchk = 0;
if(user){
up->dbgreg = ur;
cycles(&up->kentry);
}
if (up && (char *)(ur) - up->kstack < 1024 && dumps++ == 0) {
iprint("trap: proc %ld kernel stack getting full\n", up->pid);
dumpregs(ur);
dumpstack();
}
if (up == nil &&
(char *)(ur) - (char *)m->stack < 1024 && dumps++ == 0) {
iprint("trap: cpu%d kernel stack getting full\n", m->machno);
dumpregs(ur);
dumpstack();
}
setstatus(getstatus() & ~EXL);
clockintr = 0;
switch(ecode){
case CINT:
clockintr = intr(ur);
break;
case CFPE:
panic("FP exception but no FPU");
#ifdef OLD_MIPS_EXAMPLE
fptrap(ur);
clrfpintr();
fpchk = 1;
#endif
break;
case CTLBM:
case CTLBL:
case CTLBS:
if(up == 0)
kernfault(ur, ecode);
if(!user && (ur->badvaddr & KSEGM) == KSEG3) {
kfault(ur);
break;
}
x = up->insyscall;
up->insyscall = 1;
spllo();
faultmips(ur, user, ecode);
up->insyscall = x;
break;
case CVCEI:
case CVCED:
kvce(ur, ecode);
goto Default;
case CWATCH:
if(!user)
panic("watchpoint trap from kernel mode pc=%#p",
ur->pc);
fpwatch(ur);
break;
case CCPU:
cop = (ur->cause>>28)&3;
if(user && up && cop == 1) {
if(up->fpstate & FPillegal) {
postnote(up, 1,
"sys: floating point in note handler",
NDebug);
break;
}
if (fpuemu(ur) < 0)
postnote(up, 1,
"sys: fp instruction not emulated",
NDebug);
else
fpchk = 1;
break;
}
Default:
default:
if(user) {
spllo();
snprint(buf, sizeof buf, "sys: %s", excname[ecode]);
postnote(up, 1, buf, NDebug);
break;
}
if (ecode == CADREL || ecode == CADRES)
iprint("kernel addr exception for va %#p pid %#ld %s\n",
ur->badvaddr, (up? up->pid: 0),
(up? up->text: ""));
print("cpu%d: kernel %s pc=%#lux\n",
m->machno, excname[ecode], ur->pc);
dumpregs(ur);
dumpstack();
if(m->machno == 0)
spllo();
exit(1);
}
if(fpchk) {
fpfcr31 = up->fpsave.fpstatus;
if((fpfcr31>>12) & ((fpfcr31>>7)|0x20) & 0x3f) {
spllo();
fpexcep = fpexcname(ur, fpfcr31, buf1, sizeof buf1);
snprint(buf, sizeof buf, "sys: fp: %s", fpexcep);
postnote(up, 1, buf, NDebug);
}
}
splhi();
if(up && up->delaysched && clockintr){
sched();
splhi();
}
if(user){
notify(ur);
kexit(ur);
}
setstatus(getstatus() | EXL);
}
static void
resetcounts(void)
{
int i;
Handler *hp;
ilock(&intrlock);
for (i = 0; i < nelem(handlers); i++)
for (hp = &handlers[i]; hp != nil; hp = hp->next)
hp->intrs = 0;
iunlock(&intrlock);
}
void
intrenable(int irq, void (*h)(void *), void *arg)
{
Handler *hp;
static int resetclock;
if (h == nil)
panic("intrenable: nil handler intr %d", irq);
if(irq < ILmin || irq >= nelem(handlers))
panic("intrenable: bad handler intr %d %#p", irq, h);
hp = &handlers[irq];
ilock(&intrlock);
if (hp->handler != nil) {
for (; hp->next != nil; hp = hp->next)
;
hp->next = smalloc(sizeof *hp);
hp = hp->next;
hp->next = nil;
}
hp->handler = h;
hp->arg = arg;
iunlock(&intrlock);
if (irq == ILduart0) {
*Apbintrsts = 0;
*Apbintrmask = 1 << Apbintruart;
coherence();
}
intron(1 << (ILshift + irq));
if (!resetclock) {
resetclock = 1;
addclock0link(resetcounts, 100);
}
}
void
intrshutdown(void)
{
introff(INTMASK);
}
static void
jabberoff(Ureg *ur, int irq, ulong bit)
{
introff(bit);
if (ur)
ur->status &= ~bit;
offintrs |= bit;
iprint("irq %d jabbering; shutting it down\n", irq);
}
ulong
pollall(Ureg *ur, ulong cause)
{
int i, intrs, sts;
ulong bit;
Handler *hp;
intrs = cause & (INTR6|INTR5|INTR4|INTR3|INTR2) & getstatus();
if(intrs == 0)
return cause;
ilock(&intrlock);
for (i = ILmax; i >= ILmin; i--) {
bit = 1 << (ILshift + i);
if (!(intrs & bit))
continue;
intrcauses[i]++;
for (hp = &handlers[i]; hp != nil; hp = hp->next)
if (hp->handler) {
if (i == ILduart0) {
sts = *Apbintrsts;
if((sts & (1 << Apbintruart)) == 0)
continue;
}
(*hp->handler)(hp->arg);
splhi();
if (++hp->intrs > 25000) {
jabberoff(ur, i, bit);
intrs &= ~bit;
hp->intrs = 0;
}
} else if (ur)
iprint("no handler for interrupt %d\n", i);
cause &= ~bit;
}
iunlock(&intrlock);
return cause;
}
int
intr(Ureg *ur)
{
int clockintr;
ulong cause;
m->intr++;
clockintr = 0;
cause = ur->cause & ur->status & INTMASK;
cause &= ~(INTR1|INTR0);
if (cause == 0)
print("spurious interrupt\n");
if(cause & INTR7){
clock(ur);
intrcauses[ILclock]++;
cause &= ~(1 << (ILclock + ILshift));
clockintr = 1;
}
cause = pollall(ur, cause);
if(cause){
print("intr: cause %#lux not handled\n", cause);
exit(1);
}
if(up && !clockintr)
preempted();
return clockintr;
}
char*
fpexcname(Ureg *ur, ulong fcr31, char *buf, uint size)
{
int i;
char *s;
ulong fppc;
fppc = ur->pc;
if(ur->cause & BD)
fppc += 4;
s = 0;
if(fcr31 & (1<<17))
s = "unimplemented operation";
else{
fcr31 >>= 7;
fcr31 &= (fcr31>>5);
for(i=0; i<5; i++)
if(fcr31 & (1<<i))
s = fpcause[i];
}
if(s == 0)
return "no floating point exception";
snprint(buf, size, "%s fppc=%#lux", s, fppc);
return buf;
}
#define KERNPC(x) (KTZERO <= (ulong)(x) && (ulong)(x) < (ulong)&etext)
void
kernfault(Ureg *ur, int code)
{
print("panic: kfault %s badvaddr=%#lux", excname[code], ur->badvaddr);
kpteprint(ur);
print("u=%#p status=%#lux pc=%#lux sp=%#lux\n",
up, ur->status, ur->pc, ur->sp);
delay(500);
panic("kfault");
}
static void
getpcsp(ulong *pc, ulong *sp)
{
*pc = getcallerpc(&pc);
*sp = (ulong)&pc-4;
}
void
callwithureg(void (*fn)(Ureg*))
{
Ureg ureg;
memset(&ureg, 0, sizeof ureg);
getpcsp((ulong*)&ureg.pc, (ulong*)&ureg.sp);
ureg.r31 = getcallerpc(&fn);
fn(&ureg);
}
static void
_dumpstack(Ureg *ureg)
{
ulong l, v, top, i;
extern ulong etext;
if(up == 0)
return;
print("ktrace /kernel/path %.8lux %.8lux %.8lux\n",
ureg->pc, ureg->sp, ureg->r31);
top = (ulong)up->kstack + KSTACK;
i = 0;
for(l=ureg->sp; l < top; l += BY2WD) {
v = *(ulong*)l;
if(KTZERO < v && v < (ulong)&etext) {
print("%.8lux=%.8lux ", l, v);
if((++i%4) == 0){
print("\n");
delay(200);
}
}
}
print("\n");
}
void
dumpstack(void)
{
callwithureg(_dumpstack);
}
static ulong
R(Ureg *ur, int i)
{
uchar *s;
s = (uchar*)ur;
return *(ulong*)(s + regname[i].off - Uoffset);
}
void
dumpregs(Ureg *ur)
{
int i;
if(up)
print("registers for %s %lud\n", up->text, up->pid);
else
print("registers for kernel\n");
for(i = 0; i < nelem(regname); i += 2)
print("%s\t%#.8lux\t%s\t%#.8lux\n",
regname[i].name, R(ur, i),
regname[i+1].name, R(ur, i+1));
}
int
notify(Ureg *ur)
{
int l, s;
ulong sp;
Note *n;
if(up->procctl)
procctl(up);
if(up->nnote == 0)
return 0;
s = spllo();
qlock(&up->debug);
up->fpstate |= FPillegal;
up->notepending = 0;
n = &up->note[0];
if(strncmp(n->msg, "sys:", 4) == 0) {
l = strlen(n->msg);
if(l > ERRMAX-15)
l = ERRMAX-15;
seprint(n->msg+l, &n->msg[sizeof n->msg], " pc=%#lux", ur->pc);
}
if(n->flag != NUser && (up->notified || up->notify==0)) {
if(n->flag == NDebug)
pprint("suicide: %s\n", n->msg);
qunlock(&up->debug);
pexit(n->msg, n->flag!=NDebug);
}
if(up->notified) {
qunlock(&up->debug);
splx(s);
return 0;
}
if(!up->notify) {
qunlock(&up->debug);
pexit(n->msg, n->flag!=NDebug);
}
sp = ur->usp & ~(BY2V-1);
sp -= sizeof(Ureg);
if(!okaddr((ulong)up->notify, BY2WD, 0) ||
!okaddr(sp-ERRMAX-4*BY2WD, sizeof(Ureg)+ERRMAX+4*BY2WD, 1)) {
pprint("suicide: bad address or sp in notify\n");
qunlock(&up->debug);
pexit("Suicide", 0);
}
memmove((Ureg*)sp, ur, sizeof(Ureg));
*(Ureg**)(sp-BY2WD) = up->ureg;
up->ureg = (void*)sp;
sp -= BY2WD+ERRMAX;
memmove((char*)sp, up->note[0].msg, ERRMAX);
sp -= 3*BY2WD;
*(ulong*)(sp+2*BY2WD) = sp+3*BY2WD;
ur->r1 = (long)up->ureg;
((ulong*)sp)[1] = (ulong)up->ureg;
((ulong*)sp)[0] = 0;
ur->usp = sp;
ur->pc = (ulong)up->notify;
up->notified = 1;
up->nnote--;
memmove(&up->lastnote, &up->note[0], sizeof(Note));
memmove(&up->note[0], &up->note[1], up->nnote*sizeof(Note));
qunlock(&up->debug);
splx(s);
return 1;
}
int
validstatus(ulong kstatus, ulong ustatus)
{
if((kstatus & INTMASK) != (ustatus & INTMASK))
return 0;
if((ustatus&(KSU|ERL|EXL|IE)) != (KUSER|EXL|IE))
return 0;
if(ustatus & (0xFFFF0000&~CU1))
return 0;
return 1;
}
void
noted(Ureg *kur, Ureg **urp, ulong arg0)
{
Ureg *nur;
ulong oureg, sp;
qlock(&up->debug);
if(arg0!=NRSTR && !up->notified) {
qunlock(&up->debug);
pprint("call to noted() when not notified\n");
pexit("Suicide", 0);
}
up->notified = 0;
up->fpstate &= ~FPillegal;
nur = up->ureg;
oureg = (ulong)nur;
if((oureg & (BY2WD-1))
|| !okaddr((ulong)oureg-BY2WD, BY2WD+sizeof(Ureg), 0)){
pprint("bad up->ureg in noted or call to noted() when not notified\n");
qunlock(&up->debug);
pexit("Suicide", 0);
}
if(!validstatus(kur->status, nur->status)) {
qunlock(&up->debug);
pprint("bad noted ureg status %#lux\n", nur->status);
pexit("Suicide", 0);
}
memmove(*urp, up->ureg, sizeof(Ureg));
switch(arg0) {
case NCONT:
case NRSTR:
if(!okaddr(nur->pc, BY2WD, 0) || !okaddr(nur->usp, BY2WD, 0)){
pprint("suicide: trap in noted\n");
qunlock(&up->debug);
pexit("Suicide", 0);
}
up->ureg = (Ureg*)(*(ulong*)(oureg-BY2WD));
qunlock(&up->debug);
splhi();
break;
case NSAVE:
if(!okaddr(nur->pc, BY2WD, 0) || !okaddr(nur->usp, BY2WD, 0)){
pprint("suicide: trap in noted\n");
qunlock(&up->debug);
pexit("Suicide", 0);
}
qunlock(&up->debug);
sp = oureg-4*BY2WD-ERRMAX;
splhi();
(*urp)->sp = sp;
((ulong*)sp)[1] = oureg;
((ulong*)sp)[0] = 0;
(*urp)->r1 = oureg;
break;
default:
pprint("unknown noted arg %#lux\n", arg0);
up->lastnote.flag = NDebug;
case NDFLT:
if(up->lastnote.flag == NDebug)
pprint("suicide: %s\n", up->lastnote.msg);
qunlock(&up->debug);
pexit(up->lastnote.msg, up->lastnote.flag!=NDebug);
}
}
#include "../port/systab.h"
static Ref goodsyscall;
static Ref totalsyscall;
static void
sctracesetup(ulong scallnr, ulong sp, uintptr pc, vlong *startnsp)
{
if(up->procctl == Proc_tracesyscall){
if(sp < (USTKTOP-BY2PG) || sp > (USTKTOP-sizeof(Sargs)-BY2WD))
validaddr(sp, sizeof(Sargs)+BY2WD, 0);
syscallfmt(scallnr, pc, (va_list)(sp+BY2WD));
up->procctl = Proc_stopme;
procctl(up);
if(up->syscalltrace)
free(up->syscalltrace);
up->syscalltrace = nil;
*startnsp = todget(nil);
}
}
static void
sctracefinish(ulong scallnr, ulong sp, int ret, vlong startns)
{
int s;
if(up->procctl == Proc_tracesyscall){
up->procctl = Proc_stopme;
sysretfmt(scallnr, (va_list)(sp+BY2WD), ret,
startns, todget(nil));
s = splhi();
procctl(up);
splx(s);
if(up->syscalltrace)
free(up->syscalltrace);
up->syscalltrace = nil;
}
}
long
syscall(Ureg *aur)
{
int i;
volatile long ret;
ulong sp, scallnr;
vlong startns;
char *e;
Ureg *ur;
cycles(&up->kentry);
incref(&totalsyscall);
m->syscall++;
up->insyscall = 1;
ur = aur;
up->pc = ur->pc;
up->dbgreg = aur;
ur->cause = 16<<2;
scallnr = ur->r1;
up->scallnr = ur->r1;
sp = ur->sp;
sctracesetup(scallnr, sp, ur->pc, &startns);
setstatus(getstatus() & ~EXL);
spllo();
up->nerrlab = 0;
ret = -1;
if(!waserror()) {
if(scallnr >= nsyscall || systab[scallnr] == 0){
pprint("bad sys call number %ld pc %#lux\n",
scallnr, ur->pc);
postnote(up, 1, "sys: bad sys call", NDebug);
error(Ebadarg);
}
if(sp & (BY2WD-1)){
pprint("odd sp in sys call pc %#lux sp %#lux\n",
ur->pc, ur->sp);
postnote(up, 1, "sys: odd stack", NDebug);
error(Ebadarg);
}
if(sp<(USTKTOP-BY2PG) || sp>(USTKTOP-sizeof(Sargs)-BY2WD))
validaddr(sp, sizeof(Sargs)+BY2WD, 0);
up->s = *((Sargs*)(sp+BY2WD));
up->psstate = sysctab[scallnr];
ret = systab[scallnr](up->s.args);
poperror();
}else{
e = up->syserrstr;
up->syserrstr = up->errstr;
up->errstr = e;
if(0 && up->pid == 1)
print("[%lud %s] syscall %lud: %s\n",
up->pid, up->text, scallnr, up->errstr);
}
if(up->nerrlab){
print("bad errstack [%lud]: %d extra\n", scallnr, up->nerrlab);
for(i = 0; i < NERR; i++)
print("sp=%#lux pc=%#lux\n",
up->errlab[i].sp, up->errlab[i].pc);
panic("error stack");
}
sctracefinish(scallnr, sp, ret, startns);
ur->pc += 4;
ur->r1 = ret;
up->psstate = 0;
up->insyscall = 0;
if(scallnr == NOTED) {
noted(ur, &aur, *(ulong*)(sp+BY2WD));
ret = ur->r1;
}
incref(&goodsyscall);
splhi();
if(scallnr!=RFORK && (up->procctl || up->nnote)){
ur->r1 = ret;
notify(ur);
}
if(up->delaysched)
sched();
kexit(ur);
setstatus(getstatus() | EXL);
return ret;
}
void
forkchild(Proc *p, Ureg *ur)
{
Ureg *cur;
p->sched.sp = (ulong)p->kstack+KSTACK-UREGSIZE;
p->sched.pc = (ulong)forkret;
cur = (Ureg*)(p->sched.sp+2*BY2WD);
memmove(cur, ur, sizeof(Ureg));
cur->pc += 4;
p->psstate = 0;
p->insyscall = 0;
}
static
void
linkproc(void)
{
spllo();
up->kpfun(up->kparg);
pexit("kproc exiting", 0);
}
void
kprocchild(Proc *p, void (*func)(void*), void *arg)
{
p->sched.pc = (ulong)linkproc;
p->sched.sp = (ulong)p->kstack+KSTACK;
p->kpfun = func;
p->kparg = arg;
}
long
execregs(ulong entry, ulong ssize, ulong nargs)
{
Ureg *ur;
ulong *sp;
sp = (ulong*)(USTKTOP - ssize);
*--sp = nargs;
ur = (Ureg*)up->dbgreg;
ur->usp = (ulong)sp;
ur->pc = entry - 4;
up->fpsave.fpstatus = initfp.fpstatus;
return USTKTOP-sizeof(Tos);
}
ulong
userpc(void)
{
Ureg *ur;
ur = (Ureg*)up->dbgreg;
return ur->pc;
}
void
setregisters(Ureg *xp, char *pureg, char *uva, int n)
{
ulong status;
status = xp->status;
memmove(pureg, uva, n);
xp->status = status;
}
void
setkernur(Ureg *xp, Proc *p)
{
xp->pc = p->sched.pc;
xp->sp = p->sched.sp;
xp->r24 = (ulong)p;
xp->r31 = (ulong)sched;
}
ulong
dbgpc(Proc *p)
{
Ureg *ur;
ur = p->dbgreg;
if(ur == 0)
return 0;
return ur->pc;
}