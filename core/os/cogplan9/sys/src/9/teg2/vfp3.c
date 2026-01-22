#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "ureg.h"
#include "arm.h"
enum {
VFPv2 = 2,
VFPv3 = 3,
};
enum {
Fpsid = 0,
Fpscr = 1,
Mvfr1 = 6,
Mvfr0 = 7,
Fpexc = 8,
Fpinst= 9,
Fpinst2=10,
};
enum {
Fpex = 1u << 31,
Fpenabled = 1 << 30,
Fpdex = 1 << 29,
Fpmbc = Fpdex,
Stride = MASK(2) << 20,
Len = MASK(3) << 16,
Dn= 1 << 25,
Fz= 1 << 24,
FPIDNRM = 1 << 15,
Alltraps = FPIDNRM | FPINEX | FPUNFL | FPOVFL | FPZDIV | FPINVAL,
FPAIDNRM = 1 << 7,
Allexc = FPAIDNRM | FPAINEX | FPAUNFL | FPAOVFL | FPAZDIV | FPAINVAL,
Allcc = MASK(4) << 28,
};
enum {
Cpaccnosimd = 1u << 31,
Cpaccd16 = 1 << 30,
};
static char *
subarch(int impl, uint sa)
{
static char *armarchs[] = {
"VFPv1 (unsupported)",
"VFPv2",
"VFPv3+ with common VFP subarch v2",
"VFPv3+ with null subarch",
"VFPv3+ with common VFP subarch v3",
};
if (impl != 'A' || sa >= nelem(armarchs))
return "GOK";
else
return armarchs[sa];
}
static char *
implement(uchar impl)
{
if (impl == 'A')
return "arm";
else
return "unknown";
}
static int
havefp(void)
{
int gotfp;
ulong acc, sid;
if (m->havefpvalid)
return m->havefp;
m->havefp = 0;
gotfp = 1 << CpFP | 1 << CpDFP;
cpwrsc(0, CpCONTROL, 0, CpCPaccess, MASK(28));
acc = cprdsc(0, CpCONTROL, 0, CpCPaccess);
if ((acc & (MASK(2) << (2*CpFP))) == 0) {
gotfp &= ~(1 << CpFP);
print("fpon: no single FP coprocessor\n");
}
if ((acc & (MASK(2) << (2*CpDFP))) == 0) {
gotfp &= ~(1 << CpDFP);
print("fpon: no double FP coprocessor\n");
}
if (!gotfp) {
print("fpon: no FP coprocessors\n");
m->havefpvalid = 1;
return 0;
}
m->fpon = 1;
sid = fprd(Fpsid);
m->fpon = 0;
switch((sid >> 16) & MASK(7)){
case 0:
break;
case 1:
m->havefp = VFPv2;
m->fpnregs = 16;
break;
default:
m->havefp = VFPv3;
m->fpnregs = (acc & Cpaccd16) ? 16 : 32;
break;
}
if (m->machno == 0)
print("fp: %d registers, %s simd\n", m->fpnregs,
(acc & Cpaccnosimd? " no": ""));
m->havefpvalid = 1;
return 1;
}
void
fpoff(void)
{
if (m->fpon) {
fpwr(Fpexc, 0);
m->fpon = 0;
}
}
void
fpononly(void)
{
if (!m->fpon && havefp()) {
fpwr(Fpexc, Fpenabled);
m->fpon = 1;
}
}
static void
fpcfg(void)
{
int impl;
ulong sid;
static int printed;
m->fpscr = Dn | Fz | FPRNR | (FPINVAL | FPZDIV | FPOVFL) & ~Alltraps;
fpwr(Fpscr, m->fpscr);
m->fpconfiged = 1;
if (printed)
return;
sid = fprd(Fpsid);
impl = sid >> 24;
print("fp: %s arch %s; rev %ld\n", implement(impl),
subarch(impl, (sid >> 16) & MASK(7)), sid & MASK(4));
printed = 1;
}
void
fpinit(void)
{
if (havefp()) {
fpononly();
fpcfg();
}
}
void
fpon(void)
{
if (havefp()) {
fpononly();
if (m->fpconfiged)
fpwr(Fpscr, (fprd(Fpscr) & Allcc) | m->fpscr);
else
fpcfg();
}
}
void
fpclear(void)
{
fpon();
fpwr(Fpexc, fprd(Fpexc) & ~Fpmbc);
}
void
fpunotify(Ureg*)
{
if(up->fpstate == FPactive){
fpsave(&up->fpsave);
up->fpstate = FPinactive;
}
up->fpstate |= FPillegal;
}
void
fpunoted(void)
{
up->fpstate &= ~FPillegal;
}
void
fpusysrfork(Ureg*)
{
if(up->fpstate == FPactive){
fpsave(&up->fpsave);
up->fpstate = FPinactive;
}
}
void
fpusysrforkchild(Proc *p, Ureg *, Proc *up)
{
p->fpstate = up->fpstate & ~FPillegal;
}
void
fpsave(FPsave *fps)
{
int n;
fpon();
fps->control = fps->status = fprd(Fpscr);
assert(m->fpnregs);
for (n = 0; n < m->fpnregs; n++)
fpsavereg(n, (uvlong *)fps->regs[n]);
fpoff();
}
static void
fprestore(Proc *p)
{
int n;
fpon();
fpwr(Fpscr, p->fpsave.control);
m->fpscr = fprd(Fpscr) & ~Allcc;
assert(m->fpnregs);
for (n = 0; n < m->fpnregs; n++)
fprestreg(n, *(uvlong *)p->fpsave.regs[n]);
}
void
fpuprocsave(Proc *p)
{
if(p->fpstate == FPactive){
if(p->state == Moribund)
fpclear();
else{
fpsave(&p->fpsave);
}
p->fpstate = FPinactive;
}
}
void
fpuprocrestore(Proc *)
{
}
void
fpusysprocsetup(Proc *p)
{
p->fpstate = FPinit;
fpoff();
}
static void
mathnote(void)
{
ulong status;
char *msg, note[ERRMAX];
status = up->fpsave.status;
if (status & FPAINEX)
msg = "inexact";
else if (status & FPAOVFL)
msg = "overflow";
else if (status & FPAUNFL)
msg = "underflow";
else if (status & FPAZDIV)
msg = "divide by zero";
else if (status & FPAINVAL)
msg = "bad operation";
else
msg = "spurious";
snprint(note, sizeof note, "sys: fp: %s fppc=%#p status=%#lux",
msg, up->fpsave.pc, status);
postnote(up, 1, note, NDebug);
}
static void
mathemu(Ureg *)
{
switch(up->fpstate){
case FPemu:
error("illegal instruction: VFP opcode in emulated mode");
case FPinit:
fpinit();
up->fpstate = FPactive;
break;
case FPinactive:
if(up->fpsave.status & (FPAINEX|FPAUNFL|FPAOVFL|FPAZDIV|FPAINVAL)){
mathnote();
break;
}
fprestore(up);
up->fpstate = FPactive;
break;
case FPactive:
error("illegal instruction: bad vfp fpu opcode");
break;
}
fpclear();
}
void
fpstuck(uintptr pc)
{
if (m->fppc == pc && m->fppid == up->pid) {
m->fpcnt++;
if (m->fpcnt > 4)
panic("fpuemu: cpu%d stuck at pid %ld %s pc %#p "
"instr %#8.8lux", m->machno, up->pid, up->text,
pc, *(ulong *)pc);
} else {
m->fppid = up->pid;
m->fppc = pc;
m->fpcnt = 0;
}
}
enum {
N = 1<<31,
Z = 1<<30,
C = 1<<29,
V = 1<<28,
REGPC = 15,
};
static int
condok(int cc, int c)
{
switch(c){
case 0:
return cc&Z;
case 1:
return (cc&Z) == 0;
case 2:
return cc&C;
case 3:
return (cc&C) == 0;
case 4:
return cc&N;
case 5:
return (cc&N) == 0;
case 6:
return cc&V;
case 7:
return (cc&V) == 0;
case 8:
return cc&C && (cc&Z) == 0;
case 9:
return (cc&C) == 0 || cc&Z;
case 10:
return (~cc&(N|V))==0 || (cc&(N|V)) == 0;
case 11:
return (cc&(N|V))==N || (cc&(N|V))==V;
case 12:
return (cc&Z) == 0 && ((~cc&(N|V))==0 || (cc&(N|V))==0);
case 13:
return (cc&Z) || (cc&(N|V))==N || (cc&(N|V))==V;
case 14:
return 1;
case 15:
return 0;
}
return 0;
}
int
fpuemu(Ureg* ureg)
{
int s, nfp, cop, op;
uintptr pc;
if(waserror()){
postnote(up, 1, up->errstr, NDebug);
return 1;
}
if(up->fpstate & FPillegal)
error("floating point in note handler");
nfp = 0;
pc = ureg->pc;
validaddr(pc, 4, 0);
if(!condok(ureg->psr, *(ulong*)pc >> 28))
iprint("fpuemu: conditional instr shouldn't have got here\n");
op = (*(ulong *)pc >> 24) & MASK(4);
cop = (*(ulong *)pc >> 8) & MASK(4);
if(m->fpon)
fpstuck(pc);
if (ISFPAOP(cop, op)) {
s = spllo();
if(waserror()){
splx(s);
nexterror();
}
nfp = fpiarm(ureg);
if (nfp > 1)
m->fppc = m->fpcnt = 0;
splx(s);
poperror();
} else if (ISVFPOP(cop, op)) {
mathemu(ureg);
nfp = 1;
}
poperror();
return nfp;
}