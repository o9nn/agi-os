#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"init.h"
#include	"pool.h"
#include	"reboot.h"
#include	"mp.h"
Mach *m;
#define BOOTLINE	((char*)CONFADDR)
#define BOOTLINELEN	64
#define BOOTARGS	((char*)(CONFADDR+BOOTLINELEN))
#define	BOOTARGSLEN	(4096-0x200-BOOTLINELEN)
#define	MAXCONF		64
char bootdisk[KNAMELEN];
Conf conf;
char *confname[MAXCONF];
char *confval[MAXCONF];
int nconf;
uchar *sp;
int delaylink;
int idle_spin, idle_if_nproc;
static void
options(void)
{
long i, n;
char *cp, *line[MAXCONF], *p, *q;
cp = BOOTARGS;
cp[BOOTARGSLEN-1] = 0;
p = cp;
for(q = cp; *q; q++){
if(*q == '\r')
continue;
if(*q == '\t')
*q = ' ';
*p++ = *q;
}
*p = 0;
n = getfields(cp, line, MAXCONF, 1, "\n");
for(i = 0; i < n; i++){
if(*line[i] == '#')
continue;
cp = strchr(line[i], '=');
if(cp == nil)
continue;
*cp++ = '\0';
confname[nconf] = line[i];
confval[nconf] = cp;
nconf++;
}
}
extern void mmuinit0(void);
extern void (*i8237alloc)(void);
void
main(void)
{
cgapost(0);
mach0init();
options();
ioinit();
i8250console();
quotefmtinstall();
screeninit();
print("\nPlan 9\n");
trapinit0();
mmuinit0();
kbdinit();
i8253init();
cpuidentify();
meminit();
confinit();
archinit();
xinit();
if(i8237alloc != nil)
i8237alloc();
trapinit();
printinit();
cpuidprint();
mmuinit();
if(arch->intrinit)
arch->intrinit();
timersinit();
mathinit();
kbdenable();
if(arch->clockenable)
arch->clockenable();
procinit0();
initseg();
if(delaylink){
bootlinks();
pcimatch(0, 0, 0);
}else
links();
conf.monitor = 1;
chandevreset();
cgapost(0xcd);
pageinit();
i8253link();
swapinit();
userinit();
active.thunderbirdsarego = 1;
cgapost(0x99);
schedinit();
}
void
mach0init(void)
{
conf.nmach = 1;
MACHP(0) = (Mach*)CPU0MACH;
m->pdb = (ulong*)CPU0PDB;
m->gdt = (Segdesc*)CPU0GDT;
machinit();
active.machs = 1;
active.exiting = 0;
}
void
machinit(void)
{
int machno;
ulong *pdb;
Segdesc *gdt;
machno = m->machno;
pdb = m->pdb;
gdt = m->gdt;
memset(m, 0, sizeof(Mach));
m->machno = machno;
m->pdb = pdb;
m->gdt = gdt;
m->perf.period = 1;
m->loopconst = 100000;
}
void
init0(void)
{
int i;
char buf[2*KNAMELEN];
up->nerrlab = 0;
spllo();
up->slash = namec("#/", Atodir, 0, 0);
pathclose(up->slash->path);
up->slash->path = newpath("/");
up->dot = cclone(up->slash);
chandevinit();
if(!waserror()){
snprint(buf, sizeof(buf), "%s %s", arch->id, conffile);
ksetenv("terminal", buf, 0);
ksetenv("cputype", "386", 0);
if(cpuserver)
ksetenv("service", "cpu", 0);
else
ksetenv("service", "terminal", 0);
for(i = 0; i < nconf; i++){
if(confname[i][0] != '*')
ksetenv(confname[i], confval[i], 0);
ksetenv(confname[i], confval[i], 1);
}
poperror();
}
kproc("alarm", alarmkproc, 0);
cgapost(0x9);
touser(sp);
}
void
userinit(void)
{
void *v;
Proc *p;
Segment *s;
Page *pg;
p = newproc();
p->pgrp = newpgrp();
p->egrp = smalloc(sizeof(Egrp));
p->egrp->ref = 1;
p->fgrp = dupfgrp(nil);
p->rgrp = newrgrp();
p->procmode = 0640;
kstrdup(&eve, "");
kstrdup(&p->text, "*init*");
kstrdup(&p->user, eve);
p->fpstate = FPinit;
fpoff();
p->sched.pc = (ulong)init0;
p->sched.sp = (ulong)p->kstack+KSTACK-(sizeof(Sargs)+BY2WD);
s = newseg(SG_STACK, USTKTOP-USTKSIZE, USTKSIZE/BY2PG);
p->seg[SSEG] = s;
pg = newpage(0, 0, USTKTOP-BY2PG);
v = tmpmap(pg);
memset(v, 0, BY2PG);
segpage(s, pg);
bootargs(v);
tmpunmap(v);
s = newseg(SG_TEXT, UTZERO, 1);
s->flushme++;
p->seg[TSEG] = s;
pg = newpage(0, 0, UTZERO);
memset(pg->cachectl, PG_TXTFLUSH, sizeof(pg->cachectl));
segpage(s, pg);
v = tmpmap(pg);
memset(v, 0, BY2PG);
memmove(v, initcode, sizeof initcode);
tmpunmap(v);
ready(p);
}
uchar *
pusharg(char *p)
{
int n;
n = strlen(p)+1;
sp -= n;
memmove(sp, p, n);
return sp;
}
void
bootargs(void *base)
{
int i, ac;
uchar *av[32];
uchar **lsp;
char *cp = BOOTLINE;
char buf[64];
sp = (uchar*)base + BY2PG - MAXSYSARG*BY2WD;
ac = 0;
av[ac++] = pusharg("/386/9dos");
cp[BOOTLINELEN-1] = 0;
buf[0] = 0;
if(strncmp(cp, "fd", 2) == 0){
sprint(buf, "local!#f/fd%lddisk", strtol(cp+2, 0, 0));
av[ac++] = pusharg(buf);
} else if(strncmp(cp, "sd", 2) == 0){
sprint(buf, "local!#S/sd%c%c/fs", *(cp+2), *(cp+3));
av[ac++] = pusharg(buf);
} else if(strncmp(cp, "ether", 5) == 0)
av[ac++] = pusharg("-n");
sp = (uchar*)((ulong)sp & ~3);
sp -= (ac+1)*sizeof(sp);
lsp = (uchar**)sp;
for(i = 0; i < ac; i++)
*lsp++ = av[i] + ((USTKTOP - BY2PG) - (ulong)base);
*lsp = 0;
sp += (USTKTOP - BY2PG) - (ulong)base - sizeof(ulong);
}
char*
getconf(char *name)
{
int i;
for(i = 0; i < nconf; i++)
if(cistrcmp(confname[i], name) == 0)
return confval[i];
return 0;
}
static void
writeconf(void)
{
char *p, *q;
int n;
p = getconfenv();
if(waserror()) {
free(p);
nexterror();
}
for(q=p; *q; q++) {
q += strlen(q);
*q = '=';
q += strlen(q);
*q = '\n';
}
n = q - p + 1;
if(n >= BOOTARGSLEN)
error("kernel configuration too large");
memset(BOOTLINE, 0, BOOTLINELEN);
memmove(BOOTARGS, p, n);
poperror();
free(p);
}
void
confinit(void)
{
char *p;
int i, userpcnt;
ulong kpages;
if(p = getconf("*kernelpercent"))
userpcnt = 100 - strtol(p, 0, 0);
else
userpcnt = 0;
conf.npage = 0;
for(i=0; i<nelem(conf.mem); i++)
conf.npage += conf.mem[i].npage;
conf.nproc = 100 + ((conf.npage*BY2PG)/MB)*5;
if(cpuserver)
conf.nproc *= 3;
if(conf.nproc > 2000)
conf.nproc = 2000;
conf.nimage = 200;
conf.nswap = conf.nproc*80;
conf.nswppo = 4096;
if(cpuserver) {
if(userpcnt < 10)
userpcnt = 70;
kpages = conf.npage - (conf.npage*userpcnt)/100;
if(kpages > (64*MB + conf.npage*sizeof(Page))/BY2PG){
kpages = (64*MB + conf.npage*sizeof(Page))/BY2PG;
conf.nimage = 2000;
kpages += (conf.nproc*KSTACK)/BY2PG;
}
} else {
if(userpcnt < 10) {
if(conf.npage*BY2PG < 16*MB)
userpcnt = 40;
else
userpcnt = 60;
}
kpages = conf.npage - (conf.npage*userpcnt)/100;
if(conf.npage*BY2PG < 16*MB)
imagmem->minarena = 4*1024*1024;
}
if(kpages > ((ulong)-KZERO)/BY2PG)
kpages = ((ulong)-KZERO)/BY2PG;
conf.upages = conf.npage - kpages;
conf.ialloc = (kpages/2)*BY2PG;
kpages *= BY2PG;
kpages -= conf.upages*sizeof(Page)
+ conf.nproc*sizeof(Proc)
+ conf.nimage*sizeof(Image)
+ conf.nswap
+ conf.nswppo*sizeof(Page);
mainmem->maxsize = kpages;
if(!cpuserver){
imagmem->maxsize = kpages;
}
}
static char* mathmsg[] =
{
nil,
"denormalized operand",
"division by zero",
"numeric overflow",
"numeric underflow",
"precision loss",
};
static void
mathstate(ulong *stsp, ulong *pcp, ulong *ctlp)
{
ulong sts, fpc, ctl;
FPsave *f = &up->fpsave;
if(fpsave == fpx87save){
sts = f->status;
fpc = f->pc;
ctl = f->control;
} else {
sts = f->fsw;
fpc = f->fpuip;
ctl = f->fcw;
}
if(stsp)
*stsp = sts;
if(pcp)
*pcp = fpc;
if(ctlp)
*ctlp = ctl;
}
static void
mathnote(void)
{
int i;
ulong status, pc;
char *msg, note[ERRMAX];
mathstate(&status, &pc, nil);
msg = "unknown exception";
for(i = 1; i <= 5; i++){
if(!((1<<i) & status))
continue;
msg = mathmsg[i];
break;
}
if(status & 0x01){
if(status & 0x40){
if(status & 0x200)
msg = "stack overflow";
else
msg = "stack underflow";
}else
msg = "invalid operation";
}
snprint(note, sizeof note, "sys: fp: %s fppc=%#lux status=%#lux",
msg, pc, status);
postnote(up, 1, note, NDebug);
}
void
fpssesave(FPsave *fps)
{
FPsave *afps;
afps = (FPsave *)ROUND(((uintptr)fps), FPalign);
fpssesave0(afps);
if (fps != afps)
memmove(fps, afps, sizeof(FPssestate) - FPalign);
}
void
fpsserestore(FPsave *fps)
{
FPsave *afps;
afps = (FPsave *)ROUND(((uintptr)fps), FPalign);
if (fps != afps) {
if (m->fpsavalign == nil)
m->fpsavalign = mallocalign(sizeof(FPssestate),
FPalign, 0, 0);
if (m->fpsavalign)
afps = m->fpsavalign;
memmove(afps, fps, sizeof(FPssestate) - FPalign);
}
fpsserestore0(afps);
if (fps != afps && afps != m->fpsavalign)
memmove(fps, afps, sizeof(FPssestate) - FPalign);
}
static void
matherror(Ureg *ur, void*)
{
ulong status, pc;
if(!(m->cpuiddx & Fpuonchip))
outb(0xF0, 0xFF);
fpenv(&up->fpsave);
mathnote();
if((ur->pc & 0xf0000000) == KZERO){
mathstate(&status, &pc, nil);
panic("fp: status %#lux fppc=%#lux pc=%#lux", status, pc, ur->pc);
}
}
static void
mathemu(Ureg *ureg, void*)
{
ulong status, control;
if(up->fpstate & FPillegal){
postnote(up, 1, "sys: floating point in note handler", NDebug);
return;
}
switch(up->fpstate){
case FPinit:
fpinit();
up->fpstate = FPactive;
break;
case FPinactive:
mathstate(&status, nil, &control);
if((status & ~control) & 0x07F){
mathnote();
break;
}
fprestore(&up->fpsave);
up->fpstate = FPactive;
break;
case FPactive:
panic("math emu pid %ld %s pc %#lux",
up->pid, up->text, ureg->pc);
break;
}
}
static void
mathover(Ureg*, void*)
{
pexit("math overrun", 0);
}
void
mathinit(void)
{
trapenable(VectorCERR, matherror, 0, "matherror");
if(X86FAMILY(m->cpuidax) == 3)
intrenable(IrqIRQ13, matherror, 0, BUSUNKNOWN, "matherror");
trapenable(VectorCNA, mathemu, 0, "mathemu");
trapenable(VectorCSO, mathover, 0, "mathover");
}
void
procsetup(Proc*p)
{
p->fpstate = FPinit;
fpoff();
}
void
procrestore(Proc *p)
{
uvlong t;
if(p->kp)
return;
cycles(&t);
p->pcycles -= t;
}
void
procsave(Proc *p)
{
uvlong t;
cycles(&t);
p->pcycles += t;
if(p->fpstate == FPactive){
if(p->state == Moribund)
fpclear();
else{
fpsave(&p->fpsave);
}
p->fpstate = FPinactive;
}
mmuflushtlb(PADDR(m->pdb));
}
static void
shutdown(int ispanic)
{
int ms, once;
lock(&active);
if(ispanic)
active.ispanic = ispanic;
else if(m->machno == 0 && (active.machs & (1<<m->machno)) == 0)
active.ispanic = 0;
once = active.machs & (1<<m->machno);
active.machs &= ~(1<<m->machno);
active.exiting = 1;
unlock(&active);
if(once)
iprint("cpu%d: exiting\n", m->machno);
spllo();
for(ms = 5*1000; ms > 0; ms -= TK2MS(2)){
delay(TK2MS(2));
if(active.machs == 0 && consactive() == 0)
break;
}
if(active.ispanic){
if(!cpuserver)
for(;;)
halt();
if(getconf("*debug"))
delay(5*60*1000);
else
delay(10000);
}else
delay(1000);
}
void
reboot(void *entry, void *code, ulong size)
{
void (*f)(ulong, ulong, ulong);
ulong *pdb;
writeconf();
if (m->machno != 0) {
procwired(up, 0);
sched();
}
if(conf.nmach > 1) {
lock(&active);
active.rebooting = 1;
unlock(&active);
shutdown(0);
if(arch->resetothers)
arch->resetothers();
delay(20);
}
active.machs = 0;
if (m->machno != 0)
print("on cpu%d (not 0)!\n", m->machno);
print("shutting down...\n");
delay(200);
splhi();
serialoq = nil;
chandevshutdown();
arch->introff();
pdb = m->pdb;
pdb[PDX(0)] = pdb[PDX(KZERO)];
mmuflushtlb(PADDR(pdb));
f = (void*)REBOOTADDR;
memmove(f, rebootcode, sizeof(rebootcode));
print("rebooting...\n");
coherence();
(*f)(PADDR(entry), PADDR(code), size);
}
void
exit(int ispanic)
{
shutdown(ispanic);
arch->reset();
}
int
isaconfig(char *class, int ctlrno, ISAConf *isa)
{
char cc[32], *p;
int i;
snprint(cc, sizeof cc, "%s%d", class, ctlrno);
p = getconf(cc);
if(p == nil)
return 0;
isa->type = "";
isa->nopt = tokenize(p, isa->opt, NISAOPT);
for(i = 0; i < isa->nopt; i++){
p = isa->opt[i];
if(cistrncmp(p, "type=", 5) == 0)
isa->type = p + 5;
else if(cistrncmp(p, "port=", 5) == 0)
isa->port = strtoul(p+5, &p, 0);
else if(cistrncmp(p, "irq=", 4) == 0)
isa->irq = strtoul(p+4, &p, 0);
else if(cistrncmp(p, "dma=", 4) == 0)
isa->dma = strtoul(p+4, &p, 0);
else if(cistrncmp(p, "mem=", 4) == 0)
isa->mem = strtoul(p+4, &p, 0);
else if(cistrncmp(p, "size=", 5) == 0)
isa->size = strtoul(p+5, &p, 0);
else if(cistrncmp(p, "freq=", 5) == 0)
isa->freq = strtoul(p+5, &p, 0);
}
return 1;
}
int
cistrcmp(char *a, char *b)
{
int ac, bc;
for(;;){
ac = *a++;
bc = *b++;
if(ac >= 'A' && ac <= 'Z')
ac = 'a' + (ac - 'A');
if(bc >= 'A' && bc <= 'Z')
bc = 'a' + (bc - 'A');
ac -= bc;
if(ac)
return ac;
if(bc == 0)
break;
}
return 0;
}
int
cistrncmp(char *a, char *b, int n)
{
unsigned ac, bc;
while(n > 0){
ac = *a++;
bc = *b++;
n--;
if(ac >= 'A' && ac <= 'Z')
ac = 'a' + (ac - 'A');
if(bc >= 'A' && bc <= 'Z')
bc = 'a' + (bc - 'A');
ac -= bc;
if(ac)
return ac;
if(bc == 0)
break;
}
return 0;
}
void
idlehands(void)
{
if(conf.nmach == 1 || idle_spin == 0 ||
idle_if_nproc && conf.nmach >= idle_if_nproc)
halt();
}