#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"pool.h"
#include	"reboot.h"
#include	"ip.h"
#include	<tos.h>
enum {
Datamagic = 0xbabeabed,
};
Mach *m;
ulong* mach0pdb;
Mach* mach0m;
Segdesc* mach0gdt;
u32int memstart;
u32int memend;
int noclock;
extern int pcivga;
extern char hellomsg[];
char bootdisk[KNAMELEN];
Conf conf;
uchar *sp;
int delaylink;
int debug;
int v_flag;
static void
sanity(void)
{
uintptr cr3;
cr3 = (uintptr)KADDR(getcr3());
if (cr3 == 0)
panic("zero cr3");
if ((uintptr)m->pdb != cr3 || (uintptr)mach0pdb != cr3)
panic("not all same: cr3 %#p m->pdb %#p mach0pdb %#p",
cr3, m->pdb, mach0pdb);
if (m != mach0m)
panic("m %#p != mach0m %#p", m, mach0m);
if (m->gdt != mach0gdt)
panic("m->gdt %#p != mach0gdt %#p", m->gdt, mach0gdt);
if (0)
iprint("m->pdb %#p m %#p sp %#p m->gdt %#p\n",
m->pdb, m, &cr3, m->gdt);
}
enum {
Sysctla=	0x92,
Sysctlreset=	1<<0,
Sysctla20ena=	1<<1,
};
static int
isa20on(void)
{
int r;
ulong o;
ulong *zp, *mb1p;
zp = 0;
mb1p = (ulong *)MB;
o = *zp;
*zp = 0x1234;
*mb1p = 0x8765;
mb586();
wbinvd();
r = *zp != *mb1p;
*zp = o;
return r;
}
void
a20init(void)
{
int b;
if (isa20on())
return;
i8042a20();
if (isa20on())
return;
b = inb(Sysctla);
if (!(b & Sysctla20ena))
outb(Sysctla, (b & ~Sysctlreset) | Sysctla20ena);
if (!isa20on()){
iprint("a20 didn't come on!\n");
for(;;)
;
}
}
void
main(void)
{
Proc *savup;
static ulong vfy = Datamagic;
static char novga[] = "\nno vga; serial console only\n";
savup = up;
up = nil;
a20init();
mach0init();
ioinit();
i8250config("0");
quotefmtinstall();
fmtinstall('i', eipfmt);
fmtinstall('I', eipfmt);
fmtinstall('E', eipfmt);
fmtinstall('V', eipfmt);
fmtinstall('M', eipfmt);
screeninit();
cgapost(0xc);
trapinit0();
mmuinit0();
kbdinit();
i8253init();
cpuidentify();
readlsconf();
meminit();
confinit();
archinit();
xinit();
if(i8237alloc != nil)
i8237alloc();
trapinit();
printinit();
sanity();
cgapost(1);
pcimatch(nil, 0, 0);
if (!pcivga) {
screenputs = nil;
uartputs(novga, sizeof novga - 1);
}
print(" %s\n\n", hellomsg);
if (vfy != Datamagic)
panic("data segment incorrectly aligned or loaded");
if (savup)
print("up was non-nil (%#p) upon entry to main; bss wasn't zeroed!\n",
savup);
cpuidprint();
mmuinit();
if(arch->intrinit)
arch->intrinit();
timersinit();
mathinit();
kbdenable();
if(!noclock && arch->clockenable)
arch->clockenable();
procinit0();
initseg();
if(delaylink){
bootlinks();
pcimatch(0, 0, 0);
}else
links();
conf.monitor = 1;
cgapost(0xcd);
chandevreset();
cgapost(2);
pageinit();
i8253link();
userinit();
active.thunderbirdsarego = 1;
cgapost(0xb0);
schedinit();
}
void
mach0init(void)
{
conf.nmach = 1;
MACHP(0) = mach0m;
m->machno = 0;
m->pdb = mach0pdb;
m->gdt = mach0gdt;
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
if(0 && !waserror()){
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
conschan = enamecopen("#c/cons", ORDWR);
bootloadproc(0);
panic("bootloadproc returned");
}
void
userinit(void)
{
Proc *p;
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
ready(p);
}
void
confinit(void)
{
int i, userpcnt;
ulong kpages;
userpcnt = 0;
conf.npage = 0;
for(i=0; i<nelem(conf.mem); i++)
conf.npage += conf.mem[i].npage;
conf.npage = MemMax / BY2PG;
conf.nproc = 20;
if(cpuserver)
conf.nproc *= 3;
if(conf.nproc > 2000)
conf.nproc = 2000;
conf.nimage = 40;
conf.nswap = conf.nproc*80;
conf.nswppo = 4096;
kpages = conf.npage - (conf.npage*userpcnt)/100;
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
static void
mathover(Ureg*, void*)
{
pexit("math overrun", 0);
}
void
mathinit(void)
{
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
int i;
void (*f)(ulong, ulong, ulong);
ulong *pdb;
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
for (i = 0; i < LOWPTEPAGES; i++)
pdb[PDX(i*4*MB)] = pdb[PDX(KZERO + i*4*MB)];
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
spllo();
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
int less_power_slower;
void
idlehands(void)
{
if(conf.nmach == 1 || less_power_slower)
halt();
}
void
trimnl(char *s)
{
char *nl;
nl = strchr(s, '\n');
if (nl != nil)
*nl = '\0';
}