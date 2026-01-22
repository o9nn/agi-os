#include "u.h"
#include "tos.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "init.h"
#include <pool.h>
#include "reboot.h"
#define	Minfirmrev	326770
#define	Minfirmdate	"22 Jul 2012"
#define BOOTARGS	((char*)CONFADDR)
#define	BOOTARGSLEN	(MACHADDR-CONFADDR)
#define	MAXCONF		64
#define MAXCONFLINE	160
uintptr kseg0 = KZERO;
Mach*	machaddr[MAXMACH];
Conf	conf;
ulong	memsize = 128*1024*1024;
static int oargc;
static char* oargv[20];
static char oargb[128];
static int oargblen;
static uintptr sp;
static char confname[MAXCONF][KNAMELEN];
static char confval[MAXCONF][MAXCONFLINE];
static int nconf;
typedef struct Atag Atag;
struct Atag {
u32int	size;
u32int	tag;
union {
u32int	data[1];
struct {
u32int	size;
u32int	base;
} mem;
char	cmdline[1];
};
};
enum {
AtagNone	= 0x00000000,
AtagCore	= 0x54410001,
AtagMem		= 0x54410002,
AtagCmdline	= 0x54410009,
};
static int
findconf(char *name)
{
int i;
for(i = 0; i < nconf; i++)
if(cistrcmp(confname[i], name) == 0)
return i;
return -1;
}
char*
getconf(char *name)
{
int i;
i = findconf(name);
if(i >= 0)
return confval[i];
return nil;
}
void
addconf(char *name, char *val)
{
int i;
i = findconf(name);
if(i < 0){
if(val == nil || nconf >= MAXCONF)
return;
i = nconf++;
strecpy(confname[i], confname[i]+sizeof(confname[i]), name);
}
strecpy(confval[i], confval[i]+sizeof(confval[i]), val);
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
memmove(BOOTARGS, p, n);
memset(BOOTARGS + n, '\n', BOOTARGSLEN - n);
poperror();
free(p);
}
static void
plan9iniinit(char *s, int cmdline)
{
char *toks[MAXCONF];
int i, c, n;
char *v;
if((c = *s) < ' ' || c >= 0x80)
return;
if(cmdline)
n = tokenize(s, toks, MAXCONF);
else
n = getfields(s, toks, MAXCONF, 1, "\n");
for(i = 0; i < n; i++){
if(toks[i][0] == '#')
continue;
v = strchr(toks[i], '=');
if(v == nil)
continue;
*v++ = '\0';
addconf(toks[i], v);
}
}
static void
ataginit(Atag *a)
{
int n;
if(a->tag != AtagCore){
plan9iniinit((char*)a, 0);
return;
}
while(a->tag != AtagNone){
switch(a->tag){
case AtagMem:
if(conf.mem[0].limit == 0 && a->mem.size != 0){
memsize = a->mem.size;
conf.mem[0].base = a->mem.base;
conf.mem[0].limit = a->mem.base + memsize;
}
break;
case AtagCmdline:
n = (a->size * sizeof(u32int)) - offsetof(Atag, cmdline[0]);
if(a->cmdline + n < BOOTARGS + BOOTARGSLEN)
a->cmdline[n] = 0;
else
BOOTARGS[BOOTARGSLEN-1] = 0;
plan9iniinit(a->cmdline, 1);
break;
}
a = (Atag*)((u32int*)a + a->size);
}
}
void
machinit(void)
{
m->machno = 0;
machaddr[m->machno] = m;
m->ticks = 1;
m->perf.period = 1;
conf.nmach = 1;
active.machs = 1;
active.exiting = 0;
up = nil;
}
static void
optionsinit(char* s)
{
strecpy(oargb, oargb+sizeof(oargb), s);
oargblen = strlen(oargb);
oargc = tokenize(oargb, oargv, nelem(oargv)-1);
oargv[oargc] = nil;
}
void
main(void)
{
extern char edata[], end[];
uint rev;
okay(1);
m = (Mach*)MACHADDR;
memset(edata, 0, end - edata);
machinit();
mmuinit1();
optionsinit("/boot/boot boot");
quotefmtinstall();
ataginit((Atag*)BOOTARGS);
confinit();
xinit();
uartconsinit();
screeninit();
print("\nPlan 9 from Bell Labs\n");
rev = getfirmware();
print("firmware: rev %d\n", rev);
if(rev < Minfirmrev){
print("Sorry, firmware (start.elf) must be at least rev %d (%s)\n",
Minfirmrev, Minfirmdate);
for(;;)
;
}
trapinit();
clockinit();
printinit();
timersinit();
if(conf.monitor)
swcursorinit();
cpuidprint();
archreset();
procinit0();
initseg();
links();
chandevreset();
pageinit();
swapinit();
userinit();
schedinit();
assert(0);
}
void
init0(void)
{
int i;
char buf[2*KNAMELEN];
up->nerrlab = 0;
coherence();
spllo();
up->slash = namec("#/", Atodir, 0, 0);
pathclose(up->slash->path);
up->slash->path = newpath("/");
up->dot = cclone(up->slash);
chandevinit();
if(!waserror()){
snprint(buf, sizeof(buf), "%s %s", "ARM", conffile);
ksetenv("terminal", buf, 0);
ksetenv("cputype", "arm", 0);
if(cpuserver)
ksetenv("service", "cpu", 0);
else
ksetenv("service", "terminal", 0);
snprint(buf, sizeof(buf), "-a %s", getethermac());
ksetenv("etherargs", buf, 0);
for(i = 0; i < nconf; i++) {
ksetenv(confname[i], confval[i], 0);
ksetenv(confname[i], confval[i], 1);
}
poperror();
}
kproc("alarm", alarmkproc, 0);
touser(sp);
assert(0);
}
static void
bootargs(uintptr base)
{
int i;
ulong ssize;
char **av, *p;
i = oargblen+1;
p = UINT2PTR(STACKALIGN(base + BY2PG - sizeof(Tos) - i));
memmove(p, oargb, i);
av = (char**)(p - (oargc+1)*sizeof(char*));
ssize = base + BY2PG - PTR2UINT(av);
for(i = 0; i < oargc; i++)
*av++ = (oargv[i] - oargb) + (p - base) + (USTKTOP - BY2PG);
*av = nil;
sp = USTKTOP - ssize;
}
void
userinit(void)
{
Proc *p;
Segment *s;
KMap *k;
Page *pg;
up = nil;
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
p->sched.pc = PTR2UINT(init0);
p->sched.sp = PTR2UINT(p->kstack+KSTACK-sizeof(up->s.args)-sizeof(uintptr));
p->sched.sp = STACKALIGN(p->sched.sp);
s = newseg(SG_STACK, USTKTOP-USTKSIZE, USTKSIZE/BY2PG);
s->flushme++;
p->seg[SSEG] = s;
pg = newpage(1, 0, USTKTOP-BY2PG);
segpage(s, pg);
k = kmap(pg);
bootargs(VA(k));
kunmap(k);
s = newseg(SG_TEXT, UTZERO, 1);
p->seg[TSEG] = s;
pg = newpage(1, 0, UTZERO);
memset(pg->cachectl, PG_TXTFLUSH, sizeof(pg->cachectl));
segpage(s, pg);
k = kmap(s->map[0]->pages[0]);
memmove(UINT2PTR(VA(k)), initcode, sizeof initcode);
kunmap(k);
ready(p);
}
void
confinit(void)
{
int i;
ulong kpages;
uintptr pa;
char *p;
if(0 && (p = getconf("service")) != nil){
if(strcmp(p, "cpu") == 0)
cpuserver = 1;
else if(strcmp(p,"terminal") == 0)
cpuserver = 0;
}
if((p = getconf("*maxmem")) != nil){
memsize = strtoul(p, 0, 0) - PHYSDRAM;
if (memsize < 16*MB)
memsize = 16*MB;
}
getramsize(&conf.mem[0]);
if(conf.mem[0].limit == 0){
conf.mem[0].base = PHYSDRAM;
conf.mem[0].limit = PHYSDRAM + memsize;
}else if(p != nil)
conf.mem[0].limit = conf.mem[0].base + memsize;
conf.npage = 0;
pa = PADDR(PGROUND(PTR2UINT(end)));
for(i=0; i<nelem(conf.mem); i++){
if(pa > conf.mem[i].base && pa < conf.mem[i].limit)
conf.mem[i].base = pa;
conf.mem[i].npage = (conf.mem[i].limit - conf.mem[i].base)/BY2PG;
conf.npage += conf.mem[i].npage;
}
conf.upages = (conf.npage*80)/100;
conf.ialloc = ((conf.npage-conf.upages)/2)*BY2PG;
conf.nmach = 1;
conf.nproc = 100 + ((conf.npage*BY2PG)/MB)*5;
if(cpuserver)
conf.nproc *= 3;
if(conf.nproc > 2000)
conf.nproc = 2000;
conf.nswap = conf.npage*3;
conf.nswppo = 4096;
conf.nimage = 200;
conf.copymode = 0;
kpages = conf.npage - conf.upages;
kpages *= BY2PG;
kpages -= conf.upages*sizeof(Page)
+ conf.nproc*sizeof(Proc)
+ conf.nimage*sizeof(Image)
+ conf.nswap
+ conf.nswppo*sizeof(Page);
mainmem->maxsize = kpages;
if(!cpuserver)
imagmem->maxsize = kpages;
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
delay(1000);
}
void
exit(int code)
{
shutdown(code);
splfhi();
archreboot();
}
int
isaconfig(char *class, int ctlrno, ISAConf *isa)
{
USED(ctlrno);
USED(isa);
return strcmp(class, "ether") == 0;
}
void
reboot(void *entry, void *code, ulong size)
{
void (*f)(ulong, ulong, ulong);
print("starting reboot...");
writeconf();
shutdown(0);
print("reboot entry %#lux code %#lux size %ld\n",
PADDR(entry), PADDR(code), size);
delay(100);
serialoq = nil;
kprintoq = nil;
screenputs = nil;
chandevshutdown();
clockshutdown();
splfhi();
intrsoff();
f = (void*)REBOOTADDR;
memmove(f, rebootcode, sizeof(rebootcode));
cacheuwbinv();
(*f)(PADDR(entry), PADDR(code), size);
iprint("loaded kernel returned!\n");
delay(1000);
archreboot();
}
int
cmpswap(long *addr, long old, long new)
{
return cas32(addr, old, new);
}