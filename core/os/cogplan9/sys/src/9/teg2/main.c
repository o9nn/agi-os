#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "init.h"
#include <pool.h>
#include <tos.h>
#include "arm.h"
#include "reboot.h"
#define BOOTARGS ((char*)CONFADDR)
#define BOOTARGSLEN (16*KiB)
#define MAXCONF 64
#define MAXCONFLINE 160
enum {
Minmem = 256*MB,
Ustkheadroom = sizeof(Sargs) + sizeof(uintptr) + sizeof(Tos),
};
#define isascii(c) ((uchar)(c) > 0 && (uchar)(c) < 0177)
extern char bdata[], edata[], end[], etext[];
uintptr kseg0 = KZERO;
Mach* machaddr[MAXMACH];
uchar *l2pages;
Memcache cachel[8];
Lowmemcache *cacheconf;
static int oargc;
static char* oargv[20];
static char oargb[128];
static int oargblen;
static char oenv[4096];
static uintptr sp;
int vflag;
int normalprint;
char debug[256];
static Lock testlock;
static char confname[MAXCONF][KNAMELEN];
static char confval[MAXCONF][MAXCONFLINE];
static int nconf;
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
plan9iniinit(void)
{
char *k, *v, *next;
k = (char *)CONFADDR;
if(!isascii(*k))
return;
for(; k && *k != '\0'; k = next) {
if (!isascii(*k))
break;
next = strchr(k, '\n');
if (next)
*next++ = '\0';
if (*k == '\0' || *k == '\n' || *k == '#')
continue;
v = strchr(k, '=');
if(v == nil)
continue;
*v++ = '\0';
addconf(k, v);
}
}
static void
optionsinit(char* s)
{
char *o;
strcpy(oenv, "");
o = strecpy(oargb, oargb+sizeof(oargb), s)+1;
if(getenv("bootargs", o, o - oargb) != nil)
*(o-1) = ' ';
oargblen = strlen(oargb);
oargc = tokenize(oargb, oargv, nelem(oargv)-1);
oargv[oargc] = nil;
}
char*
getenv(char* name, char* buf, int n)
{
char *e, *p, *q;
p = oenv;
while(*p != 0){
if((e = strchr(p, '=')) == nil)
break;
for(q = name; p < e; p++){
if(*p != *q)
break;
q++;
}
if(p == e && *q == 0){
strecpy(buf, buf+n, e+1);
return buf;
}
p += strlen(p)+1;
}
return nil;
}
void
machon(uint cpu)
{
ulong cpubit;
cpubit = 1 << cpu;
lock(&active);
if ((active.machs & cpubit) == 0) {
conf.nmach++;
active.machs |= cpubit;
}
unlock(&active);
}
void
machoff(uint cpu)
{
ulong cpubit;
cpubit = 1 << cpu;
lock(&active);
if (active.machs & cpubit) {
conf.nmach--;
active.machs &= ~cpubit;
}
unlock(&active);
}
void
machinit(void)
{
Mach *m0;
if (m == 0) {
serialputc('?');
serialputc('m');
serialputc('0');
}
if(machaddr[m->machno] != m) {
serialputc('?');
serialputc('m');
serialputc('m');
}
if (canlock(&testlock)) {
serialputc('?');
serialputc('l');
panic("cpu%d: locks don't work", m->machno);
}
m->ticks = 1;
m->perf.period = 1;
m0 = MACHP(0);
if (m->machno != 0) {
m->ticks = m0->ticks;
m->fastclock = m0->fastclock;
m->cpuhz = m0->cpuhz;
m->delayloop = m0->delayloop;
}
if (m->machno != 0 &&
(m->fastclock == 0 || m->cpuhz == 0 || m->delayloop == 0))
panic("buggered cpu 0 Mach");
machon(m->machno);
fpoff();
}
void
mach0init(void)
{
if (m == 0) {
serialputc('?');
serialputc('m');
}
conf.nmach = 0;
m->machno = 0;
machaddr[0] = m;
lock(&testlock);
machinit();
active.exiting = 0;
l1cache->wbse(&active, sizeof active);
up = nil;
}
void
launchinit(void)
{
int mach;
Mach *mm;
PTE *l1;
for(mach = 1; mach < MAXMACH; mach++){
machaddr[mach] = mm = mallocalign(MACHSIZE, MACHSIZE, 0, 0);
l1 = mallocalign(L1SIZE, L1SIZE, 0, 0);
if(mm == nil || l1 == nil)
panic("launchinit");
memset(mm, 0, MACHSIZE);
mm->machno = mach;
memmove(l1, (void *)L1, L1SIZE);
l1cache->wbse(l1, L1SIZE);
mm->mmul1 = l1;
l1cache->wbse(mm, MACHSIZE);
}
l1cache->wbse(machaddr, sizeof machaddr);
conf.nmach = 1;
}
void
dump(void *vaddr, int words)
{
ulong *addr;
addr = vaddr;
while (words-- > 0)
iprint("%.8lux%c", *addr++, words % 8 == 0? '\n': ' ');
}
static void
cacheinit(void)
{
allcacheinfo(cachel);
cacheconf = (Lowmemcache *)CACHECONF;
cacheconf->l1waysh = cachel[1].waysh;
cacheconf->l1setsh = cachel[1].setsh;
cacheconf->l2waysh = cachel[2].waysh;
cacheconf->l2setsh = cachel[2].setsh;
l2pl310init();
allcacheson();
allcache->wb();
}
void
l2pageinit(void)
{
l2pages = KADDR(PHYSDRAM + DRAMSIZE - RESRVDHIMEM);
}
void
main(void)
{
int cpu;
static ulong vfy = 0xcafebabe;
up = nil;
if (vfy != 0xcafebabe) {
serialputc('?');
serialputc('d');
panic("data segment misaligned");
}
memset(edata, 0, end - edata);
smpon();
iprint("ll Labs ");
cacheinit();
mach0init();
l2pageinit();
mmuinit();
optionsinit("/boot/boot boot");
quotefmtinstall();
plan9iniinit();
l2cache->on();
l2cache->info(&cachel[2]);
allcache->on();
cortexa9cachecfg();
trapinit();
confinit();
delay(100);
navailcpus = getncpus();
iprint("(mp arm; %d cpus)\n\n", navailcpus);
delay(100);
for (cpu = 1; cpu < navailcpus; cpu++)
stopcpu(cpu);
xinit();
irqtooearly = 0;
mainmem->flags |= POOL_ANTAGONISM ;
archreset();
clockinit();
timersinit();
delay(50);
printinit();
kbdenable();
cpuidprint();
chkmissing();
procinit0();
initseg();
links();
conf.monitor = 1;
iprint("pcireset...");
pcireset();
iprint("ok\n");
chandevreset();
pageinit();
swapinit();
userinit();
launchinit();
for (cpu = 1; cpu < navailcpus; cpu++)
if (startcpu(cpu) < 0)
panic("cpu%d didn't start", cpu);
l1diag();
schedinit();
panic("cpu%d: schedinit returned", m->machno);
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
if(once) {
delay(m->machno*1000);
iprint("cpu%d: exiting\n", m->machno);
}
spllo();
if (m->machno == 0)
ms = 5*1000;
else
ms = 2*1000;
for(; ms > 0; ms -= TK2MS(2)){
delay(TK2MS(2));
if(active.machs == 0 && consactive() == 0)
break;
}
delay(500);
}
void
exit(int code)
{
shutdown(code);
splhi();
if (m->machno == 0)
archreboot();
else {
intrcpushutdown();
stopcpu(m->machno);
for (;;)
idlehands();
}
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
void
reboot(void *entry, void *code, ulong size)
{
int cpu, nmach, want, ms;
void (*f)(ulong, ulong, ulong);
nmach = conf.nmach;
writeconf();
if (m->machno != 0) {
procwired(up, 0);
sched();
}
if (m->machno != 0)
print("on cpu%d (not 0)!\n", m->machno);
for (want = 0, cpu = 1; cpu < navailcpus; cpu++)
want |= 1 << cpu;
active.stopped = 0;
shutdown(0);
for (ms = 15*1000; ms > 0 && active.stopped != want; ms -= 10)
delay(10);
delay(20);
if (active.stopped != want) {
for (cpu = 1; cpu < nmach; cpu++)
stopcpu(cpu);
delay(20);
}
pcireset();
serialoq = nil;
kprintoq = nil;
screenputs = nil;
chandevshutdown();
clockshutdown();
splhi();
intrshutdown();
f = (void*)REBOOTADDR;
memmove(f, rebootcode, sizeof(rebootcode));
cachedwb();
l2cache->wbinv();
l2cache->off();
cacheuwbinv();
(*f)(PADDR(entry), PADDR(code), size);
iprint("loaded kernel returned!\n");
archreboot();
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
i8250console();
if(kbdq == nil)
panic("init0: nil kbdq");
if(serialoq == nil)
panic("init0: nil serialoq");
normalprint = 1;
if(!waserror()){
snprint(buf, sizeof(buf), "%s %s", "ARM", conffile);
ksetenv("terminal", buf, 0);
ksetenv("cputype", "arm", 0);
if(cpuserver)
ksetenv("service", "cpu", 0);
else
ksetenv("service", "terminal", 0);
for(i = 0; i < nconf; i++) {
ksetenv(confname[i], confval[i], 0);
ksetenv(confname[i], confval[i], 1);
}
poperror();
}
kproc("alarm", alarmkproc, 0);
touser(sp);
}
static void
bootargs(uintptr base)
{
int i;
ulong ssize;
char **av, *p;
i = oargblen+1;
p = UINT2PTR(STACKALIGN(base + BY2PG - Ustkheadroom - i));
memmove(p, oargb, i);
av = (char**)(p - (oargc+2)*sizeof(char*));
ssize = base + BY2PG - PTR2UINT(av);
*av++ = (char*)oargc;
for(i = 0; i < oargc; i++)
*av++ = (oargv[i] - oargb) + (p - base) + (USTKTOP - BY2PG);
*av = nil;
sp = USTKTOP - ssize - sizeof(void*);
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
Conf conf;
Confmem tsmem[nelem(conf.mem)] = {
{ .base = PHYSDRAM, .limit = PHYSDRAM + Minmem, },
};
ulong memsize = DRAMSIZE;
static int
gotmem(uintptr sz)
{
uintptr addr;
addr = (uintptr)KADDR(PHYSDRAM + sz - BY2WD);
if (probeaddr(addr) >= 0) {
memsize = sz;
return 0;
}
return -1;
}
void
confinit(void)
{
int i;
ulong kpages;
uintptr pa;
char *p;
if(nelem(tsmem) > nelem(conf.mem)){
iprint("memory configuration botch\n");
exit(1);
}
if(0 && (p = getconf("*maxmem")) != nil) {
memsize = strtoul(p, 0, 0) - PHYSDRAM;
if (memsize < 16*MB)
memsize = 16*MB;
}
if (gotmem(memsize - RESRVDHIMEM) < 0)
panic("can't find 1GB of memory");
tsmem[0].limit = PHYSDRAM + memsize;
memmove(conf.mem, tsmem, sizeof(tsmem));
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
conf.nproc = 100 + ((conf.npage*BY2PG)/MB)*5;
if(cpuserver)
conf.nproc *= 3;
if(conf.nproc > 2000)
conf.nproc = 2000;
conf.nswap = conf.npage*3;
conf.nswppo = 4096;
conf.nimage = 200;
conf.copymode = 1;
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
int
cmpswap(long *addr, long old, long new)
{
return cas((int *)addr, old, new);
}
void
advertwfi(void)
{
ilock(&active);
active.wfi |= 1 << m->machno;
iunlock(&active);
}
void
unadvertwfi(void)
{
ilock(&active);
active.wfi &= ~(1 << m->machno);
iunlock(&active);
}
void
idlehands(void)
{
#ifdef use_ipi
int advertised;
if (m->ticks <= 1)
return;
advertised = 0;
m->inidlehands++;
if (m->inidlehands == 1 && m->syscall > 0) {
advertwfi();
advertised = 1;
}
wfi();
if (advertised)
unadvertwfi();
m->inidlehands--;
#endif
}
void
wakewfi(void)
{
#ifdef use_ipi
uint cpu;
cpu = BI2BY*BY2WD - 1 - clz(active.wfi & ~(1 << m->machno));
if (cpu < MAXMACH)
intrcpu(cpu);
#endif
}