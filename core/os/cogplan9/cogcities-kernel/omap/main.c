#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "init.h"
#include <pool.h>
#include "reboot.h"
#define BOOTARGS	((char*)CONFADDR)
#define	BOOTARGSLEN	(16*KiB)
#define	MAXCONF		64
#define MAXCONFLINE	160
enum {
Minmem	= 256*MB,
};
#define isascii(c) ((uchar)(c) > 0 && (uchar)(c) < 0177)
uintptr kseg0 = KZERO;
Mach* machaddr[MAXMACH];
static int oargc;
static char* oargv[20];
static char oargb[128];
static int oargblen;
static char oenv[4096];
static uintptr sp;
int vflag;
int normalprint;
char debug[256];
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
main(void)
{
extern char bdata[], edata[], end[], etext[];
static ulong vfy = 0xcafebabe;
if (vfy != 0xcafebabe) {
memmove(bdata, etext, edata - bdata);
}
memset(edata, 0, end - edata);
cacheuwbinv();
l2cacheuwbinv();
if (vfy != 0xcafebabe)
panic("data segment misaligned");
vfy = 0;
wave('l');
machinit();
mmuinit();
optionsinit("/boot/boot boot");
quotefmtinstall();
plan9iniinit();
trapinit();
confinit();
delay(500);
iprint("l Labs\n\n");
delay(500);
xinit();
mainmem->flags |= POOL_ANTAGONISM  ;
archreset();
clockinit();
timersinit();
watchdoginit();
delay(250);
printinit();
kbdenable();
cpuidprint();
procinit0();
initseg();
dmainit();
links();
conf.monitor = 1;
screeninit();
chandevreset();
pageinit();
swapinit();
userinit();
schedinit();
}
void
machinit(void)
{
if (m == 0)
wave('?');
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
splhi();
archreboot();
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
splhi();
intrsoff();
f = (void*)REBOOTADDR;
memmove(f, rebootcode, sizeof(rebootcode));
cacheuwbinv();
l2cacheuwbinv();
(*f)(PADDR(entry), PADDR(code), size);
iprint("loaded kernel returned!\n");
delay(1000);
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
dmatest();
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
p = UINT2PTR(STACKALIGN(base + BY2PG - sizeof(up->s.args) - i));
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
Confmem omapmem[nelem(conf.mem)] = {
{ .base = PHYSDRAM, .limit = PHYSDRAM + Minmem, },
};
ulong memsize = Minmem;
static int
gotmem(uintptr sz)
{
uintptr addr;
addr = PHYSDRAM + sz - BY2WD;
mmuidmap(addr, 1);
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
if(nelem(omapmem) > nelem(conf.mem)){
iprint("memory configuration botch\n");
exit(1);
}
if((p = getconf("*maxmem")) != nil) {
memsize = strtoul(p, 0, 0) - PHYSDRAM;
if (memsize < 16*MB)
memsize = 16*MB;
}
if (gotmem(memsize) < 0 && gotmem(256*MB) < 0 && gotmem(128*MB) < 0) {
iprint("can't find any memory, assuming %dMB\n", Minmem / MB);
memsize = Minmem;
}
omapmem[0].limit = PHYSDRAM + memsize;
memmove(conf.mem, omapmem, sizeof(omapmem));
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
int
cmpswap(long *addr, long old, long new)
{
return cas32(addr, old, new);
}