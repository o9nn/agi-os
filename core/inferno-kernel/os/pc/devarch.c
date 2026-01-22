#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "../port/error.h"
typedef struct IOMap IOMap;
struct IOMap
{
IOMap *next;
int reserved;
char tag[13];
ulong start;
ulong end;
};
static struct
{
Lock;
IOMap *m;
IOMap *free;
IOMap maps[32];
QLock ql;
} iomap;
enum {
Qdir = 0,
Qioalloc = 1,
Qiob,
Qiow,
Qiol,
Qbase,
Qmax = 16,
};
typedef long Rdwrfn(Chan*, void*, long, vlong);
static Rdwrfn *readfn[Qmax];
static Rdwrfn *writefn[Qmax];
static Dirtab archdir[Qmax] = {
".", { Qdir, 0, QTDIR }, 0, 0555,
"ioalloc", { Qioalloc, 0 }, 0, 0444,
"iob", { Qiob, 0 }, 0, 0660,
"iow", { Qiow, 0 }, 0, 0660,
"iol", { Qiol, 0 }, 0, 0660,
};
Lock archwlock;
int narchdir = Qbase;
int (*_pcmspecial)(char*, ISAConf*);
void (*_pcmspecialclose)(int);
static int doi8253set = 1;
Dirtab*
addarchfile(char *name, int perm, Rdwrfn *rdfn, Rdwrfn *wrfn)
{
int i;
Dirtab d;
Dirtab *dp;
memset(&d, 0, sizeof d);
strcpy(d.name, name);
d.perm = perm;
lock(&archwlock);
if(narchdir >= Qmax){
unlock(&archwlock);
return nil;
}
for(i=0; i<narchdir; i++)
if(strcmp(archdir[i].name, name) == 0){
unlock(&archwlock);
return nil;
}
d.qid.path = narchdir;
archdir[narchdir] = d;
readfn[narchdir] = rdfn;
writefn[narchdir] = wrfn;
dp = &archdir[narchdir++];
unlock(&archwlock);
return dp;
}
void
ioinit(void)
{
char *excluded;
int i;
for(i = 0; i < nelem(iomap.maps)-1; i++)
iomap.maps[i].next = &iomap.maps[i+1];
iomap.maps[i].next = nil;
iomap.free = iomap.maps;
ioalloc(0x0fff, 1, 0, "dummy");
if ((excluded = getconf("ioexclude")) != nil) {
char *s;
s = excluded;
while (s && *s != '\0' && *s != '\n') {
char *ends;
int io_s, io_e;
io_s = (int)strtol(s, &ends, 0);
if (ends == nil || ends == s || *ends != '-') {
print("ioinit: cannot parse option string\n");
break;
}
s = ++ends;
io_e = (int)strtol(s, &ends, 0);
if (ends && *ends == ',')
*ends++ = '\0';
s = ends;
ioalloc(io_s, io_e - io_s + 1, 0, "pre-allocated");
}
}
}
int
ioreserve(int, int size, int align, char *tag)
{
IOMap *m, **l;
int i, port;
lock(&iomap);
port = 0x400;
for(l = &iomap.m; *l; l = &(*l)->next){
m = *l;
if (m->start < 0x400) continue;
i = m->start - port;
if(i > size)
break;
if(align > 0)
port = ((port+align-1)/align)*align;
else
port = m->end;
}
if(*l == nil){
unlock(&iomap);
return -1;
}
m = iomap.free;
if(m == nil){
print("ioalloc: out of maps");
unlock(&iomap);
return port;
}
iomap.free = m->next;
m->next = *l;
m->start = port;
m->end = port + size;
m->reserved = 1;
strncpy(m->tag, tag, sizeof(m->tag));
m->tag[sizeof(m->tag)-1] = 0;
*l = m;
archdir[0].qid.vers++;
unlock(&iomap);
return m->start;
}
int
ioalloc(int port, int size, int align, char *tag)
{
IOMap *m, **l;
int i;
lock(&iomap);
if(port < 0){
port = 0x400;
for(l = &iomap.m; *l; l = &(*l)->next){
m = *l;
if (m->start < 0x400) continue;
i = m->start - port;
if(i > size)
break;
if(align > 0)
port = ((port+align-1)/align)*align;
else
port = m->end;
}
if(*l == nil){
unlock(&iomap);
return -1;
}
} else {
if((port+size) > 0x10000){
unlock(&iomap);
return -1;
}
for(l = &iomap.m; *l; l = &(*l)->next){
m = *l;
if(m->end <= port)
continue;
if(m->reserved && m->start == port && m->end == port + size) {
m->reserved = 0;
unlock(&iomap);
return m->start;
}
if(m->start >= port+size)
break;
unlock(&iomap);
return -1;
}
}
m = iomap.free;
if(m == nil){
print("ioalloc: out of maps");
unlock(&iomap);
return port;
}
iomap.free = m->next;
m->next = *l;
m->start = port;
m->end = port + size;
strncpy(m->tag, tag, sizeof(m->tag));
m->tag[sizeof(m->tag)-1] = 0;
*l = m;
archdir[0].qid.vers++;
unlock(&iomap);
return m->start;
}
void
iofree(int port)
{
IOMap *m, **l;
lock(&iomap);
for(l = &iomap.m; *l; l = &(*l)->next){
if((*l)->start == port){
m = *l;
*l = m->next;
m->next = iomap.free;
iomap.free = m;
break;
}
if((*l)->start > port)
break;
}
archdir[0].qid.vers++;
unlock(&iomap);
}
int
iounused(int start, int end)
{
IOMap *m;
for(m = iomap.m; m; m = m->next){
if(start >= m->start && start < m->end
|| start <= m->start && end > m->start)
return 0;
}
return 1;
}
static void
checkport(int start, int end)
{
if(start >= 0x2b0 && end <= 0x2df+1)
return;
if(start >= 0x3c0 && end <= 0x3da+1)
return;
if(iounused(start, end))
return;
error(Eperm);
}
static Chan*
archattach(char* spec)
{
return devattach('P', spec);
}
Walkqid*
archwalk(Chan* c, Chan *nc, char** name, int nname)
{
return devwalk(c, nc, name, nname, archdir, narchdir, devgen);
}
static int
archstat(Chan* c, uchar* dp, int n)
{
return devstat(c, dp, n, archdir, narchdir, devgen);
}
static Chan*
archopen(Chan* c, int omode)
{
return devopen(c, omode, archdir, narchdir, devgen);
}
static void
archclose(Chan*)
{
}
enum
{
Linelen= 31,
};
static long
archread(Chan *c, void *a, long n, vlong offset)
{
char *buf, *p;
int port;
ushort *sp;
ulong *lp;
IOMap *m;
Rdwrfn *fn;
switch((ulong)c->qid.path){
case Qdir:
return devdirread(c, a, n, archdir, narchdir, devgen);
case Qiob:
port = offset;
checkport(offset, offset+n);
for(p = a; port < offset+n; port++)
*p++ = inb(port);
return n;
case Qiow:
if(n & 1)
error(Ebadarg);
checkport(offset, offset+n);
sp = a;
for(port = offset; port < offset+n; port += 2)
*sp++ = ins(port);
return n;
case Qiol:
if(n & 3)
error(Ebadarg);
checkport(offset, offset+n);
lp = a;
for(port = offset; port < offset+n; port += 4)
*lp++ = inl(port);
return n;
case Qioalloc:
break;
default:
if(c->qid.path < narchdir && (fn = readfn[c->qid.path]))
return fn(c, a, n, offset);
error(Eperm);
break;
}
if((buf = malloc(n)) == nil)
error(Enomem);
p = buf;
n = n/Linelen;
offset = offset/Linelen;
lock(&iomap);
for(m = iomap.m; n > 0 && m != nil; m = m->next){
if(offset-- > 0)
continue;
sprint(p, "%8lux %8lux %-12.12s\n", m->start, m->end-1, m->tag);
p += Linelen;
n--;
}
unlock(&iomap);
n = p - buf;
memmove(a, buf, n);
free(buf);
return n;
}
static long
archwrite(Chan *c, void *a, long n, vlong offset)
{
char *p;
int port;
ushort *sp;
ulong *lp;
Rdwrfn *fn;
switch((ulong)c->qid.path){
case Qiob:
p = a;
checkport(offset, offset+n);
for(port = offset; port < offset+n; port++)
outb(port, *p++);
return n;
case Qiow:
if(n & 1)
error(Ebadarg);
checkport(offset, offset+n);
sp = a;
for(port = offset; port < offset+n; port += 2)
outs(port, *sp++);
return n;
case Qiol:
if(n & 3)
error(Ebadarg);
checkport(offset, offset+n);
lp = a;
for(port = offset; port < offset+n; port += 4)
outl(port, *lp++);
return n;
default:
if(c->qid.path < narchdir && (fn = writefn[c->qid.path]))
return fn(c, a, n, offset);
error(Eperm);
break;
}
return 0;
}
Dev archdevtab = {
'P',
"arch",
devreset,
devinit,
devshutdown,
archattach,
archwalk,
archstat,
archopen,
devcreate,
archclose,
archread,
devbread,
archwrite,
devbwrite,
devremove,
devwstat,
};
static int
unimplemented(int)
{
return 0;
}
static void
nop(void)
{
}
void (*coherence)(void) = nop;
PCArch* arch;
extern PCArch* knownarch[];
PCArch archgeneric = {
.id= "generic",
.ident= 0,
.reset= i8042reset,
.serialpower= unimplemented,
.modempower= unimplemented,
.intrinit= i8259init,
.intrenable= i8259enable,
.intrvecno= i8259vecno,
.intrdisable= i8259disable,
.clockenable= i8253enable,
.fastclock= i8253read,
.timerset= i8253timerset,
};
typedef struct X86type X86type;
struct X86type {
int family;
int model;
int aalcycles;
char* name;
};
static X86type x86intel[] =
{
{ 4, 0, 22, "486DX", },
{ 4, 1, 22, "486DX50", },
{ 4, 2, 22, "486SX", },
{ 4, 3, 22, "486DX2", },
{ 4, 4, 22, "486SL", },
{ 4, 5, 22, "486SX2", },
{ 4, 7, 22, "DX2WB", },
{ 4, 8, 22, "DX4", },
{ 4, 9, 22, "DX4WB", },
{ 5, 0, 23, "P5", },
{ 5, 1, 23, "P5", },
{ 5, 2, 23, "P54C", },
{ 5, 3, 23, "P24T", },
{ 5, 4, 23, "P55C MMX", },
{ 5, 7, 23, "P54C VRT", },
{ 6, 1, 16, "PentiumPro", },
{ 6, 3, 16, "PentiumII", },
{ 6, 5, 16, "PentiumII/Xeon", },
{ 6, 6, 16, "Celeron", },
{ 6, 7, 16, "PentiumIII/Xeon", },
{ 6, 8, 16, "PentiumIII/Xeon", },
{ 6, 0xB, 16, "PentiumIII/Xeon", },
{ 0xF, 1, 16, "P4", },
{ 0xF, 2, 16, "PentiumIV/Xeon", },
{ 3, -1, 32, "386", },
{ 4, -1, 22, "486", },
{ 5, -1, 23, "P5", },
{ 6, -1, 16, "P6", },
{ 0xF, -1, 16, "P4", },
{ -1, -1, 16, "unknown", },
};
static X86type x86amd[] =
{
{ 5, 0, 23, "AMD-K5", },
{ 5, 1, 23, "AMD-K5", },
{ 5, 2, 23, "AMD-K5", },
{ 5, 3, 23, "AMD-K5", },
{ 5, 6, 11, "AMD-K6", },
{ 5, 7, 11, "AMD-K6", },
{ 5, 8, 11, "AMD-K6-2", },
{ 5, 9, 11, "AMD-K6-III", },
{ 6, 1, 11, "AMD-Athlon", },
{ 6, 2, 11, "AMD-Athlon", },
{ 4, -1, 22, "Am486", },
{ 5, -1, 23, "AMD-K5/K6", },
{ 6, -1, 11, "AMD-Athlon", },
{ 0xF, -1, 11, "AMD64", },
{ -1, -1, 11, "unknown", },
};
static X86type x86winchip[] =
{
{5, 4, 23, "Winchip",},
{6, 7, 23, "Via C3 Samuel 2 or Ezra",},
{6, 8, 23, "Via C3 Ezra-T",},
{ -1, -1, 23, "unknown", },
};
static X86type x86sis[] =
{
{5, 0, 23, "SiS 55x",},
{ -1, -1, 23, "unknown", },
};
static X86type *cputype;
static void simplecycles(uvlong*);
void (*cycles)(uvlong*) = simplecycles;
void _cycles(uvlong*);
static void
simplecycles(uvlong*x)
{
*x = m->ticks;
}
void
cpuidprint(void)
{
int i;
char buf[128];
i = sprint(buf, "cpu%d: %dMHz ", m->machno, m->cpumhz);
if(m->cpuidid[0])
i += sprint(buf+i, "%12.12s ", m->cpuidid);
sprint(buf+i, "%s (cpuid: AX 0x%4.4uX DX 0x%4.4uX)\n",
m->cpuidtype, m->cpuidax, m->cpuiddx);
print(buf);
}
int
cpuidentify(void)
{
char *p;
int family, model, nomce;
X86type *t, *tab;
ulong cr4;
vlong mca, mct;
cpuid(m->cpuidid, &m->cpuidax, &m->cpuiddx);
if(strncmp(m->cpuidid, "AuthenticAMD", 12) == 0)
tab = x86amd;
else if(strncmp(m->cpuidid, "CentaurHauls", 12) == 0)
tab = x86winchip;
else if(strncmp(m->cpuidid, "SiS SiS SiS ", 12) == 0)
tab = x86sis;
else
tab = x86intel;
family = X86FAMILY(m->cpuidax);
model = X86MODEL(m->cpuidax);
for(t=tab; t->name; t++)
if((t->family == family && t->model == model)
|| (t->family == family && t->model == -1)
|| (t->family == -1))
break;
m->cpuidtype = t->name;
if(m->cpuiddx & 0x10){
m->havetsc = 1;
cycles = _cycles;
if(m->cpuiddx & 0x20)
wrmsr(0x10, 0);
}
guesscpuhz(t->aalcycles);
if(m->cpuiddx & 0x2088){
cr4 = 0;
if(m->cpuiddx & 0x08)
cr4 |= 0x10;
if(p = getconf("*nomce"))
nomce = strtoul(p, 0, 0);
else
nomce = 0;
if((m->cpuiddx & 0x80) && !nomce){
cr4 |= 0x40;
if(family == 5){
rdmsr(0x00, &mca);
rdmsr(0x01, &mct);
}
}
if(m->cpuiddx & 0x2000){
cr4 |= 0x80;
m->havepge = 1;
}
putcr4(cr4);
if(m->cpuiddx & 0x80)
rdmsr(0x01, &mct);
}
cputype = t;
return t->family;
}
static long
cputyperead(Chan*, void *a, long n, vlong offset)
{
char str[32];
ulong mhz;
mhz = (m->cpuhz+999999)/1000000;
snprint(str, sizeof(str), "%s %lud\n", cputype->name, mhz);
return readstr(offset, a, n, str);
}
static long
archctlread(Chan*, void *a, long nn, vlong offset)
{
char buf[256];
int n;
n = snprint(buf, sizeof buf, "cpu %s %lud%s\n",
cputype->name, (ulong)(m->cpuhz+999999)/1000000,
m->havepge ? " pge" : "");
n += snprint(buf+n, sizeof buf-n, "pge %s\n", getcr4()&0x80 ? "on" : "off");
n += snprint(buf+n, sizeof buf-n, "coherence ");
if(coherence == mb386)
n += snprint(buf+n, sizeof buf-n, "mb386\n");
else if(coherence == mb586)
n += snprint(buf+n, sizeof buf-n, "mb586\n");
else if(coherence == nop)
n += snprint(buf+n, sizeof buf-n, "nop\n");
else
n += snprint(buf+n, sizeof buf-n, "0x%p\n", coherence);
n += snprint(buf+n, sizeof buf-n, "i8253set %s\n", doi8253set ? "on" : "off");
buf[n] = 0;
return readstr(offset, a, nn, buf);
}
enum
{
CMpge,
CMcoherence,
CMi8253set,
};
static Cmdtab archctlmsg[] =
{
CMpge, "pge", 2,
CMcoherence, "coherence", 2,
CMi8253set, "i8253set", 2,
};
static long
archctlwrite(Chan*, void *a, long n, vlong)
{
Cmdbuf *cb;
Cmdtab *ct;
cb = parsecmd(a, n);
if(waserror()){
free(cb);
nexterror();
}
ct = lookupcmd(cb, archctlmsg, nelem(archctlmsg));
switch(ct->index){
case CMpge:
if(!m->havepge)
error("processor does not support pge");
if(strcmp(cb->f[1], "on") == 0)
putcr4(getcr4() | 0x80);
else if(strcmp(cb->f[1], "off") == 0)
putcr4(getcr4() & ~0x80);
else
cmderror(cb, "invalid pge ctl");
break;
case CMcoherence:
if(strcmp(cb->f[1], "mb386") == 0)
coherence = mb386;
else if(strcmp(cb->f[1], "mb586") == 0){
if(X86FAMILY(m->cpuidax) < 5)
error("invalid coherence ctl on this cpu family");
coherence = mb586;
}
else if(strcmp(cb->f[1], "nop") == 0){
if(conf.nmach > 1)
error("cannot disable coherence on a multiprocessor");
coherence = nop;
}else
cmderror(cb, "invalid coherence ctl");
break;
case CMi8253set:
if(strcmp(cb->f[1], "on") == 0)
doi8253set = 1;
else if(strcmp(cb->f[1], "off") == 0){
doi8253set = 0;
(*arch->timerset)(0);
}else
cmderror(cb, "invalid i2853set ctl");
break;
}
free(cb);
poperror();
return n;
}
void
archinit(void)
{
PCArch **p;
arch = 0;
for(p = knownarch; *p; p++){
if((*p)->ident && (*p)->ident() == 0){
arch = *p;
break;
}
}
if(arch == 0)
arch = &archgeneric;
else{
if(arch->id == 0)
arch->id = archgeneric.id;
if(arch->reset == 0)
arch->reset = archgeneric.reset;
if(arch->serialpower == 0)
arch->serialpower = archgeneric.serialpower;
if(arch->modempower == 0)
arch->modempower = archgeneric.modempower;
if(arch->intrinit == 0)
arch->intrinit = archgeneric.intrinit;
if(arch->intrenable == 0)
arch->intrenable = archgeneric.intrenable;
}
if(X86FAMILY(m->cpuidax) == 3)
conf.copymode = 1;
if(X86FAMILY(m->cpuidax) >= 5)
coherence = mb586;
addarchfile("cputype", 0444, cputyperead, nil);
addarchfile("archctl", 0664, archctlread, archctlwrite);
}
int
pcmspecial(char *idstr, ISAConf *isa)
{
return (_pcmspecial != nil)? _pcmspecial(idstr, isa): -1;
}
void
pcmspecialclose(int a)
{
if (_pcmspecialclose != nil)
_pcmspecialclose(a);
}
uvlong
fastticks(uvlong *hz)
{
return (*arch->fastclock)(hz);
}
void
timerset(uvlong x)
{
if(doi8253set)
(*arch->timerset)(x);
}