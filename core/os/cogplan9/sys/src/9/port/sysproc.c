#include	"u.h"
#include	"tos.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"../port/edf.h"
#include	<a.out.h>
int	shargs(char*, int, char**);
extern void checkpages(void);
extern void checkpagerefs(void);
long
sysr1(ulong*)
{
checkpagerefs();
return 0;
}
long
sysrfork(ulong *arg)
{
Proc *p;
int n, i;
Fgrp *ofg;
Pgrp *opg;
Rgrp *org;
Egrp *oeg;
ulong pid, flag;
Mach *wm;
flag = arg[0];
if((flag & (RFFDG|RFCFDG)) == (RFFDG|RFCFDG))
error(Ebadarg);
if((flag & (RFNAMEG|RFCNAMEG)) == (RFNAMEG|RFCNAMEG))
error(Ebadarg);
if((flag & (RFENVG|RFCENVG)) == (RFENVG|RFCENVG))
error(Ebadarg);
if((flag&RFPROC) == 0) {
if(flag & (RFMEM|RFNOWAIT))
error(Ebadarg);
if(flag & (RFFDG|RFCFDG)) {
ofg = up->fgrp;
if(flag & RFFDG)
up->fgrp = dupfgrp(ofg);
else
up->fgrp = dupfgrp(nil);
closefgrp(ofg);
}
if(flag & (RFNAMEG|RFCNAMEG)) {
opg = up->pgrp;
up->pgrp = newpgrp();
if(flag & RFNAMEG)
pgrpcpy(up->pgrp, opg);
up->pgrp->noattach = opg->noattach;
closepgrp(opg);
}
if(flag & RFNOMNT)
up->pgrp->noattach = 1;
if(flag & RFREND) {
org = up->rgrp;
up->rgrp = newrgrp();
closergrp(org);
}
if(flag & (RFENVG|RFCENVG)) {
oeg = up->egrp;
up->egrp = smalloc(sizeof(Egrp));
up->egrp->ref = 1;
if(flag & RFENVG)
envcpy(up->egrp, oeg);
closeegrp(oeg);
}
if(flag & RFNOTEG)
up->noteid = incref(&noteidalloc);
return 0;
}
p = newproc();
p->fpsave = up->fpsave;
p->scallnr = up->scallnr;
p->s = up->s;
p->nerrlab = 0;
p->slash = up->slash;
p->dot = up->dot;
incref(p->dot);
memmove(p->note, up->note, sizeof(p->note));
p->privatemem = up->privatemem;
p->noswap = up->noswap;
p->nnote = up->nnote;
p->notified = 0;
p->lastnote = up->lastnote;
p->notify = up->notify;
p->ureg = up->ureg;
p->dbgreg = 0;
n = flag & RFMEM;
qlock(&p->seglock);
if(waserror()){
qunlock(&p->seglock);
nexterror();
}
for(i = 0; i < NSEG; i++)
if(up->seg[i])
p->seg[i] = dupseg(up->seg, i, n);
qunlock(&p->seglock);
poperror();
if(flag & (RFFDG|RFCFDG)) {
if(flag & RFFDG)
p->fgrp = dupfgrp(up->fgrp);
else
p->fgrp = dupfgrp(nil);
}
else {
p->fgrp = up->fgrp;
incref(p->fgrp);
}
if(flag & (RFNAMEG|RFCNAMEG)) {
p->pgrp = newpgrp();
if(flag & RFNAMEG)
pgrpcpy(p->pgrp, up->pgrp);
p->pgrp->noattach = up->pgrp->noattach;
}
else {
p->pgrp = up->pgrp;
incref(p->pgrp);
}
if(flag & RFNOMNT)
p->pgrp->noattach = 1;
if(flag & RFREND)
p->rgrp = newrgrp();
else {
incref(up->rgrp);
p->rgrp = up->rgrp;
}
if(flag & (RFENVG|RFCENVG)) {
p->egrp = smalloc(sizeof(Egrp));
p->egrp->ref = 1;
if(flag & RFENVG)
envcpy(p->egrp, up->egrp);
}
else {
p->egrp = up->egrp;
incref(p->egrp);
}
p->hang = up->hang;
p->procmode = up->procmode;
forkchild(p, up->dbgreg);
p->parent = up;
p->parentpid = up->pid;
if(flag&RFNOWAIT)
p->parentpid = 0;
else {
lock(&up->exl);
up->nchild++;
unlock(&up->exl);
}
if((flag&RFNOTEG) == 0)
p->noteid = up->noteid;
p->fpstate = up->fpstate & ~FPillegal;
pid = p->pid;
memset(p->time, 0, sizeof(p->time));
p->time[TReal] = MACHP(0)->ticks;
kstrdup(&p->text, up->text);
kstrdup(&p->user, up->user);
flushmmu();
p->basepri = up->basepri;
p->priority = up->basepri;
p->fixedpri = up->fixedpri;
p->mp = up->mp;
wm = up->wired;
if(wm)
procwired(p, wm->machno);
ready(p);
sched();
return pid;
}
ulong
l2be(long l)
{
uchar *cp;
cp = (uchar*)&l;
return (cp[0]<<24) | (cp[1]<<16) | (cp[2]<<8) | cp[3];
}
long
sysexec(ulong *arg)
{
Segment *s, *ts;
ulong t, d, b;
int i;
Chan *tc;
char **argv, **argp;
char *a, *charp, *args, *file, *file0;
char *progarg[sizeof(Exec)/2+1], *elem, progelem[64];
ulong ssize, spage, nargs, nbytes, n, bssend;
int indir;
Exec exec;
char line[sizeof(Exec)];
Fgrp *f;
Image *img;
ulong magic, text, entry, data, bss;
Tos *tos;
indir = 0;
elem = nil;
validaddr(arg[0], 1, 0);
file0 = validnamedup((char*)arg[0], 1);
if(waserror()){
free(file0);
free(elem);
nexterror();
}
file = file0;
for(;;){
tc = namec(file, Aopen, OEXEC, 0);
if(waserror()){
cclose(tc);
nexterror();
}
if(!indir)
kstrdup(&elem, up->genbuf);
n = devtab[tc->type]->read(tc, &exec, sizeof(Exec), 0);
if(n < 2)
error(Ebadexec);
magic = l2be(exec.magic);
text = l2be(exec.text);
entry = l2be(exec.entry);
if(n==sizeof(Exec) && (magic == AOUT_MAGIC)){
if(text >= USTKTOP-UTZERO
|| entry < UTZERO+sizeof(Exec)
|| entry >= UTZERO+sizeof(Exec)+text)
error(Ebadexec);
break;
}
memmove(line, &exec, sizeof(Exec));
if(indir || line[0]!='#' || line[1]!='!')
error(Ebadexec);
n = shargs(line, n, progarg);
if(n == 0)
error(Ebadexec);
indir = 1;
progarg[n++] = file;
progarg[n] = 0;
validaddr(arg[1], BY2WD, 1);
arg[1] += BY2WD;
file = progarg[0];
if(strlen(elem) >= sizeof progelem)
error(Ebadexec);
strcpy(progelem, elem);
progarg[0] = progelem;
poperror();
cclose(tc);
}
data = l2be(exec.data);
bss = l2be(exec.bss);
t = UTROUND(UTZERO+sizeof(Exec)+text);
d = (t + data + (BY2PG-1)) & ~(BY2PG-1);
bssend = t + data + bss;
b = (bssend + (BY2PG-1)) & ~(BY2PG-1);
if(t >= KZERO || d >= KZERO || b >= KZERO)
error(Ebadexec);
nbytes = sizeof(Tos);
nargs = 0;
if(indir){
argp = progarg;
while(*argp){
a = *argp++;
nbytes += strlen(a) + 1;
nargs++;
}
}
validalign(arg[1], sizeof(char**));
argp = (char**)arg[1];
validaddr((ulong)argp, BY2WD, 0);
while(*argp){
a = *argp++;
if(((ulong)argp&(BY2PG-1)) < BY2WD)
validaddr((ulong)argp, BY2WD, 0);
validaddr((ulong)a, 1, 0);
nbytes += ((char*)vmemchr(a, 0, 0x7FFFFFFF) - a) + 1;
nargs++;
}
ssize = BY2WD*(nargs+1) + ((nbytes+(BY2WD-1)) & ~(BY2WD-1));
if((ssize+4) & 7)
ssize += 4;
spage = (ssize+(BY2PG-1)) >> PGSHIFT;
if(spage > TSTKSIZ)
error(Enovmem);
qlock(&up->seglock);
if(waserror()){
qunlock(&up->seglock);
nexterror();
}
up->seg[ESEG] = newseg(SG_STACK, TSTKTOP-USTKSIZE, USTKSIZE/BY2PG);
tos = (Tos*)(TSTKTOP - sizeof(Tos));
tos->cyclefreq = m->cyclefreq;
cycles((uvlong*)&tos->pcycles);
tos->pcycles = -tos->pcycles;
tos->kcycles = tos->pcycles;
tos->clock = 0;
argv = (char**)(TSTKTOP - ssize);
charp = (char*)(TSTKTOP - nbytes);
args = charp;
if(indir)
argp = progarg;
else
argp = (char**)arg[1];
for(i=0; i<nargs; i++){
if(indir && *argp==0) {
indir = 0;
argp = (char**)arg[1];
}
*argv++ = charp + (USTKTOP-TSTKTOP);
n = strlen(*argp) + 1;
memmove(charp, *argp++, n);
charp += n;
}
free(file0);
free(up->text);
up->text = elem;
elem = nil;
USED(elem);
n = charp - args;
if(n > 128)
n = 128;
a = up->args;
up->args = nil;
free(a);
up->args = smalloc(n);
memmove(up->args, args, n);
if(n>0 && up->args[n-1]!='\0'){
for(i=n-1; i>0; --i)
if(fullrune(up->args+i, n-i))
break;
up->args[i] = 0;
n = i+1;
}
up->nargs = n;
for(i = SSEG; i <= BSEG; i++) {
putseg(up->seg[i]);
up->seg[i] = 0;
}
for(i = BSEG+1; i < NSEG; i++) {
s = up->seg[i];
if(s != 0 && (s->type&SG_CEXEC)) {
putseg(s);
up->seg[i] = 0;
}
}
f = up->fgrp;
for(i=0; i<=f->maxfd; i++)
fdclose(i, CCEXEC);
img = attachimage(SG_TEXT|SG_RONLY, tc, UTZERO, (t-UTZERO)>>PGSHIFT);
ts = img->s;
up->seg[TSEG] = ts;
ts->flushme = 1;
ts->fstart = 0;
ts->flen = sizeof(Exec)+text;
unlock(img);
s = newseg(SG_DATA, t, (d-t)>>PGSHIFT);
up->seg[DSEG] = s;
incref(img);
s->image = img;
s->fstart = ts->fstart+ts->flen;
s->flen = data;
up->seg[BSEG] = newseg(SG_BSS, d, (b-d)>>PGSHIFT);
s = up->seg[ESEG];
up->seg[ESEG] = 0;
up->seg[SSEG] = s;
qunlock(&up->seglock);
poperror();
poperror();
s->base = USTKTOP-USTKSIZE;
s->top = USTKTOP;
relocateseg(s, USTKTOP-TSTKTOP);
if(devtab[tc->type]->dc == L'/')
up->basepri = PriRoot;
up->priority = up->basepri;
poperror();
cclose(tc);
flushmmu();
qlock(&up->debug);
up->nnote = 0;
up->notify = 0;
up->notified = 0;
up->privatemem = 0;
procsetup(up);
qunlock(&up->debug);
if(up->hang)
up->procctl = Proc_stopme;
return execregs(entry, ssize, nargs);
}
int
shargs(char *s, int n, char **ap)
{
int i;
s += 2;
n -= 2;
for(i=0; s[i]!='\n'; i++)
if(i == n-1)
return 0;
s[i] = 0;
*ap = 0;
i = 0;
for(;;) {
while(*s==' ' || *s=='\t')
s++;
if(*s == 0)
break;
i++;
*ap++ = s;
*ap = 0;
while(*s && *s!=' ' && *s!='\t')
s++;
if(*s == 0)
break;
else
*s++ = 0;
}
return i;
}
int
return0(void*)
{
return 0;
}
long
syssleep(ulong *arg)
{
int n;
n = arg[0];
if(n <= 0) {
if (up->edf && (up->edf->flags & Admitted))
edfyield();
else
yield();
return 0;
}
if(n < TK2MS(1))
n = TK2MS(1);
tsleep(&up->sleep, return0, 0, n);
return 0;
}
long
sysalarm(ulong *arg)
{
return procalarm(arg[0]);
}
long
sysexits(ulong *arg)
{
char *status;
char *inval = "invalid exit string";
char buf[ERRMAX];
status = (char*)arg[0];
if(status){
if(waserror())
status = inval;
else{
validaddr((ulong)status, 1, 0);
if(vmemchr(status, 0, ERRMAX) == 0){
memmove(buf, status, ERRMAX);
buf[ERRMAX-1] = 0;
status = buf;
}
poperror();
}
}
pexit(status, 1);
return 0;
}
long
sys_wait(ulong *arg)
{
int pid;
Waitmsg w;
OWaitmsg *ow;
if(arg[0] == 0)
return pwait(nil);
validaddr(arg[0], sizeof(OWaitmsg), 1);
validalign(arg[0], BY2WD);
pid = pwait(&w);
if(pid >= 0){
ow = (OWaitmsg*)arg[0];
readnum(0, ow->pid, NUMSIZE, w.pid, NUMSIZE);
readnum(0, ow->time+TUser*NUMSIZE, NUMSIZE, w.time[TUser], NUMSIZE);
readnum(0, ow->time+TSys*NUMSIZE, NUMSIZE, w.time[TSys], NUMSIZE);
readnum(0, ow->time+TReal*NUMSIZE, NUMSIZE, w.time[TReal], NUMSIZE);
strncpy(ow->msg, w.msg, sizeof(ow->msg));
ow->msg[sizeof(ow->msg)-1] = '\0';
}
return pid;
}
long
sysawait(ulong *arg)
{
int i;
int pid;
Waitmsg w;
ulong n;
n = arg[1];
validaddr(arg[0], n, 1);
pid = pwait(&w);
if(pid < 0)
return -1;
i = snprint((char*)arg[0], n, "%d %lud %lud %lud %q",
w.pid,
w.time[TUser], w.time[TSys], w.time[TReal],
w.msg);
return i;
}
void
werrstr(char *fmt, ...)
{
va_list va;
if(up == nil)
return;
va_start(va, fmt);
vseprint(up->syserrstr, up->syserrstr+ERRMAX, fmt, va);
va_end(va);
}
static long
generrstr(char *buf, uint nbuf)
{
char tmp[ERRMAX];
if(nbuf == 0)
error(Ebadarg);
validaddr((ulong)buf, nbuf, 1);
if(nbuf > sizeof tmp)
nbuf = sizeof tmp;
memmove(tmp, buf, nbuf);
tmp[nbuf-1] = '\0';
memmove(buf, up->syserrstr, nbuf);
buf[nbuf-1] = '\0';
memmove(up->syserrstr, tmp, nbuf);
return 0;
}
long
syserrstr(ulong *arg)
{
return generrstr((char*)arg[0], arg[1]);
}
long
sys_errstr(ulong *arg)
{
return generrstr((char*)arg[0], 64);
}
long
sysnotify(ulong *arg)
{
if(arg[0] != 0)
validaddr(arg[0], sizeof(ulong), 0);
up->notify = (int(*)(void*, char*))(arg[0]);
return 0;
}
long
sysnoted(ulong *arg)
{
if(arg[0]!=NRSTR && !up->notified)
error(Egreg);
return 0;
}
long
syssegbrk(ulong *arg)
{
int i;
ulong addr;
Segment *s;
addr = arg[0];
for(i = 0; i < NSEG; i++) {
s = up->seg[i];
if(s == 0 || addr < s->base || addr >= s->top)
continue;
switch(s->type&SG_TYPE) {
case SG_TEXT:
case SG_DATA:
case SG_STACK:
error(Ebadarg);
default:
return ibrk(arg[1], i);
}
}
error(Ebadarg);
return 0;
}
long
syssegattach(ulong *arg)
{
return segattach(up, arg[0], (char*)arg[1], arg[2], arg[3]);
}
long
syssegdetach(ulong *arg)
{
int i;
ulong addr;
Segment *s;
qlock(&up->seglock);
if(waserror()){
qunlock(&up->seglock);
nexterror();
}
s = 0;
addr = arg[0];
for(i = 0; i < NSEG; i++)
if(s = up->seg[i]) {
qlock(&s->lk);
if((addr >= s->base && addr < s->top) ||
(s->top == s->base && addr == s->base))
goto found;
qunlock(&s->lk);
}
error(Ebadarg);
found:
if(s == up->seg[SSEG]){
qunlock(&s->lk);
error(Ebadarg);
}
up->seg[i] = 0;
qunlock(&s->lk);
putseg(s);
qunlock(&up->seglock);
poperror();
flushmmu();
return 0;
}
long
syssegfree(ulong *arg)
{
Segment *s;
ulong from, to;
from = arg[0];
s = seg(up, from, 1);
if(s == nil)
error(Ebadarg);
to = (from + arg[1]) & ~(BY2PG-1);
from = PGROUND(from);
if(to > s->top) {
qunlock(&s->lk);
error(Ebadarg);
}
mfreeseg(s, from, (to - from) / BY2PG);
qunlock(&s->lk);
flushmmu();
return 0;
}
long
sysbrk_(ulong *arg)
{
return ibrk(arg[0], BSEG);
}
long
sysrendezvous(ulong *arg)
{
uintptr tag, val;
Proc *p, **l;
tag = arg[0];
l = &REND(up->rgrp, tag);
up->rendval = ~(uintptr)0;
lock(up->rgrp);
for(p = *l; p; p = p->rendhash) {
if(p->rendtag == tag) {
*l = p->rendhash;
val = p->rendval;
p->rendval = arg[1];
while(p->mach != 0)
;
ready(p);
unlock(up->rgrp);
return val;
}
l = &p->rendhash;
}
up->rendtag = tag;
up->rendval = arg[1];
up->rendhash = *l;
*l = up;
up->state = Rendezvous;
unlock(up->rgrp);
sched();
return up->rendval;
}
static void
semqueue(Segment *s, long *a, Sema *p)
{
memset(p, 0, sizeof *p);
p->addr = a;
lock(&s->sema);
p->next = &s->sema;
p->prev = s->sema.prev;
p->next->prev = p;
p->prev->next = p;
unlock(&s->sema);
}
static void
semdequeue(Segment *s, Sema *p)
{
lock(&s->sema);
p->next->prev = p->prev;
p->prev->next = p->next;
unlock(&s->sema);
}
static void
semwakeup(Segment *s, long *a, long n)
{
Sema *p;
lock(&s->sema);
for(p=s->sema.next; p!=&s->sema && n>0; p=p->next){
if(p->addr == a && p->waiting){
p->waiting = 0;
coherence();
wakeup(p);
n--;
}
}
unlock(&s->sema);
}
static long
semrelease(Segment *s, long *addr, long delta)
{
long value;
do
value = *addr;
while(!cmpswap(addr, value, value+delta));
semwakeup(s, addr, delta);
return value+delta;
}
static int
canacquire(long *addr)
{
long value;
while((value=*addr) > 0)
if(cmpswap(addr, value, value-1))
return 1;
return 0;
}
static int
semawoke(void *p)
{
coherence();
return !((Sema*)p)->waiting;
}
static int
semacquire(Segment *s, long *addr, int block)
{
int acquired;
Sema phore;
if(canacquire(addr))
return 1;
if(!block)
return 0;
acquired = 0;
semqueue(s, addr, &phore);
for(;;){
phore.waiting = 1;
coherence();
if(canacquire(addr)){
acquired = 1;
break;
}
if(waserror())
break;
sleep(&phore, semawoke, &phore);
poperror();
}
semdequeue(s, &phore);
coherence();
if(!phore.waiting)
semwakeup(s, addr, 1);
if(!acquired)
nexterror();
return 1;
}
static int
tsemacquire(Segment *s, long *addr, ulong ms)
{
int acquired, timedout;
ulong t, elms;
Sema phore;
if(canacquire(addr))
return 1;
if(ms == 0)
return 0;
acquired = timedout = 0;
semqueue(s, addr, &phore);
for(;;){
phore.waiting = 1;
coherence();
if(canacquire(addr)){
acquired = 1;
break;
}
if(waserror())
break;
t = m->ticks;
tsleep(&phore, semawoke, &phore, ms);
elms = TK2MS(m->ticks - t);
poperror();
if(elms >= ms){
timedout = 1;
break;
}
ms -= elms;
}
semdequeue(s, &phore);
coherence();
if(!phore.waiting)
semwakeup(s, addr, 1);
if(timedout)
return 0;
if(!acquired)
nexterror();
return 1;
}
long
syssemacquire(ulong *arg)
{
int block;
long *addr;
Segment *s;
validaddr(arg[0], sizeof(long), 1);
validalign(arg[0], sizeof(long));
addr = (long*)arg[0];
block = arg[1];
if((s = seg(up, (ulong)addr, 0)) == nil)
error(Ebadarg);
if(*addr < 0)
error(Ebadarg);
return semacquire(s, addr, block);
}
long
systsemacquire(ulong *arg)
{
long *addr;
ulong ms;
Segment *s;
validaddr(arg[0], sizeof(long), 1);
validalign(arg[0], sizeof(long));
addr = (long*)arg[0];
ms = arg[1];
if((s = seg(up, (ulong)addr, 0)) == nil)
error(Ebadarg);
if(*addr < 0)
error(Ebadarg);
return tsemacquire(s, addr, ms);
}
long
syssemrelease(ulong *arg)
{
long *addr, delta;
Segment *s;
validaddr(arg[0], sizeof(long), 1);
validalign(arg[0], sizeof(long));
addr = (long*)arg[0];
delta = arg[1];
if((s = seg(up, (ulong)addr, 0)) == nil)
error(Ebadarg);
if(delta < 0 || *addr < 0)
error(Ebadarg);
return semrelease(s, addr, delta);
}
long
sysnsec(ulong *arg)
{
validaddr(arg[0], sizeof(vlong), 1);
validalign(arg[0], sizeof(vlong));
*(vlong*)arg[0] = todget(nil);
return 0;
}