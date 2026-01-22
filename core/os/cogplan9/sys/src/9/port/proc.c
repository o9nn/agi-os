#include <u.h>
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "../port/edf.h"
#include <trace.h>
int schedgain = 30;
int nrdy;
Ref noteidalloc;
void updatecpu(Proc*);
int reprioritize(Proc*);
ulong delayedscheds;
long skipscheds;
long preempts;
ulong load;
static Ref pidalloc;
static struct Procalloc
{
Lock;
Proc* ht[128];
Proc* arena;
Proc* free;
} procalloc;
enum
{
Q=10,
DQ=4,
Scaling=2,
};
Schedq runq[Nrq];
ulong runvec;
char *statename[] =
{
"Dead",
"Moribund",
"Ready",
"Scheding",
"Running",
"Queueing",
"QueueingR",
"QueueingW",
"Wakeme",
"Broken",
"Stopped",
"Rendez",
"Waitrelease",
};
static void pidhash(Proc*);
static void pidunhash(Proc*);
static void rebalance(void);
void
schedinit(void)
{
Edf *e;
setlabel(&m->sched);
if(up) {
if((e = up->edf) && (e->flags & Admitted))
edfrecord(up);
m->proc = 0;
switch(up->state) {
case Running:
ready(up);
break;
case Moribund:
up->state = Dead;
edfstop(up);
if (up->edf)
free(up->edf);
up->edf = nil;
mmurelease(up);
up->qnext = procalloc.free;
procalloc.free = up;
unlock(&palloc);
unlock(&procalloc);
break;
}
up->mach = nil;
updatecpu(up);
up = nil;
}
sched();
}
void
sched(void)
{
Proc *p;
if(m->ilockdepth)
panic("cpu%d: ilockdepth %d, last lock %#p at %#p, sched called from %#p",
m->machno,
m->ilockdepth,
up? up->lastilock: nil,
(up && up->lastilock)? up->lastilock->pc: 0,
getcallerpc(&p+2));
if(up){
if(up->nlocks.ref)
if(up->state != Moribund)
if(up->delaysched < 20
|| palloc.Lock.p == up
|| procalloc.Lock.p == up){
up->delaysched++;
delayedscheds++;
return;
}
up->delaysched = 0;
splhi();
m->cs++;
procsave(up);
if(setlabel(&up->sched)){
procrestore(up);
spllo();
return;
}
gotolabel(&m->sched);
}
p = runproc();
if(!p->edf){
updatecpu(p);
p->priority = reprioritize(p);
}
if(p != m->readied)
m->schedticks = m->ticks + HZ/10;
m->readied = 0;
up = p;
up->state = Running;
up->mach = MACHP(m->machno);
m->proc = up;
mmuswitch(up);
gotolabel(&up->sched);
}
int
anyready(void)
{
return runvec;
}
int
anyhigher(void)
{
return runvec & ~((1<<(up->priority+1))-1);
}
void
hzsched(void)
{
if(m->machno == 0)
rebalance();
if(anyhigher()
|| (!up->fixedpri && m->ticks > m->schedticks && anyready())){
m->readied = nil;
up->delaysched++;
}
}
int
preempted(void)
{
if(up && up->state == Running)
if(up->preempted == 0)
if(anyhigher())
if(!active.exiting){
m->readied = nil;
up->preempted = 1;
sched();
splhi();
up->preempted = 0;
return 1;
}
return 0;
}
void
updatecpu(Proc *p)
{
int n, t, ocpu;
int D = schedgain*HZ*Scaling;
if(p->edf)
return;
t = MACHP(0)->ticks*Scaling + Scaling/2;
n = t - p->lastupdate;
p->lastupdate = t;
if(n == 0)
return;
if(n > D)
n = D;
ocpu = p->cpu;
if(p != up)
p->cpu = (ocpu*(D-n))/D;
else{
t = 1000 - ocpu;
t = (t*(D-n))/D;
p->cpu = 1000 - t;
}
}
int
reprioritize(Proc *p)
{
int fairshare, n, load, ratio;
load = MACHP(0)->load;
if(load == 0)
return p->basepri;
fairshare = (conf.nmach*1000*1000)/load;
n = p->cpu;
if(n == 0)
n = 1;
ratio = (fairshare+n/2) / n;
if(ratio > p->basepri)
ratio = p->basepri;
if(ratio < 0)
panic("reprioritize");
return ratio;
}
void
queueproc(Schedq *rq, Proc *p)
{
int pri;
pri = rq - runq;
lock(runq);
p->priority = pri;
p->rnext = 0;
if(rq->tail)
rq->tail->rnext = p;
else
rq->head = p;
rq->tail = p;
rq->n++;
nrdy++;
runvec |= 1<<pri;
unlock(runq);
}
Proc*
dequeueproc(Schedq *rq, Proc *tp)
{
Proc *l, *p;
if(!canlock(runq))
return nil;
l = 0;
for(p = rq->head; p; p = p->rnext){
if(p == tp)
break;
l = p;
}
if(p == 0 || p->mach){
unlock(runq);
return nil;
}
if(p->rnext == 0)
rq->tail = l;
if(l)
l->rnext = p->rnext;
else
rq->head = p->rnext;
if(rq->head == nil)
runvec &= ~(1<<(rq-runq));
rq->n--;
nrdy--;
if(p->state != Ready)
print("dequeueproc %s %lud %s\n", p->text, p->pid, statename[p->state]);
unlock(runq);
return p;
}
void
ready(Proc *p)
{
int s, pri;
Schedq *rq;
void (*pt)(Proc*, int, vlong);
s = splhi();
if(edfready(p)){
splx(s);
return;
}
if(up != p && (p->wired == nil || p->wired == m))
m->readied = p;
updatecpu(p);
pri = reprioritize(p);
p->priority = pri;
rq = &runq[pri];
p->state = Ready;
queueproc(rq, p);
pt = proctrace;
if(pt)
pt(p, SReady, 0);
splx(s);
}
void
yield(void)
{
if(anyready()){
up->lastupdate -= Scaling/2;
sched();
}
}
ulong balancetime;
static void
rebalance(void)
{
int pri, npri, t, x;
Schedq *rq;
Proc *p;
t = m->ticks;
if(t - balancetime < HZ)
return;
balancetime = t;
for(pri=0, rq=runq; pri<Npriq; pri++, rq++){
another:
p = rq->head;
if(p == nil)
continue;
if(p->mp != MACHP(m->machno))
continue;
if(pri == p->basepri)
continue;
updatecpu(p);
npri = reprioritize(p);
if(npri != pri){
x = splhi();
p = dequeueproc(rq, p);
if(p)
queueproc(&runq[npri], p);
splx(x);
goto another;
}
}
}
Proc*
runproc(void)
{
Schedq *rq;
Proc *p;
ulong start, now;
int i;
void (*pt)(Proc*, int, vlong);
start = perfticks();
if((p=m->readied) && p->mach==0 && p->state==Ready
&& (p->wired == nil || p->wired == m)
&& runq[Nrq-1].head == nil && runq[Nrq-2].head == nil){
skipscheds++;
rq = &runq[p->priority];
goto found;
}
preempts++;
loop:
spllo();
for(i = 0;; i++){
for(rq = &runq[Nrq-1]; rq >= runq; rq--){
for(p = rq->head; p; p = p->rnext){
if(p->mp == nil || p->mp == MACHP(m->machno)
|| (!p->wired && i > 0))
goto found;
}
}
idlehands();
now = perfticks();
m->perf.inidle += now-start;
start = now;
}
found:
splhi();
p = dequeueproc(rq, p);
if(p == nil)
goto loop;
p->state = Scheding;
p->mp = MACHP(m->machno);
if(edflock(p)){
edfrun(p, rq == &runq[PriEdf]);
edfunlock();
}
pt = proctrace;
if(pt)
pt(p, SRun, 0);
return p;
}
int
canpage(Proc *p)
{
int ok = 0;
splhi();
lock(runq);
if(p->mach == 0) {
p->newtlb = 1;
ok = 1;
}
unlock(runq);
spllo();
return ok;
}
void
noprocpanic(char *msg)
{
lock(&active);
active.machs &= ~(1<<m->machno);
active.exiting = 1;
unlock(&active);
procdump();
delay(1000);
panic(msg);
}
Proc*
newproc(void)
{
char msg[64];
Proc *p;
lock(&procalloc);
while((p = procalloc.free) == nil) {
unlock(&procalloc);
snprint(msg, sizeof msg, "no procs; %s forking",
up? up->text: "kernel");
if(getconf("*noprocspersist") == nil)
noprocpanic(msg);
resrcwait(msg);
lock(&procalloc);
}
procalloc.free = p->qnext;
unlock(&procalloc);
p->state = Scheding;
p->psstate = "New";
p->mach = 0;
p->qnext = 0;
p->nchild = 0;
p->nwait = 0;
p->waitq = 0;
p->parent = 0;
p->pgrp = 0;
p->egrp = 0;
p->fgrp = 0;
p->rgrp = 0;
p->pdbg = 0;
p->fpstate = FPinit;
p->kp = 0;
if(up && up->procctl == Proc_tracesyscall)
p->procctl = Proc_tracesyscall;
else
p->procctl = 0;
p->syscalltrace = 0;
p->notepending = 0;
p->ureg = 0;
p->privatemem = 0;
p->noswap = 0;
p->errstr = p->errbuf0;
p->syserrstr = p->errbuf1;
p->errbuf0[0] = '\0';
p->errbuf1[0] = '\0';
p->nlocks.ref = 0;
p->delaysched = 0;
p->trace = 0;
kstrdup(&p->user, "*nouser");
kstrdup(&p->text, "*notext");
kstrdup(&p->args, "");
p->nargs = 0;
p->setargs = 0;
memset(p->seg, 0, sizeof p->seg);
p->pid = incref(&pidalloc);
pidhash(p);
p->noteid = incref(&noteidalloc);
if(p->pid==0 || p->noteid==0)
panic("pidalloc");
if(p->kstack == 0)
p->kstack = smalloc(KSTACK);
p->mp = 0;
p->wired = 0;
procpriority(p, PriNormal, 0);
p->cpu = 0;
p->lastupdate = MACHP(0)->ticks*Scaling;
p->edf = nil;
return p;
}
void
procwired(Proc *p, int bm)
{
Proc *pp;
int i;
char nwired[MAXMACH];
Mach *wm;
if(bm < 0){
memset(nwired, 0, sizeof(nwired));
p->wired = 0;
pp = proctab(0);
for(i=0; i<conf.nproc; i++, pp++){
wm = pp->wired;
if(wm && pp->pid)
nwired[wm->machno]++;
}
bm = 0;
for(i=0; i<conf.nmach; i++)
if(nwired[i] < nwired[bm])
bm = i;
} else {
bm = bm % conf.nmach;
}
p->wired = MACHP(bm);
p->mp = p->wired;
}
void
procpriority(Proc *p, int pri, int fixed)
{
if(pri >= Npriq)
pri = Npriq - 1;
else if(pri < 0)
pri = 0;
p->basepri = pri;
p->priority = pri;
if(fixed){
p->fixedpri = 1;
} else {
p->fixedpri = 0;
}
}
void
procinit0(void)
{
Proc *p;
int i;
procalloc.free = xalloc(conf.nproc*sizeof(Proc));
if(procalloc.free == nil){
xsummary();
panic("cannot allocate %lud procs (%ludMB)\n", conf.nproc, conf.nproc*sizeof(Proc)/(1024*1024));
}
procalloc.arena = procalloc.free;
p = procalloc.free;
for(i=0; i<conf.nproc-1; i++,p++)
p->qnext = p+1;
p->qnext = 0;
}
void
sleep(Rendez *r, int (*f)(void*), void *arg)
{
int s;
void (*pt)(Proc*, int, vlong);
s = splhi();
if(up->nlocks.ref)
print("process %lud sleeps with %lud locks held, last lock %#p locked at pc %#lux, sleep called from %#p\n",
up->pid, up->nlocks.ref, up->lastlock, up->lastlock->pc, getcallerpc(&r));
lock(r);
lock(&up->rlock);
if(r->p){
print("double sleep called from %#p, %lud %lud\n", getcallerpc(&r), r->p->pid, up->pid);
dumpstack();
}
r->p = up;
if((*f)(arg) || up->notepending){
r->p = nil;
unlock(&up->rlock);
unlock(r);
} else {
pt = proctrace;
if(pt)
pt(up, SSleep, 0);
up->state = Wakeme;
up->r = r;
m->cs++;
procsave(up);
if(setlabel(&up->sched)) {
procrestore(up);
spllo();
} else {
unlock(&up->rlock);
unlock(r);
gotolabel(&m->sched);
}
}
if(up->notepending) {
up->notepending = 0;
splx(s);
if(up->procctl == Proc_exitme && up->closingfgrp)
forceclosefgrp();
error(Eintr);
}
splx(s);
}
static int
tfn(void *arg)
{
return up->trend == nil || up->tfn(arg);
}
void
twakeup(Ureg*, Timer *t)
{
Proc *p;
Rendez *trend;
p = t->ta;
trend = p->trend;
p->trend = 0;
if(trend)
wakeup(trend);
}
void
tsleep(Rendez *r, int (*fn)(void*), void *arg, ulong ms)
{
if (up->tt){
print("tsleep: timer active: mode %d, tf %#p\n", up->tmode, up->tf);
timerdel(up);
}
up->tns = MS2NS(ms);
up->tf = twakeup;
up->tmode = Trelative;
up->ta = up;
up->trend = r;
up->tfn = fn;
timeradd(up);
if(waserror()){
timerdel(up);
nexterror();
}
sleep(r, tfn, arg);
if(up->tt)
timerdel(up);
up->twhen = 0;
poperror();
}
Proc*
wakeup(Rendez *r)
{
Proc *p;
int s;
s = splhi();
lock(r);
p = r->p;
if(p != nil){
lock(&p->rlock);
if(p->state != Wakeme || p->r != r){
iprint("%p %p %d\n", p->r, r, p->state);
panic("wakeup: state");
}
r->p = nil;
p->r = nil;
ready(p);
unlock(&p->rlock);
}
unlock(r);
splx(s);
return p;
}
int
postnote(Proc *p, int dolock, char *n, int flag)
{
int s, ret;
Rendez *r;
Proc *d, **l;
if(dolock)
qlock(&p->debug);
if(flag != NUser && (p->notify == 0 || p->notified))
p->nnote = 0;
ret = 0;
if(p->nnote < NNOTE) {
strcpy(p->note[p->nnote].msg, n);
p->note[p->nnote++].flag = flag;
ret = 1;
}
p->notepending = 1;
if(dolock)
qunlock(&p->debug);
for(;;){
s = splhi();
lock(&p->rlock);
r = p->r;
if(r == nil)
break;
if(canlock(r)){
if(p->state != Wakeme || r->p != p)
panic("postnote: state %d %d %d", r->p != p, p->r != r, p->state);
p->r = nil;
r->p = nil;
ready(p);
unlock(r);
break;
}
unlock(&p->rlock);
splx(s);
sched();
}
unlock(&p->rlock);
splx(s);
if(p->state != Rendezvous)
return ret;
lock(p->rgrp);
if(p->state == Rendezvous) {
p->rendval = ~0;
l = &REND(p->rgrp, p->rendtag);
for(d = *l; d; d = d->rendhash) {
if(d == p) {
*l = p->rendhash;
break;
}
l = &d->rendhash;
}
ready(p);
}
unlock(p->rgrp);
return ret;
}
#define NBROKEN 4
struct
{
QLock;
int n;
Proc *p[NBROKEN];
}broken;
void
addbroken(Proc *p)
{
qlock(&broken);
if(broken.n == NBROKEN) {
ready(broken.p[0]);
memmove(&broken.p[0], &broken.p[1], sizeof(Proc*)*(NBROKEN-1));
--broken.n;
}
broken.p[broken.n++] = p;
qunlock(&broken);
edfstop(up);
p->state = Broken;
p->psstate = 0;
sched();
}
void
unbreak(Proc *p)
{
int b;
qlock(&broken);
for(b=0; b < broken.n; b++)
if(broken.p[b] == p) {
broken.n--;
memmove(&broken.p[b], &broken.p[b+1],
sizeof(Proc*)*(NBROKEN-(b+1)));
ready(p);
break;
}
qunlock(&broken);
}
int
freebroken(void)
{
int i, n;
qlock(&broken);
n = broken.n;
for(i=0; i<n; i++) {
ready(broken.p[i]);
broken.p[i] = 0;
}
broken.n = 0;
qunlock(&broken);
return n;
}
void
pexit(char *exitstr, int freemem)
{
Proc *p;
Segment **s, **es;
long utime, stime;
Waitq *wq, *f, *next;
Fgrp *fgrp;
Egrp *egrp;
Rgrp *rgrp;
Pgrp *pgrp;
Chan *dot;
void (*pt)(Proc*, int, vlong);
if(up->syscalltrace)
free(up->syscalltrace);
up->alarm = 0;
if (up->tt)
timerdel(up);
pt = proctrace;
if(pt)
pt(up, SDead, 0);
qlock(&up->debug);
fgrp = up->fgrp;
up->fgrp = nil;
egrp = up->egrp;
up->egrp = nil;
rgrp = up->rgrp;
up->rgrp = nil;
pgrp = up->pgrp;
up->pgrp = nil;
dot = up->dot;
up->dot = nil;
qunlock(&up->debug);
if(fgrp)
closefgrp(fgrp);
if(egrp)
closeegrp(egrp);
if(rgrp)
closergrp(rgrp);
if(dot)
cclose(dot);
if(pgrp)
closepgrp(pgrp);
if(up->kp == 0) {
p = up->parent;
if(p == 0) {
if(exitstr == 0)
exitstr = "unknown";
panic("boot process died: %s", exitstr);
}
while(waserror())
;
wq = smalloc(sizeof(Waitq));
poperror();
wq->w.pid = up->pid;
utime = up->time[TUser] + up->time[TCUser];
stime = up->time[TSys] + up->time[TCSys];
wq->w.time[TUser] = tk2ms(utime);
wq->w.time[TSys] = tk2ms(stime);
wq->w.time[TReal] = tk2ms(MACHP(0)->ticks - up->time[TReal]);
if(exitstr && exitstr[0])
snprint(wq->w.msg, sizeof(wq->w.msg), "%s %lud: %s", up->text, up->pid, exitstr);
else
wq->w.msg[0] = '\0';
lock(&p->exl);
if(p->pid == up->parentpid && p->state != Broken) {
p->nchild--;
p->time[TCUser] += utime;
p->time[TCSys] += stime;
if(p->nwait < 128) {
wq->next = p->waitq;
p->waitq = wq;
p->nwait++;
wq = nil;
wakeup(&p->waitr);
}
}
unlock(&p->exl);
if(wq)
free(wq);
}
if(!freemem)
addbroken(up);
qlock(&up->seglock);
es = &up->seg[NSEG];
for(s = up->seg; s < es; s++) {
if(*s) {
putseg(*s);
*s = 0;
}
}
qunlock(&up->seglock);
lock(&up->exl);
pidunhash(up);
up->pid = 0;
wakeup(&up->waitr);
unlock(&up->exl);
for(f = up->waitq; f; f = next) {
next = f->next;
free(f);
}
qlock(&up->debug);
if(up->pdbg) {
wakeup(&up->pdbg->sleep);
up->pdbg = 0;
}
qunlock(&up->debug);
lock(&procalloc);
lock(&palloc);
edfstop(up);
up->state = Moribund;
sched();
panic("pexit");
}
int
haswaitq(void *x)
{
Proc *p;
p = (Proc *)x;
return p->waitq != 0;
}
ulong
pwait(Waitmsg *w)
{
ulong cpid;
Waitq *wq;
if(!canqlock(&up->qwaitr))
error(Einuse);
if(waserror()) {
qunlock(&up->qwaitr);
nexterror();
}
lock(&up->exl);
if(up->nchild == 0 && up->waitq == 0) {
unlock(&up->exl);
error(Enochild);
}
unlock(&up->exl);
sleep(&up->waitr, haswaitq, up);
lock(&up->exl);
wq = up->waitq;
up->waitq = wq->next;
up->nwait--;
unlock(&up->exl);
qunlock(&up->qwaitr);
poperror();
if(w)
memmove(w, &wq->w, sizeof(Waitmsg));
cpid = wq->w.pid;
free(wq);
return cpid;
}
Proc*
proctab(int i)
{
return &procalloc.arena[i];
}
void
dumpaproc(Proc *p)
{
ulong bss;
char *s;
if(p == 0)
return;
bss = 0;
if(p->seg[BSEG])
bss = p->seg[BSEG]->top;
s = p->psstate;
if(s == 0)
s = statename[p->state];
print("%3lud:%10s pc %8lux dbgpc %8lux  %8s (%s) ut %ld st %ld bss %lux qpc %lux nl %lud nd %lud lpc %lux pri %lud\n",
p->pid, p->text, p->pc, dbgpc(p), s, statename[p->state],
p->time[0], p->time[1], bss, p->qpc, p->nlocks.ref, p->delaysched, p->lastlock ? p->lastlock->pc : 0, p->priority);
}
void
procdump(void)
{
int i;
Proc *p;
if(up)
print("up %lud\n", up->pid);
else
print("no current process\n");
for(i=0; i<conf.nproc; i++) {
p = &procalloc.arena[i];
if(p->state == Dead)
continue;
dumpaproc(p);
}
}
void
procflushseg(Segment *s)
{
int i, ns, nm, nwait;
Proc *p;
nwait = 0;
for(i=0; i<conf.nproc; i++) {
p = &procalloc.arena[i];
if(p->state == Dead)
continue;
for(ns = 0; ns < NSEG; ns++)
if(p->seg[ns] == s){
p->newtlb = 1;
for(nm = 0; nm < conf.nmach; nm++){
if(MACHP(nm)->proc == p){
MACHP(nm)->flushmmu = 1;
nwait++;
}
}
break;
}
}
if(nwait == 0)
return;
for(nm = 0; nm < conf.nmach; nm++)
if(MACHP(nm) != m)
while(MACHP(nm)->flushmmu)
sched();
}
void
scheddump(void)
{
Proc *p;
Schedq *rq;
for(rq = &runq[Nrq-1]; rq >= runq; rq--){
if(rq->head == 0)
continue;
print("rq%ld:", rq-runq);
for(p = rq->head; p; p = p->rnext)
print(" %lud(%lud)", p->pid, m->ticks - p->readytime);
print("\n");
delay(150);
}
print("nrdy %d\n", nrdy);
}
void
kproc(char *name, void (*func)(void *), void *arg)
{
Proc *p;
static Pgrp *kpgrp;
p = newproc();
p->psstate = 0;
p->procmode = 0640;
p->kp = 1;
p->noswap = 1;
p->fpsave = up->fpsave;
p->scallnr = up->scallnr;
p->s = up->s;
p->nerrlab = 0;
p->slash = up->slash;
p->dot = up->dot;
if(p->dot)
incref(p->dot);
memmove(p->note, up->note, sizeof(p->note));
p->nnote = up->nnote;
p->notified = 0;
p->lastnote = up->lastnote;
p->notify = up->notify;
p->ureg = 0;
p->dbgreg = 0;
procpriority(p, PriKproc, 0);
kprocchild(p, func, arg);
kstrdup(&p->user, eve);
kstrdup(&p->text, name);
if(kpgrp == 0)
kpgrp = newpgrp();
p->pgrp = kpgrp;
incref(kpgrp);
memset(p->time, 0, sizeof(p->time));
p->time[TReal] = MACHP(0)->ticks;
ready(p);
}
void
procctl(Proc *p)
{
char *state;
ulong s;
switch(p->procctl) {
case Proc_exitbig:
spllo();
pexit("Killed: Insufficient physical memory", 1);
case Proc_exitme:
spllo();
pexit("Killed", 1);
case Proc_traceme:
if(p->nnote == 0)
return;
case Proc_stopme:
p->procctl = 0;
state = p->psstate;
p->psstate = "Stopped";
s = spllo();
qlock(&p->debug);
if(p->pdbg) {
wakeup(&p->pdbg->sleep);
p->pdbg = 0;
}
qunlock(&p->debug);
splhi();
p->state = Stopped;
sched();
p->psstate = state;
splx(s);
return;
}
}
#include "errstr.h"
void
error(char *err)
{
spllo();
assert(up->nerrlab < NERR);
kstrcpy(up->errstr, err, ERRMAX);
setlabel(&up->errlab[NERR-1]);
nexterror();
}
void
nexterror(void)
{
gotolabel(&up->errlab[--up->nerrlab]);
}
void
exhausted(char *resource)
{
char buf[ERRMAX];
snprint(buf, sizeof buf, "no free %s", resource);
iprint("%s\n", buf);
error(buf);
}
void
killbig(char *why)
{
int i;
Segment *s;
ulong l, max;
Proc *p, *ep, *kp;
max = 0;
kp = 0;
ep = procalloc.arena+conf.nproc;
for(p = procalloc.arena; p < ep; p++) {
if(p->state == Dead || p->kp)
continue;
l = 0;
for(i=1; i<NSEG; i++) {
s = p->seg[i];
if(s != 0)
l += s->top - s->base;
}
if(l > max && ((p->procmode&0222) || strcmp(eve, p->user)!=0)) {
kp = p;
max = l;
}
}
print("%lud: %s killed: %s\n", kp->pid, kp->text, why);
for(p = procalloc.arena; p < ep; p++) {
if(p->state == Dead || p->kp)
continue;
if(p != kp && p->seg[BSEG] && p->seg[BSEG] == kp->seg[BSEG])
p->procctl = Proc_exitbig;
}
kp->procctl = Proc_exitbig;
for(i = 0; i < NSEG; i++) {
s = kp->seg[i];
if(s != 0 && canqlock(&s->lk)) {
mfreeseg(s, s->base, (s->top - s->base)/BY2PG);
qunlock(&s->lk);
}
}
}
void
renameuser(char *old, char *new)
{
Proc *p, *ep;
ep = procalloc.arena+conf.nproc;
for(p = procalloc.arena; p < ep; p++)
if(p->user!=nil && strcmp(old, p->user)==0)
kstrdup(&p->user, new);
}
void
accounttime(void)
{
Proc *p;
ulong n, per;
static ulong nrun;
p = m->proc;
if(p) {
nrun++;
p->time[p->insyscall]++;
}
n = perfticks();
per = n - m->perf.last;
m->perf.last = n;
per = (m->perf.period*(HZ-1) + per)/HZ;
if(per != 0)
m->perf.period = per;
m->perf.avg_inidle = (m->perf.avg_inidle*(HZ-1)+m->perf.inidle)/HZ;
m->perf.inidle = 0;
m->perf.avg_inintr = (m->perf.avg_inintr*(HZ-1)+m->perf.inintr)/HZ;
m->perf.inintr = 0;
if(m->machno != 0)
return;
n = nrun;
nrun = 0;
n = (nrdy+n)*1000;
m->load = (m->load*(HZ-1)+n)/HZ;
}
static void
pidhash(Proc *p)
{
int h;
h = p->pid % nelem(procalloc.ht);
lock(&procalloc);
p->pidhash = procalloc.ht[h];
procalloc.ht[h] = p;
unlock(&procalloc);
}
static void
pidunhash(Proc *p)
{
int h;
Proc **l;
h = p->pid % nelem(procalloc.ht);
lock(&procalloc);
for(l = &procalloc.ht[h]; *l != nil; l = &(*l)->pidhash)
if(*l == p){
*l = p->pidhash;
break;
}
unlock(&procalloc);
}
int
procindex(ulong pid)
{
Proc *p;
int h;
int s;
s = -1;
h = pid % nelem(procalloc.ht);
lock(&procalloc);
for(p = procalloc.ht[h]; p != nil; p = p->pidhash)
if(p->pid == pid){
s = p - procalloc.arena;
break;
}
unlock(&procalloc);
return s;
}