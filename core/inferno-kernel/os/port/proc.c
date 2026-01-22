#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	<interp.h>
Ref	pidalloc;
struct
{
Lock;
Proc*	arena;
Proc*	free;
}procalloc;
typedef struct
{
Lock;
Proc*	head;
Proc*	tail;
}Schedq;
static Schedq	runq[Nrq];
static ulong	occupied;
int	nrdy;
char *statename[] =
{
"Dead",
"Moribund",
"Ready",
"Scheding",
"Running",
"Queueing",
"Wakeme",
"Broken",
"Stopped",
"Rendez",
};
void
schedinit(void)
{
setlabel(&m->sched);
if(up) {
m->proc = nil;
switch(up->state) {
case Running:
ready(up);
break;
case Moribund:
up->state = Dead;
up->qnext = procalloc.free;
procalloc.free = up;
unlock(&procalloc);
break;
}
up->mach = nil;
up = nil;
}
sched();
}
void
sched(void)
{
if(up) {
splhi();
procsave(up);
if(setlabel(&up->sched)) {
spllo();
return;
}
gotolabel(&m->sched);
}
up = runproc();
up->state = Running;
up->mach = MACHP(m->machno);
m->proc = up;
gotolabel(&up->sched);
}
void
ready(Proc *p)
{
int s;
Schedq *rq;
s = splhi();
rq = &runq[p->pri];
lock(runq);
p->rnext = 0;
if(rq->tail)
rq->tail->rnext = p;
else
rq->head = p;
rq->tail = p;
nrdy++;
occupied |= 1<<p->pri;
p->state = Ready;
unlock(runq);
splx(s);
}
int
anyready(void)
{
return occupied & (1<<up->pri);
}
int
anyhigher(void)
{
return occupied & ((1<<up->pri)-1);
}
int
preemption(int tick)
{
if(up != nil && up->state == Running && !up->preempted &&
(anyhigher() || tick && anyready())){
up->preempted = 1;
sched();
splhi();
up->preempted = 0;
return 1;
}
return 0;
}
Proc*
runproc(void)
{
Proc *p, *l;
Schedq *rq, *erq;
erq = runq + Nrq - 1;
loop:
splhi();
for(rq = runq; rq->head == 0; rq++)
if(rq >= erq) {
idlehands();
spllo();
goto loop;
}
if(!canlock(runq))
goto loop;
l = nil;
for(p = rq->head; p != nil; p = p->rnext)
if(p->mp == nil || p->mp == MACHP(m->machno) || p->movetime < MACHP(0)->ticks)
break;
if(p == nil)
p = rq->head;
if(p == 0 || p->mach) {
unlock(runq);
goto loop;
}
if(p->rnext == nil)
rq->tail = l;
if(l)
l->rnext = p->rnext;
else
rq->head = p->rnext;
if(rq->head == nil){
rq->tail = nil;
occupied &= ~(1<<p->pri);
}
nrdy--;
if(p->dbgstop){
p->state = Stopped;
unlock(runq);
goto loop;
}
if(p->state != Ready)
print("runproc %s %lud %s\n", p->text, p->pid, statename[p->state]);
unlock(runq);
p->state = Scheding;
if(p->mp != MACHP(m->machno))
p->movetime = MACHP(0)->ticks + HZ/10;
p->mp = MACHP(m->machno);
cognitive_schedule();
return p;
}
int
setpri(int pri)
{
int p;
p = up->pri;
up->pri = pri;
if(up->state == Running && anyhigher())
sched();
return p;
}
Proc*
newproc(void)
{
Proc *p;
lock(&procalloc);
for(;;) {
if(p = procalloc.free)
break;
unlock(&procalloc);
resrcwait("no procs");
lock(&procalloc);
}
procalloc.free = p->qnext;
unlock(&procalloc);
p->type = Unknown;
p->state = Scheding;
p->pri = PriNormal;
p->psstate = "New";
p->mach = 0;
p->qnext = 0;
p->fpstate = FPINIT;
p->kp = 0;
p->killed = 0;
p->swipend = 0;
p->mp = 0;
p->movetime = 0;
p->delaysched = 0;
p->edf = nil;
memset(&p->defenv, 0, sizeof(p->defenv));
p->env = &p->defenv;
p->dbgreg = 0;
kstrdup(&p->env->user, "*nouser");
p->env->errstr = p->env->errbuf0;
p->env->syserrstr = p->env->errbuf1;
p->pid = incref(&pidalloc);
if(p->pid == 0)
panic("pidalloc");
if(p->kstack == 0)
p->kstack = smalloc(KSTACK);
addprog(p);
proc_cognitive_init(p);
return p;
}
void
procinit(void)
{
Proc *p;
int i;
procalloc.free = xalloc(conf.nproc*sizeof(Proc));
procalloc.arena = procalloc.free;
p = procalloc.free;
for(i=0; i<conf.nproc-1; i++,p++)
p->qnext = p+1;
p->qnext = 0;
debugkey('p', "processes", procdump, 0);
}
void
sleep(Rendez *r, int (*f)(void*), void *arg)
{
int s;
if(up == nil)
panic("sleep() not in process (%lux)", getcallerpc(&r));
s = splhi();
lock(&up->rlock);
lock(r);
if(up->killed || f(arg)){
unlock(r);
}else{
if(r->p != nil) {
print("double sleep pc=0x%lux %lud %lud r=0x%lux\n", getcallerpc(&r), r->p->pid, up->pid, r);
dumpstack();
panic("sleep");
}
up->state = Wakeme;
r->p = up;
unlock(r);
up->swipend = 0;
up->r = r;
unlock(&up->rlock);
sched();
splhi();
lock(&up->rlock);
up->r = nil;
}
if(up->killed || up->swipend) {
up->killed = 0;
up->swipend = 0;
unlock(&up->rlock);
splx(s);
error(Eintr);
}
unlock(&up->rlock);
splx(s);
}
int
tfn(void *arg)
{
return MACHP(0)->ticks >= up->twhen || (*up->tfn)(arg);
}
void
tsleep(Rendez *r, int (*fn)(void*), void *arg, int ms)
{
ulong when;
Proc *f, **l;
if(up == nil)
panic("tsleep() not in process (0x%lux)", getcallerpc(&r));
when = MS2TK(ms)+MACHP(0)->ticks;
lock(&talarm);
if(up->trend) {
l = &talarm.list;
for(f = *l; f; f = f->tlink) {
if(f == up) {
*l = up->tlink;
break;
}
l = &f->tlink;
}
}
l = &talarm.list;
for(f = *l; f; f = f->tlink) {
if(f->twhen >= when)
break;
l = &f->tlink;
}
up->trend = r;
up->twhen = when;
up->tfn = fn;
up->tlink = *l;
*l = up;
unlock(&talarm);
if(waserror()){
up->twhen = 0;
nexterror();
}
sleep(r, tfn, arg);
up->twhen = 0;
poperror();
}
int
wakeup(Rendez *r)
{
Proc *p;
int s;
s = splhi();
lock(r);
p = r->p;
if(p){
r->p = nil;
if(p->state != Wakeme)
panic("wakeup: state");
ready(p);
}
unlock(r);
splx(s);
return p != nil;
}
void
swiproc(Proc *p, int interp)
{
ulong s;
Rendez *r;
if(p == nil)
return;
s = splhi();
lock(&p->rlock);
if(!interp)
p->killed = 1;
r = p->r;
if(r != nil) {
lock(r);
if(r->p == p){
p->swipend = 1;
r->p = nil;
ready(p);
}
unlock(r);
}
unlock(&p->rlock);
splx(s);
}
void
notkilled(void)
{
lock(&up->rlock);
up->killed = 0;
unlock(&up->rlock);
}
void
pexit(char*, int)
{
Osenv *o;
up->alarm = 0;
o = up->env;
if(o != nil){
closefgrp(o->fgrp);
closepgrp(o->pgrp);
closeegrp(o->egrp);
closesigs(o->sigs);
}
proc_cognitive_cleanup(up);
lock(&procalloc);
up->state = Moribund;
sched();
panic("pexit");
}
Proc*
proctab(int i)
{
return &procalloc.arena[i];
}
void
procdump(void)
{
int i;
char *s;
Proc *p;
char tmp[14];
for(i=0; i<conf.nproc; i++) {
p = &procalloc.arena[i];
if(p->state == Dead)
continue;
s = p->psstate;
if(s == nil)
s = "kproc";
if(p->state == Wakeme)
snprint(tmp, sizeof(tmp), " /%.8lux", p->r);
else
*tmp = '\0';
print("%lux:%3lud:%14s pc %.8lux %s/%s qpc %.8lux pri %d%s\n",
p, p->pid, p->text, p->pc, s, statename[p->state], p->qpc, p->pri, tmp);
}
}
void
kproc(char *name, void (*func)(void *), void *arg, int flags)
{
Proc *p;
Pgrp *pg;
Fgrp *fg;
Egrp *eg;
p = newproc();
p->psstate = 0;
p->kp = 1;
p->fpsave = up->fpsave;
p->scallnr = up->scallnr;
p->nerrlab = 0;
kstrdup(&p->env->user, up->env->user);
if(flags & KPDUPPG) {
pg = up->env->pgrp;
incref(pg);
p->env->pgrp = pg;
}
if(flags & KPDUPFDG) {
fg = up->env->fgrp;
incref(fg);
p->env->fgrp = fg;
}
if(flags & KPDUPENVG) {
eg = up->env->egrp;
if(eg != nil)
incref(eg);
p->env->egrp = eg;
}
kprocchild(p, func, arg);
strcpy(p->text, name);
ready(p);
}
void
errorf(char *fmt, ...)
{
va_list arg;
char buf[PRINTSIZE];
va_start(arg, fmt);
vseprint(buf, buf+sizeof(buf), fmt, arg);
va_end(arg);
error(buf);
}
void
error(char *err)
{
if(up == nil)
panic("error(%s) not in a process", err);
spllo();
if(up->nerrlab > NERR)
panic("error stack too deep");
if(err != up->env->errstr)
kstrcpy(up->env->errstr, err, ERRMAX);
setlabel(&up->errlab[NERR-1]);
nexterror();
}
#include "errstr.h"
void
kerrstr(char *err, uint size)
{
char tmp[ERRMAX];
kstrcpy(tmp, up->env->errstr, sizeof(tmp));
kstrcpy(up->env->errstr, err, ERRMAX);
kstrcpy(err, tmp, size);
}
void
kgerrstr(char *err, uint size)
{
char tmp[ERRMAX];
kstrcpy(tmp, up->env->errstr, sizeof(tmp));
kstrcpy(up->env->errstr, err, ERRMAX);
kstrcpy(err, tmp, size);
}
void
kwerrstr(char *fmt, ...)
{
va_list arg;
char buf[ERRMAX];
va_start(arg, fmt);
vseprint(buf, buf+sizeof(buf), fmt, arg);
va_end(arg);
kstrcpy(up->env->errstr, buf, ERRMAX);
}
void
werrstr(char *fmt, ...)
{
va_list arg;
char buf[ERRMAX];
va_start(arg, fmt);
vseprint(buf, buf+sizeof(buf), fmt, arg);
va_end(arg);
kstrcpy(up->env->errstr, buf, ERRMAX);
}
void
nexterror(void)
{
gotolabel(&up->errlab[--up->nerrlab]);
}
void*
waserr(void)
{
up->nerrlab++;
return &up->errlab[up->nerrlab-1];
}
void
poperr(void)
{
up->nerrlab--;
}
char*
enverror(void)
{
return up->env->errstr;
}
void
exhausted(char *resource)
{
char buf[64];
snprint(buf, sizeof(buf), "no free %s", resource);
iprint("%s\n", buf);
error(buf);
}
void
renameuser(char *old, char *new)
{
Proc *p, *ep;
Osenv *o;
ep = procalloc.arena+conf.nproc;
for(p = procalloc.arena; p < ep; p++) {
o = &p->defenv;
if(o->user != nil && strcmp(o->user, old) == 0)
kstrdup(&o->user, new);
}
}
int
return0(void*)
{
return 0;
}
void
setid(char *name, int owner)
{
if(!owner || iseve())
kstrdup(&up->env->user, name);
}
void
rptwakeup(void *o, void *ar)
{
Rept *r;
r = ar;
if(r == nil)
return;
lock(&r->l);
r->o = o;
unlock(&r->l);
wakeup(&r->r);
}
static int
rptactive(void *a)
{
Rept *r = a;
int i;
lock(&r->l);
i = r->active(r->o);
unlock(&r->l);
return i;
}
static void
rproc(void *a)
{
long now, then;
ulong t;
int i;
void *o;
Rept *r;
r = a;
t = r->t;
Wait:
sleep(&r->r, rptactive, r);
lock(&r->l);
o = r->o;
unlock(&r->l);
then = TK2MS(MACHP(0)->ticks);
for(;;){
tsleep(&up->sleep, return0, nil, t);
now = TK2MS(MACHP(0)->ticks);
if(waserror())
break;
i = r->ck(o, now-then);
poperror();
if(i == -1)
goto Wait;
if(i == 0)
continue;
then = now;
acquire();
if(waserror()) {
release();
break;
}
r->f(o);
poperror();
release();
}
pexit("", 0);
}
void*
rptproc(char *s, int t, void *o, int (*active)(void*), int (*ck)(void*, int), void (*f)(void*))
{
Rept *r;
r = mallocz(sizeof(Rept), 1);
if(r == nil)
return nil;
r->t = t;
r->active = active;
r->ck = ck;
r->f = f;
r->o = o;
kproc(s, rproc, r, KPDUP);
return r;
}