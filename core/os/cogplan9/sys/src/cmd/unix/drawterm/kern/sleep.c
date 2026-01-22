#include	"u.h"
#include	"lib.h"
#include	"dat.h"
#include	"fns.h"
#include	"error.h"
void
sleep(Rendez *r, int (*f)(void*), void *arg)
{
int s;
s = splhi();
lock(&r->lk);
lock(&up->rlock);
if(r->p){
print("double sleep %lud %lud\n", r->p->pid, up->pid);
dumpstack();
}
r->p = up;
if((*f)(arg) || up->notepending){
r->p = nil;
unlock(&up->rlock);
unlock(&r->lk);
} else {
up->state = Wakeme;
up->r = r;
unlock(&up->rlock);
unlock(&r->lk);
procsleep();
}
if(up->notepending) {
up->notepending = 0;
splx(s);
error(Eintr);
}
splx(s);
}
Proc*
wakeup(Rendez *r)
{
Proc *p;
int s;
s = splhi();
lock(&r->lk);
p = r->p;
if(p != nil){
lock(&p->rlock);
if(p->state != Wakeme || p->r != r)
panic("wakeup: state");
r->p = nil;
p->r = nil;
p->state = Running;
procwakeup(p);
unlock(&p->rlock);
}
unlock(&r->lk);
splx(s);
return p;
}