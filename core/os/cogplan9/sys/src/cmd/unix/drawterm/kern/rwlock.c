#include "u.h"
#include "lib.h"
#include "dat.h"
#include "fns.h"
#include "error.h"
void
rlock(RWlock *l)
{
qlock(&l->x);
lock(&l->lk);
l->readers++;
canqlock(&l->k);
unlock(&l->lk);
qunlock(&l->x);
}
void
runlock(RWlock *l)
{
lock(&l->lk);
if(--l->readers == 0)
qunlock(&l->k);
unlock(&l->lk);
}
void
wlock(RWlock *l)
{
qlock(&l->x);
qlock(&l->k);
}
void
wunlock(RWlock *l)
{
qunlock(&l->k);
qunlock(&l->x);
}