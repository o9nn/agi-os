#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
typedef struct CogProcExt CogProcExt;
struct CogProcExt {
ulong atomid;
short sti;
short lti;
ulong inferences;
ulong cogcycles;
int cogstate;
Lock;
};
enum {
CogIdle = 0,
CogThinking,
CogReasoning,
CogLearning,
CogWaiting,
};
static CogProcExt** cogprocs;
static int ncogprocs;
static int maxcogprocs;
static Lock cogproclock;
void
cogprocinit(void)
{
maxcogprocs = conf.nproc;
cogprocs = malloc(maxcogprocs * sizeof(CogProcExt*));
if(cogprocs == nil)
panic("cogprocinit: no memory");
ncogprocs = 0;
}
CogProcExt*
cogprocalloc(void)
{
CogProcExt *ce;
lock(&cogproclock);
if(ncogprocs >= maxcogprocs) {
unlock(&cogproclock);
return nil;
}
ce = malloc(sizeof(CogProcExt));
if(ce == nil) {
unlock(&cogproclock);
return nil;
}
ce->atomid = 0;
ce->sti = 0;
ce->lti = 0;
ce->inferences = 0;
ce->cogcycles = 0;
ce->cogstate = CogIdle;
cogprocs[ncogprocs++] = ce;
unlock(&cogproclock);
return ce;
}
void
cogprocfree(CogProcExt *ce)
{
int i;
if(ce == nil)
return;
lock(&cogproclock);
for(i = 0; i < ncogprocs; i++) {
if(cogprocs[i] == ce) {
cogprocs[i] = nil;
break;
}
}
free(ce);
unlock(&cogproclock);
}
int
cogpriority(CogProcExt *ce)
{
int pri;
if(ce == nil)
return 0;
lock(ce);
pri = ce->sti;
if(ce->cogstate == CogThinking)
pri += 10;
else if(ce->cogstate == CogReasoning)
pri += 20;
unlock(ce);
return pri;
}
int
cogtimeslice(CogProcExt *ce)
{
int slice;
if(ce == nil)
return HZ/100;
lock(ce);
if(ce->sti > 100)
slice = HZ/50;
else if(ce->sti > 50)
slice = HZ/100;
else
slice = HZ/200;
unlock(ce);
return slice;
}
void
cogthink(CogProcExt *ce)
{
if(ce == nil)
return;
lock(ce);
ce->cogstate = CogThinking;
ce->cogcycles++;
unlock(ce);
}
void
coginfer(CogProcExt *ce)
{
if(ce == nil)
return;
lock(ce);
ce->cogstate = CogReasoning;
ce->inferences++;
ce->cogcycles++;
unlock(ce);
}
void
coglearn(CogProcExt *ce)
{
if(ce == nil)
return;
lock(ce);
ce->cogstate = CogLearning;
ce->cogcycles++;
unlock(ce);
}
void
cogupdate(CogProcExt *ce, short dsti, short dlti)
{
if(ce == nil)
return;
lock(ce);
ce->sti += dsti;
ce->lti += dlti;
if(ce->sti < 0)
ce->sti = 0;
if(ce->lti < 0)
ce->lti = 0;
if(ce->sti > 32767)
ce->sti = 32767;
if(ce->lti > 32767)
ce->lti = 32767;
unlock(ce);
}
void
cogdecayprocs(void)
{
int i;
CogProcExt *ce;
lock(&cogproclock);
for(i = 0; i < ncogprocs; i++) {
ce = cogprocs[i];
if(ce == nil)
continue;
lock(ce);
if(ce->sti > 0)
ce->sti--;
unlock(ce);
}
unlock(&cogproclock);
}
void
cogprocstats(ulong *totalinf, ulong *totalcyc, int *nprocs)
{
int i;
CogProcExt *ce;
ulong inf, cyc;
inf = 0;
cyc = 0;
lock(&cogproclock);
for(i = 0; i < ncogprocs; i++) {
ce = cogprocs[i];
if(ce == nil)
continue;
lock(ce);
inf += ce->inferences;
cyc += ce->cogcycles;
unlock(ce);
}
*nprocs = ncogprocs;
unlock(&cogproclock);
*totalinf = inf;
*totalcyc = cyc;
}
CogProcExt*
cogfindmax(void)
{
int i, maxsti;
CogProcExt *ce, *best;
maxsti = -1;
best = nil;
lock(&cogproclock);
for(i = 0; i < ncogprocs; i++) {
ce = cogprocs[i];
if(ce == nil)
continue;
lock(ce);
if(ce->sti > maxsti) {
maxsti = ce->sti;
best = ce;
}
unlock(ce);
}
unlock(&cogproclock);
return best;
}
void
cogspreadattention(CogProcExt *source, CogProcExt *target, short amount)
{
if(source == nil || target == nil)
return;
lock(source);
lock(target);
if(source->sti >= amount) {
source->sti -= amount;
target->sti += amount;
}
unlock(target);
unlock(source);
}
void
cogboost(CogProcExt *ce, short boost)
{
if(ce == nil)
return;
lock(ce);
ce->sti += boost;
ce->lti += boost / 10;
unlock(ce);
}
void
cogsleep(CogProcExt *ce, int ms)
{
if(ce == nil)
return;
lock(ce);
ce->cogstate = CogWaiting;
ce->sti = (ce->sti * 9) / 10;
unlock(ce);
}
void
cogresume(CogProcExt *ce)
{
if(ce == nil)
return;
lock(ce);
ce->cogstate = CogIdle;
unlock(ce);
}