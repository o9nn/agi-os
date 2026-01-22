#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
typedef struct CogAtom CogAtom;
struct CogAtom
{
ulong id;
int type;
char name[256];
CogAtom **outgoing;
int noutgoing;
float tvstrength;
float tvconf;
short sti;
short lti;
Lock;
};
typedef struct KernelAtomSpace KernelAtomSpace;
struct KernelAtomSpace
{
CogAtom **atoms;
int natoms;
int maxatoms;
ulong nextid;
QLock;
};
typedef struct CogContext CogContext;
struct CogContext
{
int ref;
int ctxid;
KernelAtomSpace *as;
short stitotal;
short ltitotal;
Lock;
};
typedef struct CogVMState CogVMState;
struct CogVMState
{
int running;
ulong cycles;
ulong inferences;
ulong allocations;
Lock;
};
enum
{
AtomNode = 1,
ConceptNode,
PredicateNode,
InheritanceLink,
SimilarityLink,
EvaluationLink,
Qdir = 0,
Qclone,
Qatomspace,
Qpln,
Qecan,
Qcogvm,
Qstats,
Qctl,
MaxAtoms = 1000000,
};
static Dirtab cogdir[] =
{
".", {Qdir, 0, QTDIR}, 0, DMDIR|0555,
"clone", {Qclone}, 0, 0666,
"atomspace", {Qatomspace}, 0, 0666,
"pln", {Qpln}, 0, 0666,
"ecan", {Qecan}, 0, 0666,
"cogvm", {Qcogvm}, 0, 0444,
"stats", {Qstats}, 0, 0444,
"ctl", {Qctl}, 0, 0666,
};
static struct {
KernelAtomSpace atomspace;
CogVMState vmstate;
CogContext **contexts;
int ncontexts;
int maxcontexts;
QLock;
} cogkernel;
static char Enomem[] = "out of cognitive memory";
static char Ebadcmd[] = "bad cognitive command";
static void
cogatomspaceinit(void)
{
KernelAtomSpace *as;
as = &cogkernel.atomspace;
as->maxatoms = MaxAtoms;
as->atoms = malloc(MaxAtoms * sizeof(CogAtom*));
if(as->atoms == nil)
panic("cogatomspaceinit: no memory");
as->natoms = 0;
as->nextid = 1;
}
static CogAtom*
cogatomcreate(int type, char *name)
{
KernelAtomSpace *as;
CogAtom *atom;
as = &cogkernel.atomspace;
qlock(as);
if(as->natoms >= as->maxatoms) {
qunlock(as);
error(Enomem);
}
atom = malloc(sizeof(CogAtom));
if(atom == nil) {
qunlock(as);
error(Enomem);
}
atom->id = as->nextid++;
atom->type = type;
if(name)
strncpy(atom->name, name, sizeof(atom->name)-1);
atom->outgoing = nil;
atom->noutgoing = 0;
atom->tvstrength = 0.5;
atom->tvconf = 0.5;
atom->sti = 0;
atom->lti = 0;
as->atoms[as->natoms++] = atom;
qunlock(as);
return atom;
}
static CogAtom*
cogatomfind(ulong id)
{
KernelAtomSpace *as;
int i;
as = &cogkernel.atomspace;
qlock(as);
for(i = 0; i < as->natoms; i++) {
if(as->atoms[i]->id == id) {
qunlock(as);
return as->atoms[i];
}
}
qunlock(as);
return nil;
}
static void
cogplndeduction(CogAtom *ab, CogAtom *bc, float *strength, float *conf)
{
*strength = ab->tvstrength * bc->tvstrength;
*conf = ab->tvconf * bc->tvconf;
cogkernel.vmstate.inferences++;
}
static void
cogecanupdate(void)
{
KernelAtomSpace *as;
int i;
as = &cogkernel.atomspace;
qlock(as);
for(i = 0; i < as->natoms; i++) {
CogAtom *a = as->atoms[i];
if(a->sti > 0)
a->sti--;
}
cogkernel.vmstate.allocations++;
qunlock(as);
}
static void
cogvmcycle(void)
{
lock(&cogkernel.vmstate);
cogkernel.vmstate.cycles++;
unlock(&cogkernel.vmstate);
}
static void
coginit(void)
{
cogatomspaceinit();
cogkernel.vmstate.running = 1;
cogkernel.vmstate.cycles = 0;
cogkernel.vmstate.inferences = 0;
cogkernel.vmstate.allocations = 0;
cogkernel.maxcontexts = 1024;
cogkernel.contexts = malloc(cogkernel.maxcontexts * sizeof(CogContext*));
cogkernel.ncontexts = 0;
}
static Chan*
cogattach(char *spec)
{
return devattach('Σ', spec);
}
static Walkqid*
cogwalk(Chan *c, Chan *nc, char **name, int nname)
{
return devwalk(c, nc, name, nname, cogdir, nelem(cogdir), devgen);
}
static int
cogstat(Chan *c, uchar *db, int n)
{
return devstat(c, db, n, cogdir, nelem(cogdir), devgen);
}
static Chan*
cogopen(Chan *c, int omode)
{
return devopen(c, omode, cogdir, nelem(cogdir), devgen);
}
static void
cogclose(Chan*)
{
}
static long
cogread(Chan *c, void *va, long n, vlong off)
{
char *buf, *p, *e;
KernelAtomSpace *as;
int i;
switch((ulong)c->qid.path) {
case Qdir:
return devdirread(c, va, n, cogdir, nelem(cogdir), devgen);
case Qatomspace:
buf = malloc(8192);
if(buf == nil)
error(Enomem);
p = buf;
e = buf + 8192;
as = &cogkernel.atomspace;
qlock(as);
p = seprint(p, e, "atoms: %d/%d\n", as->natoms, as->maxatoms);
for(i = 0; i < as->natoms && i < 100; i++) {
CogAtom *a = as->atoms[i];
p = seprint(p, e, "atom %ld: type=%d name=%s tv=(%.2f,%.2f) av=(%d,%d)\n",
a->id, a->type, a->name, a->tvstrength, a->tvconf, a->sti, a->lti);
}
qunlock(as);
n = readstr(off, va, n, buf);
free(buf);
return n;
case Qcogvm:
buf = malloc(1024);
if(buf == nil)
error(Enomem);
lock(&cogkernel.vmstate);
seprint(buf, buf+1024, "running: %d\ncycles: %ld\ninferences: %ld\nallocations: %ld\n",
cogkernel.vmstate.running, cogkernel.vmstate.cycles,
cogkernel.vmstate.inferences, cogkernel.vmstate.allocations);
unlock(&cogkernel.vmstate);
n = readstr(off, va, n, buf);
free(buf);
return n;
case Qstats:
buf = malloc(2048);
if(buf == nil)
error(Enomem);
as = &cogkernel.atomspace;
qlock(as);
seprint(buf, buf+2048,
"Kernel Cognitive Statistics\n"
"===========================\n"
"AtomSpace:\n"
"  Total atoms: %d\n"
"  Max atoms: %d\n"
"  Memory: %ld KB\n"
"Cognitive VM:\n"
"  Cycles: %ld\n"
"  Inferences: %ld\n"
"  Attention updates: %ld\n"
"Contexts: %d\n",
as->natoms, as->maxatoms, (as->natoms * sizeof(CogAtom)) / 1024,
cogkernel.vmstate.cycles, cogkernel.vmstate.inferences,
cogkernel.vmstate.allocations, cogkernel.ncontexts);
qunlock(as);
n = readstr(off, va, n, buf);
free(buf);
return n;
}
error(Egreg);
return 0;
}
static long
cogwrite(Chan *c, void *va, long n, vlong)
{
char *cmd, *p;
Cmdbuf *cb;
CogAtom *atom, *ab, *bc;
float strength, conf;
int type;
switch((ulong)c->qid.path) {
case Qatomspace:
cb = parsecmd(va, n);
if(waserror()) {
free(cb);
nexterror();
}
cmd = cb->f[0];
if(strcmp(cmd, "create") == 0) {
if(cb->nf < 3)
error(Ebadcmd);
type = atoi(cb->f[1]);
atom = cogatomcreate(type, cb->f[2]);
cogvmcycle();
}
else if(strcmp(cmd, "settruth") == 0) {
if(cb->nf < 4)
error(Ebadcmd);
atom = cogatomfind(atoi(cb->f[1]));
if(atom == nil)
error("atom not found");
atom->tvstrength = atof(cb->f[2]);
atom->tvconf = atof(cb->f[3]);
}
else
error(Ebadcmd);
poperror();
free(cb);
break;
case Qpln:
cb = parsecmd(va, n);
if(waserror()) {
free(cb);
nexterror();
}
cmd = cb->f[0];
if(strcmp(cmd, "deduction") == 0) {
if(cb->nf < 3)
error(Ebadcmd);
ab = cogatomfind(atoi(cb->f[1]));
bc = cogatomfind(atoi(cb->f[2]));
if(ab == nil || bc == nil)
error("atoms not found");
cogplndeduction(ab, bc, &strength, &conf);
cogvmcycle();
}
else
error(Ebadcmd);
poperror();
free(cb);
break;
case Qecan:
cb = parsecmd(va, n);
if(waserror()) {
free(cb);
nexterror();
}
cmd = cb->f[0];
if(strcmp(cmd, "update") == 0) {
cogecanupdate();
cogvmcycle();
}
else if(strcmp(cmd, "allocate") == 0) {
if(cb->nf < 3)
error(Ebadcmd);
atom = cogatomfind(atoi(cb->f[1]));
if(atom == nil)
error("atom not found");
atom->sti += atoi(cb->f[2]);
cogkernel.vmstate.allocations++;
cogvmcycle();
}
else
error(Ebadcmd);
poperror();
free(cb);
break;
case Qctl:
cb = parsecmd(va, n);
if(waserror()) {
free(cb);
nexterror();
}
cmd = cb->f[0];
if(strcmp(cmd, "start") == 0) {
lock(&cogkernel.vmstate);
cogkernel.vmstate.running = 1;
unlock(&cogkernel.vmstate);
}
else if(strcmp(cmd, "stop") == 0) {
lock(&cogkernel.vmstate);
cogkernel.vmstate.running = 0;
unlock(&cogkernel.vmstate);
}
else if(strcmp(cmd, "reset") == 0) {
int i;
qlock(&cogkernel.atomspace);
for(i = 0; i < cogkernel.atomspace.natoms; i++) {
if(cogkernel.atomspace.atoms[i]) {
if(cogkernel.atomspace.atoms[i]->outgoing)
free(cogkernel.atomspace.atoms[i]->outgoing);
free(cogkernel.atomspace.atoms[i]);
cogkernel.atomspace.atoms[i] = nil;
}
}
cogkernel.atomspace.natoms = 0;
cogkernel.atomspace.nextid = 1;
qunlock(&cogkernel.atomspace);
lock(&cogkernel.vmstate);
cogkernel.vmstate.cycles = 0;
cogkernel.vmstate.inferences = 0;
cogkernel.vmstate.allocations = 0;
unlock(&cogkernel.vmstate);
}
else
error(Ebadcmd);
poperror();
free(cb);
break;
default:
error(Egreg);
}
return n;
}
Dev cogdevtab = {
'Σ',
"cog",
devreset,
coginit,
devshutdown,
cogattach,
cogwalk,
cogstat,
cogopen,
devcreate,
cogclose,
cogread,
devbread,
cogwrite,
devbwrite,
devremove,
devwstat,
};