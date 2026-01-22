#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
int chandebug=0;
#define DBG if(chandebug)iprint
enum
{
PATHSLOP	= 20,
PATHMSLOP	= 20,
};
struct
{
Lock;
int	fid;
Chan	*free;
Chan	*list;
}chanalloc;
typedef struct Elemlist Elemlist;
struct Elemlist
{
char	*aname;
char	*name;
int	nelems;
char	**elems;
int	*off;
int	mustbedir;
int	nerror;
int	prefix;
};
#define SEP(c) ((c) == 0 || (c) == '/')
static void
dumpmount(void)
{
Pgrp *pg;
Mount *t;
Mhead **h, **he, *f;
if(up == nil){
print("no process for dumpmount\n");
return;
}
pg = up->pgrp;
if(pg == nil){
print("no pgrp for dumpmount\n");
return;
}
rlock(&pg->ns);
if(waserror()){
runlock(&pg->ns);
nexterror();
}
he = &pg->mnthash[MNTHASH];
for(h = pg->mnthash; h < he; h++){
for(f = *h; f; f = f->hash){
print("head: %#p: %s %#llux.%lud %C %lud -> \n", f,
f->from->path->s, f->from->qid.path,
f->from->qid.vers, devtab[f->from->type]->dc,
f->from->dev);
for(t = f->mount; t; t = t->next)
print("\t%#p: %s (umh %#p) (path %#.8llux dev %C %lud)\n", t, t->to->path->s, t->to->umh, t->to->qid.path, devtab[t->to->type]->dc, t->to->dev);
}
}
poperror();
runlock(&pg->ns);
}
char*
chanpath(Chan *c)
{
if(c == nil)
return "<nil chan>";
if(c->path == nil)
return "<nil path>";
if(c->path->s == nil)
return "<nil path.s>";
return c->path->s;
}
int
isdotdot(char *p)
{
return p[0]=='.' && p[1]=='.' && p[2]=='\0';
}
long
incref(Ref *r)
{
long x;
lock(r);
x = ++r->ref;
unlock(r);
return x;
}
long
decref(Ref *r)
{
long x;
lock(r);
x = --r->ref;
unlock(r);
if(x < 0)
panic("decref pc=%#p", getcallerpc(&r));
return x;
}
void
kstrcpy(char *s, char *t, int ns)
{
int nt;
nt = strlen(t);
if(nt+1 <= ns){
memmove(s, t, nt+1);
return;
}
if(ns < 4){
strncpy(s, t, ns);
return;
}
memmove(s, t, ns-4);
ns -= 4;
s[ns] = '\0';
while(ns>0 && (s[--ns]&0xC0)==0x80)
;
strcpy(s+ns, "...");
}
int
emptystr(char *s)
{
if(s == nil)
return 1;
if(s[0] == '\0')
return 1;
return 0;
}
void
kstrdup(char **p, char *s)
{
int n;
char *t, *prev;
n = strlen(s)+1;
if(up){
t = smalloc(n);
setmalloctag(t, getcallerpc(&p));
}else{
t = malloc(n);
if(t == nil)
panic("kstrdup: no memory");
}
memmove(t, s, n);
prev = *p;
*p = t;
free(prev);
}
static int debugstart = 1;
void
chandevreset(void)
{
int i;
todinit();
debugstart = getconf("*debugstart") != nil;
if(debugstart)
iprint("reset:");
for(i=0; devtab[i] != nil; i++) {
if(debugstart)
iprint(" %s", devtab[i]->name);
devtab[i]->reset();
}
if(debugstart)
iprint("\n");
}
void
chandevinit(void)
{
int i;
if(debugstart)
iprint("init:");
for(i=0; devtab[i] != nil; i++) {
if(debugstart)
iprint(" %s", devtab[i]->name);
devtab[i]->init();
}
if(debugstart)
iprint("\n");
}
void
chandevshutdown(void)
{
int i;
for(i=0; devtab[i] != nil; i++)
;
for(i--; i >= 0; i--)
devtab[i]->shutdown();
}
Chan*
newchan(void)
{
Chan *c;
lock(&chanalloc);
c = chanalloc.free;
if(c != 0)
chanalloc.free = c->next;
unlock(&chanalloc);
if(c == nil){
c = smalloc(sizeof(Chan));
lock(&chanalloc);
c->fid = ++chanalloc.fid;
c->link = chanalloc.list;
chanalloc.list = c;
unlock(&chanalloc);
}
c->type = 0;
c->flag = 0;
c->ref = 1;
c->dev = 0;
c->offset = 0;
c->devoffset = 0;
c->iounit = 0;
c->umh = 0;
c->uri = 0;
c->dri = 0;
c->aux = 0;
c->mchan = 0;
c->mcp = 0;
c->mux = 0;
memset(&c->mqid, 0, sizeof(c->mqid));
c->path = 0;
c->ismtpt = 0;
return c;
}
Ref npath;
Path*
newpath(char *s)
{
int i;
Path *p;
p = smalloc(sizeof(Path));
i = strlen(s);
p->len = i;
p->alen = i+PATHSLOP;
p->s = smalloc(p->alen);
memmove(p->s, s, i+1);
p->ref = 1;
incref(&npath);
if(strchr(s, '/') && strcmp(s, "#/") != 0 && strcmp(s, "/") != 0)
print("newpath: %s from %#p\n", s, getcallerpc(&s));
p->mlen = 1;
p->malen = PATHMSLOP;
p->mtpt = smalloc(p->malen*sizeof p->mtpt[0]);
return p;
}
static Path*
copypath(Path *p)
{
int i;
Path *pp;
pp = smalloc(sizeof(Path));
pp->ref = 1;
incref(&npath);
DBG("copypath %s %p => %p\n", p->s, p, pp);
pp->len = p->len;
pp->alen = p->alen;
pp->s = smalloc(p->alen);
memmove(pp->s, p->s, p->len+1);
pp->mlen = p->mlen;
pp->malen = p->malen;
pp->mtpt = smalloc(p->malen*sizeof pp->mtpt[0]);
for(i=0; i<pp->mlen; i++){
pp->mtpt[i] = p->mtpt[i];
if(pp->mtpt[i])
incref(pp->mtpt[i]);
}
return pp;
}
void
pathclose(Path *p)
{
int i;
if(p == nil)
return;
DBG("pathclose %p %s ref=%ld =>", p, p->s, p->ref);
for(i=0; i<p->mlen; i++)
DBG(" %p", p->mtpt[i]);
DBG("\n");
if(decref(p))
return;
decref(&npath);
free(p->s);
for(i=0; i<p->mlen; i++)
if(p->mtpt[i])
cclose(p->mtpt[i]);
free(p->mtpt);
free(p);
}
static void
fixdotdotname(Path *p)
{
char *r;
if(p->s[0] == '#'){
r = strchr(p->s, '/');
if(r == nil)
return;
cleanname(r);
if(strcmp(r, "/")==0 && p->s[1] != '/')
*r = '\0';
}else
cleanname(p->s);
p->len = strlen(p->s);
}
static Path*
uniquepath(Path *p)
{
Path *new;
if(p->ref > 1){
new = copypath(p);
pathclose(p);
p = new;
}
return p;
}
static Path*
addelem(Path *p, char *s, Chan *from)
{
char *t;
int a, i;
Chan *c, **tt;
if(s[0]=='.' && s[1]=='\0')
return p;
p = uniquepath(p);
i = strlen(s);
if(p->len+1+i+1 > p->alen){
a = p->len+1+i+1 + PATHSLOP;
t = smalloc(a);
memmove(t, p->s, p->len+1);
free(p->s);
p->s = t;
p->alen = a;
}
if(p->len>0 && p->s[p->len-1]!='/' && s[0]!='/')
p->s[p->len++] = '/';
memmove(p->s+p->len, s, i+1);
p->len += i;
if(isdotdot(s)){
fixdotdotname(p);
DBG("addelem %s .. => rm %p\n", p->s, p->mtpt[p->mlen-1]);
if(p->mlen>1 && (c = p->mtpt[--p->mlen])){
p->mtpt[p->mlen] = nil;
cclose(c);
}
}else{
if(p->mlen >= p->malen){
p->malen = p->mlen+1+PATHMSLOP;
tt = smalloc(p->malen*sizeof tt[0]);
memmove(tt, p->mtpt, p->mlen*sizeof tt[0]);
free(p->mtpt);
p->mtpt = tt;
}
DBG("addelem %s %s => add %p\n", p->s, s, from);
p->mtpt[p->mlen++] = from;
if(from)
incref(from);
}
return p;
}
void
chanfree(Chan *c)
{
c->flag = CFREE;
if(c->dirrock != nil){
free(c->dirrock);
c->dirrock = 0;
c->nrock = 0;
c->mrock = 0;
}
if(c->umh != nil){
putmhead(c->umh);
c->umh = nil;
}
if(c->umc != nil){
cclose(c->umc);
c->umc = nil;
}
if(c->mux != nil){
muxclose(c->mux);
c->mux = nil;
}
if(c->mchan != nil){
cclose(c->mchan);
c->mchan = nil;
}
pathclose(c->path);
c->path = nil;
lock(&chanalloc);
c->next = chanalloc.free;
chanalloc.free = c;
unlock(&chanalloc);
}
void
cclose(Chan *c)
{
if(c->flag&CFREE)
panic("cclose %#p", getcallerpc(&c));
DBG("cclose %p name=%s ref=%ld\n", c, c->path->s, c->ref);
if(decref(c))
return;
if(!waserror()){
devtab[c->type]->close(c);
poperror();
}
chanfree(c);
}
struct {
Chan *head;
Chan *tail;
int nqueued;
int nclosed;
Lock l;
QLock q;
Rendez r;
} clunkq;
void closeproc(void*);
void
ccloseq(Chan *c)
{
if(c->flag&CFREE)
panic("cclose %#p", getcallerpc(&c));
DBG("ccloseq %p name=%s ref=%ld\n", c, c->path->s, c->ref);
if(decref(c))
return;
lock(&clunkq.l);
clunkq.nqueued++;
c->next = nil;
if(clunkq.head)
clunkq.tail->next = c;
else
clunkq.head = c;
clunkq.tail = c;
unlock(&clunkq.l);
if(!wakeup(&clunkq.r))
kproc("closeproc", closeproc, nil);
}
static int
clunkwork(void*)
{
return clunkq.head != nil;
}
void
closeproc(void*)
{
Chan *c;
for(;;){
qlock(&clunkq.q);
if(clunkq.head == nil){
if(!waserror()){
tsleep(&clunkq.r, clunkwork, nil, 5000);
poperror();
}
if(clunkq.head == nil){
qunlock(&clunkq.q);
pexit("no work", 1);
}
}
lock(&clunkq.l);
c = clunkq.head;
clunkq.head = c->next;
clunkq.nclosed++;
unlock(&clunkq.l);
qunlock(&clunkq.q);
if(!waserror()){
devtab[c->type]->close(c);
poperror();
}
chanfree(c);
}
}
Chan*
cunique(Chan *c)
{
Chan *nc;
if(c->ref != 1){
nc = cclone(c);
cclose(c);
c = nc;
}
return c;
}
int
eqqid(Qid a, Qid b)
{
return a.path==b.path && a.vers==b.vers;
}
int
eqchan(Chan *a, Chan *b, int skipvers)
{
if(a->qid.path != b->qid.path)
return 0;
if(!skipvers && a->qid.vers!=b->qid.vers)
return 0;
if(a->type != b->type)
return 0;
if(a->dev != b->dev)
return 0;
return 1;
}
int
eqchantdqid(Chan *a, int type, int dev, Qid qid, int skipvers)
{
if(a->qid.path != qid.path)
return 0;
if(!skipvers && a->qid.vers!=qid.vers)
return 0;
if(a->type != type)
return 0;
if(a->dev != dev)
return 0;
return 1;
}
Mhead*
newmhead(Chan *from)
{
Mhead *mh;
mh = smalloc(sizeof(Mhead));
mh->ref = 1;
mh->from = from;
incref(from);
return mh;
}
int
cmount(Chan **newp, Chan *old, int flag, char *spec)
{
int order, flg;
Chan *new;
Mhead *m, **l, *mh;
Mount *nm, *f, *um, **h;
Pgrp *pg;
if(QTDIR & (old->qid.type^(*newp)->qid.type))
error(Emount);
if(old->umh)
print("cmount: unexpected umh, caller %#p\n", getcallerpc(&newp));
order = flag&MORDER;
if((old->qid.type&QTDIR)==0 && order != MREPL)
error(Emount);
new = *newp;
mh = new->umh;
if((flag&MCREATE) && mh && mh->mount
&& (mh->mount->next || !(mh->mount->mflag&MCREATE)))
error(Emount);
pg = up->pgrp;
wlock(&pg->ns);
l = &MOUNTH(pg, old->qid);
for(m = *l; m; m = m->hash){
if(eqchan(m->from, old, 1))
break;
l = &m->hash;
}
if(m == nil){
m = newmhead(old);
*l = m;
if(order != MREPL)
m->mount = newmount(m, old, 0, 0);
}
wlock(&m->lock);
if(waserror()){
wunlock(&m->lock);
nexterror();
}
wunlock(&pg->ns);
nm = newmount(m, new, flag, spec);
if(mh != nil && mh->mount != nil){
flg = order;
if(order == MREPL)
flg = MAFTER;
h = &nm->next;
um = mh->mount;
for(um = um->next; um; um = um->next){
f = newmount(m, um->to, flg, um->spec);
*h = f;
h = &f->next;
}
}
if(m->mount && order == MREPL){
mountfree(m->mount);
m->mount = 0;
}
if(flag & MCREATE)
nm->mflag |= MCREATE;
if(m->mount && order == MAFTER){
for(f = m->mount; f->next; f = f->next)
;
f->next = nm;
}else{
for(f = nm; f->next; f = f->next)
;
f->next = m->mount;
m->mount = nm;
}
wunlock(&m->lock);
poperror();
return nm->mountid;
}
void
cunmount(Chan *mnt, Chan *mounted)
{
Pgrp *pg;
Mhead *m, **l;
Mount *f, **p;
if(mnt->umh)
print("cunmount newp extra umh %p has %p\n", mnt, mnt->umh);
pg = up->pgrp;
wlock(&pg->ns);
l = &MOUNTH(pg, mnt->qid);
for(m = *l; m; m = m->hash){
if(eqchan(m->from, mnt, 1))
break;
l = &m->hash;
}
if(m == 0){
wunlock(&pg->ns);
error(Eunmount);
}
wlock(&m->lock);
if(mounted == 0){
*l = m->hash;
wunlock(&pg->ns);
mountfree(m->mount);
m->mount = nil;
cclose(m->from);
wunlock(&m->lock);
putmhead(m);
return;
}
p = &m->mount;
for(f = *p; f; f = f->next){
if(eqchan(f->to, mounted, 1) ||
(f->to->mchan && eqchan(f->to->mchan, mounted, 1))){
*p = f->next;
f->next = 0;
mountfree(f);
if(m->mount == nil){
*l = m->hash;
cclose(m->from);
wunlock(&m->lock);
wunlock(&pg->ns);
putmhead(m);
return;
}
wunlock(&m->lock);
wunlock(&pg->ns);
return;
}
p = &f->next;
}
wunlock(&m->lock);
wunlock(&pg->ns);
error(Eunion);
}
Chan*
cclone(Chan *c)
{
Chan *nc;
Walkqid *wq;
wq = devtab[c->type]->walk(c, nil, nil, 0);
if(wq == nil)
error("clone failed");
nc = wq->clone;
free(wq);
nc->path = c->path;
if(c->path)
incref(c->path);
return nc;
}
int
findmount(Chan **cp, Mhead **mp, int type, int dev, Qid qid)
{
Pgrp *pg;
Mhead *m;
pg = up->pgrp;
rlock(&pg->ns);
for(m = MOUNTH(pg, qid); m; m = m->hash){
rlock(&m->lock);
if(m->from == nil){
print("m %p m->from 0\n", m);
runlock(&m->lock);
continue;
}
if(eqchantdqid(m->from, type, dev, qid, 1)){
runlock(&pg->ns);
if(mp != nil){
incref(m);
if(*mp != nil)
putmhead(*mp);
*mp = m;
}
if(*cp != nil)
cclose(*cp);
incref(m->mount->to);
*cp = m->mount->to;
runlock(&m->lock);
return 1;
}
runlock(&m->lock);
}
runlock(&pg->ns);
return 0;
}
static int
domount(Chan **cp, Mhead **mp, Path **path)
{
Chan **lc;
Path *p;
if(findmount(cp, mp, (*cp)->type, (*cp)->dev, (*cp)->qid) == 0)
return 0;
if(path){
p = *path;
p = uniquepath(p);
if(p->mlen <= 0)
print("domount: path %s has mlen==%d\n", p->s, p->mlen);
else{
lc = &p->mtpt[p->mlen-1];
DBG("domount %p %s => add %p (was %p)\n", p, p->s, (*mp)->from, p->mtpt[p->mlen-1]);
incref((*mp)->from);
if(*lc)
cclose(*lc);
*lc = (*mp)->from;
}
*path = p;
}
return 1;
}
static Chan*
undomount(Chan *c, Path *path)
{
Chan *nc;
if(path->ref != 1 || path->mlen == 0)
print("undomount: path %s ref %ld mlen %d caller %#p\n",
path->s, path->ref, path->mlen, getcallerpc(&c));
if(path->mlen>0 && (nc=path->mtpt[path->mlen-1]) != nil){
DBG("undomount %p %s => remove %p\n", path, path->s, nc);
cclose(c);
path->mtpt[path->mlen-1] = nil;
c = nc;
}
return c;
}
static Walkqid*
ewalk(Chan *c, Chan *nc, char **name, int nname)
{
Walkqid *wq;
if(waserror())
return nil;
wq = devtab[c->type]->walk(c, nc, name, nname);
poperror();
return wq;
}
static char Edoesnotexist[] = "does not exist";
int
walk(Chan **cp, char **names, int nnames, int nomount, int *nerror)
{
int dev, didmount, dotdot, i, n, nhave, ntry, type;
Chan *c, *nc, *mtpt;
Path *path;
Mhead *mh, *nmh;
Mount *f;
Walkqid *wq;
c = *cp;
incref(c);
path = c->path;
incref(path);
mh = nil;
didmount = 0;
for(nhave=0; nhave<nnames; nhave+=n){
if((c->qid.type&QTDIR)==0){
if(nerror)
*nerror = nhave;
pathclose(path);
cclose(c);
strcpy(up->errstr, Enotdir);
if(mh != nil)
putmhead(mh);
return -1;
}
ntry = nnames - nhave;
if(ntry > MAXWELEM)
ntry = MAXWELEM;
dotdot = 0;
for(i=0; i<ntry; i++){
if(isdotdot(names[nhave+i])){
if(i==0){
dotdot = 1;
ntry = 1;
}else
ntry = i;
break;
}
}
if(!dotdot && !nomount && !didmount)
domount(&c, &mh, &path);
type = c->type;
dev = c->dev;
if((wq = ewalk(c, nil, names+nhave, ntry)) == nil){
if(mh && !nomount){
rlock(&mh->lock);
f = mh->mount;
for(f = (f? f->next: f); f; f = f->next)
if((wq = ewalk(f->to, nil, names+nhave, ntry)) != nil)
break;
runlock(&mh->lock);
if(f != nil){
type = f->to->type;
dev = f->to->dev;
}
}
if(wq == nil){
cclose(c);
pathclose(path);
if(nerror)
*nerror = nhave+1;
if(mh != nil)
putmhead(mh);
return -1;
}
}
didmount = 0;
if(dotdot){
assert(wq->nqid == 1);
assert(wq->clone != nil);
path = addelem(path, "..", nil);
nc = undomount(wq->clone, path);
nmh = nil;
n = 1;
}else{
nc = nil;
nmh = nil;
if(!nomount){
for(i=0; i<wq->nqid && i<ntry-1; i++){
if(findmount(&nc, &nmh, type, dev, wq->qid[i])){
didmount = 1;
break;
}
}
}
if(nc == nil){
if(wq->clone == nil){
cclose(c);
pathclose(path);
if(wq->nqid==0 || (wq->qid[wq->nqid-1].type&QTDIR)){
if(nerror)
*nerror = nhave+wq->nqid+1;
strcpy(up->errstr, Edoesnotexist);
}else{
if(nerror)
*nerror = nhave+wq->nqid;
strcpy(up->errstr, Enotdir);
}
free(wq);
if(mh != nil)
putmhead(mh);
return -1;
}
n = wq->nqid;
nc = wq->clone;
}else{
didmount = 1;
if(wq->clone != nil){
cclose(wq->clone);
wq->clone = nil;
}
n = i+1;
}
for(i=0; i<n; i++){
mtpt = nil;
if(i==n-1 && nmh)
mtpt = nmh->from;
path = addelem(path, names[nhave+i], mtpt);
}
}
cclose(c);
c = nc;
putmhead(mh);
mh = nmh;
free(wq);
}
putmhead(mh);
c = cunique(c);
if(c->umh != nil){
print("walk umh\n");
putmhead(c->umh);
c->umh = nil;
}
pathclose(c->path);
c->path = path;
cclose(*cp);
*cp = c;
if(nerror)
*nerror = nhave;
return 0;
}
Chan*
createdir(Chan *c, Mhead *m)
{
Chan *nc;
Mount *f;
rlock(&m->lock);
if(waserror()){
runlock(&m->lock);
nexterror();
}
for(f = m->mount; f; f = f->next){
if(f->mflag&MCREATE){
nc = cclone(f->to);
runlock(&m->lock);
poperror();
cclose(c);
return nc;
}
}
error(Enocreate);
return 0;
}
void
saveregisters(void)
{
}
static void
growparse(Elemlist *e)
{
char **new;
int *inew;
enum { Delta = 8 };
if(e->nelems % Delta == 0){
new = smalloc((e->nelems+Delta) * sizeof(char*));
memmove(new, e->elems, e->nelems*sizeof(char*));
free(e->elems);
e->elems = new;
inew = smalloc((e->nelems+Delta+1) * sizeof(int));
memmove(inew, e->off, (e->nelems+1)*sizeof(int));
free(e->off);
e->off = inew;
}
}
static void
parsename(char *aname, Elemlist *e)
{
char *name, *slash;
kstrdup(&e->name, aname);
name = e->name;
e->nelems = 0;
e->elems = nil;
e->off = smalloc(sizeof(int));
e->off[0] = skipslash(name) - name;
for(;;){
name = skipslash(name);
if(*name == '\0'){
e->off[e->nelems] = name+strlen(name) - e->name;
e->mustbedir = 1;
break;
}
growparse(e);
e->elems[e->nelems++] = name;
slash = utfrune(name, '/');
if(slash == nil){
e->off[e->nelems] = name+strlen(name) - e->name;
e->mustbedir = 0;
break;
}
e->off[e->nelems] = slash - e->name;
*slash++ = '\0';
name = slash;
}
if(0 && chandebug){
int i;
print("parsename %s:", e->name);
for(i=0; i<=e->nelems; i++)
print(" %d", e->off[i]);
print("\n");
}
}
void*
memrchr(void *va, int c, long n)
{
uchar *a, *e;
a = va;
for(e=a+n-1; e>a; e--)
if(*e == c)
return e;
return nil;
}
void
namelenerror(char *aname, int len, char *err)
{
char *ename, *name, *next;
int i, errlen;
errlen = strlen(err);
if(len < ERRMAX/3 || len+errlen < 2*ERRMAX/3)
snprint(up->genbuf, sizeof up->genbuf, "%.*s",
utfnlen(aname, len), aname);
else{
ename = aname+len;
next = ename;
do{
name = next;
next = memrchr(aname, '/', name-aname);
if(next == nil)
next = aname;
len = ename-next;
}while(len < ERRMAX/3 || len + errlen < 2*ERRMAX/3);
if(name == ename){
name = ename-ERRMAX/4;
if(name <= aname)
panic("bad math in namelenerror");
for(i=0; (*name&0xC0)==0x80 && i<UTFmax; i++)
name++;
}
snprint(up->genbuf, sizeof up->genbuf, "...%.*s",
utfnlen(name, ename-name), name);
}
snprint(up->errstr, ERRMAX, "%#q %s", up->genbuf, err);
nexterror();
}
void
nameerror(char *name, char *err)
{
namelenerror(name, strlen(name), err);
}
Chan*
namec(char *aname, int amode, int omode, ulong perm)
{
int len, n, t, nomount;
Chan *c, *cnew;
Path *path;
Elemlist e;
Rune r;
Mhead *m;
char *createerr, tmperrbuf[ERRMAX];
char *name;
if(aname[0] == '\0')
error("empty file name");
aname = validnamedup(aname, 1);
if(waserror()){
free(aname);
nexterror();
}
DBG("namec %s %d %d\n", aname, amode, omode);
name = aname;
nomount = 0;
switch(name[0]){
case '/':
c = up->slash;
incref(c);
break;
case '#':
nomount = 1;
up->genbuf[0] = '\0';
n = 0;
while(*name != '\0' && (*name != '/' || n < 2)){
if(n >= sizeof(up->genbuf)-1)
error(Efilename);
up->genbuf[n++] = *name++;
}
up->genbuf[n] = '\0';
n = chartorune(&r, up->genbuf+1)+1;
if(utfrune("M", r))
error(Enoattach);
if(up->pgrp->noattach && utfrune("|decp", r)==nil)
error(Enoattach);
t = devno(r, 1);
if(t == -1)
error(Ebadsharp);
if(debugstart && !devtab[t]->attached)
print("#%C...", devtab[t]->dc);
c = devtab[t]->attach(up->genbuf+n);
if(debugstart && c != nil)
devtab[t]->attached = 1;
break;
default:
c = up->dot;
incref(c);
break;
}
e.aname = aname;
e.prefix = name - aname;
e.name = nil;
e.elems = nil;
e.off = nil;
e.nelems = 0;
e.nerror = 0;
if(waserror()){
cclose(c);
free(e.name);
free(e.elems);
if(e.nerror == 0)
nexterror();
strcpy(tmperrbuf, up->errstr);
if(e.off[e.nerror]==0)
print("nerror=%d but off=%d\n",
e.nerror, e.off[e.nerror]);
if(0 && chandebug)
print("showing %d+%d/%d (of %d) of %s (%d %d)\n", e.prefix, e.off[e.nerror], e.nerror, e.nelems, aname, e.off[0], e.off[1]);
len = e.prefix+e.off[e.nerror];
free(e.off);
namelenerror(aname, len, tmperrbuf);
}
parsename(name, &e);
if(amode == Acreate){
if(e.mustbedir && !(perm&DMDIR)){
e.nerror = e.nelems;
error("create without DMDIR");
}
if(e.nelems == 0)
error(Eexist);
e.nelems--;
}
if(walk(&c, e.elems, e.nelems, nomount, &e.nerror) < 0){
if(e.nerror < 0 || e.nerror > e.nelems){
print("namec %s walk error nerror=%d\n", aname, e.nerror);
e.nerror = 0;
}
nexterror();
}
if(e.mustbedir && !(c->qid.type&QTDIR))
error("not a directory");
if(amode == Aopen && (omode&3) == OEXEC && (c->qid.type&QTDIR))
error("cannot exec directory");
switch(amode){
case Abind:
m = nil;
if(!nomount)
domount(&c, &m, nil);
if(c->umh != nil)
putmhead(c->umh);
c->umh = m;
break;
case Aaccess:
case Aremove:
case Aopen:
Open:
path = c->path;
incref(path);
m = nil;
if(!nomount)
domount(&c, &m, &path);
c = cunique(c);
pathclose(c->path);
c->path = path;
c->ismtpt = m!=nil;
switch(amode){
case Aaccess:
case Aremove:
putmhead(m);
break;
case Aopen:
case Acreate:
if(c->umh != nil){
print("cunique umh Open\n");
putmhead(c->umh);
c->umh = nil;
}
if(m && m->mount && m->mount->next)
c->umh = m;
else
putmhead(m);
saveregisters();
if(omode == OEXEC)
c->flag &= ~CCACHE;
c = devtab[c->type]->open(c, omode&~OCEXEC);
if(omode & OCEXEC)
c->flag |= CCEXEC;
if(omode & ORCLOSE)
c->flag |= CRCLOSE;
break;
}
break;
case Atodir:
if(!(c->qid.type & QTDIR))
error(Enotdir);
break;
case Amount:
break;
case Acreate:
e.nelems++;
e.nerror++;
if(walk(&c, e.elems+e.nelems-1, 1, nomount, nil) == 0){
if(omode&OEXCL)
error(Eexist);
omode |= OTRUNC;
goto Open;
}
m = nil;
cnew = nil;
if(!waserror()){
if(!nomount && findmount(&cnew, &m, c->type, c->dev, c->qid))
cnew = createdir(cnew, m);
else{
cnew = c;
incref(cnew);
}
cnew = cunique(cnew);
pathclose(cnew->path);
cnew->path = c->path;
incref(cnew->path);
devtab[cnew->type]->create(cnew, e.elems[e.nelems-1], omode&~(OEXCL|OCEXEC), perm);
poperror();
if(omode & OCEXEC)
cnew->flag |= CCEXEC;
if(omode & ORCLOSE)
cnew->flag |= CRCLOSE;
if(m)
putmhead(m);
cclose(c);
c = cnew;
c->path = addelem(c->path, e.elems[e.nelems-1], nil);
break;
}
cclose(cnew);
if(m)
putmhead(m);
if(omode & OEXCL)
nexterror();
createerr = up->errstr;
up->errstr = tmperrbuf;
if(walk(&c, e.elems+e.nelems-1, 1, nomount, nil) < 0){
up->errstr = createerr;
error(createerr);
}
up->errstr = createerr;
omode |= OTRUNC;
goto Open;
default:
panic("unknown namec access %d\n", amode);
}
if(e.nelems > 0)
kstrcpy(up->genbuf, e.elems[e.nelems-1], sizeof up->genbuf);
else
kstrcpy(up->genbuf, ".", sizeof up->genbuf);
free(e.name);
free(e.elems);
free(e.off);
poperror();
free(aname);
poperror();
return c;
}
char*
skipslash(char *name)
{
while(name[0]=='/' || (name[0]=='.' && (name[1]==0 || name[1]=='/')))
name++;
return name;
}
char isfrog[256]={
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
['/']	1,
[0x7f]	1,
};
static char*
validname0(char *aname, int slashok, int dup, ulong pc)
{
char *ename, *name, *s;
int c, n;
Rune r;
name = aname;
if((ulong)name < KZERO){
if(!dup)
print("warning: validname called from %#p with user pointer", pc);
ename = vmemchr(name, 0, (1<<16));
}else
ename = memchr(name, 0, (1<<16));
if(ename==nil || ename-name>=(1<<16))
error("name too long");
s = nil;
if(dup){
n = ename-name;
s = smalloc(n+1);
memmove(s, name, n);
s[n] = 0;
aname = s;
name = s;
setmalloctag(s, pc);
}
while(*name){
c = *(uchar*)name;
if(c >= Runeself)
name += chartorune(&r, name);
else{
if(isfrog[c])
if(!slashok || c!='/'){
snprint(up->genbuf, sizeof(up->genbuf), "%s: %q", Ebadchar, aname);
free(s);
error(up->genbuf);
}
name++;
}
}
return s;
}
void
validname(char *aname, int slashok)
{
validname0(aname, slashok, 0, getcallerpc(&aname));
}
char*
validnamedup(char *aname, int slashok)
{
return validname0(aname, slashok, 1, getcallerpc(&aname));
}
void
isdir(Chan *c)
{
if(c->qid.type & QTDIR)
return;
error(Enotdir);
}
void
putmhead(Mhead *m)
{
if(m && decref(m) == 0){
m->mount = (Mount*)0xCafeBeef;
free(m);
}
}