#include	"dat.h"
#include	"fns.h"
#include	"error.h"
char*
c2name(Chan *c)
{
if(c == nil)
return "<nil chan>";
if(c->name == nil)
return "<nil name>";
if(c->name->s == nil)
return "<nil name.s>";
return c->name->s;
}
enum
{
CNAMESLOP	= 20
};
struct
{
Lock	l;
int	fid;
Chan	*free;
Chan	*list;
}chanalloc;
typedef struct Elemlist Elemlist;
struct Elemlist
{
char	*name;
int	nelems;
char	**elems;
int	*off;
int	mustbedir;
};
#define SEP(c) ((c) == 0 || (c) == '/')
void cleancname(Cname*);
int
isdotdot(char *p)
{
return p[0]=='.' && p[1]=='.' && p[2]=='\0';
}
int
incref(Ref *r)
{
int x;
lock(&r->lk);
x = ++r->ref;
unlock(&r->lk);
return x;
}
int
decref(Ref *r)
{
int x;
lock(&r->lk);
x = --r->ref;
unlock(&r->lk);
if(x < 0)
panic("decref, pc=0x%lux", getcallerpc(&r));
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
return s == nil || s[0] == '\0';
}
void
kstrdup(char **p, char *s)
{
int n;
char *t, *prev;
n = strlen(s)+1;
t = kmalloc(n);
if(t == nil)
panic("kstrdup: no memory");
memmove(t, s, n);
prev = *p;
*p = t;
free(prev);
}
static char isfrog[256]=
{
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1
};
void
chandevinit(void)
{
int i;
isfrog['/'] = 1;
isfrog[0x7f] = 1;
for(i=0; devtab[i] != nil; i++)
devtab[i]->init();
}
Chan*
newchan(void)
{
Chan *c;
lock(&chanalloc.l);
c = chanalloc.free;
if(c != 0)
chanalloc.free = c->next;
unlock(&chanalloc.l);
if(c == nil) {
c = malloc(sizeof(Chan));
if(c == nil)
error(Enomem);
lock(&chanalloc.l);
c->fid = ++chanalloc.fid;
c->link = chanalloc.list;
chanalloc.list = c;
unlock(&chanalloc.l);
}
c->type = 0;
c->flag = 0;
c->r.ref = 1;
c->dev = 0;
c->offset = 0;
c->iounit = 0;
c->umh = 0;
c->uri = 0;
c->dri = 0;
c->aux = 0;
c->mchan = 0;
c->mcp = 0;
c->mux = 0;
c->mqid.path = 0;
c->mqid.vers = 0;
c->mqid.type = 0;
c->name = 0;
return c;
}
static Ref ncname;
Cname*
newcname(char *s)
{
Cname *n;
int i;
n = smalloc(sizeof(Cname));
i = strlen(s);
n->len = i;
n->alen = i+CNAMESLOP;
n->s = smalloc(n->alen);
memmove(n->s, s, i+1);
n->r.ref = 1;
incref(&ncname);
return n;
}
void
cnameclose(Cname *n)
{
if(n == nil)
return;
if(decref(&n->r))
return;
decref(&ncname);
free(n->s);
free(n);
}
Cname*
addelem(Cname *n, char *s)
{
int i, a;
char *t;
Cname *new;
if(s[0]=='.' && s[1]=='\0')
return n;
if(n->r.ref > 1){
new = newcname(n->s);
cnameclose(n);
n = new;
}
i = strlen(s);
if(n->len+1+i+1 > n->alen){
a = n->len+1+i+1 + CNAMESLOP;
t = smalloc(a);
memmove(t, n->s, n->len+1);
free(n->s);
n->s = t;
n->alen = a;
}
if(n->len>0 && n->s[n->len-1]!='/' && s[0]!='/')
n->s[n->len++] = '/';
memmove(n->s+n->len, s, i+1);
n->len += i;
if(isdotdot(s))
cleancname(n);
return n;
}
void
chanfree(Chan *c)
{
c->flag = CFREE;
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
cnameclose(c->name);
lock(&chanalloc.l);
c->next = chanalloc.free;
chanalloc.free = c;
unlock(&chanalloc.l);
}
void
cclose(Chan *c)
{
if(c == 0)
return;
if(c->flag&CFREE)
panic("cclose %lux", getcallerpc(&c));
if(decref(&c->r))
return;
if(!waserror()){
devtab[c->type]->close(c);
poperror();
}
chanfree(c);
}
Chan*
cunique(Chan *c)
{
Chan *nc;
if(c->r.ref != 1) {
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
eqchan(Chan *a, Chan *b, int pathonly)
{
if(a->qid.path != b->qid.path)
return 0;
if(!pathonly && a->qid.vers!=b->qid.vers)
return 0;
if(a->type != b->type)
return 0;
if(a->dev != b->dev)
return 0;
return 1;
}
int
eqchantdqid(Chan *a, int type, int dev, Qid qid, int pathonly)
{
if(a->qid.path != qid.path)
return 0;
if(!pathonly && a->qid.vers!=qid.vers)
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
mh->r.ref = 1;
mh->from = from;
incref(&from->r);
return mh;
}
int
cmount(Chan *new, Chan *old, int flag, char *spec)
{
Pgrp *pg;
int order, flg;
Mhead *m, **l, *mh;
Mount *nm, *f, *um, **h;
if(QTDIR & (old->qid.type^new->qid.type))
error(Emount);
if(old->umh)
print("cmount old extra umh\n");
order = flag&MORDER;
if((old->qid.type&QTDIR)==0 && order != MREPL)
error(Emount);
mh = new->umh;
if((flag&MCREATE) && mh && mh->mount
&& (mh->mount->next || !(mh->mount->mflag&MCREATE)))
error(Emount);
pg = up->env->pgrp;
wlock(&pg->ns);
l = &MOUNTH(pg, old->qid);
for(m = *l; m; m = m->hash) {
if(eqchan(m->from, old, 1))
break;
l = &m->hash;
}
if(m == nil) {
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
if(mh != nil && mh->mount != nil) {
flg = order;
if(order == MREPL)
flg = MAFTER;
h = &nm->next;
um = mh->mount;
for(um = um->next; um; um = um->next) {
f = newmount(m, um->to, flg, um->spec);
*h = f;
h = &f->next;
}
}
if(m->mount && order == MREPL) {
mountfree(m->mount);
m->mount = 0;
}
if(flag & MCREATE)
nm->mflag |= MCREATE;
if(m->mount && order == MAFTER) {
for(f = m->mount; f->next; f = f->next)
;
f->next = nm;
}
else {
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
pg = up->env->pgrp;
wlock(&pg->ns);
l = &MOUNTH(pg, mnt->qid);
for(m = *l; m; m = m->hash) {
if(eqchan(m->from, mnt, 1))
break;
l = &m->hash;
}
if(m == 0) {
wunlock(&pg->ns);
error(Eunmount);
}
wlock(&m->lock);
if(mounted == 0) {
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
for(f = *p; f; f = f->next) {
if(eqchan(f->to, mounted, 1) ||
(f->to->mchan && eqchan(f->to->mchan, mounted, 1))) {
*p = f->next;
f->next = 0;
mountfree(f);
if(m->mount == nil) {
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
nc->name = c->name;
if(c->name)
incref(&c->name->r);
return nc;
}
int
findmount(Chan **cp, Mhead **mp, int type, int dev, Qid qid)
{
Pgrp *pg;
Mhead *m;
pg = up->env->pgrp;
rlock(&pg->ns);
for(m = MOUNTH(pg, qid); m; m = m->hash){
rlock(&m->lock);
if(m->from == nil){
print("m %p m->from 0\n", m);
runlock(&m->lock);
continue;
}
if(eqchantdqid(m->from, type, dev, qid, 1)) {
runlock(&pg->ns);
if(mp != nil){
incref(&m->r);
if(*mp != nil)
putmhead(*mp);
*mp = m;
}
if(*cp != nil)
cclose(*cp);
incref(&m->mount->to->r);
*cp = m->mount->to;
runlock(&m->lock);
return 1;
}
runlock(&m->lock);
}
runlock(&pg->ns);
return 0;
}
int
domount(Chan **cp, Mhead **mp)
{
return findmount(cp, mp, (*cp)->type, (*cp)->dev, (*cp)->qid);
}
Chan*
undomount(Chan *c, Cname *name)
{
Chan *nc;
Pgrp *pg;
Mount *t;
Mhead **h, **he, *f;
pg = up->env->pgrp;
rlock(&pg->ns);
if(waserror()) {
runlock(&pg->ns);
nexterror();
}
he = &pg->mnthash[MNTHASH];
for(h = pg->mnthash; h < he; h++) {
for(f = *h; f; f = f->hash) {
if(strcmp(f->from->name->s, name->s) != 0)
continue;
for(t = f->mount; t; t = t->next) {
if(eqchan(c, t->to, 1)) {
if(strcmp(t->head->from->name->s, name->s) != 0)
continue;
nc = t->head->from;
incref(&nc->r);
cclose(c);
c = nc;
break;
}
}
}
}
poperror();
runlock(&pg->ns);
return c;
}
static char Edoesnotexist[] = "does not exist";
int
walk(Chan **cp, char **names, int nnames, int nomount, int *nerror)
{
int dev, dotdot, i, n, nhave, ntry, type;
Chan *c, *nc;
Cname *cname;
Mount *f;
Mhead *mh, *nmh;
Walkqid *wq;
c = *cp;
incref(&c->r);
cname = c->name;
incref(&cname->r);
mh = nil;
for(nhave=0; nhave<nnames; nhave+=n){
if((c->qid.type&QTDIR)==0){
if(nerror)
*nerror = nhave;
cnameclose(cname);
cclose(c);
strcpy(up->env->errstr, Enotdir);
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
if(i==0) {
dotdot = 1;
ntry = 1;
} else
ntry = i;
break;
}
}
if(!dotdot && !nomount)
domount(&c, &mh);
type = c->type;
dev = c->dev;
if((wq = devtab[type]->walk(c, nil, names+nhave, ntry)) == nil){
if(mh && !nomount){
rlock(&mh->lock);
for(f = mh->mount->next; f; f = f->next)
if((wq = devtab[f->to->type]->walk(f->to, nil, names+nhave, ntry)) != nil)
break;
runlock(&mh->lock);
if(f != nil){
type = f->to->type;
dev = f->to->dev;
}
}
if(wq == nil){
cclose(c);
cnameclose(cname);
if(nerror)
*nerror = nhave+1;
if(mh != nil)
putmhead(mh);
return -1;
}
}
nmh = nil;
if(dotdot) {
assert(wq->nqid == 1);
assert(wq->clone != nil);
cname = addelem(cname, "..");
nc = undomount(wq->clone, cname);
n = 1;
} else {
nc = nil;
if(!nomount)
for(i=0; i<wq->nqid && i<ntry-1; i++)
if(findmount(&nc, &nmh, type, dev, wq->qid[i]))
break;
if(nc == nil){
if(wq->clone == nil){
cclose(c);
cnameclose(cname);
if(wq->nqid==0 || (wq->qid[wq->nqid-1].type&QTDIR)){
if(nerror)
*nerror = nhave+wq->nqid+1;
strcpy(up->env->errstr, Edoesnotexist);
}else{
if(nerror)
*nerror = nhave+wq->nqid;
strcpy(up->env->errstr, Enotdir);
}
free(wq);
if(mh != nil)
putmhead(mh);
return -1;
}
n = wq->nqid;
nc = wq->clone;
}else{
if(wq->clone != nil){
cclose(wq->clone);
wq->clone = nil;
}
n = i+1;
}
for(i=0; i<n; i++)
cname = addelem(cname, names[nhave+i]);
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
cnameclose(c->name);
c->name = cname;
cclose(*cp);
*cp = c;
if(nerror)
*nerror = 0;
return 0;
}
Chan*
createdir(Chan *c, Mhead *m)
{
Chan *nc;
Mount *f;
rlock(&m->lock);
if(waserror()) {
runlock(&m->lock);
nexterror();
}
for(f = m->mount; f; f = f->next) {
if(f->mflag&MCREATE) {
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
cleancname(Cname *n)
{
char *p;
if(n->s[0] == '#'){
p = strchr(n->s, '/');
if(p == nil)
return;
cleanname(p);
if(strcmp(p, "/")==0 && n->s[1] != '/')
*p = '\0';
}else
cleanname(n->s);
n->len = strlen(n->s);
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
memmove(inew, e->off, e->nelems*sizeof(int));
free(e->off);
e->off = inew;
}
}
static void
parsename(char *name, Elemlist *e)
{
char *slash;
kstrdup(&e->name, name);
name = e->name;
e->nelems = 0;
e->elems = nil;
e->off = smalloc(sizeof(int));
e->off[0] = skipslash(name) - name;
for(;;){
name = skipslash(name);
if(*name=='\0'){
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
}
static void*
kmemrchr(void *va, int c, long n)
{
uchar *a, *e;
a = va;
for(e=a+n-1; e>a; e--)
if(*e == c)
return e;
return nil;
}
static void
saveregisters(void)
{
}
Chan*
namec(char *aname, int amode, int omode, ulong perm)
{
int n, prefix, len, t, nomount, npath;
Chan *c, *cnew;
Cname *cname;
Elemlist e;
Rune r;
Mhead *m;
char *createerr, tmperrbuf[ERRMAX];
char *name;
name = aname;
if(name[0] == '\0')
error("empty file name");
validname(name, 1);
nomount = 0;
switch(name[0]){
case '/':
c = up->env->pgrp->slash;
incref(&c->r);
break;
case '#':
nomount = 1;
up->genbuf[0] = '\0';
n = 0;
while(*name!='\0' && (*name != '/' || n < 2)){
if(n >= sizeof(up->genbuf)-1)
error(Efilename);
up->genbuf[n++] = *name++;
}
up->genbuf[n] = '\0';
n = chartorune(&r, up->genbuf+1)+1;
if(r == 'M')
error(Enoattach);
if(up->env->pgrp->nodevs &&
(utfrune("|esDa", r) == nil || r == 's' && up->genbuf[n]!='\0'))
error(Enoattach);
t = devno(r, 1);
if(t == -1)
error(Ebadsharp);
c = devtab[t]->attach(up->genbuf+n);
break;
default:
c = up->env->pgrp->dot;
incref(&c->r);
break;
}
prefix = name - aname;
e.name = nil;
e.elems = nil;
e.off = nil;
e.nelems = 0;
if(waserror()){
cclose(c);
free(e.name);
free(e.elems);
free(e.off);
nexterror();
}
parsename(name, &e);
if(amode == Acreate){
if(e.mustbedir && !(perm&DMDIR)){
npath = e.nelems;
strcpy(tmperrbuf, "create without DMDIR");
goto NameError;
}
if(e.nelems == 0)
error(Eexist);
e.nelems--;
}
if(walk(&c, e.elems, e.nelems, nomount, &npath) < 0){
if(npath < 0 || npath > e.nelems){
print("namec %s walk error npath=%d\n", aname, npath);
nexterror();
}
strcpy(tmperrbuf, up->env->errstr);
NameError:
len = prefix+e.off[npath];
if(len < ERRMAX/3 || (name=kmemrchr(aname, '/', len))==nil || name==aname)
snprint(up->genbuf, sizeof up->genbuf, "%.*s", len, aname);
else
snprint(up->genbuf, sizeof up->genbuf, "...%.*s", (int)(len-(name-aname)), name);
snprint(up->env->errstr, ERRMAX, "%#q %s", up->genbuf, tmperrbuf);
nexterror();
}
if(e.mustbedir && !(c->qid.type&QTDIR)){
npath = e.nelems;
strcpy(tmperrbuf, "not a directory");
goto NameError;
}
if(amode == Aopen && (omode&3) == OEXEC && (c->qid.type&QTDIR)){
npath = e.nelems;
error("cannot exec directory");
}
switch(amode){
case Aaccess:
if(!nomount)
domount(&c, nil);
break;
case Abind:
m = nil;
if(!nomount)
domount(&c, &m);
if(c->umh != nil)
putmhead(c->umh);
c->umh = m;
break;
case Aremove:
case Aopen:
Open:
cname = c->name;
incref(&cname->r);
m = nil;
if(!nomount)
domount(&c, &m);
c = cunique(c);
cnameclose(c->name);
c->name = cname;
switch(amode){
case Aremove:
putmhead(m);
break;
case Aopen:
case Acreate:
if(c->umh != nil){
print("cunique umh\n");
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
incref(&cnew->r);
}
cnew = cunique(cnew);
cnameclose(cnew->name);
cnew->name = c->name;
incref(&cnew->name->r);
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
c->name = addelem(c->name, e.elems[e.nelems-1]);
break;
}else{
cclose(cnew);
if(m)
putmhead(m);
if(omode & OEXCL)
nexterror();
createerr = up->env->errstr;
up->env->errstr = tmperrbuf;
if(walk(&c, e.elems+e.nelems-1, 1, nomount, nil) < 0){
up->env->errstr = createerr;
error(createerr);
}
up->env->errstr = createerr;
omode |= OTRUNC;
goto Open;
}
default:
panic("unknown namec access %d\n", amode);
}
poperror();
if(e.nelems > 0)
kstrcpy(up->genbuf, e.elems[e.nelems-1], sizeof up->genbuf);
else
kstrcpy(up->genbuf, ".", sizeof up->genbuf);
free(e.name);
free(e.elems);
free(e.off);
return c;
}
char*
skipslash(char *name)
{
while(name[0]=='/' || (name[0]=='.' && (name[1]==0 || name[1]=='/')))
name++;
return name;
}
void
validname(char *aname, int slashok)
{
char *ename, *name;
int c;
Rune r;
name = aname;
ename = memchr(name, 0, (1<<16));
if(ename==nil || ename-name>=(1<<16))
error("name too long");
while(*name){
c = *(uchar*)name;
if(c >= Runeself)
name += chartorune(&r, name);
else{
if(isfrog[c])
if(!slashok || c!='/'){
snprint(up->genbuf, sizeof(up->genbuf), "%s: %q", Ebadchar, aname);
error(up->genbuf);
}
name++;
}
}
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
if(m && decref(&m->r) == 0){
m->mount = (Mount*)0xCafeBeef;
free(m);
}
}