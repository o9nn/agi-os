#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "../port/error.h"
enum {
Fmirror,
Fcat,
Finter,
Fpart,
Blksize = 8*1024,
Maxconf = 1024,
Nfsdevs = 64,
Ndevs = 8,
Qtop = 0,
Qdir = 1,
Qctl = 2,
Qfirst = 3,
};
#define Cfgstr "fsdev:\n"
typedef struct Fsdev Fsdev;
struct Fsdev
{
int type;
char *name;
vlong start;
vlong size;
int ndevs;
char *iname[Ndevs];
Chan *idev[Ndevs];
vlong isize[Ndevs];
};
static Fsdev fsdev[Nfsdevs];
static Qid tqid = {Qtop, 0, QTDIR};
static Qid dqid = {Qdir, 0, QTDIR};
static Qid cqid = {Qctl, 0, 0};
static Cmdtab configs[] = {
Fmirror,"mirror", 0,
Fcat, "cat", 0,
Finter, "inter", 0,
Fpart, "part", 5,
};
static char confstr[Maxconf];
static int configed;
static Fsdev*
path2dev(int i, int mustexist)
{
if (i < 0 || i >= nelem(fsdev))
error("bug: bad index in devfsdev");
if (mustexist && fsdev[i].name == nil)
error(Enonexist);
if (fsdev[i].name == nil)
return nil;
else
return &fsdev[i];
}
static Fsdev*
devalloc(void)
{
int i;
for (i = 0; i < nelem(fsdev); i++)
if (fsdev[i].name == nil)
break;
if (i == nelem(fsdev))
error(Enodev);
return &fsdev[i];
}
static void
setdsize(Fsdev* mp)
{
uchar buf[128];
int i;
Chan *mc;
Dir d;
long l;
if (mp->type != Fpart){
mp->start= 0;
mp->size = 0LL;
}
for (i = 0; i < mp->ndevs; i++){
mc = mp->idev[i];
l = devtab[mc->type]->stat(mc, buf, sizeof(buf));
convM2D(buf, l, &d, nil);
mp->isize[i] = d.length;
switch(mp->type){
case Fmirror:
if (mp->size == 0LL || mp->size > d.length)
mp->size = d.length;
break;
case Fcat:
mp->size += d.length;
break;
case Finter:
d.length = (d.length & ~(Blksize-1));
mp->isize[i] = d.length;
mp->size += d.length;
break;
case Fpart:
if (mp->start > d.length)
mp->start = d.length;
if (d.length < mp->start + mp->size)
mp->size = d.length - mp->start;
break;
}
}
}
static void
mpshut(Fsdev *mp)
{
int i;
char *nm;
nm = mp->name;
mp->name = nil;
if (nm)
free(nm);
for (i = 0; i < mp->ndevs; i++){
if (mp->idev[i] != nil)
cclose(mp->idev[i]);
if (mp->iname[i])
free(mp->iname[i]);
}
memset(mp, 0, sizeof(*mp));
}
static void
mconfig(char* a, long n)
{
static QLock lck;
Cmdbuf *cb;
Cmdtab *ct;
Fsdev *mp;
int i;
char *oldc;
char *c;
vlong size, start;
size = 0;
start = 0;
if (confstr[0] == 0)
seprint(confstr, confstr+sizeof(confstr), Cfgstr);
oldc = confstr + strlen(confstr);
qlock(&lck);
if (waserror()){
*oldc = 0;
qunlock(&lck);
nexterror();
}
cb = parsecmd(a, n);
if(waserror()){
free(cb);
nexterror();
}
c = oldc;
for (i = 0; i < cb->nf; i++)
c = seprint(c, confstr+sizeof(confstr), "%s ", cb->f[i]);
*(c-1) = '\n';
ct = lookupcmd(cb, configs, nelem(configs));
cb->f++;
cb->nf--;
if (ct->index == Fpart){
size = strtoll(cb->f[3], nil, 10);
cb->nf--;
start = strtoll(cb->f[2], nil, 10);
cb->nf--;
}
for (i = 0; i < nelem(fsdev); i++)
if (fsdev[i].name != nil && strcmp(fsdev[i].name, cb->f[0])==0)
error(Eexist);
if (cb->nf - 1 > Ndevs)
error("too many devices; fix me");
for (i = 0; i < cb->nf; i++)
validname(cb->f[i], (i != 0));
mp = devalloc();
if(waserror()){
mpshut(mp);
nexterror();
}
mp->type = ct->index;
if (mp->type == Fpart){
mp->size = size;
mp->start = start;
}
kstrdup(&mp->name, cb->f[0]);
for (i = 1; i < cb->nf; i++){
kstrdup(&mp->iname[i-1], cb->f[i]);
mp->idev[i-1] = namec(mp->iname[i-1], Aopen, ORDWR, 0);
if (mp->idev[i-1] == nil)
error(Egreg);
mp->ndevs++;
}
setdsize(mp);
poperror();
free(cb);
poperror();
poperror();
configed = 1;
qunlock(&lck);
}
static void
rdconf(void)
{
int mustrd;
char *s;
char *c;
char *p;
char *e;
Chan *cc;
s = getconf("fsconfig");
if (s == nil){
mustrd = 0;
s = "/dev/sdC0/fscfg";
} else
mustrd = 1;
if (waserror()){
configed = 1;
if (!mustrd)
return;
nexterror();
}
cc = namec(s, Aopen, OREAD, 0);
if(waserror()){
cclose(cc);
nexterror();
}
devtab[cc->type]->read(cc, confstr, sizeof(confstr), 0);
poperror();
cclose(cc);
if (strncmp(confstr, Cfgstr, strlen(Cfgstr)) != 0)
error("config string must start with `fsdev:'");
kstrdup(&c, confstr + strlen(Cfgstr));
if(waserror()){
free(c);
nexterror();
}
memset(confstr, 0, sizeof(confstr));
for (p = c; p != nil && *p != 0; p = e){
e = strchr(p, '\n');
if (e == p){
e++;
continue;
}
if (e == nil)
e = p + strlen(p);
mconfig(p, e - p);
}
poperror();
poperror();
}
static int
mgen(Chan *c, char*, Dirtab*, int, int i, Dir *dp)
{
Qid qid;
Fsdev *mp;
if (c->qid.path == Qtop){
switch(i){
case DEVDOTDOT:
devdir(c, tqid, "#k", 0, eve, DMDIR|0775, dp);
return 1;
case 0:
devdir(c, dqid, "ds", 0, eve, DMDIR|0775, dp);
return 1;
default:
return -1;
}
}
if (c->qid.path != Qdir){
switch(i){
case DEVDOTDOT:
devdir(c, dqid, "ds", 0, eve, DMDIR|0775, dp);
return 1;
default:
return -1;
}
}
switch(i){
case DEVDOTDOT:
devdir(c, tqid, "#k", 0, eve, DMDIR|0775, dp);
return 1;
case 0:
devdir(c, cqid, "ctl", 0, eve, 0664, dp);
return 1;
}
i--;
qid.path = Qfirst + i;
qid.vers = 0;
qid.type = 0;
mp = path2dev(i, 0);
if (mp == nil)
return -1;
kstrcpy(up->genbuf, mp->name, sizeof(up->genbuf));
devdir(c, qid, up->genbuf, mp->size, eve, 0664, dp);
return 1;
}
static Chan*
mattach(char *spec)
{
*confstr = 0;
return devattach(L'k', spec);
}
static Walkqid*
mwalk(Chan *c, Chan *nc, char **name, int nname)
{
if (!configed)
rdconf();
return devwalk(c, nc, name, nname, 0, 0, mgen);
}
static int
mstat(Chan *c, uchar *db, int n)
{
Dir d;
Fsdev *mp;
int p;
p = c->qid.path;
memset(&d, 0, sizeof(d));
switch(p){
case Qtop:
devdir(c, tqid, "#k", 0, eve, DMDIR|0775, &d);
break;
case Qdir:
devdir(c, dqid, "ds", 0, eve, DMDIR|0775, &d);
break;
case Qctl:
devdir(c, cqid, "ctl", 0, eve, 0664, &d);
break;
default:
mp = path2dev(p - Qfirst, 1);
devdir(c, c->qid, mp->name, mp->size, eve, 0664, &d);
}
n = convD2M(&d, db, n);
if (n == 0)
error(Ebadarg);
return n;
}
static Chan*
mopen(Chan *c, int omode)
{
if((c->qid.type & QTDIR) && omode != OREAD)
error(Eperm);
if (omode & OTRUNC)
omode &= ~OTRUNC;
c->mode = openmode(omode);
c->flag |= COPEN;
c->offset = 0;
return c;
}
static void
mclose(Chan*)
{
}
static long
catio(Fsdev *mp, int isread, void *a, long n, vlong off)
{
int i;
Chan* mc;
long l, wl, res;
res = n;
for (i = 0; n >= 0 && i < mp->ndevs ; i++){
mc = mp->idev[i];
if (off > mp->isize[i]){
off -= mp->isize[i];
continue;
}
if (off + n > mp->isize[i])
l = mp->isize[i] - off;
else
l = n;
if (isread)
wl = devtab[mc->type]->read(mc, a, l, off);
else
wl = devtab[mc->type]->write(mc, a, l, off);
if (wl != l)
error("#k: write failed");
a = (char*)a + l;
off = 0;
n -= l;
}
return res - n;
}
static long
interio(Fsdev *mp, int isread, void *a, long n, vlong off)
{
int i;
Chan* mc;
long l, wl, wsz;
vlong woff, blk, mblk;
long boff, res;
blk = off / Blksize;
boff = off % Blksize;
wsz = Blksize - boff;
res = n;
while(n > 0){
i = blk % mp->ndevs;
mc = mp->idev[i];
mblk = blk / mp->ndevs;
woff = mblk * Blksize + boff;
if (n > wsz)
l = wsz;
else
l = n;
if (isread)
wl = devtab[mc->type]->read(mc, a, l, woff);
else
wl = devtab[mc->type]->write(mc, a, l, woff);
if (wl != l || l == 0)
error(Eio);
a = (char*)a + l;
n -= l;
blk++;
boff = 0;
wsz = Blksize;
}
return res;
}
static long
mread(Chan *c, void *a, long n, vlong off)
{
int i;
Fsdev *mp;
Chan *mc;
long l;
long res;
if (c->qid.type & QTDIR)
return devdirread(c, a, n, 0, 0, mgen);
if (c->qid.path == Qctl)
return readstr((long)off, a, n, confstr + strlen(Cfgstr));
i = c->qid.path - Qfirst;
mp = path2dev(i, 1);
if (off >= mp->size)
return 0;
if (off + n > mp->size)
n = mp->size - off;
if (n == 0)
return 0;
res = -1;
switch(mp->type){
case Fmirror:
for (i = 0; i < mp->ndevs; i++){
mc = mp->idev[i];
if (waserror()){
print("#k: mread: (%llx %d): %s\n",
c->qid.path, i, up->env->errstr);
continue;
}
l = devtab[mc->type]->read(mc, a, n, off);
poperror();
if (l >=0){
res = l;
break;
}
}
if (i == mp->ndevs)
error(Eio);
break;
case Fcat:
res = catio(mp, 1, a, n, off);
break;
case Finter:
res = interio(mp, 1, a, n, off);
break;
case Fpart:
off += mp->start;
mc = mp->idev[0];
res = devtab[mc->type]->read(mc, a, n, off);
break;
}
return res;
}
static long
mwrite(Chan *c, void *a, long n, vlong off)
{
Fsdev *mp;
long l, res;
int i;
Chan *mc;
if (c->qid.type & QTDIR)
error(Eperm);
if (c->qid.path == Qctl){
mconfig(a, n);
return n;
}
mp = path2dev(c->qid.path - Qfirst, 1);
if (off >= mp->size)
return 0;
if (off + n > mp->size)
n = mp->size - off;
if (n == 0)
return 0;
res = n;
switch(mp->type){
case Fmirror:
for (i = mp->ndevs-1; i >=0; i--){
mc = mp->idev[i];
l = devtab[mc->type]->write(mc, a, n, off);
if (l < res)
res = l;
}
break;
case Fcat:
res = catio(mp, 0, a, n, off);
break;
case Finter:
res = interio(mp, 0, a, n, off);
break;
case Fpart:
mc = mp->idev[0];
off += mp->start;
l = devtab[mc->type]->write(mc, a, n, off);
if (l < res)
res = l;
break;
}
return res;
}
Dev dsdevtab = {
'k',
"ds",
devreset,
devinit,
devshutdown,
mattach,
mwalk,
mstat,
mopen,
devcreate,
mclose,
mread,
devbread,
mwrite,
devbwrite,
devremove,
devwstat,
};