#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "ip.h"
#include "ipv6.h"
#define DPRINT if(0)print
enum {
Maxmedia = 32,
Nself = Maxmedia*5,
NHASH = 1<<6,
NCACHE = 256,
QMAX = 192*1024-1,
Maxv6repr = (128/(4*4))*(4+1),
};
Medium *media[Maxmedia] = { 0 };
struct Ipself
{
uchar a[IPaddrlen];
Ipself *hnext;
Iplink *link;
ulong expire;
uchar type;
int ref;
Ipself *next;
};
struct Ipselftab
{
QLock;
int inited;
int acceptall;
Ipself *hash[NHASH];
};
typedef struct Ipmcast Ipmcast;
struct Ipmcast
{
Ipmcast *next;
uchar ma[IPaddrlen];
uchar ia[IPaddrlen];
};
#define hashipa(a) ( ( ((a)[IPaddrlen-2]<<8) | (a)[IPaddrlen-1] )%NHASH )
static char tifc[] = "ifc ";
static void addselfcache(Fs *f, Ipifc *ifc, Iplifc *lifc, uchar *a, int type);
static void remselfcache(Fs *f, Ipifc *ifc, Iplifc *lifc, uchar *a);
static char* ipifcjoinmulti(Ipifc *ifc, char **argv, int argc);
static char* ipifcleavemulti(Ipifc *ifc, char **argv, int argc);
static void ipifcregisterproxy(Fs*, Ipifc*, uchar*);
static char* ipifcremlifc(Ipifc*, Iplifc*);
void
addipmedium(Medium *med)
{
int i;
for(i = 0; i < nelem(media)-1; i++)
if(media[i] == nil){
media[i] = med;
break;
}
}
Medium*
ipfindmedium(char *name)
{
Medium **mp;
for(mp = media; *mp != nil; mp++)
if(strcmp((*mp)->name, name) == 0)
break;
return *mp;
}
static char*
ipifcbind(Conv *c, char **argv, int argc)
{
Ipifc *ifc;
Medium *m;
if(argc < 2)
return Ebadarg;
ifc = (Ipifc*)c->ptcl;
m = ipfindmedium(argv[1]);
if(m == nil)
return "unknown interface type";
wlock(ifc);
if(ifc->m != nil){
wunlock(ifc);
return "interface already bound";
}
if(waserror()){
wunlock(ifc);
nexterror();
}
(*m->bind)(ifc, argc, argv);
if(argc > 2)
strncpy(ifc->dev, argv[2], sizeof(ifc->dev));
else
snprint(ifc->dev, sizeof ifc->dev, "%s%d", m->name, c->x);
ifc->dev[sizeof(ifc->dev)-1] = 0;
ifc->m = m;
ifc->mintu = ifc->m->mintu;
ifc->maxtu = ifc->m->maxtu;
if(ifc->m->unbindonclose == 0)
ifc->conv->inuse++;
ifc->rp.mflag = 0;
ifc->rp.oflag = 0;
ifc->rp.maxraint = 600000;
ifc->rp.minraint = 200000;
ifc->rp.linkmtu = 0;
ifc->rp.reachtime = 0;
ifc->rp.rxmitra = 0;
ifc->rp.ttl = MAXTTL;
ifc->rp.routerlt = 3 * ifc->rp.maxraint;
ifc->ifcid++;
qreopen(c->rq);
qreopen(c->eq);
qreopen(c->sq);
wunlock(ifc);
poperror();
return nil;
}
static char*
ipifcunbind(Ipifc *ifc)
{
char *err;
if(waserror()){
wunlock(ifc);
nexterror();
}
wlock(ifc);
if(ifc->m != nil && ifc->m->unbindonclose == 0)
ifc->conv->inuse--;
ifc->ifcid++;
while(ifc->lifc){
err = ipifcremlifc(ifc, ifc->lifc);
if(err)
error(err);
}
if(ifc->m && ifc->m->unbind)
(*ifc->m->unbind)(ifc);
memset(ifc->dev, 0, sizeof(ifc->dev));
ifc->arg = nil;
ifc->reassemble = 0;
qclose(ifc->conv->rq);
qclose(ifc->conv->wq);
qclose(ifc->conv->sq);
ifc->m = nil;
wunlock(ifc);
poperror();
return nil;
}
char sfixedformat[] = "device %s maxtu %d sendra %d recvra %d mflag %d oflag"
" %d maxraint %d minraint %d linkmtu %d reachtime %d rxmitra %d ttl %d routerlt"
" %d pktin %lud pktout %lud errin %lud errout %lud\n";
char slineformat[] = "	%-40I %-10M %-40I %-12lud %-12lud\n";
static int
ipifcstate(Conv *c, char *state, int n)
{
Ipifc *ifc;
Iplifc *lifc;
int m;
ifc = (Ipifc*)c->ptcl;
m = snprint(state, n, sfixedformat,
ifc->dev, ifc->maxtu, ifc->sendra6, ifc->recvra6,
ifc->rp.mflag, ifc->rp.oflag, ifc->rp.maxraint,
ifc->rp.minraint, ifc->rp.linkmtu, ifc->rp.reachtime,
ifc->rp.rxmitra, ifc->rp.ttl, ifc->rp.routerlt,
ifc->in, ifc->out, ifc->inerr, ifc->outerr);
rlock(ifc);
for(lifc = ifc->lifc; lifc && n > m; lifc = lifc->next)
m += snprint(state+m, n - m, slineformat, lifc->local,
lifc->mask, lifc->remote, lifc->validlt, lifc->preflt);
if(ifc->lifc == nil)
m += snprint(state+m, n - m, "\n");
runlock(ifc);
return m;
}
static int
ipifclocal(Conv *c, char *state, int n)
{
Ipifc *ifc;
Iplifc *lifc;
Iplink *link;
int m;
ifc = (Ipifc*)c->ptcl;
m = 0;
rlock(ifc);
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
m += snprint(state+m, n - m, "%-40.40I ->", lifc->local);
for(link = lifc->link; link; link = link->lifclink)
m += snprint(state+m, n - m, " %-40.40I", link->self->a);
m += snprint(state+m, n - m, "\n");
}
runlock(ifc);
return m;
}
static int
ipifcinuse(Conv *c)
{
Ipifc *ifc;
ifc = (Ipifc*)c->ptcl;
return ifc->m != nil;
}
static void
ipifckick(void *x)
{
Conv *c = x;
Block *bp;
Ipifc *ifc;
bp = qget(c->wq);
if(bp == nil)
return;
ifc = (Ipifc*)c->ptcl;
if(!canrlock(ifc)){
freeb(bp);
return;
}
if(waserror()){
runlock(ifc);
nexterror();
}
if(ifc->m == nil || ifc->m->pktin == nil)
freeb(bp);
else
(*ifc->m->pktin)(c->p->f, ifc, bp);
runlock(ifc);
poperror();
}
static void
ipifccreate(Conv *c)
{
Ipifc *ifc;
c->rq = qopen(QMAX, 0, 0, 0);
c->sq = qopen(QMAX, 0, 0, 0);
c->wq = qopen(QMAX, Qkick, ipifckick, c);
ifc = (Ipifc*)c->ptcl;
ifc->conv = c;
ifc->unbinding = 0;
ifc->m = nil;
ifc->reassemble = 0;
}
static void
ipifcclose(Conv *c)
{
Ipifc *ifc;
Medium *m;
ifc = (Ipifc*)c->ptcl;
m = ifc->m;
if(m && m->unbindonclose)
ipifcunbind(ifc);
}
char*
ipifcsetmtu(Ipifc *ifc, char **argv, int argc)
{
int mtu;
if(argc < 2 || ifc->m == nil)
return Ebadarg;
mtu = strtoul(argv[1], 0, 0);
if(mtu < ifc->m->mintu || mtu > ifc->m->maxtu)
return Ebadarg;
ifc->maxtu = mtu;
return nil;
}
char*
ipifcadd(Ipifc *ifc, char **argv, int argc, int tentative, Iplifc *lifcp)
{
int i, type, mtu, sendnbrdisc = 0;
uchar ip[IPaddrlen], mask[IPaddrlen], rem[IPaddrlen];
uchar bcast[IPaddrlen], net[IPaddrlen];
Iplifc *lifc, **l;
Fs *f;
if(ifc->m == nil)
return "ipifc not yet bound to device";
f = ifc->conv->p->f;
type = Rifc;
memset(ip, 0, IPaddrlen);
memset(mask, 0, IPaddrlen);
memset(rem, 0, IPaddrlen);
switch(argc){
case 6:
if(strcmp(argv[5], "proxy") == 0)
type |= Rproxy;
case 5:
mtu = strtoul(argv[4], 0, 0);
if(mtu >= ifc->m->mintu && mtu <= ifc->m->maxtu)
ifc->maxtu = mtu;
case 4:
if (parseip(ip, argv[1]) == -1 || parseip(rem, argv[3]) == -1)
return Ebadip;
parseipmask(mask, argv[2]);
maskip(rem, mask, net);
break;
case 3:
if (parseip(ip, argv[1]) == -1)
return Ebadip;
parseipmask(mask, argv[2]);
maskip(ip, mask, rem);
maskip(rem, mask, net);
break;
case 2:
if (parseip(ip, argv[1]) == -1)
return Ebadip;
memmove(mask, defmask(ip), IPaddrlen);
maskip(ip, mask, rem);
maskip(rem, mask, net);
break;
default:
return Ebadarg;
}
if(isv4(ip))
tentative = 0;
wlock(ifc);
for(lifc = ifc->lifc; lifc; lifc = lifc->next) {
if(ipcmp(lifc->local, ip) == 0) {
if(lifc->tentative != tentative)
lifc->tentative = tentative;
if(lifcp) {
lifc->onlink = lifcp->onlink;
lifc->autoflag = lifcp->autoflag;
lifc->validlt = lifcp->validlt;
lifc->preflt = lifcp->preflt;
lifc->origint = lifcp->origint;
}
goto out;
}
}
lifc = smalloc(sizeof(Iplifc));
ipmove(lifc->local, ip);
ipmove(lifc->mask, mask);
ipmove(lifc->remote, rem);
ipmove(lifc->net, net);
lifc->tentative = tentative;
if(lifcp) {
lifc->onlink = lifcp->onlink;
lifc->autoflag = lifcp->autoflag;
lifc->validlt = lifcp->validlt;
lifc->preflt = lifcp->preflt;
lifc->origint = lifcp->origint;
} else {
lifc->onlink = lifc->autoflag = 1;
lifc->validlt = lifc->preflt = ~0L;
lifc->origint = NOW / 1000;
}
lifc->next = nil;
for(l = &ifc->lifc; *l; l = &(*l)->next)
;
*l = lifc;
if(ipcmp(ip, v6loopback))
if(ipcmp(mask, IPallbits) == 0)
type |= Rptpt;
if(isv4(ip))
v4addroute(f, tifc, rem+IPv4off, mask+IPv4off, rem+IPv4off, type);
else
v6addroute(f, tifc, rem, mask, rem, type);
addselfcache(f, ifc, lifc, ip, Runi);
if((type & (Rproxy|Rptpt)) == (Rproxy|Rptpt)){
ipifcregisterproxy(f, ifc, rem);
goto out;
}
if(isv4(ip) || ipcmp(ip, IPnoaddr) == 0) {
for(i = 0; i < IPaddrlen; i++)
bcast[i] = (ip[i] & mask[i]) | ~mask[i];
addselfcache(f, ifc, lifc, bcast, Rbcast);
for(i = 0; i < IPaddrlen; i++)
bcast[i] = (ip[i] & mask[i]) & mask[i];
addselfcache(f, ifc, lifc, bcast, Rbcast);
memmove(mask, defmask(ip), IPaddrlen);
for(i = 0; i < IPaddrlen; i++)
bcast[i] = (ip[i] & mask[i]) | ~mask[i];
addselfcache(f, ifc, lifc, bcast, Rbcast);
memmove(mask, defmask(ip), IPaddrlen);
for(i = 0; i < IPaddrlen; i++)
bcast[i] = (ip[i] & mask[i]) & mask[i];
addselfcache(f, ifc, lifc, bcast, Rbcast);
addselfcache(f, ifc, lifc, IPv4bcast, Rbcast);
}
else {
if(ipcmp(ip, v6loopback) == 0) {
addselfcache(f, ifc, lifc, v6allnodesN, Rmulti);
v6addroute(f, tifc, v6allnodesN, v6allnodesNmask,
v6allnodesN, Rmulti);
}
addselfcache(f, ifc, lifc, v6allnodesL, Rmulti);
v6addroute(f, tifc, v6allnodesL, v6allnodesLmask, v6allnodesL,
Rmulti);
ipv62smcast(bcast, ip);
addselfcache(f, ifc, lifc, bcast, Rmulti);
sendnbrdisc = 1;
}
if(isv4(ip) && ifc->m->areg != nil)
(*ifc->m->areg)(ifc, ip);
out:
wunlock(ifc);
if(tentative && sendnbrdisc)
icmpns(f, 0, SRC_UNSPEC, ip, TARG_MULTI, ifc->mac);
return nil;
}
static char*
ipifcremlifc(Ipifc *ifc, Iplifc *lifc)
{
Iplifc **l;
Fs *f;
f = ifc->conv->p->f;
for(l = &ifc->lifc; *l != nil && *l != lifc; l = &(*l)->next)
;
if(*l == nil)
return "address not on this interface";
*l = lifc->next;
while(lifc->link)
remselfcache(f, ifc, lifc, lifc->link->self->a);
if(isv4(lifc->local))
v4delroute(f, lifc->remote+IPv4off, lifc->mask+IPv4off, 1);
else {
v6delroute(f, lifc->remote, lifc->mask, 1);
if(ipcmp(lifc->local, v6loopback) == 0)
v6delroute(f, v6allnodesN, v6allnodesNmask, 1);
else if(memcmp(lifc->local, v6linklocal, v6llpreflen) == 0)
v6delroute(f, v6allnodesL, v6allnodesLmask, 1);
}
free(lifc);
return nil;
}
char*
ipifcrem(Ipifc *ifc, char **argv, int argc)
{
char *rv;
uchar ip[IPaddrlen], mask[IPaddrlen], rem[IPaddrlen];
Iplifc *lifc;
if(argc < 3)
return Ebadarg;
if (parseip(ip, argv[1]) == -1)
return Ebadip;
parseipmask(mask, argv[2]);
if(argc < 4)
maskip(ip, mask, rem);
else
if (parseip(rem, argv[3]) == -1)
return Ebadip;
wlock(ifc);
for(lifc = ifc->lifc; lifc != nil; lifc = lifc->next) {
if (memcmp(ip, lifc->local, IPaddrlen) == 0
&& memcmp(mask, lifc->mask, IPaddrlen) == 0
&& memcmp(rem, lifc->remote, IPaddrlen) == 0)
break;
}
rv = ipifcremlifc(ifc, lifc);
wunlock(ifc);
return rv;
}
void
ipifcaddroute(Fs *f, int vers, uchar *addr, uchar *mask, uchar *gate, int type)
{
Medium *m;
Conv **cp, **e;
Ipifc *ifc;
e = &f->ipifc->conv[f->ipifc->nc];
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp != nil) {
ifc = (Ipifc*)(*cp)->ptcl;
m = ifc->m;
if(m && m->addroute)
m->addroute(ifc, vers, addr, mask, gate, type);
}
}
}
void
ipifcremroute(Fs *f, int vers, uchar *addr, uchar *mask)
{
Medium *m;
Conv **cp, **e;
Ipifc *ifc;
e = &f->ipifc->conv[f->ipifc->nc];
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp != nil) {
ifc = (Ipifc*)(*cp)->ptcl;
m = ifc->m;
if(m && m->remroute)
m->remroute(ifc, vers, addr, mask);
}
}
}
static char*
ipifcconnect(Conv* c, char **argv, int argc)
{
char *err;
Ipifc *ifc;
ifc = (Ipifc*)c->ptcl;
if(ifc->m == nil)
return "ipifc not yet bound to device";
if(waserror()){
wunlock(ifc);
nexterror();
}
wlock(ifc);
while(ifc->lifc){
err = ipifcremlifc(ifc, ifc->lifc);
if(err)
error(err);
}
wunlock(ifc);
poperror();
err = ipifcadd(ifc, argv, argc, 0, nil);
if(err)
return err;
Fsconnected(c, nil);
return nil;
}
char*
ipifcra6(Ipifc *ifc, char **argv, int argc)
{
int i, argsleft, vmax = ifc->rp.maxraint, vmin = ifc->rp.minraint;
argsleft = argc - 1;
i = 1;
if(argsleft % 2 != 0)
return Ebadarg;
while (argsleft > 1) {
if(strcmp(argv[i], "recvra") == 0)
ifc->recvra6 = (atoi(argv[i+1]) != 0);
else if(strcmp(argv[i], "sendra") == 0)
ifc->sendra6 = (atoi(argv[i+1]) != 0);
else if(strcmp(argv[i], "mflag") == 0)
ifc->rp.mflag = (atoi(argv[i+1]) != 0);
else if(strcmp(argv[i], "oflag") == 0)
ifc->rp.oflag = (atoi(argv[i+1]) != 0);
else if(strcmp(argv[i], "maxraint") == 0)
ifc->rp.maxraint = atoi(argv[i+1]);
else if(strcmp(argv[i], "minraint") == 0)
ifc->rp.minraint = atoi(argv[i+1]);
else if(strcmp(argv[i], "linkmtu") == 0)
ifc->rp.linkmtu = atoi(argv[i+1]);
else if(strcmp(argv[i], "reachtime") == 0)
ifc->rp.reachtime = atoi(argv[i+1]);
else if(strcmp(argv[i], "rxmitra") == 0)
ifc->rp.rxmitra = atoi(argv[i+1]);
else if(strcmp(argv[i], "ttl") == 0)
ifc->rp.ttl = atoi(argv[i+1]);
else if(strcmp(argv[i], "routerlt") == 0)
ifc->rp.routerlt = atoi(argv[i+1]);
else
return Ebadarg;
argsleft -= 2;
i += 2;
}
if(ifc->rp.maxraint < ifc->rp.minraint) {
ifc->rp.maxraint = vmax;
ifc->rp.minraint = vmin;
return Ebadarg;
}
return nil;
}
static char*
ipifcctl(Conv* c, char**argv, int argc)
{
Ipifc *ifc;
int i;
ifc = (Ipifc*)c->ptcl;
if(strcmp(argv[0], "add") == 0)
return ipifcadd(ifc, argv, argc, 0, nil);
else if(strcmp(argv[0], "try") == 0)
return ipifcadd(ifc, argv, argc, 1, nil);
else if(strcmp(argv[0], "remove") == 0)
return ipifcrem(ifc, argv, argc);
else if(strcmp(argv[0], "unbind") == 0)
return ipifcunbind(ifc);
else if(strcmp(argv[0], "joinmulti") == 0)
return ipifcjoinmulti(ifc, argv, argc);
else if(strcmp(argv[0], "leavemulti") == 0)
return ipifcleavemulti(ifc, argv, argc);
else if(strcmp(argv[0], "mtu") == 0)
return ipifcsetmtu(ifc, argv, argc);
else if(strcmp(argv[0], "reassemble") == 0){
ifc->reassemble = 1;
return nil;
}
else if(strcmp(argv[0], "iprouting") == 0){
i = 1;
if(argc > 1)
i = atoi(argv[1]);
iprouting(c->p->f, i);
return nil;
}
else if(strcmp(argv[0], "add6") == 0)
return ipifcadd6(ifc, argv, argc);
else if(strcmp(argv[0], "ra6") == 0)
return ipifcra6(ifc, argv, argc);
return "unsupported ctl";
}
int
ipifcstats(Proto *ipifc, char *buf, int len)
{
return ipstats(ipifc->f, buf, len);
}
void
ipifcinit(Fs *f)
{
Proto *ipifc;
ipifc = smalloc(sizeof(Proto));
ipifc->name = "ipifc";
ipifc->connect = ipifcconnect;
ipifc->announce = nil;
ipifc->bind = ipifcbind;
ipifc->state = ipifcstate;
ipifc->create = ipifccreate;
ipifc->close = ipifcclose;
ipifc->rcv = nil;
ipifc->ctl = ipifcctl;
ipifc->advise = nil;
ipifc->stats = ipifcstats;
ipifc->inuse = ipifcinuse;
ipifc->local = ipifclocal;
ipifc->ipproto = -1;
ipifc->nc = Maxmedia;
ipifc->ptclsize = sizeof(Ipifc);
f->ipifc = ipifc;
f->self = smalloc(sizeof(Ipselftab));
Fsproto(f, ipifc);
}
static void
addselfcache(Fs *f, Ipifc *ifc, Iplifc *lifc, uchar *a, int type)
{
Ipself *p;
Iplink *lp;
int h;
qlock(f->self);
h = hashipa(a);
for(p = f->self->hash[h]; p; p = p->next)
if(memcmp(a, p->a, IPaddrlen) == 0)
break;
if(p == nil){
p = smalloc(sizeof(*p));
ipmove(p->a, a);
p->type = type;
p->next = f->self->hash[h];
f->self->hash[h] = p;
if(ipcmp(a, v4prefix) == 0 || ipcmp(a, IPnoaddr) == 0)
f->self->acceptall = 1;
}
for(lp = p->link; lp; lp = lp->selflink)
if(lp->lifc == lifc)
break;
if(lp == nil){
lp = smalloc(sizeof(*lp));
lp->ref = 1;
lp->lifc = lifc;
lp->self = p;
lp->selflink = p->link;
p->link = lp;
lp->lifclink = lifc->link;
lifc->link = lp;
if(isv4(a))
v4addroute(f, tifc, a+IPv4off, IPallbits+IPv4off,
a+IPv4off, type);
else
v6addroute(f, tifc, a, IPallbits, a, type);
if((type & Rmulti) && ifc->m->addmulti != nil)
(*ifc->m->addmulti)(ifc, a, lifc->local);
} else
lp->ref++;
qunlock(f->self);
}
static Iplink *freeiplink;
static Ipself *freeipself;
static void
iplinkfree(Iplink *p)
{
Iplink **l, *np;
ulong now = NOW;
l = &freeiplink;
for(np = *l; np; np = *l){
if(np->expire > now){
*l = np->next;
free(np);
continue;
}
l = &np->next;
}
p->expire = now + 5000;
p->next = nil;
*l = p;
}
static void
ipselffree(Ipself *p)
{
Ipself **l, *np;
ulong now = NOW;
l = &freeipself;
for(np = *l; np; np = *l){
if(np->expire > now){
*l = np->next;
free(np);
continue;
}
l = &np->next;
}
p->expire = now + 5000;
p->next = nil;
*l = p;
}
static void
remselfcache(Fs *f, Ipifc *ifc, Iplifc *lifc, uchar *a)
{
Ipself *p, **l;
Iplink *link, **l_self, **l_lifc;
qlock(f->self);
l = &f->self->hash[hashipa(a)];
for(p = *l; p; p = *l){
if(ipcmp(p->a, a) == 0)
break;
l = &p->next;
}
if(p == nil)
goto out;
l_lifc = &lifc->link;
for(link = *l_lifc; link; link = *l_lifc){
if(link->self == p)
break;
l_lifc = &link->lifclink;
}
if(link == nil)
goto out;
l_self = &p->link;
for(link = *l_self; link; link = *l_self){
if(link == *l_lifc)
break;
l_self = &link->selflink;
}
if(link == nil)
panic("remselfcache");
if(--(link->ref) != 0)
goto out;
if((p->type & Rmulti) && ifc->m->remmulti != nil)
(*ifc->m->remmulti)(ifc, a, lifc->local);
*l_lifc = link->lifclink;
*l_self = link->selflink;
iplinkfree(link);
if(p->link != nil)
goto out;
if(isv4(a))
v4delroute(f, a+IPv4off, IPallbits+IPv4off, 1);
else
v6delroute(f, a, IPallbits, 1);
*l = p->next;
ipselffree(p);
if(ipcmp(a, v4prefix) == 0 || ipcmp(a, IPnoaddr) == 0)
f->self->acceptall = 0;
out:
qunlock(f->self);
}
static char *stformat = "%-44.44I %2.2d %4.4s\n";
enum
{
Nstformat= 41,
};
long
ipselftabread(Fs *f, char *cp, ulong offset, int n)
{
int i, m, nifc, off;
Ipself *p;
Iplink *link;
char state[8];
m = 0;
off = offset;
qlock(f->self);
for(i = 0; i < NHASH && m < n; i++){
for(p = f->self->hash[i]; p != nil && m < n; p = p->next){
nifc = 0;
for(link = p->link; link; link = link->selflink)
nifc++;
routetype(p->type, state);
m += snprint(cp + m, n - m, stformat, p->a, nifc, state);
if(off > 0){
off -= m;
m = 0;
}
}
}
qunlock(f->self);
return m;
}
int
iptentative(Fs *f, uchar *addr)
{
Ipself *p;
p = f->self->hash[hashipa(addr)];
for(; p; p = p->next){
if(ipcmp(addr, p->a) == 0)
return p->link->lifc->tentative;
}
return 0;
}
int
ipforme(Fs *f, uchar *addr)
{
Ipself *p;
p = f->self->hash[hashipa(addr)];
for(; p; p = p->next){
if(ipcmp(addr, p->a) == 0)
return p->type;
}
if(f->self->acceptall)
return Runi;
return 0;
}
Ipifc*
findipifc(Fs *f, uchar *remote, int type)
{
Ipifc *ifc, *x;
Iplifc *lifc;
Conv **cp, **e;
uchar gnet[IPaddrlen], xmask[IPaddrlen];
x = nil;
memset(xmask, 0, IPaddrlen);
e = &f->ipifc->conv[f->ipifc->nc];
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == 0)
continue;
ifc = (Ipifc*)(*cp)->ptcl;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
maskip(remote, lifc->mask, gnet);
if(ipcmp(gnet, lifc->net) == 0){
if(x == nil || ipcmp(lifc->mask, xmask) > 0){
x = ifc;
ipmove(xmask, lifc->mask);
}
}
}
}
if(x != nil)
return x;
if(type & (Rbcast|Rmulti)){
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == 0)
continue;
ifc = (Ipifc*)(*cp)->ptcl;
if(ifc->lifc != nil)
return ifc;
}
}
return nil;
}
enum {
unknownv6,
unspecifiedv6,
linklocalv6,
globalv6,
};
int
v6addrtype(uchar *addr)
{
if(isv4(addr) || ipcmp(addr, IPnoaddr) == 0)
return unknownv6;
else if(islinklocal(addr) ||
isv6mcast(addr) && (addr[1] & 0xF) <= Link_local_scop)
return linklocalv6;
else
return globalv6;
}
#define v6addrcurr(lifc) ((lifc)->preflt == ~0L || \
(lifc)->origint + (lifc)->preflt >= NOW/1000)
static void
findprimaryipv6(Fs *f, uchar *local)
{
int atype, atypel;
Conv **cp, **e;
Ipifc *ifc;
Iplifc *lifc;
ipmove(local, v6Unspecified);
atype = unspecifiedv6;
e = &f->ipifc->conv[f->ipifc->nc];
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == 0)
continue;
ifc = (Ipifc*)(*cp)->ptcl;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
atypel = v6addrtype(lifc->local);
if(atypel > atype && v6addrcurr(lifc)) {
ipmove(local, lifc->local);
atype = atypel;
if(atype == globalv6)
return;
}
}
}
}
static void
findprimaryipv4(Fs *f, uchar *local)
{
Conv **cp, **e;
Ipifc *ifc;
Iplifc *lifc;
e = &f->ipifc->conv[f->ipifc->nc];
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == 0)
continue;
ifc = (Ipifc*)(*cp)->ptcl;
if((lifc = ifc->lifc) != nil){
ipmove(local, lifc->local);
return;
}
}
}
void
findlocalip(Fs *f, uchar *local, uchar *remote)
{
int version, atype = unspecifiedv6, atypel = unknownv6;
int atyper, deprecated;
uchar gate[IPaddrlen], gnet[IPaddrlen];
Ipifc *ifc;
Iplifc *lifc;
Route *r;
USED(atype);
USED(atypel);
qlock(f->ipifc);
r = v6lookup(f, remote, nil);
version = (memcmp(remote, v4prefix, IPv4off) == 0)? V4: V6;
if(r != nil){
ifc = r->ifc;
if(r->type & Rv4)
v4tov6(gate, r->v4.gate);
else {
ipmove(gate, r->v6.gate);
ipmove(local, v6Unspecified);
}
switch(version) {
case V4:
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
maskip(gate, lifc->mask, gnet);
if(ipcmp(gnet, lifc->net) == 0){
ipmove(local, lifc->local);
goto out;
}
}
break;
case V6:
atyper = v6addrtype(remote);
deprecated = 0;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
atypel = v6addrtype(lifc->local);
if(atypel > atype && atype < atyper ||
atypel < atype && atype > atyper){
ipmove(local, lifc->local);
deprecated = !v6addrcurr(lifc);
atype = atypel;
} else if(atypel == atype){
if(deprecated && v6addrcurr(lifc)){
ipmove(local, lifc->local);
atype = atypel;
deprecated = 0;
}
}
if(atype == atyper && !deprecated)
goto out;
}
if(atype >= atyper)
goto out;
break;
default:
panic("findlocalip: version %d", version);
}
}
switch(version){
case V4:
findprimaryipv4(f, local);
break;
case V6:
findprimaryipv6(f, local);
break;
default:
panic("findlocalip2: version %d", version);
}
out:
qunlock(f->ipifc);
}
int
ipv4local(Ipifc *ifc, uchar *addr)
{
Iplifc *lifc;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
if(isv4(lifc->local)){
memmove(addr, lifc->local+IPv4off, IPv4addrlen);
return 1;
}
}
return 0;
}
int
ipv6local(Ipifc *ifc, uchar *addr)
{
Iplifc *lifc;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
if(!isv4(lifc->local) && !(lifc->tentative)){
ipmove(addr, lifc->local);
return 1;
}
}
return 0;
}
int
ipv6anylocal(Ipifc *ifc, uchar *addr)
{
Iplifc *lifc;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
if(!isv4(lifc->local)){
ipmove(addr, lifc->local);
return SRC_UNI;
}
}
return SRC_UNSPEC;
}
Iplifc*
iplocalonifc(Ipifc *ifc, uchar *ip)
{
Iplifc *lifc;
for(lifc = ifc->lifc; lifc; lifc = lifc->next)
if(ipcmp(ip, lifc->local) == 0)
return lifc;
return nil;
}
int
ipproxyifc(Fs *f, Ipifc *ifc, uchar *ip)
{
Route *r;
uchar net[IPaddrlen];
Iplifc *lifc;
r = v6lookup(f, ip, nil);
if(r == nil || (r->type & (Rifc|Rproxy)) != (Rifc|Rproxy))
return 0;
for(lifc = ifc->lifc; lifc; lifc = lifc->next){
maskip(ip, lifc->mask, net);
if(ipcmp(net, lifc->remote) == 0)
return 1;
}
return 0;
}
int
ipismulticast(uchar *ip)
{
if(isv4(ip)){
if(ip[IPv4off] >= 0xe0 && ip[IPv4off] < 0xf0)
return V4;
}
else if(ip[0] == 0xff)
return V6;
return 0;
}
int
ipisbm(uchar *ip)
{
if(isv4(ip)){
if(ip[IPv4off] >= 0xe0 && ip[IPv4off] < 0xf0)
return V4;
else if(ipcmp(ip, IPv4bcast) == 0)
return V4;
}
else if(ip[0] == 0xff)
return V6;
return 0;
}
void
ipifcaddmulti(Conv *c, uchar *ma, uchar *ia)
{
Ipifc *ifc;
Iplifc *lifc;
Conv **p;
Ipmulti *multi, **l;
Fs *f;
f = c->p->f;
for(l = &c->multi; *l; l = &(*l)->next)
if(ipcmp(ma, (*l)->ma) == 0 && ipcmp(ia, (*l)->ia) == 0)
return;
multi = *l = smalloc(sizeof(*multi));
ipmove(multi->ma, ma);
ipmove(multi->ia, ia);
multi->next = nil;
for(p = f->ipifc->conv; *p; p++){
if((*p)->inuse == 0)
continue;
ifc = (Ipifc*)(*p)->ptcl;
if(waserror()){
wunlock(ifc);
nexterror();
}
wlock(ifc);
for(lifc = ifc->lifc; lifc; lifc = lifc->next)
if(ipcmp(ia, lifc->local) == 0)
addselfcache(f, ifc, lifc, ma, Rmulti);
wunlock(ifc);
poperror();
}
}
void
ipifcremmulti(Conv *c, uchar *ma, uchar *ia)
{
Ipmulti *multi, **l;
Iplifc *lifc;
Conv **p;
Ipifc *ifc;
Fs *f;
f = c->p->f;
for(l = &c->multi; *l; l = &(*l)->next)
if(ipcmp(ma, (*l)->ma) == 0 && ipcmp(ia, (*l)->ia) == 0)
break;
multi = *l;
if(multi == nil)
return;
*l = multi->next;
for(p = f->ipifc->conv; *p; p++){
if((*p)->inuse == 0)
continue;
ifc = (Ipifc*)(*p)->ptcl;
if(waserror()){
wunlock(ifc);
nexterror();
}
wlock(ifc);
for(lifc = ifc->lifc; lifc; lifc = lifc->next)
if(ipcmp(ia, lifc->local) == 0)
remselfcache(f, ifc, lifc, ma);
wunlock(ifc);
poperror();
}
free(multi);
}
static char*
ipifcjoinmulti(Ipifc *ifc, char **argv, int argc)
{
USED(ifc, argv, argc);
return nil;
}
static char*
ipifcleavemulti(Ipifc *ifc, char **argv, int argc)
{
USED(ifc, argv, argc);
return nil;
}
static void
ipifcregisterproxy(Fs *f, Ipifc *ifc, uchar *ip)
{
Conv **cp, **e;
Ipifc *nifc;
Iplifc *lifc;
Medium *m;
uchar net[IPaddrlen];
e = &f->ipifc->conv[f->ipifc->nc];
if(!isv4(ip)) {
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == nil || (nifc = (Ipifc*)(*cp)->ptcl) == ifc)
continue;
rlock(nifc);
m = nifc->m;
if(m == nil || m->addmulti == nil) {
runlock(nifc);
continue;
}
for(lifc = nifc->lifc; lifc; lifc = lifc->next){
maskip(ip, lifc->mask, net);
if(ipcmp(net, lifc->remote) == 0) {
ipv62smcast(net, ip);
addselfcache(f, nifc, lifc, net, Rmulti);
arpenter(f, V6, ip, nifc->mac, 6, 0);
break;
}
}
runlock(nifc);
}
}
else {
for(cp = f->ipifc->conv; cp < e; cp++){
if(*cp == nil || (nifc = (Ipifc*)(*cp)->ptcl) == ifc)
continue;
rlock(nifc);
m = nifc->m;
if(m == nil || m->areg == nil){
runlock(nifc);
continue;
}
for(lifc = nifc->lifc; lifc; lifc = lifc->next){
maskip(ip, lifc->mask, net);
if(ipcmp(net, lifc->remote) == 0){
(*m->areg)(nifc, ip);
break;
}
}
runlock(nifc);
}
}
}
static void
adddefroute6(Fs *f, uchar *gate, int force)
{
Route *r;
r = v6lookup(f, v6Unspecified, nil);
if (r && !force && strcmp(r->tag, "ra") != 0)
return;
v6delroute(f, v6Unspecified, v6Unspecified, 1);
v6addroute(f, "ra", v6Unspecified, v6Unspecified, gate, 0);
}
enum {
Ngates = 3,
};
char*
ipifcadd6(Ipifc *ifc, char**argv, int argc)
{
int plen = 64;
long origint = NOW / 1000, preflt = ~0L, validlt = ~0L;
char addr[Maxv6repr], preflen[6];
char *params[3];
uchar autoflag = 1, onlink = 1;
uchar prefix[IPaddrlen];
Iplifc *lifc;
switch(argc) {
case 7:
preflt = atoi(argv[6]);
case 6:
validlt = atoi(argv[5]);
case 5:
autoflag = atoi(argv[4]);
case 4:
onlink = atoi(argv[3]);
case 3:
plen = atoi(argv[2]);
case 2:
break;
default:
return Ebadarg;
}
if (parseip(prefix, argv[1]) != 6)
return "bad ipv6 address";
if (validlt < preflt)
return "valid ipv6 lifetime less than preferred lifetime";
if (plen < 0)
return "negative ipv6 prefix length";
if (islinklocal(prefix))
return "ipv6 prefix is link-local";
lifc = smalloc(sizeof(Iplifc));
lifc->onlink = (onlink != 0);
lifc->autoflag = (autoflag != 0);
lifc->validlt = validlt;
lifc->preflt = preflt;
lifc->origint = origint;
if(!ifc->m->pref2addr)
return "no pref2addr on interface";
ifc->m->pref2addr(prefix, ifc->mac);
snprint(addr, sizeof addr, "%I", prefix);
snprint(preflen, sizeof preflen, "/%d", plen);
params[0] = "add";
params[1] = addr;
params[2] = preflen;
return ipifcadd(ifc, params, 3, 0, lifc);
}