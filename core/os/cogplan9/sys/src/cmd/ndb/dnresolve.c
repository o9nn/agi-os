#include <u.h>
#include <libc.h>
#include <ip.h>
#include <bio.h>
#include <ndb.h>
#include "dns.h"
typedef struct Dest Dest;
typedef struct Ipaddr Ipaddr;
typedef struct Query Query;
enum
{
Udp, Tcp,
Answerr= -1,
Answnone,
Maxdest= 24,
Maxoutstanding= 15,
Remntretry= 15,
Maxtrans= 5,
Maxretries= 5,
Maxwaitms= 5000,
Minwaitms= 500,
Destmagic= 0xcafebabe,
Querymagic= 0xdeadbeef,
};
enum { Hurry, Patient, };
enum { Outns, Inns, };
struct Ipaddr {
Ipaddr *next;
uchar ip[IPaddrlen];
};
struct Dest
{
uchar a[IPaddrlen];
DN *s;
int nx;
int code;
ulong magic;
};
struct Query {
DN *dp;
ushort type;
Request *req;
RR *nsrp;
Dest *dest;
Dest *curdest;
int ndest;
int udpfd;
QLock tcplock;
int tcpset;
int tcpfd;
int tcpctlfd;
uchar tcpip[IPaddrlen];
ulong magic;
};
int likely[] = {
[Ta] 95,
[Taaaa] 10,
[Tcname] 15,
[Tmx] 60,
[Tns] 90,
[Tnull] 5,
[Tptr] 35,
[Tsoa] 90,
[Tsrv] 60,
[Ttxt] 15,
[Tall] 95,
};
static RR* dnresolve1(char*, int, int, Request*, int, int);
static int netquery(Query *, int);
static char *
procgetname(void)
{
int fd, n;
char *lp, *rp;
char buf[256];
snprint(buf, sizeof buf, "#p/%d/args", getpid());
if((fd = open(buf, OREAD)) < 0)
return strdup("");
*buf = '\0';
n = read(fd, buf, sizeof buf-1);
close(fd);
if (n >= 0)
buf[n] = '\0';
if ((lp = strchr(buf, '[')) == nil ||
(rp = strrchr(buf, ']')) == nil)
return strdup("");
*rp = '\0';
return strdup(lp+1);
}
void
rrfreelistptr(RR **rpp)
{
RR *rp;
if (rpp == nil || *rpp == nil)
return;
rp = *rpp;
*rpp = nil;
rrfreelist(rp);
}
RR*
dnresolve(char *name, int class, int type, Request *req, RR **cn, int depth,
int recurse, int rooted, int *status)
{
RR *rp, *nrp, *drp;
DN *dp;
int loops;
char *procname;
char nname[Domlen];
if(status)
*status = 0;
if(depth > 12)
return nil;
procname = procgetname();
if(!rooted && strchr(name, '.') == nil){
rp = nil;
drp = domainlist(class);
for(nrp = drp; rp == nil && nrp != nil; nrp = nrp->next){
snprint(nname, sizeof nname, "%s.%s", name,
nrp->ptr->name);
rp = dnresolve(nname, class, type, req, cn, depth+1,
recurse, rooted, status);
lock(&dnlock);
rrfreelist(rrremneg(&rp));
unlock(&dnlock);
}
if(drp != nil)
rrfreelist(drp);
procsetname(procname);
free(procname);
return rp;
}
rp = dnresolve1(name, class, type, req, depth, recurse);
if(rp == nil) {
dp = dnlookup(name, class, 0);
if(type != Tptr && dp->respcode != Rname)
for(loops = 0; rp == nil && loops < Maxretries; loops++){
rp = dnresolve1(name, class, Tcname, req,
depth, recurse);
if(rp == nil)
break;
if(rp->negative || rp->host == nil){
rrfreelist(rp);
rp = nil;
break;
}
name = rp->host->name;
lock(&dnlock);
if(cn)
rrcat(cn, rp);
else
rrfreelist(rp);
unlock(&dnlock);
rp = dnresolve1(name, class, type, req,
depth, recurse);
}
if(rp == nil && status != nil && dp->respcode != Rok)
*status = dp->respcode;
}
procsetname(procname);
free(procname);
return randomize(rp);
}
static void
queryinit(Query *qp, DN *dp, int type, Request *req)
{
memset(qp, 0, sizeof *qp);
qp->udpfd = qp->tcpfd = qp->tcpctlfd = -1;
qp->dp = dp;
qp->type = type;
if (qp->type != type)
dnslog("queryinit: bogus type %d", type);
qp->req = req;
qp->nsrp = nil;
qp->dest = qp->curdest = nil;
qp->magic = Querymagic;
}
static void
queryck(Query *qp)
{
assert(qp);
assert(qp->magic == Querymagic);
}
static void
querydestroy(Query *qp)
{
queryck(qp);
if (qp->tcpfd > 0)
close(qp->tcpfd);
if (qp->tcpctlfd > 0) {
hangup(qp->tcpctlfd);
close(qp->tcpctlfd);
}
free(qp->dest);
memset(qp, 0, sizeof *qp);
qp->udpfd = qp->tcpfd = qp->tcpctlfd = -1;
}
static void
destinit(Dest *p)
{
memset(p, 0, sizeof *p);
p->magic = Destmagic;
}
static void
destck(Dest *p)
{
assert(p);
assert(p->magic == Destmagic);
}
static void
notestats(vlong start, int tmout, int type)
{
qlock(&stats);
if (tmout) {
stats.tmout++;
if (type == Taaaa)
stats.tmoutv6++;
else if (type == Tcname)
stats.tmoutcname++;
} else {
long wait10ths = NS2MS(nsec() - start) / 100;
if (wait10ths <= 0)
stats.under10ths[0]++;
else if (wait10ths >= nelem(stats.under10ths))
stats.under10ths[nelem(stats.under10ths) - 1]++;
else
stats.under10ths[wait10ths]++;
}
qunlock(&stats);
}
static void
noteinmem(void)
{
qlock(&stats);
stats.answinmem++;
qunlock(&stats);
}
static int
netqueryns(Query *qp, int depth, RR *nsrp)
{
int rv;
qp->nsrp = nsrp;
rv = netquery(qp, depth);
lock(&dnlock);
rrfreelist(nsrp);
unlock(&dnlock);
return rv;
}
static RR*
issuequery(Query *qp, char *name, int class, int depth, int recurse)
{
char *cp;
DN *nsdp;
RR *rp, *nsrp, *dbnsrp;
if(cfg.resolver){
nsrp = randomize(getdnsservers(class));
if(nsrp != nil)
if(netqueryns(qp, depth+1, nsrp) > Answnone)
return rrlookup(qp->dp, qp->type, OKneg);
}
for(cp = name; cp; cp = walkup(cp)){
dbnsrp = randomize(dblookup(cp, class, Tns, 0, 0));
if(dbnsrp && dbnsrp->local){
rp = dblookup(name, class, qp->type, 1, dbnsrp->ttl);
lock(&dnlock);
rrfreelist(dbnsrp);
unlock(&dnlock);
return rp;
}
if(recurse == Dontrecurse){
if(dbnsrp) {
lock(&dnlock);
rrfreelist(dbnsrp);
unlock(&dnlock);
}
continue;
}
nsdp = dnlookup(cp, class, 0);
nsrp = nil;
if(nsdp)
nsrp = randomize(rrlookup(nsdp, Tns, NOneg));
if(nsrp && nsrp->ttl < now){
lock(&dnlock);
rrfreelistptr(&nsrp);
unlock(&dnlock);
}
if(nsrp){
lock(&dnlock);
rrfreelistptr(&dbnsrp);
unlock(&dnlock);
if(netqueryns(qp, depth+1, nsrp) > Answnone)
return rrlookup(qp->dp, qp->type, OKneg);
} else if(dbnsrp)
if(netqueryns(qp, depth+1, dbnsrp) > Answnone)
return rrlookup(qp->dp, qp->type, NOneg);
}
return nil;
}
static RR*
dnresolve1(char *name, int class, int type, Request *req, int depth,
int recurse)
{
Area *area;
DN *dp;
RR *rp;
Query *qp;
if(debug)
dnslog("[%d] dnresolve1 %s %d %d", getpid(), name, type, class);
if(class != Cin)
return nil;
dp = dnlookup(name, class, 1);
rp = rrlookup(dp, type, OKneg);
if(rp)
if(rp->db){
if(rp->auth) {
noteinmem();
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: auth rr in db",
getpid(), name, type, class);
return rp;
}
} else
if(rp->ttl > now)
if(type != Tall || rp->query == Tall) {
noteinmem();
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: rr not in db",
getpid(), name, type, class);
return rp;
}
lock(&dnlock);
rrfreelist(rp);
unlock(&dnlock);
rp = nil;
USED(rp);
if(type != Tcname){
rp = rrlookup(dp, Tcname, NOneg);
lock(&dnlock);
rrfreelist(rp);
unlock(&dnlock);
if(rp){
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: rr from rrlookup for non-cname",
getpid(), name, type, class);
return nil;
}
}
area = inmyarea(dp->name);
if (area || strncmp(dp->name, "local#", 6) == 0) {
return nil;
}
qp = emalloc(sizeof *qp);
queryinit(qp, dp, type, req);
rp = issuequery(qp, name, class, depth, recurse);
querydestroy(qp);
free(qp);
if(rp){
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: rr from query",
getpid(), name, type, class);
return rp;
}
rp = rrlookup(dp, type, OKneg);
if(rp){
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: rr from rrlookup",
getpid(), name, type, class);
return rp;
}
rp = dblookup(name, class, type, 0, 0);
if (rp) {
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: rr from dblookup",
getpid(), name, type, class);
}else{
if(debug)
dnslog("[%d] dnresolve1 %s %d %d: no rr from dblookup; crapped out",
getpid(), name, type, class);
}
return rp;
}
char*
walkup(char *name)
{
char *cp;
cp = strchr(name, '.');
if(cp)
return cp+1;
else if(*name)
return "";
else
return 0;
}
static char *hmsg = "headers";
int
udpport(char *mtpt)
{
int fd, ctl;
char ds[64], adir[64];
snprint(ds, sizeof ds, "%s/udp!*!0", (mtpt? mtpt: "/net"));
ctl = announce(ds, adir);
if(ctl < 0){
return -1;
}
if(write(ctl, hmsg, strlen(hmsg)) != strlen(hmsg)){
close(ctl);
warning(hmsg);
return -1;
}
snprint(ds, sizeof ds, "%s/data", adir);
fd = open(ds, ORDWR);
close(ctl);
if(fd < 0)
warning("can't open udp port %s: %r", ds);
return fd;
}
void
initdnsmsg(DNSmsg *mp, RR *rp, int flags, ushort reqno)
{
mp->flags = flags;
mp->id = reqno;
mp->qd = rp;
if(rp != nil)
mp->qdcount = 1;
}
DNSmsg *
newdnsmsg(RR *rp, int flags, ushort reqno)
{
DNSmsg *mp;
mp = emalloc(sizeof *mp);
initdnsmsg(mp, rp, flags, reqno);
return mp;
}
int
mkreq(DN *dp, int type, uchar *buf, int flags, ushort reqno)
{
DNSmsg m;
int len;
Udphdr *uh = (Udphdr*)buf;
RR *rp;
memset(uh, 0, sizeof *uh);
hnputs(uh->rport, Dnsport);
memset(&m, 0, sizeof m);
rp = rralloc(type);
rp->owner = dp;
initdnsmsg(&m, rp, flags, reqno);
len = convDNS2M(&m, &buf[Udphdrsize], Maxdnspayload);
rrfreelistptr(&m.qd);
memset(&m, 0, sizeof m);
return len;
}
void
freeanswers(DNSmsg *mp)
{
lock(&dnlock);
rrfreelistptr(&mp->qd);
rrfreelistptr(&mp->an);
rrfreelistptr(&mp->ns);
rrfreelistptr(&mp->ar);
unlock(&dnlock);
mp->qdcount = mp->ancount = mp->nscount = mp->arcount = 0;
}
static int
readnet(Query *qp, int medium, uchar *ibuf, uvlong endms, uchar **replyp,
uchar *srcip)
{
int len, fd;
long ms;
vlong startns = nsec();
uchar *reply;
uchar lenbuf[2];
len = -1;
ms = endms - NS2MS(startns);
if (ms <= 0)
return -1;
reply = ibuf;
memset(srcip, 0, IPaddrlen);
alarm(ms);
if (medium == Udp)
if (qp->udpfd <= 0)
dnslog("readnet: qp->udpfd closed");
else {
len = read(qp->udpfd, ibuf, Udphdrsize+Maxpayload);
alarm(0);
notestats(startns, len < 0, qp->type);
if (len >= IPaddrlen)
memmove(srcip, ibuf, IPaddrlen);
if (len >= Udphdrsize) {
len -= Udphdrsize;
reply += Udphdrsize;
}
}
else {
if (!qp->tcpset)
dnslog("readnet: tcp params not set");
fd = qp->tcpfd;
if (fd <= 0)
dnslog("readnet: %s: tcp fd unset for dest %I",
qp->dp->name, qp->tcpip);
else if (readn(fd, lenbuf, 2) != 2) {
dnslog("readnet: short read of 2-byte tcp msg size from %I",
qp->tcpip);
notestats(startns, 1, qp->type);
} else {
len = lenbuf[0]<<8 | lenbuf[1];
if (readn(fd, ibuf, len) != len) {
dnslog("readnet: short read of tcp data from %I",
qp->tcpip);
notestats(startns, 1, qp->type);
len = -1;
}
}
memmove(srcip, qp->tcpip, IPaddrlen);
}
alarm(0);
*replyp = reply;
return len;
}
static int
readreply(Query *qp, int medium, ushort req, uchar *ibuf, DNSmsg *mp,
uvlong endms)
{
int len;
char *err;
char tbuf[32];
uchar *reply;
uchar srcip[IPaddrlen];
RR *rp;
queryck(qp);
memset(mp, 0, sizeof *mp);
memset(srcip, 0, sizeof srcip);
if (0)
len = -1;
for (; timems() < endms &&
(len = readnet(qp, medium, ibuf, endms, &reply, srcip)) >= 0;
freeanswers(mp)){
memset(mp, 0, sizeof *mp);
err = convM2DNS(reply, len, mp, nil);
if (mp->flags & Ftrunc) {
free(err);
freeanswers(mp);
return -1;
} else if(err){
dnslog("readreply: %s: input err, len %d: %s: %I",
qp->dp->name, len, err, srcip);
free(err);
continue;
}
if(debug)
logreply(qp->req->id, srcip, mp);
if(mp->id != req)
dnslog("%d: id %d instead of %d: %I", qp->req->id,
mp->id, req, srcip);
else if(mp->qd == 0)
dnslog("%d: no question RR: %I", qp->req->id, srcip);
else if(mp->qd->owner != qp->dp)
dnslog("%d: owner %s instead of %s: %I", qp->req->id,
mp->qd->owner->name, qp->dp->name, srcip);
else if(mp->qd->type != qp->type)
dnslog("%d: qp->type %d instead of %d: %I",
qp->req->id, mp->qd->type, qp->type, srcip);
else {
for(rp = mp->an; rp; rp = rp->next)
rp->query = qp->type;
return 0;
}
}
if (timems() >= endms) {
;
} else if (0) {
dnslog("readreply: %s type %s: ns %I read error or eof "
"(returned %d): %r", qp->dp->name, rrname(qp->type,
tbuf, sizeof tbuf), srcip, len);
if (medium == Udp)
for (rp = qp->nsrp; rp != nil; rp = rp->next)
if (rp->type == Tns)
dnslog("readreply: %s: query sent to "
"ns %s", qp->dp->name,
rp->host->name);
}
return -1;
}
int
contains(RR *rp1, RR *rp2)
{
RR *trp1, *trp2;
for(trp2 = rp2; trp2; trp2 = trp2->next){
for(trp1 = rp1; trp1; trp1 = trp1->next)
if(trp1->type == trp2->type)
if(trp1->host == trp2->host)
if(trp1->owner == trp2->owner)
break;
if(trp1 == nil)
return 0;
}
return 1;
}
int
ipisbm(uchar *ip)
{
if(isv4(ip)){
if (ip[IPv4off] >= 0xe0 && ip[IPv4off] < 0xf0 ||
ipcmp(ip, IPv4bcast) == 0)
return 4;
} else
if(ip[0] == 0xff)
return 6;
return 0;
}
static int
serveraddrs(Query *qp, int nd, int depth)
{
RR *rp, *arp, *trp;
Dest *cur;
if(nd >= Maxdest)
return Maxdest - 1;
arp = 0;
for(rp = qp->nsrp; rp; rp = rp->next){
assert(rp->magic == RRmagic);
if(rp->marker)
continue;
arp = rrlookup(rp->host, Ta, NOneg);
if(arp == nil)
arp = rrlookup(rp->host, Taaaa, NOneg);
if(arp){
rp->marker = 1;
break;
}
arp = dblookup(rp->host->name, Cin, Ta, 0, 0);
if(arp == nil)
arp = dblookup(rp->host->name, Cin, Taaaa, 0, 0);
if(arp){
rp->marker = 1;
break;
}
}
if(arp == 0)
for(rp = qp->nsrp; rp; rp = rp->next){
if(rp->marker)
continue;
rp->marker = 1;
if(subsume(rp->owner->name, rp->host->name))
continue;
arp = dnresolve(rp->host->name, Cin, Ta, qp->req, 0,
depth+1, Recurse, 1, 0);
if(arp == nil)
arp = dnresolve(rp->host->name, Cin, Taaaa,
qp->req, 0, depth+1, Recurse, 1, 0);
lock(&dnlock);
rrfreelist(rrremneg(&arp));
unlock(&dnlock);
if(arp)
break;
}
for(trp = arp; trp && nd < Maxdest; trp = trp->next){
cur = &qp->dest[nd];
parseip(cur->a, trp->ip->name);
if (ipisbm(cur->a) ||
cfg.straddle && !insideaddr(qp->dp->name) && insidens(cur->a))
continue;
cur->nx = 0;
cur->s = trp->owner;
cur->code = Rtimeout;
nd++;
}
lock(&dnlock);
rrfreelist(arp);
unlock(&dnlock);
if(nd >= Maxdest)
return Maxdest - 1;
return nd;
}
static void
cacheneg(DN *dp, int type, int rcode, RR *soarr)
{
RR *rp;
DN *soaowner;
ulong ttl;
stats.negcached++;
if(soarr != nil){
lock(&dnlock);
if(soarr->next != nil)
rrfreelistptr(&soarr->next);
unlock(&dnlock);
soaowner = soarr->owner;
} else
soaowner = nil;
if(soarr != nil && soarr->soa != nil)
ttl = soarr->soa->minttl+now;
else
ttl = 5*Min;
rrattach(soarr, Authoritative);
rp = rralloc(type);
rp->owner = dp;
rp->negative = 1;
rp->negsoaowner = soaowner;
rp->negrcode = rcode;
rp->ttl = ttl;
rrattach(rp, Authoritative);
}
static int
setdestoutns(Dest *p, int n)
{
uchar *outns = outsidens(n);
destck(p);
destinit(p);
if (outns == nil) {
if (n == 0)
dnslog("[%d] no outside-ns in ndb", getpid());
return -1;
}
memmove(p->a, outns, sizeof p->a);
p->s = dnlookup("outside-ns-ips", Cin, 1);
return 0;
}
static int
mydnsquery(Query *qp, int medium, uchar *udppkt, int len)
{
int rv = -1, nfd;
char *domain;
char conndir[NETPATHLEN], net[NETPATHLEN];
uchar belen[2];
NetConnInfo *nci;
queryck(qp);
domain = smprint("%I", udppkt);
if (myaddr(domain)) {
dnslog("mydnsquery: trying to send to myself (%s); bzzzt",
domain);
free(domain);
return rv;
}
switch (medium) {
case Udp:
free(domain);
nfd = dup(qp->udpfd, -1);
if (nfd < 0) {
warning("mydnsquery: qp->udpfd %d: %r", qp->udpfd);
close(qp->udpfd);
qp->udpfd = -1;
return rv;
}
close(nfd);
if (qp->udpfd <= 0)
dnslog("mydnsquery: qp->udpfd %d closed", qp->udpfd);
else {
if (write(qp->udpfd, udppkt, len+Udphdrsize) !=
len+Udphdrsize)
warning("sending udp msg: %r");
else {
stats.qsent++;
rv = 0;
}
}
break;
case Tcp:
snprint(net, sizeof net, "%s/tcp",
(mntpt[0] != '\0'? mntpt: "/net"));
alarm(10*1000);
qp->tcpfd = rv = dial(netmkaddr(domain, net, "dns"), nil,
conndir, &qp->tcpctlfd);
alarm(0);
if (qp->tcpfd < 0) {
dnslog("can't dial tcp!%s!dns: %r", domain);
free(domain);
break;
}
free(domain);
nci = getnetconninfo(conndir, qp->tcpfd);
if (nci) {
parseip(qp->tcpip, nci->rsys);
freenetconninfo(nci);
} else
dnslog("mydnsquery: getnetconninfo failed");
qp->tcpset = 1;
belen[0] = len >> 8;
belen[1] = len;
if (write(qp->tcpfd, belen, 2) != 2 ||
write(qp->tcpfd, udppkt + Udphdrsize, len) != len)
warning("sending tcp msg: %r");
break;
default:
sysfatal("mydnsquery: bad medium");
}
return rv;
}
static int
xmitquery(Query *qp, int medium, int depth, uchar *obuf, int inns, int len)
{
int j, n;
char buf[32];
Dest *p;
queryck(qp);
if(timems() >= qp->req->aborttime)
return -1;
p = qp->dest;
destck(p);
if (qp->ndest < 0 || qp->ndest > Maxdest) {
dnslog("qp->ndest %d out of range", qp->ndest);
abort();
}
if (qp->ndest > qp->curdest - p) {
j = serveraddrs(qp, qp->curdest - p, depth);
if (j < 0 || j >= Maxdest) {
dnslog("serveraddrs() result %d out of range", j);
abort();
}
qp->curdest = &qp->dest[j];
}
destck(qp->curdest);
if (qp->ndest == 0)
if (cfg.straddle && cfg.inside) {
qp->curdest = qp->dest;
for(n = 0; n < Maxdest; n++, qp->curdest++)
if (setdestoutns(qp->curdest, n) < 0)
break;
if(n == 0)
dnslog("xmitquery: %s: no outside-ns nameservers",
qp->dp->name);
} else
return -1;
j = 0;
if (medium == Tcp) {
j++;
queryck(qp);
assert(qp->dp);
procsetname("tcp %sside query for %s %s", (inns? "in": "out"),
qp->dp->name, rrname(qp->type, buf, sizeof buf));
mydnsquery(qp, medium, obuf, len);
if(debug)
logsend(qp->req->id, depth, qp->tcpip, "", qp->dp->name,
qp->type);
} else
for(; p < &qp->dest[qp->ndest] && p < qp->curdest; p++){
if(p->nx >= Maxtrans)
continue;
j++;
if((1<<p->nx) > qp->ndest)
continue;
if(memcmp(p->a, IPnoaddr, sizeof IPnoaddr) == 0)
continue;
procsetname("udp %sside query to %I/%s %s %s",
(inns? "in": "out"), p->a, p->s->name,
qp->dp->name, rrname(qp->type, buf, sizeof buf));
if(debug)
logsend(qp->req->id, depth, p->a, p->s->name,
qp->dp->name, qp->type);
memmove(obuf, p->a, sizeof p->a);
mydnsquery(qp, medium, obuf, len);
p->nx++;
}
if(j == 0) {
return -1;
}
return 0;
}
static int lckindex[Maxlcks] = {
0,
Ta,
Tns,
Tcname,
Tsoa,
Tptr,
Tmx,
Ttxt,
Taaaa,
};
static int
qtype2lck(int qtype)
{
int i;
for (i = 1; i < nelem(lckindex); i++)
if (lckindex[i] == qtype)
return i;
return 0;
}
static int
isnegrname(DNSmsg *mp)
{
return mp->an == nil && (mp->flags & Rmask) == Rname;
}
static int
procansw(Query *qp, DNSmsg *mp, uchar *srcip, int depth, Dest *p)
{
int rv;
char buf[32];
DN *ndp;
Query *nqp;
RR *tp, *soarr;
if (mp->an == nil)
stats.negans++;
if((mp->flags & Rmask) == Rserver){
stats.negserver++;
freeanswers(mp);
if(p != qp->curdest)
p->code = Rserver;
return Answerr;
}
if(mp->ns && baddelegation(mp->ns, qp->nsrp, srcip)){
stats.negbaddeleg++;
if(mp->an == nil){
stats.negbdnoans++;
freeanswers(mp);
if(p != qp->curdest)
p->code = Rserver;
dnslog(" and no answers");
return Answerr;
}
dnslog(" but has answers; ignoring ns");
lock(&dnlock);
rrfreelistptr(&mp->ns);
unlock(&dnlock);
mp->nscount = 0;
}
lock(&dnlock);
soarr = rrremtype(&mp->ns, Tsoa);
unique(mp->an);
unique(mp->ns);
unique(mp->ar);
unlock(&dnlock);
if(mp->an)
rrattach(mp->an, (mp->flags & Fauth) != 0);
if(mp->ar)
rrattach(mp->ar, Notauthoritative);
if(mp->ns && !cfg.justforw){
ndp = mp->ns->owner;
rrattach(mp->ns, Notauthoritative);
} else {
ndp = nil;
lock(&dnlock);
rrfreelistptr(&mp->ns);
unlock(&dnlock);
mp->nscount = 0;
}
if(mp->qd) {
lock(&dnlock);
rrfreelistptr(&mp->qd);
unlock(&dnlock);
mp->qdcount = 0;
}
if(mp->an != nil || (mp->flags & Fauth)){
if(isnegrname(mp))
qp->dp->respcode = Rname;
else
qp->dp->respcode = Rok;
if( mp->an == nil)
cacheneg(qp->dp, qp->type, (mp->flags & Rmask), soarr);
else {
lock(&dnlock);
rrfreelist(soarr);
unlock(&dnlock);
}
return 1;
} else if (isnegrname(mp)) {
qp->dp->respcode = Rname;
cacheneg(qp->dp, qp->type, (mp->flags & Rmask), soarr);
return 1;
}
stats.negnorname++;
lock(&dnlock);
rrfreelist(soarr);
unlock(&dnlock);
if(!mp->ns || cfg.resolver && cfg.justforw)
return Answnone;
tp = rrlookup(ndp, Tns, NOneg);
if(contains(qp->nsrp, tp)){
lock(&dnlock);
rrfreelist(tp);
unlock(&dnlock);
return Answnone;
}
procsetname("recursive query for %s %s", qp->dp->name,
rrname(qp->type, buf, sizeof buf));
nqp = emalloc(sizeof *nqp);
queryinit(nqp, qp->dp, qp->type, qp->req);
nqp->nsrp = tp;
rv = netquery(nqp, depth+1);
rrfreelist(nqp->nsrp);
querydestroy(nqp);
free(nqp);
return rv;
}
static int
tcpquery(Query *qp, DNSmsg *mp, int depth, uchar *ibuf, uchar *obuf, int len,
ulong waitms, int inns, ushort req)
{
int rv = 0;
uvlong endms;
endms = timems() + waitms;
if(endms > qp->req->aborttime)
endms = qp->req->aborttime;
if (0)
dnslog("%s: udp reply truncated; retrying query via tcp to %I",
qp->dp->name, qp->tcpip);
qlock(&qp->tcplock);
memmove(obuf, ibuf, IPaddrlen);
if (xmitquery(qp, Tcp, depth, obuf, inns, len) < 0 ||
readreply(qp, Tcp, req, ibuf, mp, endms) < 0)
rv = -1;
if (qp->tcpfd > 0) {
hangup(qp->tcpctlfd);
close(qp->tcpctlfd);
close(qp->tcpfd);
}
qp->tcpfd = qp->tcpctlfd = -1;
qunlock(&qp->tcplock);
return rv;
}
static int
queryns(Query *qp, int depth, uchar *ibuf, uchar *obuf, ulong waitms, int inns)
{
int ndest, len, replywaits, rv;
ushort req;
uvlong endms;
char buf[12];
uchar srcip[IPaddrlen];
Dest *p, *np, *dest;
req = rand();
len = mkreq(qp->dp, qp->type, obuf, Frecurse|Oquery, req);
queryck(qp);
dest = emalloc(Maxdest * sizeof *dest);
for (p = dest; p < dest + Maxdest; p++)
destinit(p);
free(qp->dest);
qp->curdest = qp->dest = dest;
for(ndest = 1; ndest < Maxdest; ndest++){
qp->ndest = ndest;
qp->tcpset = 0;
if (xmitquery(qp, Udp, depth, obuf, inns, len) < 0)
break;
endms = timems() + waitms;
if(endms > qp->req->aborttime)
endms = qp->req->aborttime;
for(replywaits = 0; replywaits < ndest; replywaits++){
DNSmsg m;
procsetname("reading %sside reply from %I: %s %s from %s",
(inns? "in": "out"), obuf, qp->dp->name,
rrname(qp->type, buf, sizeof buf), qp->req->from);
if (readreply(qp, Udp, req, ibuf, &m, endms) >= 0)
memmove(srcip, ibuf, IPaddrlen);
else if (!(m.flags & Ftrunc)) {
freeanswers(&m);
break;
} else {
freeanswers(&m);
rv = tcpquery(qp, &m, depth, ibuf, obuf, len,
waitms, inns, req);
if (rv < 0) {
freeanswers(&m);
break;
}
memmove(srcip, qp->tcpip, IPaddrlen);
}
for(p = qp->dest; p < qp->curdest; p++)
if(memcmp(p->a, srcip, sizeof p->a) == 0)
break;
for(np = qp->dest; np < qp->curdest; np++)
if(np->s == p->s)
np->nx = Maxtrans;
rv = procansw(qp, &m, srcip, depth, p);
if (rv > Answnone) {
free(qp->dest);
qp->dest = qp->curdest = nil;
return rv;
}
}
}
qp->dp->respcode = Rserver;
for(p = dest; p < qp->curdest; p++) {
destck(p);
if(p->code != Rserver)
qp->dp->respcode = Rok;
p->magic = 0;
}
free(qp->dest);
qp->dest = qp->curdest = nil;
return Answnone;
}
char *
system(int fd, char *cmd)
{
int pid, p, i;
static Waitmsg msg;
if((pid = fork()) == -1)
sysfatal("fork failed: %r");
else if(pid == 0){
dup(fd, 0);
close(fd);
for (i = 3; i < 200; i++)
close(i);
execl("/bin/rc", "rc", "-c", cmd, nil);
sysfatal("exec rc: %r");
}
for(p = waitpid(); p >= 0; p = waitpid())
if(p == pid)
return msg.msg;
return "lost child";
}
static ulong
weight(ulong ms, unsigned pcntprob)
{
ulong wait;
wait = (ms * pcntprob) / 100;
if (wait < Minwaitms)
wait = Minwaitms;
if (wait > Maxwaitms)
wait = Maxwaitms;
return wait;
}
static int
udpquery(Query *qp, char *mntpt, int depth, int patient, int inns)
{
int fd, rv;
ulong now, pcntprob;
uvlong wait, reqtm;
char *msg;
uchar *obuf, *ibuf;
static QLock mntlck;
static ulong lastmount;
ibuf = emalloc(64*1024);
obuf = emalloc(Maxpayload+Udphdrsize);
fd = udpport(mntpt);
while (fd < 0 && cfg.straddle && strcmp(mntpt, "/net.alt") == 0) {
now = time(nil);
if (now < lastmount + Remntretry)
sleep(S2MS(lastmount + Remntretry - now));
qlock(&mntlck);
fd = udpport(mntpt);
if (fd < 0) {
dnslog("[%d] remounting /net.alt", getpid());
unmount(nil, "/net.alt");
msg = system(open("/dev/null", ORDWR), "outside");
lastmount = time(nil);
if (msg && *msg) {
dnslog("[%d] can't remount /net.alt: %s",
getpid(), msg);
sleep(10*1000);
} else
fd = udpport(mntpt);
}
qunlock(&mntlck);
}
if (fd < 0) {
dnslog("can't get udpport for %s query of name %s: %r",
mntpt, qp->dp->name);
sysfatal("out of udp conversations");
}
if (qp->type >= nelem(likely))
pcntprob = 35;
else
pcntprob = likely[qp->type];
reqtm = (patient? 2 * Maxreqtm: Maxreqtm);
wait = weight(reqtm / 3, pcntprob);
qp->req->aborttime = timems() + 3*wait;
qp->udpfd = fd;
rv = queryns(qp, depth, ibuf, obuf, wait, inns);
close(fd);
qp->udpfd = -1;
free(obuf);
free(ibuf);
return rv;
}
static int
netquery(Query *qp, int depth)
{
int lock, rv, triedin, inname;
char buf[32];
RR *rp;
DN *dp;
Querylck *qlp;
static int whined;
rv = Answnone;
if(depth > 12)
return Answnone;
slave(qp->req);
lock = depth <= 1 && qp->req->isslave;
dp = qp->dp;
qlp = nil;
if(lock) {
procsetname("query lock wait: %s %s from %s", dp->name,
rrname(qp->type, buf, sizeof buf), qp->req->from);
qlp = &dp->querylck[qtype2lck(qp->type)];
qlock(qlp);
if (qlp->Ref.ref > Maxoutstanding) {
qunlock(qlp);
if (!whined) {
whined = 1;
dnslog("too many outstanding queries for %s;"
" dropping this one; no further logging"
" of drops", dp->name);
}
return 0;
}
++qlp->Ref.ref;
qunlock(qlp);
}
procsetname("netquery: %s", dp->name);
for(rp = qp->nsrp; rp; rp = rp->next)
rp->marker = 0;
triedin = 0;
inname = insideaddr(dp->name);
if (!cfg.straddle || inname) {
rv = udpquery(qp, mntpt, depth, Hurry, (cfg.inside? Inns: Outns));
triedin = 1;
}
if (rv == Answnone && cfg.inside && !inname) {
if (triedin)
dnslog(
"[%d] netquery: internal nameservers failed for %s; trying external",
getpid(), dp->name);
for(rp = qp->nsrp; rp; rp = rp->next)
rp->marker = 0;
rv = udpquery(qp, "/net.alt", depth, Patient, Outns);
}
if(lock && qlp) {
qlock(qlp);
assert(qlp->Ref.ref > 0);
qunlock(qlp);
decref(qlp);
}
return rv;
}
int
seerootns(void)
{
int rv;
char root[] = "";
Request req;
RR *rr;
Query *qp;
memset(&req, 0, sizeof req);
req.isslave = 1;
req.aborttime = timems() + Maxreqtm;
req.from = "internal";
qp = emalloc(sizeof *qp);
queryinit(qp, dnlookup(root, Cin, 1), Tns, &req);
qp->nsrp = dblookup(root, Cin, Tns, 0, 0);
for (rr = qp->nsrp; rr != nil; rr = rr->next)
dnslog("seerootns query nsrp: %R", rr);
rv = netquery(qp, 0);
rrfreelist(qp->nsrp);
querydestroy(qp);
free(qp);
return rv;
}