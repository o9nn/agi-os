#include <u.h>
#include <libc.h>
#include <ip.h>
#include <pool.h>
#include <ctype.h>
#include "dns.h"
enum {
Deftarget = 1<<30,
Minage = 1<<30,
Defagefreq = 1<<30,
Restartmins = 0,
};
DN *ht[HTLEN];
static struct {
Lock;
ulong names;
ulong oldest;
int active;
int mutex;
ushort id;
} dnvars;
char *rrtname[] =
{
[Ta] "ip",
[Tns] "ns",
[Tmd] "md",
[Tmf] "mf",
[Tcname] "cname",
[Tsoa] "soa",
[Tmb] "mb",
[Tmg] "mg",
[Tmr] "mr",
[Tnull] "null",
[Twks] "wks",
[Tptr] "ptr",
[Thinfo] "hinfo",
[Tminfo] "minfo",
[Tmx] "mx",
[Ttxt] "txt",
[Trp] "rp",
[Tafsdb] "afsdb",
[Tx25] "x.25",
[Tisdn] "isdn",
[Trt] "rt",
[Tnsap] "nsap",
[Tnsapptr] "nsap-ptr",
[Tsig] "sig",
[Tkey] "key",
[Tpx] "px",
[Tgpos] "gpos",
[Taaaa] "ipv6",
[Tloc] "loc",
[Tnxt] "nxt",
[Teid] "eid",
[Tnimloc] "nimrod",
[Tsrv] "srv",
[Tatma] "atma",
[Tnaptr] "naptr",
[Tkx] "kx",
[Tcert] "cert",
[Ta6] "a6",
[Tdname] "dname",
[Tsink] "sink",
[Topt] "opt",
[Tapl] "apl",
[Tds] "ds",
[Tsshfp] "sshfp",
[Tipseckey] "ipseckey",
[Trrsig] "rrsig",
[Tnsec] "nsec",
[Tdnskey] "dnskey",
[Tspf] "spf",
[Tuinfo] "uinfo",
[Tuid] "uid",
[Tgid] "gid",
[Tunspec] "unspec",
[Ttkey] "tkey",
[Ttsig] "tsig",
[Tixfr] "ixfr",
[Taxfr] "axfr",
[Tmailb] "mailb",
[Tmaila] "maila",
[Tall] "all",
0,
};
char *rname[Rmask+1] =
{
[Rok] "ok",
[Rformat] "format error",
[Rserver] "server failure",
[Rname] "bad name",
[Runimplimented] "unimplemented",
[Rrefused] "we don't like you",
[Ryxdomain] "name should not exist",
[Ryxrrset] "rr set should not exist",
[Rnxrrset] "rr set should exist",
[Rnotauth] "not authorative",
[Rnotzone] "not in zone",
[Rbadvers] "bad opt version",
[Rbadkey] "bad key",
[Rbadtime] "bad signature time",
[Rbadmode] "bad mode",
[Rbadname] "duplicate key name",
[Rbadalg] "bad algorithm",
};
unsigned nrname = nelem(rname);
char *opname[] =
{
[Oquery] "query",
[Oinverse] "inverse query (retired)",
[Ostatus] "status",
[Oupdate] "update",
};
ulong target = Deftarget;
ulong start;
Lock dnlock;
static ulong agefreq = Defagefreq;
static int rrequiv(RR *r1, RR *r2);
static int sencodefmt(Fmt*);
static void
ding(void*, char *msg)
{
if(strstr(msg, "alarm") != nil) {
stats.alarms++;
noted(NCONT);
} else
noted(NDFLT);
}
void
dninit(void)
{
fmtinstall('E', eipfmt);
fmtinstall('I', eipfmt);
fmtinstall('V', eipfmt);
fmtinstall('R', rrfmt);
fmtinstall('Q', rravfmt);
fmtinstall('H', sencodefmt);
dnvars.oldest = maxage;
dnvars.names = 0;
dnvars.id = truerand();
notify(ding);
}
static ulong
dnhash(char *name)
{
ulong hash;
uchar *val = (uchar*)name;
for(hash = 0; *val; val++)
hash = hash*13 + tolower(*val)-'a';
return hash % HTLEN;
}
DN*
dnlookup(char *name, int class, int enter)
{
DN **l;
DN *dp;
l = &ht[dnhash(name)];
lock(&dnlock);
for(dp = *l; dp; dp = dp->next) {
assert(dp->magic == DNmagic);
if(dp->class == class && cistrcmp(dp->name, name) == 0){
dp->referenced = now;
unlock(&dnlock);
return dp;
}
l = &dp->next;
}
if(!enter){
unlock(&dnlock);
return 0;
}
dnvars.names++;
dp = emalloc(sizeof(*dp));
dp->magic = DNmagic;
dp->name = estrdup(name);
assert(dp->name != nil);
dp->class = class;
dp->rr = 0;
dp->referenced = now;
dp->next = nil;
*l = dp;
unlock(&dnlock);
return dp;
}
static int
rrsame(RR *rr1, RR *rr2)
{
return rr1 == rr2 || rr2 && rrequiv(rr1, rr2) &&
rr1->db == rr2->db && rr1->auth == rr2->auth;
}
static int
rronlist(RR *rp, RR *lp)
{
for(; lp; lp = lp->next)
if (rrsame(lp, rp))
return 1;
return 0;
}
void
dnstats(char *file)
{
int i, fd;
fd = create(file, OWRITE, 0666);
if(fd < 0)
return;
qlock(&stats);
fprint(fd, "# system %s\n", sysname());
fprint(fd, "# slave procs high-water mark\t%lud\n", stats.slavehiwat);
fprint(fd, "# queries received by 9p\t%lud\n", stats.qrecvd9p);
fprint(fd, "# queries received by udp\t%lud\n", stats.qrecvdudp);
fprint(fd, "# queries answered from memory\t%lud\n", stats.answinmem);
fprint(fd, "# queries sent by udp\t%lud\n", stats.qsent);
for (i = 0; i < nelem(stats.under10ths); i++)
if (stats.under10ths[i] || i == nelem(stats.under10ths) - 1)
fprint(fd, "# responses arriving within %.1f s.\t%lud\n",
(double)(i+1)/10, stats.under10ths[i]);
fprint(fd, "\n# queries sent & timed-out\t%lud\n", stats.tmout);
fprint(fd, "# cname queries timed-out\t%lud\n", stats.tmoutcname);
fprint(fd, "# ipv6  queries timed-out\t%lud\n", stats.tmoutv6);
fprint(fd, "\n# negative answers received\t%lud\n", stats.negans);
fprint(fd, "# negative answers w Rserver set\t%lud\n", stats.negserver);
fprint(fd, "# negative answers w bad delegation\t%lud\n",
stats.negbaddeleg);
fprint(fd, "# negative answers w bad delegation & no answers\t%lud\n",
stats.negbdnoans);
fprint(fd, "# negative answers w no Rname set\t%lud\n", stats.negnorname);
fprint(fd, "# negative answers cached\t%lud\n", stats.negcached);
qunlock(&stats);
lock(&dnlock);
fprint(fd, "\n# domain names %lud target %lud\n", dnvars.names, target);
unlock(&dnlock);
close(fd);
}
void
dndump(char *file)
{
int i, fd;
DN *dp;
RR *rp;
fd = create(file, OWRITE, 0666);
if(fd < 0)
return;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next){
fprint(fd, "%s\n", dp->name);
for(rp = dp->rr; rp; rp = rp->next) {
fprint(fd, "\t%R %c%c %lud/%lud\n",
rp, rp->auth? 'A': 'U',
rp->db? 'D': 'N', rp->expire, rp->ttl);
if (rronlist(rp, rp->next))
fprint(fd, "*** duplicate:\n");
}
}
unlock(&dnlock);
close(fd);
}
void
dnpurge(void)
{
DN *dp;
RR *rp, *srp;
int i;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next){
srp = rp = dp->rr;
dp->rr = nil;
for(; rp != nil; rp = rp->next)
rp->cached = 0;
rrfreelist(srp);
}
unlock(&dnlock);
}
static void
rrdelhead(RR **l)
{
RR *rp;
if (canlock(&dnlock))
abort();
rp = *l;
if(rp == nil)
return;
*l = rp->next;
rp->cached = 0;
rrfree(rp);
}
void
dnage(DN *dp)
{
RR **l;
RR *rp, *next;
ulong diff;
if (canlock(&dnlock))
abort();
diff = now - dp->referenced;
if(diff < Reserved || dp->keep)
return;
l = &dp->rr;
for(rp = dp->rr; rp; rp = next){
assert(rp->magic == RRmagic);
assert(rp->cached);
next = rp->next;
if(!rp->db && (rp->expire < now || diff > dnvars.oldest))
rrdelhead(l);
else
l = &rp->next;
}
}
#define MARK(dp) { if (dp) (dp)->keep = 1; }
void
dnagenever(DN *dp, int dolock)
{
RR *rp;
if (dolock)
lock(&dnlock);
MARK(dp);
for(rp = dp->rr; rp; rp = rp->next){
MARK(rp->owner);
if(rp->negative){
MARK(rp->negsoaowner);
continue;
}
switch(rp->type){
case Thinfo:
MARK(rp->cpu);
MARK(rp->os);
break;
case Ttxt:
break;
case Tcname:
case Tmb:
case Tmd:
case Tmf:
case Tns:
case Tmx:
case Tsrv:
MARK(rp->host);
break;
case Tmg:
case Tmr:
MARK(rp->mb);
break;
case Tminfo:
MARK(rp->rmb);
MARK(rp->mb);
break;
case Trp:
MARK(rp->rmb);
MARK(rp->rp);
break;
case Ta:
case Taaaa:
MARK(rp->ip);
break;
case Tptr:
MARK(rp->ptr);
break;
case Tsoa:
MARK(rp->host);
MARK(rp->rmb);
break;
}
}
if (dolock)
unlock(&dnlock);
}
void
dnageallnever(void)
{
int i;
DN *dp;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next)
dnagenever(dp, 0);
unlock(&dnlock);
dnslog("%ld initial domain names; target is %ld", dnvars.names, target);
if(dnvars.names >= target)
dnslog("more initial domain names (%ld) than target (%ld)",
dnvars.names, target);
}
#define REF(dp) { if (dp) (dp)->refs++; }
void
dnageall(int doit)
{
DN *dp, **l;
int i;
RR *rp;
static ulong nextage;
if(dnvars.names < target || (now < nextage && !doit)){
dnvars.oldest = maxage;
return;
}
if(dnvars.names >= target) {
dnslog("more names (%lud) than target (%lud)", dnvars.names,
target);
dnvars.oldest /= 2;
if (dnvars.oldest < Minage)
dnvars.oldest = Minage;
}
if (agefreq > dnvars.oldest / 2)
nextage = now + dnvars.oldest / 2;
else
nextage = now + agefreq;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next){
dp->refs = 0;
dnage(dp);
}
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next)
for(rp = dp->rr; rp; rp = rp->next){
REF(rp->owner);
if(rp->negative){
REF(rp->negsoaowner);
continue;
}
switch(rp->type){
case Thinfo:
REF(rp->cpu);
REF(rp->os);
break;
case Ttxt:
break;
case Tcname:
case Tmb:
case Tmd:
case Tmf:
case Tns:
case Tmx:
case Tsrv:
REF(rp->host);
break;
case Tmg:
case Tmr:
REF(rp->mb);
break;
case Tminfo:
REF(rp->rmb);
REF(rp->mb);
break;
case Trp:
REF(rp->rmb);
REF(rp->rp);
break;
case Ta:
case Taaaa:
REF(rp->ip);
break;
case Tptr:
REF(rp->ptr);
break;
case Tsoa:
REF(rp->host);
REF(rp->rmb);
break;
}
}
for(i = 0; i < HTLEN; i++){
l = &ht[i];
for(dp = *l; dp; dp = *l){
if(dp->rr == 0 && dp->refs == 0 && !dp->keep){
assert(dp->magic == DNmagic);
*l = dp->next;
if(dp->name)
free(dp->name);
dp->magic = ~dp->magic;
dnvars.names--;
memset(dp, 0, sizeof *dp);
free(dp);
continue;
}
l = &dp->next;
}
}
unlock(&dnlock);
}
void
dnagedb(void)
{
DN *dp;
int i;
RR *rp;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next) {
dp->keep = 0;
for(rp = dp->rr; rp; rp = rp->next)
if(rp->db)
rp->expire = 0;
}
unlock(&dnlock);
}
void
dnauthdb(void)
{
int i;
ulong minttl;
Area *area;
DN *dp;
RR *rp;
lock(&dnlock);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next){
area = inmyarea(dp->name);
for(rp = dp->rr; rp; rp = rp->next)
if(rp->db){
if(area){
minttl = area->soarr->soa->minttl;
if(rp->ttl < minttl)
rp->ttl = minttl;
rp->auth = 1;
}
if(rp->expire == 0){
rp->db = 0;
dp->referenced = now-Reserved-1;
}
}
}
unlock(&dnlock);
}
int
getactivity(Request *req, int recursive)
{
int rv;
if(traceactivity)
dnslog("get: %d active by pid %d from %p",
dnvars.active, getpid(), getcallerpc(&req));
lock(&dnvars);
while(!recursive && dnvars.mutex){
unlock(&dnvars);
sleep(100);
lock(&dnvars);
}
rv = ++dnvars.active;
now = time(nil);
nowns = nsec();
req->id = ++dnvars.id;
unlock(&dnvars);
return rv;
}
void
putactivity(int recursive)
{
static ulong lastclean;
if(traceactivity)
dnslog("put: %d active by pid %d",
dnvars.active, getpid());
lock(&dnvars);
dnvars.active--;
assert(dnvars.active >= 0);
if (recursive || dnvars.mutex ||
(needrefresh == 0 && dnvars.active > 0)){
unlock(&dnvars);
return;
}
dnvars.mutex = 1;
while(dnvars.active > 0){
unlock(&dnvars);
sleep(100);
lock(&dnvars);
}
unlock(&dnvars);
db2cache(needrefresh);
if(start == 0)
start = time(nil);
if(Restartmins > 0 && time(nil) - start > Restartmins*60){
dnslog("killing all dns procs for timed restart");
postnote(PNGROUP, getpid(), "die");
dnvars.mutex = 0;
exits("restart");
}
dnageall(0);
lastclean = now;
needrefresh = 0;
dnvars.mutex = 0;
}
int
rrlistlen(RR *rp)
{
int n;
n = 0;
for(; rp; rp = rp->next)
++n;
return n;
}
static void
rrattach1(RR *new, int auth)
{
RR **l;
RR *rp;
DN *dp;
assert(new->magic == RRmagic);
assert(!new->cached);
if(!new->db) {
new->expire = new->ttl > now + Min? new->ttl: now + 10*Min;
} else
new->expire = now + Year;
dp = new->owner;
assert(dp->magic == DNmagic);
new->auth |= auth;
new->next = 0;
l = &dp->rr;
for(rp = *l; rp; rp = *l){
assert(rp->magic == RRmagic);
assert(rp->cached);
if(rp->type == new->type)
break;
l = &rp->next;
}
while ((rp = *l) != nil){
assert(rp->magic == RRmagic);
assert(rp->cached);
if(rp->type != new->type)
break;
if(rp->db == new->db && rp->auth == new->auth){
if(rp->negative != new->negative) {
rrdelhead(l);
continue;
}
else if(rp->arg0 == new->arg0 && rp->arg1 == new->arg1){
if (new->ttl <= rp->ttl &&
new->expire <= rp->expire) {
rrfree(new);
return;
}
rrdelhead(l);
continue;
}
else if(rp->type == Tptr &&
!rp->negative && !new->negative &&
rp->ptr->ordinal > new->ptr->ordinal)
break;
}
l = &rp->next;
}
if (rronlist(new, rp)) {
dnslog("adding duplicate %R to list of %R; aborting", new, rp);
abort();
}
new->cached = 1;
new->next = rp;
*l = new;
}
void
rrattach(RR *rp, int auth)
{
RR *next, *tp;
DN *dp;
lock(&dnlock);
for(; rp; rp = next){
next = rp->next;
rp->next = nil;
dp = rp->owner;
if(cfg.cachedb && !rp->db && inmyarea(rp->owner->name)
)
rrfree(rp);
else {
if (0 && rrlistlen(dp->rr) > 50 && !dp->keep) {
dnslog("rrattach(%s): rr list too long; "
"freeing it", dp->name);
tp = dp->rr;
dp->rr = nil;
rrfreelist(tp);
} else
USED(dp);
rrattach1(rp, auth);
}
}
unlock(&dnlock);
}
RR**
rrcopy(RR *rp, RR **last)
{
Cert *cert;
Key *key;
Null *null;
RR *nrp;
SOA *soa;
Sig *sig;
Txt *t, *nt, **l;
if (canlock(&dnlock))
abort();
nrp = rralloc(rp->type);
setmalloctag(nrp, getcallerpc(&rp));
switch(rp->type){
case Ttxt:
*nrp = *rp;
l = &nrp->txt;
*l = nil;
for(t = rp->txt; t != nil; t = t->next){
nt = emalloc(sizeof(*nt));
nt->p = estrdup(t->p);
nt->next = nil;
*l = nt;
l = &nt->next;
}
break;
case Tsoa:
soa = nrp->soa;
*nrp = *rp;
nrp->soa = soa;
*nrp->soa = *rp->soa;
nrp->soa->slaves = copyserverlist(rp->soa->slaves);
break;
case Tsrv:
*nrp = *rp;
nrp->srv = emalloc(sizeof *nrp->srv);
*nrp->srv = *rp->srv;
break;
case Tkey:
key = nrp->key;
*nrp = *rp;
nrp->key = key;
*key = *rp->key;
key->data = emalloc(key->dlen);
memmove(key->data, rp->key->data, rp->key->dlen);
break;
case Tsig:
sig = nrp->sig;
*nrp = *rp;
nrp->sig = sig;
*sig = *rp->sig;
sig->data = emalloc(sig->dlen);
memmove(sig->data, rp->sig->data, rp->sig->dlen);
break;
case Tcert:
cert = nrp->cert;
*nrp = *rp;
nrp->cert = cert;
*cert = *rp->cert;
cert->data = emalloc(cert->dlen);
memmove(cert->data, rp->cert->data, rp->cert->dlen);
break;
case Tnull:
null = nrp->null;
*nrp = *rp;
nrp->null = null;
*null = *rp->null;
null->data = emalloc(null->dlen);
memmove(null->data, rp->null->data, rp->null->dlen);
break;
default:
*nrp = *rp;
break;
}
nrp->cached = 0;
nrp->next = 0;
*last = nrp;
return &nrp->next;
}
RR*
rrlookup(DN *dp, int type, int flag)
{
RR *rp, *first, **last;
assert(dp->magic == DNmagic);
first = 0;
last = &first;
lock(&dnlock);
for(rp = dp->rr; rp; rp = rp->next){
assert(rp->magic == RRmagic);
assert(rp->cached);
if(rp->db)
if(rp->auth)
if(tsame(type, rp->type)) {
last = rrcopy(rp, last);
}
}
if(first)
goto out;
for(rp = dp->rr; rp; rp = rp->next){
if(!rp->db)
if(rp->auth)
if(rp->ttl + 60 > now)
if(tsame(type, rp->type)){
if(flag == NOneg && rp->negative)
goto out;
last = rrcopy(rp, last);
}
}
if(first)
goto out;
for(rp = dp->rr; rp; rp = rp->next){
if(!rp->db)
if(rp->ttl + 60 > now)
if(tsame(type, rp->type)){
if(flag == NOneg && rp->negative)
goto out;
last = rrcopy(rp, last);
}
}
if(first)
goto out;
for(rp = dp->rr; rp; rp = rp->next){
if(rp->db)
if(tsame(type, rp->type))
last = rrcopy(rp, last);
}
if(first)
goto out;
for(rp = dp->rr; rp; rp = rp->next)
if(tsame(type, rp->type)){
if(rp->negative)
goto out;
last = rrcopy(rp, last);
}
out:
unique(first);
unlock(&dnlock);
return first;
}
int
rrtype(char *atype)
{
int i;
for(i = 0; i <= Tall; i++)
if(rrtname[i] && strcmp(rrtname[i], atype) == 0)
return i;
if(strcmp(atype, "any") == 0)
return Tall;
else if(isascii(atype[0]) && isdigit(atype[0]))
return atoi(atype);
else
return -1;
}
int
rrsupported(int type)
{
if(type < 0 || type >Tall)
return 0;
return rrtname[type] != nil;
}
int
tsame(int t1, int t2)
{
return t1 == t2 || t1 == Tall;
}
RR*
rrcat(RR **start, RR *rp)
{
RR *olp, *nlp;
RR **last;
if (canlock(&dnlock))
abort();
for (olp = *start; 0 && olp; olp = olp->next)
for (nlp = rp; nlp; nlp = nlp->next)
if (rrsame(nlp, olp))
dnslog("rrcat: duplicate RR: %R", nlp);
USED(olp);
last = start;
while(*last != nil)
last = &(*last)->next;
*last = rp;
return *start;
}
RR*
rrremneg(RR **l)
{
RR **nl, *rp;
RR *first;
if (canlock(&dnlock))
abort();
first = nil;
nl = &first;
while(*l != nil){
rp = *l;
if(rp->negative){
*l = rp->next;
*nl = rp;
nl = &rp->next;
*nl = nil;
} else
l = &rp->next;
}
return first;
}
RR*
rrremtype(RR **l, int type)
{
RR *first, *rp;
RR **nl;
first = nil;
nl = &first;
while(*l != nil){
rp = *l;
if(rp->type == type){
*l = rp->next;
*nl = rp;
nl = &rp->next;
*nl = nil;
} else
l = &(*l)->next;
}
return first;
}
static char *
dnname(DN *dn)
{
return dn? dn->name: "<null>";
}
int
rrfmt(Fmt *f)
{
int rv;
char *strp;
char buf[Domlen];
Fmt fstr;
RR *rp;
Server *s;
SOA *soa;
Srv *srv;
Txt *t;
fmtstrinit(&fstr);
rp = va_arg(f->args, RR*);
if(rp == nil){
fmtprint(&fstr, "<null>");
goto out;
}
fmtprint(&fstr, "%s %s", dnname(rp->owner),
rrname(rp->type, buf, sizeof buf));
if(rp->negative){
fmtprint(&fstr, "\tnegative - rcode %d", rp->negrcode);
goto out;
}
switch(rp->type){
case Thinfo:
fmtprint(&fstr, "\t%s %s", dnname(rp->cpu), dnname(rp->os));
break;
case Tcname:
case Tmb:
case Tmd:
case Tmf:
case Tns:
fmtprint(&fstr, "\t%s", dnname(rp->host));
break;
case Tmg:
case Tmr:
fmtprint(&fstr, "\t%s", dnname(rp->mb));
break;
case Tminfo:
fmtprint(&fstr, "\t%s %s", dnname(rp->mb), dnname(rp->rmb));
break;
case Tmx:
fmtprint(&fstr, "\t%lud %s", rp->pref, dnname(rp->host));
break;
case Ta:
case Taaaa:
fmtprint(&fstr, "\t%s", dnname(rp->ip));
break;
case Tptr:
fmtprint(&fstr, "\t%s", dnname(rp->ptr));
break;
case Tsoa:
soa = rp->soa;
fmtprint(&fstr, "\t%s %s %lud %lud %lud %lud %lud",
dnname(rp->host), dnname(rp->rmb),
(soa? soa->serial: 0),
(soa? soa->refresh: 0), (soa? soa->retry: 0),
(soa? soa->expire: 0), (soa? soa->minttl: 0));
if (soa)
for(s = soa->slaves; s != nil; s = s->next)
fmtprint(&fstr, " %s", s->name);
break;
case Tsrv:
srv = rp->srv;
fmtprint(&fstr, "\t%ud %ud %ud %s",
(srv? srv->pri: 0), (srv? srv->weight: 0),
rp->port, dnname(rp->host));
break;
case Tnull:
if (rp->null == nil)
fmtprint(&fstr, "\t<null>");
else
fmtprint(&fstr, "\t%.*H", rp->null->dlen,
rp->null->data);
break;
case Ttxt:
fmtprint(&fstr, "\t");
for(t = rp->txt; t != nil; t = t->next)
fmtprint(&fstr, "%s", t->p);
break;
case Trp:
fmtprint(&fstr, "\t%s %s", dnname(rp->rmb), dnname(rp->rp));
break;
case Tkey:
if (rp->key == nil)
fmtprint(&fstr, "\t<null> <null> <null>");
else
fmtprint(&fstr, "\t%d %d %d", rp->key->flags,
rp->key->proto, rp->key->alg);
break;
case Tsig:
if (rp->sig == nil)
fmtprint(&fstr,
"\t<null> <null> <null> <null> <null> <null> <null> <null>");
else
fmtprint(&fstr, "\t%d %d %d %lud %lud %lud %d %s",
rp->sig->type, rp->sig->alg, rp->sig->labels,
rp->sig->ttl, rp->sig->exp, rp->sig->incep,
rp->sig->tag, dnname(rp->sig->signer));
break;
case Tcert:
if (rp->cert == nil)
fmtprint(&fstr, "\t<null> <null> <null>");
else
fmtprint(&fstr, "\t%d %d %d",
rp->cert->type, rp->cert->tag, rp->cert->alg);
break;
}
out:
strp = fmtstrflush(&fstr);
rv = fmtstrcpy(f, strp);
free(strp);
return rv;
}
int
rravfmt(Fmt *f)
{
int rv, quote;
char *strp;
Fmt fstr;
RR *rp;
Server *s;
SOA *soa;
Srv *srv;
Txt *t;
fmtstrinit(&fstr);
rp = va_arg(f->args, RR*);
if(rp == nil){
fmtprint(&fstr, "<null>");
goto out;
}
if(rp->type == Tptr)
fmtprint(&fstr, "ptr=%s", dnname(rp->owner));
else
fmtprint(&fstr, "dom=%s", dnname(rp->owner));
switch(rp->type){
case Thinfo:
fmtprint(&fstr, " cpu=%s os=%s",
dnname(rp->cpu), dnname(rp->os));
break;
case Tcname:
fmtprint(&fstr, " cname=%s", dnname(rp->host));
break;
case Tmb:
case Tmd:
case Tmf:
fmtprint(&fstr, " mbox=%s", dnname(rp->host));
break;
case Tns:
fmtprint(&fstr, " ns=%s", dnname(rp->host));
break;
case Tmg:
case Tmr:
fmtprint(&fstr, " mbox=%s", dnname(rp->mb));
break;
case Tminfo:
fmtprint(&fstr, " mbox=%s mbox=%s",
dnname(rp->mb), dnname(rp->rmb));
break;
case Tmx:
fmtprint(&fstr, " pref=%lud mx=%s", rp->pref, dnname(rp->host));
break;
case Ta:
case Taaaa:
fmtprint(&fstr, " ip=%s", dnname(rp->ip));
break;
case Tptr:
fmtprint(&fstr, " dom=%s", dnname(rp->ptr));
break;
case Tsoa:
soa = rp->soa;
fmtprint(&fstr,
" ns=%s mbox=%s serial=%lud refresh=%lud retry=%lud expire=%lud ttl=%lud",
dnname(rp->host), dnname(rp->rmb),
(soa? soa->serial: 0),
(soa? soa->refresh: 0), (soa? soa->retry: 0),
(soa? soa->expire: 0), (soa? soa->minttl: 0));
for(s = soa->slaves; s != nil; s = s->next)
fmtprint(&fstr, " dnsslave=%s", s->name);
break;
case Tsrv:
srv = rp->srv;
fmtprint(&fstr, " pri=%ud weight=%ud port=%ud target=%s",
(srv? srv->pri: 0), (srv? srv->weight: 0),
rp->port, dnname(rp->host));
break;
case Tnull:
if (rp->null == nil)
fmtprint(&fstr, " null=<null>");
else
fmtprint(&fstr, " null=%.*H", rp->null->dlen,
rp->null->data);
break;
case Ttxt:
fmtprint(&fstr, " txt=");
quote = 0;
for(t = rp->txt; t != nil; t = t->next)
if(strchr(t->p, ' '))
quote = 1;
if(quote)
fmtprint(&fstr, "\"");
for(t = rp->txt; t != nil; t = t->next)
fmtprint(&fstr, "%s", t->p);
if(quote)
fmtprint(&fstr, "\"");
break;
case Trp:
fmtprint(&fstr, " rp=%s txt=%s",
dnname(rp->rmb), dnname(rp->rp));
break;
case Tkey:
if (rp->key == nil)
fmtprint(&fstr, " flags=<null> proto=<null> alg=<null>");
else
fmtprint(&fstr, " flags=%d proto=%d alg=%d",
rp->key->flags, rp->key->proto, rp->key->alg);
break;
case Tsig:
if (rp->sig == nil)
fmtprint(&fstr,
" type=<null> alg=<null> labels=<null> ttl=<null> exp=<null> incep=<null> tag=<null> signer=<null>");
else
fmtprint(&fstr,
" type=%d alg=%d labels=%d ttl=%lud exp=%lud incep=%lud tag=%d signer=%s",
rp->sig->type, rp->sig->alg, rp->sig->labels,
rp->sig->ttl, rp->sig->exp, rp->sig->incep,
rp->sig->tag, dnname(rp->sig->signer));
break;
case Tcert:
if (rp->cert == nil)
fmtprint(&fstr, " type=<null> tag=<null> alg=<null>");
else
fmtprint(&fstr, " type=%d tag=%d alg=%d",
rp->cert->type, rp->cert->tag, rp->cert->alg);
break;
}
out:
strp = fmtstrflush(&fstr);
rv = fmtstrcpy(f, strp);
free(strp);
return rv;
}
void
warning(char *fmt, ...)
{
char dnserr[256];
va_list arg;
va_start(arg, fmt);
vseprint(dnserr, dnserr+sizeof(dnserr), fmt, arg);
va_end(arg);
syslog(1, logfile, dnserr);
}
void
dnslog(char *fmt, ...)
{
char dnserr[256];
va_list arg;
va_start(arg, fmt);
vseprint(dnserr, dnserr+sizeof(dnserr), fmt, arg);
va_end(arg);
syslog(0, logfile, dnserr);
}
void
procsetname(char *fmt, ...)
{
int fd;
char *cmdname;
char buf[128];
va_list arg;
va_start(arg, fmt);
cmdname = vsmprint(fmt, arg);
va_end(arg);
if (cmdname == nil)
return;
snprint(buf, sizeof buf, "#p/%d/args", getpid());
if((fd = open(buf, OWRITE)) >= 0){
write(fd, cmdname, strlen(cmdname)+1);
close(fd);
}
free(cmdname);
}
void
slave(Request *req)
{
int ppid, procs;
if(req->isslave)
return;
procs = getactivity(req, 1);
if (procs > stats.slavehiwat)
stats.slavehiwat = procs;
if(procs > Maxactive){
if(traceactivity)
dnslog("[%d] too much activity", getpid());
putactivity(1);
return;
}
ppid = getpid();
switch(rfork(RFPROC|RFMEM|RFNOWAIT)){
case -1:
putactivity(1);
break;
case 0:
procsetname("request slave of pid %d", ppid);
if(traceactivity)
dnslog("[%d] take activity from %d", getpid(), ppid);
req->isslave = 1;
break;
default:
alarm(0);
longjmp(req->mret, 1);
}
}
void
dncheck(void *p, int dolock)
{
int i;
DN *dp;
RR *rp;
if(p != nil){
dp = p;
assert(dp->magic == DNmagic);
}
if(!testing)
return;
if(dolock)
lock(&dnlock);
poolcheck(mainmem);
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next){
assert(dp != p);
assert(dp->magic == DNmagic);
for(rp = dp->rr; rp; rp = rp->next){
assert(rp->magic == RRmagic);
assert(rp->cached);
assert(rp->owner == dp);
if (dolock && rronlist(rp, rp->next)) {
dnslog("%R duplicates its next chain "
"(%R); aborting", rp, rp->next);
abort();
}
}
}
if(dolock)
unlock(&dnlock);
}
static int
rrequiv(RR *r1, RR *r2)
{
return r1->owner == r2->owner
&& r1->type == r2->type
&& r1->arg0 == r2->arg0
&& r1->arg1 == r2->arg1;
}
void
unique(RR *rp)
{
RR **l, *nrp;
for(; rp; rp = rp->next){
l = &rp->next;
for(nrp = *l; nrp; nrp = *l)
if(rrequiv(rp, nrp)){
*l = nrp->next;
rrfree(nrp);
} else
l = &nrp->next;
}
}
int
subsume(char *higher, char *lower)
{
int hn, ln;
ln = strlen(lower);
hn = strlen(higher);
if (ln < hn || cistrcmp(lower + ln - hn, higher) != 0 ||
ln > hn && hn != 0 && lower[ln - hn - 1] != '.')
return 0;
return 1;
}
RR*
randomize(RR *rp)
{
RR *first, *last, *x, *base;
ulong n;
if(rp == nil || rp->next == nil)
return rp;
for(x = rp; x; x = x->next)
if(x->type != Ta && x->type != Taaaa &&
x->type != Tmx && x->type != Tns)
return rp;
base = rp;
n = rand();
last = first = nil;
while(rp != nil){
if(base->auth != rp->auth || base->db != rp->db){
last->next = rp;
break;
}
x = rp;
rp = x->next;
x->next = nil;
if(n&1){
if(last == nil)
first = x;
else
last->next = x;
last = x;
} else {
if(last == nil)
last = x;
x->next = first;
first = x;
}
n >>= 1;
}
return first;
}
static int
sencodefmt(Fmt *f)
{
int i, len, ilen, rv;
char *out, *buf;
uchar *b;
char obuf[64];
if(!(f->flags&FmtPrec) || f->prec < 1)
goto error;
b = va_arg(f->args, uchar*);
if(b == nil)
goto error;
len = f->prec;
for(i = 0; i < len; i++)
if(!isprint(b[i]))
break;
if(i == len){
if(len >= sizeof obuf)
len = sizeof(obuf)-1;
memmove(obuf, b, len);
obuf[len] = 0;
fmtstrcpy(f, obuf);
return 0;
}
ilen = f->prec;
f->prec = 0;
f->flags &= ~FmtPrec;
switch(f->r){
case '<':
len = (8*ilen+4)/5 + 3;
break;
case '[':
len = (8*ilen+5)/6 + 4;
break;
case 'H':
len = 2*ilen + 1;
break;
default:
goto error;
}
if(len > sizeof(obuf)){
buf = malloc(len);
if(buf == nil)
goto error;
} else
buf = obuf;
out = buf;
switch(f->r){
case '<':
rv = enc32(out, len, b, ilen);
break;
case '[':
rv = enc64(out, len, b, ilen);
break;
case 'H':
rv = enc16(out, len, b, ilen);
break;
default:
rv = -1;
break;
}
if(rv < 0)
goto error;
fmtstrcpy(f, buf);
if(buf != obuf)
free(buf);
return 0;
error:
return fmtstrcpy(f, "<encodefmt>");
}
void*
emalloc(int size)
{
char *x;
x = mallocz(size, 1);
if(x == nil)
abort();
setmalloctag(x, getcallerpc(&size));
return x;
}
char*
estrdup(char *s)
{
int size;
char *p;
size = strlen(s)+1;
p = mallocz(size, 0);
if(p == nil)
abort();
memmove(p, s, size);
setmalloctag(p, getcallerpc(&s));
return p;
}
static RR*
mkptr(DN *dp, char *ptr, ulong ttl)
{
DN *ipdp;
RR *rp;
ipdp = dnlookup(ptr, Cin, 1);
rp = rralloc(Tptr);
rp->ptr = dp;
rp->owner = ipdp;
rp->db = 1;
if(ttl)
rp->ttl = ttl;
return rp;
}
void bytes2nibbles(uchar *nibbles, uchar *bytes, int nbytes);
void
dnptr(uchar *net, uchar *mask, char *dom, int forwtype, int subdoms, int ttl)
{
int i, j, len;
char *p, *e;
char ptr[Domlen];
uchar *ipp;
uchar ip[IPaddrlen], nnet[IPaddrlen];
uchar nibip[IPaddrlen*2];
DN *dp;
RR *rp, *nrp, *first, **l;
l = &first;
first = nil;
for(i = 0; i < HTLEN; i++)
for(dp = ht[i]; dp; dp = dp->next)
for(rp = dp->rr; rp; rp = rp->next){
if(rp->type != forwtype || rp->negative)
continue;
parseip(ip, rp->ip->name);
maskip(ip, mask, nnet);
if(ipcmp(net, nnet) != 0)
continue;
ipp = ip;
len = IPaddrlen;
if (forwtype == Taaaa) {
bytes2nibbles(nibip, ip, IPaddrlen);
ipp = nibip;
len = 2*IPaddrlen;
}
p = ptr;
e = ptr+sizeof(ptr);
for(j = len - 1; j >= len - subdoms; j--)
p = seprint(p, e, (forwtype == Ta?
"%d.": "%x."), ipp[j]);
seprint(p, e, "%s", dom);
nrp = mkptr(dp, ptr, ttl);
*l = nrp;
l = &nrp->next;
}
for(rp = first; rp != nil; rp = nrp){
nrp = rp->next;
rp->next = nil;
rrattach(rp, Authoritative);
}
}
void
addserver(Server **l, char *name)
{
Server *s;
while(*l)
l = &(*l)->next;
s = malloc(sizeof(Server)+strlen(name)+1);
if(s == nil)
return;
s->name = (char*)(s+1);
strcpy(s->name, name);
s->next = nil;
*l = s;
}
Server*
copyserverlist(Server *s)
{
Server *ns;
for(ns = nil; s != nil; s = s->next)
addserver(&ns, s->name);
return ns;
}
char*
rrname(int type, char *buf, int len)
{
char *t;
t = nil;
if(type >= 0 && type <= Tall)
t = rrtname[type];
if(t==nil){
snprint(buf, len, "%d", type);
t = buf;
}
return t;
}
void
rrfreelist(RR *rp)
{
RR *next;
for(; rp; rp = next){
next = rp->next;
rrfree(rp);
}
}
void
freeserverlist(Server *s)
{
Server *next;
for(; s != nil; s = next){
next = s->next;
free(s);
}
}
RR*
rralloc(int type)
{
RR *rp;
rp = emalloc(sizeof(*rp));
rp->magic = RRmagic;
rp->pc = getcallerpc(&type);
rp->type = type;
if (rp->type != type)
dnslog("rralloc: bogus type %d", type);
setmalloctag(rp, rp->pc);
switch(type){
case Tsoa:
rp->soa = emalloc(sizeof(*rp->soa));
rp->soa->slaves = nil;
setmalloctag(rp->soa, rp->pc);
break;
case Tsrv:
rp->srv = emalloc(sizeof(*rp->srv));
setmalloctag(rp->srv, rp->pc);
break;
case Tkey:
rp->key = emalloc(sizeof(*rp->key));
setmalloctag(rp->key, rp->pc);
break;
case Tcert:
rp->cert = emalloc(sizeof(*rp->cert));
setmalloctag(rp->cert, rp->pc);
break;
case Tsig:
rp->sig = emalloc(sizeof(*rp->sig));
setmalloctag(rp->sig, rp->pc);
break;
case Tnull:
rp->null = emalloc(sizeof(*rp->null));
setmalloctag(rp->null, rp->pc);
break;
}
rp->ttl = 0;
rp->expire = 0;
rp->next = 0;
return rp;
}
void
rrfree(RR *rp)
{
DN *dp;
RR *nrp;
Txt *t;
assert(rp->magic == RRmagic);
assert(!rp->cached);
dp = rp->owner;
if(dp){
if (canlock(&dnlock)) {
assert(dp->magic == DNmagic);
for(nrp = dp->rr; nrp; nrp = nrp->next)
assert(nrp != rp);
unlock(&dnlock);
}
}
switch(rp->type){
case Tsoa:
freeserverlist(rp->soa->slaves);
memset(rp->soa, 0, sizeof *rp->soa);
free(rp->soa);
break;
case Tsrv:
memset(rp->srv, 0, sizeof *rp->srv);
free(rp->srv);
break;
case Tkey:
free(rp->key->data);
memset(rp->key, 0, sizeof *rp->key);
free(rp->key);
break;
case Tcert:
free(rp->cert->data);
memset(rp->cert, 0, sizeof *rp->cert);
free(rp->cert);
break;
case Tsig:
free(rp->sig->data);
memset(rp->sig, 0, sizeof *rp->sig);
free(rp->sig);
break;
case Tnull:
free(rp->null->data);
memset(rp->null, 0, sizeof *rp->null);
free(rp->null);
break;
case Ttxt:
while(rp->txt != nil){
t = rp->txt;
rp->txt = t->next;
free(t->p);
memset(t, 0, sizeof *t);
free(t);
}
break;
}
rp->magic = ~rp->magic;
memset(rp, 0, sizeof *rp);
free(rp);
}