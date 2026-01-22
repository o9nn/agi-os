#include <u.h>
#include <libc.h>
#include <ip.h>
#include "dns.h"
static RR*	doextquery(DNSmsg*, Request*, int);
static void	hint(RR**, RR*);
int	norecursion;
void
dnserver(DNSmsg *reqp, DNSmsg *repp, Request *req, uchar *srcip, int rcode)
{
int recursionflag;
char *cp, *errmsg;
char tname[32];
DN *nsdp, *dp;
Area *myarea;
RR *tp, *neg, *rp;
dncheck(nil, 1);
recursionflag = norecursion? 0: Fcanrec;
memset(repp, 0, sizeof(*repp));
repp->id = reqp->id;
repp->flags = Fresp | recursionflag | Oquery;
tp = reqp->qd;
reqp->qd = tp->next;
tp->next = nil;
repp->qd = tp;
if (rcode) {
errmsg = "";
if (rcode >= 0 && rcode < nrname)
errmsg = rname[rcode];
dnslog("server: response code 0%o (%s), req from %I",
rcode, errmsg, srcip);
repp->flags = (rcode&Rmask) | Fresp | Fcanrec | Oquery;
return;
}
if(!rrsupported(repp->qd->type)){
dnslog("server: unsupported request %s from %I",
rrname(repp->qd->type, tname, sizeof tname), srcip);
repp->flags = Runimplimented | Fresp | Fcanrec | Oquery;
return;
}
if(repp->qd->owner->class != Cin){
dnslog("server: unsupported class %d from %I",
repp->qd->owner->class, srcip);
repp->flags = Runimplimented | Fresp | Fcanrec | Oquery;
return;
}
myarea = inmyarea(repp->qd->owner->name);
if(myarea != nil) {
if(repp->qd->type == Tixfr || repp->qd->type == Taxfr){
dnslog("server: unsupported xfr request %s for %s from %I",
rrname(repp->qd->type, tname, sizeof tname),
repp->qd->owner->name, srcip);
repp->flags = Runimplimented | Fresp | recursionflag |
Oquery;
return;
}
} else
if(norecursion) {
repp->flags = Rok | Fresp | Oquery;
return;
}
if(reqp->flags & Frecurse)
neg = doextquery(repp, req, Recurse);
else
neg = doextquery(repp, req, Dontrecurse);
if(myarea != nil || (repp->an && repp->an->auth))
repp->flags |= Fauth;
if(repp->an == nil){
dp = dnlookup(repp->qd->owner->name, repp->qd->owner->class, 0);
if(dp->rr == nil)
if(reqp->flags & Frecurse)
repp->flags |= dp->respcode | Fauth;
}
if(myarea == nil)
for(cp = repp->qd->owner->name; cp; cp = walkup(cp)){
nsdp = dnlookup(cp, repp->qd->owner->class, 0);
if(nsdp == nil)
continue;
repp->ns = rrlookup(nsdp, Tns, OKneg);
if(repp->ns){
if(repp->ns->negative){
lock(&dnlock);
rp = repp->ns;
repp->ns = nil;
rrfreelist(rp);
unlock(&dnlock);
}
break;
}
if (strncmp(nsdp->name, "local#", 6) == 0)
dnslog("returning %s as nameserver", nsdp->name);
repp->ns = dblookup(cp, repp->qd->owner->class, Tns, 0, 0);
if(repp->ns)
break;
}
if(repp->qd->type != Taxfr && repp->qd->type != Tixfr){
for(tp = repp->ns; tp; tp = tp->next)
hint(&repp->ar, tp);
for(tp = repp->an; tp; tp = tp->next)
hint(&repp->ar, tp);
}
if(repp->an == nil)
if(myarea != nil){
lock(&dnlock);
rrcopy(myarea->soarr, &tp);
rrcat(&repp->ns, tp);
unlock(&dnlock);
} else if(neg != nil) {
if(neg->negsoaowner != nil) {
tp = rrlookup(neg->negsoaowner, Tsoa, NOneg);
lock(&dnlock);
rrcat(&repp->ns, tp);
unlock(&dnlock);
}
repp->flags |= neg->negrcode;
}
lock(&dnlock);
unique(repp->an);
unique(repp->ns);
unique(repp->ar);
rrfreelist(neg);
unlock(&dnlock);
dncheck(nil, 1);
}
static RR*
doextquery(DNSmsg *mp, Request *req, int recurse)
{
ushort type;
char *name;
RR *rp, *neg;
name = mp->qd->owner->name;
type = mp->qd->type;
rp = dnresolve(name, Cin, type, req, &mp->an, 0, recurse, 1, 0);
lock(&dnlock);
if(rp && rp->db && !rp->auth && rp->type == Tsoa) {
rrfreelist(rp);
rp = nil;
}
neg = rrremneg(&rp);
rrcat(&mp->an, rp);
unlock(&dnlock);
return neg;
}
static void
hint(RR **last, RR *rp)
{
RR *hp;
switch(rp->type){
case Tns:
case Tmx:
case Tmb:
case Tmf:
case Tmd:
assert(rp->host != nil);
hp = rrlookup(rp->host, Ta, NOneg);
if(hp == nil)
hp = dblookup(rp->host->name, Cin, Ta, 0, 0);
if(hp == nil)
hp = rrlookup(rp->host, Taaaa, NOneg);
if(hp == nil)
hp = dblookup(rp->host->name, Cin, Taaaa, 0, 0);
if (hp && hp->owner && hp->owner->name &&
strncmp(hp->owner->name, "local#", 6) == 0)
dnslog("returning %s as hint", hp->owner->name);
lock(&dnlock);
rrcat(last, hp);
unlock(&dnlock);
break;
}
}