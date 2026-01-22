#include <u.h>
#include <libc.h>
#include <ip.h>
#include <bio.h>
#include <ndb.h>
#include "dns.h"
void
dnnotify(DNSmsg *reqp, DNSmsg *repp, Request *)
{
RR *tp;
Area *a;
memset(repp, 0, sizeof(*repp));
tp = reqp->qd;
reqp->qd = tp->next;
tp->next = 0;
repp->qd = tp;
repp->id = reqp->id;
repp->flags = Fresp  | Onotify | Fauth;
if(zonerefreshprogram == nil)
return;
if(repp->qd->type != Tsoa)
return;
dnslog("notification for %s", repp->qd->owner->name);
a = inmyarea(repp->qd->owner->name);
if(a == nil)
return;
dnslog("serial old %lud new %lud", a->soarr->soa->serial,
repp->qd->soa->serial);
if(a->soarr->soa->serial != repp->qd->soa->serial)
a->needrefresh = 1;
}
static void
send_notify(char *slave, RR *soa, Request *req)
{
int i, len, n, reqno, status, fd;
char *err;
uchar ibuf[Maxpayload+Udphdrsize], obuf[Maxpayload+Udphdrsize];
RR *rp;
Udphdr *up = (Udphdr*)obuf;
DNSmsg repmsg;
reqno = rand();
n = mkreq(soa->owner, Cin, obuf, Fauth | Onotify, reqno);
if(strcmp(ipattr(slave), "ip") == 0) {
if (parseip(up->raddr, slave) == -1)
dnslog("bad address %s to notify", slave);
} else {
rp = dnresolve(slave, Cin, Ta, req, nil, 0, 1, 1, &status);
if(rp == nil)
rp = dnresolve(slave, Cin, Taaaa, req, nil, 0, 1, 1, &status);
if(rp == nil)
return;
parseip(up->raddr, rp->ip->name);
rrfreelist(rp);
}
fd = udpport(nil);
if(fd < 0)
return;
n += Udphdrsize;
for(i = 0; i < 3; i++, freeanswers(&repmsg)){
dnslog("sending %d byte notify to %s/%I.%d about %s", n, slave,
up->raddr, nhgets(up->rport), soa->owner->name);
memset(&repmsg, 0, sizeof repmsg);
if(write(fd, obuf, n) != n)
break;
alarm(2*1000);
len = read(fd, ibuf, sizeof ibuf);
alarm(0);
if(len <= Udphdrsize)
continue;
err = convM2DNS(&ibuf[Udphdrsize], len, &repmsg, nil);
if(err != nil) {
free(err);
continue;
}
if(repmsg.id == reqno && (repmsg.flags & Omask) == Onotify)
break;
}
if (i < 3)
freeanswers(&repmsg);
close(fd);
}
static void
notify_areas(Area *a, Request *req)
{
Server *s;
for(; a != nil; a = a->next){
if(!a->neednotify)
continue;
for(s = a->soarr->soa->slaves; s != nil; s = s->next)
send_notify(s->name, a->soarr, req);
a->neednotify = 0;
}
}
void
notifyproc(void)
{
Request req;
switch(rfork(RFPROC|RFNOTEG|RFMEM|RFNOWAIT)){
case -1:
return;
case 0:
break;
default:
return;
}
procsetname("notify slaves");
memset(&req, 0, sizeof req);
req.isslave = 1;
for(;;){
getactivity(&req, 0);
notify_areas(owned, &req);
putactivity(0);
sleep(60*1000);
}
}