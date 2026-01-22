#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "kernel.h"
#include "ip.h"
static ulong fsip;
static ulong auip;
static ulong gwip;
static ulong ipmask;
static ulong ipaddr;
static ulong dnsip;
enum
{
Bootrequest = 1,
Bootreply = 2,
};
typedef struct Bootp
{
uchar raddr[IPaddrlen];
uchar laddr[IPaddrlen];
uchar rport[2];
uchar lport[2];
uchar op;
uchar htype;
uchar hlen;
uchar hops;
uchar xid[4];
uchar secs[2];
uchar pad[2];
uchar ciaddr[4];
uchar yiaddr[4];
uchar siaddr[4];
uchar giaddr[4];
uchar chaddr[16];
uchar sname[64];
uchar file[128];
uchar vend[128];
} Bootp;
static Bootp req;
static Proc* rcvprocp;
static int recv;
static int done;
static Rendez bootpr;
static char rcvbuf[512];
static int bootpdebug;
static void
parsevend(uchar* vend)
{
if ((vend[0] != 99) || (vend[1] != 130) ||
(vend[2] != 83) || (vend[3] != 99)){
if(bootpdebug)
print("bad bootp vendor field: %.2x%.2x%.2x%.2x", vend[0], vend[1], vend[2], vend[3]);
return;
}
vend += 4;
while ((vend[0] != 0) && (vend[0] != 0xFF)) {
if(bootpdebug){
int i;
print("vend %d [%d]", vend[0], vend[1]);
for(i=0; i<vend[1]; i++)
print(" %2.2x", vend[i]);
print("\n");
}
switch (vend[0]) {
case 1:
if (vend[1] != 4)
return;
ipmask = (vend[2]<<24)|
(vend[3]<<16)|
(vend[4]<<8)|
vend[5];
break;
case 3:
if (vend[1] < 4)
break;
gwip = (vend[2]<<24)|
(vend[3]<<16)|
(vend[4]<<8)|
vend[5];
break;
case 6:
if (vend[1] < 4)
break;
dnsip = (vend[2]<<24)|
(vend[3]<<16)|
(vend[4]<<8)|
vend[5];
break;
case 8:
if (vend[1] < 4)
break;
auip = (vend[2]<<24)|
(vend[3]<<16)|
(vend[4]<<8)|
vend[5];
break;
case 11:
if (vend[1] < 4)
break;
fsip = (vend[2]<<24)|
(vend[3]<<16)|
(vend[4]<<8)|
vend[5];
break;
default:
break;
}
vend += vend[1] + 2;
}
}
static void
rcvbootp(void *a)
{
int n, fd;
Bootp *rp;
if(waserror())
pexit("", 0);
rcvprocp = up;
fd = (int)a;
while(done == 0) {
n = kread(fd, rcvbuf, sizeof(rcvbuf));
if(n <= 0)
break;
rp = (Bootp*)rcvbuf;
if (memcmp(req.chaddr, rp->chaddr, 6) == 0 &&
rp->htype == 1 && rp->hlen == 6) {
ipaddr = (rp->yiaddr[0]<<24)|
(rp->yiaddr[1]<<16)|
(rp->yiaddr[2]<<8)|
rp->yiaddr[3];
parsevend(rp->vend);
break;
}
}
poperror();
rcvprocp = nil;
recv = 1;
wakeup(&bootpr);
pexit("", 0);
}
static char*
rbootp(Ipifc *ifc)
{
int cfd, dfd, tries, n;
char ia[5+3*16], im[16], *av[3];
uchar nipaddr[4], ngwip[4], nipmask[4];
char dir[Maxpath];
static uchar vend_rfc1048[] = { 99, 130, 83, 99 };
av[1] = "0.0.0.0";
av[2] = "0.0.0.0";
ipifcadd(ifc, av, 3, 0, nil);
cfd = kannounce("udp!*!68", dir);
if(cfd < 0)
return "bootp announce failed";
strcat(dir, "/data");
if(kwrite(cfd, "headers", 7) < 0){
kclose(cfd);
return "bootp ctl headers failed";
}
kwrite(cfd, "oldheaders", 10);
dfd = kopen(dir, ORDWR);
if(dfd < 0){
kclose(cfd);
return "bootp open data failed";
}
kclose(cfd);
memset(&req, 0, sizeof(req));
ipmove(req.raddr, IPv4bcast);
hnputs(req.rport, 67);
req.op = Bootrequest;
req.htype = 1;
req.hlen = 6;
memmove(req.chaddr, ifc->mac, 6);
ipv4local(ifc, req.ciaddr);
memset(req.file, 0, sizeof(req.file));
memmove(req.vend, vend_rfc1048, 4);
done = 0;
recv = 0;
kproc("rcvbootp", rcvbootp, (void*)dfd, KPDUPFDG);
tries = 0;
while(recv == 0) {
if(kwrite(dfd, &req, sizeof(req)) < 0)
print("bootp: write: %r");
tsleep(&bootpr, return0, 0, 1000);
if(++tries > 10) {
print("bootp: timed out\n");
break;
}
}
kclose(dfd);
done = 1;
if(rcvprocp != nil){
postnote(rcvprocp, 1, "timeout", 0);
rcvprocp = nil;
}
av[1] = "0.0.0.0";
av[2] = "0.0.0.0";
ipifcrem(ifc, av, 3);
hnputl(nipaddr, ipaddr);
sprint(ia, "%V", nipaddr);
hnputl(nipmask, ipmask);
sprint(im, "%V", nipmask);
av[1] = ia;
av[2] = im;
ipifcadd(ifc, av, 3, 0, nil);
if(gwip != 0) {
hnputl(ngwip, gwip);
n = sprint(ia, "add 0.0.0.0 0.0.0.0 %V", ngwip);
routewrite(ifc->conv->p->f, nil, ia, n);
}
return nil;
}
static int
rbootpread(char *bp, ulong offset, int len)
{
int n;
char *buf;
uchar a[4];
buf = smalloc(READSTR);
if(waserror()){
free(buf);
nexterror();
}
hnputl(a, fsip);
n = snprint(buf, READSTR, "fsip %15V\n", a);
hnputl(a, auip);
n += snprint(buf + n, READSTR-n, "auip %15V\n", a);
hnputl(a, gwip);
n += snprint(buf + n, READSTR-n, "gwip %15V\n", a);
hnputl(a, ipmask);
n += snprint(buf + n, READSTR-n, "ipmask %15V\n", a);
hnputl(a, ipaddr);
n += snprint(buf + n, READSTR-n, "ipaddr %15V\n", a);
hnputl(a, dnsip);
snprint(buf + n, READSTR-n, "dnsip %15V\n", a);
len = readstr(offset, bp, len, buf);
poperror();
free(buf);
return len;
}
char* (*bootp)(Ipifc*) = rbootp;
int (*bootpread)(char*, ulong, int) = rbootpread;