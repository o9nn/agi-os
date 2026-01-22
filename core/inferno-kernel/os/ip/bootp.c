#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "kernel.h"
#include "ip.h"
static	ulong	fsip;
static	ulong	auip;
static	ulong	gwip;
static	ulong	ipmask;
static	ulong	ipaddr;
enum
{
Bootrequest = 1,
Bootreply   = 2,
};
typedef struct Bootp
{
uchar	raddr[IPaddrlen];
uchar	laddr[IPaddrlen];
uchar	rport[2];
uchar	lport[2];
uchar	op;
uchar	htype;
uchar	hlen;
uchar	hops;
uchar	xid[4];
uchar	secs[2];
uchar	pad[2];
uchar	ciaddr[4];
uchar	yiaddr[4];
uchar	siaddr[4];
uchar	giaddr[4];
uchar	chaddr[16];
uchar	sname[64];
uchar	file[128];
uchar	vend[128];
} Bootp;
static	Bootp	req;
static	Proc*	rcvprocp;
static	int	recv;
static	int	done;
static	Rendez	bootpr;
static	char	rcvbuf[512+2*IPaddrlen+2*2];
static void
rcvbootp(void *a)
{
int n, fd;
Bootp *rp;
char *field[4];
uchar ip[IPaddrlen];
if(waserror())
pexit("", 0);
rcvprocp = up;
fd = (int)a;
while(done == 0) {
n = kread(fd, rcvbuf, sizeof(rcvbuf));
if(n <= 0)
break;
rp = (Bootp*)rcvbuf;
if(memcmp(req.chaddr, rp->chaddr, 6) == 0
&& rp->htype == 1 && rp->hlen == 6
&& getfields((char*)rp->vend+4, field, 4, 1, " ") == 4
&& strncmp((char*)rp->vend, "p9  ", 4) == 0){
if(ipaddr == 0)
ipaddr = nhgetl(rp->yiaddr);
if(ipmask == 0)
ipmask = parseip(ip, field[0]);
if(fsip == 0)
fsip = parseip(ip, field[1]);
if(auip == 0)
auip = parseip(ip, field[2]);
if(gwip == 0)
gwip = parseip(ip, field[3]);
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
char ia[5+3*24], im[16], *av[3];
uchar nipaddr[4], ngwip[4], nipmask[4];
char dir[Maxpath];
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
strcpy((char*)req.vend, "p9  ");
done = 0;
recv = 0;
kproc("rcvbootp", rcvbootp, (void*)dfd, KPDUPFDG);
tries = 0;
while(recv == 0) {
if(kwrite(dfd, &req, sizeof(req)) < 0)
print("bootp: write: %s\n", commonerror());
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
n = snprint(ia, sizeof(ia), "add 0.0.0.0 0.0.0.0 %V", ngwip);
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
snprint(buf + n, READSTR-n, "ipaddr %15V\n", a);
len = readstr(offset, bp, len, buf);
poperror();
free(buf);
return len;
}
char*	(*bootp)(Ipifc*) = rbootp;
int	(*bootpread)(char*, ulong, int) = rbootpread;