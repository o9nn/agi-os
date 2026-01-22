#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "kernel.h"
#include "ip.h"
#include "ppp.h"
Ipaddr pppdns[2];
static ulong fsip;
static ulong auip;
static ulong gwip;
static ulong ipmask;
static ulong ipaddr;
static ulong dns1ip;
static ulong dns2ip;
int dhcpmsgtype;
int debug=0;
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
uchar flags[2];
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
static char rcvbuf[512+2*IPaddrlen+2*2];
static uchar sid[4];
static ulong iplease;
static int
parsevend(uchar* pvend)
{
uchar *vend=pvend;
int dhcpmsg=0;
if ((vend[0] != 99) || (vend[1] != 130) || (vend[2] != 83) || (vend[3] != 99)){
print("bad bootp vendor field: %.2x%.2x%.2x%.2x", vend[0], vend[1], vend[2], vend[3]);
return -1;
}
vend += 4;
while ((vend[0] != 0) && (vend[0] != 0xFF)) {
int i;
if(debug){
print(">>>Opt[%d] [%d]", vend[0], vend[1]);
for(i=0; i<vend[1]; i++)
print(" %2.2x", vend[i+2]);
print("\n");
}
switch (vend[0]) {
case 1:
if (vend[1] == 4)
ipmask = (vend[2]<<24)|(vend[3]<<16)| (vend[4]<<8)| vend[5];
else{
return -1;
}
break;
case 3:
if (vend[1] >0 && vend[1]%4==0)
gwip = (vend[2]<<24)|(vend[3]<<16)|(vend[4]<<8)|vend[5];
else
return -1;
break;
case 6:
if(vend[1]>0 && vend[1] %4==0){
dns1ip=(vend[2]<<24)|(vend[3]<<16)|(vend[4]<<8)|vend[5];
if(vend[1]>4)
dns2ip=(vend[6]<<24)|(vend[7]<<16)|(vend[8]<<8)|vend[9];
}else
return -1;
break;
case 8:
if (vend[1] > 0 && vend[1]%4==0)
auip = (vend[2]<<24)|(vend[3]<<16)|(vend[4]<<8)|vend[5];
else
return -1;
break;
case 11:
if (vend[1] > 0 && vend[1]%4==0)
fsip = (vend[2]<<24)| (vend[3]<<16)| (vend[4]<<8)| vend[5];
else
return -1;
break;
case 51:
if(vend[1]==4){
iplease=(vend[2]<<24)|(vend[3]<<16)|(vend[4]<<8)|vend[5];
}else
return -1;
break;
case 53:
if(vend[1]==1)
dhcpmsg=vend[2];
else
return -1;
break;
case 54:
if(vend[1]==4){
memmove(sid, vend+2, 4);
}else
return -1;
break;
default:
break;
}
vend += vend[1] + 2;
}
if(debug)
print(">>>Opt[%d] [%d]\n", vend[0], vend[1]);
return dhcpmsg;
}
static void
dispvend(uchar* pvend)
{
uchar *vend=pvend;
vend += 4;
while ((vend[0] != 0) && (vend[0] != 0xFF)) {
vend += vend[1] + 2;
}
}
static void
rcvbootp(void *a)
{
int n, fd, dhcp;
Bootp *rp;
if(waserror())
pexit("", 0);
rcvprocp = up;
fd = (int)a;
while(done == 0) {
if(debug)
print("rcvbootp:looping\n");
n = kread(fd, rcvbuf, sizeof(rcvbuf));
if(n <= 0)
break;
rp = (Bootp*)rcvbuf;
if (memcmp(req.chaddr, rp->chaddr, 6) == 0 && rp->htype == 1 && rp->hlen == 6) {
ipaddr = (rp->yiaddr[0]<<24)| (rp->yiaddr[1]<<16)| (rp->yiaddr[2]<<8)| rp->yiaddr[3];
if(debug)
print("ipaddr = %2.2x %2.2x %2.2x %2.2x \n", rp->yiaddr[0], rp->yiaddr[1], rp->yiaddr[2], rp->yiaddr[3]);
dhcp = parsevend(rp->vend);
if(dhcpmsgtype < dhcp){
dhcpmsgtype=dhcp;
recv = 1;
wakeup(&bootpr);
if(dhcp==0 || dhcp ==5 || dhcp == 6 )
break;
}
}
}
poperror();
rcvprocp = nil;
if(debug)
print("rcvbootp exit\n");
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
uchar *vend;
if(debug)
print("dhcp: bootp() called\n");
tries = 0;
av[1] = "0.0.0.0";
av[2] = "0.0.0.0";
ipifcadd(ifc, av, 3, 0, nil);
cfd = kannounce("udp!*!68", dir);
if(cfd < 0)
return "dhcp announce failed";
strcat(dir, "/data");
if(kwrite(cfd, "headers", 7) < 0){
kclose(cfd);
return "dhcp ctl headers failed";
}
kwrite(cfd, "oldheaders", 10);
dfd = kopen(dir, ORDWR);
if(dfd < 0){
kclose(cfd);
return "dhcp open data failed";
}
kclose(cfd);
while(tries<1){
tries++;
memset(sid, 0, 4);
iplease=0;
dhcpmsgtype=-2;
done = 0;
recv = 0;
kproc("rcvbootp", rcvbootp, (void*)dfd, KPDUPFDG);
memset(&req, 0, sizeof(req));
ipmove(req.raddr, IPv4bcast);
hnputs(req.rport, 67);
req.op = Bootrequest;
req.htype = 1;
req.hlen = 6;
memmove(req.chaddr, ifc->mac, 6);
memset(req.file, 0, sizeof(req.file));
vend=req.vend;
memmove(vend, vend_rfc1048, 4); vend+=4;
*vend++=53; *vend++=1;*vend++=1;
*vend++=61;*vend++=7;*vend++=1;
memmove(vend, ifc->mac, 6);vend+=6;
*vend=0xff;
if(debug)
dispvend(req.vend);
for(n=0;n<4;n++){
if(kwrite(dfd, &req, sizeof(req))<0)
print("DHCPDISCOVER: %r");
tsleep(&bootpr, return0, 0, 1000);
if(debug)
print("[DHCP] DISCOVER: msgtype = %d\n", dhcpmsgtype);
if(dhcpmsgtype==2)
break;
else if(dhcpmsgtype==0)
return nil;
else if(dhcpmsgtype== -2)
continue;
else
break;
}
if(dhcpmsgtype!=2)
continue;
memset(req.vend, 0, sizeof(req.vend));
vend=req.vend;
memmove(vend, vend_rfc1048, 4);vend+=4;
*vend++=53; *vend++=1;*vend++=3;
*vend++=50; *vend++=4;
*vend++=(ipaddr >> 24)&0xff;
*vend++=(ipaddr >> 16)&0xff;
*vend++=(ipaddr >> 8) & 0xff;
*vend++=ipaddr & 0xff;
*vend++=51;*vend++=4;
*vend++=(iplease>>24)&0xff; *vend++=(iplease>>16)&0xff; *vend++=(iplease>>8)&0xff; *vend++=iplease&0xff;
*vend++=54; *vend++=4;
memmove(vend, sid, 4); vend+=4;
*vend++=61;*vend++=07;*vend++=01;
memmove(vend, ifc->mac, 6);vend+=6;
*vend=0xff;
if(debug)
dispvend(req.vend);
if(kwrite(dfd, &req, sizeof(req))<0){
print("DHCPREQUEST: %r");
continue;
}
tsleep(&bootpr, return0, 0, 2000);
if(dhcpmsgtype==5)
break;
else
continue;
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
int n, i;
char *buf;
uchar a[4];
if(debug)
print("dhcp: bootpread() \n");
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
n += snprint(buf+n, READSTR-n, "expired %lud\n", iplease);
n += snprint(buf + n, READSTR-n, "dns");
if(dns2ip){
hnputl(a, dns2ip);
n+=snprint(buf + n, READSTR-n, " %15V", a);
}
if(dns1ip){
hnputl(a, dns1ip);
n += snprint(buf + n, READSTR-n, " %15V", a);
}
for(i=0; i<2; i++)
if(ipcmp(pppdns[i], IPnoaddr) != 0 && ipcmp(pppdns[i], v4prefix) != 0)
n += snprint(buf + n, READSTR-n, " %15I", pppdns[i]);
snprint(buf + n, READSTR-n, "\n");
len = readstr(offset, bp, len, buf);
poperror();
free(buf);
return len;
}
char* (*bootp)(Ipifc*) = rbootp;
int (*bootpread)(char*, ulong, int) = rbootpread;