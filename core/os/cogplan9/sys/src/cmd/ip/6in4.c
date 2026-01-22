#include <u.h>
#include <libc.h>
#include <ip.h>
enum {
IP_IPV6PROTO	= 41,
IP_ESPPROTO	= 50,
IP_AHPROTO	= 51,
IP_ICMPV6PROTO	= 58,
V6to4pfx	= 0x2002,
};
typedef struct Iphdr Iphdr;
struct Iphdr
{
uchar	vihl;
uchar	tos;
uchar	length[2];
uchar	id[2];
uchar	frag[2];
uchar	ttl;
uchar	proto;
uchar	cksum[2];
uchar	src[4];
uchar	dst[4];
uchar	payload[];
};
#define STFHDR offsetof(Iphdr, payload[0])
int anysender;
int gateway;
int debug;
uchar local6[IPaddrlen];
uchar remote6[IPaddrlen];
uchar remote4[IPaddrlen];
uchar localmask[IPaddrlen];
uchar localnet[IPaddrlen];
uchar myip[IPaddrlen];
uchar anycast6to4[IPv4addrlen] = { 192, 88, 99, 1 };
static char *net = "/net";
static int	badipv4(uchar*);
static int	badipv6(uchar*);
static void	ip2tunnel(int, int);
static void	tunnel2ip(int, int);
static void
usage(void)
{
fprint(2, "usage: %s [-ag] [-x mtpt] [local6[/mask]] [remote4 [remote6]]\n",
argv0);
exits("Usage");
}
static char *
defv6addr(void)
{
uchar *ipv4 = &myip[IPaddrlen - IPv4addrlen];
return smprint("%ux:%2.2x%2.2x:%2.2x%2.2x::1/48", V6to4pfx,
ipv4[0], ipv4[1], ipv4[2], ipv4[3]);
}
static void
procargs(int argc, char **argv)
{
char *p, *loc6;
if (argc < 1)
loc6 = defv6addr();
else if (strcmp(argv[0], "-") == 0) {
loc6 = defv6addr();
argv++;
argc--;
} else {
loc6 = *argv++;
argc--;
}
memcpy(localmask, IPallbits, sizeof localmask);
p = strchr(loc6, '/');
if (p != nil) {
parseipmask(localmask, p);
*p = 0;
}
if (parseip(local6, loc6) == -1)
sysfatal("bad local v6 address %s", loc6);
if (isv4(local6))
usage();
if (argc >= 1 && argv[0][0] == '/') {
parseipmask(localmask, *argv++);
argc--;
}
if (debug)
fprint(2, "local6 %I %M\n", local6, localmask);
if (argc >= 1) {
if (parseip(remote4, *argv++) == -1)
sysfatal("bad remote v4 address %s", argv[-1]);
argc--;
if (!isv4(remote4))
usage();
} else {
v4tov6(remote4, anycast6to4);
anysender++;
}
if (debug)
fprint(2, "remote4 %I\n", remote4);
if (argc >= 1) {
if (parseip(remote6, *argv++) == -1)
sysfatal("bad remote v6 address %s", argv[-1]);
argc--;
} else {
remote6[0] = 0xFE;
remote6[1] = 0x80;
memcpy(remote6 + IPv4off, remote4 + IPv4off, IPv4addrlen);
}
USED(argv);
if (argc != 0)
usage();
maskip(local6, localmask, localnet);
if (debug)
fprint(2, "localnet %I remote6 %I\n", localnet, remote6);
}
static void
setup(int *v6net, int *tunp)
{
int n, cfd;
char *p, *cl, *ir;
char buf[128], path[64];
p = seprint(buf, buf + sizeof buf, "%s/ipmux!proto=%2.2x|%2.2x;dst=%V",
net, IP_IPV6PROTO, IP_ICMPV6PROTO, myip + IPv4off);
if (!anysender)
seprint(p, buf + sizeof buf, ";src=%V", remote4 + IPv4off);
*tunp = dial(buf, 0, 0, 0);
if (*tunp < 0)
sysfatal("can't access ipv6-in-ipv4 with dial str %s: %r", buf);
if (debug)
fprint(2, "dialed %s for v6-in-v4 access\n", buf);
cl = smprint("%s/ipifc/clone", net);
cfd = open(cl, ORDWR);
n = 0;
if (cfd < 0 || (n = read(cfd, buf, sizeof buf - 1)) <= 0)
sysfatal("can't make packet interface %s: %r", cl);
if (debug)
fprint(2, "cloned %s as local v6 interface\n", cl);
free(cl);
buf[n] = 0;
snprint(path, sizeof path, "%s/ipifc/%s/data", net, buf);
*v6net = open(path, ORDWR);
if (*v6net < 0 || fprint(cfd, "bind pkt") < 0)
sysfatal("can't bind packet interface: %r");
if (fprint(cfd, "add %I /128 %I 1280", local6, remote6) <= 0)
sysfatal("can't set local ipv6 address: %r");
close(cfd);
if (debug)
fprint(2, "opened & bound %s as local v6 interface\n", path);
if (gateway) {
ir = smprint("%s/iproute", net);
cfd = open(ir, OWRITE);
if (cfd >= 0 && debug)
fprint(2, "injected 2000::/3 %I into %s\n", remote6, ir);
free(ir);
if (cfd < 0 || fprint(cfd, "add 2000:: /3 %I", remote6) <= 0)
sysfatal("can't set default global route: %r");
}
}
static void
runtunnel(int v6net, int tunnel)
{
switch (rfork(RFPROC|RFNOWAIT|RFMEM|RFNOTEG)) {
case -1:
sysfatal("rfork");
default:
exits(nil);
case 0:
break;
}
switch (rfork(RFPROC|RFNOWAIT|RFMEM)) {
case -1:
sysfatal("rfork");
default:
tunnel2ip(tunnel, v6net);
break;
case 0:
ip2tunnel(v6net, tunnel);
break;
}
exits("tunnel gone");
}
void
main(int argc, char **argv)
{
int tunnel, v6net;
fmtinstall('I', eipfmt);
fmtinstall('V', eipfmt);
fmtinstall('M', eipfmt);
ARGBEGIN {
case 'a':
anysender++;
break;
case 'd':
debug++;
break;
case 'g':
gateway++;
break;
case 'x':
net = EARGF(usage());
break;
default:
usage();
} ARGEND
if (myipaddr(myip, net) < 0)
sysfatal("can't find my ipv4 address on %s", net);
if (!isv4(myip))
sysfatal("my ip, %I, is not a v4 address", myip);
procargs(argc, argv);
setup(&v6net, &tunnel);
runtunnel(v6net, tunnel);
exits(0);
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
static void
ip2tunnel(int in, int out)
{
int n, m;
char buf[64*1024];
Iphdr *op;
Ip6hdr *ip;
if (anysender)
procsetname("v6 %I -> tunnel", local6);
else
procsetname("v6 %I -> tunnel %I %I", local6, remote4, remote6);
op = (Iphdr*)buf;
op->vihl = IP_VER4 | 5;
memcpy(op->src, myip + IPv4off, sizeof op->src);
op->proto = IP_IPV6PROTO;
op->ttl = 100;
ip = (Ip6hdr*)(buf + STFHDR);
while ((n = read(in, ip, sizeof buf - STFHDR)) > 0) {
if ((ip->vcf[0] & 0xF0) != IP_VER6)
continue;
m = nhgets(ip->ploadlen) + IPV6HDR_LEN;
if (m > n)
continue;
if (m < n)
n = m;
if (badipv6(ip->src)) {
syslog(0, "6in4", "egress filtered %I -> %I; bad src",
ip->src, ip->dst);
continue;
}
if ((!equivip6(ip->dst, remote6) && badipv6(ip->dst))) {
syslog(0, "6in4", "egress filtered %I -> %I; "
"bad dst not remote", ip->src, ip->dst);
continue;
}
if (debug > 1)
fprint(2, "v6 to tunnel %I -> %I\n", ip->src, ip->dst);
if ((ip->dst[0]<<8 | ip->dst[1]) == V6to4pfx)
memcpy(op->dst, ip->dst+2, sizeof op->dst);
else
memcpy(op->dst, remote4+IPv4off, sizeof op->dst);
n += STFHDR;
if (write(out, op, n) != n) {
syslog(0, "6in4", "error writing to tunnel (%r), giving up");
break;
}
}
}
static void
tunnel2ip(int in, int out)
{
int n, m;
char buf[64*1024];
uchar a[IPaddrlen];
Ip6hdr *op;
Iphdr *ip;
if (anysender)
procsetname("tunnel -> v6 %I", local6);
else
procsetname("tunnel %I %I -> v6 %I", remote4, remote6, local6);
for (;;) {
n = read(in, buf, sizeof buf);
ip = (Iphdr*)(buf + IPaddrlen);
n -= IPaddrlen;
if (n <= 0) {
syslog(0, "6in4", "error reading from tunnel (%r), giving up");
break;
}
if ((ip->vihl & 0xF0) != IP_VER4 ||
ip->proto != IP_IPV6PROTO && ip->proto != IP_ICMPV6PROTO) {
syslog(0, "6in4",
"dropping pkt from tunnel with inner proto %d",
ip->proto);
continue;
}
m = nhgets(ip->length);
if (m > n)
continue;
if (m < n)
n = m;
op = (Ip6hdr*)(buf + IPaddrlen + STFHDR);
n -= STFHDR;
maskip(op->dst, localmask, a);
if (!equivip6(a, localnet)) {
syslog(0, "6in4", "ingress filtered %I -> %I; "
"dst not on local net", op->src, op->dst);
continue;
}
if (debug > 1)
fprint(2, "tunnel to v6 %I -> %I\n", op->src, op->dst);
if (write(out, op, n) != n) {
syslog(0, "6in4", "error writing to packet interface (%r), giving up");
break;
}
}
}
static int
badipv4(uchar *a)
{
switch (a[0]) {
case 0:
case 10:
case 127:
return 1;
case 172:
return a[1] >= 16;
case 192:
return a[1] == 168;
case 169:
return a[1] == 254;
}
return a[0] >= 240;
}
static int
badipv6(uchar *a)
{
int h = a[0]<<8 | a[1];
return h == 0 || ISIPV6MCAST(a) || ISIPV6LINKLOCAL(a) ||
h == V6to4pfx && badipv4(a+2);
}