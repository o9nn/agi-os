#include <u.h>
#include <libc.h>
#include <ip.h>
enum {
Mac0mcast	= 1<<0,
Mac0localadm	= 1<<1,
V60globaladm	= 1<<1,
};
static char *v4_6to4;
static void
usage(void)
{
fprint(2, "usage: %s [-t ipv4] ether...\n", argv0);
exits("usage");
}
void
ea2eui64(uchar *lla, uchar *ea)
{
*lla++ = *ea++ | V60globaladm;
*lla++ = *ea++;
*lla++ = *ea++;
*lla++ = 0xFF;
*lla++ = 0xFE;
*lla++ = *ea++;
*lla++ = *ea++;
*lla = *ea;
}
void
eaip26to4(uchar *lla, uchar *ea, uchar *ipv4)
{
*lla++ = 0x20;
*lla++ = 0x02;
memmove(lla, ipv4, IPv4addrlen);
lla += IPv4addrlen;
memset(lla, 0, 2);
ea2eui64(lla + 2, ea);
}
void
ea2lla(uchar *lla, uchar *ea)
{
*lla++ = 0xFE;
*lla++ = 0x80;
memset(lla, 0, 6);
ea2eui64(lla + 6, ea);
}
static void
process(char *ether)
{
uchar ethaddr[6], ipaddr[IPaddrlen], ipv4[IPv4addrlen];
if (parseether(ethaddr, ether) < 0)
sysfatal("%s: not an ether address", ether);
if (v4_6to4) {
v4parseip(ipv4, v4_6to4);
eaip26to4(ipaddr, ethaddr, ipv4);
} else
ea2lla(ipaddr, ethaddr);
print("%I\n", ipaddr);
}
void
main(int argc, char *argv[])
{
int i;
ARGBEGIN {
case 't':
v4_6to4 = EARGF(usage());
break;
default:
usage();
break;
} ARGEND
fmtinstall('I', eipfmt);
if (argc <= 0)
usage();
for (i = 0; i < argc; i++)
process(argv[i]);
exits(0);
}