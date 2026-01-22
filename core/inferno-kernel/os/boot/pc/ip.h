typedef struct Udphdr Udphdr;
struct Udphdr
{
uchar d[6];
uchar s[6];
uchar type[2];
uchar vihl;
uchar tos;
uchar length[2];
uchar id[2];
uchar frag[2];
uchar ttl;
uchar udpproto;
uchar udpplen[2];
uchar udpsrc[4];
uchar udpdst[4];
uchar udpsport[2];
uchar udpdport[2];
uchar udplen[2];
uchar udpcksum[2];
};
typedef struct Etherhdr Etherhdr;
struct Etherhdr
{
uchar d[6];
uchar s[6];
uchar type[2];
uchar vihl;
uchar tos;
uchar length[2];
uchar id[2];
uchar frag[2];
uchar ttl;
uchar proto;
uchar cksum[2];
uchar src[4];
uchar dst[4];
};
enum
{
IP_VER = 0x40,
IP_HLEN = 0x05,
UDP_EHSIZE = 22,
UDP_PHDRSIZE = 12,
UDP_HDRSIZE = 20,
ETHER_HDR = 14,
IP_UDPPROTO = 17,
ET_IP = 0x800,
Bcastip = 0xffffffff,
BPportsrc = 68,
BPportdst = 67,
TFTPport = 69,
Timeout = 5000,
Bootrequest = 1,
Bootreply = 2,
Tftp_READ = 1,
Tftp_WRITE = 2,
Tftp_DATA = 3,
Tftp_ACK = 4,
Tftp_ERROR = 5,
Segsize = 512,
TFTPSZ = Segsize+10,
};
typedef struct Bootp Bootp;
struct Bootp
{
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
char sname[64];
char file[128];
char vend[128];
};
typedef struct Netaddr Netaddr;
struct Netaddr
{
ulong ip;
ushort port;
char ea[Eaddrlen];
};
extern int eipfmt(Fmt*);