enum
{
ETHER_HDR = 14,
ET_IP = 0x800,
IP_VER = 0x40,
IP_HLEN = 0x05,
IP_UDPPROTO = 17,
UDP_EHSIZE = 22,
UDP_PHDRSIZE = 12,
UDP_HDRSIZE = 20,
BPportsrc = 68,
BPportdst = 67,
Bootrequest = 1,
Bootreply = 2,
TFTPport = 69,
Timeout = 2000,
Tftp_READ = 1,
Tftp_WRITE = 2,
Tftp_DATA = 3,
Tftp_ACK = 4,
Tftp_ERROR = 5,
Tftp_OACK = 6,
Defsegsize = 512,
Maxhwlen= 16,
Maxfilelen= 128,
Maxoptlen= 312-4,
OBend= 255,
OBpad= 0,
OBmask= 1,
};
enum
{
Udphdrsize= 52,
};
typedef struct Udphdr Udphdr;
struct Udphdr
{
uchar raddr[IPaddrlen];
uchar laddr[IPaddrlen];
uchar ifcaddr[IPaddrlen];
uchar rport[2];
uchar lport[2];
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
uchar flags[2];
uchar ciaddr[4];
uchar yiaddr[4];
uchar siaddr[4];
uchar giaddr[4];
uchar chaddr[16];
char sname[64];
char file[128];
uchar optmagic[4];
uchar optdata[Maxoptlen];
};
typedef struct Pxenetaddr Pxenetaddr;
struct Pxenetaddr
{
uchar ip[IPaddrlen];
ushort port;
};
extern int chatty;