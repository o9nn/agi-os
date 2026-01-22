#pragma	src	"/sys/src/libip"
#pragma	lib	"libip.a"
enum
{
IPaddrlen=	16,
IPv4addrlen=	4,
IPv4off=	12,
IPllen=		4,
IPV4HDR_LEN=	20,
IP_VER4= 	0x40,
IP_VER6=	0x60,
};
typedef struct Ipifc Ipifc;
typedef struct Iplifc Iplifc;
typedef struct Ipv6rp Ipv6rp;
struct Iplifc
{
Iplifc	*next;
uchar	ip[IPaddrlen];
uchar	mask[IPaddrlen];
uchar	net[IPaddrlen];
ulong	preflt;
ulong	validlt;
};
struct Ipv6rp
{
int	mflag;
int	oflag;
int 	maxraint;
int	minraint;
int	linkmtu;
int	reachtime;
int	rxmitra;
int	ttl;
int	routerlt;
};
struct Ipifc
{
Ipifc	*next;
Iplifc	*lifc;
int	index;
char	dev[64];
uchar	sendra6;
uchar	recvra6;
int	mtu;
ulong	pktin;
ulong	pktout;
ulong	errin;
ulong	errout;
Ipv6rp	rp;
};
#define ISIPV6MCAST(addr)	((addr)[0] == 0xff)
#define ISIPV6LINKLOCAL(addr) ((addr)[0] == 0xfe && ((addr)[1] & 0xc0) == 0x80)
enum {
IPV6HDR_LEN	= 40,
V6nd_srclladdr	= 1,
V6nd_targlladdr	= 2,
V6nd_pfxinfo	= 3,
V6nd_redirhdr	= 4,
V6nd_mtu	= 5,
V6nd_home	= 8,
V6nd_srcaddrs	= 9,
V6nd_ip		= 17,
V6nd_rdns	= 25,
V6nd_9fs	= 250,
V6nd_9auth	= 251,
Maxv6initraintvl= 16000,
Maxv6initras	= 3,
Maxv6finalras	= 3,
Minv6interradelay= 3000,
Maxv6radelay	= 500,
Maxv6rsdelay	= 1000,
V6rsintvl	= 4000,
Maxv6rss	= 3,
Maxv6mcastrss	= 3,
Maxv6unicastrss	= 3,
Maxv6anycastdelay= 1000,
Maxv6na		= 3,
V6reachabletime	= 30000,
V6retranstimer	= 1000,
V6initprobedelay= 5000,
};
typedef struct Ip6hdr Ip6hdr;
struct Ip6hdr {
uchar	vcf[4];
uchar	ploadlen[2];
uchar	proto;
uchar	ttl;
uchar	src[IPaddrlen];
uchar	dst[IPaddrlen];
uchar	payload[];
};
typedef struct Icmp6hdr Icmp6hdr;
struct Icmp6hdr {
uchar	_0_[8];
uchar	laddr[IPaddrlen];
uchar	raddr[IPaddrlen];
};
enum
{
Udphdrsize=	52,
};
typedef struct Udphdr Udphdr;
struct Udphdr
{
uchar	raddr[IPaddrlen];
uchar	laddr[IPaddrlen];
uchar	ifcaddr[IPaddrlen];
uchar	rport[2];
uchar	lport[2];
};
uchar*	defmask(uchar*);
void	maskip(uchar*, uchar*, uchar*);
int	eipfmt(Fmt*);
int	isv4(uchar*);
vlong	parseip(uchar*, char*);
vlong	parseipmask(uchar*, char*);
char*	v4parseip(uchar*, char*);
char*	v4parsecidr(uchar*, uchar*, char*);
int	parseether(uchar*, char*);
int	myipaddr(uchar*, char*);
int	myetheraddr(uchar*, char*);
int	equivip4(uchar*, uchar*);
int	equivip6(uchar*, uchar*);
Ipifc*	readipifc(char*, Ipifc*, int);
void	hnputv(void*, uvlong);
void	hnputl(void*, uint);
void	hnputs(void*, ushort);
uvlong	nhgetv(void*);
uint	nhgetl(void*);
ushort	nhgets(void*);
ushort	ptclbsum(uchar*, int);
int	v6tov4(uchar*, uchar*);
void	v4tov6(uchar*, uchar*);
#define	ipcmp(x, y) memcmp(x, y, IPaddrlen)
#define	ipmove(x, y) memmove(x, y, IPaddrlen)
extern uchar IPv4bcast[IPaddrlen];
extern uchar IPv4bcastobs[IPaddrlen];
extern uchar IPv4allsys[IPaddrlen];
extern uchar IPv4allrouter[IPaddrlen];
extern uchar IPnoaddr[IPaddrlen];
extern uchar v4prefix[IPaddrlen];
extern uchar IPallbits[IPaddrlen];
#define CLASS(p) ((*(uchar*)(p))>>6)
#pragma	varargck	type	"I"	uchar*
#pragma	varargck	type	"V"	uchar*
#pragma	varargck	type	"E"	uchar*
#pragma	varargck	type	"M"	uchar*