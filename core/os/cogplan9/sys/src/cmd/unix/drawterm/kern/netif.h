typedef struct Etherpkt	Etherpkt;
typedef struct Netaddr	Netaddr;
typedef struct Netfile	Netfile;
typedef struct Netif	Netif;
enum
{
Nmaxaddr=	64,
Nmhash=		31,
Ncloneqid=	1,
Naddrqid,
N2ndqid,
N3rdqid,
Ndataqid,
Nctlqid,
Nstatqid,
Ntypeqid,
Nifstatqid,
};
#define NETTYPE(x)	(((ulong)x)&0x1f)
#define NETID(x)	((((ulong)x))>>5)
#define NETQID(i,t)	((((ulong)i)<<5)|(t))
struct Netfile
{
QLock lk;
int	inuse;
ulong	mode;
char	owner[KNAMELEN];
int	type;
int	prom;
int	scan;
int	bridge;
int	headersonly;
uchar	maddr[8];
int	nmaddr;
Queue	*in;
};
struct Netaddr
{
Netaddr	*next;
Netaddr	*hnext;
uchar	addr[Nmaxaddr];
int	ref;
};
struct Netif
{
QLock lk;
char	name[KNAMELEN];
int	nfile;
Netfile	**f;
int	limit;
int	alen;
int	mbps;
uchar	addr[Nmaxaddr];
uchar	bcast[Nmaxaddr];
Netaddr	*maddr;
int	nmaddr;
Netaddr *mhash[Nmhash];
int	prom;
int	scan;
int	all;
int	misses;
int	inpackets;
int	outpackets;
int	crcs;
int	oerrs;
int	frames;
int	overflows;
int	buffs;
int	soverflows;
void	*arg;
void	(*promiscuous)(void*, int);
void	(*multicast)(void*, uchar*, int);
void	(*scanbs)(void*, uint);
};
void	netifinit(Netif*, char*, int, ulong);
Walkqid*	netifwalk(Netif*, Chan*, Chan*, char **, int);
Chan*	netifopen(Netif*, Chan*, int);
void	netifclose(Netif*, Chan*);
long	netifread(Netif*, Chan*, void*, long, ulong);
Block*	netifbread(Netif*, Chan*, long, ulong);
long	netifwrite(Netif*, Chan*, void*, long);
int	netifwstat(Netif*, Chan*, uchar*, int);
int	netifstat(Netif*, Chan*, uchar*, int);
int	activemulti(Netif*, uchar*, int);
enum
{
Eaddrlen=	6,
ETHERMINTU =	60,
ETHERMAXTU =	1514,
ETHERHDRSIZE =	14,
};
struct Etherpkt
{
uchar	d[Eaddrlen];
uchar	s[Eaddrlen];
uchar	type[2];
uchar	data[1500];
};