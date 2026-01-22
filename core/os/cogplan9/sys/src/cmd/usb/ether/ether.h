typedef struct Ether Ether;
typedef struct Etherops Etherops;
typedef struct Conn Conn;
typedef struct Cinfo Cinfo;
typedef struct Buf Buf;
typedef struct Etherpkt Etherpkt;
enum
{
Cdc = 0,
A8817x,
A88178,
A88179,
A88772,
S95xx,
Eaddrlen = 6,
Epktlen = 1514,
Ehdrsize = 2*Eaddrlen + 2,
Maxpkt	= 2000,
Nconns	= 8,
Nbufs	= 32,
Scether = 6,
Fnheader = 0,
Fnunion = 6,
Fnether = 15,
Cdcunion	= 6,
};
struct Buf
{
int	type;
int	ndata;
uchar*	rp;
uchar	data[Hdrsize+Maxpkt];
};
struct Conn
{
Ref;
int	nb;
int	type;
int	headersonly;
int	prom;
Channel*rc;
};
struct Etherops
{
int	(*init)(Ether*, int *epin, int *epout);
long	(*bread)(Ether*, Buf*);
long	(*bwrite)(Ether*, Buf*);
int	(*ctl)(Ether*, char*);
int	(*promiscuous)(Ether*, int);
int	(*multicast)(Ether*, uchar*, int);
char*	(*seprintstats)(char*, char*, Ether*);
void	(*free)(Ether*);
int	bufsize;
char	*name;
void*	aux;
};
struct Ether
{
QLock;
QLock	wlck;
int	epinid;
int	epoutid;
Dev*	dev;
Dev*	epin;
Dev*	epout;
int	cid;
int	phy;
Ref	prom;
int	exiting;
int	wrexited;
uchar	addr[Eaddrlen];
int	nconns;
Conn*	conns[Nconns];
int	nabufs;
int	nbufs;
int	nblock;
long	nin;
long	nout;
long	nierrs;
long	noerrs;
int	mbps;
int	nmcasts;
Channel*rc;
Channel*wc;
Channel*bc;
Etherops;
Usbfs	fs;
};
struct Cinfo
{
int vid;
int did;
int cid;
};
struct Etherpkt
{
uchar d[Eaddrlen];
uchar s[Eaddrlen];
uchar type[2];
uchar data[1500];
};
int	ethermain(Dev *dev, int argc, char **argv);
int	asixreset(Ether*);
int smscreset(Ether*);
int	cdcreset(Ether*);
int	parseaddr(uchar *m, char *s);
void	dumpframe(char *tag, void *p, int n);
extern Cinfo cinfo[];
extern int etherdebug;
#define	deprint	if(etherdebug)fprint