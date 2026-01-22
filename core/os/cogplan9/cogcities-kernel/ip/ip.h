typedef struct	Conv	Conv;
typedef struct	Fragment4 Fragment4;
typedef struct	Fragment6 Fragment6;
typedef struct	Fs	Fs;
typedef union	Hwaddr	Hwaddr;
typedef struct	IP	IP;
typedef struct	IPaux	IPaux;
typedef struct	Ip4hdr	Ip4hdr;
typedef struct	Ipfrag	Ipfrag;
typedef struct	Ipself	Ipself;
typedef struct	Ipselftab	Ipselftab;
typedef struct	Iplink	Iplink;
typedef struct	Iplifc	Iplifc;
typedef struct	Ipmulti	Ipmulti;
typedef struct	Ipifc	Ipifc;
typedef struct	Iphash	Iphash;
typedef struct	Ipht	Ipht;
typedef struct	Netlog	Netlog;
typedef struct	Medium	Medium;
typedef struct	Proto	Proto;
typedef struct	Arpent	Arpent;
typedef struct	Arp Arp;
typedef struct	Route	Route;
typedef struct	Routerparams	Routerparams;
typedef struct 	Hostparams	Hostparams;
typedef struct 	v6router	v6router;
typedef struct	v6params	v6params;
#pragma incomplete Arp
#pragma incomplete Ipself
#pragma incomplete Ipselftab
#pragma incomplete IP
#pragma incomplete Netlog
enum
{
Addrlen=	64,
Maxproto=	20,
Nhash=		64,
Maxincall=	10,
Nchans=		1024,
MAClen=		16,
MAXTTL=		255,
DFLTTOS=	0,
IPaddrlen=	16,
IPv4addrlen=	4,
IPv4off=	12,
IPllen=		4,
V4=		4,
V6=		6,
IP_VER4= 	0x40,
IP_VER6=	0x60,
IP_HLEN4=	5,
IP_DF=		0x4000,
IP_MF=		0x2000,
IP4HDR=		20,
IP_MAX=		64*1024,
Lroot=		10,
Maxpath =	64,
};
enum
{
Idle=		0,
Announcing=	1,
Announced=	2,
Connecting=	3,
Connected=	4,
};
enum
{
Forwarding,
DefaultTTL,
InReceives,
InHdrErrors,
InAddrErrors,
ForwDatagrams,
InUnknownProtos,
InDiscards,
InDelivers,
OutRequests,
OutDiscards,
OutNoRoutes,
ReasmTimeout,
ReasmReqds,
ReasmOKs,
ReasmFails,
FragOKs,
FragFails,
FragCreates,
Nipstats,
};
struct Fragment4
{
Block*	blist;
Fragment4*	next;
ulong 	src;
ulong 	dst;
ushort	id;
ulong 	age;
};
struct Fragment6
{
Block*	blist;
Fragment6*	next;
uchar 	src[IPaddrlen];
uchar 	dst[IPaddrlen];
uint	id;
ulong 	age;
};
struct Ipfrag
{
ushort	foff;
ushort	flen;
uchar	payload[];
};
#define IPFRAGSZ offsetof(Ipfrag, payload[0])
struct IP
{
uvlong		stats[Nipstats];
QLock		fraglock4;
Fragment4*	flisthead4;
Fragment4*	fragfree4;
Ref		id4;
QLock		fraglock6;
Fragment6*	flisthead6;
Fragment6*	fragfree6;
Ref		id6;
int		iprouting;
};
struct Ip4hdr
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
};
struct Conv
{
QLock;
int	x;
Proto*	p;
int	restricted;
uint	ttl;
uint	tos;
int	ignoreadvice;
uchar	ipversion;
uchar	laddr[IPaddrlen];
uchar	raddr[IPaddrlen];
ushort	lport;
ushort	rport;
char	*owner;
int	perm;
int	inuse;
int	length;
int	state;
int	maxfragsize;
int	headers;
int	reliable;
Conv*	incall;
Conv*	next;
Queue*	rq;
Queue*	wq;
Queue*	eq;
Queue*	sq;
Ref	snoopers;
QLock	car;
Rendez	cr;
char	cerr[ERRMAX];
QLock	listenq;
Rendez	listenr;
Ipmulti	*multi;
void*	ptcl;
Route	*r;
ulong	rgen;
};
struct Medium
{
char	*name;
int	hsize;
int	mintu;
int	maxtu;
int	maclen;
void	(*bind)(Ipifc*, int, char**);
void	(*unbind)(Ipifc*);
void	(*bwrite)(Ipifc *ifc, Block *b, int version, uchar *ip);
void	(*addmulti)(Ipifc *ifc, uchar *a, uchar *ia);
void	(*remmulti)(Ipifc *ifc, uchar *a, uchar *ia);
void	(*pktin)(Fs *f, Ipifc *ifc, Block *bp);
void	(*addroute)(Ipifc *ifc, int, uchar*, uchar*, uchar*, int);
void	(*remroute)(Ipifc *ifc, int, uchar*, uchar*);
void	(*flushroutes)(Ipifc *ifc);
void	(*joinmulti)(Ipifc *ifc, uchar *a, uchar *ia);
void	(*leavemulti)(Ipifc *ifc, uchar *a, uchar *ia);
void	(*ares)(Fs*, int, uchar*, uchar*, int, int);
void	(*areg)(Ipifc*, uchar*);
void	(*pref2addr)(uchar *pref, uchar *ea);
int	unbindonclose;
};
struct Iplifc
{
uchar	local[IPaddrlen];
uchar	mask[IPaddrlen];
uchar	remote[IPaddrlen];
uchar	net[IPaddrlen];
uchar	tentative;
uchar	onlink;
uchar	autoflag;
long 	validlt;
long 	preflt;
long	origint;
Iplink	*link;
Iplifc	*next;
};
struct Iplink
{
Ipself	*self;
Iplifc	*lifc;
Iplink	*selflink;
Iplink	*lifclink;
ulong	expire;
Iplink	*next;
int	ref;
};
struct Routerparams {
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
struct Hostparams {
int	rxmithost;
};
struct Ipifc
{
RWlock;
Conv	*conv;
char	dev[64];
Medium	*m;
int	maxtu;
int	mintu;
int	mbps;
void	*arg;
int	reassemble;
Lock	idlock;
uchar	ifcid;
int	ref;
Rendez	wait;
int	unbinding;
uchar	mac[MAClen];
Iplifc	*lifc;
ulong	in, out;
ulong	inerr, outerr;
uchar	sendra6;
uchar	recvra6;
Routerparams rp;
};
struct Ipmulti
{
uchar	ma[IPaddrlen];
uchar	ia[IPaddrlen];
Ipmulti	*next;
};
enum
{
Nipht=		521,
IPmatchexact=	0,
IPmatchany,
IPmatchport,
IPmatchaddr,
IPmatchpa,
};
struct Iphash
{
Iphash	*next;
Conv	*c;
int	match;
};
struct Ipht
{
Lock;
Iphash	*tab[Nipht];
};
void iphtadd(Ipht*, Conv*);
void iphtrem(Ipht*, Conv*);
Conv* iphtlook(Ipht *ht, uchar *sa, ushort sp, uchar *da, ushort dp);
struct Proto
{
QLock;
char*		name;
int		x;
int		ipproto;
char*		(*connect)(Conv*, char**, int);
char*		(*announce)(Conv*, char**, int);
char*		(*bind)(Conv*, char**, int);
int		(*state)(Conv*, char*, int);
void		(*create)(Conv*);
void		(*close)(Conv*);
void		(*rcv)(Proto*, Ipifc*, Block*);
char*		(*ctl)(Conv*, char**, int);
void		(*advise)(Proto*, Block*, char*);
int		(*stats)(Proto*, char*, int);
int		(*local)(Conv*, char*, int);
int		(*remote)(Conv*, char*, int);
int		(*inuse)(Conv*);
int		(*gc)(Proto*);
Fs		*f;
Conv		**conv;
int		ptclsize;
int		nc;
int		ac;
Qid		qid;
ushort		nextrport;
void		*priv;
};
struct Fs
{
RWlock;
int	dev;
int	np;
Proto*	p[Maxproto+1];
Proto*	t2p[256];
Proto*	ipifc;
Proto*	ipmux;
IP	*ip;
Ipselftab	*self;
Arp	*arp;
v6params	*v6p;
Route	*v4root[1<<Lroot];
Route	*v6root[1<<Lroot];
Route	*queue;
Netlog	*alog;
char	ndb[1024];
int	ndbvers;
long	ndbmtime;
};
struct v6router {
uchar	inuse;
Ipifc	*ifc;
int	ifcid;
uchar	routeraddr[IPaddrlen];
long	ltorigin;
Routerparams	rp;
};
struct v6params
{
Routerparams	rp;
Hostparams	hp;
v6router	v6rlist[3];
int		cdrouter;
};
int	Fsconnected(Conv*, char*);
Conv*	Fsnewcall(Conv*, uchar*, ushort, uchar*, ushort, uchar);
int	Fspcolstats(char*, int);
int	Fsproto(Fs*, Proto*);
int	Fsbuiltinproto(Fs*, uchar);
Conv*	Fsprotoclone(Proto*, char*);
Proto*	Fsrcvpcol(Fs*, uchar);
Proto*	Fsrcvpcolx(Fs*, uchar);
char*	Fsstdconnect(Conv*, char**, int);
char*	Fsstdannounce(Conv*, char**, int);
char*	Fsstdbind(Conv*, char**, int);
ulong	scalednconv(void);
void	closeconv(Conv*);
enum
{
Logip=		1<<1,
Logtcp=		1<<2,
Logfs=		1<<3,
Logicmp=	1<<5,
Logudp=		1<<6,
Logcompress=	1<<7,
Loggre=		1<<9,
Logppp=		1<<10,
Logtcprxmt=	1<<11,
Logigmp=	1<<12,
Logudpmsg=	1<<13,
Logipmsg=	1<<14,
Logrudp=	1<<15,
Logrudpmsg=	1<<16,
Logesp=		1<<17,
Logtcpwin=	1<<18,
};
void	netloginit(Fs*);
void	netlogopen(Fs*);
void	netlogclose(Fs*);
void	netlogctl(Fs*, char*, int);
long	netlogread(Fs*, void*, ulong, long);
void	netlog(Fs*, int, char*, ...);
void	ifcloginit(Fs*);
long	ifclogread(Fs*, Chan *,void*, ulong, long);
void	ifclog(Fs*, uchar *, int);
void	ifclogopen(Fs*, Chan*);
void	ifclogclose(Fs*, Chan*);
#pragma varargck argpos netlog	3
typedef	struct RouteTree RouteTree;
typedef struct Routewalk Routewalk;
typedef struct V4route V4route;
typedef struct V6route V6route;
enum
{
Rv4=		(1<<0),
Rifc=		(1<<1),
Rptpt=		(1<<2),
Runi=		(1<<3),
Rbcast=		(1<<4),
Rmulti=		(1<<5),
Rproxy=		(1<<6),
};
struct Routewalk
{
int	o;
int	h;
char*	p;
char*	e;
void*	state;
void	(*walk)(Route*, Routewalk*);
};
struct	RouteTree
{
Route*	right;
Route*	left;
Route*	mid;
uchar	depth;
uchar	type;
uchar	ifcid;
Ipifc	*ifc;
char	tag[4];
int	ref;
};
struct V4route
{
ulong	address;
ulong	endaddress;
uchar	gate[IPv4addrlen];
};
struct V6route
{
ulong	address[IPllen];
ulong	endaddress[IPllen];
uchar	gate[IPaddrlen];
};
struct Route
{
RouteTree;
union {
V6route	v6;
V4route v4;
};
};
extern void	v4addroute(Fs *f, char *tag, uchar *a, uchar *mask, uchar *gate, int type);
extern void	v6addroute(Fs *f, char *tag, uchar *a, uchar *mask, uchar *gate, int type);
extern void	v4delroute(Fs *f, uchar *a, uchar *mask, int dolock);
extern void	v6delroute(Fs *f, uchar *a, uchar *mask, int dolock);
extern Route*	v4lookup(Fs *f, uchar *a, Conv *c);
extern Route*	v6lookup(Fs *f, uchar *a, Conv *c);
extern long	routeread(Fs *f, char*, ulong, int);
extern long	routewrite(Fs *f, Chan*, char*, int);
extern void	routetype(int, char*);
extern void	ipwalkroutes(Fs*, Routewalk*);
extern void	convroute(Route*, uchar*, uchar*, uchar*, char*, int*);
struct IPaux
{
char	*owner;
char	tag[4];
};
extern IPaux*	newipaux(char*, char*);
struct Arpent
{
uchar	ip[IPaddrlen];
uchar	mac[MAClen];
Medium	*type;
Arpent*	hash;
Block*	hold;
Block*	last;
uint	ctime;
uint	utime;
uchar	state;
Arpent	*nextrxt;
uint	rtime;
uchar	rxtsrem;
Ipifc	*ifc;
uchar	ifcid;
};
extern void	arpinit(Fs*);
extern int	arpread(Arp*, char*, ulong, int);
extern int	arpwrite(Fs*, char*, int);
extern Arpent*	arpget(Arp*, Block *bp, int version, Ipifc *ifc, uchar *ip, uchar *h);
extern void	arprelease(Arp*, Arpent *a);
extern Block*	arpresolve(Arp*, Arpent *a, Medium *type, uchar *mac);
extern void	arpenter(Fs*, int version, uchar *ip, uchar *mac, int len, int norefresh);
extern int	myetheraddr(uchar*, char*);
extern vlong	parseip(uchar*, char*);
extern vlong	parseipmask(uchar*, char*);
extern char*	v4parseip(uchar*, char*);
extern void	maskip(uchar *from, uchar *mask, uchar *to);
extern int	parsemac(uchar *to, char *from, int len);
extern uchar*	defmask(uchar*);
extern int	isv4(uchar*);
extern void	v4tov6(uchar *v6, uchar *v4);
extern int	v6tov4(uchar *v4, uchar *v6);
extern int	eipfmt(Fmt*);
#define	ipmove(x, y) memmove(x, y, IPaddrlen)
#define	ipcmp(x, y) ( (x)[IPaddrlen-1] != (y)[IPaddrlen-1] || memcmp(x, y, IPaddrlen) )
extern uchar IPv4bcast[IPaddrlen];
extern uchar IPv4bcastobs[IPaddrlen];
extern uchar IPv4allsys[IPaddrlen];
extern uchar IPv4allrouter[IPaddrlen];
extern uchar IPnoaddr[IPaddrlen];
extern uchar v4prefix[IPaddrlen];
extern uchar IPallbits[IPaddrlen];
#define	NOW	TK2MS(MACHP(0)->ticks)
extern Medium	ethermedium;
extern Medium	nullmedium;
extern Medium	pktmedium;
extern Medium*	ipfindmedium(char *name);
extern void	addipmedium(Medium *med);
extern int	ipforme(Fs*, uchar *addr);
extern int	iptentative(Fs*, uchar *addr);
extern int	ipisbm(uchar *);
extern int	ipismulticast(uchar *);
extern Ipifc*	findipifc(Fs*, uchar *remote, int type);
extern void	findlocalip(Fs*, uchar *local, uchar *remote);
extern int	ipv4local(Ipifc *ifc, uchar *addr);
extern int	ipv6local(Ipifc *ifc, uchar *addr);
extern int	ipv6anylocal(Ipifc *ifc, uchar *addr);
extern Iplifc*	iplocalonifc(Ipifc *ifc, uchar *ip);
extern int	ipproxyifc(Fs *f, Ipifc *ifc, uchar *ip);
extern int	ipismulticast(uchar *ip);
extern int	ipisbooting(void);
extern int	ipifccheckin(Ipifc *ifc, Medium *med);
extern void	ipifccheckout(Ipifc *ifc);
extern int	ipifcgrab(Ipifc *ifc);
extern void	ipifcaddroute(Fs*, int, uchar*, uchar*, uchar*, int);
extern void	ipifcremroute(Fs*, int, uchar*, uchar*);
extern void	ipifcremmulti(Conv *c, uchar *ma, uchar *ia);
extern void	ipifcaddmulti(Conv *c, uchar *ma, uchar *ia);
extern char*	ipifcrem(Ipifc *ifc, char **argv, int argc);
extern char*	ipifcadd(Ipifc *ifc, char **argv, int argc, int tentative, Iplifc *lifcp);
extern long	ipselftabread(Fs*, char *a, ulong offset, int n);
extern char*	ipifcadd6(Ipifc *ifc, char**argv, int argc);
extern void	iprouting(Fs*, int);
extern void	icmpnoconv(Fs*, Block*);
extern void	icmpcantfrag(Fs*, Block*, int);
extern void	icmpttlexceeded(Fs*, uchar*, Block*);
extern ushort	ipcsum(uchar*);
extern void	ipiput4(Fs*, Ipifc*, Block*);
extern void	ipiput6(Fs*, Ipifc*, Block*);
extern int	ipoput4(Fs*, Block*, int, int, int, Conv*);
extern int	ipoput6(Fs*, Block*, int, int, int, Conv*);
extern int	ipstats(Fs*, char*, int);
extern ushort	ptclbsum(uchar*, int);
extern ushort	ptclcsum(Block*, int, int);
extern void	ip_init(Fs*);
extern void	update_mtucache(uchar*, ulong);
extern ulong	restrict_mtu(uchar*, ulong);
extern int	bootpread(char*, ulong, int);
char*		commonuser(void);
char*		commonerror(void);
extern Chan*	chandial(char*, char*, char*, Chan**);
extern void	(*igmpreportfn)(Ipifc*, uchar*);