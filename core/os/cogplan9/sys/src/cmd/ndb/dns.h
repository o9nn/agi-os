#include <thread.h>
#define NS2MS(ns) ((ns) / 1000000L)
#define S2MS(s)   ((s)  * 1000LL)
#define timems()	NS2MS(nsec())
typedef struct Ndbtuple Ndbtuple;
enum
{
Ta=		1,
Tns=		2,
Tmd=		3,
Tmf=		4,
Tcname=		5,
Tsoa=		6,
Tmb=		7,
Tmg=		8,
Tmr=		9,
Tnull=		10,
Twks=		11,
Tptr=		12,
Thinfo=		13,
Tminfo=		14,
Tmx=		15,
Ttxt=		16,
Trp=		17,
Tafsdb=		18,
Tx25=		19,
Tisdn=		20,
Trt=		21,
Tnsap=		22,
Tnsapptr=	23,
Tsig=		24,
Tkey=		25,
Tpx=		26,
Tgpos=		27,
Taaaa=		28,
Tloc=		29,
Tnxt=		30,
Teid=		31,
Tnimloc=	32,
Tsrv=		33,
Tatma=		34,
Tnaptr=		35,
Tkx=		36,
Tcert=		37,
Ta6=		38,
Tdname=		39,
Tsink=		40,
Topt=		41,
Tapl=		42,
Tds=		43,
Tsshfp=		44,
Tipseckey=	45,
Trrsig=		46,
Tnsec=		47,
Tdnskey=	48,
Tspf=		99,
Tuinfo=		100,
Tuid=		101,
Tgid=		102,
Tunspec=	103,
Ttkey=	249,
Ttsig=	250,
Tixfr=	251,
Taxfr=	252,
Tmailb=	253,
Tmaila= 254,
Tall=	255,
Csym=	0,
Cin=	1,
Ccs,
Cch,
Chs,
Call=	255,
Oquery=		0<<11,
Oinverse=	1<<11,
Ostatus=	2<<11,
Onotify=	4<<11,
Oupdate=	5<<11,
Omask=		0xf<<11,
Rok=		0,
Rformat=	1,
Rserver=	2,
Rname=		3,
Runimplimented=	4,
Rrefused=	5,
Ryxdomain=	6,
Ryxrrset=	7,
Rnxrrset=	8,
Rnotauth=	9,
Rnotzone=	10,
Rbadvers=	16,
Rbadkey=	17,
Rbadtime=	18,
Rbadmode=	19,
Rbadname=	20,
Rbadalg=	21,
Rmask=		0x1f,
Rtimeout=	1<<5,
Fresp=		1<<15,
Fauth=		1<<10,
Ftrunc=		1<<9,
Frecurse=	1<<8,
Fcanrec=	1<<7,
Domlen=		256,
Labellen=	64,
Strlen=		256,
Min=		60,
Hour=		60*Min,
Day=		24*Hour,
Week=		7*Day,
Year=		52*Week,
DEFTTL=		Day,
Reserved=	5*Min,
Dnsport=	53,
Maxdnspayload=	512,
Maxpayload=	4096,
HTLEN= 		4*1024,
Maxpath=	128,
Maxlcks=	10,
RRmagic=	0xdeadbabe,
DNmagic=	0xa110a110,
Maxactive=	250,
Maxreqtm=	8*1000,
Notauthoritative = 0,
Authoritative,
};
typedef struct Area	Area;
typedef struct Block	Block;
typedef struct Cert	Cert;
typedef struct DN	DN;
typedef struct DNSmsg	DNSmsg;
typedef struct Key	Key;
typedef struct Null	Null;
typedef struct RR	RR;
typedef struct Request	Request;
typedef struct SOA	SOA;
typedef struct Server	Server;
typedef struct Sig	Sig;
typedef struct Srv	Srv;
typedef struct Txt	Txt;
struct Request
{
int	isslave;
uvlong	aborttime;
jmp_buf	mret;
int	id;
char	*from;
};
typedef struct Querylck Querylck;
struct Querylck
{
QLock;
Ref;
};
struct DN
{
DN	*next;
ulong	magic;
char	*name;
RR	*rr;
ulong	referenced;
ulong	lookuptime;
ulong	refs;
ulong	ordinal;
ushort	class;
uchar	keep;
uchar	respcode;
Querylck querylck[Maxlcks];
};
struct Block
{
int	dlen;
uchar	*data;
};
struct Key
{
int	flags;
int	proto;
int	alg;
Block;
};
struct Cert
{
int	type;
int	tag;
int	alg;
Block;
};
struct Sig
{
Cert;
int	labels;
ulong	ttl;
ulong	exp;
ulong	incep;
DN	*signer;
};
struct Null
{
Block;
};
struct Txt
{
Txt	*next;
char	*p;
};
struct RR
{
RR	*next;
ulong	magic;
DN	*owner;
uintptr	pc;
ulong	ttl;
ulong	expire;
ulong	marker;
ushort	type;
ushort	query;
uchar	auth;
uchar	db;
uchar	cached;
uchar	negative;
union {
DN	*negsoaowner;
DN	*host;
DN	*cpu;
DN	*mb;
DN	*ip;
DN	*rp;
uintptr	arg0;
};
union {
int	negrcode;
DN	*rmb;
DN	*ptr;
DN	*os;
ulong	pref;
ulong	local;
ushort	port;
uintptr	arg1;
};
union {
SOA	*soa;
Key	*key;
Cert	*cert;
Sig	*sig;
Null	*null;
Txt	*txt;
Srv	*srv;
};
};
struct Server
{
Server	*next;
char	*name;
};
struct SOA
{
ulong	serial;
ulong	refresh;
ulong	retry;
ulong	expire;
ulong	minttl;
Server	*slaves;
};
struct Srv
{
ushort	pri;
ushort	weight;
};
typedef struct Rrlist Rrlist;
struct Rrlist
{
int	count;
RR	*rrs;
};
struct DNSmsg
{
ushort	id;
int	flags;
int	qdcount;
RR 	*qd;
int	ancount;
RR	*an;
int	nscount;
RR	*ns;
int	arcount;
RR	*ar;
};
struct Area
{
Area	*next;
int	len;
RR	*soarr;
int	neednotify;
int	needrefresh;
};
typedef struct Cfg Cfg;
struct Cfg {
int	cachedb;
int	resolver;
int	justforw;
int	serve;
int	inside;
int	straddle;
};
typedef struct {
QLock;
ulong	slavehiwat;
ulong	qrecvd9p;
ulong	qrecvdudp;
ulong	qsent;
ulong	qrecvd9prpc;
ulong	alarms;
ulong	under10ths[3*10+2];
ulong	tmout;
ulong	tmoutcname;
ulong	tmoutv6;
ulong	answinmem;
ulong	negans;
ulong	negserver;
ulong	negbaddeleg;
ulong	negbdnoans;
ulong	negnorname;
ulong	negcached;
} Stats;
Stats stats;
enum
{
Recurse,
Dontrecurse,
NOneg,
OKneg,
};
extern Cfg	cfg;
extern char	*dbfile;
extern int	debug;
extern Area	*delegated;
extern char	*logfile;
extern int	maxage;
extern char	mntpt[];
extern int	needrefresh;
extern int	norecursion;
extern ulong	now;
extern vlong	nowns;
extern Area	*owned;
extern int	sendnotifies;
extern ulong	target;
extern int	testing;
extern char	*trace;
extern int	traceactivity;
extern char	*zonerefreshprogram;
#pragma	varargck	type	"R"	RR*
#pragma	varargck	type	"Q"	RR*
extern char	*rrtname[];
extern char	*rname[];
extern unsigned	nrname;
extern char	*opname[];
extern Lock	dnlock;
void	abort(); ;
void	addserver(Server**, char*);
Server*	copyserverlist(Server*);
void	db2cache(int);
void	dnage(DN*);
void	dnageall(int);
void	dnagedb(void);
void	dnageallnever(void);
void	dnagenever(DN *, int);
void	dnauthdb(void);
void	dncheck(void*, int);
void	dndump(char*);
void	dnget(void);
void	dninit(void);
DN*	dnlookup(char*, int, int);
void	dnptr(uchar*, uchar*, char*, int, int, int);
void	dnpurge(void);
void	dnput(void);
void	dnslog(char*, ...);
void	dnstats(char *file);
void*	emalloc(int);
char*	estrdup(char*);
void	freeanswers(DNSmsg *mp);
void	freeserverlist(Server*);
int	getactivity(Request*, int);
Area*	inmyarea(char*);
void	putactivity(int);
RR*	randomize(RR*);
RR*	rralloc(int);
void	rrattach(RR*, int);
int	rravfmt(Fmt*);
RR*	rrcat(RR**, RR*);
RR**	rrcopy(RR*, RR**);
int	rrfmt(Fmt*);
void	rrfree(RR*);
void	rrfreelist(RR*);
RR*	rrlookup(DN*, int, int);
char*	rrname(int, char*, int);
RR*	rrremneg(RR**);
RR*	rrremtype(RR**, int);
int	rrsupported(int);
int	rrtype(char*);
void	slave(Request*);
int	subsume(char*, char*);
int	tsame(int, int);
void	unique(RR*);
void	warning(char*, ...);
void	refresh_areas(Area*);
void	freearea(Area**);
void	addarea(DN *dp, RR *rp, Ndbtuple *t);
int	baddelegation(RR*, RR*, uchar*);
RR*	dbinaddr(DN*, int);
RR*	dblookup(char*, int, int, int, int);
void	dnforceage(void);
RR*	dnsservers(int);
RR*	domainlist(int);
int	insideaddr(char *dom);
int	insidens(uchar *ip);
int	myaddr(char *addr);
int	opendatabase(void);
uchar*	outsidens(int);
char*	walkup(char*);
RR*	getdnsservers(int);
void	logreply(int, uchar*, DNSmsg*);
void	logsend(int, int, uchar*, char*, char*, int);
void	procsetname(char *fmt, ...);
RR*	dnresolve(char*, int, int, Request*, RR**, int, int, int, int*);
int	udpport(char *);
int	mkreq(DN *dp, int type, uchar *buf, int flags, ushort reqno);
int	seerootns(void);
void	initdnsmsg(DNSmsg *mp, RR *rp, int flags, ushort reqno);
DNSmsg*	newdnsmsg(RR *rp, int flags, ushort reqno);
void	dnserver(DNSmsg*, DNSmsg*, Request*, uchar *, int);
void	dnudpserver(char*);
void	dntcpserver(char*);
void	dnnotify(DNSmsg*, DNSmsg*, Request*);
void	notifyproc(void);
int	convDNS2M(DNSmsg*, uchar*, int);
char*	convM2DNS(uchar*, int, DNSmsg*, int*);
#pragma varargck argpos dnslog 1