#define	KNAMELEN		28
#define	DOMLEN			64
#define	BLOCKALIGN		8
typedef struct Alarms	Alarms;
typedef struct Block	Block;
typedef struct CSN	CSN;
typedef struct Chan	Chan;
typedef struct Cmdbuf	Cmdbuf;
typedef struct Cmdtab	Cmdtab;
typedef struct Cname	Cname;
typedef struct Conf	Conf;
typedef struct Dev	Dev;
typedef struct Dirtab	Dirtab;
typedef struct Edfinterface	Edfinterface;
typedef struct Egrp	Egrp;
typedef struct Evalue	Evalue;
typedef struct Fgrp	Fgrp;
typedef struct FPsave	FPsave;
typedef struct DevConf	DevConf;
typedef struct Label	Label;
typedef struct List	List;
typedef struct Log	Log;
typedef struct Logflag	Logflag;
typedef struct Mntcache Mntcache;
typedef struct Mount	Mount;
typedef struct Mntrpc	Mntrpc;
typedef struct Mntwalk	Mntwalk;
typedef struct Mnt	Mnt;
typedef struct Mhead	Mhead;
typedef struct Note	Note;
typedef struct Page	Page;
typedef struct Palloc	Palloc;
typedef struct Perf	Perf;
typedef struct Pgrps	Pgrps;
typedef struct PhysUart	PhysUart;
typedef struct Pgrp	Pgrp;
typedef struct Physseg	Physseg;
typedef struct Proc	Proc;
typedef struct Pte	Pte;
typedef struct Pthash	Pthash;
typedef struct Queue	Queue;
typedef struct Ref	Ref;
typedef struct Rendez	Rendez;
typedef struct Rgrp	Rgrp;
typedef struct RWlock	RWlock;
typedef struct Schedq	Schedq;
typedef struct Segment	Segment;
typedef struct Session	Session;
typedef struct Task	Task;
typedef struct Talarm	Talarm;
typedef struct Timer	Timer;
typedef struct Uart	Uart;
typedef struct Ureg Ureg;
typedef struct Waitq	Waitq;
typedef struct Walkqid	Walkqid;
typedef int    Devgen(Chan*, char*, Dirtab*, int, int, Dir*);
#include "fcall.h"
enum
{
SnarfSize = 64*1024,
};
struct Conf
{
ulong	nmach;
ulong	nproc;
ulong	monitor;
ulong	npage0;
ulong	npage1;
ulong	npage;
ulong	upages;
ulong	nimage;
ulong	nswap;
int	nswppo;
ulong	base0;
ulong	base1;
ulong	copymode;
ulong	ialloc;
ulong	pipeqsize;
int	nuart;
};
struct Label
{
jmp_buf	buf;
};
struct Ref
{
Lock lk;
long	ref;
};
struct Rendez
{
Lock lk;
Proc	*p;
};
struct RWlock
{
int	readers;
Lock	lk;
QLock	x;
QLock	k;
};
struct Talarm
{
Lock lk;
Proc	*list;
};
struct Alarms
{
QLock lk;
Proc	*head;
};
enum
{
Aaccess,
Abind,
Atodir,
Aopen,
Amount,
Acreate,
Aremove,
COPEN	= 0x0001,
CMSG	= 0x0002,
CCEXEC	= 0x0008,
CFREE	= 0x0010,
CRCLOSE	= 0x0020,
CCACHE	= 0x0080,
};
enum
{
BINTR	=	(1<<0),
BFREE	=	(1<<1),
Bipck	=	(1<<2),
Budpck	=	(1<<3),
Btcpck	=	(1<<4),
Bpktck	=	(1<<5),
};
struct Block
{
Block*	next;
Block*	list;
uchar*	rp;
uchar*	wp;
uchar*	lim;
uchar*	base;
void	(*free)(Block*);
ushort	flag;
ushort	checksum;
};
#define BLEN(s)	((s)->wp - (s)->rp)
#define BALLOC(s) ((s)->lim - (s)->base)
struct Chan
{
Ref ref;
Chan*	next;
Chan*	link;
vlong	offset;
ushort	type;
ulong	dev;
ushort	mode;
ushort	flag;
Qid	qid;
int	fid;
ulong	iounit;
Mhead*	umh;
Chan*	umc;
QLock	umqlock;
int	uri;
int	dri;
ulong	mountid;
Mntcache *mcp;
Mnt		*mux;
void*	aux;
Qid	pgrpid;
ulong	mid;
Chan*	mchan;
Qid	mqid;
Session*session;
Cname	*name;
};
struct Cname
{
Ref ref;
int	alen;
int	len;
char	*s;
};
struct Dev
{
int	dc;
char*	name;
void	(*reset)(void);
void	(*init)(void);
void	(*shutdown)(void);
Chan*	(*attach)(char*);
Walkqid*	(*walk)(Chan*, Chan*, char**, int);
int	(*stat)(Chan*, uchar*, int);
Chan*	(*open)(Chan*, int);
void	(*create)(Chan*, char*, int, ulong);
void	(*close)(Chan*);
long	(*read)(Chan*, void*, long, vlong);
Block*	(*bread)(Chan*, long, ulong);
long	(*write)(Chan*, void*, long, vlong);
long	(*bwrite)(Chan*, Block*, ulong);
void	(*remove)(Chan*);
int	(*wstat)(Chan*, uchar*, int);
void	(*power)(int);
int	(*config)(int, char*, DevConf*);
};
struct Dirtab
{
char	name[KNAMELEN];
Qid	qid;
vlong length;
ulong	perm;
};
struct Walkqid
{
Chan	*clone;
int	nqid;
Qid	qid[1];
};
enum
{
NSMAX	=	1000,
NSLOG	=	7,
NSCACHE	=	(1<<NSLOG),
};
struct Mntwalk
{
int		cddone;
ulong	id;
Mhead*	mh;
Mount*	cm;
};
struct Mount
{
ulong	mountid;
Mount*	next;
Mhead*	head;
Mount*	copy;
Mount*	order;
Chan*	to;
int	mflag;
char	*spec;
};
struct Mhead
{
Ref ref;
RWlock	lock;
Chan*	from;
Mount*	mount;
Mhead*	hash;
};
struct Mnt
{
Lock lk;
Chan	*c;
Proc	*rip;
Mntrpc	*queue;
ulong	id;
Mnt	*list;
int	flags;
int	msize;
char	*version;
Queue	*q;
};
enum
{
NUser,
NExit,
NDebug,
};
struct Note
{
char	msg[ERRMAX];
int	flag;
};
enum
{
RENDLOG	=	5,
RENDHASH =	1<<RENDLOG,
MNTLOG	=	5,
MNTHASH =	1<<MNTLOG,
NFD =		100,
PGHLOG  =	9,
PGHSIZE	=	1<<PGHLOG,
};
#define REND(p,s)	((p)->rendhash[(s)&((1<<RENDLOG)-1)])
#define MOUNTH(p,qid)	((p)->mnthash[(qid).path&((1<<MNTLOG)-1)])
struct Pgrp
{
Ref ref;
int	noattach;
ulong	pgrpid;
QLock	debug;
RWlock	ns;
Mhead	*mnthash[MNTHASH];
};
struct Rgrp
{
Ref ref;
Proc	*rendhash[RENDHASH];
};
struct Egrp
{
Ref ref;
RWlock lk;
Evalue	**ent;
int nent;
int ment;
ulong	path;
ulong	vers;
};
struct Evalue
{
char	*name;
char	*value;
int	len;
Evalue	*link;
Qid	qid;
};
struct Fgrp
{
Ref ref;
Chan	**fd;
int	nfd;
int	maxfd;
int	exceed;
};
enum
{
DELTAFD	= 20,
NERR = 20
};
typedef uvlong	Ticks;
enum
{
Running,
Rendezvous,
Wakeme,
};
struct Proc
{
uint		state;
uint		mach;
ulong	pid;
ulong	parentpid;
Pgrp	*pgrp;
Fgrp	*fgrp;
Rgrp *rgrp;
Lock	rlock;
Rendez	*r;
Rendez	sleep;
int	notepending;
int	kp;
void*	rendtag;
void*	rendval;
Proc	*rendhash;
int	nerrlab;
Label	errlab[NERR];
char user[KNAMELEN];
char	*syserrstr;
char	*errstr;
char	errbuf0[ERRMAX];
char	errbuf1[ERRMAX];
char	genbuf[128];
char text[KNAMELEN];
Chan	*slash;
Chan	*dot;
Proc		*qnext;
void	(*fn)(void*);
void *arg;
char oproc[1024];
};
enum
{
PRINTSIZE =	256,
MAXCRYPT = 	127,
NUMSIZE	=	12,
MB =		(1024*1024),
READSTR =	1000,
};
extern	char*	conffile;
extern	int	cpuserver;
extern	Dev*	devtab[];
extern  char	*eve;
extern	char	hostdomain[];
extern	uchar	initcode[];
extern  Queue*	kbdq;
extern  Queue*	kprintoq;
extern  Ref	noteidalloc;
extern	Palloc	palloc;
extern  Queue	*serialoq;
extern	char*	statename[];
extern	int	nsyscall;
extern	char	*sysname;
extern	uint	qiomaxatomic;
extern	Conf	conf;
enum
{
LRESPROF	= 3,
};
struct Log {
Lock lk;
int	opens;
char*	buf;
char	*end;
char	*rptr;
int	len;
int	nlog;
int	minread;
int	logmask;
QLock	readq;
Rendez	readr;
};
struct Logflag {
char*	name;
int	mask;
};
enum
{
NCMDFIELD = 128
};
struct Cmdbuf
{
char	*buf;
char	**f;
int	nf;
};
struct Cmdtab
{
int	index;
char	*cmd;
int	narg;
};
enum
{
Qstarve		= (1<<0),
Qmsg		= (1<<1),
Qclosed		= (1<<2),
Qflow		= (1<<3),
Qcoalesce	= (1<<4),
Qkick		= (1<<5),
};
#define DEVDOTDOT -1
extern Proc *_getproc(void);
extern void _setproc(Proc*);
#define	up	(_getproc())