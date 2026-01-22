typedef struct Block	Block;
typedef struct Chan	Chan;
typedef struct Cmdbuf	Cmdbuf;
typedef struct Cmdtab	Cmdtab;
typedef struct Cname	Cname;
typedef struct Dev	Dev;
typedef struct Dirtab	Dirtab;
typedef struct Egrp	Egrp;
typedef struct Evalue	Evalue;
typedef struct Fgrp	Fgrp;
typedef struct Mount	Mount;
typedef struct Mntcache Mntcache;
typedef struct Mntparam Mntparam;
typedef struct Mntrpc	Mntrpc;
typedef struct Mntwalk	Mntwalk;
typedef struct Mnt	Mnt;
typedef struct Mhead	Mhead;
typedef struct Osenv	Osenv;
typedef struct Pgrp	Pgrp;
typedef struct Proc	Proc;
typedef struct Queue	Queue;
typedef struct Ref	Ref;
typedef struct Rendez	Rendez;
typedef struct Rept	Rept;
typedef struct Rootdata Rootdata;
typedef struct RWLock	RWlock;
typedef struct Procs	Procs;
typedef struct Signerkey Signerkey;
typedef struct Skeyset	Skeyset;
typedef struct Uqid	Uqid;
typedef struct Uqidtab	Uqidtab;
typedef struct Walkqid	Walkqid;
#include "lib9.h"
#undef CHDIR
#undef NAMELEN
#undef ERRLEN
#include "emu.h"
#pragma incomplete Queue
#pragma incomplete Mntrpc
#include "fcall.h"
#include "pool.h"
typedef int    Devgen(Chan*, char*, Dirtab*, int, int, Dir*);
enum
{
NERR		= 32,
KNAMELEN	= 28,
MAXROOT		= 5*KNAMELEN,
NUMSIZE		= 11,
PRINTSIZE	= 256,
READSTR		= 1000
};
struct Ref
{
Lock	lk;
long	ref;
};
struct Rendez
{
Lock	l;
Proc*	p;
};
struct Rept
{
Lock	l;
Rendez	r;
void	*o;
int	t;
int	(*active)(void*);
int	(*ck)(void*, int);
void	(*f)(void*);
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
CCACHE	= 0x0080
};
struct Chan
{
Lock	l;
Ref	r;
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
Chan*	mchan;
Qid	mqid;
Cname	*name;
};
struct Cname
{
Ref	r;
int	alen;
int	len;
char	*s;
};
struct Dev
{
int	dc;
char*	name;
void	(*init)(void);
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
};
enum
{
BINTR		=	(1<<0),
BFREE		=	(1<<1),
BMORE		=	(1<<2)
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
ulong	flag;
};
#define BLEN(s)	((s)->wp - (s)->rp)
#define BALLOC(s) ((s)->lim - (s)->base)
struct Dirtab
{
char	name[KNAMELEN];
Qid	qid;
vlong	length;
long	perm;
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
NSCACHE	=	(1<<NSLOG)
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
Ref	r;
RWlock	lock;
Chan*	from;
Mount*	mount;
Mhead*	hash;
};
struct Mnt
{
Lock	l;
Chan*	c;
Proc*	rip;
Mntrpc*	queue;
ulong	id;
Mnt*	list;
int	flags;
int	msize;
char	*version;
Queue	*q;
};
enum
{
MNTLOG	=	5,
MNTHASH =	1<<MNTLOG,
DELTAFD=		20,
MAXNFD =		4000,
MAXKEY =		8
};
#define MOUNTH(p,qid)	((p)->mnthash[(qid).path&((1<<MNTLOG)-1)])
struct Mntparam {
Chan*	chan;
Chan*	authchan;
char*	spec;
int	flags;
};
struct Pgrp
{
Ref	r;
ulong	pgrpid;
RWlock	ns;
QLock	nsh;
Mhead*	mnthash[MNTHASH];
int	progmode;
Chan*	dot;
Chan*	slash;
int	nodevs;
int	pin;
};
enum
{
Nopin =	-1
};
struct Fgrp
{
Lock	l;
Ref	r;
Chan**	fd;
int	nfd;
int	maxfd;
int	minfd;
};
struct Evalue
{
char	*var;
char	*val;
int	len;
Qid	qid;
Evalue	*next;
};
struct Egrp
{
Ref	r;
QLock	l;
ulong	path;
ulong	vers;
Evalue	*entries;
};
struct Signerkey
{
Ref	r;
char*	owner;
ushort	footprint;
ulong	expires;
void*	alg;
void*	pk;
void	(*pkfree)(void*);
};
struct Skeyset
{
Ref	r;
QLock	l;
ulong	flags;
char*	devs;
int	nkey;
Signerkey	*keys[MAXKEY];
};
struct Uqid
{
Ref	r;
int	type;
int	dev;
vlong	oldpath;
vlong	newpath;
Uqid*	next;
};
enum
{
Nqidhash = 32
};
struct Uqidtab
{
QLock	l;
Uqid*	qids[Nqidhash];
ulong	pathgen;
};
struct Osenv
{
char	*syserrstr;
char	*errstr;
char	errbuf0[ERRMAX];
char	errbuf1[ERRMAX];
Pgrp*	pgrp;
Fgrp*	fgrp;
Egrp*	egrp;
Skeyset*		sigs;
Rendez*	rend;
Queue*	waitq;
Queue*	childq;
void*	debug;
char*	user;
FPU	fpu;
int	uid;
int	gid;
void	*ui;
};
enum
{
Unknown	= 0xdeadbabe,
IdleGC	= 0x16,
Interp	= 0x17,
BusyGC	= 0x18,
Moribund
};
struct Proc
{
int	type;
char	text[KNAMELEN];
Proc*	qnext;
long	pid;
Proc*	next;
Proc*	prev;
Lock	rlock;
Rendez*	r;
Rendez	sleep;
int		killed;
int	swipend;
int	syscall;
int	intwait;
int	sigid;
Lock	sysio;
char	genbuf[128];
int	nerr;
osjmpbuf	estack[NERR];
char*	kstack;
void	(*func)(void*);
void*	arg;
void*	iprog;
void*	prog;
Osenv*	env;
Osenv	defenv;
osjmpbuf	privstack;
osjmpbuf	sharestack;
Proc	*kid;
void	*kidsp;
void	*os;
};
#define poperror()	up->nerr--
#define	waserror()	(up->nerr++, ossetjmp(up->estack[up->nerr-1]))
enum
{
KPDUPPG		= (1<<0),
KPDUPFDG	= (1<<1),
KPDUPENVG	= (1<<2),
KPX11		= (1<<8),
KPDUP		= (KPDUPPG|KPDUPFDG|KPDUPENVG)
};
struct Procs
{
Lock	l;
Proc*	head;
Proc*	tail;
};
struct Rootdata
{
int	dotdot;
void	*ptr;
int	size;
int	*sizep;
};
extern	Dev*	devtab[];
extern	char	*ossysname;
extern	char	*eve;
extern	Queue*	kbdq;
extern	Queue*	gkbdq;
extern	Queue*	gkscanq;
extern	int	Xsize;
extern	int	Ysize;
extern	Pool*	mainmem;
extern	char	rootdir[MAXROOT];
extern	Procs	procs;
extern	int	sflag;
extern	int	xtblbit;
extern	int	globfs;
extern	int	greyscale;
extern	uint	qiomaxatomic;
enum
{
INVAL		= 0x0001,
ZDIV		= 0x0002,
OVFL		= 0x0004,
UNFL		= 0x0008,
INEX		= 0x0010,
RND_NR		= 0x0000,
RND_NINF	= 0x0100,
RND_PINF	= 0x0200,
RND_Z		= 0x0300,
RND_MASK	= 0x0300
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
#pragma varargck	type	"I" uchar*
#pragma	varargck	type	"E" uchar*
extern void	(*mainmonitor)(int, void*, ulong);