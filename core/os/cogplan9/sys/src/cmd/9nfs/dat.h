enum
{
FHSIZE	= 32
};
typedef struct Accept	Accept;
typedef struct Auth	Auth;
typedef struct Authunix	Authunix;
typedef struct Chalstuff Chalstuff;
typedef uchar		Fhandle[FHSIZE];
typedef struct Fid	Fid;
typedef struct Procmap	Procmap;
typedef struct Progmap	Progmap;
typedef struct Reject	Reject;
typedef struct Rpccall	Rpccall;
typedef struct Rpccache	Rpccache;
typedef struct Sattr	Sattr;
typedef struct Session	Session;
typedef struct String	String;
typedef struct Strnode	Strnode;
typedef struct Unixid	Unixid;
typedef struct Unixidmap Unixidmap;
typedef struct Unixmap	Unixmap;
typedef struct Unixscmap Unixscmap;
typedef struct Xfid	Xfid;
typedef struct Xfile	Xfile;
struct String
{
ulong	n;
char *	s;
};
struct Progmap
{
int	progno;
int	vers;
void	(*init)(int, char**);
Procmap *pmap;
};
struct Procmap
{
int	procno;
int	(*procp)(int, Rpccall*, Rpccall*);
};
struct Auth
{
ulong	flavor;
ulong	count;
void *	data;
};
struct Authunix
{
ulong	stamp;
String	mach;
ulong	uid;
ulong	gid;
int	gidlen;
ulong	gids[10];
};
struct Accept
{
Auth	averf;
ulong	astat;
union{
void *	results;
struct{
ulong	plow;
ulong	phigh;
};
};
};
struct Reject
{
ulong	rstat;
union{
struct{
ulong	rlow;
ulong	rhigh;
};
ulong	authstat;
};
};
struct Rpccall
{
uchar	prefix0[12];
ulong	host;
uchar	prefix1[12];
ulong	lhost;
ulong	port;
ulong	lport;
ulong	xid;
ulong	mtype;
union{
struct{
ulong	rpcvers;
ulong	prog;
ulong	vers;
ulong	proc;
Auth	cred;
Auth	verf;
Unixidmap *up;
char *	user;
void *	args;
};
struct{
ulong	stat;
union{
Accept;
Reject;
};
};
};
};
struct Rpccache
{
Rpccache *prev;
Rpccache *next;
ulong	host;
ulong	port;
ulong	xid;
int	n;
uchar	data[4];
};
struct Sattr
{
ulong	mode;
ulong	uid;
ulong	gid;
ulong	size;
ulong	atime;
ulong	ausec;
ulong	mtime;
ulong	musec;
};
struct Strnode
{
Strnode *next;
char	str[4];
};
struct Unixid
{
Unixid *next;
char *	name;
int	id;
};
struct Unixmap
{
char *	file;
int	style;
long	timestamp;
Unixid *ids;
};
struct Unixidmap
{
Unixidmap *next;
int	flag;
char *	server;
char *	client;
Reprog *sexp;
Reprog *cexp;
Unixmap	u;
Unixmap	g;
};
struct Unixscmap
{
Unixscmap *next;
char *	server;
ulong	clientip;
Unixidmap *map;
};
struct Xfile
{
Xfile *	next;
Session	*s;
Qid		qid;
Xfile *	parent;
Xfile *	child;
Xfile *	sib;
char *	name;
Xfid *	users;
};
enum
{
Oread	= 1,
Owrite	= 2,
Open	= 3,
Trunc	= 4
};
struct Xfid
{
Xfid *	next;
Xfile *	xp;
char *	uid;
Fid *	urfid;
Fid *	opfid;
ulong	mode;
ulong	offset;
};
struct Fid
{
Fid **	owner;
Fid *	prev;
Fid *	next;
long	tstale;
};
enum
{
Maxfdata = 8192,
Maxstatdata = 2048,
};
struct Session
{
Session *next;
char *	service;
int	fd;
#define CHALLEN 1
char	cchal[CHALLEN];
char	schal[CHALLEN];
char	authid[ANAMELEN];
char	authdom[DOMLEN];
char *	spec;
Xfile *	root;
ushort	tag;
Fcall	f;
uchar	data[IOHDRSZ+Maxfdata];
uchar	statbuf[Maxstatdata];
Fid *	free;
Fid	list;
Fid	fids[1000];
int	noauth;
};
struct Chalstuff
{
Chalstuff *next;
Xfid *	xf;
long	tstale;
Chalstate;
};
extern int	rpcdebug;
extern int	p9debug;
extern int	chatty;
extern void	(*rpcalarm)(void);
extern long	starttime;
extern long	nfstime;
extern char *	config;
extern int	staletime;
extern int	messagesize;
extern char *	commonopts;