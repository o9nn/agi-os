#pragma	src	"/sys/src/libndb"
#pragma	lib	"libndb.a"
typedef struct Ndb	Ndb;
typedef struct Ndbtuple	Ndbtuple;
typedef struct Ndbhf	Ndbhf;
typedef struct Ndbs	Ndbs;
typedef struct Ndbcache	Ndbcache;
#pragma incomplete Ndbhf
#pragma incomplete Ndbcache
enum
{
Ndbalen=	32,
Ndbvlen=	64,
};
struct Ndb
{
Ndb		*next;
Biobufhdr	b;
uchar		buf[256];
ulong		mtime;
Qid		qid;
char		file[128];
ulong		length;
int		nohash;
Ndbhf		*hf;
int		ncache;
Ndbcache	*cache;
};
struct Ndbtuple
{
char		attr[Ndbalen];
char		*val;
Ndbtuple	*entry;
Ndbtuple	*line;
ulong		ptr;
char		valbuf[Ndbvlen];
};
#define NDBULLEN	4
#define NDBPLEN		3
#define NDBHLEN		(2*NDBULLEN)
struct Ndbs
{
Ndb	*db;
Ndbhf	*hf;
int	type;
ulong	ptr;
ulong	ptr1;
Ndbtuple *t;
};
#define NDBSPEC 	(1<<23)
#define NDBCHAIN	NDBSPEC
#define NDBNAP		(NDBSPEC|1)
#define NDBPUTP(v,a) { (a)[0] = v; (a)[1] = (v)>>8; (a)[2] = (v)>>16; }
#define NDBGETP(a) ((a)[0] | ((a)[1]<<8) | ((a)[2]<<16))
#define NDBPUTUL(v,a) { (a)[0] = v; (a)[1] = (v)>>8; (a)[2] = (v)>>16; (a)[3] = (v)>>24; }
#define NDBGETUL(a) ((a)[0] | ((a)[1]<<8) | ((a)[2]<<16) | ((a)[3]<<24))
#define NDB_IPlen 16
Ndbtuple*	csgetval(char*, char*, char*, char*, char*);
char*		csgetvalue(char*, char*, char*, char*, Ndbtuple**);
Ndbtuple*	csipinfo(char*, char*, char*, char**, int);
Ndbtuple*	dnsquery(char*, char*, char*);
char*		ipattr(char*);
Ndb*		ndbcat(Ndb*, Ndb*);
int		ndbchanged(Ndb*);
void		ndbclose(Ndb*);
Ndbtuple*	ndbconcatenate(Ndbtuple*, Ndbtuple*);
Ndbtuple*	ndbdiscard(Ndbtuple*, Ndbtuple*);
void		ndbfree(Ndbtuple*);
Ndbtuple*	ndbgetipaddr(Ndb*, char*);
Ndbtuple*	ndbgetval(Ndb*, Ndbs*, char*, char*, char*, char*);
char*		ndbgetvalue(Ndb*, Ndbs*, char*, char*, char*, Ndbtuple**);
Ndbtuple*	ndbfindattr(Ndbtuple*, Ndbtuple*, char*);
ulong		ndbhash(char*, int);
Ndbtuple*	ndbipinfo(Ndb*, char*, char*, char**, int);
Ndbtuple*	ndblookval(Ndbtuple*, Ndbtuple*, char*, char*);
Ndbtuple*	ndbnew(char*, char*);
Ndb*		ndbopen(char*);
Ndbtuple*	ndbparse(Ndb*);
int		ndbreopen(Ndb*);
Ndbtuple*	ndbreorder(Ndbtuple*, Ndbtuple*);
Ndbtuple*	ndbsearch(Ndb*, Ndbs*, char*, char*);
void		ndbsetval(Ndbtuple*, char*, int);
Ndbtuple*	ndbsnext(Ndbs*, char*, char*);
Ndbtuple*	ndbsubstitute(Ndbtuple*, Ndbtuple*, Ndbtuple*);
void		ndbsetmalloctag(Ndbtuple*, uintptr);