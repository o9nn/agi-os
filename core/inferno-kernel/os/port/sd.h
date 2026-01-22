typedef struct SDev SDev;
typedef struct SDifc SDifc;
typedef struct SDpart SDpart;
typedef struct SDperm SDperm;
typedef struct SDreq SDreq;
typedef struct SDunit SDunit;
struct SDperm {
char*	name;
char*	user;
ulong	perm;
};
struct SDpart {
ulong	start;
ulong	end;
SDperm;
int	valid;
ulong	vers;
};
struct SDunit {
SDev*	dev;
int	subno;
uchar	inquiry[256];
SDperm;
QLock	ctl;
ulong	sectors;
ulong	secsize;
SDpart*	part;
int	npart;
ulong	vers;
SDperm	ctlperm;
QLock	raw;
ulong	rawinuse;
int	state;
SDreq*	req;
SDperm	rawperm;
};
struct SDev {
Ref	r;
SDifc*	ifc;
void*	ctlr;
int	idno;
char*	name;
SDev*	next;
QLock;
int	enabled;
int	nunit;
QLock	unitlock;
int*	unitflg;
SDunit**unit;
};
struct SDifc {
char*	name;
SDev*	(*pnp)(void);
SDev*	(*legacy)(int, int);
SDev*	(*id)(SDev*);
int	(*enable)(SDev*);
int	(*disable)(SDev*);
int	(*verify)(SDunit*);
int	(*online)(SDunit*);
int	(*rio)(SDreq*);
int	(*rctl)(SDunit*, char*, int);
int	(*wctl)(SDunit*, Cmdbuf*);
long	(*bio)(SDunit*, int, int, void*, long, long);
SDev*	(*probe)(DevConf*);
void	(*clear)(SDev*);
char*	(*stat)(SDev*, char*, char*);
};
struct SDreq {
SDunit*	unit;
int	lun;
int	write;
uchar	cmd[16];
int	clen;
void*	data;
int	dlen;
int	flags;
int	status;
long	rlen;
uchar	sense[256];
};
enum {
SDnosense	= 0x00000001,
SDvalidsense	= 0x00010000,
};
enum {
SDretry		= -5,
SDmalloc	= -4,
SDeio		= -3,
SDtimeout	= -2,
SDnostatus	= -1,
SDok		= 0,
SDcheck		= 0x02,
SDbusy		= 0x08,
SDmaxio		= 2048*1024,
SDnpart		= 16,
};
#define sdmalloc(n)	malloc(n)
#define sdfree(p)	free(p)
extern int scsiverify(SDunit*);
extern int scsionline(SDunit*);
extern long scsibio(SDunit*, int, int, void*, long, long);
extern SDev* scsiid(SDev*, SDifc*);