typedef struct SDev SDev;
typedef struct SDifc SDifc;
typedef struct SDio SDio;
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
uvlong	start;
uvlong	end;
SDperm;
int	valid;
ulong	vers;
};
struct SDunit {
SDev*	dev;
int	subno;
uchar	inquiry[255];
uchar	sense[18];
SDperm;
QLock	ctl;
uvlong	sectors;
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
char	name[8];
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
int	(*enable)(SDev*);
int	(*disable)(SDev*);
int	(*verify)(SDunit*);
int	(*online)(SDunit*);
int	(*rio)(SDreq*);
int	(*rctl)(SDunit*, char*, int);
int	(*wctl)(SDunit*, Cmdbuf*);
long	(*bio)(SDunit*, int, int, void*, long, uvlong);
SDev*	(*probe)(DevConf*);
void	(*clear)(SDev*);
char*	(*rtopctl)(SDev*, char*, char*);
int	(*wtopctl)(SDev*, Cmdbuf*);
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
SDinq0periphqual= 0xe0,
SDinq0periphtype= 0x1f,
SDinq1removable	= 0x80,
SDperdisk	= 0,
SDpertape	= 1,
SDperpr		= 2,
SDperworm	= 4,
SDpercd		= 5,
SDpermo		= 7,
SDperjuke	= 8,
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
#ifndef sdmalloc
#define sdmalloc(n)	malloc(n)
#define sdfree(p)	free(p)
#endif
struct SDio {
char	*name;
int	(*init)(void);
void	(*enable)(void);
int	(*inquiry)(char*, int);
int	(*cmd)(u32int, u32int, u32int*);
void	(*iosetup)(int, void*, int, int);
void	(*io)(int, uchar*, int);
};
extern SDio sdio;
extern void sdadddevs(SDev*);
extern void sdaddconf(SDunit*);
extern void sdaddallconfs(void (*f)(SDunit*));
extern void sdaddpart(SDunit*, char*, uvlong, uvlong);
extern int sdsetsense(SDreq*, int, int, int, int);
extern int sdmodesense(SDreq*, uchar*, void*, int);
extern int sdfakescsi(SDreq*, void*, int);
extern int scsiverify(SDunit*);
extern int scsionline(SDunit*);
extern long scsibio(SDunit*, int, int, void*, long, uvlong);
extern SDev* scsiid(SDev*, SDifc*);