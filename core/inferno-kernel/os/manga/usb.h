typedef struct Ctlr Ctlr;
typedef struct Endpt Endpt;
typedef struct Udev Udev;
typedef struct Usbhost Usbhost;
enum
{
MaxUsb = 4,
MaxUsbDev = 32,
TokIN = 0x69,
TokOUT = 0xE1,
TokSETUP = 0x2D,
RH2D = 0<<7,
RD2H = 1<<7,
Rstandard = 0<<5,
Rclass = 1<<5,
Rvendor = 2<<5,
Rdevice = 0,
Rinterface = 1,
Rendpt = 2,
Rother = 3,
};
#define Class(csp)		((csp)&0xff)
#define Subclass(csp)	(((csp)>>8)&0xff)
#define Proto(csp)		(((csp)>>16)&0xff)
#define CSP(c, s, p)	((c) | ((s)<<8) | ((p)<<16))
struct Endpt
{
Ref;
Lock;
int		x;
int		id;
int		maxpkt;
int		data01;
uchar	eof;
ulong	csp;
uchar	mode;
uchar	nbuf;
uchar	iso;
uchar	debug;
uchar	active;
int		setin;
int		hz;
int		remain;
int		samplesz;
int		sched;
int		pollms;
int		psize;
int		off;
ulong	foffset;
ulong	poffset;
ulong	toffset;
vlong	time;
int		buffered;
Udev*	dev;
ulong	nbytes;
ulong	nblocks;
void	*private;
QLock	rlock;
Rendez	rr;
Queue*	rq;
QLock	wlock;
Rendez	wr;
Queue*	wq;
int		ntd;
char*	err;
Endpt*	activef;
};
enum
{
Disabled = 0,
Attached,
Enabled,
Assigned,
Configured,
Noclass = 0,
Hubclass = 9,
};
struct Udev
{
Ref;
Lock;
Usbhost	*uh;
int		x;
int		busy;
int		state;
int		id;
uchar	port;
ulong	csp;
ushort	vid;
ushort	did;
int		ls;
int		npt;
Endpt*	ep[16];
Udev*	ports;
Udev*	next;
};
struct Usbhost
{
ISAConf;
int	tbdf;
QLock;
int		idgen;
Udev*	dev[MaxUsbDev];
void	(*init)(Usbhost*);
void	(*interrupt)(Ureg*, void*);
void	(*portinfo)(Usbhost*, char*, char*);
void	(*portreset)(Usbhost*, int);
void	(*portenable)(Usbhost*, int, int);
void	(*epalloc)(Usbhost*, Endpt*);
void	(*epfree)(Usbhost*, Endpt*);
void	(*epopen)(Usbhost*, Endpt*);
void	(*epclose)(Usbhost*, Endpt*);
void	(*epmode)(Usbhost*, Endpt*);
long	(*read)(Usbhost*, Endpt*, void*, long, vlong);
long	(*write)(Usbhost*, Endpt*, void*, long, vlong, int);
void	*ctlr;
};
extern void addusbtype(char*, int(*)(Usbhost*));