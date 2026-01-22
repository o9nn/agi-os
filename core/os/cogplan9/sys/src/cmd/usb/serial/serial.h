typedef struct Serial Serial;
typedef struct Serialops Serialops;
typedef struct Serialport Serialport;
struct Serialops {
int	(*seteps)(Serialport*);
int	(*init)(Serialport*);
int	(*getparam)(Serialport*);
int	(*setparam)(Serialport*);
int	(*clearpipes)(Serialport*);
int	(*reset)(Serial*, Serialport*);
int	(*sendlines)(Serialport*);
int	(*modemctl)(Serialport*, int);
int	(*setbreak)(Serialport*, int);
int	(*readstatus)(Serialport*);
int	(*wait4data)(Serialport*, uchar *, int);
int	(*wait4write)(Serialport*, uchar *, int);
};
enum {
DataBufSz = 8*1024,
Maxifc = 16,
};
struct Serialport {
char name[32];
Serial	*s;
int	isjtag;
Dev	*epintr;
Dev	*epin;
Dev	*epout;
Usbfs	fs;
uchar	ctlstate;
uint	baud;
int	stop;
int	mctl;
int	parity;
int	bits;
int	fifo;
int	limit;
int	rts;
int	cts;
int	dsr;
int	dcd;
int	dtr;
int	rlsd;
vlong	timer;
int	blocked;
int	nbreakerr;
int	ring;
int	nframeerr;
int	nparityerr;
int	novererr;
int	enabled;
int	interfc;
Channel *w4data;
Channel *gotdata;
Channel *readc;
int	ndata;
uchar	data[DataBufSz];
};
struct Serial {
QLock;
Dev	*dev;
int	type;
int	recover;
Serialops;
int	hasepintr;
int	jtag;
int	nifcs;
Serialport p[Maxifc];
int	maxrtrans;
int	maxwtrans;
int	maxread;
int	maxwrite;
int	inhdrsz;
int	outhdrsz;
int	baudbase;
};
enum {
CTLS	= 023,
CTLQ	= 021,
CtlDTR	= 1,
CtlRTS	= 2,
};
int serialmain(Dev *d, int argc, char *argv[]);
typedef struct Cinfo Cinfo;
struct Cinfo {
int	vid;
int	did;
int	cid;
};
extern Cinfo plinfo[];
extern Cinfo uconsinfo[];
extern int serialdebug;
#define	dsprint	if(serialdebug)fprint
int	serialrecover(Serial *ser, Serialport *p, Dev *ep, char *err);
int	serialreset(Serial *ser);
char	*serdumpst(Serialport *p, char *buf, int bufsz);