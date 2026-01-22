#define dprint if(debug)print
#define ddprint if(debug>1)print
#define deprint if(debug || ep->debug)print
#define ddeprint if(debug>1 || ep->debug>1)print
#define GET2(p) ((((p)[1]&0xFF)<<8)|((p)[0]&0xFF))
#define PUT2(p,v) {((p)[0] = (v)); ((p)[1] = (v)>>8);}
typedef struct Udev Udev;
typedef struct Ep Ep;
typedef struct Hci Hci;
typedef struct Hciimpl Hciimpl;
enum
{
Ndeveps = 16,
Nhcis = 16,
Neps = 128,
Maxctllen = 32*1024,
Xfertmout = 2000,
Tnone = 0,
Tctl,
Tiso,
Tbulk,
Tintr,
Nttypes,
Epmax = 0xF,
Devmax = 0x7F,
Fullspeed = 0,
Lowspeed,
Highspeed,
Nospeed,
Rh2d = 0<<7,
Rd2h = 1<<7,
Rstd = 0<<5,
Rclass = 1<<5,
Rdev = 0,
Rep = 2,
Rother = 3,
Rtype = 0,
Rreq = 1,
Rvalue = 2,
Rindex = 4,
Rcount = 6,
Rsetuplen = 8,
Rgetstatus = 0,
Rclearfeature = 1,
Rsetfeature = 3,
Rsetaddr = 5,
Rgetdesc = 6,
Dconfig = 0,
Denabled,
Ddetach,
Dreset,
HPpresent = 0x1,
HPenable = 0x2,
HPsuspend = 0x4,
HPovercurrent = 0x8,
HPreset = 0x10,
HPpower = 0x100,
HPslow = 0x200,
HPhigh = 0x400,
HPstatuschg = 0x10000,
HPchange = 0x20000,
};
struct Hciimpl
{
void *aux;
void (*init)(Hci*);
void (*dump)(Hci*);
void (*interrupt)(Ureg*, void*);
void (*epopen)(Ep*);
void (*epclose)(Ep*);
long (*epread)(Ep*,void*,long);
long (*epwrite)(Ep*,void*,long);
char* (*seprintep)(char*,char*,Ep*);
int (*portenable)(Hci*, int, int);
int (*portreset)(Hci*, int, int);
int (*portstatus)(Hci*, int);
void (*shutdown)(Hci*);
void (*debug)(Hci*, int);
};
struct Hci
{
ISAConf;
int tbdf;
int ctlrno;
int nports;
int highspeed;
Hciimpl;
};
struct Ep
{
Ref;
int idx;
int nb;
Hci* hp;
Udev* dev;
Ep* ep0;
QLock;
char* name;
int inuse;
int mode;
int clrhalt;
int debug;
char* info;
long maxpkt;
int ttype;
ulong load;
void* aux;
int rhrepl;
int toggle[2];
long pollival;
long hz;
long samplesz;
int ntds;
int tmout;
};
struct Udev
{
int nb;
int state;
int ishub;
int isroot;
int speed;
int hub;
int port;
Ep* eps[Ndeveps];
};
void addhcitype(char *type, int (*reset)(Hci*));
extern char *usbmodename[];
extern char Estalled[];
extern char *seprintdata(char*,char*,uchar*,int);