typedef struct Alarms Alarms;
typedef struct Block Block;
typedef struct Chan Chan;
typedef struct Cmdbuf Cmdbuf;
typedef struct Cmdtab Cmdtab;
typedef struct Confmem Confmem;
typedef struct Dev Dev;
typedef struct Dirtab Dirtab;
typedef struct Edf Edf;
typedef struct Egrp Egrp;
typedef struct Evalue Evalue;
typedef struct Execvals Execvals;
typedef struct Fgrp Fgrp;
typedef struct DevConf DevConf;
typedef struct Image Image;
typedef struct Log Log;
typedef struct Logflag Logflag;
typedef struct Mntcache Mntcache;
typedef struct Mount Mount;
typedef struct Mntrpc Mntrpc;
typedef struct Mntwalk Mntwalk;
typedef struct Mnt Mnt;
typedef struct Mhead Mhead;
typedef struct Note Note;
typedef struct Page Page;
typedef struct Path Path;
typedef struct Palloc Palloc;
typedef struct Pallocmem Pallocmem;
typedef struct Perf Perf;
typedef struct PhysUart PhysUart;
typedef struct Pgrp Pgrp;
typedef struct Physseg Physseg;
typedef struct Proc Proc;
typedef struct Pte Pte;
typedef struct QLock QLock;
typedef struct Queue Queue;
typedef struct Ref Ref;
typedef struct Rendez Rendez;
typedef struct Rgrp Rgrp;
typedef struct RWlock RWlock;
typedef struct Sargs Sargs;
typedef struct Schedq Schedq;
typedef struct Segment Segment;
typedef struct Sema Sema;
typedef struct Timer Timer;
typedef struct Timers Timers;
typedef struct Uart Uart;
typedef struct Waitq Waitq;
typedef struct Walkqid Walkqid;
typedef struct Watchdog Watchdog;
typedef struct Watermark Watermark;
typedef int Devgen(Chan*, char*, Dirtab*, int, int, Dir*);
#pragma incomplete DevConf
#pragma incomplete Edf
#pragma incomplete Mntcache
#pragma incomplete Mntrpc
#pragma incomplete Queue
#pragma incomplete Timers
#include <fcall.h>
#define HOWMANY(x, y) (((x)+((y)-1))/(y))
#define ROUNDUP(x, y) (HOWMANY((x), (y))*(y))
#define ROUNDDN(x, y) (((x)/(y))*(y))
#define ROUND(s, sz) (((s)+(sz-1))&~(sz-1))
#define PGROUND(s) ROUNDUP(s, BY2PG)
#define MIN(a, b) ((a) < (b)? (a): (b))
#define MAX(a, b) ((a) > (b)? (a): (b))
#define FIELD(v, o, w) (((v) & ((1<<(w))-1))<<(o))
#define FCLR(d, o, w) ((d) & ~(((1<<(w))-1)<<(o)))
#define FEXT(d, o, w) (((d)>>(o)) & ((1<<(w))-1))
#define FINS(d, o, w, v) (FCLR((d), (o), (w))|FIELD((v), (o), (w)))
#define FSET(d, o, w) ((d)|(((1<<(w))-1)<<(o)))
#define FMASK(o, w) (((1<<(w))-1)<<(o))
#ifndef KMESGSIZE
#define KMESGSIZE (16*1024)
#endif
#ifndef PCICONSSIZE
#define PCICONSSIZE (16*1024)
#endif
#ifndef STAGESIZE
#define STAGESIZE 64
#endif
#ifndef MAXBY2PG
#define MAXBY2PG BY2PG
#endif
struct Ref
{
Lock;
long ref;
};
struct Rendez
{
Lock;
Proc *p;
};
struct QLock
{
Lock use;
Proc *head;
Proc *tail;
int locked;
uintptr qpc;
};
struct RWlock
{
Lock use;
Proc *head;
Proc *tail;
ulong wpc;
Proc *wproc;
int readers;
int writer;
};
struct Alarms
{
QLock;
Proc *head;
};
struct Sargs
{
ulong args[MAXSYSARG];
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
COPEN = 0x0001,
CMSG = 0x0002,
CCEXEC = 0x0008,
CFREE = 0x0010,
CRCLOSE = 0x0020,
CCACHE = 0x0080,
};
enum
{
BINTR = (1<<0),
BFREE = (1<<1),
Bipck = (1<<2),
Budpck = (1<<3),
Btcpck = (1<<4),
Bpktck = (1<<5),
};
struct Block
{
long ref;
Block* next;
Block* list;
uchar* rp;
uchar* wp;
uchar* lim;
uchar* base;
void (*free)(Block*);
ushort flag;
ushort checksum;
ulong magic;
};
#define BLEN(s) ((s)->wp - (s)->rp)
#define BALLOC(s) ((s)->lim - (s)->base)
struct Chan
{
Ref;
Chan* next;
Chan* link;
vlong offset;
vlong devoffset;
ushort type;
ulong dev;
ushort mode;
ushort flag;
Qid qid;
int fid;
ulong iounit;
Mhead* umh;
Chan* umc;
QLock umqlock;
int uri;
int dri;
uchar* dirrock;
int nrock;
int mrock;
QLock rockqlock;
int ismtpt;
Mntcache*mcp;
Mnt* mux;
union {
void* aux;
Qid pgrpid;
ulong mid;
};
Chan* mchan;
Qid mqid;
Path* path;
};
struct Path
{
Ref;
char *s;
Chan **mtpt;
int len;
int alen;
int mlen;
int malen;
};
struct Dev
{
int dc;
char* name;
void (*reset)(void);
void (*init)(void);
void (*shutdown)(void);
Chan* (*attach)(char*);
Walkqid*(*walk)(Chan*, Chan*, char**, int);
int (*stat)(Chan*, uchar*, int);
Chan* (*open)(Chan*, int);
void (*create)(Chan*, char*, int, ulong);
void (*close)(Chan*);
long (*read)(Chan*, void*, long, vlong);
Block* (*bread)(Chan*, long, ulong);
long (*write)(Chan*, void*, long, vlong);
long (*bwrite)(Chan*, Block*, ulong);
void (*remove)(Chan*);
int (*wstat)(Chan*, uchar*, int);
void (*power)(int);
int (*config)(int, char*, DevConf*);
int attached;
};
struct Dirtab
{
char name[KNAMELEN];
Qid qid;
vlong length;
long perm;
};
struct Walkqid
{
Chan *clone;
int nqid;
Qid qid[1];
};
enum
{
NSMAX = 1000,
NSLOG = 7,
NSCACHE = (1<<NSLOG),
};
struct Mntwalk
{
int cddone;
Mhead* mh;
Mount* cm;
};
struct Mount
{
ulong mountid;
Mount* next;
Mhead* head;
Mount* copy;
Mount* order;
Chan* to;
int mflag;
char *spec;
};
struct Mhead
{
Ref;
RWlock lock;
Chan* from;
Mount* mount;
Mhead* hash;
};
struct Mnt
{
Lock;
Chan *c;
Proc *rip;
Mntrpc *queue;
ulong id;
Mnt *list;
int flags;
int msize;
char *version;
Queue *q;
};
enum
{
NUser,
NExit,
NDebug,
};
struct Note
{
char msg[ERRMAX];
int flag;
};
enum
{
PG_NOFLUSH = 0,
PG_TXTFLUSH = 1,
PG_DATFLUSH = 2,
PG_NEWCOL = 3,
PG_MOD = 0x01,
PG_REF = 0x02,
};
struct Page
{
Lock;
ulong pa;
ulong va;
ulong daddr;
ulong gen;
ushort ref;
char modref;
char color;
char cachectl[MAXMACH];
Image *image;
Page *next;
Page *prev;
Page *hash;
};
struct Swapalloc
{
Lock;
int free;
uchar* swmap;
uchar* alloc;
uchar* last;
uchar* top;
Rendez r;
ulong highwater;
ulong headroom;
}swapalloc;
struct Image
{
Ref;
Chan *c;
Qid qid;
Qid mqid;
Chan *mchan;
ushort type;
Segment *s;
Image *hash;
Image *next;
int notext;
};
struct Pte
{
Page *pages[PTEPERTAB];
Page **first;
Page **last;
};
enum
{
SG_TYPE = 07,
SG_TEXT = 00,
SG_DATA = 01,
SG_BSS = 02,
SG_STACK = 03,
SG_SHARED = 04,
SG_PHYSICAL = 05,
SG_RONLY = 0040,
SG_CEXEC = 0100,
};
#define PG_ONSWAP 1
#define onswap(s) (((ulong)s)&PG_ONSWAP)
#define pagedout(s) (((ulong)s)==0 || onswap(s))
#define swapaddr(s) (((ulong)s)&~PG_ONSWAP)
#define SEGMAXSIZE (SEGMAPSIZE*PTEMAPMEM)
struct Physseg
{
ulong attr;
char *name;
ulong pa;
ulong size;
Page *(*pgalloc)(Segment*, ulong);
void (*pgfree)(Page*);
};
struct Sema
{
Rendez;
long *addr;
int waiting;
Sema *next;
Sema *prev;
};
struct Segment
{
Ref;
QLock lk;
ushort steal;
ushort type;
ulong base;
ulong top;
ulong size;
ulong fstart;
ulong flen;
int flushme;
Image *image;
Physseg *pseg;
ulong* profile;
Pte **map;
int mapsize;
Pte *ssegmap[SSEGMAPSIZE];
Lock semalock;
Sema sema;
ulong mark;
};
enum
{
RENDLOG = 5,
RENDHASH = 1<<RENDLOG,
MNTLOG = 5,
MNTHASH = 1<<MNTLOG,
NFD = 100,
PGHLOG = 9,
PGHSIZE = 1<<PGHLOG,
};
#define REND(p,s) ((p)->rendhash[(s)&((1<<RENDLOG)-1)])
#define MOUNTH(p,qid) ((p)->mnthash[(qid).path&((1<<MNTLOG)-1)])
struct Pgrp
{
Ref;
int noattach;
ulong pgrpid;
QLock debug;
RWlock ns;
Mhead *mnthash[MNTHASH];
};
struct Rgrp
{
Ref;
Proc *rendhash[RENDHASH];
};
struct Egrp
{
Ref;
RWlock;
Evalue **ent;
int nent;
int ment;
ulong path;
ulong vers;
};
struct Evalue
{
char *name;
char *value;
int len;
Evalue *link;
Qid qid;
};
struct Fgrp
{
Ref;
Chan **fd;
int nfd;
int maxfd;
int exceed;
};
enum
{
DELTAFD = 20
};
struct Pallocmem
{
ulong base;
ulong npage;
};
struct Palloc
{
Lock;
Pallocmem mem[4];
Page *head;
Page *tail;
ulong freecount;
Page *pages;
ulong user;
Page *hash[PGHSIZE];
Lock hashlock;
Rendez r;
QLock pwait;
};
struct Waitq
{
Waitmsg w;
Waitq *next;
};
enum {
Trelative,
Tperiodic,
};
struct Timer
{
int tmode;
vlong tns;
void (*tf)(Ureg*, Timer*);
void *ta;
Lock;
Timers *tt;
Tval tticks;
Tval twhen;
Timer *tnext;
};
enum
{
RFNAMEG = (1<<0),
RFENVG = (1<<1),
RFFDG = (1<<2),
RFNOTEG = (1<<3),
RFPROC = (1<<4),
RFMEM = (1<<5),
RFNOWAIT = (1<<6),
RFCNAMEG = (1<<10),
RFCENVG = (1<<11),
RFCFDG = (1<<12),
RFREND = (1<<13),
RFNOMNT = (1<<14),
};
enum
{
SSEG, TSEG, DSEG, BSEG, ESEG, LSEG, SEG1, SEG2, SEG3, SEG4, NSEG
};
enum
{
Dead = 0,
Moribund,
Ready,
Scheding,
Running,
Queueing,
QueueingR,
QueueingW,
Wakeme,
Broken,
Stopped,
Rendezvous,
Waitrelease,
Proc_stopme = 1,
Proc_exitme,
Proc_traceme,
Proc_exitbig,
Proc_tracesyscall,
TUser = 0,
TSys,
TReal,
TCUser,
TCSys,
TCReal,
NERR = 64,
NNOTE = 5,
Npriq = 20,
Nrq = Npriq+2,
PriRelease = Npriq,
PriEdf = Npriq+1,
PriNormal = 10,
PriExtra = Npriq-1,
PriKproc = 13,
PriRoot = 13,
};
struct Schedq
{
Lock;
Proc* head;
Proc* tail;
int n;
};
struct Proc
{
Label sched;
char *kstack;
Mach *mach;
char *text;
char *user;
char *args;
int nargs;
Proc *rnext;
Proc *qnext;
QLock *qlock;
int state;
char *psstate;
Segment *seg[NSEG];
QLock seglock;
ulong pid;
ulong noteid;
Proc *pidhash;
Lock exl;
Waitq *waitq;
int nchild;
int nwait;
QLock qwaitr;
Rendez waitr;
Proc *parent;
Pgrp *pgrp;
Egrp *egrp;
Fgrp *fgrp;
Rgrp *rgrp;
Fgrp *closingfgrp;
ulong parentpid;
ulong time[6];
uvlong kentry;
vlong pcycles;
int insyscall;
int fpstate;
QLock debug;
Proc *pdbg;
ulong procmode;
ulong privatemem;
int hang;
int procctl;
ulong pc;
Lock rlock;
Rendez *r;
Rendez sleep;
int notepending;
int kp;
Proc *palarm;
ulong alarm;
int newtlb;
int noswap;
uintptr rendtag;
uintptr rendval;
Proc *rendhash;
Timer;
Rendez *trend;
int (*tfn)(void*);
void (*kpfun)(void*);
void *kparg;
FPsave fpsave;
int scallnr;
Sargs s;
int nerrlab;
Label errlab[NERR];
char *syserrstr;
char *errstr;
char errbuf0[ERRMAX];
char errbuf1[ERRMAX];
char genbuf[128];
Chan *slash;
Chan *dot;
Note note[NNOTE];
short nnote;
short notified;
Note lastnote;
int (*notify)(void*, char*);
Lock *lockwait;
Lock *lastlock;
Lock *lastilock;
Mach *wired;
Mach *mp;
Ref nlocks;
ulong delaysched;
ulong priority;
ulong basepri;
uchar fixedpri;
ulong cpu;
ulong lastupdate;
uchar yield;
ulong readytime;
ulong movetime;
int preempted;
Edf *edf;
int trace;
ulong qpc;
int setargs;
void *ureg;
void *dbgreg;
Notsave;
PMMU;
char *syscalltrace;
};
enum
{
PRINTSIZE = 256,
MAXCRYPT = 127,
NUMSIZE = 12,
MB = (1024*1024),
READSTR = 4000,
};
struct Execvals {
uvlong entry;
ulong textsize;
ulong datasize;
};
extern Conf conf;
extern char* conffile;
extern int cpuserver;
extern Dev* devtab[];
extern char* eve;
extern char hostdomain[];
extern uchar initcode[];
extern int kbdbuttons;
extern Queue* kbdq;
extern Queue* kprintoq;
extern Ref noteidalloc;
extern int nsyscall;
extern Palloc palloc;
int (*parseboothdr)(Chan *, ulong, Execvals *);
extern Queue* serialoq;
extern char* statename[];
extern Image swapimage;
extern char* sysname;
extern uint qiomaxatomic;
extern char* sysctab[];
Watchdog*watchdog;
int watchdogon;
enum
{
LRESPROF = 3,
};
struct Log {
Lock;
int opens;
char* buf;
char *end;
char *rptr;
int len;
int nlog;
int minread;
int logmask;
QLock readq;
Rendez readr;
};
struct Logflag {
char* name;
int mask;
};
enum
{
NCMDFIELD = 128
};
struct Cmdbuf
{
char *buf;
char **f;
int nf;
};
struct Cmdtab
{
int index;
char *cmd;
int narg;
};
struct PhysUart
{
char* name;
Uart* (*pnp)(void);
void (*enable)(Uart*, int);
void (*disable)(Uart*);
void (*kick)(Uart*);
void (*dobreak)(Uart*, int);
int (*baud)(Uart*, int);
int (*bits)(Uart*, int);
int (*stop)(Uart*, int);
int (*parity)(Uart*, int);
void (*modemctl)(Uart*, int);
void (*rts)(Uart*, int);
void (*dtr)(Uart*, int);
long (*status)(Uart*, void*, long, long);
void (*fifo)(Uart*, int);
void (*power)(Uart*, int);
int (*getc)(Uart*);
void (*putc)(Uart*, int);
};
enum {
Stagesize= STAGESIZE
};
struct Uart
{
void* regs;
void* saveregs;
char* name;
ulong freq;
int bits;
int stop;
int parity;
int baud;
PhysUart*phys;
int console;
int special;
Uart* next;
QLock;
int type;
int dev;
int opens;
int enabled;
Uart *elist;
int perr;
int ferr;
int oerr;
int berr;
int serr;
int (*putc)(Queue*, int);
Queue *iq;
Queue *oq;
Lock rlock;
uchar istage[Stagesize];
uchar *iw;
uchar *ir;
uchar *ie;
Lock tlock;
uchar ostage[Stagesize];
uchar *op;
uchar *oe;
int drain;
int modem;
int xonoff;
int blocked;
int cts, dsr, dcd;
int ctsbackoff;
int hup_dsr, hup_dcd;
int dohup;
Rendez r;
};
extern Uart* consuart;
void (*lprint)(char *, int);
struct Perf
{
ulong intrts;
ulong inintr;
ulong avg_inintr;
ulong inidle;
ulong avg_inidle;
ulong last;
ulong period;
};
struct Watchdog
{
void (*enable)(void);
void (*disable)(void);
void (*restart)(void);
void (*stat)(char*, char*);
};
struct Watermark
{
int highwater;
int curr;
int max;
int hitmax;
char *name;
};
enum
{
Qstarve = (1<<0),
Qmsg = (1<<1),
Qclosed = (1<<2),
Qflow = (1<<3),
Qcoalesce = (1<<4),
Qkick = (1<<5),
};
#define DEVDOTDOT -1
#pragma varargck type "I" uchar*
#pragma varargck type "V" uchar*
#pragma varargck type "E" uchar*
#pragma varargck type "M" uchar*