typedef struct Conf Conf;
typedef struct Confmem Confmem;
typedef struct FPsave FPsave;
typedef struct ISAConf ISAConf;
typedef struct Label Label;
typedef struct Lock Lock;
typedef struct Mach Mach;
typedef struct Notsave Notsave;
typedef struct Page Page;
typedef struct PCArch PCArch;
typedef struct Pcidev Pcidev;
typedef struct PMMU PMMU;
typedef struct Proc Proc;
typedef struct Sys Sys;
typedef struct Ureg Ureg;
typedef struct Vctl Vctl;
typedef long Tval;
#pragma incomplete Ureg
#define MAXSYSARG 5
#define AOUT_MAGIC Q_MAGIC
struct Lock
{
ulong key;
ulong sr;
ulong pc;
Proc *p;
Mach *m;
ushort isilock;
};
struct Label
{
ulong sp;
ulong pc;
};
enum
{
FPinit,
FPactive,
FPinactive,
FPillegal= 0x100,
};
struct FPsave
{
double fpreg[32];
union {
double fpscrd;
struct {
ulong pad;
ulong fpscr;
};
};
};
struct Confmem
{
ulong base;
ulong npage;
ulong kbase;
ulong klimit;
};
struct Conf
{
ulong nmach;
ulong nproc;
Confmem mem[1];
ulong npage;
ulong upages;
ulong nimage;
ulong nswap;
int nswppo;
ulong copymode;
int monitor;
ulong ialloc;
ulong pipeqsize;
};
#define NCOLOR 1
struct PMMU
{
int mmupid;
};
struct Notsave
{
ulong UNUSED;
};
#include "../port/portdat.h"
typedef void KMap;
#define VA(k) ((ulong)(k))
#define kmap(p) (KMap*)((p)->pa|KZERO)
#define kunmap(k)
struct Mach
{
int machno;
ulong splpc;
Proc *proc;
ulong ticks;
Label sched;
Lock alarmlock;
void *alarm;
int inclockintr;
int cputype;
ulong loopconst;
Proc* readied;
ulong schedticks;
vlong cpuhz;
ulong bushz;
ulong dechz;
ulong tbhz;
uvlong cyclefreq;
ulong pcclast;
uvlong fastclock;
Perf perf;
int tlbfault;
int tlbpurge;
int pfault;
int cs;
int syscall;
int load;
int intr;
int flushmmu;
int ilockdepth;
ulong ptabbase;
int slotgen;
int mmupid;
int sweepcolor;
int trigcolor;
Rendez sweepr;
ulong spuriousintr;
int lastintr;
int stack[1];
};
struct
{
Lock;
short machs;
short exiting;
short ispanic;
}active;
#define NISAOPT 8
struct ISAConf {
char *type;
ulong port;
int irq;
ulong dma;
ulong mem;
ulong size;
ulong freq;
int nopt;
char *opt[NISAOPT];
};
#define MACHP(n) ((Mach *)((int)&mach0+n*BY2PG))
extern Mach mach0;
extern register Mach *m;
extern register Proc *up;
extern FPsave initfp;