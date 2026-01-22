#define HZ 100
#define MS2HZ (1000/HZ)
#define TK2SEC(t) ((t)/HZ)
enum {
Mhz = 1000 * 1000,
};
typedef struct Conf Conf;
typedef struct Confmem Confmem;
typedef struct FPsave FPsave;
typedef struct ISAConf ISAConf;
typedef struct Label Label;
typedef struct Lock Lock;
typedef struct Memcache Memcache;
typedef struct MMMU MMMU;
typedef struct Mach Mach;
typedef struct Notsave Notsave;
typedef struct Page Page;
typedef struct PhysUart PhysUart;
typedef struct PMMU PMMU;
typedef struct Proc Proc;
typedef u32int PTE;
typedef struct Uart Uart;
typedef struct Ureg Ureg;
typedef uvlong Tval;
#pragma incomplete Ureg
#define MAXSYSARG 5
#define AOUT_MAGIC (E_MAGIC)
struct Lock
{
ulong key;
u32int sr;
uintptr pc;
Proc* p;
Mach* m;
int isilock;
};
struct Label
{
uintptr sp;
uintptr pc;
};
enum {
Maxfpregs = 32,
Nfpctlregs = 16,
};
struct FPsave
{
ulong status;
ulong control;
ulong regs[Maxfpregs][3];
int fpstate;
uintptr pc;
};
enum
{
FPinit,
FPactive,
FPinactive,
FPemu,
FPillegal= 0x100,
};
struct Confmem
{
uintptr base;
usize npage;
uintptr limit;
uintptr kbase;
uintptr klimit;
};
struct Conf
{
ulong nmach;
ulong nproc;
Confmem mem[1];
ulong npage;
usize upages;
ulong copymode;
ulong ialloc;
ulong pipeqsize;
ulong nimage;
ulong nswap;
int nswppo;
ulong hz;
ulong mhz;
int monitor;
};
struct Notsave {
int emptiness;
};
struct MMMU
{
PTE* mmul1;
int mmul1lo;
int mmul1hi;
int mmupid;
};
#define NCOLOR 1
struct PMMU
{
Page* mmul2;
Page* mmul2cache;
};
#include "../port/portdat.h"
struct Mach
{
int machno;
uintptr splpc;
Proc* proc;
MMMU;
int flushmmu;
ulong ticks;
Label sched;
Lock alarmlock;
void* alarm;
Proc* readied;
ulong schedticks;
int cputype;
ulong delayloop;
int tlbfault;
int tlbpurge;
int pfault;
int cs;
int syscall;
int load;
int intr;
uvlong fastclock;
uvlong inidle;
ulong spuriousintr;
int lastintr;
int ilockdepth;
Perf perf;
int cpumhz;
uvlong cpuhz;
uvlong cyclefreq;
int havefp;
int havefpvalid;
int fpon;
int fpconfiged;
int fpnregs;
ulong fpscr;
int fppid;
uintptr fppc;
int fpcnt;
u32int sfiq[5];
u32int sirq[5];
u32int sund[5];
u32int sabt[5];
u32int smon[5];
u32int ssys[5];
int stack[1];
};
typedef void KMap;
#define VA(k) ((uintptr)(k))
#define kmap(p) (KMap*)((p)->pa|kseg0)
#define kunmap(k)
struct
{
Lock;
int machs;
int exiting;
int ispanic;
}active;
extern register Mach* m;
extern register Proc* up;
extern uintptr kseg0;
extern Mach* machaddr[MAXMACH];
extern ulong memsize;
extern int normalprint;
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
#define MACHP(n) (machaddr[n])
#ifdef _DBGC_
#define DBGFLG (dbgflg[_DBGC_])
#else
#define DBGFLG (0)
#endif
int vflag;
extern char dbgflg[256];
#define dbgprint print
typedef struct {
ulong port;
int size;
} Devport;
struct DevConf
{
ulong intnum;
char *type;
int nports;
Devport *ports;
};