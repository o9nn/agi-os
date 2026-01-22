typedef struct Conf	Conf;
typedef struct Confmem	Confmem;
typedef struct FPsave	FPsave;
typedef struct KMap	KMap;
typedef struct Lance	Lance;
typedef struct Lancemem	Lancemem;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct Mach	Mach;
typedef struct MMU	MMU;
typedef struct Notsave	Notsave;
typedef struct Pcidev	Pcidev;
typedef struct PMMU	PMMU;
typedef struct Softtlb	Softtlb;
typedef struct Ureg	Ureg;
typedef struct Proc	Proc;
typedef uvlong		Tval;
#pragma incomplete Pcidev
#define MAXSYSARG	5
#define AOUT_MAGIC	V_MAGIC || magic==M_MAGIC
#define BOOT_MAGIC	(0x160<<16) || magic == ((0x160<<16)|3)
struct Lock
{
ulong	key;
ulong	sr;
ulong	pc;
Proc	*p;
Mach	*m;
ushort	isilock;
};
struct Label
{
ulong	sp;
ulong	pc;
};
struct Confmem
{
ulong	base;
ulong	npage;
ulong	kbase;
ulong	klimit;
};
struct Conf
{
ulong	nmach;
ulong	nproc;
Confmem	mem[1];
ulong	npage;
ulong	upages;
ulong	nimage;
ulong	nswap;
int	nswppo;
ulong	copymode;
ulong	ialloc;
ulong	pipeqsize;
int	nuart;
};
enum
{
FPinit,
FPactive,
FPinactive,
FPemu,
FPillegal= 0x100,
};
enum {
Nfpregs		= 32,
};
struct FPsave
{
ulong	reg[Nfpregs];
union {
ulong	fpstatus;
ulong	fpcontrol;
};
int	fpdelayexec;
uintptr	fpdelaypc;
ulong	fpdelaysts;
uintptr	fppc;
int	fpcnt;
};
int fpemudebug;
struct PMMU
{
int	pidonmach[MAXMACH];
};
struct Notsave
{
ulong	nonempty;
};
#include "../port/portdat.h"
struct Mach
{
int	machno;
Softtlb*stb;
Proc*	proc;
ulong	splpc;
ulong	tlbfault;
ulong	ktlbfault;
ulong	utlbfault;
ulong	tlbpurge;
ulong	ticks;
Label	sched;
void*	alarm;
int	lastpid;
Proc*	pidproc[NTLBPID];
KMap*	kactive;
int	knext;
uchar	ktlbx[NTLB];
uchar	ktlbnext;
int	speed;
ulong	delayloop;
ulong	fairness;
int	flushmmu;
int	inclockintr;
int	ilockdepth;
Perf	perf;
uvlong	cyclefreq;
ulong	lastcount;
uvlong	fastticks;
ulong	hz;
ulong	maxperiod;
ulong	minperiod;
Proc*	readied;
ulong	schedticks;
int	pfault;
int	cs;
int	syscall;
int	load;
int	intr;
int	hashcoll;
int	paststartup;
int	stack[1];
};
struct KMap
{
Ref;
ulong	virt;
ulong	phys0;
ulong	phys1;
KMap*	next;
KMap*	konmach[MAXMACH];
Page*	pg;
ulong	pc;
};
#define	VA(k)		((k)->virt)
#define PPN(x)		((ulong)(x)>>6)
struct Softtlb
{
ulong	virt;
ulong	phys0;
ulong	phys1;
};
struct
{
Lock;
long	machs;
short	exiting;
int	ispanic;
}active;
extern KMap kpte[];
extern register Mach	*m;
extern register Proc	*up;
extern FPsave initfp;
extern	int normalprint;