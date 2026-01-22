typedef struct Conf	Conf;
typedef struct Confmem	Confmem;
typedef struct FPsave	FPsave;
typedef struct ISAConf	ISAConf;
typedef struct Imap	Imap;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct Mach	Mach;
typedef struct Notsave	Notsave;
typedef struct PCArch	PCArch;
typedef struct PMMU	PMMU;
typedef struct Page	Page;
typedef struct Pcidev	Pcidev;
typedef struct Proc	Proc;
typedef struct Sys	Sys;
typedef vlong		Tval;
typedef struct Ureg	Ureg;
typedef struct Vctl	Vctl;
#pragma incomplete Ureg
#pragma incomplete Imap
#pragma incomplete Mach
#define MAXSYSARG	5
#define AOUT_MAGIC	Q_MAGIC
struct Lock
{
ulong	key;
ulong	sr;
ulong	pc;
Proc	*p;
Mach	*m;
ulong	pid;
ushort	isilock;
};
struct Label
{
ulong	sp;
ulong	pc;
};
enum
{
FPinit = 0,
FPactive = 1,
FPinactive = 2,
FPillegal = 0x100,
};
struct FPsave
{
double	fpreg[32];
union {
double	fpscrd;
struct {
ulong	pad;
ulong	fpscr;
};
};
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
Confmem	mem[2];
ulong	npage0;
ulong	npage1;
ulong	npage;
ulong	base0;
ulong	base1;
ulong	upages;
ulong	nimage;
ulong	nswap;
int	nswppo;
ulong	copymode;
int	monitor;
ulong	ialloc;
ulong	pipeqsize;
};
#define NCOLOR 1
struct PMMU
{
int	mmupid;
Ureg	*mmureg;
};
struct Notsave
{
ulong	UNUSED;
};
#include "../port/portdat.h"
typedef	void		KMap;
#define	VA(k)		((ulong)(k))
#define	kmap(p)		(KMap*)((p)->pa|KZERO)
#define	kunmap(k)
struct IMM;
typedef struct IMM IMM;
struct Mach
{
int	machno;
ulong	splpc;
Proc	*proc;
ulong	tlbfault;
ulong	imiss;
ulong	dmiss;
Imap*	imap;
#ifndef ucuconf
IMM*	immr;
#endif
ulong	ticks;
Label	sched;
Lock	alarmlock;
void	*alarm;
int	inclockintr;
int	cputype;
ulong	loopconst;
Perf	perf;
Proc*	readied;
ulong	schedticks;
ulong	clkin;
ulong	vco_out;
vlong	cpuhz;
uvlong	cyclefreq;
ulong	bushz;
ulong	dechz;
ulong	tbhz;
ulong	cpmhz;
ulong	brghz;
ulong	pcclast;
uvlong	fastclock;
int	tlbpurge;
int	pfault;
int	cs;
int	syscall;
int	load;
int	intr;
int	flushmmu;
int	ilockdepth;
ulong	ptabbase;
int	slotgen;
int	mmupid;
int	sweepcolor;
int	trigcolor;
Rendez	sweepr;
ulong	spuriousintr;
int	lastintr;
int	stack[1];
};
struct
{
Lock;
short	machs;
short	exiting;
short	ispanic;
}active;
#define NISAOPT		8
struct ISAConf {
char	*type;
ulong	port;
int	irq;
ulong	dma;
ulong	mem;
ulong	size;
ulong	freq;
int	nopt;
char	*opt[NISAOPT];
};
struct Vctl {
Vctl*	next;
char	name[KNAMELEN];
int	isintr;
int	irq;
void	(*f)(Ureg*, void*);
void*	a;
};
extern Mach mach0;
extern register Mach *m;
extern register Proc *up;
extern FPsave initfp;