#define	HZ		100
#define	MS2HZ		(1000/HZ)
#define	TK2SEC(t)	((t)/HZ)
enum {
Mhz	= 1000 * 1000,
Dogsectimeout = 4,
};
#define MS2TMR(t)	((ulong)(((uvlong)(t) * m->cpuhz)/1000))
#define US2TMR(t)	((ulong)(((uvlong)(t) * m->cpuhz)/1000000))
#define CONSOLE 0
typedef struct Conf	Conf;
typedef struct Confmem	Confmem;
typedef struct FPsave	FPsave;
typedef struct ISAConf	ISAConf;
typedef struct Isolated Isolated;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct Lowmemcache Lowmemcache;
typedef struct Memcache	Memcache;
typedef struct MMMU	MMMU;
typedef struct Mach	Mach;
typedef u32int Mreg;
typedef struct Notsave	Notsave;
typedef struct Page	Page;
typedef struct Pcisiz Pcisiz;
typedef struct Pcidev Pcidev;
typedef struct PhysUart	PhysUart;
typedef struct PMMU	PMMU;
typedef struct Proc	Proc;
typedef u32int		PTE;
typedef struct Soc	Soc;
typedef struct Uart	Uart;
typedef struct Ureg	Ureg;
typedef uvlong		Tval;
#pragma incomplete Pcidev
#pragma incomplete Ureg
#define MAXSYSARG	5
#define AOUT_MAGIC	(E_MAGIC)
struct Lock
{
ulong	key;
u32int	sr;
uintptr	pc;
Proc*	p;
Mach*	m;
int	isilock;
};
struct Label
{
uintptr	sp;
uintptr	pc;
};
enum {
Maxfpregs	= 32,
Nfpctlregs	= 16,
};
struct FPsave
{
ulong	status;
ulong	control;
ulong	regs[Maxfpregs][3];
int	fpstate;
uintptr	pc;
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
uintptr	base;
usize	npage;
uintptr	limit;
uintptr	kbase;
uintptr	klimit;
};
struct Conf
{
ulong	nmach;
ulong	nproc;
Confmem	mem[1];
ulong	npage;
usize	upages;
ulong	copymode;
ulong	ialloc;
ulong	pipeqsize;
ulong	nimage;
ulong	nswap;
int	nswppo;
ulong	hz;
ulong	mhz;
int	monitor;
};
struct Notsave {
int	emptiness;
};
struct MMMU
{
PTE*	mmul1;
int	mmul1lo;
int	mmul1hi;
int	mmupid;
};
#define NCOLOR	1
struct PMMU
{
Page*	mmul2;
Page*	mmul2cache;
};
#include "../port/portdat.h"
struct Mach
{
int	machno;
uintptr	splpc;
Proc*	proc;
MMMU;
int	flushmmu;
ulong	ticks;
Label	sched;
Lock	alarmlock;
void*	alarm;
int	inclockintr;
Proc*	readied;
ulong	schedticks;
int	cputype;
ulong	delayloop;
int	tlbfault;
int	tlbpurge;
int	pfault;
int	cs;
int	syscall;
int	load;
int	intr;
uvlong	fastclock;
uvlong	inidle;
ulong	spuriousintr;
int	lastintr;
int	ilockdepth;
Perf	perf;
int	probing;
int	trapped;
Lock	probelock;
int	inidlehands;
int	cpumhz;
uvlong	cpuhz;
uvlong	cyclefreq;
int	havefp;
int	havefpvalid;
int	fpon;
int	fpconfiged;
int	fpnregs;
ulong	fpscr;
int	fppid;
uintptr	fppc;
int	fpcnt;
u32int	sfiq[5];
u32int	sirq[5];
u32int	sund[5];
u32int	sabt[5];
u32int	smon[5];
u32int	ssys[5];
int	stack[1];
};
typedef void		KMap;
#define	VA(k)		((uintptr)(k))
#define	kmap(p)		(KMap*)((p)->pa|kseg0)
#define	kunmap(k)
struct
{
Lock;
int	machs;
int	wfi;
int	stopped;
int	exiting;
int	ispanic;
int	thunderbirdsarego;
}active;
extern register Mach* m;
extern register Proc* up;
typedef uchar Cacheline[CACHELINESZ];
struct Isolated {
Cacheline c0;
ulong	word;
Cacheline c1;
};
extern Memcache cachel[];
extern ulong intrcount[MAXMACH];
extern int irqtooearly;
extern uintptr kseg0;
extern Isolated l1ptstable;
extern uchar *l2pages;
extern Mach* machaddr[MAXMACH];
extern ulong memsize;
extern int navailcpus;
extern int normalprint;
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
#define	MACHP(n) machaddr[n]
#ifdef _DBGC_
#define DBGFLG		(dbgflg[_DBGC_])
#else
#define DBGFLG		(0)
#endif
int vflag;
extern char dbgflg[256];
#define dbgprint	print
typedef struct {
ulong	port;
int	size;
} Devport;
struct DevConf
{
ulong	intnum;
char	*type;
int	nports;
Devport	*ports;
};
struct Memcache {
uint	waysh;
uint	setsh;
uint	log2linelen;
uint	level;
uint	type;
uint	external;
uint	l1ip;
uint	nways;
uint	nsets;
uint	linelen;
uint	setsways;
};
enum Cachetype {
Nocache,
Ionly,
Donly,
Splitid,
Unified,
};
enum {
Intcache,
Extcache,
};
struct Lowmemcache {
uint	l1waysh;
uint	l1setsh;
uint	l2waysh;
uint	l2setsh;
};
enum {
Cawt	= 1 << 31,
Cawb	= 1 << 30,
Cara	= 1 << 29,
Cawa	= 1 << 28,
};
typedef struct Cacheimpl Cacheimpl;
struct Cacheimpl {
void	(*info)(Memcache *);
void	(*on)(void);
void	(*off)(void);
void	(*inv)(void);
void	(*wb)(void);
void	(*wbinv)(void);
void	(*invse)(void *, int);
void	(*wbse)(void *, int);
void	(*wbinvse)(void *, int);
};
Cacheimpl *l2cache, *allcache, *nocache, *l1cache;
enum Dmamode {
Const,
Postincr,
Index,
Index2,
};
enum Irqs {
Cpu0irq		= 0,
Cpu1irq,
Cpu15irq	= 15,
Glbtmrirq	= 27,
Loctmrirq	= 29,
Wdtmrirq	= 30,
Ctlr0base	= (1+0)*32,
Tn0irq		= Ctlr0base + 0,
Tn1irq		= Ctlr0base + 1,
Rtcirq		= Ctlr0base + 2,
Ctlr1base	= (1+1)*32,
Uartirq		= Ctlr1base + 4,
Tn2irq		= Ctlr1base + 9,
Tn3irq		= Ctlr1base + 10,
Ctlr2base	= (1+2)*32,
Extpmuirq	= Ctlr2base + 22,
Ctlr3base	= (1+3)*32,
Pcieirq		= Ctlr3base + 2,
};
struct Soc {
uintptr clkrst;
uintptr	power;
uintptr	exceptvec;
uintptr	sema;
uintptr	l2cache;
uintptr	flow;
uintptr	scu;
uintptr	intr;
uintptr	glbtmr;
uintptr	loctmr;
uintptr	intrdist;
uintptr	uart[5];
uintptr	rtc;
uintptr	tmr[4];
uintptr	µs;
uintptr	pci;
uintptr	ether;
uintptr	ehci;
uintptr	ide;
uintptr	nand;
uintptr	nor;
uintptr	spi[4];
uintptr	twsi;
uintptr	mmc[4];
uintptr	gpio[7];
} soc;
extern Soc soc;