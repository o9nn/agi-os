typedef struct BIOS32si	BIOS32si;
typedef struct BIOS32ci	BIOS32ci;
typedef struct Conf	Conf;
typedef struct Confmem	Confmem;
typedef union FPsave	FPsave;
typedef struct FPssestate FPssestate;
typedef struct FPstate	FPstate;
typedef struct ISAConf	ISAConf;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct MMU	MMU;
typedef struct Mach	Mach;
typedef struct Notsave	Notsave;
typedef struct PCArch	PCArch;
typedef struct Pcidev	Pcidev;
typedef struct PCMmap	PCMmap;
typedef struct PCMslot	PCMslot;
typedef struct Page	Page;
typedef struct PMMU	PMMU;
typedef struct Proc	Proc;
typedef struct Segdesc	Segdesc;
typedef struct SFPssestate SFPssestate;
typedef vlong		Tval;
typedef struct Ureg	Ureg;
typedef struct Vctl	Vctl;
#pragma incomplete BIOS32si
#pragma incomplete Pcidev
#pragma incomplete Ureg
#define MAXSYSARG	5
#define KMESGSIZE (256*1024)
#define STAGESIZE 2048
#define AOUT_MAGIC	(I_MAGIC)
struct Lock
{
ulong	key;
ulong	sr;
ulong	pc;
Proc	*p;
Mach	*m;
ushort	isilock;
long	lockcycles;
};
struct Label
{
ulong	sp;
ulong	pc;
};
enum
{
FPinit=		0,
FPactive=	1,
FPinactive=	2,
FPillegal=	0x100,
};
struct	FPstate
{
ushort	control;
ushort	r1;
ushort	status;
ushort	r2;
ushort	tag;
ushort	r3;
ulong	pc;
ushort	selector;
ushort	r4;
ulong	operand;
ushort	oselector;
ushort	r5;
uchar	regs[80];
};
struct	FPssestate
{
ushort	fcw;
ushort	fsw;
ushort	ftw;
ushort	fop;
ulong	fpuip;
ushort	cs;
ushort	r1;
ulong	fpudp;
ushort	ds;
ushort	r2;
ulong	mxcsr;
ulong	mxcsr_mask;
uchar	xregs[480];
};
struct	SFPssestate
{
FPssestate;
uchar	alignpad[FPalign];
ulong	magic;
};
union FPsave {
FPstate;
SFPssestate;
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
ulong	monitor;
Confmem	mem[4];
ulong	npage;
ulong	upages;
ulong	nimage;
ulong	nswap;
int	nswppo;
ulong	base0;
ulong	base1;
ulong	copymode;
ulong	ialloc;
ulong	pipeqsize;
int	nuart;
};
#define NCOLOR 1
struct PMMU
{
Page*	mmupdb;
Page*	mmufree;
Page*	mmuused;
Page*	kmaptable;
uint	lastkmap;
int	nkmap;
};
struct Notsave
{
ulong	svflags;
ulong	svcs;
ulong	svss;
};
#include "../port/portdat.h"
typedef struct {
ulong	link;
ulong	esp0;
ulong	ss0;
ulong	esp1;
ulong	ss1;
ulong	esp2;
ulong	ss2;
ulong	xcr3;
ulong	eip;
ulong	eflags;
ulong	eax;
ulong 	ecx;
ulong	edx;
ulong	ebx;
ulong	esp;
ulong	ebp;
ulong	esi;
ulong	edi;
ulong	es;
ulong	cs;
ulong	ss;
ulong	ds;
ulong	fs;
ulong	gs;
ulong	ldt;
ulong	iomap;
} Tss;
struct Segdesc
{
ulong	d0;
ulong	d1;
};
struct Mach
{
int	machno;
ulong	splpc;
ulong*	pdb;
Tss*	tss;
Segdesc	*gdt;
Proc*	proc;
Proc*	externup;
Page*	pdbpool;
int	pdbcnt;
ulong	ticks;
Label	sched;
Lock	alarmlock;
void*	alarm;
int	inclockintr;
Proc*	readied;
ulong	schedticks;
int	tlbfault;
int	tlbpurge;
int	pfault;
int	cs;
int	syscall;
int	load;
int	intr;
int	flushmmu;
int	ilockdepth;
Perf	perf;
ulong	spuriousintr;
int	lastintr;
int	loopconst;
Lock	apictimerlock;
int	cpumhz;
uvlong	cyclefreq;
uvlong	cpuhz;
int	cpuidax;
int	cpuiddx;
char	cpuidid[16];
char*	cpuidtype;
int	havetsc;
int	havepge;
uvlong	tscticks;
int	pdballoc;
int	pdbfree;
FPsave	*fpsavalign;
vlong	mtrrcap;
vlong	mtrrdef;
vlong	mtrrfix[11];
vlong	mtrrvar[32];
int	stack[1];
};
typedef struct KMap		KMap;
#define	VA(k)		((void*)(k))
KMap*	kmap(Page*);
void	kunmap(KMap*);
struct
{
Lock;
int	machs;
int	exiting;
int	ispanic;
int	thunderbirdsarego;
int	rebooting;
}active;
struct PCArch
{
char*	id;
int	(*ident)(void);
void	(*reset)(void);
int	(*serialpower)(int);
int	(*modempower)(int);
void	(*intrinit)(void);
int	(*intrenable)(Vctl*);
int	(*intrvecno)(int);
int	(*intrdisable)(int);
void	(*introff)(void);
void	(*intron)(void);
void	(*clockenable)(void);
uvlong	(*fastclock)(uvlong*);
void	(*timerset)(uvlong);
void	(*resetothers)(void);
};
enum {
Fpuonchip = 1<<0,
Vmex	= 1<<1,
Pse	= 1<<3,
Tsc	= 1<<4,
Cpumsr	= 1<<5,
Pae	= 1<<6,
Mce	= 1<<7,
Cmpxchg8b = 1<<8,
Cpuapic	= 1<<9,
Mtrr	= 1<<12,
Pge	= 1<<13,
Pse2	= 1<<17,
Clflush = 1<<19,
Mmx	= 1<<23,
Fxsr	= 1<<24,
Sse	= 1<<25,
Sse2	= 1<<26,
};
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
extern PCArch	*arch;
Mach* machp[MAXMACH];
#define	MACHP(n)	(machp[n])
extern Mach	*m;
#define up	(((Mach*)MACHADDR)->externup)
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
typedef struct BIOS32ci {
u32int	eax;
u32int	ebx;
u32int	ecx;
u32int	edx;
u32int	esi;
u32int	edi;
} BIOS32ci;