typedef struct Conf	Conf;
typedef struct FPU	FPU;
typedef struct FPenv	FPenv;
typedef ulong Instr;
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
typedef struct Segdesc	Segdesc;
typedef struct Ureg	Ureg;
typedef struct Vctl	Vctl;
#pragma incomplete Ureg
#pragma incomplete Vctl
struct Lock
{
ulong	key;
ulong	sr;
ulong	pc;
ulong	pri;
};
struct Label
{
ulong	sp;
ulong	pc;
};
enum
{
FPINIT,
FPACTIVE,
FPINACTIVE,
};
struct FPenv
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
};
struct	FPU
{
FPenv	env;
uchar	regs[80];
};
struct Conf
{
ulong	nmach;
ulong	nproc;
ulong	monitor;
ulong	npage0;
ulong	npage1;
ulong	npage;
ulong	base0;
ulong	base1;
ulong	copymode;
ulong	ialloc;
ulong	pipeqsize;
int	nuart;
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
ulong	cr3;
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
Proc*	externup;
ulong	ticks;
Proc*	proc;
Label	sched;
Lock	alarmlock;
void*	alarm;
int	inclockintr;
int	nrdy;
int	ilockdepth;
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
uvlong	tscoff;
int	intr;
ulong	spuriousintr;
int	lastintr;
vlong	mtrrcap;
vlong	mtrrdef;
vlong	mtrrfix[11];
vlong	mtrrvar[32];
int	stack[1];
};
struct
{
Lock;
int	machs;
int	exiting;
int	ispanic;
int	thunderbirdsarego;
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
void	(*clockenable)(void);
uvlong	(*fastclock)(uvlong*);
void	(*timerset)(uvlong);
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
extern int swcursor;
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