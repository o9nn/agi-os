typedef struct BIOS32si	BIOS32si;
typedef struct BIOS32ci	BIOS32ci;
typedef struct Conf	Conf;
typedef struct Confmem	Confmem;
typedef struct FPsave	FPsave;
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
typedef vlong		Tval;
typedef struct Ureg	Ureg;
typedef struct Vctl	Vctl;
#pragma incomplete BIOS32si
#pragma incomplete Pcidev
#pragma incomplete Ureg
#define MAXSYSARG	5
#define BOOTLINE	((char*)CONFADDR)
#define BOOTLINELEN	64
#define BOOTARGS	((char*)(CONFADDR+BOOTLINELEN))
#define	BOOTARGSLEN	(3584-0x200-BOOTLINELEN)
#define	MAXCONF		100
enum {
Promptsecs	= 60,
};
char *confname[MAXCONF];
char *confval[MAXCONF];
int nconf;
#define KMESGSIZE 64
#define PCICONSSIZE 64
#define STAGESIZE 64
#define NAMELEN 28
#define	GSHORT(p)	(((p)[1]<<8)|(p)[0])
#define	GLSHORT(p)	(((p)[0]<<8)|(p)[1])
#define	GLONG(p)	((GSHORT(p+2)<<16)|GSHORT(p))
#define	GLLONG(p)	(((ulong)GLSHORT(p)<<16)|GLSHORT(p+2))
#define	PLLONG(p,v)	(p)[3]=(v);(p)[2]=(v)>>8;(p)[1]=(v)>>16;(p)[0]=(v)>>24
#define	PLVLONG(p,v)	(p)[7]=(v);(p)[6]=(v)>>8;(p)[5]=(v)>>16;(p)[4]=(v)>>24;\
(p)[3]=(v)>>32; (p)[2]=(v)>>40;\
(p)[1]=(v)>>48; (p)[0]=(v)>>56;
enum {
Stkpat =	0,
};
#define AOUT_MAGIC	(I_MAGIC)
struct Lock
{
ulong	magic;
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
struct	FPsave
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
extern int	v_flag;
typedef struct Apminfo {
int haveinfo;
int ax;
int cx;
int dx;
int di;
int ebx;
int esi;
} Apminfo;
extern Apminfo	apm;
typedef struct Mbi Mbi;
struct Mbi {
u32int	flags;
u32int	memlower;
u32int	memupper;
u32int	bootdevice;
u32int	cmdline;
u32int	modscount;
u32int	modsaddr;
u32int	syms[4];
u32int	mmaplength;
u32int	mmapaddr;
u32int	driveslength;
u32int	drivesaddr;
u32int	configtable;
u32int	bootloadername;
u32int	apmtable;
u32int	vbe[6];
};
enum {
Fmem		= 0x00000001,
Fbootdevice	= 0x00000002,
Fcmdline	= 0x00000004,
Fmods		= 0x00000008,
Fsyms		= 0x00000010,
Felf		= 0x00000020,
Fmmap		= 0x00000040,
Fdrives		= 0x00000080,
Fconfigtable	= 0x00000100,
Fbootloadername	= 0x00000200,
Fapmtable	= 0x00000400,
Fvbe		= 0x00000800,
};
typedef struct Mod Mod;
struct Mod {
u32int	modstart;
u32int	modend;
u32int	string;
u32int	reserved;
};
typedef struct MMap MMap;
struct MMap {
u32int	size;
u32int	base[2];
u32int	length[2];
u32int	type;
};
MMap mmap[32+1];
int nmmap;
Mbi *multibootheader;
enum {
Maxfile = 4096,
};
enum {
MORE, ENOUGH, FAIL
};
enum {
INITKERNEL,
READEXEC,
READ9TEXT,
READ9DATA,
READGZIP,
READEHDR,
READPHDR,
READEPAD,
READEDATA,
READE64HDR,
READ64PHDR,
READE64PAD,
READE64DATA,
TRYBOOT,
TRYEBOOT,
TRYE64BOOT,
INIT9LOAD,
READ9LOAD,
FAILED
};
typedef struct Execbytes Execbytes;
struct	Execbytes
{
uchar	magic[4];
uchar	text[4];
uchar	data[4];
uchar	bss[4];
uchar	syms[4];
uchar	entry[4];
uchar	spsz[4];
uchar	pcsz[4];
};
typedef struct {
Execbytes;
uvlong uvl[1];
} Exechdr;
typedef struct Boot Boot;
struct Boot {
int state;
Exechdr hdr;
uvlong	entry;
char *bp;
char *wp;
char *ep;
};
extern int	debugload;
extern Apminfo	apm;
extern Chan	*conschan;
extern char	*defaultpartition;
extern int	iniread;
extern u32int	memstart;
extern u32int	memend;
extern int	noclock;
extern int	pxe;
extern int	vga;
extern int	biosinited;
extern void _KTZERO(void);
#define KTZERO ((uintptr)_KTZERO)