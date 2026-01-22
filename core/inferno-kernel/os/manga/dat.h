typedef struct Conf	Conf;
typedef struct Dma	Dma;
typedef struct FPU	FPU;
typedef struct FPenv	FPenv;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct Mach	Mach;
typedef struct Ureg	Ureg;
typedef struct ISAConf	ISAConf;
typedef struct Pcidev Pcidev;
typedef ulong Instr;
struct Conf
{
ulong	nmach;
ulong	nproc;
ulong	npage0;
ulong	npage1;
ulong	topofmem;
ulong	npage;
ulong	base0;
ulong	base1;
ulong	ialloc;
int		useminicache;
int		textwrite;
int		portrait;
};
#define NISAOPT 8
struct ISAConf {
char	type[KNAMELEN];
ulong	port;
ulong	irq;
int	itype;
ulong	dma;
ulong	mem;
ulong	size;
ulong	freq;
int	nopt;
char	*opt[NISAOPT];
};
enum
{
FPINIT,
FPACTIVE,
FPINACTIVE,
};
struct	FPenv
{
ulong	status;
ulong	control;
ushort	fpistate;
ulong	regs[8][3];
};
struct	FPU
{
FPenv	env;
};
struct Label
{
ulong	sp;
ulong	pc;
};
struct Lock
{
ulong	key;
ulong	sr;
ulong	pc;
int	pri;
};
#include "../port/portdat.h"
struct Mach
{
ulong	splpc;
int	machno;
ulong	ticks;
Proc	*proc;
Label	sched;
Lock	alarmlock;
void	*alarm;
ulong	cpuhz;
ulong	delayloop;
ulong	fiqstack[4];
ulong	irqstack[4];
ulong	abtstack[4];
ulong	undstack[4];
int	stack[1];
};
#define	MACHP(n)	(n == 0 ? (Mach*)(MACHADDR) : (Mach*)0)
extern Mach *m;
extern Proc *up;
typedef struct Vectorpage {
void	(*vectors[8])(void);
uint	vtable[8];
} Vectorpage;
extern Vectorpage *page0;