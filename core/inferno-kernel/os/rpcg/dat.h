typedef struct Conf	Conf;
typedef struct FPU	FPU;
typedef struct FPenv	FPenv;
typedef struct IMM	IMM;
typedef struct Irqctl	Irqctl;
typedef struct ISAConf	ISAConf;
typedef struct Label	Label;
typedef struct Lock	Lock;
typedef struct Mach	Mach;
typedef struct Map	Map;
typedef struct Power Power;
typedef struct RMap RMap;
typedef struct Ureg	Ureg;
typedef ulong Instr;
#define	MACHP(n)	(n==0? &mach0 : *(Mach**)0)
struct	Lock
{
ulong	key;
ulong	pc;
ulong	sr;
int	pri;
};
struct	Label
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
union {
double	fpscrd;
struct {
ulong	pad;
ulong	fpscr;
};
};
int	fpistate;
ulong	emreg[32][3];
};
struct	FPU
{
double	fpreg[32];
FPenv	env;
};
struct Conf
{
ulong	nmach;
ulong	nproc;
ulong	npage0;
ulong	npage1;
ulong	npage;
ulong	base0;
ulong	base1;
ulong	ialloc;
int	nscc;
ulong	smcuarts;
ulong	sccuarts;
int	nocts2;
uchar*	nvrambase;
ulong	nvramsize;
};
#include "../port/portdat.h"
struct Mach
{
int	machno;
ulong	splpc;
int	mmask;
ulong	ticks;
Proc	*proc;
Label	sched;
Lock	alarmlock;
void	*alarm;
int	nrdy;
int	speed;
long	oscclk;
long	cpuhz;
long	clockgen;
int	cputype;
ulong	delayloop;
ulong*	bcsr;
IMM*	iomem;
int	stack[1];
};
extern	Mach	mach0;
#define NISAOPT		8
struct ISAConf {
char*	type;
ulong	port;
ulong	irq;
ulong	mem;
int	dma;
ulong	size;
ulong	freq;
uchar	bus;
int	nopt;
char*	opt[NISAOPT];
};
struct Map {
int	size;
ulong	addr;
};
struct RMap {
char*	name;
Map*	map;
Map*	mapend;
Lock;
};
struct Power {
Dev*	dev;
int	(*powerdown)(Power*);
int	(*powerup)(Power*);
int	state;
void*	arg;
};
extern register Mach	*m;
extern register Proc	*up;