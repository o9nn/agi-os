#define nil		((void*)0)
typedef	unsigned short	ushort;
typedef	unsigned char	uchar;
typedef	unsigned long	ulong;
typedef	unsigned int	uint;
typedef	  signed char	schar;
typedef	long long	vlong;
typedef	unsigned long long uvlong;
typedef unsigned long	uintptr;
typedef unsigned long	usize;
typedef	uint		Rune;
typedef union FPdbleword FPdbleword;
typedef long		jmp_buf[2];
#define	JMPBUFSP	0
#define	JMPBUFPC	1
#define	JMPBUFDPC	0
typedef unsigned int	mpdigit;
typedef unsigned char	u8int;
typedef unsigned short	u16int;
typedef unsigned int	u32int;
typedef unsigned long long u64int;
#define	FPSFX	(1<<31)
#define	FPSEX	(1<<30)
#define	FPSVX	(1<<29)
#define	FPSOX	(1<<28)
#define	FPSUX	(1<<27)
#define	FPSZX	(1<<26)
#define	FPSXX	(1<<25)
#define	FPSVXSNAN (1<<24)
#define	FPSVXISI (1<<23)
#define	FPSVXIDI (1<<22)
#define	FPSVXZDZ (1<<21)
#define	FPSVXIMZ (1<<20)
#define	FPSVXVC	(1<<19)
#define	FPSFR	(1<<18)
#define	FPSFI	(1<<17)
#define	FPSFPRF	(1<<16)
#define	FPSFPCC	(0xF<<12)
#define	FPVXCVI	(1<<8)
#define	FPVE	(1<<7)
#define	FPOVFL	(1<<6)
#define	FPUNFL	(1<<5)
#define	FPZDIV	(1<<4)
#define	FPINEX	(1<<3)
#define	FPRMASK	(3<<0)
#define	FPRNR	(0<<0)
#define	FPRZ	(1<<0)
#define	FPRPINF	(2<<0)
#define	FPRNINF	(3<<0)
#define	FPPEXT	0
#define	FPPSGL	0
#define	FPPDBL	0
#define	FPPMASK	0
#define	FPINVAL	FPVE
#define	FPAOVFL	FPSOX
#define	FPAINEX	FPSXX
#define	FPAUNFL	FPSUX
#define	FPAZDIV	FPSZX
#define	FPAINVAL	FPSVX
union FPdbleword
{
double	x;
struct {
ulong hi;
ulong lo;
};
};
typedef	char*	va_list;
#define va_start(list, start) list =\
(sizeof(start) < 4?\
(char*)((int*)&(start)+1):\
(char*)(&(start)+1))
#define va_end(list)\
USED(list)
#define va_arg(list, mode)\
((sizeof(mode) <= 4)?\
((list += 4), (mode*)list)[-1]:\
(signof(mode) != signof(double))?\
((list += sizeof(mode)), (mode*)list)[-1]:\
((list = (char*)((uintptr)(list+7) & ~7) + sizeof(mode)), (mode*)list)[-1])