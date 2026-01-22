typedef struct Conf Conf;
typedef struct FPenv FPenv;
typedef struct FPU FPU;
typedef struct Label Label;
typedef struct Lock Lock;
typedef struct Mach Mach;
typedef struct Ureg Ureg;
typedef struct Lance Lance;
typedef struct Lancemem Lancemem;
typedef struct Etherpkt Etherpkt;
typedef struct Lancepkt Lancepkt;
typedef ulong Instr;
struct Conf
{
int nmach;
int nproc;
ulong monitor;
char ss2;
char ss2cachebug;
int ncontext;
int vacsize;
int vaclinesize;
ulong npage0;
ulong npage1;
ulong base0;
ulong base1;
ulong ialloc;
ulong npage;
int copymode;
ulong ipif;
ulong ip;
ulong arp;
ulong frag;
};
enum
{
FPINIT,
FPACTIVE,
FPINACTIVE,
};
struct FPenv
{
ulong status;
ulong pad;
};
struct FPU
{
double regs[17];
FPenv env;
};
struct Label
{
ulong sp;
ulong pc;
};
struct Lock
{
ulong key;
ulong pc;
ulong sr;
int pri;
};
#include "../port/portdat.h"
struct Mach
{
ulong ticks;
int machno;
Proc *proc;
Label sched;
Lock alarmlock;
void *alarm;
ulong *contexts;
ulong *ctx;
int fptrap;
int nrdy;
int stack[1];
};
#define BSWP 0x4
#define ACON 0x2
#define BCON 0x1
struct Lancepkt
{
uchar d[6];
uchar s[6];
uchar type[2];
uchar data[1500];
uchar crc[4];
};
struct Lance
{
ushort lognrrb;
ushort logntrb;
ushort nrrb;
ushort ntrb;
ushort *rap;
ushort *rdp;
ushort busctl;
uchar ea[6];
int sep;
ushort *lanceram;
Lancemem *lm;
Lancepkt *rp;
Lancepkt *tp;
Lancepkt *lrp;
Lancepkt *ltp;
};
typedef void KMap;
#define VA(k) ((ulong)(k))
#define kmap(p) (KMap*)((p)->pa|KZERO)
#define kunmap(k)
#define MACHP(n) (n==0? &mach0 : *(Mach**)0)
extern Mach *m;
extern Proc *up;
extern Mach mach0;
#define swcursor 1