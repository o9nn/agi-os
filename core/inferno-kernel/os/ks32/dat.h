typedef struct Conf Conf;
typedef struct FPU FPU;
typedef struct FPenv FPenv;
typedef struct Label Label;
typedef struct Lock Lock;
typedef struct Mach Mach;
typedef struct Ureg Ureg;
typedef struct ISAConf ISAConf;
typedef struct PCMmap PCMmap;
typedef struct PCIcfg PCIcfg;
typedef struct TouchPnt TouchPnt;
typedef struct TouchTrans TouchTrans;
typedef struct TouchCal TouchCal;
typedef struct Vmode Vmode;
typedef ulong Instr;
#define ISAOPTLEN 16
#define NISAOPT 8
struct Conf
{
ulong nmach;
ulong nproc;
ulong npage0;
ulong npage1;
ulong topofmem;
ulong npage;
ulong base0;
ulong base1;
ulong ialloc;
ulong flashbase;
ulong cpuspeed;
ulong pagetable;
int useminicache;
int cansetbacklight;
int cansetcontrast;
int remaplo;
int textwrite;
};
struct ISAConf {
char type[KNAMELEN];
ulong port;
ulong irq;
ulong sairq;
ulong dma;
ulong mem;
ulong size;
ulong freq;
int nopt;
char opt[NISAOPT][ISAOPTLEN];
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
ulong control;
ushort fpistate;
ulong regs[8][3];
};
struct FPU
{
FPenv env;
uchar regs[80];
};
struct Label
{
ulong sp;
ulong pc;
};
struct Lock
{
ulong key;
ulong sr;
ulong pc;
int pri;
};
#include "../port/portdat.h"
struct Mach
{
ulong ticks;
Proc *proc;
Label sched;
Lock alarmlock;
void *alarm;
int machno;
int nrdy;
int stack[1];
};
#define MACHP(n) (n == 0 ? (Mach*)(MACHADDR) : (Mach*)0)
extern Mach Mach0;
extern Mach *m;
extern Proc *up;
typedef struct MemBank {
uint pbase;
uint plimit;
uint vbase;
uint vlimit;
} MemBank;
enum {
DmaOUT= 0,
DmaIN= 1,
DmaLittle= 0,
DmaBig= 1,
DmaUDC= 0,
DmaSDLC= 2,
DmaUART0= 4,
DmaHSSP= 6,
DmaUART1= 7,
DmaUART2= 8,
DmaMCPaudio= 10,
DmaMCPtelecom= 12,
DmaSSP= 14,
};
enum touch_source {
TOUCH_READ_X1, TOUCH_READ_X2, TOUCH_READ_X3, TOUCH_READ_X4,
TOUCH_READ_Y1, TOUCH_READ_Y2, TOUCH_READ_Y3, TOUCH_READ_Y4,
TOUCH_READ_P1, TOUCH_READ_P2,
TOUCH_READ_RX1, TOUCH_READ_RX2,
TOUCH_READ_RY1, TOUCH_READ_RY2,
TOUCH_NUMRAWCAL = 10,
};
struct TouchPnt {
int x;
int y;
};
struct TouchTrans {
int xxm;
int xym;
int yxm;
int yym;
int xa;
int ya;
};
struct TouchCal {
TouchPnt p[4];
TouchPnt r[4][4];
TouchTrans t[4];
TouchPnt err;
TouchPnt var;
int ptp;
int ptr;
};
extern TouchCal touchcal;
struct Vmode {
int wid;
int hgt;
uchar d;
uchar hz;
ushort flags;
};
enum {
VMODE_MONO = 0x0001,
VMODE_COLOR = 0x0002,
VMODE_TFT = 0x0004,
VMODE_STATIC = 0x0010,
VMODE_PSEUDO = 0x0020,
VMODE_LINEAR = 0x0100,
VMODE_PAGED = 0x0200,
VMODE_PLANAR = 0x1000,
VMODE_PACKED = 0x2000,
VMODE_LILEND = 0x4000,
VMODE_BIGEND = 0x8000,
};
enum {
PCMready,
PCMeject,
PCMstschng,
};
#define swcursor 1