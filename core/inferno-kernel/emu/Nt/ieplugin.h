typedef struct Pop Pop;
typedef struct Iop Iop;
typedef struct IPoint IPoint;
typedef struct IRectangle IRectangle;
typedef struct Plugin Plugin;
enum {
Pgfxkey,
Pmouse,
Iattachscr,
Iflushscr,
Isetcur,
Idrawcur,
Iquit,
};
struct Pop {
int	op;
union {
int key;
struct {
int	x;
int	y;
int	b;
int	modify;
} m;
} u;
};
struct IPoint
{
LONG	x;
LONG	y;
};
struct IRectangle
{
IPoint	min;
IPoint	max;
};
struct Iop {
int	op;
int	val;
union {
IRectangle	r;
} u;
};
#define PI_NCLOSE	2
struct Plugin {
LONG sz;
HANDLE	conin;
HANDLE	conout;
HANDLE	datain;
HANDLE	dopop;
HANDLE	popdone;
HANDLE	doiop;
HANDLE	iopdone;
HANDLE	closehandles[PI_NCLOSE];
Pop pop;
Iop iop;
int Xsize;
int Ysize;
ULONG cdesc;
int cflag;
ULONG screen[1];
};
#define IOP	(plugin->iop)
#define POP	(plugin->pop)