enum {
Maxsteps = 200 * 100 * 2,
Admitted		= 0x01,
Sporadic		= 0x02,
Yieldonblock		= 0x04,
Sendnotes		= 0x08,
Deadline		= 0x10,
Yield			= 0x20,
Extratime		= 0x40,
Infinity = ~0ULL,
};
typedef struct Edf		Edf;
struct Edf {
vlong		D;
vlong		Delta;
vlong		T;
vlong		C;
vlong		S;
vlong		r;
vlong		d;
vlong		t;
vlong		s;
vlong		testDelta;
int			testtype;
vlong		testtime;
Proc		*testnext;
ushort		flags;
Timer;
vlong		edfused;
vlong		extraused;
vlong		aged;
ulong		periods;
ulong		missed;
};
extern Lock	edftestlock;
#pragma	varargck	type	"t"		vlong
#pragma	varargck	type	"U"		uvlong
Edf*		edflock(Proc*);
void		edfunlock(void);