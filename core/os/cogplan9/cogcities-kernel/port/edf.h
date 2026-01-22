enum {
Maxsteps = 200 * 100 * 2,
Admitted = 0x01,
Sporadic = 0x02,
Yieldonblock = 0x04,
Sendnotes = 0x08,
Deadline = 0x10,
Yield = 0x20,
Extratime = 0x40,
Infinity = ~0ULL,
};
typedef struct Edf Edf;
struct Edf {
long D;
long Delta;
long T;
long C;
long S;
long r;
long d;
long t;
long s;
long testDelta;
int testtype;
long testtime;
Proc *testnext;
ushort flags;
Timer;
long edfused;
long extraused;
long aged;
ulong periods;
ulong missed;
};
extern Lock edftestlock;
#pragma varargck type "t" long
#pragma varargck type "U" uvlong
Edf* edflock(Proc*);
void edfunlock(void);