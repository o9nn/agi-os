typedef enum Tevent {
SAdmit = 0,
SRelease,
SEdf,
SRun,
SReady,
SSleep,
SYield,
SSlice,
SDeadline,
SExpel,
SDead,
SInts,
SInte,
SUser,
Nevent,
} Tevent;
typedef struct Traceevent	Traceevent;
struct Traceevent {
ulong	pid;
ulong	etype;
vlong	time;
};