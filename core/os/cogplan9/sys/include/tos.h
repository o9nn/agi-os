typedef struct Tos Tos;
typedef struct Plink Plink;
#pragma incomplete Plink
struct Tos {
struct
{
Plink	*pp;
Plink	*next;
Plink	*last;
Plink	*first;
ulong	pid;
ulong	what;
} prof;
uvlong	cyclefreq;
vlong	kcycles;
vlong	pcycles;
ulong	pid;
ulong	clock;
ulong	kscr[4];
};
extern Tos *_tos;