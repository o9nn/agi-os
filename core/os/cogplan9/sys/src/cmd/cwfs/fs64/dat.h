#ifndef RBUFSIZE
#define RBUFSIZE	(8*1024)
#endif
#include "64bit.h"
enum { FIXEDSIZE = 1 };
#include "portdat.h"
enum { MAXBANK = 2 };
typedef struct Mbank {
ulong	base;
ulong	limit;
} Mbank;
typedef struct Mconf {
Lock;
Mbank	bank[MAXBANK];
int	nbank;
ulong	memsize;
} Mconf;
extern Mconf mconf;