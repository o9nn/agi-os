#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"ureg.h"
#include	"io.h"
enum
{
MC_IFETCH	= (1<<30),
MC_STORE	= (1<<11),
DSI_STORE	= (1<<25),
DSI_PROT		= (1<<27),
};
void
faultpower(Ureg *ur)
{
ulong addr;
char buf[ERRMAX];
int read, i;
addr = ur->pc;
read = 1;
i = ur->cause >> 8;
if(i == CDSI || i == CDTLBE || i == CMCHECK && (ur->status&MC_IFETCH) == 0) {
addr = getdar();
if(getdsisr() & (DSI_STORE|MC_STORE))
read = 0;
} else if(i == CDMISS)
addr = getdepn() & ~0x3FF;
up->dbgreg = ur;
spllo();
sprint(buf, "trap: fault %s pc=0x%lux addr=0x%lux",
read ? "read" : "write", ur->pc, addr);
if(up->type == Interp)
disfault(ur, buf);
dumpregs(ur);
panic("fault: %s\n", buf);
}