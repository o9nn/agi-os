#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"ureg.h"
#include	"../port/error.h"
#include	"io.h"
enum {
Debug = 0,
};
typedef struct Fault Fault;
struct Fault {
uintptr	va;
ulong	pid;
uintptr	pc;
int	cnt;
char	*prog;
int	code;
};
extern char *excname[];
static Fault lflt, maxflt;
int
tstbadvaddr(Ureg *ur)
{
int rn;
ulong iw, off, ea;
iw = ur->pc;
if(ur->cause & BD)
iw += 4;
if(seg(up, iw, 0) == 0)
return 0;
iw = *(ulong*)iw;
switch((iw>>26) & 0x3f) {
default:
return 1;
case 0x20:
case 0x24:
case 0x35:
case 0x36:
case 0x37:
case 0x1A:
case 0x1B:
case 0x21:
case 0x25:
case 0x30:
case 0x34:
case 0x23:
case 0x31:
case 0x32:
case 0x27:
case 0x22:
case 0x26:
break;
case 0x28:
case 0x38:
case 0x3C:
case 0x3D:
case 0x3E:
case 0x3F:
case 0x2C:
case 0x2D:
case 0x29:
case 0x2B:
case 0x39:
case 0x3A:
case 0x2A:
case 0x2E:
break;
}
off = iw & 0xffff;
if(off & 0x8000)
off |= ~0xffff;
rn = (iw>>21) & 0x1f;
ea = *reg(ur, rn);
if(rn == 0)
ea = 0;
ea += off;
if(ur->badvaddr == ea)
return 0;
return 1;
}
static int
ckfaultstuck(Ureg *ur, int read, int code)
{
uintptr pc, va;
va = ur->badvaddr;
pc = ur->pc;
if (va != lflt.va || up->pid != lflt.pid || pc != lflt.pc ||
code != lflt.code) {
lflt.cnt = 1;
lflt.va = va;
lflt.pid = up->pid;
lflt.pc = pc;
lflt.code = code;
return 0;
}
++lflt.cnt;
if (lflt.cnt >= 1000)
panic("fault: %d consecutive faults for va %#p", lflt.cnt, va);
if (lflt.cnt > maxflt.cnt) {
maxflt.cnt = lflt.cnt;
maxflt.va = va;
maxflt.pid = up->pid;
maxflt.pc = pc;
kstrdup(&maxflt.prog, up->text);
}
if (lflt.cnt < 5 || strncmp(up->text, "8l", 2) != 0)
return 0;
iprint("%d consecutive faults for va %#p at pc %#p in %s "
"pid %ld\n", lflt.cnt, lflt.va, pc, up->text, lflt.pid);
iprint("\t%s: %s%s r31 %#lux tlbvirt %#lux\n",
excname[code], va == pc? "[instruction] ": "",
(read? "read": "write"), ur->r31, tlbvirt());
return 0;
}
char *
faultsprint(char *p, char *ep)
{
if (Debug)
p = seprint(p, ep,
"max consecutive faults %d for va %#p in %s\n",
maxflt.cnt, maxflt.va, maxflt.prog);
return p;
}
void
faultmips(Ureg *ur, int user, int code)
{
int read;
ulong addr;
char *p, buf[ERRMAX];
static int infault, printed;
if (0 && infault && !printed) {
printed = 1;
print("fault: recursive fault (%d deep) pc %#p va %#p\n",
infault+1, ur->pc, ur->badvaddr);
}
infault++;
if(waserror()){
infault--;
nexterror();
}
addr = ur->badvaddr;
addr &= ~(BY2PG-1);
read = !(code==CTLBM || code==CTLBS);
if (Debug && ckfaultstuck(ur, read, code) || fault(addr, read) == 0){
infault--;
poperror();
return;
}
infault--;
poperror();
if(tstbadvaddr(ur)) {
print("fault: spurious badvaddr %#lux in %s at pc %#lux\n",
ur->badvaddr, up->text, ur->pc);
return;
}
if(user) {
p = "store";
if(read)
p = "load";
snprint(buf, sizeof buf, "sys: trap: fault %s addr=%#lux r31=%#lux",
p, ur->badvaddr, ur->r31);
postnote(up, 1, buf, NDebug);
return;
}
print("kernel %s vaddr=%#lux\n", excname[code], ur->badvaddr);
print("st=%#lux pc=%#lux r31=%#lux sp=%#lux\n",
ur->status, ur->pc, ur->r31, ur->sp);
dumpregs(ur);
panic("fault");
}
void
validalign(uintptr addr, unsigned align)
{
if(align == sizeof(vlong))
align = sizeof(long);
if((align != 0 && !(align & (align-1))) && !(addr & (align-1)))
return;
postnote(up, 1, "sys: odd address", NDebug);
error(Ebadarg);
}