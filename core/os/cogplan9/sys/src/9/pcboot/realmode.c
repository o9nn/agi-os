#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"../port/error.h"
void realmode0(void);
void realmodeintrinst(void);
void realmodeend(void);
extern ushort rmseg;
static Ureg rmu;
static QLock rmlock;
static int beenhere;
void
realmode(Ureg *ureg)
{
int s, sz;
ulong cr3;
uchar *ip;
qlock(&rmlock);
if (!beenhere)
iprint("into bios in real mode...");
*(Ureg *)RMUADDR = *ureg;
ip = (void *)realmodeintrinst;
ip[1] = ureg->trap;
coherence();
if ((uintptr)KTZERO == KZERO+PXEBASE)
rmseg = 0;
else {
sz = (char *)realmodeend - (char *)KTZERO;
if (sz > RMSIZE)
panic("real mode code %d bytes > %d", sz, RMSIZE);
rmseg = (RMCODE - KZERO) >> 4;
memmove((void*)RMCODE, (void*)KTZERO, sz);
}
coherence();
s = splhi();
m->pdb[PDX(0)] = m->pdb[PDX(KZERO)];
cr3 = getcr3();
putcr3(PADDR(m->pdb));
if (arch)
arch->introff();
else
i8259off();
realmode0();
splhi();
if(m->tss){
if (arch)
arch->intron();
else
i8259on();
}
m->pdb[PDX(0)] = 0;
putcr3(cr3);
splx(s);
*ureg = *(Ureg *)RMUADDR;
if (!beenhere) {
beenhere = 1;
iprint("and back\n");
}
qunlock(&rmlock);
}