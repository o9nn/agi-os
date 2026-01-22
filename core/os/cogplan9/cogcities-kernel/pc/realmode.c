#include	"u.h"
#include	"tos.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"../port/error.h"
#define realmoderegs (*(Ureg*)RMUADDR)
#define LORMBUF (RMBUF-KZERO)
static Ureg rmu;
static Lock rmlock;
void
realmode(Ureg *ureg)
{
int s;
ulong cr3;
extern void realmode0(void);
if(getconf("*norealmode"))
return;
lock(&rmlock);
realmoderegs = *ureg;
memmove((void*)RMCODE, (void*)KTZERO, 0x1000);
s = splhi();
m->pdb[PDX(0)] = m->pdb[PDX(KZERO)];
cr3 = getcr3();
putcr3(PADDR(m->pdb));
if (arch)
arch->introff();
else
i8259off();
realmode0();
if(m->tss){
if (arch)
arch->intron();
else
i8259on();
}
m->pdb[PDX(0)] = 0;
putcr3(cr3);
splx(s);
*ureg = realmoderegs;
unlock(&rmlock);
}
static long
rtrapread(Chan*, void *a, long n, vlong off)
{
if(off < 0)
error("badarg");
if(n+off > sizeof rmu)
n = sizeof rmu - off;
if(n <= 0)
return 0;
memmove(a, (char*)&rmu+off, n);
return n;
}
static long
rtrapwrite(Chan*, void *a, long n, vlong off)
{
if(off || n != sizeof rmu)
error("write a Ureg");
memmove(&rmu, a, sizeof rmu);
if(rmu.trap == 0x10){
rmu.es = (LORMBUF>>4)&0xF000;
rmu.di = LORMBUF&0xFFFF;
}else
error("invalid trap arguments");
realmode(&rmu);
return n;
}
static long
rmemrw(int isr, void *a, long n, vlong off)
{
if(off < 0 || n < 0)
error("bad offset/count");
if(isr){
if(off >= MB)
return 0;
if(off+n >= MB)
n = MB - off;
memmove(a, KADDR((ulong)off), n);
}else{
if(off >= MB || off+n > MB ||
(off < LORMBUF || off+n > LORMBUF+BY2PG) &&
(off < 0xA0000 || off+n > 0xB0000+0x10000))
error("bad offset/count in write");
memmove(KADDR((ulong)off), a, n);
}
return n;
}
static long
rmemread(Chan*, void *a, long n, vlong off)
{
return rmemrw(1, a, n, off);
}
static long
rmemwrite(Chan*, void *a, long n, vlong off)
{
return rmemrw(0, a, n, off);
}
void
realmodelink(void)
{
addarchfile("realmode", 0660, rtrapread, rtrapwrite);
addarchfile("realmodemem", 0660, rmemread, rmemwrite);
}