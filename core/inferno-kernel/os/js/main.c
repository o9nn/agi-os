#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"version.h"
Mach *m = &mach0;
Proc *up;
int cflag;
ulong cachetable[1024];
Sysint	*sysintr;
struct {
uchar	format;
uchar	type;
uchar	ea[6];
uchar	pad[32-8];
} idprom;
int	cpuserver;
ulong	bank[8];
uchar	mempres[64];
char	fbstr[32];
ulong	fbslot;
int	usecg6;
Label	catch;
uchar	*sp;
int cold=1;
typedef struct Sysparam Sysparam;
struct Sysparam
{
int	id;
char	*name;
char	ss2;
int	vacsize;
int	vacline;
int	ncontext;
char	cachebug;
int	nbank;
int	banksize;
int	pcnt;
}
sysparam[] =
{
{ 0xFF, "unknown Sun4M",0, 0, 0,  64, 0, 4, 32 ,0},
{ 0x80, "JavaStation uSparcII",0, 0, 0,  256, 0, 4, 32 ,2},
{ 0 }
};
Sysparam *sparam;
void
doc(char *m)
{
print("%s\n", m);
}
static void poolsizeinit(void);
void
main(void)
{
machinit();
trapinit();
quotefmtinstall();
confinit();
xinit();
mmuinit();
intrinit();
clockinit();
printinit();
screeninit();
ioinit();
doc("ioinit...");
ns16552install();
poolsizeinit();
doc("ns16552install...");
kbdinit();
doc("kbdinit...");
cacheinit();
doc("cacheinit...");
procinit();
doc("procinit...");
putphys(MID, 0x1F<<16);
links();
doc("links");
chandevreset();
doc("chandevreset...");
print("\nInferno with OpenCog AGI kernel extensions\n");
print("%s-%s \n\n",VERSION, conffile);
print("JIT Compilation Mode = %d\n",cflag);
doc("opencoginit");
opencoginit();
userinit();
doc("userinit...");
putphys(PROCINTCLR, (~0<<17)|(1<<15));
print("berore schedinit\n");
schedinit();
}
extern int main_pool_pcnt;
extern int heap_pool_pcnt;
extern int image_pool_pcnt;
static void
poolsizeinit(void)
{
ulong nb = conf.npage*BY2PG;
print("Total memory available: %ld K\n",nb/1024);
poolsize(mainmem, (nb*main_pool_pcnt)/100, 0);
poolsize(heapmem, (nb*heap_pool_pcnt)/100, 0);
poolsize(imagmem, (nb*image_pool_pcnt)/100, 1);
}
void
intrinit(void)
{
KMap *k;
getphys(AFSR);
k = kmappa(SYSINTR, PTEIO|PTENOCACHE);
sysintr = (Sysint*)VA(k);
sysintr->maskset = ~0;
sysintr->maskclr=MaskAllIntr|MEIntr|MSIIntr|EMCIntr|EtherIntr|KbdIntr;
putphys(PROCINTCLR, (~0<<17)|(1<<15));
}
void
systemreset(void)
{
microdelay(200);
putphys(SYSCTL, getphys(SYSCTL)|1);
}
void
machinit(void)
{
memset(m, 0, sizeof(Mach));
}
void
ioinit(void)
{
KMap *k;
uchar *sindex;
uchar *sdata;
uchar *mkctl;
uchar *mkdata;
k = kmappa(SUPERIO_PHYS_PAGE, PTEIO|PTENOCACHE);
sindex = (uchar*)(VA(k)+SUPERIO_INDEX_OFFSET);
sdata = (uchar*)(VA(k)+SUPERIO_DATA_OFFSET);
mkdata = (uchar*)(VA(k)+SUPERIO_MOUSE_KBD_DATA_PORT);
mkctl = (uchar*)(VA(k)+SUPERIO_MOUSE_KBD_CTL_PORT);
superioinit(VA(k),sindex,sdata,mkctl,mkdata);
doc("superioinit...");
}
void
init0(void)
{
Osenv *o;
up->nerrlab = 0;
print("before spllo");
spllo();
print("Sun Sparc %s\n", sparam->name);
print("bank 0: %ldM  1: %ldM\n", bank[0], bank[1]);
print("frame buffer id %lux slot %ld %s\n",conf.monitor,fbslot,fbstr);
if(waserror())
panic("init0");
o = up->env;
o->pgrp->slash = namec("#/", Atodir, 0, 0);
cnameclose(o->pgrp->slash->name);
o->pgrp->slash->name = newcname("/");
o->pgrp->dot = cclone(o->pgrp->slash);
chandevinit();
poperror();
disinit("/osinit.dis");
}
void
userinit(void)
{
Proc *p;
Osenv *o;
p = newproc();
o = p->env;
o->fgrp = newfgrp(nil);
o->pgrp = newpgrp();
kstrdup(&o->user, eve);
strcpy(p->text,"interp");
p->fpstate = FPINIT;
fpinit();
p->sched.pc = (ulong)init0;
p->sched.sp = (ulong)p->kstack+KSTACK-8;
p->sched.sp &= ~7;
ready(p);
}
uchar *
pusharg(char *p)
{
int n;
n = strlen(p)+1;
sp -= n;
memmove(sp, p, n);
return sp;
}
void
exit(int ispanic)
{
USED(ispanic);
spllo();
print("cpu exiting\n");
chandevshutdown();
microdelay(500);
systemreset();
}
void
reboot(void)
{
exit(0);
}
void
halt(void)
{
spllo();
print("cpu halted\n");
microdelay(500);
for(;;);
}
int
probemem(ulong addr)
{
ulong pcr, save0;
int works;
save0 = getphys(0);
pcr = getpcr()|NOFAULT;
works = 0;
setpcr(pcr & ~MEMPCHECK);
putphys(addr, ~addr);
if(addr)
putphys(0, 0x89ABCDEF);
if(getphys(addr) == ~addr){
setpcr(pcr);
putphys(addr, addr);
if(addr)
putphys(0, 0x89ABCDEF);
if(getphys(addr) == addr)
works = 1;
}
setpcr(pcr & ~NOFAULT);
putphys(0, save0);
getphys(AFSR);
getrmmu(SFSR);
return works;
}
void
scanbank(ulong base, uchar *mempres, int n)
{
int i;
ulong addr, npg;
npg = 0;
for(i=0; i<n; i++){
mempres[i] = 0;
addr = base + i*MB;
if(!probemem(addr))
break;
if(addr != base) {
putphys(addr, addr);
if(getphys(base) == addr)
break;
}
mempres[i] = 1;
npg += MB/BY2PG;
}
if(npg){
if(conf.npage0 == 0){
conf.base0 = base;
conf.npage0 = npg;
}else if(conf.npage1 < npg){
conf.base1 = base;
conf.npage1 = npg;
}
}
}
void
physcopyin(void *d, ulong s, int n)
{
int i, j;
ulong w;
for(i=0; i<n; i+=sizeof(ulong)) {
w = getphys(s+i);
j = n-i;
if(j > sizeof(ulong))
j = sizeof(ulong);
memmove((uchar*)d+i, &w, j);
}
}
Conf	conf;
void
confinit(void)
{
ulong i;
ulong ktop;
conf.monitor = 0;
conf.nmach = 1;
if(conf.nmach > MAXMACH)
panic("confinit");
physcopyin(&idprom, NVR_PHYS+IDOFF, sizeof(idprom));
if(idprom.format!=1 || (idprom.type&0xF0)!=0x80)
*(ulong*)~0 = 0;
for(sparam = sysparam; sparam->id; sparam++)
if(sparam->id == idprom.type)
break;
if(sparam->id == 0)
sparam = sysparam;
conf.ss2 = sparam->ss2;
conf.vacsize = sparam->vacsize;
conf.vaclinesize = sparam->vacline;
conf.ncontext = sparam->ncontext;
conf.ss2cachebug = sparam->cachebug;
for(i=0; i<sparam->nbank; i++)
if(probemem(i*sparam->banksize*MB))
scanbank(i*sparam->banksize*MB, mempres,
sparam->banksize);
bank[0] = conf.npage0*BY2PG/MB;
bank[1] = conf.npage1*BY2PG/MB;
if(bank[1] == 0){
conf.npage1 = conf.npage0 - (8*MB)/BY2PG;
conf.base1 = conf.base0 + 8*MB;
conf.npage0 = (8*MB)/BY2PG;
bank[1] = bank[0]-8;
bank[0] = 8;
}
conf.npage = conf.npage0+conf.npage1;
ktop = PGROUND((ulong)end);
ktop = PADDR(ktop);
conf.npage0 -= ktop/BY2PG;
conf.base0 += ktop;
conf.nproc = 100 + ((conf.npage*BY2PG)/MB)*5;
conf.copymode = 0;
conf.arp = 32;
conf.ialloc = (((conf.npage*(100-sparam->pcnt))/100)/2)*BY2PG;
eve = strdup("inferno");
#ifdef notdef
mainmem->maxsize = (conf.npage*BY2PG)/8;
heapmem->maxsize = ((conf.npage*BY2PG)*5)/8;
imagmem->maxsize = ((conf.npage*BY2PG)*2)/8;
#endif
}
void
lancesetup(Lance *lp)
{
KMap *k;
DMAdev *dma;
ulong pa, va;
int i;
k = kmappa(ETHER, PTEIO|PTENOCACHE);
lp->rdp = (void*)(VA(k)+0);
lp->rap = (void*)(VA(k)+2);
for(i=0; i<6; i++)
lp->ea[i] = idprom.ea[i];
lp->lognrrb = 7;
lp->logntrb = 7;
lp->nrrb = 1<<lp->lognrrb;
lp->ntrb = 1<<lp->logntrb;
lp->sep = 1;
lp->busctl = BSWP | ACON | BCON;
pa = PADDR(xspanalloc(BY2PG, BY2PG, 0));
va = kmapdma(pa, BY2PG);
lp->lanceram = (ushort*)va;
lp->lm = (Lancemem*)va;
i = (lp->nrrb+lp->ntrb)*sizeof(Lancepkt);
i = (i+(BY2PG-1))/BY2PG;
pa = PADDR(xspanalloc(i*BY2PG, BY2PG, 0));
va = kmapdma(pa, i*BY2PG);
lp->lrp = (Lancepkt*)va;
lp->rp = (Lancepkt*)va;
lp->ltp = lp->lrp+lp->nrrb;
lp->tp = lp->rp+lp->nrrb;
k = kmappa(DMA, PTEIO|PTENOCACHE);
dma = (DMAdev*)VA(k);
dma->base = 0xff;
#ifdef notdef
if(dma->ecsr & E_TP_select)
print("Twisted pair ethernet\n");
else
print("AUI ethernet\n");
#endif
microdelay(1);
dma->ecsr |= E_Int_en|E_Invalidate|E_Dsbl_wr_inval|E_Dsbl_rd_drn;
microdelay(1);
}
static void
linkproc(void)
{
spllo();
(*up->kpfun)(up->arg);
}
void
kprocchild(Proc *p, void (*func)(void*), void *arg)
{
p->sched.pc = (ulong)linkproc;
p->sched.sp = (ulong)p->kstack+KSTACK-8;
p->kpfun = func;
p->arg = arg;
}
void
FPsave(void *f)
{
savefsr(f);
}
void
FPrestore(void *f)
{
restfsr(f);
}
void
fpsave(FPU *f)
{
savefpregs( f );
}
void
fprestore(FPU *f)
{
restfpregs(f);
}
int
islo(void)
{
int val;
val =  (getpsr()&SPL(15)) == 0;
return val;
}
void
setvec(void)
{
}