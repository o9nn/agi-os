#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"io.h"
typedef struct Ctlr Ctlr;
typedef struct I2Cregs I2Cregs;
struct I2Cregs {
ulong	ibmr;
ulong	pad0;
ulong	idbr;
ulong	pad1;
ulong	icr;
ulong	pad2;
ulong	isr;
ulong	pad3;
ulong	isar;
};
enum {
Scls=	1<<1,
Sdas=	1<<0,
Fm=		1<<15,
Ur=		1<<14,
Sadie=	1<<13,
Aldie=	1<<12,
Ssdie=	1<<11,
Beie=	1<<10,
Irfie=	1<<9,
Iteie=	1<<8,
Gcd=	1<<7,
Scle=	1<<6,
Iue=		1<<5,
Ma=		1<<4,
Tb=		1<<3,
Ack=		0<<2,
Nak=	1<<2,
Stop=	1<<1,
Start=	1<<0,
Bed=		1<<10,
Sad=		1<<9,
Gcad=	1<<8,
Irf=		1<<7,
Ite=		1<<6,
Ald=		1<<5,
Ssd=		1<<4,
Ibb=		1<<3,
Ub=		1<<2,
Nakrcv=	1<<1,
Rwm=	1<<0,
Err=		Bed | Ssd,
Rbit =	1<<0,
Wbit=	0<<0,
MaxIO =	8192,
MaxSA=	2,
Bufsize =	MaxIO,
Freq =	0,
I2Ctimeout = 10000,
Chatty = 0,
};
#define	DPRINT	if(Chatty)print
struct Ctlr {
Lock;
QLock	io;
int	init;
int	polling;
I2Cregs*	regs;
int	status;
int	phase;
Rendez	r;
int	addr;
int	salen;
int	offset;
int	cntl;
int	rdcount;
Block*	b;
};
enum {
Idle,
Done,
Failed,
Busy,
Address,
Subaddress,
Read,
Write,
Halting,
};
static	Ctlr	i2cctlr[1];
static void	interrupt(Ureg*, void*);
static int readyxfer(Ctlr*, int);
static void	rxstart(Ctlr*);
static void	txstart(Ctlr*);
static void	stopxfer(Ctlr*);
static void	txoffset(Ctlr*, ulong, int);
static int idlectlr(Ctlr*);
static void
i2cdump(char *t, I2Cregs *i2c)
{
iprint("i2c %s: ibmr=%.4lux icr=%.4lux isr=%.4lux\n", t, i2c->ibmr, i2c->icr, i2c->isr);
}
static void
initialise(I2Cregs *i2c, int eintr)
{
int ctl;
i2c->isar = 0;
ctl = Freq | Gcd | Scle | Iue;
if(eintr)
ctl |= Beie | Irfie;
i2c->icr = ctl;
if(Chatty)
iprint("ctl=%4.4ux icr=%4.4lux\n", ctl, i2c->icr);
}
void
i2csetup(int polling)
{
I2Cregs *i2c;
Ctlr *ctlr;
ctlr = i2cctlr;
ctlr->polling = polling;
i2c = KADDR(PHYSI2C);
ctlr->regs = i2c;
if(!polling){
if(ctlr->init == 0){
initialise(i2c, 1);
ctlr->init = 1;
intrenable(IRQ, IRQi2c, interrupt, i2cctlr, "i2c");
if(Chatty)
i2cdump("init", i2c);
}
}else
initialise(i2c, 0);
}
static void
done(Ctlr *ctlr)
{
ctlr->phase = Done;
wakeup(&ctlr->r);
}
static void
failed(Ctlr *ctlr)
{
ctlr->phase = Failed;
wakeup(&ctlr->r);
}
static void
interrupt(Ureg*, void *arg)
{
int sts, idl;
Ctlr *ctlr;
Block *b;
I2Cregs *i2c;
char xx[12];
ctlr = arg;
i2c = ctlr->regs;
idl = (i2c->ibmr & 3) == 3;
if(Chatty && ctlr->phase != Read && ctlr->phase != Write){
snprint(xx, sizeof(xx), "intr %d", ctlr->phase);
i2cdump(xx, i2c);
}
sts = i2c->isr;
if(sts & (Bed | Sad | Gcad | Ald))
iprint("i2c: unexpected status: %.4ux", sts);
i2c->isr = sts;
ctlr->status = sts;
i2c->icr &= ~(Start | Stop | Nak | Ma | Iteie);
if(sts & Err){
failed(ctlr);
return;
}
switch(ctlr->phase){
default:
iprint("i2c: unexpected interrupt: p-%d s=%.4ux\n", ctlr->phase, sts);
break;
case Halting:
ctlr->phase = Idle;
break;
case Subaddress:
if(ctlr->salen){
ctlr->salen -= 8;
i2c->idbr = ctlr->offset >> ctlr->salen;
i2c->icr |= Aldie | Tb | Iteie;
break;
}
if(ctlr->cntl & Rbit){
i2c->idbr = (ctlr->addr << 1) | Rbit;
i2c->icr |= Start | Tb | Iteie;
ctlr->phase = Address;
break;
}
case Address:
if(ctlr->cntl & Rbit)
rxstart(ctlr);
else
txstart(ctlr);
break;
case Read:
b = ctlr->b;
if(b == nil)
panic("i2c: no buffer");
if(sts & Irf){
ctlr->rdcount--;
if(b->wp < b->lim)
*b->wp++ = i2c->idbr;
}
if(ctlr->rdcount <= 0 || sts & Nakrcv || idl){
if(Chatty)
iprint("done: %.4ux\n", sts);
done(ctlr);
break;
}
rxstart(ctlr);
break;
case Write:
b = ctlr->b;
if(b == nil)
panic("i2c: no buffer");
if(BLEN(b) <= 0 || sts & Nakrcv){
done(ctlr);
break;
}
txstart(ctlr);
break;
}
}
static int
isdone(void *a)
{
return ((Ctlr*)a)->phase < Busy;
}
static int
i2cerror(char *s)
{
DPRINT("i2c error: %s\n", s);
if(up)
error(s);
return -1;
}
static char*
startxfer(I2Cdev *d, int op, Block *b, int n, ulong offset)
{
I2Cregs *i2c;
Ctlr *ctlr;
int i, p, s;
ctlr = i2cctlr;
if(up){
qlock(&ctlr->io);
if(waserror()){
qunlock(&ctlr->io);
nexterror();
}
}
ilock(ctlr);
if(!idlectlr(ctlr)){
iunlock(ctlr);
if(up)
error("bus confused");
return "bus confused";
}
if(ctlr->phase >= Busy)
panic("i2c: ctlr busy");
ctlr->cntl = op;
ctlr->b = b;
ctlr->rdcount = n;
ctlr->addr = d->addr;
i2c = ctlr->regs;
ctlr->salen = d->salen*8;
ctlr->offset = offset;
if(ctlr->salen){
ctlr->phase = Subaddress;
op = Wbit;
}else
ctlr->phase = Address;
i2c->idbr = (d->addr<<1) | op;
i2c->icr |= Start | Tb | Iteie;
if(Chatty)
i2cdump("start", i2c);
iunlock(ctlr);
if(ctlr->polling){
for(i=0; !isdone(ctlr); i++){
delay(2);
interrupt(nil, ctlr);
}
}else
tsleep(&ctlr->r, isdone, ctlr, I2Ctimeout);
ilock(ctlr);
p = ctlr->phase;
s = ctlr->status;
ctlr->b = nil;
if(ctlr->phase != Done && ctlr->phase != Idle)
stopxfer(ctlr);
iunlock(ctlr);
if(up){
poperror();
qunlock(&ctlr->io);
}
if(p != Done || s & (Bed|Ald)){
if(s & Ald)
return "i2c lost arbitration";
if(s & Bed)
return "i2c bus error";
if(s & Ssd)
return "i2c transfer aborted";
if(0 && p != Done)
return "i2c timed out";
sprint(up->genbuf, "i2c error: phase=%d status=%.4ux", p, s);
return up->genbuf;
}
return nil;
}
long
i2csend(I2Cdev *d, void *buf, long n, ulong offset)
{
Block *b;
char *e;
if(n <= 0)
return 0;
if(n > MaxIO)
n = MaxIO;
if(up){
b = allocb(n);
if(b == nil)
error(Enomem);
if(waserror()){
freeb(b);
nexterror();
}
}else{
b = iallocb(n);
if(b == nil)
return -1;
}
memmove(b->wp, buf, n);
b->wp += n;
e = startxfer(d, 0, b, 0, offset);
if(up)
poperror();
n -= BLEN(b);
freeb(b);
if(e)
return i2cerror(e);
return n;
}
long
i2crecv(I2Cdev *d, void *buf, long n, ulong offset)
{
Block *b;
long nr;
char *e;
if(n <= 0)
return 0;
if(n > MaxIO)
n = MaxIO;
if(up){
b = allocb(n);
if(b == nil)
error(Enomem);
if(waserror()){
freeb(b);
nexterror();
}
}else{
b = iallocb(n);
if(b == nil)
return -1;
}
e = startxfer(d, Rbit, b, n, offset);
nr = BLEN(b);
if(nr > 0)
memmove(buf, b->rp, nr);
if(up)
poperror();
freeb(b);
if(e)
return i2cerror(e);
return nr;
}
static int
readyxfer(Ctlr *ctlr, int phase)
{
I2Cregs *i2c;
i2c = ctlr->regs;
if((i2c->isr & Bed) != 0){
failed(ctlr);
return 0;
}
ctlr->phase = phase;
return 1;
}
static void
rxstart(Ctlr *ctlr)
{
Block *b;
int cntl;
b = ctlr->b;
if(b == nil || ctlr->rdcount<= 0){
done(ctlr);
return;
}
if(!readyxfer(ctlr, Read))
return;
cntl = Aldie | Tb;
if(ctlr->rdcount == 1)
cntl |= Stop | Nak | Iteie;
ctlr->regs->icr |= cntl;
}
static void
txstart(Ctlr *ctlr)
{
Block *b;
int cntl;
long nb;
I2Cregs *i2c;
b = ctlr->b;
if(b == nil || (nb = BLEN(b)) <= 0){
done(ctlr);
return;
}
if(!readyxfer(ctlr, Write))
return;
i2c = ctlr->regs;
i2c->idbr = *b->rp++;
cntl = Aldie | Tb | Iteie;
if(nb == 1)
cntl |= Stop;
i2c->icr |= cntl;
}
static void
stopxfer(Ctlr *ctlr)
{
I2Cregs *i2c;
i2c = ctlr->regs;
if((i2c->isr & Ub) == 0){
ctlr->phase = Idle;
return;
}
if((i2c->isr & Ibb) == 0 && ctlr->phase != Halting){
ctlr->phase = Halting;
i2c->icr |= Ma;
}
}
static int
idlectlr(Ctlr *ctlr)
{
I2Cregs *i2c;
i2c = ctlr->regs;
if((i2c->isr & Ibb) == 0){
if((i2c->isr & Ub) == 0){
ctlr->phase = Idle;
return 1;
}
iprint("i2c: bus free, ctlr busy: isr=%.4lux icr=%.4lux\n", i2c->isr, i2c->icr);
}
iprint("i2c: soft reset\n");
i2c->icr = Ur;
iunlock(ctlr);
delay(1);
ilock(ctlr);
initialise(i2c, !ctlr->polling);
ctlr->phase = Idle;
return (i2c->isr & (Ibb | Ub)) == 0;
}