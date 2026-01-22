#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
typedef struct Ctlr Ctlr;
enum {
BDContin=	1<<9,
RxeOV=		1<<1,
TxeUN=		1<<1,
BDme=		1<<0,
BDrxerr=		RxeOV|BDme,
BDtxerr=		TxeUN|BDme,
MLoop=	1<<14,
MClockInv= 1<<13,
MClockPhs= 1<<12,
MDiv16=	1<<11,
MRev=	1<<10,
MMaster=	1<<9,
MSlave=	0<<9,
MEnable=	1<<8,
STR=		1<<7,
MME =	1<<5,
TXE =	1<<4,
BSY =	1<<2,
TXB =	1<<1,
RXB =	1<<0,
SPIMISO =	IBIT(28),
SPIMOSI = IBIT(29),
SPICLK = IBIT(30),
Bufsize =	64,
};
struct Ctlr {
Lock;
QLock	io;
int	init;
SPI*	spi;
IOCparam*	sp;
BD*	rd;
BD*	td;
int	phase;
Rendez	r;
char*	txbuf;
char*	rxbuf;
};
static	Ctlr	spictlr[1];
#define	DCFLUSH(a,n)
static	void	interrupt(Ureg*, void*);
void
spireset(void)
{
IMM *io;
SPI *spi;
IOCparam *sp;
CPMdev *cpm;
Ctlr *ctlr;
ctlr = spictlr;
if(ctlr->init)
return;
ctlr->init = 1;
cpm = cpmdev(CPspi);
spi = cpm->regs;
ctlr->spi = spi;
sp = cpm->param;
if(sp == nil){
print("SPI: can't allocate new parameter memory\n");
return;
}
ctlr->sp = sp;
if(ctlr->rxbuf == nil)
ctlr->rxbuf = cpmalloc(Bufsize, 2);
if(ctlr->txbuf == nil)
ctlr->txbuf = cpmalloc(Bufsize, 2);
if(ctlr->rd == nil){
ctlr->rd = bdalloc(1);
ctlr->rd->addr = PADDR(ctlr->rxbuf);
ctlr->rd->length = 0;
ctlr->rd->status = BDWrap;
}
if(ctlr->td == nil){
ctlr->td = bdalloc(1);
ctlr->td->addr = PADDR(ctlr->txbuf);
ctlr->td->length = 0;
ctlr->td->status = BDWrap|BDLast;
}
io = ioplock();
io->pbdir |= SPICLK | SPIMOSI | SPIMISO;
io->pbpar |= SPICLK | SPIMOSI | SPIMISO;
iopunlock();
sp = ctlr->sp;
sp->rbase = PADDR(ctlr->rd);
sp->tbase = PADDR(ctlr->td);
sp->rfcr = 0x18;
sp->tfcr = 0x18;
sp->mrblr = Bufsize;
sp->rstate = 0;
sp->rptr = 0;
sp->rbptr = sp->rbase;
sp->rcnt = 0;
sp->tstate = 0;
sp->tbptr = sp->tbase;
sp->tptr = 0;
sp->tcnt = 0;
eieio();
spi->spmode = MDiv16 | MRev | MMaster | ((8-1)<<4) | 1;
if(0)
spi->spmode |= MLoop;
spi->spie = ~0;
eieio();
spi->spim = MME|TXE|BSY|TXB|RXB;
eieio();
spi->spmode |= MEnable;
intrenable(VectorCPIC+cpm->irq, interrupt, spictlr, BUSUNKNOWN, "spi");
}
enum {
Idling,
Waitval,
Readyval
};
static void
interrupt(Ureg*, void *arg)
{
int events;
Ctlr *ctlr;
SPI *spi;
ctlr = arg;
spi = ctlr->spi;
events = spi->spie;
eieio();
spi->spie = events;
if(events & (MME|BSY|TXE)){
print("SPI#%x\n", events);
if(ctlr->phase != Idling){
ctlr->phase = Idling;
wakeup(&ctlr->r);
}
}else if(events & RXB){
if(ctlr->phase == Waitval){
ctlr->phase = Readyval;
wakeup(&ctlr->r);
}
}
}
static int
done(void *a)
{
return ((Ctlr*)a)->phase != Waitval;
}
long
spioutin(void *out, long nout, void *in)
{
Ctlr *ctlr;
int nb, p;
ctlr = spictlr;
if(nout > Bufsize)
return -1;
qlock(&ctlr->io);
if(waserror()){
qunlock(&ctlr->io);
return -1;
}
if(ctlr->phase != Idling)
sleep(&ctlr->r, done, ctlr);
memmove(ctlr->txbuf, out, nout);
DCFLUSH(ctlr->txbuf, Bufsize);
DCFLUSH(ctlr->rxbuf, Bufsize);
ilock(ctlr);
ctlr->phase = Waitval;
ctlr->td->length = nout;
ctlr->rd->status = BDEmpty|BDWrap|BDInt;
ctlr->td->status = BDReady|BDWrap|BDLast;
eieio();
ctlr->spi->spcom = STR;
eieio();
iunlock(ctlr);
sleep(&ctlr->r, done, ctlr);
nb = ctlr->rd->length;
if(nb > nout)
nb = nout;
p = ctlr->phase;
poperror();
qunlock(&ctlr->io);
if(p != Readyval)
return -1;
memmove(in, ctlr->rxbuf, nb);
return nb;
}