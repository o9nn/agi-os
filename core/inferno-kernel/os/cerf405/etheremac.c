#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "ethermii.h"
#include "etherif.h"
#include "ureg.h"
#define DBG if(0)iprint
#define MIIDBG if(0)iprint
enum {
Nrdre = 64,
Ntdre = 32,
Nrxchan = 2,
Ntxchan = 2,
Rbsize = ETHERMAXTU,
Bufsize = (Rbsize+CACHELINESZ-1)&~(CACHELINESZ-1),
};
enum {
RxOverrun= 1<<9,
RxPause= 1<<8,
RxBad= 1<<7,
RxRunt= 1<<6,
RxShort= 1<<5,
RxAlign= 1<<4,
RxFCS= 1<<3,
RxLong= 1<<2,
RxRange= 1<<1,
RxInRange= 1<<0,
RxError= (0x3FF & ~RxPause),
TxFCS= 1<<9,
TxPad= 1<<8,
TxInsSA= 1<<7,
TxRepSA= 1<<6,
TxInsVLAN= 1<<5,
TxRepVLAN= 1<<4,
TxBadFCS= 1<<9,
TxBadPrev= 1<<8,
TxLostCarrier= 1<<7,
TxEDef= 1<<6,
TxECol= 1<<5,
TxLateCol= 1<<4,
TxManyCol= 1<<3,
TxCollision= 1<<2,
TxUnderrun= 1<<1,
TxSQE= 1<<0,
TxError= 0x3FF,
};
typedef struct Emac Emac;
struct Emac {
ulong mr0;
ulong mr1;
ulong tmr0;
ulong tmr1;
ulong rmr;
ulong isr;
ulong iser;
ulong iahr;
ulong ialr;
ulong vtpid;
ulong vtci;
ulong ptr;
ulong iaht[4];
ulong gaht[4];
ulong lsah;
ulong lsal;
ulong ipgvr;
ulong stacr;
ulong trtr;
ulong rwmr;
ulong octx;
ulong ocrx;
};
enum {
Mr0Rxi= 1<<31,
Mr0Txi= 1<<30,
Mr0Srst= 1<<29,
Mr0Txe= 1<<28,
Mr0Rxe= 1<<27,
Mr0Wke= 1<<26,
Mr1Fde= 1<<31,
Mr1Ile= 1<<30,
Mr1Vle= 1<<29,
Mr1Eifc= 1<<28,
Mr1App= 1<<27,
Mr1Ist= 1<<24,
Mr1Mf10= 0<<22,
Mr1Mf100= 1<<22,
Mr1Rfs512= 0<<20,
Mr1Rfs1024= 1<<20,
Mr1Rfs2048= 2<<20,
Mr1Rfs4096= 3<<20,
Mr1Tfs1024= 1<<18,
Mr1Tfs2048= 2<<18,
Mr1Tr0sp= 0<<15,
Mr1Tr0mp= 1<<15,
Mr1Tr0dm= 2<<15,
Mr1Tr1sp= 0<<13,
Mr1Tr1mp= 1<<13,
Mr1Tr1dm= 2<<13,
Tmr0Gnp0= 1<<31,
Tmr0Gnp1= 1<<30,
Tmr0Gnpd= 1<<29,
Tmr0Fc= 1<<28,
Tmr1Trl_s= 27,
Tmr1Tur_s= 16,
RmrSp= 1<<31,
RmrSfcs= 1<<30,
RmrRrp= 1<<29,
RmrRfp= 1<<28,
RmrRop= 1<<27,
RmrRpir= 1<<26,
RmrPpp= 1<<25,
RmrPme= 1<<24,
RmrPmme= 1<<23,
RmrIae= 1<<22,
RmrMiae= 1<<21,
RmrBae= 1<<20,
RmrMae= 1<<19,
IsrOvr= 1<<25,
IsrPp= 1<<24,
IsrBp= 1<<23,
IsrRp= 1<<22,
IsrSe= 1<<21,
IsrAle= 1<<20,
IsrBfcs= 1<<19,
IsrPtle= 1<<18,
IsrOre= 1<<17,
IsrIre= 1<<16,
IsrDbdm= 1<<9,
IsrDb0= 1<<8,
IsrSe0= 1<<7,
IsrTe0= 1<<6,
IsrDb1= 1<<5,
IsrSe1= 1<<4,
IsrTe1= 1<<3,
IsrMos= 1<<1,
IsrMof= 1<<0,
StaOc= 1<<15,
StaPhye= 1<<14,
StaRead= 1<<12,
StaWrite= 2<<12,
StaOpb50= 0<<10,
StaOpb66= 1<<10,
StaOpb83= 2<<10,
StaOpb100= 3<<10,
TrtrTrt_s= 27,
RwmrRlwm_s= 23,
RwmrRhwm_s= 7,
};
typedef struct {
Lock;
int port;
int init;
int active;
Emac *regs;
Emac *miiregs;
Mal* rx;
Mal* tx;
Mii *mii;
Ring;
ulong interrupts;
ulong deferred;
ulong heartbeat;
ulong latecoll;
ulong retrylim;
ulong underrun;
ulong overrun;
ulong carrierlost;
ulong retrycount;
} Ctlr;
static void dumpemac(Emac*);
static void
attach(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
if(!ctlr->active){
malrxenable(ctlr->rx);
maltxenable(ctlr->tx);
eieio();
ctlr->regs->mr0 = Mr0Txe | Mr0Rxe;
eieio();
ctlr->active = 1;
}
iunlock(ctlr);
}
static void
closed(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
if(ctlr->active){
ilock(ctlr);
iprint("ether closed\n");
ctlr->regs->mr0 &= ~(Mr0Txe | Mr0Rxe);
ctlr->active = 0;
iunlock(ctlr);
}
}
static void
promiscuous(void* arg, int on)
{
Ether *ether;
Ctlr *ctlr;
ether = (Ether*)arg;
ctlr = ether->ctlr;
ilock(ctlr);
if(on || ether->nmaddr)
ctlr->regs->rmr |= RmrPme | RmrPmme;
else
ctlr->regs->rmr &= ~(RmrPme | RmrPmme);
iunlock(ctlr);
}
static void
multicast(void* arg, uchar *addr, int on)
{
Ether *ether;
Ctlr *ctlr;
USED(addr, on);
ether = (Ether*)arg;
ctlr = ether->ctlr;
ilock(ctlr);
if(ether->prom || ether->nmaddr)
ctlr->regs->rmr |= RmrPmme;
else
ctlr->regs->rmr &= ~RmrPmme;
iunlock(ctlr);
}
static void
txstart(Ether *ether)
{
int len;
Ctlr *ctlr;
Block *b;
BD *dre;
ctlr = ether->ctlr;
while(ctlr->ntq < ctlr->ntdre-1){
b = qget(ether->oq);
if(b == 0)
break;
dre = &ctlr->tdr[ctlr->tdrh];
if(dre->status & BDReady)
panic("ether: txstart");
len = BLEN(b);
if(ctlr->txb[ctlr->tdrh] != nil)
panic("etheremac: txstart");
ctlr->txb[ctlr->tdrh] = b;
dre->addr = PADDR(b->rp);
dre->length = len;
dcflush(b->rp, len);
eieio();
dre->status = (dre->status & BDWrap) | BDReady|BDInt|BDLast|TxFCS|TxPad;
eieio();
ctlr->regs->tmr0 = Tmr0Gnp0;
eieio();
ctlr->ntq++;
ctlr->tdrh = NEXT(ctlr->tdrh, ctlr->ntdre);
}
}
static void
transmit(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
txstart(ether);
iunlock(ctlr);
}
static Block*
clallocb(void)
{
Block *b;
b = iallocb(Bufsize+CACHELINESZ-1);
if(b == nil)
return b;
dcflush(b->base, BALLOC(b));
b->wp = b->rp = (uchar*)(((ulong)b->base + CACHELINESZ - 1) & ~(CACHELINESZ-1));
return b;
}
static void
rxring(Ureg*, void *arg)
{
Ether *ether;
ulong status;
Ctlr *ctlr;
BD *dre;
Block *b, *rb;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
dre = &ctlr->rdr[ctlr->rdrx];
while(((status = dre->status) & BDEmpty) == 0){
if(status & RxError || (status & (BDFirst|BDLast)) != (BDFirst|BDLast)){
if(status & (RxShort|RxLong))
ether->buffs++;
if(status & (RxBad|RxAlign|RxRange|RxInRange))
ether->frames++;
if(status & RxFCS)
ether->crcs++;
if(status & RxOverrun)
ether->overflows++;
iprint("eth rx: %lux\n", status);
}else if((status & RxPause) == 0){
b = clallocb();
if(b != nil){
rb = ctlr->rxb[ctlr->rdrx];
rb->wp += dre->length;
ctlr->rxb[ctlr->rdrx] = b;
ctlr->rdr[ctlr->rdrx].addr = PADDR(b->wp);
etheriq(ether, rb, 1);
}else
ether->soverflows++;
}
dre->status = (status & BDWrap) | BDEmpty | BDInt;
eieio();
ctlr->rdrx = NEXT(ctlr->rdrx, ctlr->nrdre);
dre = &ctlr->rdr[ctlr->rdrx];
}
}
static void
txring(Ureg*, void *arg)
{
Ether *ether;
ulong status;
Ctlr *ctlr;
BD *dre;
Block *b;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
lock(ctlr);
while(ctlr->ntq){
dre = &ctlr->tdr[ctlr->tdri];
status = dre->status;
if(status & BDReady)
break;
if(status & TxEDef)
ctlr->deferred++;
if(status & TxLateCol)
ctlr->latecoll++;
if(status & TxECol)
ctlr->retrylim++;
if(status & TxUnderrun)
ctlr->underrun++;
if(status & (TxManyCol|TxCollision))
ctlr->retrycount++;
b = ctlr->txb[ctlr->tdri];
if(b == nil)
panic("etheremac: bufp");
ctlr->txb[ctlr->tdri] = nil;
freeb(b);
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, ctlr->ntdre);
}
txstart(ether);
unlock(ctlr);
}
static void
interrupt(Ureg*, void *arg)
{
Ether *ether;
ulong events;
Ctlr *ctlr;
ether = arg;
ctlr = ether->ctlr;
events = ctlr->regs->isr;
eieio();
ctlr->regs->isr = events;
eieio();
ctlr->interrupts++;
if(!ctlr->active || events == 0)
return;
if(events & IsrOvr)
ctlr->overrun++;
if(events & (IsrTe0|IsrTe1))
ether->oerrs++;
rxring(nil, arg);
txring(nil, arg);
ctlr->interrupts -= 2;
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
char *p;
int len;
Ctlr *ctlr;
if(n == 0)
return 0;
ctlr = ether->ctlr;
p = malloc(READSTR);
len = snprint(p, READSTR, "interrupts: %lud\n", ctlr->interrupts);
len += snprint(p+len, READSTR-len, "carrierlost: %lud\n", ctlr->carrierlost);
len += snprint(p+len, READSTR-len, "heartbeat: %lud\n", ctlr->heartbeat);
len += snprint(p+len, READSTR-len, "retrylimit: %lud\n", ctlr->retrylim);
len += snprint(p+len, READSTR-len, "retrycount: %lud\n", ctlr->retrycount);
len += snprint(p+len, READSTR-len, "latecollisions: %lud\n", ctlr->latecoll);
len += snprint(p+len, READSTR-len, "rxoverruns: %lud\n", ctlr->overrun);
len += snprint(p+len, READSTR-len, "txunderruns: %lud\n", ctlr->underrun);
snprint(p+len, READSTR-len, "framesdeferred: %lud\n", ctlr->deferred);
n = readstr(offset, a, n, p);
free(p);
return n;
}
static QLock miilock;
static int
miird(Mii *mii, int pa, int ra)
{
Ctlr *ctlr;
Emac *em;
ulong r;
int i;
if(up)
qlock(&miilock);
ctlr = mii->ctlr;
em = ctlr->miiregs;
MIIDBG("r: %x.%x:", pa, ra);
if((em->stacr & StaOc) == 0)
iprint("mii-not oc\n");
em->stacr = StaRead | StaOpb66 | (pa<<5) | ra;
for(i=0; i<100 && (em->stacr & StaOc) == 0; i++)
microdelay(1);
r = em->stacr;
if(up)
qunlock(&miilock);
if((r & StaOc) == 0)
iprint("mii'-not oc\n");
if(r & StaPhye)
return -1;
MIIDBG(" %8.8lux\n", r);
return r >> 16;
}
static int
miiwr(Mii *mii, int pa, int ra, int v)
{
Ctlr *ctlr;
Emac *em;
ulong r;
int i;
if(up)
qlock(&miilock);
ctlr = mii->ctlr;
em = ctlr->miiregs;
if((em->stacr & StaOc) == 0)
iprint("miiw-not oc\n");
em->stacr = (v<<16) | StaWrite | StaOpb66 | (pa<<5) | ra;
for(i=0; i<100 && (em->stacr & StaOc) == 0; i++)
microdelay(1);
r = em->stacr;
if(up)
qunlock(&miilock);
if((r & StaOc) == 0)
iprint("miiw'-not oc\n");
if(r & StaPhye)
return -1;
MIIDBG("w: %x.%x: %8.8lux\n", pa, ra, r);
return 0;
}
static int
emacmii(Ctlr *ctlr)
{
MiiPhy *phy;
int i;
MIIDBG("mii\n");
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
return -1;
ctlr->mii->ctlr = ctlr;
ctlr->mii->mir = miird;
ctlr->mii->miw = miiwr;
if(mii(ctlr->mii, 1<<(ctlr->port+1)) == 0 || (phy = ctlr->mii->curphy) == nil){
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
iprint("oui %X phyno %d\n", phy->oui, phy->phyno);
if(miistatus(ctlr->mii) < 0){
miireset(ctlr->mii);
MIIDBG("miireset\n");
if(miiane(ctlr->mii, ~0, 0, ~0) < 0){
iprint("miiane failed\n");
return -1;
}
MIIDBG("miistatus...\n");
miistatus(ctlr->mii);
if(miird(ctlr->mii, phy->phyno, Bmsr) & BmsrLs){
for(i=0;; i++){
if(i > 600){
iprint("emac%d: autonegotiation failed\n", ctlr->port);
break;
}
if(miird(ctlr->mii, phy->phyno, Bmsr) & BmsrAnc)
break;
delay(10);
}
if(miistatus(ctlr->mii) < 0)
iprint("miistatus failed\n");
}else{
iprint("emac%d: no link\n", ctlr->port);
phy->speed = 10;
}
}
iprint("emac%d mii: fd=%d speed=%d tfc=%d rfc=%d\n", ctlr->port, phy->fd, phy->speed, phy->tfc, phy->rfc);
MIIDBG("mii done\n");
return 0;
}
static void
emacsetup(Ctlr *ctlr, Ether *ether)
{
int i;
Emac *em;
ulong mode;
MiiPhy *phy;
em = ctlr->regs;
if(em->mr0 & Mr0Rxe){
em->mr0 &= ~Mr0Rxe;
eieio();
for(i=0; (em->mr0 & Mr0Rxi) == 0; i++){
if(i > 100){
iprint("ethermac: Rxe->Rxi timed out\n");
break;
}
microdelay(100);
}
}
em->mr0 = Mr0Srst;
eieio();
for(i=0; em->mr0 & Mr0Srst; i++){
if(i > 20){
iprint("ethermac: reset (PHY clocks not running?)");
i=0;
}
microdelay(100);
}
iprint("%d: rx=%8.8lux tx=%8.8lux\n", ctlr->port, PADDR(ctlr->rdr), PADDR(ctlr->tdr));
malrxinit(ctlr->rx, ctlr, Bufsize/16);
maltxinit(ctlr->tx, ctlr);
malrxreset(ctlr->rx);
maltxreset(ctlr->tx);
em->mr0 = 0;
mode = Mr1Rfs4096 | Mr1Tfs2048 | Mr1Tr0mp;
if(ctlr->mii != nil && (phy = ctlr->mii->curphy) != nil){
if(phy->speed == 10){
mode |= Mr1Mf10;
if(phy->fd)
mode |= Mr1Ist;
}else
mode |= Mr1Mf100 | Mr1Ist;
if(phy->fd)
mode |= Mr1Fde;
if(0 && (phy->rfc || phy->tfc))
mode |= Mr1App | Mr1Eifc;
ether->mbps = phy->speed;
ether->fullduplex = phy->fd;
}else{
iprint("mii: didn't work: default 100FD\n");
mode |= Mr1Mf100 | Mr1Ist | Mr1Fde;
ether->mbps = 100;
ether->fullduplex = 1;
}
em->mr1 = mode;
em->tmr1 = (9<<Tmr1Trl_s) | (256<<Tmr1Tur_s);
em->rmr = RmrSp | RmrSfcs | RmrIae | RmrBae;
em->iahr = (ether->ea[0]<<8) | ether->ea[1];
em->ialr = (ether->ea[2]<<24) | (ether->ea[3]<<16) | (ether->ea[4]<<8) | ether->ea[5];
em->vtpid = 0;
em->vtci = 0;
em->ptr = 1;
for(i=0; i<4; i++){
em->iaht[i] = 0;
em->gaht[i] = 0;
}
em->ipgvr = (96/8)/3;
em->trtr = ((256/64)-1)<<TrtrTrt_s;
em->rwmr = (32<<RwmrRlwm_s) | (128<<RwmrRhwm_s);
eieio();
em->isr = em->isr;
eieio();
em->iser = IsrOvr | IsrBp | IsrSe | IsrSe0 | IsrTe0 | IsrSe1 | IsrTe1;
eieio();
}
static int
reset(Ether* ether)
{
uchar ea[Eaddrlen];
Ctlr *ctlr;
int i;
ioringreserve(Nrxchan, Nrdre, Ntxchan, Ntdre);
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
print("no ether address");
return -1;
}
ctlr = malloc(sizeof(*ctlr));
ctlr->port = ether->port;
switch(ether->port){
case 0:
ctlr->regs = KADDR(PHYSEMAC0);
ctlr->miiregs = ctlr->regs;
ctlr->rx = malchannel(0, 0, rxring, ether);
ctlr->tx = malchannel(0, 1, txring, ether);
ether->irq = VectorEMAC0;
break;
case 1:
ctlr->regs = KADDR(PHYSEMAC1);
ctlr->miiregs = KADDR(PHYSEMAC0);
ctlr->rx = malchannel(1, 0, rxring, ether);
ctlr->tx = malchannel(2, 1, txring, ether);
ether->irq = VectorEMAC1;
break;
default:
print("%s ether: no port %lud\n", ether->type, ether->port);
free(ctlr);
return -1;
}
if(emacmii(ctlr) < 0){
free(ctlr);
return -1;
}
ether->ctlr = ctlr;
if(ioringinit(ctlr, Nrdre, Ntdre) < 0)
panic("etheremac initring");
for(i = 0; i < ctlr->nrdre; i++){
ctlr->rxb[i] = clallocb();
ctlr->rdr[i].addr = PADDR(ctlr->rxb[i]->wp);
}
emacsetup(ctlr, ether);
ether->attach = attach;
ether->closed = closed;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
return 0;
}
void
etheremaclink(void)
{
addethercard("EMAC", reset);
}
static void
dumpemac(Emac *r)
{
iprint("mr0=%8.8lux\n", r->mr0);
iprint("mr1=%8.8lux\n", r->mr1);
iprint("tmr0=%8.8lux\n", r->tmr0);
iprint("tmr1=%8.8lux\n", r->tmr1);
iprint("rmr=%8.8lux\n", r->rmr);
iprint("isr=%8.8lux\n", r->isr);
iprint("iser=%8.8lux\n", r->iser);
iprint("iahr=%8.8lux\n", r->iahr);
iprint("ialr=%8.8lux\n", r->ialr);
iprint("vtpid=%8.8lux\n", r->vtpid);
iprint("vtci=%8.8lux\n", r->vtci);
iprint("ptr=%8.8lux\n", r->ptr);
iprint("lsah=%8.8lux\n", r->lsah);
iprint("lsal=%8.8lux\n", r->lsal);
iprint("ipgvr=%8.8lux\n", r->ipgvr);
iprint("stacr=%8.8lux\n", r->stacr);
iprint("trtr=%8.8lux\n", r->trtr);
iprint("rwmr=%8.8lux\n", r->rwmr);
iprint("octx=%8.8lux\n", r->octx);
iprint("ocrx=%8.8lux\n", r->ocrx);
}