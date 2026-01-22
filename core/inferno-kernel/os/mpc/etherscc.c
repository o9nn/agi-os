#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
enum {
Nrdre		= 16,
Ntdre		= 16,
Rbsize		= ETHERMAXTU+4,
Bufsize		= (Rbsize+7)&~7,
};
enum {
RxMiss=		1<<8,
RxeLG=		1<<5,
RxeNO=		1<<4,
RxeSH=		1<<3,
RxeCR=		1<<2,
RxeOV=		1<<1,
RxeCL=		1<<0,
RxError=		(RxeLG|RxeNO|RxeSH|RxeCR|RxeOV|RxeCL),
TxPad=		1<<14,
TxTC=		1<<10,
TxeDEF=		1<<9,
TxeHB=		1<<8,
TxeLC=		1<<7,
TxeRL=		1<<6,
TxeUN=		1<<1,
TxeCSL=		1<<0,
RXB=	1<<0,
TXB=	1<<1,
BSY=		1<<2,
RXF=		1<<3,
TXE=		1<<4,
PRO=	1<<9,
ENR=	1<<5,
ENT=	1<<4,
RXD1=	SIBIT(15),
TXD1=	SIBIT(14),
RTS1=	IBIT(19),
CTS1=	SIBIT(11),
CD1=	SIBIT(10),
};
typedef struct Etherparam Etherparam;
struct Etherparam {
SCCparam;
ulong	c_pres;
ulong	c_mask;
ulong	crcec;
ulong	alec;
ulong	disfc;
ushort	pads;
ushort	ret_lim;
ushort	ret_cnt;
ushort	mflr;
ushort	minflr;
ushort	maxd1;
ushort	maxd2;
ushort	maxd;
ushort	dma_cnt;
ushort	max_b;
ushort	gaddr[4];
ulong	tbuf0_data0;
ulong	tbuf0_data1;
ulong	tbuf0_rba0;
ulong	tbuf0_crc;
ushort	tbuf0_bcnt;
ushort	paddr[3];
ushort	p_per;
ushort	rfbd_ptr;
ushort	tfbd_ptr;
ushort	tlbd_ptr;
ulong	tbuf1_data0;
ulong	tbuf1_data1;
ulong	tbuf1_rba0;
ulong	tbuf1_crc;
ushort	tbuf1_bcnt;
ushort	tx_len;
ushort	iaddr[4];
ushort	boff_cnt;
ushort	taddr[3];
};
typedef struct {
Lock;
int	port;
int	init;
int	active;
SCC*	scc;
CPMdev*	cpm;
Ring;
ulong	interrupts;
ulong	deferred;
ulong	heartbeat;
ulong	latecoll;
ulong	retrylim;
ulong	underrun;
ulong	overrun;
ulong	carrierlost;
ulong	retrycount;
} Ctlr;
static	int	sccid[] = {-1, CPscc1, CPscc2, CPscc3, CPscc4};
static void
attach(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ctlr->active = 1;
ctlr->scc->gsmrl |= ENR|ENT;
eieio();
}
static void
closed(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
if(ctlr->active){
sccxstop(ctlr->cpm);
ilock(ctlr);
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
ctlr->scc->psmr |= PRO;
else
ctlr->scc->psmr &= ~PRO;
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
ctlr->scc->psmr |= PRO;
else
ctlr->scc->psmr &= ~PRO;
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
while(ctlr->ntq < Ntdre-1){
b = qget(ether->oq);
if(b == 0)
break;
dre = &ctlr->tdr[ctlr->tdrh];
if(dre->status & BDReady)
panic("ether: txstart");
len = BLEN(b);
dcflush(b->rp, len);
if(ctlr->txb[ctlr->tdrh] != nil)
panic("scc/ether: txstart");
ctlr->txb[ctlr->tdrh] = b;
if((ulong)b->rp&1)
panic("scc/ether: txstart align");
dre->addr = PADDR(b->rp);
dre->length = len;
eieio();
dre->status = (dre->status & BDWrap) | BDReady|TxPad|BDInt|BDLast|TxTC;
eieio();
ctlr->scc->todr = 1<<15;
eieio();
ctlr->ntq++;
ctlr->tdrh = NEXT(ctlr->tdrh, Ntdre);
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
static void
interrupt(Ureg*, void *arg)
{
Ether *ether;
int len, events, status;
Ctlr *ctlr;
BD *dre;
Block *b;
ether = arg;
ctlr = ether->ctlr;
if(!ctlr->active)
return;
events = ctlr->scc->scce;
eieio();
ctlr->scc->scce = events;
eieio();
ctlr->interrupts++;
if(events & (TXE|BSY|RXB)){
if(events & RXB)
ctlr->overrun++;
if(events & TXE)
ether->oerrs++;
if(0 || events & TXE)
print("ETHER.SCC#%d: scce = 0x%uX\n", ether->ctlrno, events);
}
if(events & (RXF|RXB) || 1){
dre = &ctlr->rdr[ctlr->rdrx];
while(((status = dre->status) & BDEmpty) == 0){
if(status & RxError || (status & (BDFirst|BDLast)) != (BDFirst|BDLast)){
if(status & (RxeLG|RxeSH))
ether->buffs++;
if(status & RxeNO)
ether->frames++;
if(status & RxeCR)
ether->crcs++;
if(status & RxeOV)
ether->overflows++;
}
else{
len = dre->length-4;
if((b = iallocb(len)) != 0){
dcinval(KADDR(dre->addr), len);
memmove(b->wp, KADDR(dre->addr), len);
b->wp += len;
etheriq(ether, b, 1);
}else
ether->soverflows++;
}
dre->length = 0;
dre->status = (status & BDWrap) | BDEmpty | BDInt;
eieio();
ctlr->rdrx = NEXT(ctlr->rdrx, Nrdre);
dre = &ctlr->rdr[ctlr->rdrx];
}
}
if(events & TXB){
lock(ctlr);
while(ctlr->ntq){
dre = &ctlr->tdr[ctlr->tdri];
status = dre->status;
if(status & BDReady)
break;
if(status & TxeDEF)
ctlr->deferred++;
if(status & TxeHB)
ctlr->heartbeat++;
if(status & TxeLC)
ctlr->latecoll++;
if(status & TxeRL)
ctlr->retrylim++;
if(status & TxeUN)
ctlr->underrun++;
if(status & TxeCSL)
ctlr->carrierlost++;
ctlr->retrycount += (status>>2)&0xF;
b = ctlr->txb[ctlr->tdri];
if(b == nil)
panic("scce/interrupt: bufp");
ctlr->txb[ctlr->tdri] = nil;
freeb(b);
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, Ntdre);
}
txstart(ether);
unlock(ctlr);
}
if(events & TXE)
cpmop(ctlr->cpm, RestartTx, 0);
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
static void
sccsetup(Ctlr *ctlr, SCC *scc, Ether *ether)
{
int i, rcs, tcs, w;
Etherparam *p;
IMM *io;
i = 2*(ctlr->port-1);
io = ioplock();
w = (TXD1|RXD1)<<i;
io->papar |= w;
io->padir &= ~w;
io->paodr &= ~w;
w = (CD1|CTS1)<<i;
io->pcpar &= ~w;
io->pcdir &= ~w;
io->pcso |= w;
iopunlock();
archetherenable(sccid[ctlr->port], &rcs, &tcs, ether->mbps, ether->fullduplex);
sccnmsi(ctlr->port, rcs, tcs);
p = ctlr->cpm->param;
memset(p, 0, sizeof(*p));
p->rfcr = 0x18;
p->tfcr = 0x18;
p->mrblr = Bufsize;
p->rbase = PADDR(ctlr->rdr);
p->tbase = PADDR(ctlr->tdr);
cpmop(ctlr->cpm, InitRxTx, 0);
p->c_pres = ~0;
p->c_mask = 0xDEBB20E3;
p->crcec = 0;
p->alec = 0;
p->disfc = 0;
p->pads = 0x8888;
p->ret_lim = 0xF;
p->mflr = Rbsize;
p->minflr = ETHERMINTU+4;
p->maxd1 = Bufsize;
p->maxd2 = Bufsize;
p->p_per = 0;
for(i=0; i<Eaddrlen; i+=2)
p->paddr[2-i/2] = (ether->ea[i+1]<<8)|ether->ea[i];
scc->psmr = (2<<10)|(5<<1);
scc->dsr = 0xd555;
scc->gsmrh = 0;
scc->gsmrl = (1<<28)|(4<<21)|(1<<19)|0xC;
eieio();
scc->scce = ~0;
eieio();
scc->sccm = TXE | RXF | TXB;
eieio();
io = ioplock();
w = RTS1<<(ctlr->port-1);
io->pbpar |= w;
io->pbdir |= w;
iopunlock();
}
static int
reset(Ether* ether)
{
uchar ea[Eaddrlen];
CPMdev *cpm;
Ctlr *ctlr;
SCC *scc;
if(m->speed < 24){
print("%s ether: system speed must be >= 24MHz for ether use\n", ether->type);
return -1;
}
if(!(ether->port >= 1 && ether->port <= 4)){
print("%s ether: no SCC port %lud\n", ether->type, ether->port);
return -1;
}
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
print("no ether address");
return -1;
}
cpm = cpmdev(sccid[ether->port]);
if(cpm == nil)
return -1;
ether->irq = VectorCPIC + cpm->irq;
scc = cpm->regs;
ctlr = malloc(sizeof(*ctlr));
ether->ctlr = ctlr;
memset(ctlr, 0, sizeof(*ctlr));
ctlr->cpm = cpm;
ctlr->scc = scc;
ctlr->port = ether->port;
if(ioringinit(ctlr, Nrdre, Ntdre, Bufsize) < 0)
panic("etherscc init");
sccsetup(ctlr, scc, ether);
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
etherscclink(void)
{
addethercard("SCC", reset);
}