#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
enum {
Nrdre		= 32,
Ntdre		= 4,
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
SCC*	scc;
int	port;
int	cpm;
BD*	rdr;
void*	rrb;
int	rdrx;
BD*	tdr;
void*	trb;
int	tdrx;
} Mot;
static Mot mot[MaxEther];
static	int	sccid[] = {-1, SCC1ID, SCC2ID, SCC3ID, SCC4ID};
static	int	sccparam[] = {-1, SCC1P, SCC2P, SCC3P, SCC4P};
static	int	sccreg[] = {-1, 0xA00, 0xA20, 0xA40, 0xA60};
static	int	sccirq[] = {-1, 0x1E, 0x1D, 0x1C, 0x1B};
static void
attach(Ctlr *ctlr)
{
mot[ctlr->ctlrno].scc->gsmrl |= ENR|ENT;
eieio();
}
static void
transmit(Ctlr *ctlr)
{
int len;
Mot *motp;
Block *b;
BD *tdre;
motp = &mot[ctlr->ctlrno];
while(((tdre = &motp->tdr[motp->tdrx])->status & BDReady) == 0){
b = qget(ctlr->oq);
if(b == 0)
break;
len = BLEN(b);
memmove(KADDR(tdre->addr), b->rp, len);
tdre->length = len;
eieio();
tdre->status = (tdre->status & BDWrap) | BDReady|TxPad|BDInt|BDLast|TxTC;
eieio();
motp->scc->todr = 1<<15;
eieio();
motp->tdrx = NEXT(motp->tdrx, Ntdre);
freeb(b);
}
}
static void
interrupt(Ureg*, void *ap)
{
int len, events, status;
Mot *motp;
BD *rdre;
Block *b;
Ctlr *ctlr;
ctlr = ap;
motp = &mot[ctlr->ctlrno];
events = motp->scc->scce;
eieio();
motp->scc->scce = events;
eieio();
if(events & (TXE|BSY|RXB))
print("ETHER.SCC#%d: scce = 0x%uX\n", ctlr->ctlrno, events);
if(events & (RXF|RXB) || 1){
rdre = &motp->rdr[motp->rdrx];
while(((status = rdre->status) & BDEmpty) == 0){
if(status & RxError || (status & (BDFirst|BDLast)) != (BDFirst|BDLast)){
if(status & (1<<2))
ctlr->crcs++;
if(status & (1<<1))
ctlr->overflows++;
if(status & RxError)
print("~");
else if((status & BDLast) == 0)
print("@");
}
else{
len = rdre->length-4;
if((b = iallocb(len)) != 0){
memmove(b->wp, KADDR(rdre->addr), len);
b->wp += len;
etheriq(ctlr, b, 1);
}
}
rdre->length = 0;
rdre->status = (rdre->status & BDWrap) | BDEmpty | BDInt;
eieio();
motp->rdrx = NEXT(motp->rdrx, Nrdre);
rdre = &motp->rdr[motp->rdrx];
}
}
if(events & TXB)
transmit(ctlr);
if(events & TXE)
cpmop(RestartTx, motp->cpm, 0);
}
static void
ringinit(Mot* motp)
{
int i, x;
if(motp->rdr == 0)
motp->rdr = bdalloc(Nrdre);
if(motp->rrb == 0)
motp->rrb = ialloc(Nrdre*Bufsize, 0);
x = PADDR(motp->rrb);
for(i = 0; i < Nrdre; i++){
motp->rdr[i].length = 0;
motp->rdr[i].addr = x;
motp->rdr[i].status = BDEmpty|BDInt;
x += Bufsize;
}
motp->rdr[i-1].status |= BDWrap;
motp->rdrx = 0;
if(motp->tdr == 0)
motp->tdr = bdalloc(Ntdre);
if(motp->trb == 0)
motp->trb = ialloc(Ntdre*Bufsize, 0);
x = PADDR(motp->trb);
for(i = 0; i < Ntdre; i++){
motp->tdr[i].addr = x;
motp->tdr[i].length = 0;
motp->tdr[i].status = TxPad|BDInt|BDLast|TxTC;
x += Bufsize;
}
motp->tdr[i-1].status |= BDWrap;
motp->tdrx = 0;
}
static void
sccsetup(Mot *ctlr, SCC *scc, uchar *ea)
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
archetherenable(ctlr->cpm, &rcs, &tcs);
sccnmsi(ctlr->port, rcs, tcs);
p = (Etherparam*)KADDR(sccparam[ctlr->port]);
memset(p, 0, sizeof(*p));
p->rfcr = 0x18;
p->tfcr = 0x18;
p->mrblr = Bufsize;
p->rbase = PADDR(ctlr->rdr);
p->tbase = PADDR(ctlr->tdr);
cpmop(InitRxTx, ctlr->cpm, 0);
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
p->paddr[2-i/2] = (ea[i+1]<<8)|ea[i];
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
int
sccethreset(Ctlr* ctlr)
{
uchar ea[Eaddrlen];
Mot *motp;
SCC *scc;
char line[50], def[50];
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ctlr->card.ea, Eaddrlen) == 0){
print("no preset Ether address\n");
for(;;){
strcpy(def, "00108bf12900");
if(getstr("ether MAC address", line, sizeof(line), def) < 0)
return -1;
if(parseether(ctlr->card.ea, line) >= 0 || ctlr->card.ea[0] == 0xFF)
break;
print("invalid MAC address\n");
}
}
scc = IOREGS(sccreg[ctlr->card.port], SCC);
ctlr->card.irq = VectorCPIC+sccirq[ctlr->card.port];
motp = &mot[ctlr->ctlrno];
motp->scc = scc;
motp->port = ctlr->card.port;
motp->cpm = sccid[ctlr->card.port];
ringinit(motp);
sccsetup(motp, scc, ctlr->card.ea);
ctlr->card.reset = sccethreset;
ctlr->card.attach = attach;
ctlr->card.transmit = transmit;
ctlr->card.intr = interrupt;
return 0;
}