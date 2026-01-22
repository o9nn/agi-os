#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "imm.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "../ppc/ethermii.h"
#define DBG 1
enum {
Nrdre		= 128,
Ntdre		= 128,
Rbsize		= ETHERMAXTU+4,
Bufsize		= Rbsize+CACHELINESZ,
};
enum {
RxMiss=		SBIT(7),
RxeLG=		SBIT(10),
RxeNO=		SBIT(11),
RxeSH=		SBIT(12),
RxeCR=		SBIT(13),
RxeOV=		SBIT(14),
RxeCL=		SBIT(15),
RxError=	(RxeLG|RxeNO|RxeSH|RxeCR|RxeOV|RxeCL),
TxPad=		SBIT(1),
TxTC=		SBIT(5),
TxeDEF=		SBIT(6),
TxeHB=		SBIT(7),
TxeLC=		SBIT(8),
TxeRL=		SBIT(9),
TxeUN=		SBIT(14),
TxeCSL=		SBIT(15),
CRCE=		BIT(24),
FCE=		BIT(10),
PRO=		BIT(9),
FDE=		BIT(5),
LPB=		BIT(3),
ENET=		0xc,
ENT=		BIT(27),
ENR=		BIT(26),
TCI=		BIT(2),
GBL=		0x20,
BO=		0x18,
EB=		0x10,
TC2=		0x04,
DTB=		0x02,
BDB=		0x01,
GRA=		SBIT(8),
RXC=		SBIT(9),
TXC=		SBIT(10),
TXE=		SBIT(11),
RXF=		SBIT(12),
BSY=		SBIT(13),
TXB=		SBIT(14),
RXB=		SBIT(15),
};
enum {
MDIread	=	0x60020000,
MDIwrite =	0x50020000,
};
typedef struct Etherparam Etherparam;
struct Etherparam {
FCCparam;
ulong	stat_buf;
ulong	cam_ptr;
ulong	cmask;
ulong	cpres;
ulong	crcec;
ulong	alec;
ulong	disfc;
ushort	retlim;
ushort	retcnt;
ushort	p_per;
ushort	boff_cnt;
ulong	gaddr[2];
ushort	tfcstat;
ushort	tfclen;
ulong	tfcptr;
ushort	mflr;
ushort	paddr[3];
ushort	ibd_cnt;
ushort	ibd_start;
ushort	ibd_end;
ushort	tx_len;
uchar	ibd_base[32];
ulong	iaddr[2];
ushort	minflr;
ushort	taddr[3];
ushort	padptr;
ushort	Rsvdb2;
ushort	cf_range;
ushort	max_b;
ushort	maxd1;
ushort	maxd2;
ushort	maxd;
ushort	dma_cnt;
ulong	octc;
ulong	colc;
ulong	broc;
ulong	mulc;
ulong	uspc;
ulong	frgc;
ulong	ospc;
ulong	jbrc;
ulong	p64c;
ulong	p65c;
ulong	p128c;
ulong	p256c;
ulong	p512c;
ulong	p1024c;
ulong	cam_buf;
ulong	Rsvdfc;
};
typedef struct Ctlr Ctlr;
struct Ctlr {
Lock;
int	fccid;
int	port;
ulong	pmdio;
ulong	pmdck;
int	init;
int	active;
int	duplex;
FCC*	fcc;
Ring;
Block*	rcvbufs[Nrdre];
Mii*	mii;
Timer;
ulong	interrupts;
ulong	deferred;
ulong	heartbeat;
ulong	latecoll;
ulong	retrylim;
ulong	underrun;
ulong	overrun;
ulong	carrierlost;
ulong	retrycount;
};
static	int	fccirq[] = {0x20, 0x21, 0x22};
static	int	fccid[] = {FCC1ID, FCC2ID, FCC3ID};
#ifdef DBG
ulong fccrhisto[16];
ulong fccthisto[16];
ulong fccrthisto[16];
ulong fcctrhisto[16];
ulong ehisto[0x80];
#endif
static int fccmiimir(Mii*, int, int);
static int fccmiimiw(Mii*, int, int, int);
static void fccltimer(Ureg*, Timer*);
static void
attach(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
ctlr->active = 1;
ctlr->fcc->gfmr |= ENR|ENT;
iunlock(ctlr);
ctlr->tmode = Tperiodic;
ctlr->tf = fccltimer;
ctlr->ta = ether;
ctlr->tns = 5000000000LL;
timeradd(ctlr);
}
static void
closed(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
ctlr->active = 0;
ctlr->fcc->gfmr &= ~(ENR|ENT);
iunlock(ctlr);
print("Ether closed\n");
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
ctlr->fcc->fpsmr |= PRO;
else
ctlr->fcc->fpsmr &= ~PRO;
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
ctlr->fcc->fpsmr |= PRO;
else
ctlr->fcc->fpsmr &= ~PRO;
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
if(ctlr->init)
return;
while(ctlr->ntq < Ntdre-1){
b = qget(ether->oq);
if(b == 0)
break;
dre = &ctlr->tdr[ctlr->tdrh];
dczap(dre, sizeof(BD));
if(dre->status & BDReady)
panic("ether: txstart");
len = BLEN(b);
if(ctlr->txb[ctlr->tdrh] != nil)
panic("fcc/ether: txstart");
ctlr->txb[ctlr->tdrh] = b;
if((ulong)b->rp&1)
panic("fcc/ether: txstart align");
dre->addr = PADDR(b->rp);
dre->length = len;
dcflush(b->rp, len);
dcflush(dre, sizeof(BD));
dre->status = (dre->status & BDWrap) | BDReady|TxPad|BDInt|BDLast|TxTC;
dcflush(dre, sizeof(BD));
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
int len, status, rcvd, xmtd, restart;
ushort events;
Ctlr *ctlr;
BD *dre;
Block *b, *nb;
Ether *ether = arg;
ctlr = ether->ctlr;
if(!ctlr->active)
return;
events = ctlr->fcc->fcce;
ctlr->fcc->fcce = events;
#ifdef DBG
ehisto[events & 0x7f]++;
#endif
ctlr->interrupts++;
if(events & BSY)
ctlr->overrun++;
if(events & TXE)
ether->oerrs++;
#ifdef DBG
rcvd = xmtd = 0;
#endif
if(events & RXF){
dre = &ctlr->rdr[ctlr->rdrx];
dczap(dre, sizeof(BD));
while(((status = dre->status) & BDEmpty) == 0){
rcvd++;
if(status & RxError || (status & (BDFirst|BDLast)) != (BDFirst|BDLast)){
if(status & (RxeLG|RxeSH))
ether->buffs++;
if(status & RxeNO)
ether->frames++;
if(status & RxeCR)
ether->crcs++;
if(status & RxeOV)
ether->overflows++;
print("eth rx: %ux\n", status);
}else{
len = dre->length-4;
b = ctlr->rcvbufs[ctlr->rdrx];
assert(dre->addr == PADDR(b->rp));
dczap(b->rp, len);
if(nb = iallocb(Bufsize)){
b->wp += len;
etheriq(ether, b, 1);
b = nb;
b->rp = (uchar*)(((ulong)b->rp + CACHELINESZ-1) & ~(CACHELINESZ-1));
b->wp = b->rp;
ctlr->rcvbufs[ctlr->rdrx] = b;
ctlr->rdr[ctlr->rdrx].addr = PADDR(b->wp);
}else
ether->soverflows++;
}
dre->length = 0;
dre->status = (status & BDWrap) | BDEmpty | BDInt;
dcflush(dre, sizeof(BD));
ctlr->rdrx = NEXT(ctlr->rdrx, Nrdre);
dre = &ctlr->rdr[ctlr->rdrx];
dczap(dre, sizeof(BD));
}
}
if(events & (TXB|TXE)){
ilock(ctlr);
restart = 0;
while(ctlr->ntq){
dre = &ctlr->tdr[ctlr->tdri];
dczap(dre, sizeof(BD));
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
if(status & (TxeLC|TxeRL|TxeUN))
restart = 1;
ctlr->retrycount += (status>>2)&0xF;
b = ctlr->txb[ctlr->tdri];
if(b == nil)
panic("fcce/interrupt: bufp");
ctlr->txb[ctlr->tdri] = nil;
freeb(b);
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, Ntdre);
xmtd++;
}
if(restart){
ctlr->fcc->gfmr &= ~ENT;
delay(10);
ctlr->fcc->gfmr |= ENT;
cpmop(RestartTx, ctlr->fccid, 0xc);
}
txstart(ether);
iunlock(ctlr);
}
#ifdef DBG
if(rcvd >= nelem(fccrhisto))
rcvd = nelem(fccrhisto) - 1;
if(xmtd >= nelem(fccthisto))
xmtd = nelem(fccthisto) - 1;
if(rcvd)
fcctrhisto[xmtd]++;
else
fccthisto[xmtd]++;
if(xmtd)
fccrthisto[rcvd]++;
else
fccrhisto[rcvd]++;
#endif
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
char *p;
int len, i, r;
Ctlr *ctlr;
MiiPhy *phy;
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
len += snprint(p+len, READSTR-len, "framesdeferred: %lud\n", ctlr->deferred);
miistatus(ctlr->mii);
phy = ctlr->mii->curphy;
len += snprint(p+len, READSTR-len, "phy: link=%d, tfc=%d, rfc=%d, speed=%d, fd=%d\n",
phy->link, phy->tfc, phy->rfc, phy->speed, phy->fd);
#ifdef DBG
if(ctlr->mii != nil && ctlr->mii->curphy != nil){
len += snprint(p+len, READSTR, "phy:   ");
for(i = 0; i < NMiiPhyr; i++){
if(i && ((i & 0x07) == 0))
len += snprint(p+len, READSTR-len, "\n       ");
r = miimir(ctlr->mii, i);
len += snprint(p+len, READSTR-len, " %4.4uX", r);
}
snprint(p+len, READSTR-len, "\n");
}
#endif
snprint(p+len, READSTR-len, "\n");
n = readstr(offset, a, n, p);
free(p);
return n;
}
IMM* imm;
static int
fccsetup(Ctlr *ctlr, FCC *fcc, uchar *ea)
{
int i;
Etherparam *p;
MiiPhy *phy;
fcc->gfmr &= ~(ENR | ENT);
ioplock();
switch(ctlr->port) {
default:
iopunlock();
return -1;
case 0:
ctlr->pmdio = 0x01000000;
ctlr->pmdck = 0x08000000;
imm->port[0].pdir &= ~A1dir0;
imm->port[0].pdir |= A1dir1;
imm->port[0].psor &= ~A1psor0;
imm->port[0].psor |= A1psor1;
imm->port[0].ppar |= (A1dir0 | A1dir1);
imm->port[2].psor &= ~0x00000c00;
imm->port[2].pdir &= ~0x00000c00;
imm->port[2].ppar |= 0x00000c00;
imm->port[3].pdat |= (ctlr->pmdio | ctlr->pmdck);
imm->port[3].podr |= ctlr->pmdio;
imm->port[3].pdir |= (ctlr->pmdio | ctlr->pmdck);
imm->port[3].ppar &= ~(ctlr->pmdio | ctlr->pmdck);
eieio();
imm->cmxfcr &= ~0xff000000;
imm->cmxfcr |= 0x37000000;
break;
case 1:
ctlr->pmdio = 0x00400000;
ctlr->pmdck = 0x00200000;
imm->port[1].pdir &= ~B2dir0;
imm->port[1].pdir |= B2dir1;
imm->port[1].psor &= ~B2psor0;
imm->port[1].psor |= B2psor1;
imm->port[1].ppar |= (B2dir0 | B2dir1);
imm->port[2].psor &= ~0x00003000;
imm->port[2].pdir &= ~0x00003000;
imm->port[2].ppar |= 0x00003000;
imm->port[2].pdat |= (ctlr->pmdio | ctlr->pmdck);
imm->port[2].podr |= ctlr->pmdio;
imm->port[2].pdir |= (ctlr->pmdio | ctlr->pmdck);
imm->port[2].ppar &= ~(ctlr->pmdio | ctlr->pmdck);
eieio();
imm->cmxfcr &= ~0x00ff0000;
imm->cmxfcr |= 0x00250000;
break;
case 2:
imm->port[1].pdir &= ~B3dir0;
imm->port[1].pdir |= B3dir1;
imm->port[1].psor &= ~B3psor0;
imm->port[1].psor |= B3psor1;
imm->port[1].ppar |= (B3dir0 | B3dir1);
imm->port[2].psor &= ~0x0000c000;
imm->port[2].pdir &= ~0x0000c000;
imm->port[2].ppar |= 0x0000c000;
imm->port[3].pdat |= (ctlr->pmdio | ctlr->pmdck);
imm->port[3].podr |= ctlr->pmdio;
imm->port[3].pdir |= (ctlr->pmdio | ctlr->pmdck);
imm->port[3].ppar &= ~(ctlr->pmdio | ctlr->pmdck);
eieio();
imm->cmxfcr &= ~0x0000ff00;
imm->cmxfcr |= 0x00003700;
break;
}
iopunlock();
p = (Etherparam*)(m->immr->prmfcc + ctlr->port);
memset(p, 0, sizeof(Etherparam));
fcc->gfmr |= ENET;
fcc->fpsmr = CRCE | FDE | LPB;
ctlr->duplex = ~0;
fcc->fdsr = 0xd555;
p->rbase = PADDR(ctlr->rdr);
p->tbase = PADDR(ctlr->tdr);
p->rstate = (GBL | EB) << 24;
p->tstate = (GBL | EB) << 24;
p->cmask = 0xdebb20e3;
p->cpres = 0xffffffff;
p->retlim = 15;
p->mrblr = (Rbsize+0x1f)&~0x1f;
p->mflr = Rbsize;
p->minflr = ETHERMINTU;
p->maxd1 = (Rbsize+7) & ~7;
p->maxd2 = (Rbsize+7) & ~7;
for(i=0; i<Eaddrlen; i+=2)
p->paddr[2-i/2] = (ea[i+1]<<8)|ea[i];
p->riptr = m->immr->fccextra[ctlr->port].ri - (uchar*)IMMR;
p->tiptr = m->immr->fccextra[ctlr->port].ti - (uchar*)IMMR;
p->padptr = m->immr->fccextra[ctlr->port].pad - (uchar*)IMMR;
memset(m->immr->fccextra[ctlr->port].pad, 0x88, 0x20);
fcc->fcce = ~0;
fcc->fccm = TXE | RXF | TXB;
cpmop(InitRxTx, fccid[ctlr->port], 0xc);
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
return -1;
ctlr->mii->mir = fccmiimir;
ctlr->mii->miw = fccmiimiw;
ctlr->mii->ctlr = ctlr;
if(mii(ctlr->mii, ~0) == 0 || (phy = ctlr->mii->curphy) == nil){
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
miiane(ctlr->mii, ~0, ~0, ~0);
#ifdef DBG
print("oui=%X, phyno=%d, ", phy->oui, phy->phyno);
print("anar=%ux, ", phy->anar);
print("fc=%ux, ", phy->fc);
print("mscr=%ux, ", phy->mscr);
print("link=%ux, ", phy->link);
print("speed=%ux, ", phy->speed);
print("fd=%ux, ", phy->fd);
print("rfc=%ux, ", phy->rfc);
print("tfc=%ux\n", phy->tfc);
#endif
return 0;
}
static int
reset(Ether* ether)
{
uchar ea[Eaddrlen];
Ctlr *ctlr;
FCC *fcc;
Block *b;
int i;
if(m->cpuhz < 24000000){
print("%s ether: system speed must be >= 24MHz for ether use\n", ether->type);
return -1;
}
if(ether->port > 3){
print("%s ether: no FCC port %ld\n", ether->type, ether->port);
return -1;
}
ether->irq = fccirq[ether->port];
ether->tbdf = BusPPC;
fcc = imm->fcc + ether->port;
ctlr = malloc(sizeof(*ctlr));
ether->ctlr = ctlr;
memset(ctlr, 0, sizeof(*ctlr));
ctlr->fcc = fcc;
ctlr->port = ether->port;
ctlr->fccid = fccid[ether->port];
if(ioringinit(ctlr, Nrdre, Ntdre, 0) < 0)
panic("etherfcc init");
for(i = 0; i < Nrdre; i++){
b = iallocb(Bufsize);
b->rp = (uchar*)(((ulong)b->rp + CACHELINESZ-1) & ~(CACHELINESZ-1));
b->wp = b->rp;
ctlr->rcvbufs[i] = b;
ctlr->rdr[i].addr = PADDR(b->wp);
}
fccsetup(ctlr, fcc, ether->ea);
ether->mbps = 100;
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
print("no ether address");
return -1;
}
return 0;
}
void
etherfcclink(void)
{
addethercard("fcc", reset);
}
static void
nanodelay(void)
{
static int count;
int i;
for(i = 0; i < 500; i++)
count++;
return;
}
static
void miiwriteloop(Ctlr *ctlr, Port *port, int cnt, ulong cmd)
{
int i;
for(i = 0; i < cnt; i++){
port->pdat &= ~ctlr->pmdck;
if(cmd & BIT(i))
port->pdat |= ctlr->pmdio;
else
port->pdat &= ~ctlr->pmdio;
nanodelay();
port->pdat |= ctlr->pmdck;
nanodelay();
}
}
static int
fccmiimiw(Mii *mii, int pa, int ra, int data)
{
int x;
Port *port;
ulong cmd;
Ctlr *ctlr;
ctlr = mii->ctlr;
port = imm->port + 3;
cmd = MDIwrite | (pa<<(5+2+16))| (ra<<(2+16)) | (data & 0xffff);
x = splhi();
port->pdir |= (ctlr->pmdio|ctlr->pmdck);
nanodelay();
miiwriteloop(ctlr, port, 32, ~0);
miiwriteloop(ctlr, port, 32, cmd);
port->pdir |= (ctlr->pmdio|ctlr->pmdck);
nanodelay();
miiwriteloop(ctlr, port, 32, ~0);
splx(x);
return 1;
}
static int
fccmiimir(Mii *mii, int pa, int ra)
{
int data, i, x;
Port *port;
ulong cmd;
Ctlr *ctlr;
ctlr = mii->ctlr;
port = imm->port + 3;
cmd = MDIread | pa<<(5+2+16) | ra<<(2+16);
x = splhi();
port->pdir |= (ctlr->pmdio|ctlr->pmdck);
nanodelay();
miiwriteloop(ctlr, port, 32, ~0);
miiwriteloop(ctlr, port, 14, cmd);
port->pdat &= ~ctlr->pmdck;
port->pdir &= ~ctlr->pmdio;
nanodelay();
data = 0;
for(i=0; i<18; i++){
data <<= 1;
if(port->pdat & ctlr->pmdio)
data |= 1;
port->pdat |= ctlr->pmdck;
nanodelay();
port->pdat &= ~ctlr->pmdck;
nanodelay();
}
port->pdir |= (ctlr->pmdio|ctlr->pmdck);
nanodelay();
miiwriteloop(ctlr, port, 32, ~0);
splx(x);
return data & 0xffff;
}
static void
fccltimer(Ureg*, Timer *t)
{
Ether *ether;
Ctlr *ctlr;
MiiPhy *phy;
ulong gfmr;
ether = t->ta;
ctlr = ether->ctlr;
if(ctlr->mii == nil || ctlr->mii->curphy == nil)
return;
phy = ctlr->mii->curphy;
if(miistatus(ctlr->mii) < 0){
print("miistatus failed\n");
return;
}
if(phy->link == 0){
print("link lost\n");
return;
}
ether->mbps = phy->speed;
if(phy->fd != ctlr->duplex)
print("set duplex\n");
ilock(ctlr);
gfmr = ctlr->fcc->gfmr;
if(phy->fd != ctlr->duplex){
ctlr->fcc->gfmr &= ~(ENR|ENT);
if(phy->fd)
ctlr->fcc->fpsmr |= FDE | LPB;
else
ctlr->fcc->fpsmr &= ~(FDE | LPB);
ctlr->duplex = phy->fd;
}
ctlr->fcc->gfmr = gfmr;
iunlock(ctlr);
}