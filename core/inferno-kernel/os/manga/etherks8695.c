#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "ureg.h"
#define	DBG	if(0)iprint
#define	MIIDBG	if(0)iprint
enum {
Nrdre		= 64,
Ntdre		= 32,
Rbsize		= ROUNDUP(ETHERMAXTU+4, 4),
Bufsize		= ROUNDUP(Rbsize, CACHELINESZ),
};
typedef struct DmaReg DmaReg;
struct DmaReg {
ulong	dtxc;
ulong	drxc;
ulong	dtsc;
ulong	drsc;
ulong	tdlb;
ulong	rdlb;
ulong	mal;
ulong	mah;
ulong	pad[0x80-0x20];
ulong	maal[16][2];
};
enum {
TxSoftReset=	1<<31,
TxUDPck=	1<<18,
TxTCPck=		1<<17,
TxIPck=		1<<16,
TxFCE=		1<<9,
TxLB=		1<<8,
TxEP=		1<<2,
TxCrc=		1<<1,
TxEnable=	1<<0,
RxUDPck=	1<<18,
RxTCPck=		1<<17,
RxIPck=		1<<16,
RxFCE=		1<<9,
RxRB=		1<<6,
RxRM=		1<<5,
RxRU=		1<<4,
RxAE=		1<<3,
RxRA=		1<<2,
RxEnable=	1<<0,
};
typedef struct WanPhy WanPhy;
struct WanPhy {
ulong	did;
ulong	rid;
ulong	pad0;
ulong	wmc;
ulong	wppm;
ulong	wpc;
ulong	wps;
ulong	pps;
};
enum {
WAnc=	1<<30,
WAnr=	1<<29,
WAnaP=	1<<28,
WAna100FD=	1<<27,
WAna100HD=	1<<26,
WAna10FD=	1<<25,
WAna10HD=	1<<24,
WLs=	1<<23,
WDs=	1<<22,
WSs=	1<<21,
WLparP=	1<<20,
WLpar100FD=	1<<19,
WLpar100HD=	1<<18,
WLpar10FD=	1<<17,
WLpar10HD=	1<<16,
WAnDis=	1<<15,
WForce100=	1<<14,
WForceFD=	1<<13,
LedSpeed=	0,
LedLink,
LedFD,
LedColl,
LedTxRx,
LedFDColl,
LedLinkTxRx,
WLpbk=	1<<14,
WRlpblk=	1<<13,
WPhyIso=	1<<12,
WPhyLink=	1<<10,
WMdix=	1<<9,
WFef=	1<<8,
WAmdixp=	1<<7,
WTxdis=	1<<6,
WDfef=	1<<5,
Wpd=	1<<4,
WDmdx=	1<<3,
WFmdx=	1<<2,
WMlpbk=	1<<1,
Ppsm=	1<<0,
};
#define	DMABURST(n)	((n)<<24)
typedef struct {
Lock;
int	port;
int	init;
int	active;
int	reading;
ulong	anap;
DmaReg*	regs;
WanPhy*	wphy;
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
static void	switchinit(uchar*);
static void switchdump(void);
static void
attach(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
if(!ctlr->active){
ctlr->regs->dtxc |= TxEnable;
ctlr->regs->drxc |= RxEnable;
microdelay(10);
ctlr->regs->drsc = 1;
microdelay(10);
ctlr->reading = (INTRREG->st & (1<<IRQwmrps)) == 0;
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
ctlr->regs->dtxc &= ~TxEnable;
ctlr->regs->drxc &= ~RxEnable;
ctlr->active = 0;
iunlock(ctlr);
}
}
static void
promiscuous(void* arg, int on)
{
Ether *ether;
Ctlr *ctlr;
ulong w;
ether = (Ether*)arg;
ctlr = ether->ctlr;
ilock(ctlr);
w = ctlr->regs->drxc;
if(on != ((w&RxRA)!=0)){
ctlr->regs->drxc = w ^ RxRA;
}
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
if(ether->nmaddr)
ctlr->regs->drxc |= RxRM;
else
ctlr->regs->drxc &= ~RxRM;
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
if(dre->ctrl & BdBusy)
panic("ether: txstart");
len = BLEN(b);
if(ctlr->txb[ctlr->tdrh] != nil)
panic("etherks8695: txstart");
ctlr->txb[ctlr->tdrh] = b;
dcflush(b->rp, len);
dre->addr = PADDR(b->rp);
dre->size = TxIC|TxFS|TxLS | len;
dre->ctrl = BdBusy;
ctlr->regs->dtsc = 1;
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
while(((status = dre->ctrl) & BdBusy) == 0){
if(status & RxES || (status & (RxFS|RxLS)) != (RxFS|RxLS)){
if(status & (RxRF|RxTL))
ether->buffs++;
if(status & RxRE)
ether->frames++;
if(status & RxCE)
ether->crcs++;
iprint("eth rx: %lux\n", status);
}else{
b = clallocb();
if(b != nil){
rb = ctlr->rxb[ctlr->rdrx];
rb->wp += (dre->ctrl & RxFL)-4;
etheriq(ether, rb, 1);
ctlr->rxb[ctlr->rdrx] = b;
dre->addr = PADDR(b->wp);
}else
ether->soverflows++;
}
dre->ctrl = BdBusy;
ctlr->rdrx = NEXT(ctlr->rdrx, ctlr->nrdre);
dre = &ctlr->rdr[ctlr->rdrx];
}
}
static void
txring(Ureg*, void *arg)
{
Ether *ether;
Ctlr *ctlr;
BD *dre;
Block *b;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
lock(ctlr);
while(ctlr->ntq){
dre = &ctlr->tdr[ctlr->tdri];
if(dre->ctrl & BdBusy)
break;
b = ctlr->txb[ctlr->tdri];
if(b == nil)
panic("etherks8695: bufp");
ctlr->txb[ctlr->tdri] = nil;
freeb(b);
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, ctlr->ntdre);
}
txstart(ether);
unlock(ctlr);
}
static void
rbuintr(Ureg*, void *arg)
{
Ether *ether;
Ctlr *ctlr;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
if(ctlr->active)
ctlr->overrun++;
ctlr->reading = 0;
}
static void
rxstopintr(Ureg*, void *arg)
{
Ether *ether;
Ctlr *ctlr;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
if(!ctlr->active)
return;
iprint("rxstopintr\n");
ctlr->regs->drsc = 1;
}
static void
txstopintr(Ureg*, void *arg)
{
Ether *ether;
Ctlr *ctlr;
ether = arg;
ctlr = ether->ctlr;
ctlr->interrupts++;
if(!ctlr->active)
return;
iprint("txstopintr\n");
ctlr->regs->dtsc = 1;
}
static void
linkchangeintr(Ureg*, void*)
{
iprint("link change\n");
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
{DmaReg *d = ctlr->regs; len += snprint(p+len, READSTR-len, "dtxc=%8.8lux drxc=%8.8lux\n", d->dtxc, d->drxc);}
snprint(p+len, READSTR-len, "framesdeferred: %lud\n", ctlr->deferred);
n = readstr(offset, a, n, p);
free(p);
if(ctlr->port == 1)
switchdump();
return n;
}
static void
physinit(Ether *ether, int force)
{
Ctlr *ctlr;
WanPhy *p;
ulong anap;
int i;
ctlr = ether->ctlr;
p = ctlr->wphy;
if(p == nil){
if(ctlr->port){
ether->mbps = 100;
ether->fullduplex = 1;
switchinit(nil);
}
return;
}
iprint("phy%d: wmc=%8.8lux wpm=%8.8lux wpc=%8.8lux wps=%8.8lux pps=%8.8lux\n", ctlr->port, p->wmc, p->wppm, p->wpc, p->wps, p->pps);
p->wppm = 0;
if(p->rid & 7)
p->wpc = 0x0200b000;
else
p->wpc = 0xb000;
if(p->wppm & WFef)
iprint("ether%d: far end fault\n", ctlr->port);
if((p->wmc & WLs) == 0){
iprint("ether%d: no link\n", ctlr->port);
ether->mbps = 100;
ether->fullduplex = 0;
return;
}
if((p->wmc & WAnc) == 0 || force){
p->wmc = WAnr | WAnaP | WAna100FD | WAna100HD | WAna10FD | WAna10HD | (p->wmc & 0x7F);
microdelay(10);
if(p->wmc & WLs){
for(i=0;; i++){
if(i > 600){
iprint("ether%d: auto negotiation failed\n", ctlr->port);
ether->mbps = 10;
ether->fullduplex = 0;
return;
}
if(p->wmc & WAnc){
microdelay(10);
break;
}
delay(1);
}
}
}
anap = p->wmc;
ether->mbps = anap & WSs? 100: 10;
if(anap & (WLpar100FD|WLpar10FD) && anap & WDs)
ether->fullduplex = 1;
else
ether->fullduplex = 0;
ctlr->anap = anap;
iprint("ks8695%d mii: fd=%d speed=%d wmc=%8.8lux\n", ctlr->port, ether->fullduplex, ether->mbps, anap);
}
static void
ctlrinit(Ctlr *ctlr, Ether *ether)
{
int i;
DmaReg *em;
ulong mode;
em = ctlr->regs;
em->dtxc = TxSoftReset;
microdelay(10);
for(i=0; em->dtxc & TxSoftReset; i++){
if(i > 20){
iprint("etherks8695.%d: soft reset failed\n", ctlr->port);
i=0;
}
microdelay(100);
}
iprint("%d: rx=%8.8lux tx=%8.8lux\n", ctlr->port, PADDR(ctlr->rdr), PADDR(ctlr->tdr));
physinit(ether, 0);
em->mah = (ether->ea[0]<<8) | ether->ea[1];
em->mal = (ether->ea[2]<<24) | (ether->ea[3]<<16) | (ether->ea[4]<<8) | ether->ea[5];
if(ctlr->port == 0){
for(i=0; i<nelem(em->maal); i++){
em->maal[i][0] = 0;
em->maal[i][1] = 0;
}
}
em->tdlb = PADDR(ctlr->tdr);
em->dtxc = DMABURST(8) | TxFCE | TxCrc;
em->rdlb = PADDR(ctlr->rdr);
mode = DMABURST(8) | RxRB | RxRU | RxAE;
if(ether->fullduplex)
mode |= RxFCE;
em->drxc = mode;
}
static int
reset(Ether* ether)
{
uchar ea[Eaddrlen];
char name[KNAMELEN];
Ctlr *ctlr;
int i, irqdelta;
snprint(name, sizeof(name), "ether%d", ether->ctlrno);
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
print("%s (%s %ld): no ether address", name, ether->type, ether->port);
return -1;
}
ctlr = malloc(sizeof(*ctlr));
ctlr->port = ether->port;
switch(ether->port){
case 0:
ctlr->regs = KADDR(PHYSWANDMA);
ctlr->wphy = KADDR(PHYSMISC);
ctlr->wphy->wmc = (ctlr->wphy->wmc & ~0x7F) | (LedLinkTxRx<<0) | (LedSpeed<<4);
break;
case 1:
ctlr->regs = KADDR(PHYSLANDMA);
ctlr->wphy = nil;
break;
default:
print("%s: %s ether: no port %lud\n", name, ether->type, ether->port);
free(ctlr);
return -1;
}
ether->ctlr = ctlr;
irqdelta = ether->irq - IRQwmrps;
physinit(ether, 0);
if(ioringinit(ctlr, Nrdre, Ntdre) < 0)
panic("etherks8695 initring");
for(i = 0; i < ctlr->nrdre; i++){
if(ctlr->rxb[i] == nil)
ctlr->rxb[i] = clallocb();
ctlr->rdr[i].addr = PADDR(ctlr->rxb[i]->wp);
ctlr->rdr[i].size = Rbsize;
ctlr->rdr[i].ctrl = BdBusy;
}
ctlrinit(ctlr, ether);
ether->attach = attach;
ether->closed = closed;
ether->transmit = transmit;
ether->ifstat = ifstat;
ether->irq = irqdelta + IRQwmrs;
ether->interrupt = rxring;
intrenable(IRQ, irqdelta+IRQwmts, txring, ether, "ethertx");
intrenable(IRQ, irqdelta+IRQwmrbu, rbuintr, ether, "etherrbu");
intrenable(IRQ, irqdelta+IRQwmrps, rxstopintr, ether, "etherrps");
intrenable(IRQ, irqdelta+IRQwmtps, txstopintr, ether, "ethertps");
if(ether->port == 0)
intrenable(IRQ, IRQwmlc, linkchangeintr, ether, "etherwanlink");
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
return 0;
}
typedef struct Switch Switch;
struct Switch {
ulong	sec0;
ulong	sec1;
ulong	sec2;
ulong	cfg[5][3];
ulong	an[2];
ulong	seiac;
ulong	seiadh2;
ulong	seiadh1;
ulong	seiadl;
ulong	seafc;
ulong	scph;
ulong	scpl;
ulong	mah;
ulong	mal;
ulong	ppm[2];
};
enum {
Nbe=	1<<31,
Unh=	1<<21,
Lca=		1<<20,
Paf=		1<<19,
Sfce=	1<<18,
Flfc=		1<<17,
Bsm=	1<<16,
Age=	1<<15,
Agef=	1<<14,
Aboe=	1<<13,
Uvmd=	1<<12,
Mspd=	1<<11,
Bpm=	1<<10,
Fair=		1<<9,
Ncd=	1<<8,
Lmpsd=	1<<7,
Pbr=		1<<6,
Sbpe=	1<<5,
Shdm=	1<<4,
PrioHi=	0<<2,
Prio10_1= 1<<2,
Prio5_1=	2<<2,
Prio2_1=	3<<2,
Etm=	1<<1,
Esf=		1<<0,
IEEEneg=	1<<11,
Tpid=	1<<10,
PhyEn=	1<<8,
TfcDis=	1<<7,
RfcDis=	1<<6,
Hps=	1<<5,
VlanEn=	1<<4,
Sw10BT=	1<<1,
VIDrep=	1<<0,
};
#define	BASEPRIO(n)	(((n)&7)<<28)
enum {
AnegDis=	1<<15,
Force100=	1<<14,
ForceFD=	1<<13,
STTxEn=	1<<7,
STRxEn=	1<<6,
STLnDis=	1<<5,
Bsp=		1<<4,
Pce=		1<<3,
Dpce=	1<<2,
IEEEpce=	1<<1,
PrioEn=	1<<0,
IngressFilter=	1<<28,
DiscardNonPVID=	1<<27,
ForcePortFC=	1<<26,
EnablePortBP=	1<<25,
Rdprc=	1<<7,
Lprrc=	1<<6,
Hprrc=	1<<5,
Lprfce=	1<<4,
Hprfce=	1<<3,
Tdprc=	1<<2,
Lptrc=	1<<1,
Hptrc=	1<<0,
Cread=	1<<12,
Cwrite=	0<<12,
StaticMacs=	0<<10,
VLANs=		1<<10,
DynMacs=	2<<10,
MibCounter=	3<<10,
};
enum {
VlanValid=	1<<21,
MACempty=	1<<(68-2*32),
NotReady=	1<<(55-32),
NVlans=	16,
NSMacs=	8,
};
static char* portmibnames[] = {
"RxLoPriorityByte",
"RxHiPriorityByte",
"RxUndersizePkt",
"RxFragments",
"RxOversize",
"RxJabbers",
"RxSymbolError",
"RxCRCerror",
"RxAlignmentError",
"RxControl8808Pkts",
"RxPausePkts",
"RxBroadcast",
"RxMulticast",
"RxUnicast",
"Rx64Octets",
"Rx65to127Octets",
"Rx128to255Octets",
"Rx256to511Octets",
"Rx512to1023Octets",
"Rx1024to1522Octets",
"TxLoPriorityByte",
"TxHiPriorityByte",
"TxLateCollision",
"TxPausePkts",
"TxBroadcastPkts",
"TxMulticastPkts",
"TxUnicastPkts",
"TxDeferred",
"TxTotalCollision",
"TxExcessiveCollision",
"TxSingleCollision",
"TxMultipleCollision",
};
enum {
MibOverflow=	1<<31,
MibValid=		1<<30,
};
static char* allportnames[] = {
"Port1TxDropPackets",
"Port2TxDropPackets",
"Port3TxDropPackets",
"Port4TxDropPackets",
"LanTxDropPackets",
"Port1RxDropPackets",
"Port2RxDropPackets",
"Port3RxDropPackets",
"Port4RxDropPackets",
"LanRxDropPackets",
};
static void
switchinit(uchar *ea)
{
Switch *sw;
int i;
ulong an;
GPIOREG->iopm |= 0xF0;
iprint("switch init...\n");
sw = KADDR(PHYSSWITCH);
if(sw->sec0 & Esf){
iprint("already inited\n");
return;
}
sw->seafc = 0;
microdelay(10);
sw->scph = 0;
microdelay(10);
sw->scpl = 0;
microdelay(10);
if(ea != nil){
sw->mah = (ea[0]<<8) | ea[1];
microdelay(10);
sw->mal = (ea[2]<<24) | (ea[3]<<16) | (ea[4]<<8) | ea[5];
microdelay(10);
}
for(i = 0; i < 5; i++){
sw->cfg[i][0] = (0x1F<<8) | STTxEn | STRxEn | Bsp;
microdelay(10);
sw->cfg[i][1] = 0;
microdelay(10);
sw->cfg[i][2] = 0;
microdelay(10);
}
sw->ppm[0] = 0;
microdelay(10);
sw->ppm[1] = 0;
microdelay(10);
an = WAnr | WAnaP | WAna100FD | WAna100HD | WAna10FD | WAna10HD;
sw->an[0] = an | (an >> 16);
microdelay(10);
sw->an[1] = an | (an >> 16);
microdelay(10);
sw->sec1 = (0x4A<<21) | PhyEn;
microdelay(10);
sw->sec0 = Nbe | (0<<28) | (LedSpeed<<25) | (LedLinkTxRx<<22) | Sfce | Bsm | Age | Aboe | Bpm | Fair | Sbpe | Shdm | Esf;
microdelay(10);
}
typedef struct Vidmap Vidmap;
struct Vidmap {
uchar	ports;
uchar	fid;
ushort	vid;
};
static Vidmap
getvidmap(Switch *sw, int i)
{
ulong w;
Vidmap v;
v.ports = 0;
v.fid = 0;
v.vid = 0;
if(i < 0 || i >= NVlans)
return v;
sw->seiac = Cread | VLANs | i;
microdelay(10);
w = sw->seiadl;
if((w & VlanValid) == 0)
return v;
v.vid = w & 0xFFFF;
v.fid = (w>>12) & 0xF;
v.ports = (w>>16) & 0x1F;
return v;
}
static void
putvidmap(Switch *sw, int i, Vidmap v)
{
ulong w;
w = ((v.ports & 0x1F)<<16) | ((v.fid & 0xF)<<12) | (v.vid & 0xFFFF);
if(v.vid != 0)
w |= VlanValid;
sw->seiadl = w;
microdelay(10);
sw->seiac = Cwrite | VLANs | i;
microdelay(10);
}
typedef struct StaticMac StaticMac;
struct StaticMac {
uchar	valid;
uchar	fid;
uchar	usefid;
uchar	override;
uchar	ports;
uchar	mac[Eaddrlen];
};
static StaticMac
getstaticmac(Switch *sw, int i)
{
StaticMac s;
ulong w;
memset(&s, 0, sizeof(s));
if(i < 0 || i >= NSMacs)
return s;
sw->seiac = Cread | StaticMacs | i;
microdelay(10);
w = sw->seiadh1;
if((w & (1<<(53-32))) == 0)
return s;
s.valid = 1;
s.fid= (w>>(57-32)) & 0xF;
s.usefid = (w & (1<<(56-32))) != 0;
s.override = (w & (1<<(54-32))) != 0;
s.ports = (w>>(48-32)) & 0x1F;
s.mac[5] = w >> 8;
s.mac[4] = w;
w = sw->seiadl;
s.mac[3] = w>>24;
s.mac[2] = w>>16;
s.mac[1] = w>>8;
s.mac[0] = w;
return s;
}
static void
putstaticmac(Switch *sw, int i, StaticMac s)
{
ulong w;
if(s.valid){
w = 1<<(53-32);
if(s.usefid)
w |= 1<<(55-32);
if(s.override)
w |= 1<<(54-32);
w |= (s.fid & 0xF) << (56-32);
w |= (s.ports & 0x1F) << (48-32);
w |= (s.mac[5] << 8) | s.mac[4];
sw->seiadh1 = w;
microdelay(10);
w = (s.mac[3]<<24) | (s.mac[2]<<16) | (s.mac[1]<<8) | s.mac[0];
sw->seiadl = w;
microdelay(10);
}else{
sw->seiadh1 = 0;
microdelay(10);
}
sw->seiac = Cwrite | StaticMacs | i;
microdelay(10);
}
typedef struct DynMac DynMac;
struct DynMac {
ushort	nentry;
uchar	valid;
uchar	age;
uchar	port;
uchar	fid;
uchar	mac[Eaddrlen];
};
static DynMac
getdynmac(Switch *sw, int i)
{
DynMac d;
ulong w;
int n, l;
memset(&d, 0, sizeof d);
l = 0;
do{
if(++l > 100)
return d;
sw->seiac = Cread | DynMacs | i;
microdelay(10);
w = sw->seiadh2;
if(w & MACempty)
return d;
n = w & 0xF;
w = sw->seiadh1;
}while(w & NotReady);
d.nentry = ((n<<6) | (w>>(58-32))) + 1;
if(i < 0 || i >= d.nentry)
return d;
d.valid = 1;
d.age = (w>>(56-32)) & 3;
d.port = (w>>(52-32)) & 7;
d.fid = (w>>(48-32)) & 0xF;
d.mac[5] = w>>8;
d.mac[4] = w;
w = sw->seiadl;
d.mac[3] = w>>24;
d.mac[2] = w>>16;
d.mac[1] = w>>8;
d.mac[0] = w;
return d;
}
static void
switchdump(void)
{
Switch *sw;
int i, j;
ulong w;
sw = KADDR(PHYSSWITCH);
iprint("sec0 %8.8lux\n", sw->sec0);
iprint("sec1 %8.8lux\n", sw->sec1);
for(i = 0; i < 5; i++){
iprint("cfg%d", i);
for(j = 0; j < 3; j++){
w = sw->cfg[i][j];
iprint(" %8.8lux", w);
}
iprint("\n");
if(i < 2){
w = sw->an[i];
iprint(" an=%8.8lux pm=%8.8lux\n", w, sw->ppm[i]);
}
}
for(i = 0; i < 8; i++){
sw->seiac = Cread | DynMacs | i;
microdelay(10);
w = sw->seiadh2;
microdelay(10);
iprint("dyn%d: %8.8lux", i, w);
w = sw->seiadh1;
microdelay(10);
iprint(" %8.8lux", w);
w = sw->seiadl;
microdelay(10);
iprint(" %8.8lux\n", w);
}
for(i=0; i<0x20; i++){
sw->seiac = Cread | MibCounter | i;
microdelay(10);
w = sw->seiadl;
microdelay(10);
if(w & (1<<30))
iprint("%.2ux: %s: %lud\n", i, portmibnames[i], w & ~(3<<30));
}
}
static void
switchstatproc(void*)
{
for(;;){
tsleep(&up->sleep, return0, nil, 30*1000);
}
}
void
etherks8695link(void)
{
addethercard("ks8695", reset);
}