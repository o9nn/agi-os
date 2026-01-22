#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
typedef struct QLock { int r; } QLock;
#define qlock(i)	while(0)
#define qunlock(i)	while(0)
#define iallocb		allocb
#define iprint		print
#define mallocalign(n, a, o, s)	ialloc((n), (a))
#include "etherif.h"
#include "ethermii.h"
enum {
Idr0		= 0x00,
Mar0		= 0x08,
Dtccr		= 0x10,
Tnpds		= 0x20,
Thpds		= 0x28,
Flash		= 0x30,
Erbcr		= 0x34,
Ersr		= 0x36,
Cr		= 0x37,
Tppoll		= 0x38,
Imr		= 0x3C,
Isr		= 0x3E,
Tcr		= 0x40,
Rcr		= 0x44,
Tctr		= 0x48,
Mpc		= 0x4C,
Cr9346		= 0x50,
Config0		= 0x51,
Config1		= 0x52,
Config2		= 0x53,
Config3		= 0x54,
Config4		= 0x55,
Config5		= 0x56,
Timerint	= 0x58,
Mulint		= 0x5C,
Phyar		= 0x60,
Tbicsr0		= 0x64,
Tbianar		= 0x68,
Tbilpar		= 0x6A,
Phystatus	= 0x6C,
Rms		= 0xDA,
Cplusc		= 0xE0,
Rdsar		= 0xE4,
Mtps		= 0xEC,
};
enum {
Cmd		= 0x00000008,
};
enum {
Te		= 0x04,
Re		= 0x08,
Rst		= 0x10,
};
enum {
Fswint		= 0x01,
Npq		= 0x40,
Hpq		= 0x80,
};
enum {
Rok		= 0x0001,
Rer		= 0x0002,
Tok		= 0x0004,
Ter		= 0x0008,
Rdu		= 0x0010,
Punlc		= 0x0020,
Fovw		= 0x0040,
Tdu		= 0x0080,
Swint		= 0x0100,
Timeout		= 0x4000,
Serr		= 0x8000,
};
enum {
MtxdmaSHIFT	= 8,
MtxdmaMASK	= 0x00000700,
Mtxdmaunlimited	= 0x00000700,
Acrc		= 0x00010000,
Lbk0		= 0x00020000,
Lbk1		= 0x00040000,
Ifg2		= 0x00080000,
HwveridSHIFT	= 23,
HwveridMASK	= 0x7C800000,
Macv01		= 0x00000000,
Macv02		= 0x00800000,
Macv03		= 0x04000000,
Macv04		= 0x10000000,
Macv05		= 0x18000000,
Macv11		= 0x30000000,
Macv12		= 0x38000000,
Macv13		= 0x34000000,
Macv14		= 0x30800000,
Macv15		= 0x38800000,
Ifg0		= 0x01000000,
Ifg1		= 0x02000000,
};
enum {
Aap		= 0x00000001,
Apm		= 0x00000002,
Am		= 0x00000004,
Ab		= 0x00000008,
Ar		= 0x00000010,
Aer		= 0x00000020,
Sel9356		= 0x00000040,
MrxdmaSHIFT	= 8,
MrxdmaMASK	= 0x00000700,
Mrxdmaunlimited	= 0x00000700,
RxfthSHIFT	= 13,
RxfthMASK	= 0x0000E000,
Rxfth256	= 0x00008000,
Rxfthnone	= 0x0000E000,
Rer8		= 0x00010000,
MulERINT	= 0x01000000,
};
enum {
Eedo		= 0x01,
Eedi		= 0x02,
Eesk		= 0x04,
Eecs		= 0x08,
Eem0		= 0x40,
Eem1		= 0x80,
};
enum {
DataMASK	= 0x0000FFFF,
DataSHIFT	= 0,
RegaddrMASK	= 0x001F0000,
RegaddrSHIFT	= 16,
Flag		= 0x80000000,
};
enum {
Fd		= 0x01,
Linksts		= 0x02,
Speed10		= 0x04,
Speed100	= 0x08,
Speed1000	= 0x10,
Rxflow		= 0x20,
Txflow		= 0x40,
Entbi		= 0x80,
};
enum {
Mulrw		= 0x0008,
Dac		= 0x0010,
Rxchksum	= 0x0020,
Rxvlan		= 0x0040,
Endian		= 0x0200,
};
typedef struct D D;
struct D {
u32int	control;
u32int	vlan;
u32int	addrlo;
u32int	addrhi;
};
enum {
TxflMASK	= 0x0000FFFF,
TxflSHIFT	= 0,
Tcps		= 0x00010000,
Udpcs		= 0x00020000,
Ipcs		= 0x00040000,
Lgsen		= 0x08000000,
};
enum {
RxflMASK	= 0x00003FFF,
RxflSHIFT	= 0,
Tcpf		= 0x00004000,
Udpf		= 0x00008000,
Ipf		= 0x00010000,
Pid0		= 0x00020000,
Pid1		= 0x00040000,
Crce		= 0x00080000,
Runt		= 0x00100000,
Res		= 0x00200000,
Rwt		= 0x00400000,
Fovf		= 0x00800000,
Bovf		= 0x01000000,
Bar		= 0x02000000,
Pam		= 0x04000000,
Mar		= 0x08000000,
};
enum {
Ls		= 0x10000000,
Fs		= 0x20000000,
Eor		= 0x40000000,
Own		= 0x80000000,
};
enum {
Ntd		= 8,
Nrd		= 32,
Mps		= ROUNDUP(ETHERMAXTU+4, 128),
};
typedef struct Dtcc Dtcc;
struct Dtcc {
u64int	txok;
u64int	rxok;
u64int	txer;
u32int	rxer;
u16int	misspkt;
u16int	fae;
u32int	tx1col;
u32int	txmcol;
u64int	rxokph;
u64int	rxokbrd;
u32int	rxokmu;
u16int	txabt;
u16int	txundrn;
};
enum {
Rtl8100e	= (0x8136<<16)|0x10EC,
Rtl8169c		= (0x0116<<16)|0x16EC,
Rtl8169sc	= (0x8167<<16)|0x10EC,
Rtl8168b	= (0x8168<<16)|0x10EC,
Rtl8169		= (0x8169<<16)|0x10EC,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
int	active;
void*	nic;
QLock	alock;
Lock	ilock;
int	init;
int	pciv;
int	macv;
int	phyv;
Mii*	mii;
Lock	tlock;
D*	td;
Block**	tb;
int	ntd;
int	tdh;
int	tdt;
int	ntdfree;
int	ntq;
int	mtps;
Lock	rlock;
D*	rd;
void**	rb;
int	nrd;
int	rdh;
int	rdt;
int	nrdfree;
int	rcr;
QLock	slock;
Dtcc*	dtcc;
uint	txdu;
uint	tcpf;
uint	udpf;
uint	ipf;
uint	fovf;
uint	ierrs;
uint	rer;
uint	rdu;
uint	punlc;
uint	fovw;
} Ctlr;
static Ctlr* rtl8169ctlrhead;
static Ctlr* rtl8169ctlrtail;
#define csr8r(c, r)	(inb((c)->port+(r)))
#define csr16r(c, r)	(ins((c)->port+(r)))
#define csr32r(c, r)	(inl((c)->port+(r)))
#define csr8w(c, r, b)	(outb((c)->port+(r), (int)(b)))
#define csr16w(c, r, w)	(outs((c)->port+(r), (ushort)(w)))
#define csr32w(c, r, l)	(outl((c)->port+(r), (ulong)(l)))
static int
rtl8169miimir(Mii* mii, int pa, int ra)
{
uint r;
int timeo;
Ctlr *ctlr;
if(pa != 1)
return -1;
ctlr = mii->ctlr;
r = (ra<<16) & RegaddrMASK;
csr32w(ctlr, Phyar, r);
delay(1);
for(timeo = 0; timeo < 2000; timeo++){
if((r = csr32r(ctlr, Phyar)) & Flag)
break;
microdelay(100);
}
if(!(r & Flag))
return -1;
return (r & DataMASK)>>DataSHIFT;
}
static int
rtl8169miimiw(Mii* mii, int pa, int ra, int data)
{
uint r;
int timeo;
Ctlr *ctlr;
if(pa != 1)
return -1;
ctlr = mii->ctlr;
r = Flag|((ra<<16) & RegaddrMASK)|((data<<DataSHIFT) & DataMASK);
csr32w(ctlr, Phyar, r);
delay(1);
for(timeo = 0; timeo < 2000; timeo++){
if(!((r = csr32r(ctlr, Phyar)) & Flag))
break;
microdelay(100);
}
if(r & Flag)
return -1;
return 0;
}
static int
rtl8169mii(Ctlr* ctlr)
{
MiiPhy *phy;
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
return -1;
ctlr->mii->mir = rtl8169miimir;
ctlr->mii->miw = rtl8169miimiw;
ctlr->mii->ctlr = ctlr;
ctlr->phyv = rtl8169miimir(ctlr->mii, 1, Phyidr2) & 0x0F;
if(ctlr->macv == Macv02){
csr8w(ctlr, 0x82, 1);
rtl8169miimiw(ctlr->mii, 1, 0x0B, 0x0000);
}
if(mii(ctlr->mii, (1<<1)) == 0 || (phy = ctlr->mii->curphy) == nil){
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
print("oui %#ux phyno %d, macv = %#8.8ux phyv = %#4.4ux\n",
phy->oui, phy->phyno, ctlr->macv, ctlr->phyv);
miiane(ctlr->mii, ~0, ~0, ~0);
return 0;
}
static void
rtl8169halt(Ctlr* ctlr)
{
csr8w(ctlr, Cr, 0);
csr16w(ctlr, Imr, 0);
csr16w(ctlr, Isr, ~0);
}
static int
rtl8169reset(Ctlr* ctlr)
{
u32int r;
int timeo;
csr8w(ctlr, Cr, Rst);
for(r = timeo = 0; timeo < 1000; timeo++){
r = csr8r(ctlr, Cr);
if(!(r & Rst))
break;
delay(1);
}
rtl8169halt(ctlr);
if(r & Rst)
return -1;
return 0;
}
static void
rtl8169detach(Ether* edev)
{
rtl8169reset(edev->ctlr);
}
static void
rtl8169replenish(Ctlr* ctlr)
{
D *d;
int rdt;
void *bp;
rdt = ctlr->rdt;
while(NEXT(rdt, ctlr->nrd) != ctlr->rdh){
d = &ctlr->rd[rdt];
if(ctlr->rb[rdt] == nil){
bp = mallocalign(Mps, 8, 0, 0);
ctlr->rb[rdt] = bp;
d->addrlo = PCIWADDR(bp);
d->addrhi = 0;
}
coherence();
d->control |= Own|Mps;
rdt = NEXT(rdt, ctlr->nrd);
ctlr->nrdfree++;
}
ctlr->rdt = rdt;
}
static int
rtl8169init(Ether* edev)
{
u32int r;
Ctlr *ctlr;
u8int cplusc;
ctlr = edev->ctlr;
ilock(&ctlr->ilock);
rtl8169halt(ctlr);
csr8w(ctlr, Cr9346, Eem1|Eem0);
r = (edev->ea[3]<<24)|(edev->ea[2]<<16)|(edev->ea[1]<<8)|edev->ea[0];
csr32w(ctlr, Idr0, r);
r = (edev->ea[5]<<8)|edev->ea[4];
csr32w(ctlr, Idr0+4, r);
memset(ctlr->td, 0, sizeof(D)*ctlr->ntd);
ctlr->tdh = ctlr->tdt = 0;
ctlr->td[ctlr->ntd-1].control = Eor;
memset(ctlr->rd, 0, sizeof(D)*ctlr->nrd);
ctlr->rdh = ctlr->rdt = 0;
ctlr->rd[ctlr->nrd-1].control = Eor;
rtl8169replenish(ctlr);
ctlr->rcr = Rxfthnone|Mrxdmaunlimited|Ab|Apm;
ctlr->mtps = HOWMANY(Mps, 128);
cplusc = csr16r(ctlr, Cplusc) & ~(1<<14);
cplusc |= Rxchksum|Mulrw;
switch(ctlr->macv){
default:
return -1;
case Macv01:
ctlr->mtps = HOWMANY(Mps, 32);
break;
case Macv02:
case Macv03:
cplusc |= (1<<14);
break;
case Macv05:
r = csr8r(ctlr, Config2) & 0x07;
if(r == 0x01)
csr32w(ctlr, 0x7C, 0x0007FFFF);
else
csr32w(ctlr, 0x7C, 0x0007FF00);
pciclrmwi(ctlr->pcidev);
break;
case Macv13:
pcicfgw8(ctlr->pcidev, 0x68, 0x00);
pcicfgw8(ctlr->pcidev, 0x69, 0x08);
break;
case Macv04:
case Macv11:
case Macv12:
case Macv14:
case Macv15:
break;
}
switch(ctlr->pciv){
default:
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
case Rtl8169sc:
case Rtl8168b:
break;
}
csr32w(ctlr, Timerint, 0);
csr16w(ctlr, Imr, Serr|Timeout|Fovw|Punlc|Rdu|Ter|Rer|Rok);
csr32w(ctlr, Mpc, 0);
csr8w(ctlr, Mtps, ctlr->mtps);
csr32w(ctlr, Tnpds+4, 0);
csr32w(ctlr, Tnpds, PCIWADDR(ctlr->td));
csr32w(ctlr, Rdsar+4, 0);
csr32w(ctlr, Rdsar, PCIWADDR(ctlr->rd));
csr16w(ctlr, Rms, Mps);
r = csr16r(ctlr, Mulint) & 0xF000;
csr16w(ctlr, Mulint, r);
csr16w(ctlr, Cplusc, cplusc);
switch(ctlr->pciv){
default:
break;
case Rtl8169sc:
csr16w(ctlr, 0xE2, 0);
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
break;
case Rtl8168b:
case Rtl8169c:
csr16w(ctlr, 0xE2, 0);
csr16w(ctlr, Cplusc, 0x2000);
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
csr16w(ctlr, Rms, 0x0800);
csr8w(ctlr, Mtps, 0x3F);
break;
}
csr8w(ctlr, Cr9346, 0);
iunlock(&ctlr->ilock);
return 0;
}
static void
rtl8169attach(Ether* edev)
{
int timeo;
Ctlr *ctlr;
ctlr = edev->ctlr;
qlock(&ctlr->alock);
if(ctlr->init == 0){
ctlr->td = xspanalloc(sizeof(D)*Ntd, 256, 0);
ctlr->tb = malloc(Ntd*sizeof(Block*));
ctlr->ntd = Ntd;
ctlr->rd = xspanalloc(sizeof(D)*Nrd, 256, 0);
ctlr->rb = malloc(Nrd*sizeof(Block*));
ctlr->nrd = Nrd;
ctlr->dtcc = xspanalloc(sizeof(Dtcc), 64, 0);
rtl8169init(edev);
ctlr->init = 1;
}
qunlock(&ctlr->alock);
for(timeo = 0; timeo < 3500; timeo++){
if(miistatus(ctlr->mii) == 0)
break;
delay(10);
}
}
static void
rtl8169transmit(Ether* edev)
{
D *d;
Block *bp;
Ctlr *ctlr;
int control, x;
RingBuf *tb;
ctlr = edev->ctlr;
ilock(&ctlr->tlock);
for(x = ctlr->tdh; ctlr->ntq > 0; x = NEXT(x, ctlr->ntd)){
d = &ctlr->td[x];
if((control = d->control) & Own)
break;
USED(control);
freeb(ctlr->tb[x]);
ctlr->tb[x] = nil;
d->control &= Eor;
ctlr->ntq--;
}
ctlr->tdh = x;
x = ctlr->tdt;
while(ctlr->ntq < (ctlr->ntd-1)){
tb = &edev->tb[edev->ti];
if(tb->owner != Interface)
break;
bp = allocb(tb->len);
memmove(bp->wp, tb->pkt, tb->len);
memmove(bp->wp+Eaddrlen, edev->ea, Eaddrlen);
bp->wp += tb->len;
tb->owner = Host;
edev->ti = NEXT(edev->ti, edev->ntb);
d = &ctlr->td[x];
d->addrlo = PCIWADDR(bp->rp);
d->addrhi = 0;
ctlr->tb[x] = bp;
coherence();
d->control |= Own|Fs|Ls|((BLEN(bp)<<TxflSHIFT) & TxflMASK);
x = NEXT(x, ctlr->ntd);
ctlr->ntq++;
}
if(x != ctlr->tdt){
ctlr->tdt = x;
csr8w(ctlr, Tppoll, Npq);
}
else if(ctlr->ntq >= (ctlr->ntd-1))
ctlr->txdu++;
iunlock(&ctlr->tlock);
}
static void
rtl8169receive(Ether* edev)
{
D *d;
int len, rdh;
Ctlr *ctlr;
u32int control;
RingBuf *ring;
ctlr = edev->ctlr;
rdh = ctlr->rdh;
for(;;){
d = &ctlr->rd[rdh];
if(d->control & Own)
break;
control = d->control;
if((control & (Fs|Ls|Res)) == (Fs|Ls)){
len = ((control & RxflMASK)>>RxflSHIFT) - 4;
ring = &edev->rb[edev->ri];
if(ring->owner == Interface){
ring->owner = Host;
ring->len = len;
memmove(ring->pkt, ctlr->rb[rdh], len);
edev->ri = NEXT(edev->ri, edev->nrb);
}
}
else{
}
d->control &= Eor;
ctlr->nrdfree--;
rdh = NEXT(rdh, ctlr->nrd);
}
ctlr->rdh = rdh;
if(ctlr->nrdfree < ctlr->nrd/2)
rtl8169replenish(ctlr);
}
static void
rtl8169interrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Ether *edev;
u32int isr;
edev = arg;
ctlr = edev->ctlr;
while((isr = csr16r(ctlr, Isr)) != 0 && isr != 0xFFFF){
csr16w(ctlr, Isr, isr);
if(isr & (Fovw|Punlc|Rdu|Rer|Rok)){
rtl8169receive(edev);
if(!(isr & (Punlc|Rok)))
ctlr->ierrs++;
if(isr & Rer)
ctlr->rer++;
if(isr & Rdu)
ctlr->rdu++;
if(isr & Punlc)
ctlr->punlc++;
if(isr & Fovw)
ctlr->fovw++;
isr &= ~(Fovw|Rdu|Rer|Rok);
}
if(isr & (Tdu|Ter|Tok)){
rtl8169transmit(edev);
isr &= ~(Tdu|Ter|Tok);
}
if(isr & Punlc){
isr &= ~Punlc;
}
if(isr & (Serr|Timeout|Tdu|Fovw|Punlc|Rdu|Ter|Tok|Rer|Rok))
panic("rtl8169interrupt: imr %#4.4ux isr %#4.4ux\n",
csr16r(ctlr, Imr), isr);
}
}
static void
rtl8169pci(void)
{
Pcidev *p;
Ctlr *ctlr;
int i, port;
u32int bar;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch(i = ((p->did<<16)|p->vid)){
default:
continue;
case Rtl8100e:
case Rtl8169c:
case Rtl8169sc:
case Rtl8168b:
case Rtl8169:
break;
case (0xC107<<16)|0x1259:
i = Rtl8169;
break;
}
bar = p->mem[0].bar;
port = bar & ~0x01;
if(ioalloc(port, p->mem[0].size, 0, "rtl8169") < 0){
print("rtl8169: port %#ux in use\n", port);
continue;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = port;
ctlr->pcidev = p;
ctlr->pciv = i;
if(pcigetpms(p) > 0){
pcisetpms(p, 0);
for(i = 0; i < 6; i++)
pcicfgw32(p, PciBAR0+i*4, p->mem[i].bar);
pcicfgw8(p, PciINTL, p->intl);
pcicfgw8(p, PciLTR, p->ltr);
pcicfgw8(p, PciCLS, p->cls);
pcicfgw16(p, PciPCR, p->pcr);
}
if(rtl8169reset(ctlr)){
iofree(port);
free(ctlr);
continue;
}
ctlr->macv = csr32r(ctlr, Tcr) & HwveridMASK;
rtl8169mii(ctlr);
pcisetbme(p);
if(rtl8169ctlrhead != nil)
rtl8169ctlrtail->next = ctlr;
else
rtl8169ctlrhead = ctlr;
rtl8169ctlrtail = ctlr;
}
}
int
rtl8169pnp(Ether* edev)
{
u32int r;
Ctlr *ctlr;
if(rtl8169ctlrhead == nil)
rtl8169pci();
for(ctlr = rtl8169ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
if(edev->port == 0 || edev->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
edev->ctlr = ctlr;
edev->port = ctlr->port;
edev->irq = ctlr->pcidev->intl;
edev->tbdf = ctlr->pcidev->tbdf;
r = csr32r(ctlr, Idr0);
edev->ea[0] = r;
edev->ea[1] = r>>8;
edev->ea[2] = r>>16;
edev->ea[3] = r>>24;
r = csr32r(ctlr, Idr0+4);
edev->ea[4] = r;
edev->ea[5] = r>>8;
edev->attach = rtl8169attach;
edev->transmit = rtl8169transmit;
edev->interrupt = rtl8169interrupt;
edev->detach = rtl8169detach;
return 0;
}