#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#include "ethermii.h"
enum {
Idr0 = 0x00,
Mar0 = 0x08,
Dtccr = 0x10,
Tnpds = 0x20,
Thpds = 0x28,
Flash = 0x30,
Erbcr = 0x34,
Ersr = 0x36,
Cr = 0x37,
Tppoll = 0x38,
Imr = 0x3C,
Isr = 0x3E,
Tcr = 0x40,
Rcr = 0x44,
Tctr = 0x48,
Mpc = 0x4C,
Cr9346 = 0x50,
Config0 = 0x51,
Config1 = 0x52,
Config2 = 0x53,
Config3 = 0x54,
Config4 = 0x55,
Config5 = 0x56,
Timerint = 0x58,
Mulint = 0x5C,
Phyar = 0x60,
Tbicsr0 = 0x64,
Tbianar = 0x68,
Tbilpar = 0x6A,
Phystatus = 0x6C,
Rms = 0xDA,
Cplusc = 0xE0,
Coal = 0xE2,
Rdsar = 0xE4,
Etx = 0xEC,
};
enum {
Cmd = 0x00000008,
};
enum {
Te = 0x04,
Re = 0x08,
Rst = 0x10,
};
enum {
Fswint = 0x01,
Npq = 0x40,
Hpq = 0x80,
};
enum {
Rok = 0x0001,
Rer = 0x0002,
Tok = 0x0004,
Ter = 0x0008,
Rdu = 0x0010,
Punlc = 0x0020,
Fovw = 0x0040,
Tdu = 0x0080,
Swint = 0x0100,
Timeout = 0x4000,
Serr = 0x8000,
};
enum {
MtxdmaSHIFT = 8,
MtxdmaMASK = 0x00000700,
Mtxdmaunlimited = 0x00000700,
Acrc = 0x00010000,
Lbk0 = 0x00020000,
Lbk1 = 0x00040000,
Ifg2 = 0x00080000,
HwveridSHIFT = 23,
HwveridMASK = 0x7C800000,
Macv01 = 0x00000000,
Macv02 = 0x00800000,
Macv03 = 0x04000000,
Macv04 = 0x10000000,
Macv05 = 0x18000000,
Macv07 = 0x24800000,
Macv07a = 0x34800000,
Macv11 = 0x30000000,
Macv12 = 0x38000000,
Macv12a = 0x3c000000,
Macv13 = 0x34000000,
Macv14 = 0x30800000,
Macv15 = 0x38800000,
Macv25 = 0x28000000,
Macv2c = 0x2c000000,
Ifg0 = 0x01000000,
Ifg1 = 0x02000000,
};
enum {
Aap = 0x00000001,
Apm = 0x00000002,
Am = 0x00000004,
Ab = 0x00000008,
Ar = 0x00000010,
Aer = 0x00000020,
Sel9356 = 0x00000040,
MrxdmaSHIFT = 8,
MrxdmaMASK = 0x00000700,
Mrxdmaunlimited = 0x00000700,
RxfthSHIFT = 13,
RxfthMASK = 0x0000E000,
Rxfth256 = 0x00008000,
Rxfthnone = 0x0000E000,
Rer8 = 0x00010000,
MulERINT = 0x01000000,
};
enum {
Eedo = 0x01,
Eedi = 0x02,
Eesk = 0x04,
Eecs = 0x08,
Eem0 = 0x40,
Eem1 = 0x80,
};
enum {
DataMASK = 0x0000FFFF,
DataSHIFT = 0,
RegaddrMASK = 0x001F0000,
RegaddrSHIFT = 16,
Flag = 0x80000000,
};
enum {
Fd = 0x01,
Linksts = 0x02,
Speed10 = 0x04,
Speed100 = 0x08,
Speed1000 = 0x10,
Rxflow = 0x20,
Txflow = 0x40,
Entbi = 0x80,
};
enum {
Mulrw = 0x0008,
Dac = 0x0010,
Rxchksum = 0x0020,
Rxvlan = 0x0040,
Endian = 0x0200,
};
typedef struct D D;
struct D {
u32int control;
u32int vlan;
u32int addrlo;
u32int addrhi;
};
enum {
TxflMASK = 0x0000FFFF,
TxflSHIFT = 0,
Tcps = 0x00010000,
Udpcs = 0x00020000,
Ipcs = 0x00040000,
Lgsen = 0x08000000,
};
enum {
RxflMASK = 0x00001FFF,
Tcpf = 0x00004000,
Udpf = 0x00008000,
Ipf = 0x00010000,
Pid0 = 0x00020000,
Pid1 = 0x00040000,
Crce = 0x00080000,
Runt = 0x00100000,
Res = 0x00200000,
Rwt = 0x00400000,
Fovf = 0x00800000,
Bovf = 0x01000000,
Bar = 0x02000000,
Pam = 0x04000000,
Mar = 0x08000000,
};
enum {
Ls = 0x10000000,
Fs = 0x20000000,
Eor = 0x40000000,
Own = 0x80000000,
};
enum {
Ntd = 64,
Nrd = 1024,
Mtu = ETHERMAXTU,
Mps = ROUNDUP(ETHERMAXTU+4, 128),
};
typedef struct Dtcc Dtcc;
struct Dtcc {
u64int txok;
u64int rxok;
u64int txer;
u32int rxer;
u16int misspkt;
u16int fae;
u32int tx1col;
u32int txmcol;
u64int rxokph;
u64int rxokbrd;
u32int rxokmu;
u16int txabt;
u16int txundrn;
};
enum {
Rtl8100e = (0x8136<<16)|0x10EC,
Rtl8169c = (0x0116<<16)|0x16EC,
Rtl8169sc = (0x8167<<16)|0x10EC,
Rtl8168b = (0x8168<<16)|0x10EC,
Rtl8169 = (0x8169<<16)|0x10EC,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int port;
Pcidev* pcidev;
Ctlr* next;
int active;
QLock alock;
Lock ilock;
int init;
int pciv;
int macv;
int phyv;
int pcie;
uvlong mchash;
Mii* mii;
Lock tlock;
D* td;
Block** tb;
int ntd;
int tdh;
int tdt;
int ntdfree;
int ntq;
Lock rlock;
D* rd;
Block** rb;
int nrd;
int rdh;
int rdt;
int nrdfree;
int tcr;
int rcr;
int imr;
QLock slock;
Dtcc* dtcc;
uint txdu;
uint tcpf;
uint udpf;
uint ipf;
uint fovf;
uint ierrs;
uint rer;
uint rdu;
uint punlc;
uint fovw;
uint mcast;
uint frag;
} Ctlr;
static Ctlr* rtl8169ctlrhead;
static Ctlr* rtl8169ctlrtail;
#define csr8r(c, r) (inb((c)->port+(r)))
#define csr16r(c, r) (ins((c)->port+(r)))
#define csr32r(c, r) (inl((c)->port+(r)))
#define csr8w(c, r, b) (outb((c)->port+(r), (u8int)(b)))
#define csr16w(c, r, w) (outs((c)->port+(r), (u16int)(w)))
#define csr32w(c, r, l) (outl((c)->port+(r), (u32int)(l)))
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
rtl8169promiscuous(void* arg, int on)
{
Ether *edev;
Ctlr * ctlr;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->ilock);
if(on)
ctlr->rcr |= Aap;
else
ctlr->rcr &= ~Aap;
csr32w(ctlr, Rcr, ctlr->rcr);
iunlock(&ctlr->ilock);
}
enum {
Etherpolybe = 0x04c11db6,
Bytemask = (1<<8) - 1,
};
static ulong
ethercrcbe(uchar *addr, long len)
{
int i, j;
ulong c, crc, carry;
crc = ~0UL;
for (i = 0; i < len; i++) {
c = addr[i];
for (j = 0; j < 8; j++) {
carry = ((crc & (1UL << 31))? 1: 0) ^ (c & 1);
crc <<= 1;
c >>= 1;
if (carry)
crc = (crc ^ Etherpolybe) | carry;
}
}
return crc;
}
static ulong
swabl(ulong l)
{
return l>>24 | (l>>8) & (Bytemask<<8) |
(l<<8) & (Bytemask<<16) | l<<24;
}
static void
rtl8169multicast(void* ether, uchar *eaddr, int add)
{
Ether *edev;
Ctlr *ctlr;
if (!add)
return;
edev = ether;
ctlr = edev->ctlr;
ilock(&ctlr->ilock);
ctlr->mchash |= 1ULL << (ethercrcbe(eaddr, Eaddrlen) >> 26);
ctlr->rcr |= Am;
csr32w(ctlr, Rcr, ctlr->rcr);
if (ctlr->pcie) {
csr32w(ctlr, Mar0, swabl(ctlr->mchash>>32));
csr32w(ctlr, Mar0+4, swabl(ctlr->mchash));
} else {
csr32w(ctlr, Mar0, ctlr->mchash);
csr32w(ctlr, Mar0+4, ctlr->mchash>>32);
}
iunlock(&ctlr->ilock);
}
static long
rtl8169ifstat(Ether* edev, void* a, long n, ulong offset)
{
char *p;
Ctlr *ctlr;
Dtcc *dtcc;
int i, l, r, timeo;
ctlr = edev->ctlr;
qlock(&ctlr->slock);
p = nil;
if(waserror()){
qunlock(&ctlr->slock);
free(p);
nexterror();
}
csr32w(ctlr, Dtccr+4, 0);
csr32w(ctlr, Dtccr, PCIWADDR(ctlr->dtcc)|Cmd);
for(timeo = 0; timeo < 1000; timeo++){
if(!(csr32r(ctlr, Dtccr) & Cmd))
break;
delay(1);
}
if(csr32r(ctlr, Dtccr) & Cmd)
error(Eio);
dtcc = ctlr->dtcc;
edev->oerrs = dtcc->txer;
edev->crcs = dtcc->rxer;
edev->frames = dtcc->fae;
edev->buffs = dtcc->misspkt;
edev->overflows = ctlr->txdu+ctlr->rdu;
if(n == 0){
qunlock(&ctlr->slock);
poperror();
return 0;
}
if((p = malloc(READSTR)) == nil)
error(Enomem);
l = snprint(p, READSTR, "TxOk: %llud\n", dtcc->txok);
l += snprint(p+l, READSTR-l, "RxOk: %llud\n", dtcc->rxok);
l += snprint(p+l, READSTR-l, "TxEr: %llud\n", dtcc->txer);
l += snprint(p+l, READSTR-l, "RxEr: %ud\n", dtcc->rxer);
l += snprint(p+l, READSTR-l, "MissPkt: %ud\n", dtcc->misspkt);
l += snprint(p+l, READSTR-l, "FAE: %ud\n", dtcc->fae);
l += snprint(p+l, READSTR-l, "Tx1Col: %ud\n", dtcc->tx1col);
l += snprint(p+l, READSTR-l, "TxMCol: %ud\n", dtcc->txmcol);
l += snprint(p+l, READSTR-l, "RxOkPh: %llud\n", dtcc->rxokph);
l += snprint(p+l, READSTR-l, "RxOkBrd: %llud\n", dtcc->rxokbrd);
l += snprint(p+l, READSTR-l, "RxOkMu: %ud\n", dtcc->rxokmu);
l += snprint(p+l, READSTR-l, "TxAbt: %ud\n", dtcc->txabt);
l += snprint(p+l, READSTR-l, "TxUndrn: %ud\n", dtcc->txundrn);
l += snprint(p+l, READSTR-l, "txdu: %ud\n", ctlr->txdu);
l += snprint(p+l, READSTR-l, "tcpf: %ud\n", ctlr->tcpf);
l += snprint(p+l, READSTR-l, "udpf: %ud\n", ctlr->udpf);
l += snprint(p+l, READSTR-l, "ipf: %ud\n", ctlr->ipf);
l += snprint(p+l, READSTR-l, "fovf: %ud\n", ctlr->fovf);
l += snprint(p+l, READSTR-l, "ierrs: %ud\n", ctlr->ierrs);
l += snprint(p+l, READSTR-l, "rer: %ud\n", ctlr->rer);
l += snprint(p+l, READSTR-l, "rdu: %ud\n", ctlr->rdu);
l += snprint(p+l, READSTR-l, "punlc: %ud\n", ctlr->punlc);
l += snprint(p+l, READSTR-l, "fovw: %ud\n", ctlr->fovw);
l += snprint(p+l, READSTR-l, "tcr: %#8.8ux\n", ctlr->tcr);
l += snprint(p+l, READSTR-l, "rcr: %#8.8ux\n", ctlr->rcr);
l += snprint(p+l, READSTR-l, "multicast: %ud\n", ctlr->mcast);
if(ctlr->mii != nil && ctlr->mii->curphy != nil){
l += snprint(p+l, READSTR, "phy:   ");
for(i = 0; i < NMiiPhyr; i++){
if(i && ((i & 0x07) == 0))
l += snprint(p+l, READSTR-l, "\n       ");
r = miimir(ctlr->mii, i);
l += snprint(p+l, READSTR-l, " %4.4ux", r);
}
snprint(p+l, READSTR-l, "\n");
}
n = readstr(offset, a, n, p);
qunlock(&ctlr->slock);
poperror();
free(p);
return n;
}
static void
rtl8169halt(Ctlr* ctlr)
{
csr32w(ctlr, Timerint, 0);
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
rtl8169shutdown(Ether *ether)
{
rtl8169reset(ether->ctlr);
}
static void
rtl8169replenish(Ctlr* ctlr)
{
D *d;
int rdt;
Block *bp;
rdt = ctlr->rdt;
while(NEXT(rdt, ctlr->nrd) != ctlr->rdh){
d = &ctlr->rd[rdt];
if(ctlr->rb[rdt] == nil){
bp = iallocb(Mps);
if(bp == nil){
iprint("no available buffers\n");
break;
}
ctlr->rb[rdt] = bp;
d->addrlo = PCIWADDR(bp->rp);
d->addrhi = 0;
coherence();
}else
iprint("i8169: rx overrun\n");
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
rtl8169reset(ctlr);
csr8w(ctlr, Cr9346, Eem1|Eem0);
memset(ctlr->td, 0, sizeof(D)*ctlr->ntd);
ctlr->tdh = ctlr->tdt = 0;
ctlr->td[ctlr->ntd-1].control = Eor;
memset(ctlr->rd, 0, sizeof(D)*ctlr->nrd);
ctlr->nrdfree = ctlr->rdh = ctlr->rdt = 0;
ctlr->rd[ctlr->nrd-1].control = Eor;
rtl8169replenish(ctlr);
ctlr->rcr = Rxfthnone|Mrxdmaunlimited|Ab|Am|Apm;
cplusc = csr16r(ctlr, Cplusc) & ~(1<<14);
cplusc |= Mulrw;
switch(ctlr->macv){
default:
panic("ether8169: unknown macv %#08ux for vid %#ux did %#ux",
ctlr->macv, ctlr->pcidev->vid, ctlr->pcidev->did);
case Macv01:
break;
case Macv02:
case Macv03:
cplusc |= 1<<14;
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
case Macv07:
case Macv07a:
case Macv11:
case Macv12:
case Macv12a:
case Macv14:
case Macv15:
case Macv25:
case Macv2c:
break;
}
switch(ctlr->pciv){
default:
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
csr32w(ctlr, Mar0, 0);
csr32w(ctlr, Mar0+4, 0);
ctlr->mchash = 0;
case Rtl8169sc:
case Rtl8168b:
break;
}
csr32w(ctlr, Timerint, 0);
ctlr->imr = Serr|Timeout|Fovw|Punlc|Rdu|Ter|Rer|Rok;
csr16w(ctlr, Imr, ctlr->imr);
csr32w(ctlr, Mpc, 0);
csr8w(ctlr, Etx, 0x3f);
csr32w(ctlr, Tnpds+4, 0);
csr32w(ctlr, Tnpds, PCIWADDR(ctlr->td));
csr32w(ctlr, Rdsar+4, 0);
csr32w(ctlr, Rdsar, PCIWADDR(ctlr->rd));
csr16w(ctlr, Rms, 16383);
r = csr16r(ctlr, Mulint) & 0xF000;
csr16w(ctlr, Mulint, r);
csr16w(ctlr, Cplusc, cplusc);
csr16w(ctlr, Coal, 0);
switch(ctlr->pciv){
case Rtl8169sc:
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
break;
case Rtl8168b:
case Rtl8169c:
csr16w(ctlr, Cplusc, 0x2000);
csr8w(ctlr, Cr, Te|Re);
csr32w(ctlr, Tcr, Ifg1|Ifg0|Mtxdmaunlimited);
csr32w(ctlr, Rcr, ctlr->rcr);
break;
}
ctlr->tcr = csr32r(ctlr, Tcr);
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
ctlr->td = mallocalign(sizeof(D)*Ntd, 256, 0, 0);
ctlr->tb = malloc(Ntd*sizeof(Block*));
ctlr->ntd = Ntd;
ctlr->rd = mallocalign(sizeof(D)*Nrd, 256, 0, 0);
ctlr->rb = malloc(Nrd*sizeof(Block*));
ctlr->nrd = Nrd;
ctlr->dtcc = mallocalign(sizeof(Dtcc), 64, 0, 0);
if(ctlr->td == nil || ctlr->tb == nil || ctlr->rd == nil ||
ctlr->rb == nil || ctlr->dtcc == nil) {
free(ctlr->td);
free(ctlr->tb);
free(ctlr->rd);
free(ctlr->rb);
free(ctlr->dtcc);
qunlock(&ctlr->alock);
error(Enomem);
}
memset(ctlr->dtcc, 0, sizeof(Dtcc));
rtl8169init(edev);
ctlr->init = 1;
}
qunlock(&ctlr->alock);
for(timeo = 0; timeo < 10; timeo++){
if(miistatus(ctlr->mii) == 0)
break;
delay(100);
}
}
static void
rtl8169link(Ether* edev)
{
uint r;
int limit;
Ctlr *ctlr;
ctlr = edev->ctlr;
if(!((r = csr8r(ctlr, Phystatus)) & Linksts)){
edev->link = 0;
return;
}
edev->link = 1;
limit = 256*1024;
if(r & Speed10){
edev->mbps = 10;
limit = 65*1024;
} else if(r & Speed100)
edev->mbps = 100;
else if(r & Speed1000)
edev->mbps = 1000;
if(edev->oq != nil)
qsetlimit(edev->oq, limit);
}
static void
rtl8169transmit(Ether* edev)
{
D *d;
Block *bp;
Ctlr *ctlr;
int control, x;
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
if((bp = qget(edev->oq)) == nil)
break;
d = &ctlr->td[x];
d->addrlo = PCIWADDR(bp->rp);
d->addrhi = 0;
ctlr->tb[x] = bp;
coherence();
d->control |= Own | Fs | Ls | BLEN(bp);
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
int rdh;
Block *bp;
Ctlr *ctlr;
u32int control;
ctlr = edev->ctlr;
rdh = ctlr->rdh;
for(;;){
d = &ctlr->rd[rdh];
if(d->control & Own)
break;
control = d->control;
if((control & (Fs|Ls|Res)) == (Fs|Ls)){
bp = ctlr->rb[rdh];
bp->wp = bp->rp + (control & RxflMASK) - 4;
if(control & Fovf)
ctlr->fovf++;
if(control & Mar)
ctlr->mcast++;
switch(control & (Pid1|Pid0)){
default:
break;
case Pid0:
if(control & Tcpf){
ctlr->tcpf++;
break;
}
bp->flag |= Btcpck;
break;
case Pid1:
if(control & Udpf){
ctlr->udpf++;
break;
}
bp->flag |= Budpck;
break;
case Pid1|Pid0:
if(control & Ipf){
ctlr->ipf++;
break;
}
bp->flag |= Bipck;
break;
}
etheriq(edev, bp, 1);
}else{
if(!(control & Res))
ctlr->frag++;
freeb(ctlr->rb[rdh]);
}
ctlr->rb[rdh] = nil;
d->control &= Eor;
ctlr->nrdfree--;
rdh = NEXT(rdh, ctlr->nrd);
if(ctlr->nrdfree < ctlr->nrd/2)
rtl8169replenish(ctlr);
}
ctlr->rdh = rdh;
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
if((isr & ctlr->imr) == 0)
break;
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
rtl8169link(edev);
isr &= ~Punlc;
}
if(isr & (Serr|Timeout|Tdu|Fovw|Punlc|Rdu|Ter|Tok|Rer|Rok))
panic("rtl8169interrupt: imr %#4.4ux isr %#4.4ux",
csr16r(ctlr, Imr), isr);
}
}
int
vetmacv(Ctlr *ctlr, uint *macv)
{
*macv = csr32r(ctlr, Tcr) & HwveridMASK;
switch(*macv){
default:
return -1;
case Macv01:
case Macv02:
case Macv03:
case Macv04:
case Macv05:
case Macv07:
case Macv07a:
case Macv11:
case Macv12:
case Macv12a:
case Macv13:
case Macv14:
case Macv15:
case Macv25:
case Macv2c:
break;
}
return 0;
}
static void
rtl8169pci(void)
{
Pcidev *p;
Ctlr *ctlr;
int i, port, pcie;
uint macv;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
pcie = 0;
switch(i = ((p->did<<16)|p->vid)){
default:
continue;
case Rtl8100e:
case Rtl8168b:
pcie = 1;
break;
case Rtl8169c:
case Rtl8169sc:
case Rtl8169:
break;
case (0xC107<<16)|0x1259:
i = Rtl8169;
break;
}
port = p->mem[0].bar & ~0x01;
if(ioalloc(port, p->mem[0].size, 0, "rtl8169") < 0){
print("rtl8169: port %#ux in use\n", port);
continue;
}
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil)
error(Enomem);
ctlr->port = port;
ctlr->pcidev = p;
ctlr->pciv = i;
ctlr->pcie = pcie;
if(vetmacv(ctlr, &macv) == -1){
iofree(port);
free(ctlr);
print("rtl8169: unknown mac %.4ux %.8ux\n", p->did, macv);
continue;
}
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
ctlr->macv = macv;
rtl8169mii(ctlr);
pcisetbme(p);
if(rtl8169ctlrhead != nil)
rtl8169ctlrtail->next = ctlr;
else
rtl8169ctlrhead = ctlr;
rtl8169ctlrtail = ctlr;
}
}
static int
rtl8169pnp(Ether* edev)
{
u32int r;
Ctlr *ctlr;
uchar ea[Eaddrlen];
static int once;
if(once == 0){
once = 1;
rtl8169pci();
}
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
edev->mbps = 1000;
edev->maxmtu = Mtu;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0){
r = csr32r(ctlr, Idr0);
edev->ea[0] = r;
edev->ea[1] = r>>8;
edev->ea[2] = r>>16;
edev->ea[3] = r>>24;
r = csr32r(ctlr, Idr0+4);
edev->ea[4] = r;
edev->ea[5] = r>>8;
}
edev->attach = rtl8169attach;
edev->transmit = rtl8169transmit;
edev->interrupt = rtl8169interrupt;
edev->ifstat = rtl8169ifstat;
edev->arg = edev;
edev->promiscuous = rtl8169promiscuous;
edev->multicast = rtl8169multicast;
edev->shutdown = rtl8169shutdown;
rtl8169link(edev);
return 0;
}
void
ether8169link(void)
{
addethercard("rtl8169", rtl8169pnp);
}