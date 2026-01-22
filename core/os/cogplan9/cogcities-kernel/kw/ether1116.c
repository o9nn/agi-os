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
#include "../ip/ip.h"
#define MIIDBG if(0)iprint
#define WINATTR(v) (((v) & MASK(8)) << 8)
#define WINSIZE(v) (((v)/(64*1024) - 1) << 16)
enum {
Nrx = 512,
Ntx = 32,
Nrxblks = 1024,
Rxblklen = 2+1522,
Maxrxintrsec = 20*1000,
Etherstuck = 70,
Descralign = 16,
Bufalign = 8,
Pass = 1,
Qno = 0,
};
typedef struct Ctlr Ctlr;
typedef struct Gbereg Gbereg;
typedef struct Mibstats Mibstats;
typedef struct Rx Rx;
typedef struct Tx Tx;
static struct {
Lock;
Block *head;
} freeblocks;
struct Rx {
ulong cs;
ulong countsize;
ulong buf;
ulong next;
};
struct Tx {
ulong cs;
ulong countchk;
ulong buf;
ulong next;
};
struct Mibstats {
union {
uvlong rxby;
struct {
ulong rxbylo;
ulong rxbyhi;
};
};
ulong badrxby;
ulong mactxerr;
ulong rxpkt;
ulong badrxpkt;
ulong rxbcastpkt;
ulong rxmcastpkt;
ulong rx64;
ulong rx65_127;
ulong rx128_255;
ulong rx256_511;
ulong rx512_1023;
ulong rx1024_max;
union {
uvlong txby;
struct {
ulong txbylo;
ulong txbyhi;
};
};
ulong txpkt;
ulong txcollpktdrop;
ulong txmcastpkt;
ulong txbcastpkt;
ulong badmacctlpkts;
ulong txflctl;
ulong rxflctl;
ulong badrxflctl;
ulong rxundersized;
ulong rxfrags;
ulong rxtoobig;
ulong rxjabber;
ulong rxerr;
ulong crcerr;
ulong collisions;
ulong latecoll;
};
struct Ctlr {
Lock;
Ether *ether;
Gbereg *reg;
Lock initlock;
int init;
Rx *rx;
Block *rxb[Nrx];
int rxhead;
int rxtail;
Rendez rrendez;
int haveinput;
Tx *tx;
Block *txb[Ntx];
int txhead;
int txtail;
Mii *mii;
int port;
ulong intrs;
ulong newintrs;
ulong txunderrun;
ulong txringfull;
ulong rxdiscard;
ulong rxoverrun;
ulong nofirstlast;
Mibstats;
};
#define Rxqon(q) (1<<(q))
#define Txqon(q) (1<<(q))
enum {
Portreset = 1 << 20,
Burst1 = 0,
Burst2,
Burst4,
Burst8,
Burst16,
SDCrifb = 1<<0,
#define SDCrxburst(v) ((v)<<1)
SDCrxnobyteswap = 1<<4,
SDCtxnobyteswap = 1<<5,
SDCswap64byte = 1<<6,
#define SDCtxburst(v) ((v)<<22)
#define SDCipgintrx(v) ((((v)>>15) & 1)<<25) | (((v) & MASK(15))<<7)
PCFGupromisc = 1<<0,
#define Rxqdefault(q) ((q)<<1)
#define Rxqarp(q) ((q)<<4)
PCFGbcrejectnoiparp = 1<<7,
PCFGbcrejectip = 1<<8,
PCFGbcrejectarp = 1<<9,
PCFGamnotxes = 1<<12,
PCFGtcpq = 1<<14,
PCFGudpq = 1<<15,
#define Rxqtcp(q) ((q)<<16)
#define Rxqudp(q) ((q)<<19)
#define Rxqbpdu(q) ((q)<<22)
PCFGrxcs = 1<<25,
PCFGXspanq = 1<<1,
PCFGXcrcoff = 1<<2,
PSC0porton = 1<<0,
PSC0forcelinkup = 1<<1,
PSC0an_dplxoff = 1<<2,
PSC0an_flctloff = 1<<3,
PSC0an_pauseadv = 1<<4,
PSC0nofrclinkdown = 1<<10,
PSC0an_spdoff = 1<<13,
PSC0dteadv = 1<<14,
#define PSC0mru(v) ((v)<<17)
PSC0mrumask = PSC0mru(MASK(3)),
PSC0mru1518 = 0,
PSC0mru1522,
PSC0mru1552,
PSC0mru9022,
PSC0mru9192,
PSC0mru9700,
PSC0fd_frc = 1<<21,
PSC0flctlfrc = 1<<22,
PSC0gmiispd_gbfrc = 1<<23,
PSC0miispdfrc100mbps = 1<<24,
PS0linkup = 1<<1,
PS0fd = 1<<2,
PS0flctl = 1<<3,
PS0gmii_gb = 1<<4,
PS0mii100mbps = 1<<5,
PS0txbusy = 1<<7,
PS0txfifoempty = 1<<10,
PS0rxfifo1empty = 1<<11,
PS0rxfifo2empty = 1<<12,
PSC1loopback = 1<<1,
PSC1mii = 0<<2,
PSC1rgmii = 1<<3,
PSC1portreset = 1<<4,
PSC1clockbypass = 1<<5,
PSC1iban = 1<<6,
PSC1iban_bypass = 1<<7,
PSC1iban_restart= 1<<8,
PSC1_gbonly = 1<<11,
PSC1encolonbp = 1<<15,
PSC1coldomlimmask= MASK(6)<<16,
#define PSC1coldomlim(v) (((v) & MASK(6))<<16)
PSC1miiallowoddpreamble = 1<<22,
PS1rxpause = 1<<0,
PS1txpause = 1<<1,
PS1pressure = 1<<2,
PS1syncfail10ms = 1<<3,
PS1an_done = 1<<4,
PS1inbandan_bypassed = 1<<5,
PS1serdesplllocked = 1<<6,
PS1syncok = 1<<7,
PS1nosquelch = 1<<8,
Irx = 1<<0,
Iextend = 1<<1,
#define Irxbufferq(q) (1<<((q)+2))
Irxerr = 1<<10,
#define Irxerrq(q) (1<<((q)+11))
#define Itxendq(q) (1<<((q)+19))
Isum = 1<<31,
#define IEtxbufferq(q) (1<<((q)+0))
#define IEtxerrq(q) (1<<((q)+8))
IEphystschg = 1<<16,
IEptp = 1<<17,
IErxoverrun = 1<<18,
IEtxunderrun = 1<<19,
IElinkchg = 1<<20,
IEintaddrerr = 1<<23,
IEprbserr = 1<<25,
IEsum = 1<<31,
#define TFUTipginttx(v) (((v) & MASK(16))<<4);
MFS40by = 10<<2,
MFS44by = 11<<2,
MFS48by = 12<<2,
MFS52by = 13<<2,
MFS56by = 14<<2,
MFS60by = 15<<2,
MFS64by = 16<<2,
RCSmacerr = 1<<0,
RCSmacmask = 3<<1,
RCSmacce = 0<<1,
RCSmacor = 1<<1,
RCSmacmf = 2<<1,
RCSl4chkshift = 3,
RCSl4chkmask = MASK(16),
RCSvlan = 1<<17,
RCSbpdu = 1<<18,
RCSl4mask = 3<<21,
RCSl4tcp4 = 0<<21,
RCSl4udp4 = 1<<21,
RCSl4other = 2<<21,
RCSl4rsvd = 3<<21,
RCSl2ev2 = 1<<23,
RCSl3ip4 = 1<<24,
RCSip4headok = 1<<25,
RCSlast = 1<<26,
RCSfirst = 1<<27,
RCSunknownaddr = 1<<28,
RCSenableintr = 1<<29,
RCSl4chkok = 1<<30,
RCSdmaown = 1<<31,
TCSmacerr = 1<<0,
TCSmacmask = 3<<1,
TCSmaclc = 0<<1,
TCSmacur = 1<<1,
TCSmacrl = 2<<1,
TCSllc = 1<<9,
TCSl4chkmode = 1<<10,
TCSipv4hdlenshift= 11,
TCSvlan = 1<<15,
TCSl4type = 1<<16,
TCSgl4chk = 1<<17,
TCSgip4chk = 1<<18,
TCSpadding = 1<<19,
TCSlast = 1<<20,
TCSfirst = 1<<21,
TCSenableintr = 1<<23,
TCSautomode = 1<<30,
TCSdmaown = 1<<31,
};
enum {
PhysmiTimeout = 10000,
Physmidataoff = 0,
Physmidatamask = 0xffff<<Physmidataoff,
Physmiaddroff = 16,
Physmiaddrmask = 0x1f << Physmiaddroff,
Physmiop = 26,
Physmiopmask = 3<<Physmiop,
PhysmiopWr = 0<<Physmiop,
PhysmiopRd = 1<<Physmiop,
PhysmiReadok = 1<<27,
PhysmiBusy = 1<<28,
SmiRegaddroff = 21,
SmiRegaddrmask = 0x1f << SmiRegaddroff,
};
struct Gbereg {
ulong phy;
ulong smi;
ulong euda;
ulong eudid;
uchar _pad0[0x80-0x10];
ulong euirq;
ulong euirqmask;
uchar _pad1[0x94-0x88];
ulong euea;
ulong euiae;
uchar _pad2[0xb0-0x9c];
ulong euc;
uchar _pad3[0x200-0xb4];
struct {
ulong base;
ulong size;
} base[6];
uchar _pad4[0x280-0x230];
ulong harr[4];
ulong bare;
ulong epap;
uchar _pad5[0x400-0x298];
ulong portcfg;
ulong portcfgx;
ulong mii;
ulong _pad6;
ulong evlane;
ulong macal;
ulong macah;
ulong sdc;
ulong dscp[7];
ulong psc0;
ulong vpt2p;
ulong ps0;
ulong tqc;
ulong psc1;
ulong ps1;
ulong mvhdr;
ulong _pad8[2];
ulong irq;
ulong irqe;
ulong irqmask;
ulong irqemask;
ulong _pad9;
ulong pxtfut;
ulong _pad10;
ulong pxmfs;
ulong _pad11;
ulong pxdfc;
ulong pxofc;
ulong _pad12[2];
ulong piae;
uchar _pad13[0x4bc-0x498];
ulong etherprio;
uchar _pad14[0x4dc-0x4c0];
ulong tqfpc;
ulong pttbrc;
ulong tqc1;
ulong pmtu;
ulong pmtbs;
uchar _pad15[0x600-0x4f0];
struct {
ulong _pad[3];
ulong r;
} crdp[8];
ulong rqc;
ulong tcsdp;
uchar _pad16[0x6c0-0x688];
ulong tcqdp[8];
uchar _pad17[0x700-0x6e0];
struct {
ulong tbctr;
ulong tbcfg;
ulong acfg;
ulong _pad;
} tq[8];
ulong pttbc;
uchar _pad18[0x7a8-0x784];
ulong ipg2;
ulong _pad19[3];
ulong ipg3;
ulong _pad20;
ulong htlp;
ulong htap;
ulong ltap;
ulong _pad21;
ulong ts;
uchar _pad22[0x1000-0x7d4];
Mibstats;
uchar _pad23[0x1400-0x1080];
ulong dfsmt[64];
ulong dfomt[64];
ulong dfut[4];
};
static Ctlr *ctlrs[MaxEther];
static uchar zeroea[Eaddrlen];
static void getmibstats(Ctlr *);
static void
rxfreeb(Block *b)
{
_xinc(&b->ref);
b->wp = b->rp =
(uchar*)((uintptr)(b->lim - Rxblklen) & ~(Bufalign - 1));
assert(((uintptr)b->rp & (Bufalign - 1)) == 0);
b->free = rxfreeb;
ilock(&freeblocks);
b->next = freeblocks.head;
freeblocks.head = b;
iunlock(&freeblocks);
}
static Block *
rxallocb(void)
{
Block *b;
ilock(&freeblocks);
b = freeblocks.head;
if(b != nil) {
freeblocks.head = b->next;
b->next = nil;
b->free = rxfreeb;
}
iunlock(&freeblocks);
return b;
}
static void
rxkick(Ctlr *ctlr)
{
Gbereg *reg = ctlr->reg;
if (reg->crdp[Qno].r == 0)
reg->crdp[Qno].r = PADDR(&ctlr->rx[ctlr->rxhead]);
if ((reg->rqc & 0xff) == 0)
reg->rqc = Rxqon(Qno);
coherence();
}
static void
txkick(Ctlr *ctlr)
{
Gbereg *reg = ctlr->reg;
if (reg->tcqdp[Qno] == 0)
reg->tcqdp[Qno] = PADDR(&ctlr->tx[ctlr->txhead]);
if ((reg->tqc & 0xff) == 0)
reg->tqc = Txqon(Qno);
coherence();
}
static void
rxreplenish(Ctlr *ctlr)
{
Rx *r;
Block *b;
while(ctlr->rxb[ctlr->rxtail] == nil) {
b = rxallocb();
if(b == nil) {
iprint("#l%d: rxreplenish out of buffers\n",
ctlr->ether->ctlrno);
break;
}
ctlr->rxb[ctlr->rxtail] = b;
r = &ctlr->rx[ctlr->rxtail];
assert(((uintptr)r & (Descralign - 1)) == 0);
r->countsize = ROUNDUP(Rxblklen, 8);
r->buf = PADDR(b->rp);
coherence();
r->cs = RCSdmaown | RCSenableintr;
coherence();
ctlr->rxtail = NEXT(ctlr->rxtail, Nrx);
}
}
static void
dump(uchar *bp, long max)
{
if (max > 64)
max = 64;
for (; max > 0; max--, bp++)
iprint("%02.2ux ", *bp);
print("...\n");
}
static void
etheractive(Ether *ether)
{
ether->starttime = TK2MS(MACHP(0)->ticks)/1000;
}
static void
ethercheck(Ether *ether)
{
if (ether->starttime != 0 &&
TK2MS(MACHP(0)->ticks)/1000 - ether->starttime > Etherstuck) {
etheractive(ether);
if (ether->ctlrno == 0)
iprint("#l%d: ethernet stuck\n", ether->ctlrno);
}
}
static void
receive(Ether *ether)
{
int i;
ulong n;
Block *b;
Ctlr *ctlr = ether->ctlr;
Rx *r;
ethercheck(ether);
for (i = Nrx-2; i > 0; i--) {
r = &ctlr->rx[ctlr->rxhead];
assert(((uintptr)r & (Descralign - 1)) == 0);
if(r->cs & RCSdmaown)
break;
b = ctlr->rxb[ctlr->rxhead];
if (b == nil)
panic("ether1116: nil ctlr->rxb[ctlr->rxhead] "
"in receive");
ctlr->rxb[ctlr->rxhead] = nil;
ctlr->rxhead = NEXT(ctlr->rxhead, Nrx);
if((r->cs & (RCSfirst|RCSlast)) != (RCSfirst|RCSlast)) {
ctlr->nofirstlast++;
freeb(b);
continue;
}
if(r->cs & RCSmacerr) {
freeb(b);
continue;
}
n = r->countsize >> 16;
assert(n >= 2 && n < 2048);
l2cacheuinvse(b->rp, n+2);
cachedinvse(b->rp, n+2);
b->wp = b->rp + n;
b->rp += 2;
etheriq(ether, b, 1);
etheractive(ether);
if (i % (Nrx / 2) == 0) {
rxreplenish(ctlr);
rxkick(ctlr);
}
}
rxreplenish(ctlr);
rxkick(ctlr);
}
static void
txreplenish(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
while(ctlr->txtail != ctlr->txhead) {
if(ctlr->tx[ctlr->txtail].cs & TCSdmaown)
break;
if(ctlr->txb[ctlr->txtail] == nil)
panic("no block for sent packet?!");
freeb(ctlr->txb[ctlr->txtail]);
ctlr->txb[ctlr->txtail] = nil;
ctlr->txtail = NEXT(ctlr->txtail, Ntx);
etheractive(ether);
}
}
static void
transmit(Ether *ether)
{
int i, kick, len;
Block *b;
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
Tx *t;
ethercheck(ether);
ilock(ctlr);
txreplenish(ether);
kick = 0;
for (i = Ntx/2 - 2; i > 0; i--) {
t = &ctlr->tx[ctlr->txhead];
assert(((uintptr)t & (Descralign - 1)) == 0);
if(t->cs & TCSdmaown) {
ctlr->txringfull++;
break;
}
b = qget(ether->oq);
if (b == nil)
break;
len = BLEN(b);
if(len < ether->minmtu || len > ether->maxmtu) {
freeb(b);
continue;
}
ctlr->txb[ctlr->txhead] = b;
cachedwbse(b->rp, len);
l2cacheuwbse(b->rp, len);
t->buf = PADDR(b->rp);
t->countchk = len << 16;
coherence();
t->cs = TCSpadding | TCSfirst | TCSlast | TCSdmaown |
TCSenableintr;
coherence();
kick++;
ctlr->txhead = NEXT(ctlr->txhead, Ntx);
}
if (kick) {
txkick(ctlr);
reg->irqmask |= Itxendq(Qno);
reg->irqemask |= IEtxerrq(Qno) | IEtxunderrun;
}
iunlock(ctlr);
}
static void
dumprxdescs(Ctlr *ctlr)
{
int i;
Gbereg *reg = ctlr->reg;
iprint("\nrxhead %d rxtail %d; txcdp %#p rxcdp %#p\n",
ctlr->rxhead, ctlr->rxtail, reg->tcqdp[Qno], reg->crdp[Qno].r);
for (i = 0; i < Nrx; i++) {
iprint("rxb %d @ %#p: %#p\n", i, &ctlr->rxb[i], ctlr->rxb[i]);
delay(50);
}
for (i = 0; i < Nrx; i++) {
iprint("rx %d @ %#p: cs %#lux countsize %lud buf %#lux next %#lux\n",
i, &ctlr->rx[i], ctlr->rx[i].cs,
ctlr->rx[i].countsize >> 3, ctlr->rx[i].buf,
ctlr->rx[i].next);
delay(50);
}
delay(1000);
}
static int
gotinput(void* ctlr)
{
return ((Ctlr*)ctlr)->haveinput != 0;
}
static void
rcvproc(void* arg)
{
Ctlr *ctlr;
Ether *ether;
ether = arg;
ctlr = ether->ctlr;
for(;;){
tsleep(&ctlr->rrendez, gotinput, ctlr, 10*1000);
ilock(ctlr);
getmibstats(ctlr);
if (ctlr->haveinput) {
ctlr->haveinput = 0;
iunlock(ctlr);
receive(ether);
} else
iunlock(ctlr);
}
}
static void
interrupt(Ureg*, void *arg)
{
ulong irq, irqe, handled;
Ether *ether = arg;
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
handled = 0;
irq = reg->irq;
irqe = reg->irqe;
reg->irqe = 0;
reg->irq = 0;
ethercheck(ether);
if(irq & (Irx | Irxbufferq(Qno))) {
ctlr->haveinput = 1;
wakeup(&ctlr->rrendez);
irq &= ~(Irx | Irxbufferq(Qno));
handled++;
} else
rxkick(ctlr);
if(irq & Itxendq(Qno)) {
reg->irqmask &= ~Itxendq(Qno);
reg->irqemask &= ~(IEtxerrq(Qno) | IEtxunderrun);
transmit(ether);
irq &= ~Itxendq(Qno);
handled++;
}
if(irqe & IEsum) {
if(irqe & IEphystschg) {
ether->link = (reg->ps0 & PS0linkup) != 0;
ether->linkchg = 1;
}
if(irqe & IEtxerrq(Qno))
ether->oerrs++;
if(irqe & IErxoverrun)
ether->overflows++;
if(irqe & IEtxunderrun)
ctlr->txunderrun++;
if(irqe & (IEphystschg | IEtxerrq(Qno) | IErxoverrun |
IEtxunderrun))
handled++;
}
if (irq & Isum) {
if (irq & Irxerr) {
ether->buffs++;
ctlr->haveinput = 1;
wakeup(&ctlr->rrendez);
}
if(irq & (Irxerr | Irxerrq(Qno)))
handled++;
irq &= ~(Irxerr | Irxerrq(Qno));
}
if(ether->linkchg && (reg->ps1 & PS1an_done)) {
handled++;
ether->link = (reg->ps0 & PS0linkup) != 0;
ether->linkchg = 0;
}
ctlr->newintrs++;
if (!handled) {
irq &= ~Isum;
irqe &= ~IEtxbufferq(Qno);
if (irq == 0 && irqe == 0) {
} else
iprint("ether1116: interrupt cause unknown; "
"irq %#lux irqe %#lux\n", irq, irqe);
}
intrclear(Irqlo, ether->irq);
}
void
promiscuous(void *arg, int on)
{
Ether *ether = arg;
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
ilock(ctlr);
ether->prom = on;
if(on)
reg->portcfg |= PCFGupromisc;
else
reg->portcfg &= ~PCFGupromisc;
iunlock(ctlr);
}
void
multicast(void *, uchar *, int)
{
}
static void quiesce(Gbereg *reg);
static void
shutdown(Ether *ether)
{
int i;
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
ilock(ctlr);
quiesce(reg);
reg->euc |= Portreset;
coherence();
iunlock(ctlr);
delay(100);
ilock(ctlr);
reg->euc &= ~Portreset;
coherence();
delay(20);
reg->psc0 = 0;
reg->psc1 |= PSC1portreset;
coherence();
delay(50);
reg->psc1 &= ~PSC1portreset;
coherence();
for (i = 0; i < nelem(reg->tcqdp); i++)
reg->tcqdp[i] = 0;
for (i = 0; i < nelem(reg->crdp); i++)
reg->crdp[i].r = 0;
coherence();
iunlock(ctlr);
}
enum {
CMjumbo,
};
static Cmdtab ctlmsg[] = {
CMjumbo, "jumbo", 2,
};
long
ctl(Ether *e, void *p, long n)
{
Cmdbuf *cb;
Cmdtab *ct;
Ctlr *ctlr = e->ctlr;
Gbereg *reg = ctlr->reg;
cb = parsecmd(p, n);
if(waserror()) {
free(cb);
nexterror();
}
ct = lookupcmd(cb, ctlmsg, nelem(ctlmsg));
switch(ct->index) {
case CMjumbo:
if(strcmp(cb->f[1], "on") == 0) {
error("jumbo disabled");
reg->psc0 = (reg->psc0 & ~PSC0mrumask) |
PSC0mru(PSC0mru9022);
e->maxmtu = 9022;
} else if(strcmp(cb->f[1], "off") == 0) {
reg->psc0 = (reg->psc0 & ~PSC0mrumask) |
PSC0mru(PSC0mru1522);
e->maxmtu = ETHERMAXTU;
} else
error(Ebadctl);
break;
default:
error(Ebadctl);
break;
}
free(cb);
poperror();
return n;
}
static int
smibusywait(Gbereg *reg, ulong waitbit)
{
ulong timeout, smi_reg;
timeout = PhysmiTimeout;
do {
smi_reg = reg->smi;
if (timeout-- == 0) {
MIIDBG("SMI busy timeout\n");
return -1;
}
} while (smi_reg & waitbit);
return 0;
}
static int
miird(Mii *mii, int pa, int ra)
{
ulong smi_reg, timeout;
Gbereg *reg;
reg = ((Ctlr*)mii->ctlr)->reg;
if ((pa<<Physmiaddroff) & ~Physmiaddrmask ||
(ra<<SmiRegaddroff) & ~SmiRegaddrmask)
return -1;
smibusywait(reg, PhysmiBusy);
reg->smi = pa << Physmiaddroff | ra << SmiRegaddroff | PhysmiopRd;
coherence();
timeout = PhysmiTimeout;
do {
smi_reg = reg->smi;
if (timeout-- == 0) {
MIIDBG("SMI read-valid timeout\n");
return -1;
}
} while (!(smi_reg & PhysmiReadok));
for (timeout = 0; timeout < PhysmiTimeout; timeout++)
;
return reg->smi & Physmidatamask;
}
static int
miiwr(Mii *mii, int pa, int ra, int v)
{
Gbereg *reg;
ulong smi_reg;
reg = ((Ctlr*)mii->ctlr)->reg;
if (((pa<<Physmiaddroff) & ~Physmiaddrmask) ||
((ra<<SmiRegaddroff) & ~SmiRegaddrmask))
return -1;
smibusywait(reg, PhysmiBusy);
smi_reg = v << Physmidataoff | pa << Physmiaddroff | ra << SmiRegaddroff;
reg->smi = smi_reg & ~PhysmiopRd;
coherence();
return 0;
}
#define MIIMODEL(idr2) (((idr2) >> 4) & MASK(6))
enum {
Hacknone,
Hackdual,
Ouimarvell = 0x005043,
Phy1000 = 0x00,
Phy1011 = 0x02,
Phy1000_3 = 0x03,
Phy1000s = 0x04,
Phy1000_5 = 0x05,
Phy1000_6 = 0x06,
Phy3082 = 0x08,
Phy1112 = 0x09,
Phy1121r = 0x0b,
Phy1149 = 0x0b,
Phy1111 = 0x0c,
Phy1116 = 0x21,
Phy1116r = 0x24,
Phy1118 = 0x22,
Phy3016 = 0x26,
};
static int hackflavour;
int
mymii(Mii* mii, int mask)
{
Ctlr *ctlr;
MiiPhy *miiphy;
int bit, ctlrno, oui, model, phyno, r, rmask;
static int dualport, phyidx;
static int phynos[NMiiPhy];
ctlr = mii->ctlr;
ctlrno = ctlr->ether->ctlrno;
dualport = 0;
if (ctlrno == 0) {
for(phyno = 0; phyno < NMiiPhy; phyno++){
bit = 1<<phyno;
if(!(mask & bit) || mii->mask & bit)
continue;
if(mii->mir(mii, phyno, Bmsr) == -1)
continue;
r = mii->mir(mii, phyno, Phyidr1);
oui = (r & 0x3FFF)<<6;
r = mii->mir(mii, phyno, Phyidr2);
oui |= r>>10;
model = MIIMODEL(r);
if (oui == 0xfffff && model == 0x3f)
continue;
MIIDBG("ctlrno %d phy %d oui %#ux model %#ux\n",
ctlrno, phyno, oui, model);
if (oui == Ouimarvell &&
(model == Phy1121r || model == Phy1116r))
++dualport;
phynos[phyidx++] = phyno;
}
hackflavour = dualport == 2 && phyidx == 2? Hackdual: Hacknone;
MIIDBG("ether1116: %s-port phy\n",
hackflavour == Hackdual? "dual": "single");
}
rmask = 0;
if (hackflavour == Hackdual && ctlrno < phyidx) {
MIIDBG("ctlrno %d using ctlrno 0's phyno %d\n",
ctlrno, phynos[ctlrno]);
ctlr->mii = mii = ctlrs[0]->mii;
mask = 1 << phynos[ctlrno];
mii->mask = ~mask;
}
for(phyno = 0; phyno < NMiiPhy; phyno++){
bit = 1<<phyno;
if(!(mask & bit))
continue;
if(mii->mask & bit){
rmask |= bit;
continue;
}
if(mii->mir(mii, phyno, Bmsr) == -1)
continue;
r = mii->mir(mii, phyno, Phyidr1);
oui = (r & 0x3FFF)<<6;
r = mii->mir(mii, phyno, Phyidr2);
oui |= r>>10;
if(oui == 0xFFFFF || oui == 0)
continue;
if((miiphy = malloc(sizeof(MiiPhy))) == nil)
continue;
miiphy->mii = mii;
miiphy->oui = oui;
miiphy->phyno = phyno;
miiphy->anar = ~0;
miiphy->fc = ~0;
miiphy->mscr = ~0;
mii->phy[phyno] = miiphy;
if(ctlrno == 0 || hackflavour != Hackdual && mii->curphy == nil)
mii->curphy = miiphy;
mii->mask |= bit;
mii->nphy++;
rmask |= bit;
}
return rmask;
}
static int
kirkwoodmii(Ether *ether)
{
int i;
Ctlr *ctlr;
MiiPhy *phy;
MIIDBG("mii\n");
ctlr = ether->ctlr;
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
return -1;
ctlr->mii->ctlr = ctlr;
ctlr->mii->mir = miird;
ctlr->mii->miw = miiwr;
if(mymii(ctlr->mii, ~0) == 0 || (phy = ctlr->mii->curphy) == nil){
print("#l%d: ether1116: init mii failure\n", ether->ctlrno);
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
MIIDBG("oui %#X phyno %d\n", phy->oui, phy->phyno);
if((ctlr->ether->ctlrno == 0 || hackflavour != Hackdual) &&
miistatus(ctlr->mii) < 0){
miireset(ctlr->mii);
MIIDBG("miireset\n");
if(miiane(ctlr->mii, ~0, 0, ~0) < 0){
iprint("miiane failed\n");
return -1;
}
MIIDBG("miistatus\n");
miistatus(ctlr->mii);
if(miird(ctlr->mii, phy->phyno, Bmsr) & BmsrLs){
for(i = 0; ; i++){
if(i > 600){
iprint("ether1116: autonegotiation failed\n");
break;
}
if(miird(ctlr->mii, phy->phyno, Bmsr) & BmsrAnc)
break;
delay(10);
}
if(miistatus(ctlr->mii) < 0)
iprint("miistatus failed\n");
}else{
iprint("ether1116: no link\n");
phy->speed = 10;
}
}
ether->mbps = phy->speed;
MIIDBG("#l%d: kirkwoodmii: fd %d speed %d tfc %d rfc %d\n",
ctlr->port, phy->fd, phy->speed, phy->tfc, phy->rfc);
MIIDBG("mii done\n");
return 0;
}
enum {
Pagcopper,
Pagfiber,
Pagrgmii,
Pagled,
Pagrsvd1,
Pagvct,
Pagtest,
Pagrsvd2,
Pagfactest,
};
static void
miiregpage(Mii *mii, ulong dev, ulong page)
{
miiwr(mii, dev, Eadr, page);
}
static int
miiphyinit(Mii *mii)
{
ulong dev;
Ctlr *ctlr;
Gbereg *reg;
ctlr = (Ctlr*)mii->ctlr;
reg = ctlr->reg;
dev = reg->phy;
MIIDBG("phy dev addr %lux\n", dev);
miiregpage(mii, dev, Pagled);
miiwr(mii, dev, Scr, (miird(mii, dev, Scr) & ~0xf) | 1);
miiregpage(mii, dev, Pagrgmii);
miiwr(mii, dev, Scr, miird(mii, dev, Scr) | Rgmiipwrup);
miireset(ctlr->mii);
miiwr(mii, dev, Recr, miird(mii, dev, Recr) | Rxtiming | Rxtiming);
miireset(ctlr->mii);
miiregpage(mii, dev, Pagcopper);
miiwr(mii, dev, Scr,
(miird(mii, dev, Scr) & ~(Pwrdown|Endetect)) | Mdix);
return 0;
}
static void
quiesce(Gbereg *reg)
{
ulong v;
v = reg->tqc;
if (v & 0xFF)
reg->tqc = v << 8;
v = reg->rqc;
if (v & 0xFF)
reg->rqc = v << 8;
while (reg->tqc & 0xFF || reg->rqc & 0xFF)
;
}
static void
p16(uchar *p, ulong v)
{
*p++ = v>>8;
*p = v;
}
static void
p32(uchar *p, ulong v)
{
*p++ = v>>24;
*p++ = v>>16;
*p++ = v>>8;
*p = v;
}
void
archetheraddr(Ether *ether, Gbereg *reg, int rxqno)
{
uchar *ea;
ulong nibble, ucreg, tbloff, regoff;
ea = ether->ea;
p32(ea, reg->macah);
p16(ea+4, reg->macal);
if (memcmp(ea, zeroea, sizeof zeroea) == 0 && ether->ctlrno > 0) {
memmove(ea, ctlrs[0]->ether->ea, Eaddrlen);
ea[Eaddrlen-1] += ether->ctlrno;
reg->macah = ea[0] << 24 | ea[1] << 16 | ea[2] << 8 | ea[3];
reg->macal = ea[4] << 8 | ea[5];
coherence();
}
nibble = ea[5] & 0xf;
tbloff = nibble / 4;
regoff = nibble % 4;
regoff *= 8;
ucreg = reg->dfut[tbloff] & (0xff << regoff);
ucreg |= (rxqno << 1 | Pass) << regoff;
reg->dfut[tbloff] = ucreg;
memset(reg->dfsmt, Qno<<1 | Pass, sizeof reg->dfsmt);
memset(reg->dfomt, Qno<<1 | Pass, sizeof reg->dfomt);
coherence();
}
static void
cfgdramacc(Gbereg *reg)
{
memset(reg->harr, 0, sizeof reg->harr);
memset(reg->base, 0, sizeof reg->base);
reg->bare = MASK(6) - MASK(2);
reg->epap = 3 << 2 | 3;
coherence();
reg->base[0].base = PHYSDRAM | WINATTR(Attrcs0) | Targdram;
reg->base[0].size = WINSIZE(256*MB);
reg->base[1].base = (PHYSDRAM + 256*MB) | WINATTR(Attrcs1) | Targdram;
reg->base[1].size = WINSIZE(256*MB);
coherence();
}
static void
ctlralloc(Ctlr *ctlr)
{
int i;
Block *b;
Rx *r;
Tx *t;
ilock(&freeblocks);
for(i = 0; i < Nrxblks; i++) {
b = iallocb(Rxblklen+Bufalign-1);
if(b == nil) {
iprint("ether1116: no memory for rx buffers\n");
break;
}
assert(b->ref == 1);
b->wp = b->rp = (uchar*)
((uintptr)(b->lim - Rxblklen) & ~(Bufalign - 1));
assert(((uintptr)b->rp & (Bufalign - 1)) == 0);
b->free = rxfreeb;
b->next = freeblocks.head;
freeblocks.head = b;
}
iunlock(&freeblocks);
ctlr->rx = ucallocalign(Nrx * sizeof(Rx), Descralign, 0);
if(ctlr->rx == nil)
panic("ether1116: no memory for rx ring");
for(i = 0; i < Nrx; i++) {
r = &ctlr->rx[i];
assert(((uintptr)r & (Descralign - 1)) == 0);
r->cs = 0;
r->buf = 0;
r->next = PADDR(&ctlr->rx[NEXT(i, Nrx)]);
ctlr->rxb[i] = nil;
}
ctlr->rxtail = ctlr->rxhead = 0;
rxreplenish(ctlr);
ctlr->tx = ucallocalign(Ntx * sizeof(Tx), Descralign, 0);
if(ctlr->tx == nil)
panic("ether1116: no memory for tx ring");
for(i = 0; i < Ntx; i++) {
t = &ctlr->tx[i];
assert(((uintptr)t & (Descralign - 1)) == 0);
t->cs = 0;
t->buf = 0;
t->next = PADDR(&ctlr->tx[NEXT(i, Ntx)]);
ctlr->txb[i] = nil;
}
ctlr->txtail = ctlr->txhead = 0;
}
static void
ctlrinit(Ether *ether)
{
int i;
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
static char name[KNAMELEN];
static Ctlr fakectlr;
for (i = 0; i < nelem(reg->tcqdp); i++)
reg->tcqdp[i] = 0;
for (i = 0; i < nelem(reg->crdp); i++)
reg->crdp[i].r = 0;
coherence();
cfgdramacc(reg);
ctlralloc(ctlr);
reg->tcqdp[Qno] = PADDR(&ctlr->tx[ctlr->txhead]);
reg->crdp[Qno].r = PADDR(&ctlr->rx[ctlr->rxhead]);
coherence();
getmibstats(&fakectlr);
reg->pxmfs = MFS40by;
if (CLOCKFREQ/(Maxrxintrsec*64) >= (1<<16))
panic("rx coalescing value %d too big for short",
CLOCKFREQ/(Maxrxintrsec*64));
reg->sdc = SDCrifb | SDCrxburst(Burst16) | SDCtxburst(Burst16) |
SDCrxnobyteswap | SDCtxnobyteswap |
SDCipgintrx(CLOCKFREQ/(Maxrxintrsec*64));
reg->pxtfut = 0;
reg->irqmask = Isum | Irx | Irxbufferq(Qno) | Irxerr | Itxendq(Qno);
reg->irqemask = IEsum | IEtxerrq(Qno) | IEphystschg | IErxoverrun |
IEtxunderrun;
reg->irqe = 0;
reg->euirqmask = 0;
coherence();
reg->irq = 0;
reg->euirq = 0;
reg->euda = 0;
reg->eudid = Attrcs1 << 4 | Targdram;
reg->portcfg = Rxqdefault(Qno) | Rxqarp(Qno);
reg->portcfgx = 0;
coherence();
reg->psc1 = PSC1rgmii | PSC1encolonbp | PSC1coldomlim(0x23);
reg->psc0 = PSC0porton | PSC0an_flctloff |
PSC0an_pauseadv | PSC0nofrclinkdown | PSC0mru(PSC0mru1522);
coherence();
for (i = 0; i < 4000; i++)
;
ether->link = (reg->ps0 & PS0linkup) != 0;
reg->pmtu = 0;
etheractive(ether);
snprint(name, sizeof name, "#l%drproc", ether->ctlrno);
kproc(name, rcvproc, ether);
reg->rqc = Rxqon(Qno);
coherence();
}
static void
attach(Ether* ether)
{
Ctlr *ctlr = ether->ctlr;
lock(&ctlr->initlock);
if(ctlr->init == 0) {
ctlrinit(ether);
ctlr->init = 1;
}
unlock(&ctlr->initlock);
}
static void
getmibstats(Ctlr *ctlr)
{
Gbereg *reg = ctlr->reg;
ctlr->rxby += reg->rxbylo;
ctlr->txby += reg->txbylo;
ctlr->badrxby += reg->badrxby;
ctlr->mactxerr += reg->mactxerr;
ctlr->rxpkt += reg->rxpkt;
ctlr->badrxpkt += reg->badrxpkt;
ctlr->rxbcastpkt+= reg->rxbcastpkt;
ctlr->rxmcastpkt+= reg->rxmcastpkt;
ctlr->rx64 += reg->rx64;
ctlr->rx65_127 += reg->rx65_127;
ctlr->rx128_255 += reg->rx128_255;
ctlr->rx256_511 += reg->rx256_511;
ctlr->rx512_1023+= reg->rx512_1023;
ctlr->rx1024_max+= reg->rx1024_max;
ctlr->txpkt += reg->txpkt;
ctlr->txcollpktdrop+= reg->txcollpktdrop;
ctlr->txmcastpkt+= reg->txmcastpkt;
ctlr->txbcastpkt+= reg->txbcastpkt;
ctlr->badmacctlpkts+= reg->badmacctlpkts;
ctlr->txflctl += reg->txflctl;
ctlr->rxflctl += reg->rxflctl;
ctlr->badrxflctl+= reg->badrxflctl;
ctlr->rxundersized+= reg->rxundersized;
ctlr->rxfrags += reg->rxfrags;
ctlr->rxtoobig += reg->rxtoobig;
ctlr->rxjabber += reg->rxjabber;
ctlr->rxerr += reg->rxerr;
ctlr->crcerr += reg->crcerr;
ctlr->collisions+= reg->collisions;
ctlr->latecoll += reg->latecoll;
}
long
ifstat(Ether *ether, void *a, long n, ulong off)
{
Ctlr *ctlr = ether->ctlr;
Gbereg *reg = ctlr->reg;
char *buf, *p, *e;
buf = p = malloc(READSTR);
if(p == nil)
panic("ether1116 ifstat: no memory");
e = p + READSTR;
ilock(ctlr);
getmibstats(ctlr);
ctlr->intrs += ctlr->newintrs;
p = seprint(p, e, "interrupts: %lud\n", ctlr->intrs);
p = seprint(p, e, "new interrupts: %lud\n", ctlr->newintrs);
ctlr->newintrs = 0;
p = seprint(p, e, "tx underrun: %lud\n", ctlr->txunderrun);
p = seprint(p, e, "tx ring full: %lud\n", ctlr->txringfull);
ctlr->rxdiscard += reg->pxdfc;
ctlr->rxoverrun += reg->pxofc;
p = seprint(p, e, "rx discarded frames: %lud\n", ctlr->rxdiscard);
p = seprint(p, e, "rx overrun frames: %lud\n", ctlr->rxoverrun);
p = seprint(p, e, "no first+last flag: %lud\n", ctlr->nofirstlast);
p = seprint(p, e, "duplex: %s\n", (reg->ps0 & PS0fd)? "full": "half");
p = seprint(p, e, "flow control: %s\n", (reg->ps0 & PS0flctl)? "on": "off");
p = seprint(p, e, "received bytes: %llud\n", ctlr->rxby);
p = seprint(p, e, "bad received bytes: %lud\n", ctlr->badrxby);
p = seprint(p, e, "internal mac transmit errors: %lud\n", ctlr->mactxerr);
p = seprint(p, e, "total received frames: %lud\n", ctlr->rxpkt);
p = seprint(p, e, "received broadcast frames: %lud\n", ctlr->rxbcastpkt);
p = seprint(p, e, "received multicast frames: %lud\n", ctlr->rxmcastpkt);
p = seprint(p, e, "bad received frames: %lud\n", ctlr->badrxpkt);
p = seprint(p, e, "received frames 0-64: %lud\n", ctlr->rx64);
p = seprint(p, e, "received frames 65-127: %lud\n", ctlr->rx65_127);
p = seprint(p, e, "received frames 128-255: %lud\n", ctlr->rx128_255);
p = seprint(p, e, "received frames 256-511: %lud\n", ctlr->rx256_511);
p = seprint(p, e, "received frames 512-1023: %lud\n", ctlr->rx512_1023);
p = seprint(p, e, "received frames 1024-max: %lud\n", ctlr->rx1024_max);
p = seprint(p, e, "transmitted bytes: %llud\n", ctlr->txby);
p = seprint(p, e, "total transmitted frames: %lud\n", ctlr->txpkt);
p = seprint(p, e, "transmitted broadcast frames: %lud\n", ctlr->txbcastpkt);
p = seprint(p, e, "transmitted multicast frames: %lud\n", ctlr->txmcastpkt);
p = seprint(p, e, "transmit frames dropped by collision: %lud\n", ctlr->txcollpktdrop);
p = seprint(p, e, "misaligned buffers: %lud\n", ether->pktsmisaligned);
p = seprint(p, e, "bad mac control frames: %lud\n", ctlr->badmacctlpkts);
p = seprint(p, e, "transmitted flow control messages: %lud\n", ctlr->txflctl);
p = seprint(p, e, "received flow control messages: %lud\n", ctlr->rxflctl);
p = seprint(p, e, "bad received flow control messages: %lud\n", ctlr->badrxflctl);
p = seprint(p, e, "received undersized packets: %lud\n", ctlr->rxundersized);
p = seprint(p, e, "received fragments: %lud\n", ctlr->rxfrags);
p = seprint(p, e, "received oversized packets: %lud\n", ctlr->rxtoobig);
p = seprint(p, e, "received jabber packets: %lud\n", ctlr->rxjabber);
p = seprint(p, e, "mac receive errors: %lud\n", ctlr->rxerr);
p = seprint(p, e, "crc errors: %lud\n", ctlr->crcerr);
p = seprint(p, e, "collisions: %lud\n", ctlr->collisions);
p = seprint(p, e, "late collisions: %lud\n", ctlr->latecoll);
USED(p);
iunlock(ctlr);
n = readstr(off, a, n, buf);
free(buf);
return n;
}
static int
reset(Ether *ether)
{
Ctlr *ctlr;
ether->ctlr = ctlr = malloc(sizeof *ctlr);
if (ctlr == nil)
panic("ether1116 reset: no memory");
switch(ether->ctlrno) {
case 0:
ether->irq = IRQ0gbe0sum;
break;
case 1:
ether->irq = IRQ0gbe1sum;
break;
default:
panic("ether1116: bad ether ctlr #%d", ether->ctlrno);
}
ctlr->reg = (Gbereg*)soc.ether[ether->ctlrno];
*(ulong *)soc.iocfg |= 1 << 7 | 1 << 15;
coherence();
ctlr->ether = ether;
ctlrs[ether->ctlrno] = ctlr;
shutdown(ether);
((Gbereg*)soc.ether[0])->psc1 |= PSC1rgmii;
((Gbereg*)soc.ether[1])->psc1 |= PSC1rgmii;
coherence();
ctlr->port = ether->ctlrno;
ctlr->reg->phy = ether->ctlrno;
coherence();
ether->port = (uintptr)ctlr->reg;
if(kirkwoodmii(ether) < 0){
free(ctlr);
ether->ctlr = nil;
return -1;
}
miiphyinit(ctlr->mii);
archetheraddr(ether, ctlr->reg, Qno);
if (memcmp(ether->ea, zeroea, sizeof zeroea) == 0){
iprint("ether1116: reset: zero ether->ea\n");
free(ctlr);
ether->ctlr = nil;
return -1;
}
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->shutdown = shutdown;
ether->ctl = ctl;
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
return 0;
}
void
ether1116link(void)
{
addethercard("88e1116", reset);
}