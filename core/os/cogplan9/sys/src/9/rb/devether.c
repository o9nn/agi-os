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
#include <pool.h>
enum {
Ntd = 64,
Nrd = 256,
Nrb = 1024,
Bufalign= 4,
Rbsz = ETHERMAXTU + 4,
};
extern uchar arge0mac[Eaddrlen];
extern uchar arge1mac[Eaddrlen];
typedef struct Arge Arge;
typedef struct Ctlr Ctlr;
typedef struct Desc Desc;
typedef struct Etherif Etherif;
struct Arge {
ulong cfg1;
ulong cfg2;
ulong ifg;
ulong hduplex;
ulong maxframelen;
uchar _pad0[0x20 - 0x14];
ulong miicfg;
ulong miicmd;
ulong miiaddr;
ulong miictl;
ulong miists;
ulong miiindic;
ulong ifctl;
ulong _pad1;
ulong staaddr1;
ulong staaddr2;
ulong fifocfg[3];
ulong fifotxthresh;
ulong fiforxfiltmatch;
ulong fiforxfiltmask;
ulong fiforam[7];
uchar _pad2[0x180 - 0x7c];
ulong txctl;
ulong txdesc;
ulong txsts;
ulong rxctl;
ulong rxdesc;
ulong rxsts;
ulong dmaintr;
ulong dmaintrsts;
};
enum {
Cfg1softrst = 1 << 31,
Cfg1simulrst = 1 << 30,
Cfg1macrxblkrst = 1 << 19,
Cfg1mactxblkrst = 1 << 18,
Cfg1rxfuncrst = 1 << 17,
Cfg1txfuncrst = 1 << 16,
Cfg1loopback = 1 << 8,
Cfg1rxflowctl = 1 << 5,
Cfg1txflowctl = 1 << 4,
Cfg1syncrx = 1 << 3,
Cfg1rxen = 1 << 2,
Cfg1synctx = 1 << 1,
Cfg1txen = 1 << 0,
Cfg2preamblelenmask = 0xf,
Cfg2preamblelenshift = 12,
Cfg2ifmode1000 = 2 << 8,
Cfg2ifmode10_100 = 1 << 8,
Cfg2ifmodeshift = 8,
Cfg2ifmodemask = 3,
Cfg2hugeframe = 1 << 5,
Cfg2lenfield = 1 << 4,
Cfg2enpadcrc = 1 << 2,
Cfg2encrc = 1 << 1,
Cfg2fdx = 1 << 0,
Miicfgrst = 1 << 31,
Miicfgscanautoinc = 1 << 5,
Miicfgpreamblesup = 1 << 4,
Miicfgclkselmask = 0x7,
Miicfgclkdiv4 = 0,
Miicfgclkdiv6 = 2,
Miicfgclkdiv8 = 3,
Miicfgclkdiv10 = 4,
Miicfgclkdiv14 = 5,
Miicfgclkdiv20 = 6,
Miicfgclkdiv28 = 7,
Miicmdscancycle = 1 << 1,
Miicmdread = 1,
Miicmdwrite = 0,
Miiphyaddrshift = 8,
Miiphyaddrmask = 0xff,
Miiregmask = 0x1f,
Miictlmask = 0xffff,
Miistsmask = 0xffff,
Miiindicinvalid = 1 << 2,
Miiindicscanning = 1 << 1,
Miiindicbusy = 1 << 0,
Ifctlspeed = 1 << 16,
Fifocfg0txfabric = 1 << 4,
Fifocfg0txsys = 1 << 3,
Fifocfg0rxfabric = 1 << 2,
Fifocfg0rxsys = 1 << 1,
Fifocfg0watermark = 1 << 0,
Fifocfg0all = MASK(5),
Fifocfg0enshift = 8,
Ffunicast = 1 << 17,
Fftruncframe = 1 << 16,
Ffvlantag = 1 << 15,
Ffunsupopcode = 1 << 14,
Ffpauseframe = 1 << 13,
Ffctlframe = 1 << 12,
Fflongevent = 1 << 11,
Ffdribblenibble = 1 << 10,
Ffbcast = 1 << 9,
Ffmcast = 1 << 8,
Ffok = 1 << 7,
Ffoorange = 1 << 6,
Fflenmsmtch = 1 << 5,
Ffcrcerr = 1 << 4,
Ffcodeerr = 1 << 3,
Fffalsecarrier = 1 << 2,
Ffrxdvevent = 1 << 1,
Ffdropevent = 1 << 0,
Ffmatchdflt = Ffvlantag | Ffunsupopcode | Ffpauseframe | Ffctlframe |
Fflongevent | Ffdribblenibble | Ffbcast | Ffmcast | Ffok |
Ffoorange | Fflenmsmtch | Ffcrcerr | Ffcodeerr |
Fffalsecarrier | Ffrxdvevent | Ffdropevent,
Frmbytemode = 1 << 19,
Frmnoshortframe = 1 << 18,
Frmbit17 = 1 << 17,
Frmbit16 = 1 << 16,
Frmtruncframe = 1 << 15,
Frmlongevent = 1 << 14,
Frmvlantag = 1 << 13,
Frmunsupopcode = 1 << 12,
Frmpauseframe = 1 << 11,
Frmctlframe = 1 << 10,
Frmdribblenibble = 1 << 9,
Frmbcast = 1 << 8,
Frmmcast = 1 << 7,
Frmok = 1 << 6,
Frmoorange = 1 << 5,
Frmlenmsmtch = 1 << 4,
Frmcodeerr = 1 << 3,
Frmfalsecarrier = 1 << 2,
Frmrxdvevent = 1 << 1,
Frmdropevent = 1 << 0,
Ffmaskdflt = Frmnoshortframe | Frmbit17 | Frmbit16 | Frmtruncframe |
Frmlongevent | Frmvlantag | Frmpauseframe | Frmctlframe |
Frmdribblenibble | Frmbcast | Frmmcast | Frmok | Frmoorange |
Frmcodeerr | Frmfalsecarrier | Frmrxdvevent | Frmdropevent,
Dmatxctlen = 1 << 0,
Txpcountmask = 0xff,
Txpcountshift = 16,
Txbuserr = 1 << 3,
Txunderrun = 1 << 1,
Txpktsent = 1 << 0,
Dmarxctlen = 1 << 0,
Rxpcountmask = 0xff,
Rxpcountshift = 16,
Rxbuserr = 1 << 3,
Rxovflo = 1 << 2,
Rxpktrcvd = 1 << 0,
Dmarxbuserr = 1 << 7,
Dmarxovflo = 1 << 6,
Dmarxpktrcvd = 1 << 4,
Dmatxbuserr = 1 << 3,
Dmatxunderrun = 1 << 1,
Dmatxpktsent = 1 << 0,
Dmaall = Dmarxbuserr | Dmarxovflo | Dmarxpktrcvd | Dmatxbuserr,
Spictlremapdisable = 1 << 6,
Spictlclkdividermask = MASK(6),
Spiioctlcs2 = 1 << 18,
Spiioctlcs1 = 1 << 17,
Spiioctlcs0 = 1 << 16,
Spiioctlcsmask = 7 << 16,
Spiioctlclk = 1 << 8,
Spiioctldo = 1,
};
struct Spi {
ulong fs;
ulong ctl;
ulong ioctl;
ulong rds;
};
struct Desc {
ulong addr;
ulong ctl;
Desc *next;
ulong _pad;
};
enum {
Descempty = 1 << 31,
Descmore = 1 << 24,
Descszmask = MASK(12),
};
#define DMASIZE(len) ((len) & Descszmask)
struct Ctlr {
Arge *regs;
Ether* edev;
Lock;
int init;
int attached;
Mii* mii;
Rendez lrendez;
int lim;
int link;
int phymask;
Rendez rrendez;
uint rintr;
int pktstoread;
int discard;
Desc* rdba;
Block** rd;
uint rdh;
uint rdt;
uint nrdfree;
Rendez trendez;
uint tintr;
int pktstosend;
int ntq;
Desc* tdba;
Block** td;
uint tdh;
uint tdt;
};
struct Etherif {
uintptr regs;
int irq;
uchar *mac;
int phymask;
};
static Etherif etherifs[] = {
{ 0x1a000000, ILenet0, arge0mac, 1<<4 },
{ 0x19000000, ILenet1, arge1mac, MASK(4) },
};
static Ether *etherxx[MaxEther];
static Lock athrblock;
static Block* athrbpool;
static void athrbfree(Block* bp);
enum {
Swrgmii = 0,
Swgmii = 1,
Swphy4cpu = 0,
};
typedef struct Switch Switch;
struct Switch {
int page;
int scdev;
};
enum {
Miiathdbgaddr = 0x1d,
Miiathdbgdata = 0x1e,
Swregmask = 0,
Swmaskrevmask = 0x00ff,
Swmaskvermask = 0xff00,
Swmaskvershift = 8,
Swmasksoftreset = 1 << 31,
Swregmode = 8,
Swdir615uboot = 0x8d1003e0,
Swrgmiiport4iso = 0x81461bea,
Swrgmiiport4sw = 0x01261be2,
Swgmiiavm = 0x010e5b71,
Swmac0gmiien = 1 << 0,
Swmac0rgmiien = 1 << 1,
Swphy4gmiien = 1 << 2,
Swphy4rgmiien = 1 << 3,
Swmac0macmode = 1 << 4,
Swrgmiirxclkdelayen= 1 << 6,
Swrgmiitxclkdelayen= 1 << 7,
Swmac5macmode = 1 << 14,
Swmac5phymode = 1 << 15,
Swtxdelays0 = 1 << 21,
Swtxdelays1 = 1 << 22,
Swrxdelays0 = 1 << 23,
Swledopenen = 1 << 24,
Swspien = 1 << 25,
Swrxdelays1 = 1 << 26,
Swpoweronsel = 1 << 31,
Swregfloodmask = 0x2c,
Swfloodmaskbcast2cpu= 1 << 26,
Swregglobal = 0x30,
Swglobalmtumask = 0x7fff,
};
#ifdef NOTYET
void *
devicegetparent(int)
{
static int glop;
return &glop;
}
static void
arswsplitsetpage(int dev, ulong addr, ushort *phy, ushort *reg)
{
static Switch ar8316;
Switch *sc = &ar8316;
ushort page;
page = ((addr) >> 9) & 0xffff;
*phy = (((addr) >> 6) & 0x7) | 0x10;
*reg = ((addr) >> 1) & 0x1f;
MDIOWRREG(devicegetparent(dev), 0x18, 0, page);
sc->page = page;
}
static int
arswrdreg16(int dev, int addr)
{
ushort phy, reg;
arswsplitsetpage(dev, addr, &phy, &reg);
return MDIORDREG(devicegetparent(dev), phy, reg);
}
void
arswwritedbg(int dev, int phy, ushort dbgaddr, ushort dbgdata)
{
MDIOWRREG(devicegetparent(dev), phy, Miiathdbgaddr, dbgaddr);
MDIOWRREG(devicegetparent(dev), phy, Miiathdbgdata, dbgdata);
}
static inline int
arswwrreg16(int dev, int addr, int data)
{
ushort phy, reg;
arswsplitsetpage(dev, addr, &phy, &reg);
return MDIOWRREG(devicegetparent(dev), phy, reg, data);
}
int
arswrdreg(int dev, int addr)
{
return arswrdreglsb(dev, addr) | arswrdregmsb(dev, addr);
}
int
arswwrreg(int dev, int addr, int value)
{
arswwrreglsb(dev, addr, value);
return arswwrregmsb(dev, addr, value);
}
int
arswmodifyreg(int dev, int addr, int mask, int set)
{
return arswwrreg(dev, addr, (arswrdreg(dev, addr) & ~mask) | set);
}
static int
ar8316init(Switch *sc)
{
if (Swrgmii && Swphy4cpu) {
arswwrreg(sc->scdev, Swregmode, Swrgmiiport4iso);
iprint("ar8316: MAC port == RGMII, port 4 = dedicated PHY\n");
} else if (Swrgmii) {
arswwrreg(sc->scdev, Swregmode, Swrgmiiport4sw);
iprint("ar8316: MAC port == RGMII, port 4 = switch port\n");
} else if (Swgmii) {
arswwrreg(sc->scdev, Swregmode, Swgmiiavm);
iprint("ar8316: MAC port == GMII\n");
} else {
iprint("ar8316: unknown switch PHY config\n");
return -1;
}
delay(1);
if (Swrgmii && Swphy4cpu) {
iprint("ar8316: port 4 RGMII hack\n");
arswwritedbg(sc->scdev, 4, 0x12, 0x480c);
arswwritedbg(sc->scdev, 4, 0x0, 0x824e);
arswwritedbg(sc->scdev, 4, 0x5, 0x3d47);
delay(1);
}
arswwrreg(sc->scdev, 0x38, 0xc000050e);
arswwrreg(sc->scdev, Swregfloodmask, Swfloodmaskbcast2cpu | 0x003f003f);
arswmodifyreg(sc->scdev, Swregglobal, Swglobalmtumask, ETHERMAXTU+8+2);
return 0;
}
#endif
static long
ifstat(Ether* edev, void* a, long n, ulong offset)
{
int l, i, r;
char *p;
Ctlr *ctlr;
ctlr = edev->ctlr;
p = malloc(READSTR);
if(p == nil)
error(Enomem);
l = 0;
l += snprint(p+l, READSTR-l, "tintr: %ud\n", ctlr->tintr);
l += snprint(p+l, READSTR-l, "rintr: %ud\n", ctlr->rintr);
l += snprint(p+l, READSTR-l, "discarded: %ud\n", ctlr->discard);
if(ctlr->mii != nil && ctlr->mii->curphy != nil){
l += snprint(p+l, READSTR-l, "phy:   ");
for(i = 0; i < NMiiPhyr; i++){
if(i && ((i & 0x07) == 0))
l += snprint(p+l, READSTR-l, "\n       ");
r = miimir(ctlr->mii, i);
l += snprint(p+l, READSTR-l, " %4.4uX", r);
}
snprint(p+l, READSTR-l, "\n");
}
n = readstr(offset, a, n, p);
free(p);
return n;
}
static void
etherrtrace(Netfile* f, Etherpkt* pkt, int len)
{
int i, n;
Block *bp;
if(qwindow(f->in) <= 0)
return;
if(len > 58)
n = 58;
else
n = len;
bp = iallocb(64);
if(bp == nil)
return;
memmove(bp->wp, pkt->d, n);
i = TK2MS(MACHP(0)->ticks);
bp->wp[58] = len>>8;
bp->wp[59] = len;
bp->wp[60] = i>>24;
bp->wp[61] = i>>16;
bp->wp[62] = i>>8;
bp->wp[63] = i;
bp->wp += 64;
qpass(f->in, bp);
}
Block*
etheriq(Ether* ether, Block* bp, int fromwire)
{
Etherpkt *pkt;
ushort type;
int len, multi, tome, fromme;
Netfile **ep, *f, **fp, *fx;
Block *xbp;
Ctlr *ctlr;
ether->inpackets++;
ctlr = ether->ctlr;
pkt = (Etherpkt*)bp->rp;
len = BLEN(bp);
type = (pkt->type[0]<<8)|pkt->type[1];
fx = 0;
ep = &ether->f[Ntypes];
multi = pkt->d[0] & 1;
if(multi && memcmp(pkt->d, ether->bcast, sizeof(pkt->d)) != 0 &&
ether->prom == 0)
if(!activemulti(ether, pkt->d, sizeof(pkt->d))){
if(fromwire){
ctlr->discard++;
freeb(bp);
bp = 0;
}
return bp;
}
tome = memcmp(pkt->d, ether->ea, sizeof(pkt->d)) == 0;
fromme = memcmp(pkt->s, ether->ea, sizeof(pkt->s)) == 0;
for(fp = ether->f; fp < ep; fp++)
if((f = *fp) != nil && (f->type == type || f->type < 0))
if(tome || multi || f->prom)
if(f->bridge && !fromwire && !fromme)
continue;
else if(f->headersonly)
etherrtrace(f, pkt, len);
else if(fromwire && fx == 0)
fx = f;
else if(xbp = iallocb(len)){
memmove(xbp->wp, pkt, len);
xbp->wp += len;
if(qpass(f->in, xbp) < 0){
iprint("soverflow for f->in\n");
ether->soverflows++;
}
}else{
iprint("soverflow iallocb\n");
ether->soverflows++;
}
if(fx){
if(qpass(fx->in, bp) < 0){
iprint("soverflow for fx->in\n");
ether->soverflows++;
}
return 0;
}
if(fromwire){
ctlr->discard++;
freeb(bp);
return 0;
}
return bp;
}
static void
athhwreset(Ether *ether)
{
Ctlr *ctlr;
Arge *arge;
ctlr = ether->ctlr;
if (ctlr == nil)
return;
arge = ctlr->regs;
if (arge == nil)
return;
arge->dmaintr = 0;
arge->rxctl = 0;
arge->txctl = 0;
coherence();
delay(1);
arge->rxdesc = 0;
arge->txdesc = 0;
coherence();
while (arge->rxsts & Rxpktrcvd)
arge->rxsts = Rxpktrcvd;
while (arge->txsts & Txpktsent)
arge->txsts = Txpktsent;
arge->rxsts = Rxbuserr | Rxovflo;
arge->txsts = Txbuserr | Txunderrun;
}
static void
txreclaim(Ctlr *ctlr)
{
uint tdh;
Arge *arge;
Block *bp;
arge = ctlr->regs;
tdh = ctlr->tdh;
while (tdh != ctlr->tdt && ctlr->tdba[tdh].ctl & Descempty){
arge->txsts = Txpktsent;
bp = ctlr->td[tdh];
ctlr->td[tdh] = nil;
if (bp)
freeb(bp);
ctlr->tdba[tdh].addr = 0;
ctlr->ntq--;
tdh = NEXT(tdh, Ntd);
}
ctlr->tdh = tdh;
}
static Block*
athrballoc(void)
{
Block *bp;
ilock(&athrblock);
if((bp = athrbpool) != nil){
athrbpool = bp->next;
bp->next = nil;
_xinc(&bp->ref);
}
iunlock(&athrblock);
return bp;
}
static void
athrbfree(Block* bp)
{
bp->wp = bp->rp = bp->lim - ROUND(Rbsz, BLOCKALIGN);
bp->flag &= ~(Bipck | Budpck | Btcpck | Bpktck);
ilock(&athrblock);
bp->next = athrbpool;
athrbpool = bp;
iunlock(&athrblock);
}
static void
rxnewbuf(Ctlr *ctlr, int i)
{
Block *bp;
Desc *rd;
if (ctlr->rd[i] != nil)
return;
ctlr->rd[i] = bp = athrballoc();
if(bp == nil)
panic("#l%d: can't allocate receive buffer",
ctlr->edev->ctlrno);
dcflush(bp->rp, Rbsz);
rd = &ctlr->rdba[i];
rd->addr = PADDR(bp->rp);
rd->ctl = Descempty | DMASIZE(Rbsz);
ctlr->nrdfree++;
}
static void
rxreclaim(Ctlr *ctlr)
{
uint rdt;
rdt = ctlr->rdt;
while (rdt != ctlr->rdh && !(ctlr->rdba[rdt].ctl & Descempty)){
rxnewbuf(ctlr, rdt);
rdt = NEXT(rdt, Nrd);
}
ctlr->rdt = rdt;
}
static void
etherintr(void *arg)
{
int sts;
Arge *arge;
Ctlr *ctlr;
Ether *ether;
ether = arg;
ctlr = ether->ctlr;
arge = ctlr->regs;
ilock(ctlr);
sts = arge->dmaintrsts;
if (sts & Dmarxpktrcvd) {
arge->dmaintr &= ~Dmarxpktrcvd;
ctlr->pktstoread = 1;
wakeup(&ctlr->rrendez);
ctlr->rintr++;
sts &= ~Dmarxpktrcvd;
}
if (sts & (Dmatxpktsent | Dmatxunderrun)) {
arge->dmaintr &= ~(Dmatxpktsent | Dmatxunderrun);
ctlr->pktstosend = 1;
wakeup(&ctlr->trendez);
ctlr->tintr++;
sts &= ~(Dmatxpktsent | Dmatxunderrun);
}
iunlock(ctlr);
if (sts)
iprint("#l%d: sts %#ux\n", ether->ctlrno, sts);
}
static int
pktstoread(void* v)
{
Ctlr *ctlr = v;
return ctlr->pktstoread || !(ctlr->rdba[ctlr->rdh].ctl & Descempty);
}
static void
rproc(void* arg)
{
uint rdh, sz;
Arge *arge;
Block *bp;
Ctlr *ctlr;
Desc *rd;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
arge = ctlr->regs;
for(;;){
ilock(ctlr);
arge->dmaintr |= Dmarxpktrcvd;
iunlock(ctlr);
sleep(&ctlr->rrendez, pktstoread, ctlr);
ctlr->pktstoread = 0;
rxreclaim(ctlr);
rdh = ctlr->rdh;
for (rd = &ctlr->rdba[rdh]; !(rd->ctl & Descempty);
rd = &ctlr->rdba[rdh]){
bp = ctlr->rd[rdh];
assert(bp != nil);
ctlr->rd[rdh] = nil;
sz = DMASIZE(rd->ctl) - 4;
assert(sz > 0 && sz <= Rbsz);
bp->wp = bp->rp + sz;
bp = etheriq(edev, bp, 1);
assert(bp == nil);
arge->rxsts = Rxpktrcvd;
ctlr->nrdfree--;
rdh = NEXT(rdh, Nrd);
if(ctlr->nrdfree < Nrd/2) {
ctlr->rdh = rdh;
rxreclaim(edev->ctlr);
}
}
ctlr->rdh = rdh;
}
}
static int
pktstosend(void* v)
{
Ether *edev = v;
Ctlr *ctlr = edev->ctlr;
return ctlr->pktstosend || ctlr->ntq > 0 || qlen(edev->oq) > 0;
}
static void
tproc(void* arg)
{
uint tdt, added;
Arge *arge;
Block *bp;
Ctlr *ctlr;
Desc *td;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
arge = ctlr->regs;
for(;;){
sleep(&ctlr->trendez, pktstosend, edev);
ctlr->pktstosend = 0;
txreclaim(ctlr);
added = 0;
tdt = ctlr->tdt;
while(ctlr->ntq < Ntd - 1){
td = &ctlr->tdba[tdt];
if (!(td->ctl & Descempty))
break;
bp = qget(edev->oq);
if(bp == nil)
break;
dcflush(bp->rp, BLEN(bp));
ctlr->td[tdt] = bp;
td->addr = PADDR(bp->rp);
td->ctl = DMASIZE(BLEN(bp));
coherence();
added++;
ctlr->ntq++;
tdt = NEXT(tdt, Ntd);
}
ctlr->tdt = tdt;
if (arge->dmaintrsts & Dmatxunderrun)
arge->txsts = Txunderrun;
if(1 || added)
arge->txctl = Dmatxctlen;
ilock(ctlr);
if(ctlr->ntq >= Ntd/2)
arge->dmaintr |= Dmatxpktsent;
else if (ctlr->ntq > 0)
arge->dmaintr |= Dmatxunderrun;
iunlock(ctlr);
txreclaim(ctlr);
}
}
static void
promiscuous(void *ve, int on)
{
USED(ve, on);
}
static void
multicast(void *ve, uchar*, int on)
{
USED(ve, on);
}
static void
linkdescs(Desc *base, int ndesc)
{
int i;
for(i = 0; i < ndesc - 1; i++)
base[i].next = (Desc *)PADDR(&base[i+1]);
base[ndesc - 1].next = (Desc *)PADDR(&base[0]);
}
static void
ringinit(Ctlr* ctlr)
{
int i;
void *v;
if(ctlr->rdba == 0){
v = xspanalloc(Nrd * sizeof(Desc), CACHELINESZ, 0);
assert(v);
ctlr->rdba = (Desc *)KSEG1ADDR(v);
ctlr->rd = xspanalloc(Nrd * sizeof(Block *), 0, 0);
assert(ctlr->rd != nil);
linkdescs(ctlr->rdba, Nrd);
for(i = 0; i < Nrd; i++)
rxnewbuf(ctlr, i);
}
ctlr->rdt = ctlr->rdh = 0;
if(ctlr->tdba == 0) {
v = xspanalloc(Ntd * sizeof(Desc), CACHELINESZ, 0);
assert(v);
ctlr->tdba = (Desc *)KSEG1ADDR(v);
ctlr->td = xspanalloc(Ntd * sizeof(Block *), 0, 0);
assert(ctlr->td != nil);
}
memset(ctlr->td, 0, Ntd * sizeof(Block *));
linkdescs(ctlr->tdba, Ntd);
for(i = 0; i < Ntd; i++)
ctlr->tdba[i].ctl = Descempty;
ctlr->tdh = ctlr->tdt = 0;
}
static void
cfgmediaduplex(Ether *ether)
{
Arge *arge, *arge0;
Ctlr *ctlr;
ctlr = ether->ctlr;
arge = ctlr->regs;
arge->cfg2 = (arge->cfg2 & ~Cfg2ifmode10_100) | Cfg2ifmode1000 | Cfg2fdx;
arge->ifctl &= ~Ifctlspeed;
arge->fiforxfiltmask |= Frmbytemode;
arge->fifotxthresh = 0x008001ff;
if (ether->ctlrno > 0) {
arge0 = (Arge *)(KSEG1 | etherifs[0].regs);
USED(arge0);
}
}
static void
athmii(Ether *ether, int phymask)
{
USED(ether, phymask);
}
static void
athcfg(Ether *ether, int phymask)
{
uchar *eaddr;
Arge *arge;
Ctlr *ctlr;
ctlr = ether->ctlr;
arge = ctlr->regs;
if(ether->ctlrno > 0){
if(0){
arge->cfg1 |= Cfg1softrst;
delay(20);
*Reset |= Rstge1mac;
delay(100);
}
*Reset &= ~Rstge1mac;
delay(200);
}
arge->cfg1 = Cfg1syncrx | Cfg1rxen | Cfg1synctx | Cfg1txen;
arge->cfg2 |= Cfg2enpadcrc | Cfg2lenfield | Cfg2encrc;
arge->maxframelen = Rbsz;
if(ether->ctlrno > 0){
arge->miicfg = Miicfgrst;
delay(100);
arge->miicfg = Miicfgclkdiv28;
delay(100);
}
eaddr = ether->ea;
arge->staaddr1 = eaddr[2]<<24 | eaddr[3]<<16 | eaddr[4]<<8 | eaddr[5];
arge->staaddr2 = eaddr[0]<< 8 | eaddr[1];
arge->fifocfg[0] = Fifocfg0all << Fifocfg0enshift;
arge->fifocfg[1] = 0x0fff0000;
arge->fifocfg[2] = 0x00001fff;
arge->fiforxfiltmatch = Ffmatchdflt;
arge->fiforxfiltmask = Ffmaskdflt;
athmii(ether, phymask);
if (ether->ctlrno > 0)
cfgmediaduplex(ether);
}
static int
athattach(Ether *ether)
{
int i;
char name[32];
Arge *arge;
Block *bp;
Ctlr *ctlr;
ctlr = ether->ctlr;
if (ctlr->attached)
return -1;
ilock(ctlr);
ctlr->init = 1;
for(i = 0; i < Nrb; i++){
if((bp = allocb(Rbsz + Bufalign)) == nil)
error(Enomem);
bp->free = athrbfree;
freeb(bp);
}
ringinit(ctlr);
ctlr->init = 0;
iunlock(ctlr);
athcfg(ether, ctlr->phymask);
arge = ctlr->regs;
arge->txdesc = PADDR(ctlr->tdba);
arge->rxdesc = PADDR(ctlr->rdba);
coherence();
arge->rxctl = Dmarxctlen;
snprint(name, KNAMELEN, "#l%drproc", ether->ctlrno);
kproc(name, rproc, ether);
snprint(name, KNAMELEN, "#l%dtproc", ether->ctlrno);
kproc(name, tproc, ether);
ilock(ctlr);
arge->dmaintr |= Dmaall;
iunlock(ctlr);
ctlr->attached = 1;
return 0;
}
static int
athreset(Ether *ether)
{
Arge *arge;
Ctlr *ctlr;
Etherif *ep;
if (ether->ctlrno < 0 || ether->ctlrno >= MaxEther)
return -1;
if (ether->ctlr == nil) {
ether->ctlr = ctlr = malloc(sizeof(Ctlr));
if (ctlr == nil)
return -1;
ctlr->edev = ether;
ep = etherifs + ether->ctlrno;
ctlr->regs = arge = (Arge *)(KSEG1 | ep->regs);
ctlr->phymask = ep->phymask;
ether->port = (uint)arge;
ether->irq = ep->irq;
memmove(ether->ea, ep->mac, Eaddrlen);
ether->ifstat = ifstat;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
ether->arg = ether;
}
athhwreset(ether);
return 0;
}
static Ether*
etherprobe(int ctlrno)
{
int i, lg;
ulong mb, bsz;
Ether *ether;
char buf[128], name[32];
ether = malloc(sizeof(Ether));
if(ether == nil)
error(Enomem);
memset(ether, 0, sizeof(Ether));
ether->ctlrno = ctlrno;
ether->tbdf = BUSUNKNOWN;
ether->mbps = 1000;
ether->minmtu = ETHERMINTU;
ether->maxmtu = ETHERMAXTU;
ether->mtu = ETHERMAXTU;
if(ctlrno >= MaxEther || athreset(ether) < 0){
free(ether);
return nil;
}
snprint(name, sizeof(name), "ether%d", ctlrno);
if(ether->irq >= 0)
intrenable(ether->irq, etherintr, ether);
i = sprint(buf, "#l%d: atheros71xx: ", ctlrno);
if(ether->mbps >= 1000)
i += sprint(buf+i, "%dGbps", ether->mbps/1000);
else
i += sprint(buf+i, "%dMbps", ether->mbps);
i += sprint(buf+i, " port %#luX irq %d", PADDR(ether->port), ether->irq);
i += sprint(buf+i, ": %2.2ux%2.2ux%2.2ux%2.2ux%2.2ux%2.2ux",
ether->ea[0], ether->ea[1], ether->ea[2],
ether->ea[3], ether->ea[4], ether->ea[5]);
sprint(buf+i, "\n");
print(buf);
for(lg = 0, mb = ether->mbps; mb >= 10; lg++)
mb /= 10;
if (lg > 13)
lg = 13;
bsz = 1UL << (lg + 16);
while (bsz > mainmem->maxsize / 8 && bsz > 128*1024)
bsz /= 2;
netifinit(ether, name, Ntypes, bsz);
if(ether->oq == nil)
ether->oq = qopen(1 << (lg + 13), Qmsg, 0, 0);
if(ether->oq == nil)
panic("etherreset %s: can't allocate output queue", name);
ether->alen = Eaddrlen;
memmove(ether->addr, ether->ea, Eaddrlen);
memset(ether->bcast, 0xFF, Eaddrlen);
return ether;
}
static void
etherreset(void)
{
int ctlrno;
for(ctlrno = 0; ctlrno < MaxEther; ctlrno++)
etherxx[ctlrno] = etherprobe(ctlrno);
}
static void
ethershutdown(void)
{
Ether *ether;
int i;
for(i = 0; i < MaxEther; i++){
ether = etherxx[i];
if(ether)
athhwreset(ether);
}
}
static Chan *
etherattach(char* spec)
{
ulong ctlrno;
char *p;
Chan *chan;
ctlrno = 0;
if(spec && *spec){
ctlrno = strtoul(spec, &p, 0);
if((ctlrno == 0 && p == spec) || *p || (ctlrno >= MaxEther))
error(Ebadarg);
}
if(etherxx[ctlrno] == 0)
error(Enodev);
chan = devattach('l', spec);
if(waserror()){
chanfree(chan);
nexterror();
}
chan->dev = ctlrno;
athattach(etherxx[ctlrno]);
poperror();
return chan;
}
static Walkqid*
etherwalk(Chan *c, Chan *nc, char **name, int nname)
{
return netifwalk(etherxx[c->dev], c, nc, name, nname);
}
static Chan*
etheropen(Chan *c, int omode)
{
return netifopen(etherxx[c->dev], c, omode);
}
static void
ethercreate(Chan*, char*, int, ulong)
{
}
static void
etherclose(Chan *c)
{
netifclose(etherxx[c->dev], c);
}
static long
etherread(Chan *chan, void *buf, long n, vlong off)
{
Ether *ether;
ulong offset = off;
ether = etherxx[chan->dev];
if((chan->qid.type & QTDIR) == 0 && ether->ifstat){
if(NETTYPE(chan->qid.path) == Nifstatqid)
return ether->ifstat(ether, buf, n, offset);
else if(NETTYPE(chan->qid.path) == Nstatqid)
ether->ifstat(ether, buf, 0, offset);
}
return netifread(ether, chan, buf, n, offset);
}
static Block*
etherbread(Chan *c, long n, ulong offset)
{
return netifbread(etherxx[c->dev], c, n, offset);
}
static void
athtransmit(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
ctlr->pktstosend = 1;
wakeup(&ctlr->trendez);
iunlock(ctlr);
}
static long (*athctl)(Ether *, char *, int) = nil;
static int
etheroq(Ether* ether, Block* bp)
{
int len, loopback, s;
Etherpkt *pkt;
ether->outpackets++;
pkt = (Etherpkt*)bp->rp;
len = BLEN(bp);
loopback = memcmp(pkt->d, ether->ea, sizeof(pkt->d)) == 0;
if(loopback || memcmp(pkt->d, ether->bcast, sizeof(pkt->d)) == 0 || ether->prom){
s = splhi();
etheriq(ether, bp, 0);
splx(s);
}
if(!loopback){
if(qfull(ether->oq))
print("etheroq: WARNING: ether->oq full!\n");
qbwrite(ether->oq, bp);
athtransmit(ether);
} else
freeb(bp);
return len;
}
static long
etherwrite(Chan* chan, void* buf, long n, vlong)
{
Ether *ether;
Block *bp;
int nn, onoff;
Cmdbuf *cb;
ether = etherxx[chan->dev];
if(NETTYPE(chan->qid.path) != Ndataqid) {
nn = netifwrite(ether, chan, buf, n);
if(nn >= 0)
return nn;
cb = parsecmd(buf, n);
if(cb->f[0] && strcmp(cb->f[0], "nonblocking") == 0){
if(cb->nf <= 1)
onoff = 1;
else
onoff = atoi(cb->f[1]);
qnoblock(ether->oq, onoff);
free(cb);
return n;
}
free(cb);
if(athctl != nil)
return athctl(ether, buf, n);
error(Ebadctl);
}
assert(ether->ctlr != nil);
if(n > ether->mtu)
error(Etoobig);
if(n < ether->minmtu)
error(Etoosmall);
bp = allocb(n);
if(waserror()){
freeb(bp);
nexterror();
}
memmove(bp->rp, buf, n);
memmove(bp->rp+Eaddrlen, ether->ea, Eaddrlen);
poperror();
bp->wp += n;
return etheroq(ether, bp);
}
static long
etherbwrite(Chan *c, Block *bp, ulong offset)
{
return devbwrite(c, bp, offset);
}
static int
etherstat(Chan *c, uchar *dp, int n)
{
return netifstat(etherxx[c->dev], c, dp, n);
}
static int
etherwstat(Chan *c, uchar *dp, int n)
{
return netifwstat(etherxx[c->dev], c, dp, n);
}
Dev etherdevtab = {
'l',
"ether",
etherreset,
devinit,
ethershutdown,
etherattach,
etherwalk,
etherstat,
etheropen,
ethercreate,
etherclose,
etherread,
etherbread,
etherwrite,
etherbwrite,
devremove,
etherwstat,
devpower,
devconfig,
};