#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#include "ethermii.h"
#define iprint print
#define waserror()	(0)
#define poperror()
enum {
Cr		= 0x00,
Cfg		= 0x04,
Mear		= 0x08,
Ptscr		= 0x0C,
Isr		= 0x10,
Imr		= 0x14,
Ier		= 0x18,
Ihr		= 0x1C,
Txdp		= 0x20,
Txdphi		= 0x24,
Txcfg		= 0x28,
Gpior		= 0x2C,
Rxdp		= 0x30,
Rxdphi		= 0x34,
Rxcfg		= 0x38,
Pqcr		= 0x3C,
Wcsr		= 0x40,
Pcr		= 0x44,
Rfcr		= 0x48,
Rfdr		= 0x4C,
Brar		= 0x50,
Brdr		= 0x54,
Srr		= 0x58,
Mibc		= 0x5C,
Mibd		= 0x60,
Txdp1		= 0xA0,
Txdp2		= 0xA4,
Txdp3		= 0xA8,
Rxdp1		= 0xB0,
Rxdp2		= 0xB4,
Rxdp3		= 0xB8,
Vrcr		= 0xBC,
Vtcr		= 0xC0,
Vdr		= 0xC4,
Ccsr		= 0xCC,
Tbicr		= 0xE0,
Tbisr		= 0xE4,
Tanar		= 0xE8,
Tanlpar		= 0xEC,
Taner		= 0xF0,
Tesr		= 0xF4,
};
enum {
Txe		= 0x00000001,
Txd		= 0x00000002,
Rxe		= 0x00000004,
Rxd		= 0x00000008,
Txr		= 0x00000010,
Rxr		= 0x00000020,
Swien		= 0x00000080,
Rst		= 0x00000100,
TxpriSHFT	= 9,
TxpriMASK	= 0x00001E00,
RxpriSHFT	= 13,
RxpriMASK	= 0x0001E000,
};
enum {
Bem		= 0x00000001,
Ext125		= 0x00000002,
Bromdis		= 0x00000004,
Pesel		= 0x00000008,
Exd		= 0x00000010,
Pow		= 0x00000020,
Sb		= 0x00000040,
Reqalg		= 0x00000080,
Extstsen	= 0x00000100,
Phydis		= 0x00000200,
Phyrst		= 0x00000400,
M64addren	= 0x00000800,
Data64en	= 0x00001000,
Pci64det	= 0x00002000,
T64addren	= 0x00004000,
Mwidis		= 0x00008000,
Mrmdis		= 0x00010000,
Tmrtest		= 0x00020000,
Spdstsien	= 0x00040000,
Lnkstsien	= 0x00080000,
Dupstsien	= 0x00100000,
Mode1000	= 0x00400000,
Tbien		= 0x01000000,
Dupsts		= 0x10000000,
Spdsts100	= 0x20000000,
Spdsts1000	= 0x40000000,
Lnksts		= 0x80000000,
};
enum {
Eedi		= 0x00000001,
Eedo		= 0x00000002,
Eeclk		= 0x00000004,
Eesel		= 0x00000008,
Mdio		= 0x00000010,
Mddir		= 0x00000020,
Mdc		= 0x00000040,
};
enum {
Rxok		= 0x00000001,
Rxdesc		= 0x00000002,
Rxerr		= 0x00000004,
Rxearly		= 0x00000008,
Rxidle		= 0x00000010,
Rxorn		= 0x00000020,
Txok		= 0x00000040,
Txdesc		= 0x00000080,
Txerr		= 0x00000100,
Txidle		= 0x00000200,
Txurn		= 0x00000400,
Mib		= 0x00000800,
Swi		= 0x00001000,
Pme		= 0x00002000,
Phy		= 0x00004000,
Hibint		= 0x00008000,
Rxsovr		= 0x00010000,
Rtabt		= 0x00020000,
Rmabt		= 0x00040000,
Sserr		= 0x00080000,
Dperr		= 0x00100000,
Rxrcmp		= 0x00200000,
Txrcmp		= 0x00400000,
Rxdesc0		= 0x00800000,
Rxdesc1		= 0x01000000,
Rxdesc2		= 0x02000000,
Rxdesc3		= 0x04000000,
Txdesc0		= 0x08000000,
Txdesc1		= 0x10000000,
Txdesc2		= 0x20000000,
Txdesc3		= 0x40000000,
};
enum {
Ien		= 0x00000001,
};
enum {
IhSHFT		= 0,
IhMASK		= 0x000000FF,
Ihctl		= 0x00000100,
};
enum {
TxdrthSHFT	= 0,
TxdrthMASK	= 0x000000FF,
FlthSHFT	= 16,
FlthMASK	= 0x0000FF00,
Brstdis		= 0x00080000,
MxdmaSHFT	= 20,
MxdmaMASK	= 0x00700000,
Ecretryen	= 0x00800000,
Atp		= 0x10000000,
Mlb		= 0x20000000,
Hbi		= 0x40000000,
Csi		= 0x80000000,
};
enum {
RxdrthSHFT	= 1,
RxdrthMASK	= 0x0000003E,
Airl		= 0x04000000,
Alp		= 0x08000000,
Rxfd		= 0x10000000,
Stripcrc	= 0x20000000,
Arp		= 0x40000000,
Aep		= 0x80000000,
};
enum {
Txpqen		= 0x00000001,
Txfairen	= 0x00000002,
RxpqenSHFT	= 2,
RxpqenMASK	= 0x0000000C,
};
enum {
PscntSHFT	= 0,
PscntMASK	= 0x0000FFFF,
Pstx		= 0x00020000,
PsffloSHFT	= 18,
PsffloMASK	= 0x000C0000,
PsffhiSHFT	= 20,
PsffhiMASK	= 0x00300000,
PsstloSHFT	= 22,
PsstloMASK	= 0x00C00000,
PssthiSHFT	= 24,
PssthiMASK	= 0x03000000,
Psrcvd		= 0x08000000,
Psact		= 0x10000000,
Psda		= 0x20000000,
Psmcast		= 0x40000000,
Psen		= 0x80000000,
};
enum {
RfaddrSHFT	= 0,
RfaddrMASK	= 0x000003FF,
Ulm		= 0x00080000,
Uhen		= 0x00100000,
Mhen		= 0x00200000,
Aarp		= 0x00400000,
ApatSHFT	= 23,
ApatMASK	= 0x07800000,
Apm		= 0x08000000,
Aau		= 0x10000000,
Aam		= 0x20000000,
Aab		= 0x40000000,
Rfen		= 0x80000000,
};
enum {
RfdataSHFT	= 0,
RfdataMASK	= 0x0000FFFF,
BmaskSHFT	= 16,
BmaskMASK	= 0x00030000,
};
enum {
Wrn		= 0x00000001,
Frz		= 0x00000002,
Aclr		= 0x00000004,
Mibs		= 0x00000008,
};
enum {
Nmibd		= 11,
};
enum {
Vtden		= 0x00000001,
Vtren		= 0x00000002,
Dvtf		= 0x00000004,
Dutf		= 0x00000008,
Ipen		= 0x00000010,
Ripe		= 0x00000020,
Rtcpe		= 0x00000040,
Rudpe		= 0x00000080,
};
enum {
Vgti		= 0x00000001,
Vppti		= 0x00000002,
Gchk		= 0x00000004,
Ppchk		= 0x00000008,
};
enum {
VtypeSHFT	= 0,
VtypeMASK	= 0x0000FFFF,
VtciSHFT	= 16,
VtciMASK	= 0xFFFF0000,
};
enum {
Clkrunen	= 0x00000001,
Pmeen		= 0x00000100,
Pmests		= 0x00008000,
};
typedef struct {
u32int	link;
u32int	bufptr;
int	cmdsts;
int	extsts;
Block*	bp;
u32int	unused;
} Desc;
enum {
SizeMASK	= 0x0000FFFF,
SizeSHFT	= 0,
Ok		= 0x08000000,
Crc		= 0x10000000,
Intr		= 0x20000000,
More		= 0x40000000,
Own		= 0x80000000,
};
enum {
CcntMASK	= 0x000F0000,
CcntSHFT	= 16,
Ec		= 0x00100000,
Owc		= 0x00200000,
Ed		= 0x00400000,
Td		= 0x00800000,
Crs		= 0x01000000,
Tfu		= 0x02000000,
Txa		= 0x04000000,
};
enum {
Irl		= 0x00010000,
Lbp		= 0x00020000,
Fae		= 0x00040000,
Crce		= 0x00080000,
Ise		= 0x00100000,
Runt		= 0x00200000,
Long		= 0x00400000,
DestMASK	= 0x01800000,
DestSHFT	= 23,
Rxo		= 0x02000000,
Rxa		= 0x04000000,
};
enum {
EvtciMASK	= 0x0000FFFF,
EvtciSHFT	= 0,
Vpkt		= 0x00010000,
Ippkt		= 0x00020000,
Iperr		= 0x00040000,
Tcppkt		= 0x00080000,
Tcperr		= 0x00100000,
Udppkt		= 0x00200000,
Udperr		= 0x00400000,
};
enum {
Nrd		= 32,
Nrbf		= 4*Nrd,
Rbsz		= ROUNDUP(sizeof(Etherpkt)+8, 8),
Ntd		= 8,
};
typedef struct Ctlr Ctlr;
struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
int	active;
int	id;
int	eepromsz;
ushort*	eeprom;
int*	nic;
int	cfg;
int	imr;
Lock	alock;
Lock	ilock;
void*	alloc;
Mii*	mii;
Lock	rdlock;
Desc*	rd;
int	nrd;
int	nrb;
int	rdx;
int	rxcfg;
Lock	tlock;
Desc*	td;
int	ntd;
int	tdh;
int	tdt;
int	ntq;
int	txcfg;
int	rxidle;
uint	mibd[Nmibd];
int	ec;
int	owc;
int	ed;
int	crs;
int	tfu;
int	txa;
};
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static Ctlr* dp83820ctlrhead;
static Ctlr* dp83820ctlrtail;
static Lock dp83820rblock;
static Block* dp83820rbpool;
static char* dp83820mibs[Nmibd] = {
"RXErroredPkts",
"RXFCSErrors",
"RXMsdPktErrors",
"RXFAErrors",
"RXSymbolErrors",
"RXFrameToLong",
"RXIRLErrors",
"RXBadOpcodes",
"RXPauseFrames",
"TXPauseFrames",
"TXSQEErrors",
};
static int
mdior(Ctlr* ctlr, int n)
{
int data, i, mear, r;
mear = csr32r(ctlr, Mear);
r = ~(Mdc|Mddir) & mear;
data = 0;
for(i = n-1; i >= 0; i--){
if(csr32r(ctlr, Mear) & Mdio)
data |= (1<<i);
csr32w(ctlr, Mear, Mdc|r);
csr32w(ctlr, Mear, r);
}
csr32w(ctlr, Mear, mear);
return data;
}
static void
mdiow(Ctlr* ctlr, int bits, int n)
{
int i, mear, r;
mear = csr32r(ctlr, Mear);
r = Mddir|(~Mdc & mear);
for(i = n-1; i >= 0; i--){
if(bits & (1<<i))
r |= Mdio;
else
r &= ~Mdio;
csr32w(ctlr, Mear, r);
csr32w(ctlr, Mear, Mdc|r);
}
csr32w(ctlr, Mear, mear);
}
static int
dp83820miimir(Mii* mii, int pa, int ra)
{
int data;
Ctlr *ctlr;
ctlr = mii->ctlr;
mdiow(ctlr, 0xFFFFFFFF, 32);
mdiow(ctlr, 0x1800|(pa<<5)|ra, 14);
data = mdior(ctlr, 18);
if(data & 0x10000)
return -1;
return data & 0xFFFF;
}
static int
dp83820miimiw(Mii* mii, int pa, int ra, int data)
{
Ctlr *ctlr;
ctlr = mii->ctlr;
mdiow(ctlr, 0xFFFFFFFF, 32);
data &= 0xFFFF;
data |= (0x05<<(5+5+2+16))|(pa<<(5+2+16))|(ra<<(2+16))|(0x02<<16);
mdiow(ctlr, data, 32);
return 0;
}
static Block *
dp83820rballoc(Desc* desc)
{
Block *bp;
if(desc->bp == nil){
ilock(&dp83820rblock);
if((bp = dp83820rbpool) == nil){
iunlock(&dp83820rblock);
desc->bp = nil;
desc->cmdsts = Own;
return nil;
}
dp83820rbpool = bp->next;
bp->next = nil;
iunlock(&dp83820rblock);
desc->bufptr = PCIWADDR(bp->rp);
desc->bp = bp;
}
else{
bp = desc->bp;
bp->rp = bp->lim - Rbsz;
bp->wp = bp->rp;
}
coherence();
desc->cmdsts = Intr|Rbsz;
return bp;
}
static void
dp83820rbfree(Block *bp)
{
bp->rp = bp->lim - Rbsz;
bp->wp = bp->rp;
ilock(&dp83820rblock);
bp->next = dp83820rbpool;
dp83820rbpool = bp;
iunlock(&dp83820rblock);
}
static void
dp83820halt(Ctlr* ctlr)
{
int i, timeo;
ilock(&ctlr->ilock);
csr32w(ctlr, Imr, 0);
csr32w(ctlr, Ier, 0);
csr32w(ctlr, Cr, Rxd|Txd);
for(timeo = 0; timeo < 1000; timeo++){
if(!(csr32r(ctlr, Cr) & (Rxe|Txe)))
break;
microdelay(1);
}
csr32w(ctlr, Mibc, Frz);
iunlock(&ctlr->ilock);
if(ctlr->rd != nil){
for(i = 0; i < ctlr->nrd; i++){
if(ctlr->rd[i].bp == nil)
continue;
freeb(ctlr->rd[i].bp);
ctlr->rd[i].bp = nil;
}
}
if(ctlr->td != nil){
for(i = 0; i < ctlr->ntd; i++){
if(ctlr->td[i].bp == nil)
continue;
freeb(ctlr->td[i].bp);
ctlr->td[i].bp = nil;
}
}
}
static void
dp83820cfg(Ctlr* ctlr)
{
int cfg;
if(ctlr->mii == nil)
return;
cfg = csr32r(ctlr, Cfg);
if(!(cfg & Dupsts)){
ctlr->rxcfg |= Rxfd;
ctlr->txcfg |= Csi|Hbi;
iprint("83820: full duplex, ");
}
else{
ctlr->rxcfg &= ~Rxfd;
ctlr->txcfg &= ~(Csi|Hbi);
iprint("83820: half duplex, ");
}
csr32w(ctlr, Rxcfg, ctlr->rxcfg);
csr32w(ctlr, Txcfg, ctlr->txcfg);
switch(cfg & (Spdsts1000|Spdsts100)){
case Spdsts1000:
default:
ctlr->cfg &= ~Mode1000;
if((cfg & (Spdsts1000|Spdsts100)) == Spdsts1000)
iprint("100Mb/s\n");
else
iprint("10Mb/s\n");
break;
case Spdsts100:
ctlr->cfg |= Mode1000;
iprint("1Gb/s\n");
break;
}
csr32w(ctlr, Cfg, ctlr->cfg);
}
static void
dp83820init(Ether* edev)
{
int i;
Ctlr *ctlr;
Desc *desc;
uchar *alloc;
ctlr = edev->ctlr;
dp83820halt(ctlr);
alloc = (uchar*)ROUNDUP((ulong)ctlr->alloc, 8);
ctlr->rd = (Desc*)alloc;
alloc += ctlr->nrd*sizeof(Desc);
memset(ctlr->rd, 0, ctlr->nrd*sizeof(Desc));
ctlr->rdx = 0;
for(i = 0; i < ctlr->nrd; i++){
desc = &ctlr->rd[i];
desc->link = PCIWADDR(&ctlr->rd[NEXT(i, ctlr->nrd)]);
if(dp83820rballoc(desc) == nil)
continue;
}
csr32w(ctlr, Rxdphi, 0);
csr32w(ctlr, Rxdp, PCIWADDR(ctlr->rd));
for(i = 0; i < Eaddrlen; i += 2){
csr32w(ctlr, Rfcr, i);
csr32w(ctlr, Rfdr, (edev->ea[i+1]<<8)|edev->ea[i]);
}
csr32w(ctlr, Rfcr, Rfen|Aab|Aam|Apm);
ctlr->rxcfg = Stripcrc|(((2*(ETHERMINTU+4))/8)<<RxdrthSHFT);
ctlr->imr |= Rxorn|Rxidle|Rxearly|Rxdesc|Rxok;
ctlr->td = (Desc*)alloc;
memset(ctlr->td, 0, ctlr->ntd*sizeof(Desc));
ctlr->tdh = ctlr->tdt = ctlr->ntq = 0;
for(i = 0; i < ctlr->ntd; i++){
desc = &ctlr->td[i];
desc->link = PCIWADDR(&ctlr->td[NEXT(i, ctlr->ntd)]);
}
csr32w(ctlr, Txdphi, 0);
csr32w(ctlr, Txdp, PCIWADDR(ctlr->td));
ctlr->txcfg = Atp|(((2*(ETHERMINTU+4))/32)<<FlthSHFT)|((4096/32)<<TxdrthSHFT);
ctlr->imr |= Txurn|Txidle|Txdesc|Txok;
ilock(&ctlr->ilock);
dp83820cfg(ctlr);
csr32w(ctlr, Mibc, Aclr);
ctlr->imr |= Mib;
csr32w(ctlr, Imr, ctlr->imr);
csr32w(ctlr, Ihr, Ihctl|(1<<IhSHFT));
csr32w(ctlr, Ier, Ien);
csr32w(ctlr, Cr, Rxe|Txe);
iunlock(&ctlr->ilock);
}
static void
dp83820attach(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
ctlr = edev->ctlr;
lock(&ctlr->alock);
if(ctlr->alloc != nil){
unlock(&ctlr->alock);
return;
}
if(waserror()){
err:
if(ctlr->mii != nil){
free(ctlr->mii);
ctlr->mii = nil;
}
if(ctlr->alloc != nil){
free(ctlr->alloc);
ctlr->alloc = nil;
}
unlock(&ctlr->alock);
return;
}
if(!(ctlr->cfg & Tbien)){
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
goto err;
ctlr->mii->ctlr = ctlr;
ctlr->mii->mir = dp83820miimir;
ctlr->mii->miw = dp83820miimiw;
if(mii(ctlr->mii, ~0) == 0)
goto err;
ctlr->cfg |= Dupstsien|Lnkstsien|Spdstsien;
ctlr->imr |= Phy;
}
ctlr->nrd = Nrd;
ctlr->nrb = Nrbf;
ctlr->ntd = Ntd;
ctlr->alloc = mallocz((ctlr->nrd+ctlr->ntd)*sizeof(Desc) + 7, 0);
if(ctlr->alloc == nil)
goto err;
for(ctlr->nrb = 0; ctlr->nrb < Nrbf; ctlr->nrb++){
if((bp = allocb(Rbsz+8-1)) == nil)
break;
bp->rp += 8 - (uintptr)bp->rp % 8;
bp->wp = bp->rp;
dp83820rbfree(bp);
}
dp83820init(edev);
unlock(&ctlr->alock);
poperror();
}
static void
freeblist(Block *b)
{
Block *next;
for(; b != 0; b = next){
next = b->next;
b->next = 0;
freeb(b);
}
}
static void
toringbuf(Ether *ether, Block *bp)
{
RingBuf *rb = &ether->rb[ether->ri];
if (rb->owner == Interface) {
rb->len = BLEN(bp);
memmove(rb->pkt, bp->rp, rb->len);
rb->owner = Host;
ether->ri = NEXT(ether->ri, ether->nrb);
}
}
static Block *
fromringbuf(Ether *ether)
{
RingBuf *tb = &ether->tb[ether->ti];
Block *bp = allocb(tb->len);
memmove(bp->wp, tb->pkt, tb->len);
memmove(bp->wp+Eaddrlen, ether->ea, Eaddrlen);
bp->wp += tb->len;
return bp;
}
static void
dp83820transmit(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
Desc *desc;
RingBuf *tb;
int cmdsts, r, x;
ctlr = edev->ctlr;
ilock(&ctlr->tlock);
bp = nil;
for(x = ctlr->tdh; ctlr->ntq; x = NEXT(x, ctlr->ntd)){
desc = &ctlr->td[x];
if((cmdsts = desc->cmdsts) & Own)
break;
if(!(cmdsts & Ok)){
if(cmdsts & Ec)
ctlr->ec++;
if(cmdsts & Owc)
ctlr->owc++;
if(cmdsts & Ed)
ctlr->ed++;
if(cmdsts & Crs)
ctlr->crs++;
if(cmdsts & Tfu)
ctlr->tfu++;
if(cmdsts & Txa)
ctlr->txa++;
}
desc->bp->next = bp;
bp = desc->bp;
desc->bp = nil;
ctlr->ntq--;
}
ctlr->tdh = x;
if(bp != nil)
freeblist(bp);
x = ctlr->tdt;
while(ctlr->ntq < ctlr->ntd - 1  ){
bp = fromringbuf(edev);
if (bp == nil)
break;
desc = &ctlr->td[x];
desc->bufptr = PCIWADDR(bp->rp);
desc->bp = bp;
ctlr->ntq++;
coherence();
desc->cmdsts = Own|Intr|BLEN(bp);
tb = &edev->tb[edev->ti];
tb->owner = Host;
edev->ti = NEXT(edev->ti, edev->ntb);
x = NEXT(x, ctlr->ntd);
}
if(x != ctlr->tdt){
ctlr->tdt = x;
r = csr32r(ctlr, Cr);
csr32w(ctlr, Cr, Txe|r);
}
iunlock(&ctlr->tlock);
}
static void
dp83820interrupt(Ureg*, void* arg)
{
Block *bp;
Ctlr *ctlr;
Desc *desc;
Ether *edev;
int cmdsts, i, isr, r, x;
edev = arg;
ctlr = edev->ctlr;
for(isr = csr32r(ctlr, Isr); isr & ctlr->imr; isr = csr32r(ctlr, Isr)){
if(isr & (Rxorn|Rxidle|Rxearly|Rxerr|Rxdesc|Rxok)){
x = ctlr->rdx;
desc = &ctlr->rd[x];
while((cmdsts = desc->cmdsts) & Own){
if((cmdsts & Ok) && desc->bp != nil){
bp = desc->bp;
desc->bp = nil;
bp->wp += cmdsts & SizeMASK;
toringbuf(edev, bp);
}
dp83820rballoc(desc);
x = NEXT(x, ctlr->nrd);
desc = &ctlr->rd[x];
}
ctlr->rdx = x;
if(isr & Rxidle){
r = csr32r(ctlr, Cr);
csr32w(ctlr, Cr, Rxe|r);
ctlr->rxidle++;
}
isr &= ~(Rxorn|Rxidle|Rxearly|Rxerr|Rxdesc|Rxok);
}
if(isr & Txurn){
x = (ctlr->txcfg & TxdrthMASK)>>TxdrthSHFT;
r = (ctlr->txcfg & FlthMASK)>>FlthSHFT;
if(x < ((TxdrthMASK)>>TxdrthSHFT)
&& x < (2048/32 - r)){
ctlr->txcfg &= ~TxdrthMASK;
x++;
ctlr->txcfg |= x<<TxdrthSHFT;
csr32w(ctlr, Txcfg, ctlr->txcfg);
}
}
if(isr & (Txurn|Txidle|Txdesc|Txok)){
dp83820transmit(edev);
isr &= ~(Txurn|Txidle|Txdesc|Txok);
}
if(isr & Mib){
for(i = 0; i < Nmibd; i++){
r = csr32r(ctlr, Mibd+(i*sizeof(int)));
ctlr->mibd[i] += r & 0xFFFF;
}
isr &= ~Mib;
}
if((isr & Phy) && ctlr->mii != nil){
ctlr->mii->mir(ctlr->mii, 1, Bmsr);
print("phy: cfg %8.8uX bmsr %4.4uX\n",
csr32r(ctlr, Cfg),
ctlr->mii->mir(ctlr->mii, 1, Bmsr));
dp83820cfg(ctlr);
isr &= ~Phy;
}
USED(isr);
}
}
static int
dp83820detach(Ctlr* ctlr)
{
csr32w(ctlr, Cr, Rst);
delay(1);
while(csr32r(ctlr, Cr) & Rst)
delay(1);
return 0;
}
static void
dp83820shutdown(Ether* ether)
{
print("dp83820shutdown\n");
dp83820detach(ether->ctlr);
}
static int
atc93c46r(Ctlr* ctlr, int address)
{
int data, i, mear, r, size;
mear = csr32r(ctlr, Mear);
mear &= ~(Eesel|Eeclk|Eedo|Eedi);
r = Eesel|mear;
reread:
csr32w(ctlr, Mear, r);
data = 0x06;
for(i = 3-1; i >= 0; i--){
if(data & (1<<i))
r |= Eedi;
else
r &= ~Eedi;
csr32w(ctlr, Mear, r);
csr32w(ctlr, Mear, Eeclk|r);
microdelay(1);
csr32w(ctlr, Mear, r);
microdelay(1);
}
if((size = ctlr->eepromsz) == 0)
size = 8;
for(size = size-1; size >= 0; size--){
if(address & (1<<size))
r |= Eedi;
else
r &= ~Eedi;
csr32w(ctlr, Mear, r);
microdelay(1);
csr32w(ctlr, Mear, Eeclk|r);
microdelay(1);
csr32w(ctlr, Mear, r);
microdelay(1);
if(!(csr32r(ctlr, Mear) & Eedo))
break;
}
r &= ~Eedi;
data = 0;
for(i = 16-1; i >= 0; i--){
csr32w(ctlr, Mear, Eeclk|r);
microdelay(1);
if(csr32r(ctlr, Mear) & Eedo)
data |= (1<<i);
csr32w(ctlr, Mear, r);
microdelay(1);
}
csr32w(ctlr, Mear, mear);
if(ctlr->eepromsz == 0){
ctlr->eepromsz = 8-size;
ctlr->eeprom = malloc((1<<ctlr->eepromsz)*sizeof(ushort));
goto reread;
}
return data;
}
static int
dp83820reset(Ctlr* ctlr)
{
int i, r;
unsigned char sum;
csr32w(ctlr, Cr, Rst);
delay(1);
while(csr32r(ctlr, Cr) & Rst)
delay(1);
atc93c46r(ctlr, 0);
if(ctlr->eeprom == nil) {
print("dp83820reset: no eeprom\n");
return -1;
}
sum = 0;
for(i = 0; i < 0x0E; i++){
r = atc93c46r(ctlr, i);
ctlr->eeprom[i] = r;
sum += r;
sum += r>>8;
}
if(sum != 0){
print("dp83820reset: bad EEPROM checksum\n");
return -1;
}
#ifdef notdef
csr32w(ctlr, Gpior, ctlr->eeprom[4]);
cfg = Extstsen|Exd;
r = csr32r(ctlr, Cfg);
if(ctlr->eeprom[5] & 0x0001)
cfg |= Ext125;
if(ctlr->eeprom[5] & 0x0002)
cfg |= M64addren;
if((ctlr->eeprom[5] & 0x0004) && (r & Pci64det))
cfg |= Data64en;
if(ctlr->eeprom[5] & 0x0008)
cfg |= T64addren;
if(!(pcicfgr16(ctlr->pcidev, PciPCR) & 0x10))
cfg |= Mwidis;
if(ctlr->eeprom[5] & 0x0020)
cfg |= Mrmdis;
if(ctlr->eeprom[5] & 0x0080)
cfg |= Mode1000;
if(ctlr->eeprom[5] & 0x0200)
cfg |= Tbien|Mode1000;
#else
#endif
ctlr->cfg = csr32r(ctlr, Cfg);
print("cfg %8.8uX pcicfg %8.8uX\n", ctlr->cfg, pcicfgr32(ctlr->pcidev, PciPCR));
ctlr->cfg &= ~(T64addren|Data64en|M64addren);
csr32w(ctlr, Cfg, ctlr->cfg);
csr32w(ctlr, Mibc, Aclr|Frz);
return 0;
}
static void
dp83820pci(void)
{
int port;
Pcidev *p;
Ctlr *ctlr;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
default:
continue;
case (0x0022<<16)|0x100B:
break;
}
port = upamalloc(p->mem[1].bar & ~0x0F, p->mem[1].size, 0);
if(port == 0){
print("dp83820: can't map %d @ 0x%8.8luX\n",
p->mem[1].size, p->mem[1].bar);
continue;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = port;
ctlr->pcidev = p;
ctlr->id = p->did<<16 | p->vid;
ctlr->nic = KADDR(ctlr->port);
if(dp83820reset(ctlr)){
free(ctlr);
continue;
}
pcisetbme(p);
if(dp83820ctlrhead != nil)
dp83820ctlrtail->next = ctlr;
else
dp83820ctlrhead = ctlr;
dp83820ctlrtail = ctlr;
}
}
int
dp83820pnp(Ether* edev)
{
int i;
Ctlr *ctlr;
uchar ea[Eaddrlen];
if(dp83820ctlrhead == nil)
dp83820pci();
for(ctlr = dp83820ctlrhead; ctlr != nil; ctlr = ctlr->next){
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
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0)
for(i = 0; i < Eaddrlen/2; i++){
edev->ea[2*i] = ctlr->eeprom[0x0C-i];
edev->ea[2*i+1] = ctlr->eeprom[0x0C-i]>>8;
}
edev->attach = dp83820attach;
edev->transmit = dp83820transmit;
edev->interrupt = dp83820interrupt;
edev->detach = dp83820shutdown;
return 0;
}