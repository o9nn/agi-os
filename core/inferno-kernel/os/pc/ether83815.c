#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
#define DEBUG		(0)
#define debug		if(DEBUG)print
enum {
Nrde		= 64,
Ntde		= 64,
};
#define Rbsz		ROUNDUP(sizeof(Etherpkt)+4, 4)
typedef struct Des {
ulong	next;
int	cmdsts;
ulong	addr;
Block*	bp;
} Des;
enum {
Own	= 1<<31,
More	= 1<<30,
Intr	= 1<<29,
Supcrc	= 1<<28,
Inccrc	= 1<<28,
Ok	= 1<<27,
Size	= 0xFFF,
Txa	= 1<<26,
Tfu	= 1<<25,
Crs	= 1<<24,
Td	= 1<<23,
Ed	= 1<<22,
Owc	= 1<<21,
Ec	= 1<<20,
Rxa	= 1<<26,
Rxo	= 1<<25,
Dest	= 3<<23,
Drej=	0<<23,
Duni=	1<<23,
Dmulti=	2<<23,
Dbroad=	3<<23,
Long = 1<<22,
Runt =  1<<21,
Ise =	1<<20,
Crce =	1<<19,
Fae =	1<<18,
Lbp =	1<<17,
Col =	1<<16,
};
enum {
Nat83815	= (0x0020<<16)|0x100B,
SiS = 	0x1039,
SiS900 =	(0x0900<<16)|SiS,
SiS7016 =	(0x7016<<16)|SiS,
SiS630bridge	= 0x0008,
SiSrev630s =	0x81,
SiSrev630e =	0x82,
SiSrev630ea1 =	0x83,
SiSeenodeaddr =	8,
SiS630eenodeaddr =	9,
Nseenodeaddr =	6,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
int	active;
int	id;
ushort	srom[0xB+1];
uchar	sromea[Eaddrlen];
uchar	fd;
int	mbps;
Lock	lock;
Des*	rdr;
int	nrdr;
int	rdrx;
Lock	tlock;
Des*	tdr;
int	ntdr;
int	tdrh;
int	tdri;
int	ntq;
int	ntqmax;
ulong	rxa;
ulong	rxo;
ulong	rlong;
ulong	runt;
ulong	ise;
ulong	crce;
ulong	fae;
ulong	lbp;
ulong	col;
ulong	rxsovr;
ulong	rxorn;
ulong	txa;
ulong	tfu;
ulong	crs;
ulong	td;
ulong	ed;
ulong	owc;
ulong	ec;
ulong	txurn;
ulong	dperr;
ulong	rmabt;
ulong	rtabt;
ulong	sserr;
ulong	rxsover;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
enum {
Rcr=	0x00,
Rst=		1<<8,
Rxr=		1<<5,
Txr=		1<<4,
Rxd=		1<<3,
Rxe=		1<<2,
Txd=		1<<1,
Txe=		1<<0,
Rcfg=	0x04,
Lnksts=	1<<31,
Speed100=	1<<30,
Fdup=		1<<29,
Pol=		1<<28,
Aneg_dn=	1<<27,
Pint_acen=	1<<17,
Pause_adv=	1<<16,
Paneg_ena=	1<<13,
Paneg_all=	7<<13,
Ext_phy=	1<<12,
Phy_rst=	1<<10,
Phy_dis=	1<<9,
Req_alg=	1<<7,
Sb=		1<<6,
Pow=		1<<5,
Exd=		1<<4,
Pesel=	1<<3,
Brom_dis=	1<<2,
Bem=		1<<0,
Rmear=	0x08,
Mdc=		1<<6,
Mddir=	1<<5,
Mdio=		1<<4,
Eesel=	1<<3,
Eeclk=	1<<2,
Eedo=		1<<1,
Eedi=		1<<0,
Rptscr=	0x0C,
Risr=	0x10,
Txrcmp=	1<<25,
Rxrcmp=	1<<24,
Dperr=	1<<23,
Sserr=	1<<22,
Rmabt=	1<<21,
Rtabt=	1<<20,
Rxsovr=	1<<16,
Hiberr=	1<<15,
Phy=		1<<14,
Pme=		1<<13,
Swi=		1<<12,
Mib=		1<<11,
Txurn=	1<<10,
Txidle=	1<<9,
Txerr=	1<<8,
Txdesc=	1<<7,
Txok=		1<<6,
Rxorn=	1<<5,
Rxidle=	1<<4,
Rxearly=	1<<3,
Rxerr=	1<<2,
Rxdesc=	1<<1,
Rxok=		1<<0,
Rimr=	0x14,
Rier=	0x18,
Ie=		1<<0,
Rtxdp=	0x20,
Rtxcfg=	0x24,
Csi=		1<<31,
Hbi=		1<<30,
Atp=		1<<28,
Mxdma=	7<<20,
Mxdma32=	4<<20,
Mxdma64=	5<<20,
Flth=		0x3F<<8,
Drth=		0x3F<<0,
Flth128=	4<<8,
Drth512=	16<<0,
Rrxdp=	0x30,
Rrxcfg=	0x34,
Atx=		1<<28,
Rdrth=	0x1F<<1,
Rdrth64=	2<<1,
Rccsr=	0x3C,
Pmests=	1<<15,
Rwcsr=	0x40,
Rpcr=	0x44,
Rrfcr=	0x48,
Rfen=		1<<31,
Aab=		1<<30,
Aam=		1<<29,
Aau=		1<<28,
Apm=		1<<27,
Apat=		0xF<<23,
Aarp=		1<<22,
Mhen=		1<<21,
Uhen=		1<<20,
Ulm=		1<<19,
Rrfdr=	0x4C,
Rbrar=	0x50,
Rbrdr=	0x54,
Rsrr=	0x58,
Rmibc=	0x5C,
Rbmcr=	0x80,
Reset=	1<<15,
Sel100=	1<<13,
Anena=	1<<12,
Anrestart=	1<<9,
Selfdx=	1<<8,
Rbmsr=	0x84,
Ancomp=	1<<5,
Rphyidr1= 0x88,
Rphyidr2= 0x8C,
Ranar=	0x90,
Ranlpar= 0x94,
Raner=	0x98,
Rannptr= 0x9C,
Rphysts= 0xC0,
Rmicr=	0xC4,
Inten=	1<<1,
Rmisr=	0xC8,
Rfcscr=	0xD0,
Rrecr=	0xD4,
Rpcsr=	0xD8,
Rphycr=	0xE4,
Rtbscr=	0xE8,
};
#define csr32r(c, r)	(inl((c)->port+(r)))
#define csr32w(c, r, l)	(outl((c)->port+(r), (ulong)(l)))
#define csr16r(c, r)	(ins((c)->port+(r)))
#define csr16w(c, r, l)	(outs((c)->port+(r), (ulong)(l)))
static void
dumpcregs(Ctlr *ctlr)
{
int i;
for(i=0; i<=0x5C; i+=4)
print("%2.2ux %8.8lux\n", i, csr32r(ctlr, i));
}
static void
promiscuous(void* arg, int on)
{
Ctlr *ctlr;
ulong w;
ctlr = ((Ether*)arg)->ctlr;
ilock(&ctlr->lock);
w = csr32r(ctlr, Rrfcr);
if(on != ((w&Aau)!=0)){
csr32w(ctlr, Rrfcr, w & ~Rfen);
csr32w(ctlr, Rrfcr, Rfen | (w ^ Aau));
}
iunlock(&ctlr->lock);
}
static void
attach(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(&ctlr->lock);
if(0)
dumpcregs(ctlr);
csr32w(ctlr, Rcr, Rxe);
iunlock(&ctlr->lock);
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *buf, *p;
int i, l, len;
ctlr = ether->ctlr;
ether->crcs = ctlr->crce;
ether->frames = ctlr->runt+ctlr->ise+ctlr->rlong+ctlr->fae;
ether->buffs = ctlr->rxorn+ctlr->tfu;
ether->overflows = ctlr->rxsovr;
if(n == 0)
return 0;
p = malloc(READSTR);
l = snprint(p, READSTR, "Rxa: %lud\n", ctlr->rxa);
l += snprint(p+l, READSTR-l, "Rxo: %lud\n", ctlr->rxo);
l += snprint(p+l, READSTR-l, "Rlong: %lud\n", ctlr->rlong);
l += snprint(p+l, READSTR-l, "Runt: %lud\n", ctlr->runt);
l += snprint(p+l, READSTR-l, "Ise: %lud\n", ctlr->ise);
l += snprint(p+l, READSTR-l, "Fae: %lud\n", ctlr->fae);
l += snprint(p+l, READSTR-l, "Lbp: %lud\n", ctlr->lbp);
l += snprint(p+l, READSTR-l, "Tfu: %lud\n", ctlr->tfu);
l += snprint(p+l, READSTR-l, "Txa: %lud\n", ctlr->txa);
l += snprint(p+l, READSTR-l, "CRC Error: %lud\n", ctlr->crce);
l += snprint(p+l, READSTR-l, "Collision Seen: %lud\n", ctlr->col);
l += snprint(p+l, READSTR-l, "Frame Too Long: %lud\n", ctlr->rlong);
l += snprint(p+l, READSTR-l, "Runt Frame: %lud\n", ctlr->runt);
l += snprint(p+l, READSTR-l, "Rx Underflow Error: %lud\n", ctlr->rxorn);
l += snprint(p+l, READSTR-l, "Tx Underrun: %lud\n", ctlr->txurn);
l += snprint(p+l, READSTR-l, "Excessive Collisions: %lud\n", ctlr->ec);
l += snprint(p+l, READSTR-l, "Late Collision: %lud\n", ctlr->owc);
l += snprint(p+l, READSTR-l, "Loss of Carrier: %lud\n", ctlr->crs);
l += snprint(p+l, READSTR-l, "Parity: %lud\n", ctlr->dperr);
l += snprint(p+l, READSTR-l, "Aborts: %lud\n", ctlr->rmabt+ctlr->rtabt);
l += snprint(p+l, READSTR-l, "RX Status overrun: %lud\n", ctlr->rxsover);
snprint(p+l, READSTR-l, "ntqmax: %d\n", ctlr->ntqmax);
ctlr->ntqmax = 0;
buf = a;
len = readstr(offset, buf, n, p);
if(offset > l)
offset -= l;
else
offset = 0;
buf += len;
n -= len;
l = snprint(p, READSTR, "srom:");
for(i = 0; i < nelem(ctlr->srom); i++){
if(i && ((i & 0x0F) == 0))
l += snprint(p+l, READSTR-l, "\n     ");
l += snprint(p+l, READSTR-l, " %4.4uX", ctlr->srom[i]);
}
snprint(p+l, READSTR-l, "\n");
len += readstr(offset, buf, n, p);
free(p);
return len;
}
static void
txstart(Ether* ether)
{
Ctlr *ctlr;
Block *bp;
Des *des;
int started;
ctlr = ether->ctlr;
started = 0;
while(ctlr->ntq < ctlr->ntdr-1){
bp = qget(ether->oq);
if(bp == nil)
break;
des = &ctlr->tdr[ctlr->tdrh];
des->bp = bp;
des->addr = PADDR(bp->rp);
ctlr->ntq++;
coherence();
des->cmdsts = Own | BLEN(bp);
ctlr->tdrh = NEXT(ctlr->tdrh, ctlr->ntdr);
started = 1;
}
if(started){
coherence();
csr32w(ctlr, Rcr, Txe);
}
if(ctlr->ntq > ctlr->ntqmax)
ctlr->ntqmax = ctlr->ntq;
}
static void
transmit(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(&ctlr->tlock);
txstart(ether);
iunlock(&ctlr->tlock);
}
static void
txrxcfg(Ctlr *ctlr, int txdrth)
{
ulong rx, tx;
rx = csr32r(ctlr, Rrxcfg);
tx = csr32r(ctlr, Rtxcfg);
if(ctlr->fd){
rx |= Atx;
tx |= Csi | Hbi;
}else{
rx &= ~Atx;
tx &= ~(Csi | Hbi);
}
tx &= ~(Mxdma|Drth|Flth);
tx |= Mxdma64 | Flth128 | txdrth;
csr32w(ctlr, Rtxcfg, tx);
rx &= ~(Mxdma|Rdrth);
rx |= Mxdma64 | Rdrth64;
csr32w(ctlr, Rrxcfg, rx);
}
static void
interrupt(Ureg*, void* arg)
{
int len, status, cmdsts, n;
Ctlr *ctlr;
Ether *ether;
Des *des;
Block *bp;
ether = arg;
ctlr = ether->ctlr;
while((status = csr32r(ctlr, Risr)) != 0){
status &= ~(Pme|Mib);
if(status & Hiberr){
if(status & Rxsovr)
ctlr->rxsover++;
if(status & Sserr)
ctlr->sserr++;
if(status & Dperr)
ctlr->dperr++;
if(status & Rmabt)
ctlr->rmabt++;
if(status & Rtabt)
ctlr->rtabt++;
status &= ~(Hiberr|Txrcmp|Rxrcmp|Rxsovr|Dperr|Sserr|Rmabt|Rtabt);
}
if(status&Phy){
status &= ~Phy;
csr32r(ctlr, Rcfg);
n = csr32r(ctlr, Rcfg);
ether->link = (n&Lnksts) != 0;
}
if(status & (Rxdesc|Rxok|Rxerr|Rxearly|Rxorn)){
des = &ctlr->rdr[ctlr->rdrx];
while((cmdsts = des->cmdsts) & Own){
if((cmdsts&Ok) == 0){
if(cmdsts & Rxa)
ctlr->rxa++;
if(cmdsts & Rxo)
ctlr->rxo++;
if(cmdsts & Long)
ctlr->rlong++;
if(cmdsts & Runt)
ctlr->runt++;
if(cmdsts & Ise)
ctlr->ise++;
if(cmdsts & Crce)
ctlr->crce++;
if(cmdsts & Fae)
ctlr->fae++;
if(cmdsts & Lbp)
ctlr->lbp++;
if(cmdsts & Col)
ctlr->col++;
}
else if(bp = iallocb(Rbsz)){
len = (cmdsts&Size)-4;
if(len <= 0){
debug("ns83815: packet len %d <=0\n", len);
freeb(des->bp);
}else{
des->bp->wp = des->bp->rp+len;
etheriq(ether, des->bp, 1);
}
des->bp = bp;
des->addr = PADDR(bp->rp);
coherence();
}else{
debug("ns83815: interrupt: iallocb for input buffer failed\n");
des->bp->next = 0;
}
des->cmdsts = Rbsz;
coherence();
ctlr->rdrx = NEXT(ctlr->rdrx, ctlr->nrdr);
des = &ctlr->rdr[ctlr->rdrx];
}
status &= ~(Rxdesc|Rxok|Rxerr|Rxearly|Rxorn);
}
if(status & Txurn){
ctlr->txurn++;
ilock(&ctlr->lock);
iunlock(&ctlr->lock);
status &= ~(Txurn);
}
ilock(&ctlr->tlock);
while(ctlr->ntq){
des = &ctlr->tdr[ctlr->tdri];
cmdsts = des->cmdsts;
if(cmdsts & Own)
break;
if((cmdsts & Ok) == 0){
if(cmdsts & Txa)
ctlr->txa++;
if(cmdsts & Tfu)
ctlr->tfu++;
if(cmdsts & Td)
ctlr->td++;
if(cmdsts & Ed)
ctlr->ed++;
if(cmdsts & Owc)
ctlr->owc++;
if(cmdsts & Ec)
ctlr->ec++;
ether->oerrs++;
}
freeb(des->bp);
des->bp = nil;
des->cmdsts = 0;
ctlr->ntq--;
ctlr->tdri = NEXT(ctlr->tdri, ctlr->ntdr);
}
txstart(ether);
iunlock(&ctlr->tlock);
status &= ~(Txurn|Txidle|Txerr|Txdesc|Txok);
if(status)
print("#l%d: status %8.8uX\n", ether->ctlrno, status);
}
}
static void
ctlrinit(Ether* ether)
{
Ctlr *ctlr;
Des *des, *last;
ctlr = ether->ctlr;
des = xspanalloc((ctlr->nrdr+ctlr->ntdr)*sizeof(Des), 32, 0);
ctlr->tdr = des;
ctlr->rdr = des+ctlr->ntdr;
last = nil;
for(des = ctlr->rdr; des < &ctlr->rdr[ctlr->nrdr]; des++){
des->bp = iallocb(Rbsz);
if(des->bp == nil)
error(Enomem);
des->cmdsts = Rbsz;
des->addr = PADDR(des->bp->rp);
if(last != nil)
last->next = PADDR(des);
last = des;
}
ctlr->rdr[ctlr->nrdr-1].next = PADDR(ctlr->rdr);
ctlr->rdrx = 0;
csr32w(ctlr, Rrxdp, PADDR(ctlr->rdr));
last = nil;
for(des = ctlr->tdr; des < &ctlr->tdr[ctlr->ntdr]; des++){
des->cmdsts = 0;
des->bp = nil;
des->addr = ~0;
if(last != nil)
last->next = PADDR(des);
last = des;
}
ctlr->tdr[ctlr->ntdr-1].next = PADDR(ctlr->tdr);
ctlr->tdrh = 0;
ctlr->tdri = 0;
csr32w(ctlr, Rtxdp, PADDR(ctlr->tdr));
txrxcfg(ctlr, Drth512);
csr32w(ctlr, Rimr, Dperr|Sserr|Rmabt|Rtabt|Rxsovr|Hiberr|Txurn|Txerr|
Txdesc|Txok|Rxorn|Rxerr|Rxdesc|Rxok);
csr32w(ctlr, Rmicr, Inten);
csr32r(ctlr, Risr);
csr32w(ctlr, Rier, Ie);
}
static void
eeclk(Ctlr *ctlr, int clk)
{
csr32w(ctlr, Rmear, Eesel | clk);
microdelay(2);
}
static void
eeidle(Ctlr *ctlr)
{
int i;
eeclk(ctlr, 0);
eeclk(ctlr, Eeclk);
for(i=0; i<25; i++){
eeclk(ctlr, 0);
eeclk(ctlr, Eeclk);
}
eeclk(ctlr, 0);
csr32w(ctlr, Rmear, 0);
microdelay(2);
}
static int
eegetw(Ctlr *ctlr, int a)
{
int d, i, w, v;
eeidle(ctlr);
eeclk(ctlr, 0);
eeclk(ctlr, Eeclk);
d = 0x180 | a;
for(i=0x400; i; i>>=1){
v = (d & i) ? Eedi : 0;
eeclk(ctlr, v);
eeclk(ctlr, Eeclk|v);
}
eeclk(ctlr, 0);
w = 0;
for(i=0x8000; i; i >>= 1){
eeclk(ctlr, Eeclk);
if(csr32r(ctlr, Rmear) & Eedo)
w |= i;
microdelay(2);
eeclk(ctlr, 0);
}
eeidle(ctlr);
return w;
}
static void
resetctlr(Ctlr *ctlr)
{
int i;
csr32w(ctlr, Rcr, Rst);
for(i=0;; i++){
if(i > 100)
panic("ns83815: soft reset did not complete");
microdelay(250);
if((csr32r(ctlr, Rcr) & Rst) == 0)
break;
delay(1);
}
}
static void
shutdown(Ether* ether)
{
Ctlr *ctlr = ether->ctlr;
print("ether83815 shutting down\n");
csr32w(ctlr, Rcr, Rxd|Txd);
resetctlr(ctlr);
}
static void
softreset(Ctlr* ctlr, int resetphys)
{
int i, w;
resetctlr(ctlr);
csr32w(ctlr, Rccsr, Pmests);
csr32w(ctlr, Rccsr, 0);
csr32w(ctlr, Rcfg, csr32r(ctlr, Rcfg) | Pint_acen);
if(resetphys){
csr32w(ctlr, Rbmcr, Reset);
for(i=0;; i++){
if(i > 100)
panic("ns83815: PHY soft reset time out");
if((csr32r(ctlr, Rbmcr) & Reset) == 0)
break;
delay(1);
}
}
csr16w(ctlr, 0xCC, 0x0001);
csr16w(ctlr, 0xE4, 0x189C);
csr16w(ctlr, 0xFC, 0x0000);
csr16w(ctlr, 0xF4, 0x5040);
csr16w(ctlr, 0xF8, 0x008C);
w = csr16r(ctlr, Rbmsr);
debug("anar: %4.4ux\n", csr16r(ctlr, Ranar));
csr16w(ctlr, Rbmcr, Anena);
if(csr16r(ctlr, Ranar) == 0 || (csr32r(ctlr, Rcfg) & Aneg_dn) == 0){
csr16w(ctlr, Rbmcr, Anena|Anrestart);
for(i=0;; i++){
if(i > 3000){
print("ns83815: auto neg timed out\n");
break;
}
if((w = csr16r(ctlr, Rbmsr)) & Ancomp)
break;
delay(1);
}
debug("%d ms\n", i);
w &= 0xFFFF;
debug("bmsr: %4.4ux\n", w);
}
USED(w);
debug("anar: %4.4ux\n", csr16r(ctlr, Ranar));
debug("anlpar: %4.4ux\n", csr16r(ctlr, Ranlpar));
debug("aner: %4.4ux\n", csr16r(ctlr, Raner));
debug("physts: %4.4ux\n", csr16r(ctlr, Rphysts));
debug("tbscr: %4.4ux\n", csr16r(ctlr, Rtbscr));
}
static int
media(Ether* ether)
{
Ctlr* ctlr;
ulong cfg;
ctlr = ether->ctlr;
cfg = csr32r(ctlr, Rcfg);
ctlr->fd = (cfg & Fdup) != 0;
ether->link = (cfg&Lnksts) != 0;
return (cfg&(Lnksts|Speed100)) == Lnksts? 10: 100;
}
static char* mediatable[9] = {
"10BASE-T",
"10BASE-2",
"10BASE-5",
"100BASE-TX",
"10BASE-TFD",
"100BASE-TXFD",
"100BASE-T4",
"100BASE-FX",
"100BASE-FXFD",
};
static int
is630(ulong id, Pcidev *p)
{
if(id == SiS900)
switch (p->rid) {
case SiSrev630s:
case SiSrev630e:
case SiSrev630ea1:
return 1;
}
return 0;
}
enum {
MagicReg = 0x48,
MagicRegSz = 1,
Magicrden = 0x40,
Paddr=		0x70,
Pdata=		0x71,
};
static int
sisrdcmos(Ctlr *ctlr)
{
int i;
unsigned reg;
ulong port;
Pcidev *p;
debug("ns83815: SiS 630 rev. %ux reading mac address from cmos\n", ctlr->pcidev->rid);
p = pcimatch(nil, SiS, SiS630bridge);
if(p == nil) {
print("ns83815: no SiS 630 rev. %ux bridge for mac addr\n",
ctlr->pcidev->rid);
return 0;
}
port = p->mem[0].bar & ~0x01;
debug("ns83815: SiS 630 rev. %ux reading mac addr from cmos via bridge at port 0x%lux\n", ctlr->pcidev->rid, port);
reg = pcicfgr8(p, MagicReg);
pcicfgw8(p, MagicReg, reg|Magicrden);
for (i = 0; i < Eaddrlen; i++) {
outb(port+Paddr, SiS630eenodeaddr + i);
ctlr->sromea[i] = inb(port+Pdata);
}
pcicfgw8(p, MagicReg, reg & ~Magicrden);
return 1;
}
static void
sissrom(Ctlr *ctlr)
{
union {
uchar	eaddr[Eaddrlen];
ushort	alignment;
} ee;
int i, off = SiSeenodeaddr, cnt = sizeof ee.eaddr / sizeof(short);
ushort *shp = (ushort *)ee.eaddr;
if(!is630(ctlr->id, ctlr->pcidev) || !sisrdcmos(ctlr)) {
for (i = 0; i < cnt; i++)
*shp++ = eegetw(ctlr, off++);
memmove(ctlr->sromea, ee.eaddr, sizeof ctlr->sromea);
}
}
static void
nssrom(Ctlr* ctlr)
{
int i, j;
for(i = 0; i < nelem(ctlr->srom); i++)
ctlr->srom[i] = eegetw(ctlr, i);
j = Nseenodeaddr*16 + 15;
for(i=0; i<48; i++){
ctlr->sromea[i>>3] |= ((ctlr->srom[j>>4] >> (15-(j&0xF))) & 1) << (i&7);
j++;
}
}
static void
srom(Ctlr* ctlr)
{
memset(ctlr->sromea, 0, sizeof(ctlr->sromea));
switch (ctlr->id) {
case SiS900:
case SiS7016:
sissrom(ctlr);
break;
case Nat83815:
nssrom(ctlr);
break;
default:
print("ns83815: srom: unknown id 0x%ux\n", ctlr->id);
break;
}
}
static void
scanpci83815(void)
{
Ctlr *ctlr;
Pcidev *p;
ulong id;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != Pcibcnet || p->ccru != 0)
continue;
id = (p->did<<16)|p->vid;
switch(id){
default:
continue;
case Nat83815:
break;
case SiS900:
break;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = p->mem[0].bar & ~0x01;
ctlr->pcidev = p;
ctlr->id = id;
if(ioalloc(ctlr->port, p->mem[0].size, 0, "ns83815") < 0){
print("ns83815: port 0x%uX in use\n", ctlr->port);
free(ctlr);
continue;
}
softreset(ctlr, 0);
srom(ctlr);
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
static void
multicast(void*, uchar*, int)
{
}
static int
reset(Ether* ether)
{
Ctlr *ctlr;
int i, x;
ulong ctladdr;
uchar ea[Eaddrlen];
static int scandone;
if(scandone == 0){
scanpci83815();
scandone = 1;
}
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
if(ether->port == 0 || ether->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
ether->ctlr = ctlr;
ether->port = ctlr->port;
ether->irq = ctlr->pcidev->intl;
ether->tbdf = ctlr->pcidev->tbdf;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0)
memmove(ether->ea, ctlr->sromea, Eaddrlen);
for(i=0; i<Eaddrlen; i+=2){
x = ether->ea[i] | (ether->ea[i+1]<<8);
ctladdr = (ctlr->id == Nat83815? i: i<<15);
csr32w(ctlr, Rrfcr, ctladdr);
csr32w(ctlr, Rrfdr, x);
}
csr32w(ctlr, Rrfcr, Rfen|Apm|Aab|Aam);
ether->mbps = media(ether);
for(i = 0; i < ether->nopt; i++){
if(cistrcmp(ether->opt[i], "FD") == 0){
ctlr->fd = 1;
continue;
}
for(x = 0; x < nelem(mediatable); x++){
debug("compare <%s> <%s>\n", mediatable[x],
ether->opt[i]);
if(cistrcmp(mediatable[x], ether->opt[i]) == 0){
if(x != 4 && x >= 3)
ether->mbps = 100;
else
ether->mbps = 10;
switch(x){
default:
ctlr->fd = 0;
break;
case 0x04:
case 0x05:
case 0x08:
ctlr->fd = 1;
break;
}
break;
}
}
}
ctlr->nrdr = Nrde;
ctlr->ntdr = Ntde;
pcisetbme(ctlr->pcidev);
ctlrinit(ether);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
ether->shutdown = shutdown;
return 0;
}
void
ether83815link(void)
{
addethercard("83815",  reset);
}