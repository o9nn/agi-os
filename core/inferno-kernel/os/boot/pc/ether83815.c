#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#define DEBUG		(1)
#define debug		if(DEBUG)print
enum {
Nrde		= 8,
Ntde		= 8,
};
#define Rbsz		ROUNDUP(sizeof(Etherpkt)+4, 4)
typedef struct Des {
ulong	next;
int	cmdsts;
ulong	addr;
Block*	bp;
} Des;
enum {
Own		= 1<<31,
More	= 1<<30,
Intr		= 1<<29,
Supcrc	= 1<<28,
Inccrc	= 1<<28,
Ok		= 1<<27,
Size		= 0xFFF,
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
Nat83815		= (0x0020<<16)|0x100B,
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
Lock	ilock;
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
Block*	bqhead;
Block*	bqtail;
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
Rcr=		0x00,
Rst=		1<<8,
Rxr=		1<<5,
Txr=		1<<4,
Rxd=		1<<3,
Rxe=		1<<2,
Txd=		1<<1,
Txe=		1<<0,
Rcfg=	0x04,
Lnksts=		1<<31,
Speed100=	1<<30,
Fdup=		1<<29,
Pol=		1<<28,
Aneg_dn=	1<<27,
Pint_acen=	1<<17,
Pause_adv=	1<<16,
Paneg_ena=	1<<13,
Paneg_all=	7<<13,
Ext_phy=	1<<12,
Phy_rst=		1<<10,
Phy_dis=		1<<9,
Req_alg=	1<<7,
Sb=			1<<6,
Pow=		1<<5,
Exd=		1<<4,
Pesel=		1<<3,
Brom_dis=	1<<2,
Bem=		1<<0,
Rmear=	0x08,
Mdc=		1<<6,
Mddir=		1<<5,
Mdio=		1<<4,
Eesel=		1<<3,
Eeclk=		1<<2,
Eedo=		1<<1,
Eedi=		1<<0,
Rptscr=	0x0C,
Risr=		0x10,
Txrcmp=	1<<25,
Rxrcmp=	1<<24,
Dperr=		1<<23,
Sserr=		1<<22,
Rmabt=		1<<21,
Rtabt=		1<<20,
Rxsovr=		1<<16,
Hiberr=		1<<15,
Phy=		1<<14,
Pme=		1<<13,
Swi=		1<<12,
Mib=		1<<11,
Txurn=		1<<10,
Txidle=		1<<9,
Txerr=		1<<8,
Txdesc=		1<<7,
Txok=		1<<6,
Rxorn=		1<<5,
Rxidle=		1<<4,
Rxearly=		1<<3,
Rxerr=		1<<2,
Rxdesc=		1<<1,
Rxok=		1<<0,
Rimr=	0x14,
Rier=	0x18,
Ie=			1<<0,
Rtxdp=	0x20,
Rtxcfg=	0x24,
Csi=		1<<31,
Hbi=		1<<30,
Atp=		1<<28,
Mxdma=		7<<20,
Mxdma32=	4<<20,
Mxdma64=	5<<20,
Flth=		0x3F<<8,
Drth=		0x3F<<0,
Flth128=		4<<8,
Drth512=	16<<0,
Rrxdp=	0x30,
Rrxcfg=	0x34,
Atx=		1<<28,
Rdrth=		0x1F<<1,
Rdrth64=	2<<1,
Rccsr=	0x3C,
Pmests=		1<<15,
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
Reset=		1<<15,
Sel100=		1<<13,
Anena=		1<<12,
Anrestart=	1<<9,
Selfdx=		1<<8,
Rbmsr=	0x84,
Ancomp=	1<<5,
Rphyidr1= 0x88,
Rphyidr2= 0x8C,
Ranar=	0x90,
Ranlpar=	0x94,
Raner=	0x98,
Rannptr=	0x9C,
Rphysts=	0xC0,
Rmicr=	0xC4,
Inten=		1<<1,
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
attach(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(&ctlr->ilock);
if(0)
dumpcregs(ctlr);
csr32w(ctlr, Rcr, Rxe);
iunlock(&ctlr->ilock);
}
static void
detach(Ether* ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
csr32w(ctlr, Rcr, 0);
delay(1);
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
bp = ctlr->bqhead;
if(bp == nil)
break;
ctlr->bqhead = bp->next;
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
Block *bp;
RingBuf *tb;
ctlr = ether->ctlr;
ilock(&ctlr->tlock);
while((tb = &ether->tb[ether->ti])->owner == Interface){
bp = allocb(tb->len);
memmove(bp->wp, tb->pkt, tb->len);
memmove(bp->wp+Eaddrlen, ether->ea, Eaddrlen);
bp->wp += tb->len;
if(ctlr->bqhead)
ctlr->bqtail->next = bp;
else
ctlr->bqhead = bp;
ctlr->bqtail = bp;
txstart(ether);
tb->owner = Host;
ether->ti = NEXT(ether->ti, ether->ntb);
}
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
Ctlr *ctlr;
Ether *ether;
int status, cmdsts;
Des *des;
RingBuf *rb;
ether = arg;
ctlr = ether->ctlr;
while((status = csr32r(ctlr, Risr)) != 0){
status &= ~(Pme|Mib);
status &= ~(Hiberr|Txrcmp|Rxrcmp|Rxsovr|Dperr|Sserr|Rmabt|Rtabt);
if(status & (Rxdesc|Rxok|Rxerr|Rxearly|Rxorn)){
des = &ctlr->rdr[ctlr->rdrx];
while((cmdsts = des->cmdsts) & Own){
rb = &ether->rb[ether->ri];
if(rb->owner == Interface && (cmdsts&Ok)){
rb->len = (cmdsts&Size)-4;
memmove(rb->pkt, des->bp->rp, rb->len);
rb->owner = Host;
ether->ri = NEXT(ether->ri, ether->nrb);
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
ilock(&ctlr->ilock);
iunlock(&ctlr->ilock);
status &= ~(Txurn);
}
ilock(&ctlr->tlock);
while(ctlr->ntq){
des = &ctlr->tdr[ctlr->tdri];
cmdsts = des->cmdsts;
if(cmdsts & Own)
break;
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
ctlr->rdr = malloc(ctlr->nrdr*sizeof(Des));
last = nil;
for(des = ctlr->rdr; des < &ctlr->rdr[ctlr->nrdr]; des++){
des->bp = allocb(Rbsz);
des->cmdsts = Rbsz;
des->addr = PADDR(des->bp->rp);
if(last != nil)
last->next = PADDR(des);
last = des;
}
ctlr->rdr[ctlr->nrdr-1].next = PADDR(ctlr->rdr);
ctlr->rdrx = 0;
csr32w(ctlr, Rrxdp, PADDR(ctlr->rdr));
ctlr->tdr = xspanalloc(ctlr->ntdr*sizeof(Des), 8*sizeof(ulong), 0);
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
csr32w(ctlr, Rimr, Dperr|Sserr|Rmabt|Rtabt|Rxsovr|Hiberr|Txurn|Txerr|Txdesc|Txok|Rxorn|Rxerr|Rxdesc|Rxok);
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
softreset(Ctlr* ctlr, int resetphys)
{
int i, w;
csr32w(ctlr, Rcr, Rst);
for(i=0;; i++){
if(i > 100)
panic("ns83815: soft reset did not complete");
microdelay(250);
if((csr32r(ctlr, Rcr) & Rst) == 0)
break;
delay(1);
}
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
if(i > 6000){
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
debug("anar: %4.4ux\n", csr16r(ctlr, Ranar));
debug("anlpar: %4.4ux\n", csr16r(ctlr, Ranlpar));
debug("aner: %4.4ux\n", csr16r(ctlr, Raner));
debug("physts: %4.4ux\n", csr16r(ctlr, Rphysts));
debug("tbscr: %4.4ux\n", csr16r(ctlr, Rtbscr));
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
static void
srom(Ctlr* ctlr)
{
int i, j;
for(i = 0; i < nelem(ctlr->srom); i++)
ctlr->srom[i] = eegetw(ctlr, i);
memset(ctlr->sromea, 0, sizeof(ctlr->sromea));
j = 6*16 + 15;
for(i=0; i<48; i++){
ctlr->sromea[i>>3] |= ((ctlr->srom[j>>4] >> (15-(j&0xF))) & 1) << (i&7);
j++;
}
}
static void
scanpci83815(void)
{
Ctlr *ctlr;
Pcidev *p;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
default:
continue;
case Nat83815:
break;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = p->mem[0].bar & ~0x01;
ctlr->pcidev = p;
ctlr->id = (p->did<<16)|p->vid;
softreset(ctlr, 0);
srom(ctlr);
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
int
ether83815reset(Ether* ether)
{
Ctlr *ctlr;
int i, x;
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
csr32w(ctlr, Rrfcr, i);
csr32w(ctlr, Rrfdr, x);
}
csr32w(ctlr, Rrfcr, Rfen|Apm|Aab|Aam);
for(i = 0; i < ether->nopt; i++){
if(cistrcmp(ether->opt[i], "FD") == 0){
ctlr->fd = 1;
continue;
}
for(x = 0; x < nelem(mediatable); x++){
debug("compare <%s> <%s>\n", mediatable[x],
ether->opt[i]);
if(cistrcmp(mediatable[x], ether->opt[i]) == 0){
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
ether->detach = detach;
return 0;
}