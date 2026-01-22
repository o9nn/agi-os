#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#define iallocb allocb
#ifndef CACHELINESZ
#define CACHELINESZ	32
#endif
enum
{
IOen		= 1<<0,
MEMen		= 1<<1,
MASen		= 1<<2,
MemWrInv	= 1<<4,
PErrEn		= 1<<6,
SErrEn		= 1<<8,
};
enum {
Ctrl		= 0x00000000,
Status		= 0x00000008,
Eec		= 0x00000010,
Eerd		= 0x00000014,
Ctrlext		= 0x00000018,
Fla		= 0x0000001c,
Mdic		= 0x00000020,
Seresctl	= 0x00000024,
Fcal		= 0x00000028,
Fcah		= 0x0000002C,
Fct		= 0x00000030,
Kumctrlsta	= 0x00000034,
Vet		= 0x00000038,
Fcttv		= 0x00000170,
Txcw		= 0x00000178,
Rxcw		= 0x00000180,
Ledctl		= 0x00000E00,
Pba		= 0x00001000,
Icr		= 0x000000C0,
Ics		= 0x000000C8,
Ims		= 0x000000D0,
Imc		= 0x000000D8,
Iam		= 0x000000E0,
Rctl		= 0x00000100,
Ert		= 0x00002008,
Fcrtl		= 0x00002160,
Fcrth		= 0x00002168,
Psrctl		= 0x00002170,
Rdbal		= 0x00002800,
Rdbah		= 0x00002804,
Rdlen		= 0x00002808,
Rdh		= 0x00002810,
Rdt		= 0x00002818,
Rdtr		= 0x00002820,
Rxdctl		= 0x00002828,
Radv		= 0x0000282C,
Rdbal1		= 0x00002900,
Rdbah1		= 0x00002804,
Rdlen1		= 0x00002908,
Rdh1		= 0x00002910,
Rdt1		= 0x00002918,
Rxdctl1		= 0x00002928,
Rsrpd		= 0x00002c00,
Raid		= 0x00002c08,
Cpuvec		= 0x00002c10,
Rxcsum		= 0x00005000,
Rfctl		= 0x00005008,
Mta		= 0x00005200,
Ral		= 0x00005400,
Rah		= 0x00005404,
Vfta		= 0x00005600,
Mrqc		= 0x00005818,
Rssim		= 0x00005864,
Rssir		= 0x00005868,
Reta		= 0x00005c00,
Rssrk		= 0x00005c80,
Tctl		= 0x00000400,
Tipg		= 0x00000410,
Tdbal		= 0x00003800,
Tdbah		= 0x00003804,
Tdlen		= 0x00003808,
Tdh		= 0x00003810,
Tdt		= 0x00003818,
Tidv		= 0x00003820,
Txdctl		= 0x00003828,
Tadv		= 0x0000382C,
Tarc0		= 0x00003840,
Tdbal1		= 0x00003900,
Tdbah1		= 0x00003904,
Tdlen1		= 0x00003908,
Tdh1		= 0x00003910,
Tdt1		= 0x00003918,
Txdctl1		= 0x00003928,
Tarc1		= 0x00003940,
Statistics	= 0x00004000,
Gorcl		= 0x88/4,
Gotcl		= 0x90/4,
Torl		= 0xC0/4,
Totl		= 0xC8/4,
Nstatistics	= 64,
};
enum {
GIOmd		= 1<<2,
Lrst		= 1<<3,
Slu		= 1<<6,
SspeedMASK	= 3<<8,
SspeedSHIFT	= 8,
Sspeed10	= 0x00000000,
Sspeed100	= 0x00000100,
Sspeed1000	= 0x00000200,
Frcspd		= 1<<11,
Frcdplx		= 1<<12,
SwdpinsloMASK	= 0x003C0000,
SwdpinsloSHIFT	= 18,
SwdpioloMASK	= 0x03C00000,
SwdpioloSHIFT	= 22,
Devrst		= 1<<26,
Rfce		= 1<<27,
Tfce		= 1<<28,
Vme		= 1<<30,
Phy_rst		= 1<<31,
};
enum {
Lu		= 1<<1,
Lanid		= 3<<2,
Txoff		= 1<<4,
Tbimode		= 1<<5,
SpeedMASK	= 0x000000C0,
Speed10		= 0x00000000,
Speed100	= 0x00000040,
Speed1000	= 0x00000080,
Phyra		= 1<<10,
GIOme		= 1<<19,
};
enum {
Fd		= 0x00000001,
AsdvMASK	= 0x00000300,
Asdv10		= 0x00000000,
Asdv100		= 0x00000100,
Asdv1000	= 0x00000200,
};
enum {
Sk		= 1<<0,
Cs		= 1<<1,
Di		= 1<<2,
Do		= 1<<3,
Areq		= 1<<6,
Agnt		= 1<<7,
};
enum {
ee_start	= 1<<0,
ee_done		= 1<<1,
ee_addr		= 0xfff8<<2,
ee_data		= 0xffff<<16,
};
enum {
Asdchk		= 1<<12,
Eerst		= 1<<13,
Spdbyps		= 1<<15,
};
enum {
Ea		= 0x00,
Cf		= 0x03,
Icw1		= 0x0A,
Sid		= 0x0B,
Svid		= 0x0C,
Did		= 0x0D,
Vid		= 0x0E,
Icw2		= 0x0F,
};
enum {
MDIdMASK	= 0x0000FFFF,
MDIdSHIFT	= 0,
MDIrMASK	= 0x001F0000,
MDIrSHIFT	= 16,
MDIpMASK	= 0x03E00000,
MDIpSHIFT	= 21,
MDIwop		= 0x04000000,
MDIrop		= 0x08000000,
MDIready	= 0x10000000,
MDIie		= 0x20000000,
MDIe		= 0x40000000,
};
enum {
Txdw		= 0x00000001,
Txqe		= 0x00000002,
Lsc		= 0x00000004,
Rxseq		= 0x00000008,
Rxdmt0		= 0x00000010,
Rxo		= 0x00000040,
Rxt0		= 0x00000080,
Mdac		= 0x00000200,
Rxcfg		= 0x00000400,
Gpi0		= 0x00000800,
Gpi1		= 0x00001000,
Gpi2		= 0x00002000,
Gpi3		= 0x00004000,
Ack		= 0x00020000,
};
enum {
TxcwFd		= 0x00000020,
TxcwHd		= 0x00000040,
TxcwPauseMASK	= 0x00000180,
TxcwPauseSHIFT	= 7,
TxcwPs		= 1<<TxcwPauseSHIFT,
TxcwAs		= 2<<TxcwPauseSHIFT,
TxcwRfiMASK	= 0x00003000,
TxcwRfiSHIFT	= 12,
TxcwNpr		= 0x00008000,
TxcwConfig	= 0x40000000,
TxcwAne		= 0x80000000,
};
enum {
Rrst		= 0x00000001,
Ren		= 0x00000002,
Sbp		= 0x00000004,
Upe		= 0x00000008,
Mpe		= 0x00000010,
Lpe		= 0x00000020,
LbmMASK		= 0x000000C0,
LbmOFF		= 0x00000000,
LbmTBI		= 0x00000040,
LbmMII		= 0x00000080,
LbmXCVR		= 0x000000C0,
RdtmsMASK	= 0x00000300,
RdtmsHALF	= 0x00000000,
RdtmsQUARTER	= 0x00000100,
RdtmsEIGHTH	= 0x00000200,
MoMASK		= 0x00003000,
Bam		= 0x00008000,
BsizeMASK	= 0x00030000,
Bsize2048	= 0x00000000,
Bsize1024	= 0x00010000,
Bsize512	= 0x00020000,
Bsize256	= 0x00030000,
Vfe		= 0x00040000,
Cfien		= 0x00080000,
Cfi		= 0x00100000,
Dpf		= 0x00400000,
Pmcf		= 0x00800000,
Bsex		= 0x02000000,
Secrc		= 0x04000000,
};
enum {
Trst		= 0x00000001,
Ten		= 0x00000002,
Psp		= 0x00000008,
Mulr		= 0x10000000,
CtMASK		= 0x00000FF0,
CtSHIFT		= 4,
ColdMASK	= 0x003FF000,
ColdSHIFT	= 12,
Swxoff		= 0x00400000,
Pbe		= 0x00800000,
Rtlc		= 0x01000000,
Nrtu		= 0x02000000,
};
enum {
PthreshMASK	= 0x0000003F,
PthreshSHIFT	= 0,
HthreshMASK	= 0x00003F00,
HthreshSHIFT	= 8,
WthreshMASK	= 0x003F0000,
WthreshSHIFT	= 16,
Gran		= 0x01000000,
};
enum {
PcssMASK	= 0x000000FF,
PcssSHIFT	= 0,
Ipofl		= 0x00000100,
Tuofl		= 0x00000200,
};
typedef struct Rdesc {
uint	addr[2];
ushort	length;
ushort	checksum;
uchar	status;
uchar	errors;
ushort	special;
} Rdesc;
enum {
Rdd		= 0x01,
Reop		= 0x02,
Ixsm		= 0x04,
Vp		= 0x08,
Tcpcs		= 0x20,
Ipcs		= 0x40,
Pif		= 0x80,
};
enum {
Ce		= 0x01,
Se		= 0x02,
Seq		= 0x04,
Cxe		= 0x10,
Tcpe		= 0x20,
Ipe		= 0x40,
Rxe		= 0x80,
};
typedef struct Tdesc {
uint	addr[2];
uint	control;
uint	status;
} Tdesc;
enum {
LenMASK		= 0x000FFFFF,
LenSHIFT	= 0,
DtypeCD		= 0x00000000,
DtypeDD		= 0x00100000,
PtypeTCP	= 0x01000000,
Teop		= 0x01000000,
PtypeIP		= 0x02000000,
Ifcs		= 0x02000000,
Tse		= 0x04000000,
Rs		= 0x08000000,
Rps		= 0x10000000,
Dext		= 0x20000000,
Vle		= 0x40000000,
Ide		= 0x80000000,
};
enum {
Tdd		= 0x00000001,
Ec		= 0x00000002,
Lc		= 0x00000004,
Tu		= 0x00000008,
CssMASK		= 0x0000FF00,
CssSHIFT	= 8,
};
enum {
Nrdesc		= 128,
Ntdesc		= 128,
};
enum {
i82563,
i82571,
i82573,
};
static char *tname[] = {
"i82563",
"i82571",
"i82573",
};
#define Type	tname[ctlr->type]
typedef struct Ctlr Ctlr;
struct Ctlr {
int	port;
Pcidev	*pcidev;
Ctlr	*next;
int	active;
int	cls;
ushort	eeprom[0x40];
uchar	ra[Eaddrlen];
int	type;
int*	nic;
Lock	imlock;
int	im;
Lock	slock;
uint	statistics[Nstatistics];
Rdesc	*rdba;
Block	**rb;
int	rdh;
int	rdt;
Tdesc	*tdba;
Lock	tdlock;
Block	**tb;
int	tdh;
int	tdt;
int	txcw;
int	fcrtl;
int	fcrth;
Block	*bqhead;
Block	*bqtail;
};
static Ctlr	*ctlrhead;
static Ctlr	*ctlrtail;
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static void
i82563im(Ctlr* ctlr, int im)
{
ilock(&ctlr->imlock);
ctlr->im |= im;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
}
static void
i82563attach(Ether* edev)
{
int ctl;
Ctlr *ctlr;
ctlr = edev->ctlr;
i82563im(ctlr, 0);
ctl = csr32r(ctlr, Rctl)|Ren;
csr32w(ctlr, Rctl, ctl);
ctl = csr32r(ctlr, Tctl)|Ten;
csr32w(ctlr, Tctl, ctl);
}
static void
txstart(Ether *edev)
{
int tdh, tdt;
Ctlr *ctlr = edev->ctlr;
Block *bp;
Tdesc *tdesc;
tdh = PREV(ctlr->tdh, Ntdesc);
for(tdt = ctlr->tdt; tdt != tdh; tdt = NEXT(tdt, Ntdesc)){
if((bp = ctlr->bqhead) == nil)
break;
ctlr->bqhead = bp->next;
if (ctlr->bqtail == bp)
ctlr->bqtail = nil;
tdesc = &ctlr->tdba[tdt];
tdesc->addr[0] = PCIWADDR(bp->rp);
tdesc->addr[1] = 0;
tdesc->control =  Rs | Ifcs | Teop | BLEN(bp);
ctlr->tb[tdt] = bp;
}
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
i82563im(ctlr, Txdw);
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
i82563transmit(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
Tdesc *tdesc;
RingBuf *tb;
int tdh;
ctlr = edev->ctlr;
ilock(&ctlr->tdlock);
tdh = ctlr->tdh;
for(;;){
tdesc = &ctlr->tdba[tdh];
if(!(tdesc->status & Tdd))
break;
if(ctlr->tb[tdh] != nil){
freeb(ctlr->tb[tdh]);
ctlr->tb[tdh] = nil;
}
tdesc->status = 0;
tdh = NEXT(tdh, Ntdesc);
}
ctlr->tdh = tdh;
while((tb = &edev->tb[edev->ti])->owner == Interface){
bp = fromringbuf(edev);
if(ctlr->bqhead)
ctlr->bqtail->next = bp;
else
ctlr->bqhead = bp;
ctlr->bqtail = bp;
txstart(edev);
tb->owner = Host;
edev->ti = NEXT(edev->ti, edev->ntb);
}
iunlock(&ctlr->tdlock);
}
static void
i82563replenish(Ctlr* ctlr)
{
int rdt;
Block *bp;
Rdesc *rdesc;
rdt = ctlr->rdt;
while(NEXT(rdt, Nrdesc) != ctlr->rdh){
rdesc = &ctlr->rdba[rdt];
if(ctlr->rb[rdt] != nil){
}
else if((bp = iallocb(2048)) != nil){
ctlr->rb[rdt] = bp;
rdesc->addr[0] = PCIWADDR(bp->rp);
rdesc->addr[1] = 0;
}
else
break;
rdesc->status = 0;
rdt = NEXT(rdt, Nrdesc);
}
ctlr->rdt = rdt;
csr32w(ctlr, Rdt, rdt);
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
} else if (debug)
print("#l%d: toringbuf: dropping packets @ ri %d\n",
ether->ctlrno, ether->ri);
}
static void
i82563interrupt(Ureg*, void* arg)
{
int icr, im, rdh, txdw = 0;
Block *bp;
Ctlr *ctlr;
Ether *edev;
Rdesc *rdesc;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
im = ctlr->im;
for(icr = csr32r(ctlr, Icr); icr & ctlr->im; icr = csr32r(ctlr, Icr)){
if(icr & (Rxseq|Lsc)){
}
rdh = ctlr->rdh;
for (;;) {
rdesc = &ctlr->rdba[rdh];
if(!(rdesc->status & Rdd))
break;
if ((rdesc->status & Reop) && rdesc->errors == 0) {
bp = ctlr->rb[rdh];
if(0 && memcmp(bp->rp, broadcast, 6) != 0)
print("#l%d: rx %d %E %E %d\n",
edev->ctlrno, rdh, bp->rp,
bp->rp+6, rdesc->length);
ctlr->rb[rdh] = nil;
bp->wp += rdesc->length;
toringbuf(edev, bp);
freeb(bp);
} else if (rdesc->status & Reop && rdesc->errors)
print("%s: input packet error 0x%ux\n",
Type, rdesc->errors);
rdesc->status = 0;
rdh = NEXT(rdh, Nrdesc);
}
ctlr->rdh = rdh;
if(icr & Rxdmt0)
i82563replenish(ctlr);
if(icr & Txdw){
im &= ~Txdw;
txdw++;
}
}
ctlr->im = im;
csr32w(ctlr, Ims, im);
iunlock(&ctlr->imlock);
if(txdw)
i82563transmit(edev);
}
static void
i82563init(Ether* edev)
{
int csr, i, r;
Ctlr *ctlr;
ctlr = edev->ctlr;
csr = edev->ea[3]<<24 | edev->ea[2]<<16 | edev->ea[1]<<8 | edev->ea[0];
csr32w(ctlr, Ral, csr);
csr = 0x80000000 | edev->ea[5]<<8 | edev->ea[4];
csr32w(ctlr, Rah, csr);
for (i = 1; i < 16; i++) {
csr32w(ctlr, Ral+i*8, 0);
csr32w(ctlr, Rah+i*8, 0);
}
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta+i*4, 0);
csr32w(ctlr, Rctl, 0);
ctlr->rdba = xspanalloc(Nrdesc*sizeof(Rdesc), 256, 0);
csr32w(ctlr, Rdbal, PCIWADDR(ctlr->rdba));
csr32w(ctlr, Rdbah, 0);
csr32w(ctlr, Rdlen, Nrdesc*sizeof(Rdesc));
ctlr->rdh = 0;
csr32w(ctlr, Rdh, ctlr->rdh);
ctlr->rdt = 0;
csr32w(ctlr, Rdt, ctlr->rdt);
ctlr->rb = malloc(sizeof(Block*)*Nrdesc);
i82563replenish(ctlr);
csr32w(ctlr, Rdtr, 0);
csr32w(ctlr, Rctl, Dpf | Bsize2048 | Bam | RdtmsHALF);
i82563im(ctlr, Rxt0 | Rxo | Rxdmt0 | Rxseq | Ack);
csr32w(ctlr, Tctl, 0x0F<<CtSHIFT | Psp | 0x3f<<ColdSHIFT | Mulr);
csr32w(ctlr, Tipg, 6<<20 | 8<<10 | 8);
csr32w(ctlr, Tidv, 1);
ctlr->tdba = xspanalloc(Ntdesc*sizeof(Tdesc), 256, 0);
memset(ctlr->tdba, 0, Ntdesc*sizeof(Tdesc));
csr32w(ctlr, Tdbal, PCIWADDR(ctlr->tdba));
csr32w(ctlr, Tdbah, 0);
csr32w(ctlr, Tdlen, Ntdesc*sizeof(Tdesc));
ctlr->tdh = 0;
csr32w(ctlr, Tdh, ctlr->tdh);
ctlr->tdt = 0;
csr32w(ctlr, Tdt, ctlr->tdt);
ctlr->tb = malloc(sizeof(Block*)*Ntdesc);
csr32w(ctlr, Rxcsum, Tuofl | Ipofl | ETHERHDRSIZE<<PcssSHIFT);
r = csr32r(ctlr, Tctl);
r |= Ten;
csr32w(ctlr, Tctl, r);
}
static ushort
eeread(Ctlr* ctlr, int adr)
{
csr32w(ctlr, Eerd, ee_start | adr << 2);
while ((csr32r(ctlr, Eerd) & ee_done) == 0)
;
return csr32r(ctlr, Eerd) >> 16;
}
static int
eeload(Ctlr* ctlr)
{
ushort sum;
int data, adr;
sum = 0;
for (adr = 0; adr < 0x40; adr++) {
data = eeread(ctlr, adr);
ctlr->eeprom[adr] = data;
sum += data;
}
return sum;
}
static void
detach(Ctlr *ctlr)
{
int r;
csr32w(ctlr, Imc, ~0);
csr32w(ctlr, Rctl, 0);
csr32w(ctlr, Tctl, 0);
delay(10);
r = csr32r(ctlr, Ctrl);
csr32w(ctlr, Ctrl, Devrst | r);
delay(1);
while(csr32r(ctlr, Ctrl) & Devrst)
;
if(1 || ctlr->type != i82563){
r = csr32r(ctlr, Ctrl);
csr32w(ctlr, Ctrl, Slu | r);
}
csr32w(ctlr, Ctrlext, Eerst | csr32r(ctlr, Ctrlext));
delay(1);
while(csr32r(ctlr, Ctrlext) & Eerst)
;
csr32w(ctlr, Imc, ~0);
delay(1);
while(csr32r(ctlr, Icr))
;
}
static void
i82563detach(Ether *edev)
{
detach(edev->ctlr);
}
static void
i82563shutdown(Ether* ether)
{
i82563detach(ether);
}
static int
i82563reset(Ctlr* ctlr)
{
int i, r;
detach(ctlr);
r = eeload(ctlr);
if (r != 0 && r != 0xBABA){
print("%s: bad EEPROM checksum - 0x%4.4ux\n", Type, r);
return -1;
}
for(i = Ea; i < Eaddrlen/2; i++){
ctlr->ra[2*i]   = ctlr->eeprom[i];
ctlr->ra[2*i+1] = ctlr->eeprom[i]>>8;
}
r = (csr32r(ctlr, Status) & Lanid) >> 2;
ctlr->ra[5] += r;
r = ctlr->ra[3]<<24 | ctlr->ra[2]<<16 | ctlr->ra[1]<<8 | ctlr->ra[0];
csr32w(ctlr, Ral, r);
r = 0x80000000 | ctlr->ra[5]<<8 | ctlr->ra[4];
csr32w(ctlr, Rah, r);
for(i = 1; i < 16; i++){
csr32w(ctlr, Ral+i*8, 0);
csr32w(ctlr, Rah+i*8, 0);
}
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta+i*4, 0);
csr32w(ctlr, Fcal, 0x00C28001);
csr32w(ctlr, Fcah, 0x00000100);
csr32w(ctlr, Fct,  0x00008808);
csr32w(ctlr, Fcttv, 0x00000100);
csr32w(ctlr, Fcrtl, ctlr->fcrtl);
csr32w(ctlr, Fcrth, ctlr->fcrth);
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
ctlr->im = 0;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
return 0;
}
static void
i82563pci(void)
{
int port, type, cls;
Pcidev *p;
Ctlr *ctlr;
static int first = 1;
if (first)
first = 0;
else
return;
p = nil;
while(p = pcimatch(p, 0x8086, 0)){
switch(p->did){
case 0x1096:
case 0x10ba:
type = i82563;
break;
case 0x108b:
case 0x108c:
case 0x109a:
type = i82573;
break;
default:
continue;
}
port = upamalloc(p->mem[0].bar & ~0x0F, p->mem[0].size, 0);
if(port == 0){
print("%s: can't map %d @ 0x%8.8lux\n", tname[type],
p->mem[0].size, p->mem[0].bar);
continue;
}
if(p->pcr & MemWrInv){
cls = pcicfgr8(p, PciCLS) * 4;
if(cls != CACHELINESZ)
pcicfgw8(p, PciCLS, CACHELINESZ/4);
}
cls = pcicfgr8(p, PciCLS);
switch(cls){
default:
print("%s: unexpected CLS - %d bytes\n",
tname[type], cls*sizeof(long));
break;
case 0x00:
case 0xFF:
print("%s: unusable PciCLS: %d, using %d longs\n",
tname[type], cls, CACHELINESZ/sizeof(long));
cls = CACHELINESZ/sizeof(long);
pcicfgw8(p, PciCLS, cls);
break;
case 0x08:
case 0x10:
break;
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = port;
ctlr->pcidev = p;
ctlr->cls = cls*4;
ctlr->type = type;
ctlr->nic = KADDR(ctlr->port);
if(i82563reset(ctlr)){
free(ctlr);
continue;
}
pcisetbme(p);
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
static uchar nilea[Eaddrlen];
int
i82563pnp(Ether* edev)
{
Ctlr *ctlr;
if(ctlrhead == nil)
i82563pci();
for(ctlr = ctlrhead; ctlr != nil; ctlr = ctlr->next){
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
if(memcmp(edev->ea, nilea, Eaddrlen) == 0)
memmove(edev->ea, ctlr->ra, Eaddrlen);
i82563init(edev);
edev->attach = i82563attach;
edev->transmit = i82563transmit;
edev->interrupt = i82563interrupt;
edev->detach = i82563detach;
if((csr32r(ctlr, Status)&Lu) == 0){
print("ether#%d: 82563 (%s): link down\n", edev->ctlrno, Type);
return -1;
}
return 0;
}