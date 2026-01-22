#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#include "ethermii.h"
enum {
Debug = 0,
};
enum {
i82542     = (0x1000<<16)|0x8086,
i82543gc   = (0x1004<<16)|0x8086,
i82544ei   = (0x1008<<16)|0x8086,
i82540em   = (0x100E<<16)|0x8086,
i82546eb   = (0x1010<<16)|0x8086,
i82547ei   = (0x1019<<16)|0x8086,
i82540eplp = (0x101E<<16)|0x8086,
i82547gi   = (0x1075<<16)|0x8086,
i82541gi   = (0x1076<<16)|0x8086,
i82541gi2  = (0x1077<<16)|0x8086,
i82546gb   = (0x1079<<16)|0x8086,
i82541pi   = (0x107c<<16)|0x8086,
};
#define iallocb allocb
#ifndef CACHELINESZ
#define CACHELINESZ	32
#endif
enum
{
IOen		= (1<<0),
MEMen		= (1<<1),
MASen		= (1<<2),
MemWrInv	= (1<<4),
PErrEn		= (1<<6),
SErrEn		= (1<<8),
};
enum {
Ctrl		= 0x00000000,
Status		= 0x00000008,
Eecd		= 0x00000010,
Ctrlext		= 0x00000018,
Mdic		= 0x00000020,
Fcal		= 0x00000028,
Fcah		= 0x0000002C,
Fct		= 0x00000030,
Icr		= 0x000000C0,
Ics		= 0x000000C8,
Ims		= 0x000000D0,
Imc		= 0x000000D8,
Rctl		= 0x00000100,
Fcttv		= 0x00000170,
Txcw		= 0x00000178,
Tctl		= 0x00000400,
Tipg		= 0x00000410,
Tbt		= 0x00000448,
Ait		= 0x00000458,
Fcrtl		= 0x00002160,
Fcrth		= 0x00002168,
Rdbal		= 0x00002800,
Rdbah		= 0x00002804,
Rdlen		= 0x00002808,
Rdh		= 0x00002810,
Rdt		= 0x00002818,
Rdtr		= 0x00002820,
Rxdctl		= 0x00002828,
Radv		= 0x0000282C,
Txdmac		= 0x00003000,
Ett		= 0x00003008,
Tdbal		= 0x00003800,
Tdbah		= 0x00003804,
Tdlen		= 0x00003808,
Tdh		= 0x00003810,
Tdt		= 0x00003818,
Tidv		= 0x00003820,
Txdctl		= 0x00003828,
Tadv		= 0x0000382C,
Statistics	= 0x00004000,
Gorcl		= 0x88/4,
Gotcl		= 0x90/4,
Torl		= 0xC0/4,
Totl		= 0xC8/4,
Nstatistics	= 64,
Rxcsum		= 0x00005000,
Mta		= 0x00005200,
Ral		= 0x00005400,
Rah		= 0x00005404,
Manc		= 0x00005820,
};
enum {
Bem		= 0x00000002,
Prior		= 0x00000004,
Lrst		= 0x00000008,
Asde		= 0x00000020,
Slu		= 0x00000040,
Ilos		= 0x00000080,
SspeedMASK	= 0x00000300,
SspeedSHIFT	= 8,
Sspeed10	= 0x00000000,
Sspeed100	= 0x00000100,
Sspeed1000	= 0x00000200,
Frcspd		= 0x00000800,
Frcdplx		= 0x00001000,
SwdpinsloMASK	= 0x003C0000,
SwdpinsloSHIFT	= 18,
SwdpioloMASK	= 0x03C00000,
SwdpioloSHIFT	= 22,
Devrst		= 0x04000000,
Rfce		= 0x08000000,
Tfce		= 0x10000000,
Vme		= 0x40000000,
};
enum {
Lu		= 0x00000002,
Lanid		= 0x0000000C,
Txoff		= 0x00000010,
Tbimode		= 0x00000020,
SpeedMASK	= 0x000000C0,
Speed10		= 0x00000000,
Speed100	= 0x00000040,
Speed1000	= 0x00000080,
Mtxckok		= 0x00000400,
Pci66		= 0x00000800,
Bus64		= 0x00001000,
};
enum {
Fd		= 0x00000001,
AsdvMASK	= 0x00000300,
Asdv10		= 0x00000000,
Asdv100		= 0x00000100,
Asdv1000	= 0x00000200,
};
enum {
Sk		= 0x00000001,
Cs		= 0x00000002,
Di		= 0x00000004,
Do		= 0x00000008,
Areq		= 0x00000040,
Agnt		= 0x00000080,
Eepresent	= 0x00000100,
Eesz256		= 0x00000200,
Eeszaddr	= 0x00000400,
Spi		= 0x00002000,
};
enum {
Gpien		= 0x0000000F,
SwdpinshiMASK	= 0x000000F0,
SwdpinshiSHIFT	= 4,
SwdpiohiMASK	= 0x00000F00,
SwdpiohiSHIFT	= 8,
Asdchk		= 0x00001000,
Eerst		= 0x00002000,
Ips		= 0x00004000,
Spdbyps		= 0x00008000,
};
enum {
Ea		= 0x00,
Cf		= 0x03,
Pba		= 0x08,
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
};
enum {
Mdd		= ((1<<2)<<SwdpinsloSHIFT),
Mddo		= ((1<<2)<<SwdpioloSHIFT),
Mdc		= ((1<<3)<<SwdpinsloSHIFT),
Mdco		= ((1<<3)<<SwdpioloSHIFT),
Mdr		= ((1<<0)<<SwdpinshiSHIFT),
Mdro		= ((1<<0)<<SwdpiohiSHIFT),
};
enum {
TxcwFd		= 0x00000020,
TxcwHd		= 0x00000040,
TxcwPauseMASK	= 0x00000180,
TxcwPauseSHIFT	= 7,
TxcwPs		= (1<<TxcwPauseSHIFT),
TxcwAs		= (2<<TxcwPauseSHIFT),
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
enum {
Arpen		= 0x00002000,
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
Nrdesc		= 32,
Ntdesc		= 8,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
int	active;
int	id;
int	cls;
ushort	eeprom[0x40];
int*	nic;
Lock	imlock;
int	im;
Mii*	mii;
Lock	slock;
uint	statistics[Nstatistics];
uchar	ra[Eaddrlen];
ulong	mta[128];
Rdesc*	rdba;
Block**	rb;
int	rdh;
int	rdt;
Tdesc*	tdba;
Lock	tdlock;
Block**	tb;
int	tdh;
int	tdt;
int	ett;
int	txcw;
int	fcrtl;
int	fcrth;
Block*	bqhead;
Block*	bqtail;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static void
igbeim(Ctlr* ctlr, int im)
{
ilock(&ctlr->imlock);
ctlr->im |= im;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
}
static void
igbeattach(Ether* edev)
{
int ctl;
Ctlr *ctlr;
ctlr = edev->ctlr;
igbeim(ctlr, 0);
ctl = csr32r(ctlr, Rctl)|Ren;
csr32w(ctlr, Rctl, ctl);
ctl = csr32r(ctlr, Tctl)|Ten;
csr32w(ctlr, Tctl, ctl);
}
static char* statistics[Nstatistics] = {
"CRC Error",
"Alignment Error",
"Symbol Error",
"RX Error",
"Missed Packets",
"Single Collision",
"Excessive Collisions",
"Multiple Collision",
"Late Collisions",
nil,
"Collision",
"Transmit Underrun",
"Defer",
"Transmit - No CRS",
"Sequence Error",
"Carrier Extension Error",
"Receive Error Length",
nil,
"XON Received",
"XON Transmitted",
"XOFF Received",
"XOFF Transmitted",
"FC Received Unsupported",
"Packets Received (64 Bytes)",
"Packets Received (65-127 Bytes)",
"Packets Received (128-255 Bytes)",
"Packets Received (256-511 Bytes)",
"Packets Received (512-1023 Bytes)",
"Packets Received (1024-1522 Bytes)",
"Good Packets Received",
"Broadcast Packets Received",
"Multicast Packets Received",
"Good Packets Transmitted",
nil,
"Good Octets Received",
nil,
"Good Octets Transmitted",
nil,
nil,
nil,
"Receive No Buffers",
"Receive Undersize",
"Receive Fragment",
"Receive Oversize",
"Receive Jabber",
nil,
nil,
nil,
"Total Octets Received",
nil,
"Total Octets Transmitted",
nil,
"Total Packets Received",
"Total Packets Transmitted",
"Packets Transmitted (64 Bytes)",
"Packets Transmitted (65-127 Bytes)",
"Packets Transmitted (128-255 Bytes)",
"Packets Transmitted (256-511 Bytes)",
"Packets Transmitted (512-1023 Bytes)",
"Packets Transmitted (1024-1522 Bytes)",
"Multicast Packets Transmitted",
"Broadcast Packets Transmitted",
"TCP Segmentation Context Transmitted",
"TCP Segmentation Context Fail",
};
static void
txstart(Ether *edev)
{
int tdh, tdt, len, olen;
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
len = olen = BLEN(bp);
if (len < ETHERMINTU) {
if (bp->rp + ETHERMINTU <= bp->lim)
bp->wp = bp->rp + ETHERMINTU;
else
bp->wp = bp->lim;
len = BLEN(bp);
print("txstart: extended short pkt %d -> %d bytes\n",
olen, len);
}
tdesc = &ctlr->tdba[tdt];
tdesc->addr[0] = PCIWADDR(bp->rp);
tdesc->addr[1] = 0;
tdesc->control =  Rs|Dext|Ifcs|Teop|DtypeDD|len;
tdesc->status = 0;
ctlr->tb[tdt] = bp;
}
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
igbeim(ctlr, Txdw);
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
igbetransmit(Ether* edev)
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
if(tdesc->status & Tu){
ctlr->ett++;
csr32w(ctlr, Ett, ctlr->ett);
}
tdesc->status = 0;
if(ctlr->tb[tdh] != nil){
freeb(ctlr->tb[tdh]);
ctlr->tb[tdh] = nil;
}
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
igbereplenish(Ctlr* ctlr)
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
}
}
static void
igbeinterrupt(Ureg*, void* arg)
{
Block *bp;
Ctlr *ctlr;
Ether *edev;
Rdesc *rdesc;
int icr, im, rdh, txdw = 0;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
im = ctlr->im;
for(icr = csr32r(ctlr, Icr); icr & ctlr->im; icr = csr32r(ctlr, Icr)){
if(icr & (Rxseq|Lsc)){
}
rdh = ctlr->rdh;
for(;;){
rdesc = &ctlr->rdba[rdh];
if(!(rdesc->status & Rdd))
break;
if ((rdesc->status & Reop) && rdesc->errors == 0) {
bp = ctlr->rb[rdh];
ctlr->rb[rdh] = nil;
bp->wp += rdesc->length ;
toringbuf(edev, bp);
freeb(bp);
} else if ((rdesc->status & Reop) && rdesc->errors)
print("igbe: input packet error 0x%ux\n",
rdesc->errors);
rdesc->status = 0;
rdh = NEXT(rdh, Nrdesc);
}
ctlr->rdh = rdh;
if(icr & Rxdmt0)
igbereplenish(ctlr);
if(icr & Txdw){
im &= ~Txdw;
txdw++;
}
}
ctlr->im = im;
csr32w(ctlr, Ims, im);
iunlock(&ctlr->imlock);
if(txdw)
igbetransmit(edev);
}
static int
igbeinit(Ether* edev)
{
int csr, i, r, ctrl;
MiiPhy *phy;
Ctlr *ctlr;
ctlr = edev->ctlr;
csr = (edev->ea[3]<<24)|(edev->ea[2]<<16)|(edev->ea[1]<<8)|edev->ea[0];
csr32w(ctlr, Ral, csr);
csr = 0x80000000|(edev->ea[5]<<8)|edev->ea[4];
csr32w(ctlr, Rah, csr);
for(i = 1; i < 16; i++){
csr32w(ctlr, Ral+i*8, 0);
csr32w(ctlr, Rah+i*8, 0);
}
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta+i*4, 0);
csr32w(ctlr, Rctl, 0);
ctlr->rdba = xspanalloc(Nrdesc*sizeof(Rdesc), 128 , 0);
csr32w(ctlr, Rdbal, PCIWADDR(ctlr->rdba));
csr32w(ctlr, Rdbah, 0);
csr32w(ctlr, Rdlen, Nrdesc*sizeof(Rdesc));
ctlr->rdh = 0;
csr32w(ctlr, Rdh, ctlr->rdh);
ctlr->rdt = 0;
csr32w(ctlr, Rdt, ctlr->rdt);
ctlr->rb = malloc(sizeof(Block*)*Nrdesc);
igbereplenish(ctlr);
csr32w(ctlr, Rdtr, 0);
switch(ctlr->id){
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82546gb:
case i82546eb:
case i82547gi:
csr32w(ctlr, Radv, 64);
break;
}
csr32w(ctlr, Rxdctl, (8<<WthreshSHIFT)|(8<<HthreshSHIFT)|4);
csr32w(ctlr, Rxcsum, Tuofl|Ipofl|(ETHERHDRSIZE<<PcssSHIFT));
csr32w(ctlr, Rctl, Dpf|Bsize2048|Bam|RdtmsHALF);
igbeim(ctlr, Rxt0|Rxo|Rxdmt0|Rxseq);
csr32w(ctlr, Tctl, (0x0F<<CtSHIFT)|Psp|(66<<ColdSHIFT));
switch(ctlr->id){
default:
r = 6;
break;
case i82543gc:
case i82544ei:
case i82547ei:
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82546gb:
case i82546eb:
case i82547gi:
r = 8;
break;
}
csr32w(ctlr, Tipg, (6<<20)|(8<<10)|r);
csr32w(ctlr, Ait, 0);
csr32w(ctlr, Txdmac, 0);
csr32w(ctlr, Tidv, 128);
ctlr->tdba = xspanalloc(Ntdesc*sizeof(Tdesc), 128 , 0);
csr32w(ctlr, Tdbal, PCIWADDR(ctlr->tdba));
csr32w(ctlr, Tdbah, 0);
csr32w(ctlr, Tdlen, Ntdesc*sizeof(Tdesc));
ctlr->tdh = 0;
csr32w(ctlr, Tdh, ctlr->tdh);
ctlr->tdt = 0;
csr32w(ctlr, Tdt, ctlr->tdt);
ctlr->tb = malloc(sizeof(Block*)*Ntdesc);
r = (4<<WthreshSHIFT)|(4<<HthreshSHIFT)|(8<<PthreshSHIFT);
switch(ctlr->id){
default:
break;
case i82540em:
case i82540eplp:
case i82547gi:
case i82541pi:
case i82546gb:
case i82546eb:
case i82541gi:
case i82541gi2:
r = csr32r(ctlr, Txdctl);
r &= ~WthreshMASK;
r |= Gran|(4<<WthreshSHIFT);
csr32w(ctlr, Tadv, 64);
break;
}
csr32w(ctlr, Txdctl, r);
r = csr32r(ctlr, Tctl);
r |= Ten;
csr32w(ctlr, Tctl, r);
if(ctlr->mii == nil || ctlr->mii->curphy == nil) {
print("igbe: no mii (yet)\n");
return 0;
}
if (miistatus(ctlr->mii) < 0)
return -1;
print("igbe: phy: ");
phy = ctlr->mii->curphy;
if (phy->fd)
print("full duplex");
else
print("half duplex");
print(", %d Mb/s\n", phy->speed);
ctrl = csr32r(ctlr, Ctrl);
if(phy->rfc)
ctrl |= Rfce;
if(phy->tfc)
ctrl |= Tfce;
csr32w(ctlr, Ctrl, ctrl);
return 0;
}
static int
i82543mdior(Ctlr* ctlr, int n)
{
int ctrl, data, i, r;
ctrl = csr32r(ctlr, Ctrl);
r = (ctrl & ~Mddo)|Mdco;
data = 0;
for(i = n-1; i >= 0; i--){
if(csr32r(ctlr, Ctrl) & Mdd)
data |= (1<<i);
csr32w(ctlr, Ctrl, Mdc|r);
csr32w(ctlr, Ctrl, r);
}
csr32w(ctlr, Ctrl, ctrl);
return data;
}
static int
i82543mdiow(Ctlr* ctlr, int bits, int n)
{
int ctrl, i, r;
ctrl = csr32r(ctlr, Ctrl);
r = Mdco|Mddo|ctrl;
for(i = n-1; i >= 0; i--){
if(bits & (1<<i))
r |= Mdd;
else
r &= ~Mdd;
csr32w(ctlr, Ctrl, Mdc|r);
csr32w(ctlr, Ctrl, r);
}
csr32w(ctlr, Ctrl, ctrl);
return 0;
}
static int
i82543miimir(Mii* mii, int pa, int ra)
{
int data;
Ctlr *ctlr;
ctlr = mii->ctlr;
i82543mdiow(ctlr, 0xFFFFFFFF, 32);
i82543mdiow(ctlr, 0x1800|(pa<<5)|ra, 14);
data = i82543mdior(ctlr, 18);
if(data & 0x10000)
return -1;
return data & 0xFFFF;
}
static int
i82543miimiw(Mii* mii, int pa, int ra, int data)
{
Ctlr *ctlr;
ctlr = mii->ctlr;
i82543mdiow(ctlr, 0xFFFFFFFF, 32);
data &= 0xFFFF;
data |= (0x05<<(5+5+2+16))|(pa<<(5+2+16))|(ra<<(2+16))|(0x02<<16);
i82543mdiow(ctlr, data, 32);
return 0;
}
static int
igbemiimir(Mii* mii, int pa, int ra)
{
Ctlr *ctlr;
int mdic, timo;
ctlr = mii->ctlr;
csr32w(ctlr, Mdic, MDIrop|(pa<<MDIpSHIFT)|(ra<<MDIrSHIFT));
mdic = 0;
for(timo = 64; timo; timo--){
mdic = csr32r(ctlr, Mdic);
if(mdic & (MDIe|MDIready))
break;
microdelay(1);
}
if((mdic & (MDIe|MDIready)) == MDIready)
return mdic & 0xFFFF;
return -1;
}
static int
igbemiimiw(Mii* mii, int pa, int ra, int data)
{
Ctlr *ctlr;
int mdic, timo;
ctlr = mii->ctlr;
data &= MDIdMASK;
csr32w(ctlr, Mdic, MDIwop|(pa<<MDIpSHIFT)|(ra<<MDIrSHIFT)|data);
mdic = 0;
for(timo = 64; timo; timo--){
mdic = csr32r(ctlr, Mdic);
if(mdic & (MDIe|MDIready))
break;
microdelay(1);
}
if((mdic & (MDIe|MDIready)) == MDIready)
return 0;
return -1;
}
static int
igbemii(Ctlr* ctlr)
{
MiiPhy *phy = (MiiPhy *)1;
int ctrl, p, r;
USED(phy);
r = csr32r(ctlr, Status);
if(r & Tbimode)
return -1;
if((ctlr->mii = malloc(sizeof(Mii))) == nil)
return -1;
ctlr->mii->ctlr = ctlr;
ctrl = csr32r(ctlr, Ctrl);
ctrl |= Slu;
switch(ctlr->id){
case i82543gc:
ctrl |= Frcdplx|Frcspd;
csr32w(ctlr, Ctrl, ctrl);
r = csr32r(ctlr, Ctrlext);
if(!(r & Mdro))
return -1;
csr32w(ctlr, Ctrlext, r);
delay(20);
r = csr32r(ctlr, Ctrlext);
r &= ~Mdr;
csr32w(ctlr, Ctrlext, r);
delay(20);
r = csr32r(ctlr, Ctrlext);
r |= Mdr;
csr32w(ctlr, Ctrlext, r);
delay(20);
ctlr->mii->mir = i82543miimir;
ctlr->mii->miw = i82543miimiw;
break;
case i82544ei:
case i82547ei:
case i82540em:
case i82540eplp:
case i82547gi:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82546gb:
case i82546eb:
ctrl &= ~(Frcdplx|Frcspd);
csr32w(ctlr, Ctrl, ctrl);
ctlr->mii->mir = igbemiimir;
ctlr->mii->miw = igbemiimiw;
break;
default:
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
if(mii(ctlr->mii, ~0) == 0 || (phy = ctlr->mii->curphy) == nil){
if (0)
print("phy trouble: phy = 0x%lux\n", (ulong)phy);
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
if (Debug)
print("oui %X phyno %d\n", phy->oui, phy->phyno);
else
USED(phy);
switch(ctlr->id){
case i82547gi:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82546gb:
case i82546eb:
break;
default:
r = miimir(ctlr->mii, 16);
r |= 0x0800;
r |= 0x0060;
r |= 0x0002;
miimiw(ctlr->mii, 16, r);
r = miimir(ctlr->mii, 20);
r |= 0x0070;
r &= ~0x0F00;
r |= 0x0100;
miimiw(ctlr->mii, 20, r);
miireset(ctlr->mii);
break;
}
p = 0;
if(ctlr->txcw & TxcwPs)
p |= AnaP;
if(ctlr->txcw & TxcwAs)
p |= AnaAP;
miiane(ctlr->mii, ~0, p, ~0);
return 0;
}
static int
at93c46io(Ctlr* ctlr, char* op, int data)
{
char *lp, *p;
int i, loop, eecd, r;
eecd = csr32r(ctlr, Eecd);
r = 0;
loop = -1;
lp = nil;
for(p = op; *p != '\0'; p++){
switch(*p){
default:
return -1;
case ' ':
continue;
case ':':
loop = strtol(p+1, &lp, 0)-1;
lp--;
if(p == lp)
loop = 7;
p = lp;
continue;
case ';':
if(lp == nil)
return -1;
loop--;
if(loop >= 0)
p = lp;
else
lp = nil;
continue;
case 'C':
eecd |= Sk;
break;
case 'c':
eecd &= ~Sk;
break;
case 'D':
if(loop < 0)
return -1;
if(data & (1<<loop))
eecd |= Di;
else
eecd &= ~Di;
break;
case 'O':
i = (csr32r(ctlr, Eecd) & Do) != 0;
if(loop >= 0)
r |= (i<<loop);
else
r = i;
continue;
case 'I':
eecd |= Di;
break;
case 'i':
eecd &= ~Di;
break;
case 'S':
eecd |= Cs;
break;
case 's':
eecd &= ~Cs;
break;
}
csr32w(ctlr, Eecd, eecd);
microdelay(50);
}
if(loop >= 0)
return -1;
return r;
}
static int
at93c46r(Ctlr* ctlr)
{
ushort sum;
char rop[20];
int addr, areq, bits, data, eecd, i;
eecd = csr32r(ctlr, Eecd);
if(eecd & Spi){
print("igbe: SPI EEPROM access not implemented\n");
return 0;
}
if(eecd & (Eeszaddr|Eesz256))
bits = 8;
else
bits = 6;
sum = 0;
switch(ctlr->id){
default:
areq = 0;
break;
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82547gi:
case i82546gb:
case i82546eb:
areq = 1;
csr32w(ctlr, Eecd, eecd|Areq);
for(i = 0; i < 1000; i++){
if((eecd = csr32r(ctlr, Eecd)) & Agnt)
break;
microdelay(5);
}
if(!(eecd & Agnt)){
print("igbe: not granted EEPROM access\n");
goto release;
}
break;
}
snprint(rop, sizeof(rop), "S :%dDCc;", bits+3);
for(addr = 0; addr < 0x40; addr++){
if(at93c46io(ctlr, rop, (0x06<<bits)|addr) != 0){
print("igbe: can't set EEPROM address 0x%2.2X\n", addr);
goto release;
}
data = at93c46io(ctlr, ":16COc;", 0);
at93c46io(ctlr, "sic", 0);
ctlr->eeprom[addr] = data;
sum += data;
if (Debug) {
if(addr && ((addr & 0x07) == 0))
print("\n");
print(" %4.4ux", data);
}
}
if (Debug)
print("\n");
release:
if(areq)
csr32w(ctlr, Eecd, eecd & ~Areq);
return sum;
}
static void
detach(Ctlr *ctlr)
{
int r;
csr32w(ctlr, Imc, ~0);
csr32w(ctlr, Rctl, 0);
csr32w(ctlr, Tctl, 0);
delay(20);
csr32w(ctlr, Ctrl, Devrst);
delay(1);
while(csr32r(ctlr, Ctrl) & Devrst)
;
csr32w(ctlr, Ctrlext, Eerst | csr32r(ctlr, Ctrlext));
delay(1);
while(csr32r(ctlr, Ctrlext) & Eerst)
;
switch(ctlr->id){
default:
break;
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82547gi:
case i82546gb:
case i82546eb:
r = csr32r(ctlr, Manc);
r &= ~Arpen;
csr32w(ctlr, Manc, r);
break;
}
csr32w(ctlr, Imc, ~0);
delay(1);
while(csr32r(ctlr, Icr))
;
}
static void
igbedetach(Ether *edev)
{
detach(edev->ctlr);
}
static void
igbeshutdown(Ether* ether)
{
print("igbeshutdown\n");
igbedetach(ether);
}
static int
igbereset(Ctlr* ctlr)
{
int ctrl, i, pause, r, swdpio, txcw;
detach(ctlr);
r = at93c46r(ctlr);
if (r != 0 && r != 0xBABA){
print("igbe: bad EEPROM checksum - 0x%4.4uX\n", r);
return -1;
}
if ((ctlr->id == i82546gb || ctlr->id == i82546eb) &&
BUSFNO(ctlr->pcidev->tbdf) == 1)
ctlr->eeprom[Ea+2] += 0x100;
for(i = Ea; i < Eaddrlen/2; i++){
ctlr->ra[2*i]   = ctlr->eeprom[i];
ctlr->ra[2*i+1] = ctlr->eeprom[i]>>8;
}
if (ctlr->id != i82543gc) {
r = (csr32r(ctlr, Status) & Lanid) >> 2;
ctlr->ra[5] += r;
}
r = (ctlr->ra[3]<<24)|(ctlr->ra[2]<<16)|(ctlr->ra[1]<<8)|ctlr->ra[0];
csr32w(ctlr, Ral, r);
r = 0x80000000|(ctlr->ra[5]<<8)|ctlr->ra[4];
csr32w(ctlr, Rah, r);
for(i = 1; i < 16; i++){
csr32w(ctlr, Ral+i*8, 0);
csr32w(ctlr, Rah+i*8, 0);
}
memset(ctlr->mta, 0, sizeof(ctlr->mta));
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta+i*4, 0);
if (ctlr->id == i82543gc) {
txcw = csr32r(ctlr, Txcw);
txcw &= ~(TxcwAne|TxcwPauseMASK|TxcwFd);
ctrl = csr32r(ctlr, Ctrl);
ctrl &= ~(SwdpioloMASK|Frcspd|Ilos|Lrst|Fd);
if(ctlr->eeprom[Icw1] & 0x0400){
ctrl |= Fd;
txcw |= TxcwFd;
}
if(ctlr->eeprom[Icw1] & 0x0200)
ctrl |= Lrst;
if(ctlr->eeprom[Icw1] & 0x0010)
ctrl |= Ilos;
if(ctlr->eeprom[Icw1] & 0x0800)
ctrl |= Frcspd;
swdpio = (ctlr->eeprom[Icw1] & 0x01E0)>>5;
ctrl |= swdpio<<SwdpioloSHIFT;
csr32w(ctlr, Ctrl, ctrl);
ctrl = csr32r(ctlr, Ctrlext);
ctrl &= ~(Ips|SwdpiohiMASK);
swdpio = (ctlr->eeprom[Icw2] & 0x00F0)>>4;
if(ctlr->eeprom[Icw1] & 0x1000)
ctrl |= Ips;
ctrl |= swdpio<<SwdpiohiSHIFT;
csr32w(ctlr, Ctrlext, ctrl);
if(ctlr->eeprom[Icw2] & 0x0800)
txcw |= TxcwAne;
pause = (ctlr->eeprom[Icw2] & 0x3000)>>12;
txcw |= pause<<TxcwPauseSHIFT;
switch(pause){
default:
ctlr->fcrtl = 0x00002000;
ctlr->fcrth = 0x00004000;
txcw |= TxcwAs|TxcwPs;
break;
case 0:
ctlr->fcrtl = 0x00002000;
ctlr->fcrth = 0x00004000;
break;
case 2:
ctlr->fcrtl = 0;
ctlr->fcrth = 0;
txcw |= TxcwAs;
break;
}
ctlr->txcw = txcw;
csr32w(ctlr, Txcw, txcw);
}
csr32w(ctlr, Fcal, 0x00C28001);
csr32w(ctlr, Fcah, 0x00000100);
csr32w(ctlr, Fct, 0x00008808);
csr32w(ctlr, Fcttv, 0x00000100);
csr32w(ctlr, Fcrtl, ctlr->fcrtl);
csr32w(ctlr, Fcrth, ctlr->fcrth);
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
ctlr->im = 0;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
if(!(csr32r(ctlr, Status) & Tbimode) && igbemii(ctlr) < 0) {
print("igbe: igbemii failed\n");
return -1;
}
return 0;
}
static void
igbepci(void)
{
int port, cls;
Pcidev *p;
Ctlr *ctlr;
static int first = 1;
if (first)
first = 0;
else
return;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
case i82542:
default:
continue;
case (0x1001<<16)|0x8086:
break;
case i82543gc:
case i82544ei:
case i82547ei:
case i82540em:
case i82540eplp:
case i82547gi:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82546gb:
case i82546eb:
break;
}
port = upamalloc(p->mem[0].bar & ~0x0F, p->mem[0].size, 0);
if(port == 0){
print("igbe: can't map %d @ 0x%8.8luX\n",
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
print("igbe: unexpected CLS - %d bytes\n",
cls*sizeof(long));
break;
case 0x00:
case 0xFF:
print("igbe: unusable PciCLS: %d, using %d longs\n",
cls, CACHELINESZ/sizeof(long));
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
ctlr->id = (p->did<<16)|p->vid;
ctlr->cls = cls*4;
ctlr->nic = KADDR(ctlr->port);
if (Debug)
print("status0 %8.8uX\n", csr32r(ctlr, Status));
if(igbereset(ctlr)){
free(ctlr);
continue;
}
if (Debug)
print("status1 %8.8uX\n", csr32r(ctlr, Status));
pcisetbme(p);
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
}
}
int
igbepnp(Ether* edev)
{
int i;
Ctlr *ctlr;
uchar ea[Eaddrlen];
if(ctlrhead == nil)
igbepci();
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
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0){
for(i = 0; i < Eaddrlen/2; i++){
edev->ea[2*i] = ctlr->eeprom[i];
edev->ea[2*i+1] = ctlr->eeprom[i]>>8;
}
}
igbeinit(edev);
edev->attach = igbeattach;
edev->transmit = igbetransmit;
edev->interrupt = igbeinterrupt;
edev->detach = igbedetach;
return 0;
}