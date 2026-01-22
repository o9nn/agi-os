#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
#include "etherif.h"
enum {
Ctrl		= 0x0000,
Status		= 0x0008,
Eec		= 0x0010,
Eerd		= 0x0014,
Ctrlext		= 0x0018,
Fla		= 0x001c,
Mdic		= 0x0020,
Seresctl	= 0x0024,
Fcal		= 0x0028,
Fcah		= 0x002C,
Fct		= 0x0030,
Kumctrlsta	= 0x0034,
Vet		= 0x0038,
Fcttv		= 0x0170,
Txcw		= 0x0178,
Rxcw		= 0x0180,
Ledctl		= 0x0E00,
Pba		= 0x1000,
Pbs		= 0x1008,
Icr		= 0x00C0,
Itr		= 0x00c4,
Ics		= 0x00C8,
Ims		= 0x00D0,
Imc		= 0x00D8,
Iam		= 0x00E0,
Rctl		= 0x0100,
Ert		= 0x2008,
Fcrtl		= 0x2160,
Fcrth		= 0x2168,
Psrctl		= 0x2170,
Rdbal		= 0x2800,
Rdbah		= 0x2804,
Rdlen		= 0x2808,
Rdh		= 0x2810,
Rdt		= 0x2818,
Rdtr		= 0x2820,
Rxdctl		= 0x2828,
Radv		= 0x282C,
Rdbal1		= 0x2900,
Rdbah1		= 0x2804,
Rdlen1		= 0x2908,
Rdh1		= 0x2910,
Rdt1		= 0x2918,
Rxdctl1		= 0x2928,
Rsrpd		= 0x2c00,
Raid		= 0x2c08,
Cpuvec		= 0x2c10,
Rxcsum		= 0x5000,
Rfctl		= 0x5008,
Mta		= 0x5200,
Ral		= 0x5400,
Rah		= 0x5404,
Vfta		= 0x5600,
Mrqc		= 0x5818,
Rssim		= 0x5864,
Rssir		= 0x5868,
Reta		= 0x5c00,
Rssrk		= 0x5c80,
Tctl		= 0x0400,
Tipg		= 0x0410,
Tkabgtxd	= 0x3004,
Tdbal		= 0x3800,
Tdbah		= 0x3804,
Tdlen		= 0x3808,
Tdh		= 0x3810,
Tdt		= 0x3818,
Tidv		= 0x3820,
Txdctl		= 0x3828,
Tadv		= 0x382C,
Tarc0		= 0x3840,
Tdbal1		= 0x3900,
Tdbah1		= 0x3904,
Tdlen1		= 0x3908,
Tdh1		= 0x3910,
Tdt1		= 0x3918,
Txdctl1		= 0x3928,
Tarc1		= 0x3940,
Statistics	= 0x4000,
Gorcl		= 0x88/4,
Gotcl		= 0x90/4,
Torl		= 0xC0/4,
Totl		= 0xC8/4,
Nstatistics	= 0x124/4,
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
Phyrst		= 1<<31,
};
enum {
Lu		= 1<<1,
Lanid		= 3<<2,
Txoff		= 1<<4,
Tbimode		= 1<<5,
Phyra		= 1<<10,
GIOme		= 1<<19,
};
enum {
EEstart		= 1<<0,
EEdone		= 1<<1,
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
Phyctl		= 0,
Physsr		= 17,
Phyier		= 18,
Phyisr		= 19,
Phylhr		= 19,
Rtlink		= 1<<10,
Phyan		= 1<<11,
Ran		= 1<<9,
Ean		= 1<<12,
Lscie		= 1<<10,
Ancie		= 1<<11,
Spdie		= 1<<14,
Panie		= 1<<15,
Anf		= 1<<6,
Ane		= 1<<15,
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
Bsize16384	= 0x00010000,
Bsize8192	= 0x00020000,
Bsize2048	= 0x00000000,
Bsize1024	= 0x00010000,
Bsize512	= 0x00020000,
Bsize256	= 0x00030000,
BsizeFlex	= 0x08000000,
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
Qenable		= 0x02000000,
};
enum {
PcssMASK	= 0x00FF,
PcssSHIFT	= 0,
Ipofl		= 0x0100,
Tuofl		= 0x0200,
};
enum {
DelayMASK	= 0xFFFF,
DelaySHIFT	= 0,
Fpd		= 0x80000000,
};
typedef struct Ctlr Ctlr;
typedef struct Rd Rd;
typedef struct Td Td;
struct Rd {
u32int	addr[2];
u16int	length;
u16int	checksum;
u8int	status;
u8int	errors;
u16int	special;
};
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
struct Td {
u32int	addr[2];
u32int	control;
u32int	status;
};
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
Tdd		= 0x0001,
Ec		= 0x0002,
Lc		= 0x0004,
Tu		= 0x0008,
CssMASK		= 0xFF00,
CssSHIFT	= 8,
};
typedef struct {
u16int	*reg;
u32int	*reg32;
u16int	base;
u16int	lim;
} Flash;
enum {
Bfpr	= 0x00/4,
Fsts	= 0x04/2,
Fctl	= 0x06/2,
Faddr	= 0x08/4,
Fdata	= 0x10/4,
Fdone	= 1<<0,
Fcerr	= 1<<1,
Ael	= 1<<2,
Scip	= 1<<5,
Fvalid	= 1<<14,
Fgo	= 1<<0,
Flcycle	= 1<<1,
Fdbc	= 1<<8,
};
enum {
Nrd		= 128,
Nrb		= 512,
Ntd		= 32,
};
enum {
Iany,
i82563,
i82566,
i82567,
i82571,
i82572,
i82573,
i82574,
i82575,
i82576,
i82577,
i82579,
};
static int rbtab[] = {
0,
9014,
ETHERMAXTU,
ETHERMAXTU,
9234,
9234,
8192,
ETHERMAXTU,
ETHERMAXTU,
ETHERMAXTU,
ETHERMAXTU,
9018,
};
static char *tname[] = {
"any",
"i82563",
"i82566",
"i82567",
"i82571",
"i82572",
"i82573",
"i82574",
"i82575",
"i82576",
"i82577",
"i82579",
};
struct Ctlr {
int	port;
Pcidev	*pcidev;
Ctlr	*next;
Ether	*edev;
int	active;
int	type;
ushort	eeprom[0x40];
QLock	alock;
int	attached;
int	nrd;
int	ntd;
int	nrb;
unsigned rbsz;
int	*nic;
Lock	imlock;
int	im;
Rendez	lrendez;
int	lim;
Watermark wmrb;
Watermark wmrd;
Watermark wmtd;
QLock	slock;
uint	statistics[Nstatistics];
uint	lsleep;
uint	lintr;
uint	rsleep;
uint	rintr;
uint	txdw;
uint	tintr;
uint	ixsm;
uint	ipcs;
uint	tcpcs;
uint	speeds[4];
uchar	ra[Eaddrlen];
ulong	mta[128];
Rendez	rrendez;
int	rim;
int	rdfree;
Rd	*rdba;
Block	**rb;
int	rdh;
int	rdt;
int	rdtr;
int	radv;
Rendez	trendez;
QLock	tlock;
Td	*tdba;
Block	**tb;
int	tdh;
int	tdt;
int	fcrtl;
int	fcrth;
uint	pba;
};
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static Ctlr* i82563ctlrhead;
static Ctlr* i82563ctlrtail;
static Lock i82563rblock;
static Block* i82563rbpool;
static int nrbfull;
static char* statistics[] = {
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
"Packets Received (1024-mtu Bytes)",
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
"Management Packets Rx",
"Management Packets Drop",
"Management Packets Tx",
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
"Packets Transmitted (1024-mtu Bytes)",
"Multicast Packets Transmitted",
"Broadcast Packets Transmitted",
"TCP Segmentation Context Transmitted",
"TCP Segmentation Context Fail",
"Interrupt Assertion",
"Interrupt Rx Pkt Timer",
"Interrupt Rx Abs Timer",
"Interrupt Tx Pkt Timer",
"Interrupt Tx Abs Timer",
"Interrupt Tx Queue Empty",
"Interrupt Tx Desc Low",
"Interrupt Rx Min",
"Interrupt Rx Overrun",
};
static long
i82563ifstat(Ether* edev, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *s, *p, *e, *stat;
int i, r;
uvlong tuvl, ruvl;
ctlr = edev->ctlr;
qlock(&ctlr->slock);
p = s = malloc(READSTR);
if(p == nil) {
qunlock(&ctlr->slock);
error(Enomem);
}
e = p + READSTR;
for(i = 0; i < Nstatistics; i++){
r = csr32r(ctlr, Statistics + i*4);
if((stat = statistics[i]) == nil)
continue;
switch(i){
case Gorcl:
case Gotcl:
case Torl:
case Totl:
ruvl = r;
ruvl += (uvlong)csr32r(ctlr, Statistics+(i+1)*4) << 32;
tuvl = ruvl;
tuvl += ctlr->statistics[i];
tuvl += (uvlong)ctlr->statistics[i+1] << 32;
if(tuvl == 0)
continue;
ctlr->statistics[i] = tuvl;
ctlr->statistics[i+1] = tuvl >> 32;
p = seprint(p, e, "%s: %llud %llud\n", stat, tuvl, ruvl);
i++;
break;
default:
ctlr->statistics[i] += r;
if(ctlr->statistics[i] == 0)
continue;
p = seprint(p, e, "%s: %ud %ud\n", stat,
ctlr->statistics[i], r);
break;
}
}
p = seprint(p, e, "lintr: %ud %ud\n", ctlr->lintr, ctlr->lsleep);
p = seprint(p, e, "rintr: %ud %ud\n", ctlr->rintr, ctlr->rsleep);
p = seprint(p, e, "tintr: %ud %ud\n", ctlr->tintr, ctlr->txdw);
p = seprint(p, e, "ixcs: %ud %ud %ud\n", ctlr->ixsm, ctlr->ipcs, ctlr->tcpcs);
p = seprint(p, e, "rdtr: %ud\n", ctlr->rdtr);
p = seprint(p, e, "radv: %ud\n", ctlr->radv);
p = seprint(p, e, "ctrl: %.8ux\n", csr32r(ctlr, Ctrl));
p = seprint(p, e, "ctrlext: %.8ux\n", csr32r(ctlr, Ctrlext));
p = seprint(p, e, "status: %.8ux\n", csr32r(ctlr, Status));
p = seprint(p, e, "txcw: %.8ux\n", csr32r(ctlr, Txcw));
p = seprint(p, e, "txdctl: %.8ux\n", csr32r(ctlr, Txdctl));
p = seprint(p, e, "pba: %.8ux\n", ctlr->pba);
p = seprint(p, e, "speeds: 10:%ud 100:%ud 1000:%ud ?:%ud\n",
ctlr->speeds[0], ctlr->speeds[1], ctlr->speeds[2], ctlr->speeds[3]);
p = seprint(p, e, "type: %s\n", tname[ctlr->type]);
p = seprint(p, e, "nrbfull (rcv blocks outstanding): %d\n", nrbfull);
p = seprintmark(p, e, &ctlr->wmrb);
p = seprintmark(p, e, &ctlr->wmrd);
p = seprintmark(p, e, &ctlr->wmtd);
USED(p);
n = readstr(offset, a, n, s);
free(s);
qunlock(&ctlr->slock);
return n;
}
enum {
CMrdtr,
CMradv,
};
static Cmdtab i82563ctlmsg[] = {
CMrdtr,	"rdtr",	2,
CMradv,	"radv",	2,
};
static long
i82563ctl(Ether* edev, void* buf, long n)
{
ulong v;
char *p;
Ctlr *ctlr;
Cmdbuf *cb;
Cmdtab *ct;
if((ctlr = edev->ctlr) == nil)
error(Enonexist);
cb = parsecmd(buf, n);
if(waserror()){
free(cb);
nexterror();
}
ct = lookupcmd(cb, i82563ctlmsg, nelem(i82563ctlmsg));
switch(ct->index){
case CMrdtr:
v = strtoul(cb->f[1], &p, 0);
if(p == cb->f[1] || v > 0xFFFF)
error(Ebadarg);
ctlr->rdtr = v;
csr32w(ctlr, Rdtr, v);
break;
case CMradv:
v = strtoul(cb->f[1], &p, 0);
if(p == cb->f[1] || v > 0xFFFF)
error(Ebadarg);
ctlr->radv = v;
csr32w(ctlr, Radv, v);
}
free(cb);
poperror();
return n;
}
static void
i82563promiscuous(void* arg, int on)
{
int rctl;
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
rctl = csr32r(ctlr, Rctl);
rctl &= ~MoMASK;
if(on)
rctl |= Upe|Mpe;
else
rctl &= ~(Upe|Mpe);
csr32w(ctlr, Rctl, rctl);
}
static void
i82563multicast(void* arg, uchar* addr, int on)
{
int bit, x;
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
x = addr[5]>>1;
if(ctlr->type == i82566 || ctlr->type == i82567)
x &= 31;
bit = ((addr[5] & 1)<<4)|(addr[4]>>4);
if(on)
ctlr->mta[x] |= 1<<bit;
csr32w(ctlr, Mta+x*4, ctlr->mta[x]);
}
static Block*
i82563rballoc(void)
{
Block *bp;
ilock(&i82563rblock);
if((bp = i82563rbpool) != nil){
i82563rbpool = bp->next;
bp->next = nil;
_xinc(&bp->ref);
}
iunlock(&i82563rblock);
return bp;
}
static void
i82563rbfree(Block* b)
{
b->rp = b->wp = (uchar*)PGROUND((uintptr)b->base);
b->flag &= ~(Bipck | Budpck | Btcpck | Bpktck);
ilock(&i82563rblock);
b->next = i82563rbpool;
i82563rbpool = b;
nrbfull--;
iunlock(&i82563rblock);
}
static void
i82563im(Ctlr* ctlr, int im)
{
ilock(&ctlr->imlock);
ctlr->im |= im;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
}
static void
i82563txinit(Ctlr* ctlr)
{
int i, r;
Block *bp;
csr32w(ctlr, Tctl, 0x0F<<CtSHIFT | Psp | 66<<ColdSHIFT | Mulr);
csr32w(ctlr, Tipg, 6<<20 | 8<<10 | 8);
csr32w(ctlr, Tdbal, PCIWADDR(ctlr->tdba));
csr32w(ctlr, Tdbah, 0);
csr32w(ctlr, Tdlen, ctlr->ntd * sizeof(Td));
ctlr->tdh = PREV(0, ctlr->ntd);
csr32w(ctlr, Tdh, 0);
ctlr->tdt = 0;
csr32w(ctlr, Tdt, 0);
for(i = 0; i < ctlr->ntd; i++){
if((bp = ctlr->tb[i]) != nil){
ctlr->tb[i] = nil;
freeb(bp);
}
memset(&ctlr->tdba[i], 0, sizeof(Td));
}
csr32w(ctlr, Tidv, 128);
r = csr32r(ctlr, Txdctl);
r &= ~(WthreshMASK|PthreshMASK);
r |= 4<<WthreshSHIFT | 4<<PthreshSHIFT;
if(ctlr->type == i82575 || ctlr->type == i82576)
r |= Qenable;
csr32w(ctlr, Tadv, 64);
csr32w(ctlr, Txdctl, r);
r = csr32r(ctlr, Tctl);
r |= Ten;
csr32w(ctlr, Tctl, r);
}
#define Next(x, m)	(((x)+1) & (m))
static int
i82563cleanup(Ctlr *ctlr)
{
Block *b;
int tdh, m, n;
tdh = ctlr->tdh;
m = ctlr->ntd-1;
while(ctlr->tdba[n = Next(tdh, m)].status & Tdd){
tdh = n;
if((b = ctlr->tb[tdh]) != nil){
ctlr->tb[tdh] = nil;
freeb(b);
}else
iprint("82563 tx underrun!\n");
ctlr->tdba[tdh].status = 0;
}
return ctlr->tdh = tdh;
}
static void
i82563transmit(Ether* edev)
{
Td *td;
Block *bp;
Ctlr *ctlr;
int tdh, tdt, m;
ctlr = edev->ctlr;
qlock(&ctlr->tlock);
tdh = i82563cleanup(ctlr);
tdt = ctlr->tdt;
m = ctlr->ntd-1;
for(;;){
if(Next(tdt, m) == tdh){
ctlr->txdw++;
i82563im(ctlr, Txdw);
break;
}
if((bp = qget(edev->oq)) == nil)
break;
td = &ctlr->tdba[tdt];
td->addr[0] = PCIWADDR(bp->rp);
td->control = Ide|Rs|Ifcs|Teop|BLEN(bp);
ctlr->tb[tdt] = bp;
notemark(&ctlr->wmtd, (tdt + Ntd - tdh) % Ntd);
tdt = Next(tdt, m);
}
if(ctlr->tdt != tdt){
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
}
qunlock(&ctlr->tlock);
}
static void
i82563replenish(Ctlr* ctlr)
{
Rd *rd;
int rdt, m;
Block *bp;
rdt = ctlr->rdt;
m = ctlr->nrd-1;
while(Next(rdt, m) != ctlr->rdh){
rd = &ctlr->rdba[rdt];
if(ctlr->rb[rdt] != nil){
print("#l%d: 82563: rx overrun\n", ctlr->edev->ctlrno);
break;
}
bp = i82563rballoc();
if(bp == nil)
panic("#l%d: 82563: all %d rx buffers in use, nrbfull %d",
ctlr->edev->ctlrno, ctlr->nrb, nrbfull);
ctlr->rb[rdt] = bp;
rd->addr[0] = PCIWADDR(bp->rp);
rd->status = 0;
ctlr->rdfree++;
rdt = Next(rdt, m);
}
ctlr->rdt = rdt;
csr32w(ctlr, Rdt, rdt);
}
static void
i82563rxinit(Ctlr* ctlr)
{
Block *bp;
int i, r, rctl;
if(ctlr->rbsz <= 2048)
rctl = Dpf|Bsize2048|Bam|RdtmsHALF;
else if(ctlr->rbsz <= 8192)
rctl = Lpe|Dpf|Bsize8192|Bsex|Bam|RdtmsHALF|Secrc;
else if(ctlr->rbsz <= 12*1024){
i = ctlr->rbsz / 1024;
if(ctlr->rbsz % 1024)
i++;
rctl = Lpe|Dpf|BsizeFlex*i|Bam|RdtmsHALF|Secrc;
}
else
rctl = Lpe|Dpf|Bsize16384|Bsex|Bam|RdtmsHALF|Secrc;
if(ctlr->type == i82575 || ctlr->type == i82576){
csr32w(ctlr, Rctl, Ren|rctl);
r = csr32r(ctlr, Rxdctl);
r |= Qenable;
csr32w(ctlr, Rxdctl, r);
}
csr32w(ctlr, Rctl, rctl);
if(ctlr->type == i82573 || ctlr->type == i82577 || ctlr->type == i82579)
csr32w(ctlr, Ert, 1024/8);
if(ctlr->type == i82566 || ctlr->type == i82567)
csr32w(ctlr, Pbs, 16);
csr32w(ctlr, Rdbal, PCIWADDR(ctlr->rdba));
csr32w(ctlr, Rdbah, 0);
csr32w(ctlr, Rdlen, ctlr->nrd * sizeof(Rd));
ctlr->rdh = 0;
csr32w(ctlr, Rdh, 0);
ctlr->rdt = 0;
csr32w(ctlr, Rdt, 0);
ctlr->radv = ctlr->rdtr = 0;
csr32w(ctlr, Rdtr, ctlr->rdtr);
csr32w(ctlr, Radv, ctlr->radv);
for(i = 0; i < ctlr->nrd; i++){
if((bp = ctlr->rb[i]) != nil){
ctlr->rb[i] = nil;
freeb(bp);
}
}
i82563replenish(ctlr);
if(ctlr->type != i82575 || ctlr->type == i82576){
r = csr32r(ctlr, Rxdctl);
r &= ~(WthreshMASK|PthreshMASK);
r |= (2<<WthreshSHIFT)|(2<<PthreshSHIFT);
csr32w(ctlr, Rxdctl, r);
}
csr32w(ctlr, Rxcsum, 0);
}
static int
i82563rim(void* ctlr)
{
return ((Ctlr*)ctlr)->rim != 0;
}
static void
i82563rproc(void* arg)
{
Rd *rd;
Block *bp;
Ctlr *ctlr;
int r, m, rdh, rim, passed;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
i82563rxinit(ctlr);
r = csr32r(ctlr, Rctl);
r |= Ren;
csr32w(ctlr, Rctl, r);
m = ctlr->nrd-1;
for(;;){
i82563replenish(ctlr);
i82563im(ctlr, Rxt0|Rxo|Rxdmt0|Rxseq|Ack);
ctlr->rsleep++;
sleep(&ctlr->rrendez, i82563rim, ctlr);
rdh = ctlr->rdh;
passed = 0;
for(;;){
rim = ctlr->rim;
ctlr->rim = 0;
rd = &ctlr->rdba[rdh];
if(!(rd->status & Rdd))
break;
bp = ctlr->rb[rdh];
if((rd->status & Reop) && rd->errors == 0){
bp->wp += rd->length;
bp->lim = bp->wp;
if(!(rd->status & Ixsm)){
ctlr->ixsm++;
if(rd->status & Ipcs){
ctlr->ipcs++;
bp->flag |= Bipck;
}
if(rd->status & Tcpcs){
ctlr->tcpcs++;
bp->flag |= Btcpck|Budpck;
}
bp->checksum = rd->checksum;
bp->flag |= Bpktck;
}
ilock(&i82563rblock);
nrbfull++;
iunlock(&i82563rblock);
notemark(&ctlr->wmrb, nrbfull);
etheriq(edev, bp, 1);
passed++;
} else {
if (rd->status & Reop && rd->errors)
print("%s: input packet error %#ux\n",
tname[ctlr->type], rd->errors);
freeb(bp);
}
ctlr->rb[rdh] = nil;
rd->status = 0;
ctlr->rdfree--;
ctlr->rdh = rdh = Next(rdh, m);
if(ctlr->rdfree <= ctlr->nrd - 32 || (rim & Rxdmt0))
i82563replenish(ctlr);
}
notemark(&ctlr->wmrd, passed);
}
}
static int
i82563lim(void* ctlr)
{
return ((Ctlr*)ctlr)->lim != 0;
}
static int speedtab[] = {
10, 100, 1000, 0
};
static uint
phyread(Ctlr *ctlr, int reg)
{
uint phy, i;
csr32w(ctlr, Mdic, MDIrop | 1<<MDIpSHIFT | reg<<MDIrSHIFT);
phy = 0;
for(i = 0; i < 64; i++){
phy = csr32r(ctlr, Mdic);
if(phy & (MDIe|MDIready))
break;
microdelay(1);
}
if((phy & (MDIe|MDIready)) != MDIready)
return ~0;
return phy & 0xffff;
}
static uint
phywrite(Ctlr *ctlr, int reg, ushort val)
{
uint phy, i;
csr32w(ctlr, Mdic, MDIwop | 1<<MDIpSHIFT | reg<<MDIrSHIFT | val);
phy = 0;
for(i = 0; i < 64; i++){
phy = csr32r(ctlr, Mdic);
if(phy & (MDIe|MDIready))
break;
microdelay(1);
}
if((phy & (MDIe|MDIready)) != MDIready)
return ~0;
return 0;
}
static void
i82563lproc(void *v)
{
uint phy, i, a;
Ctlr *ctlr;
Ether *e;
e = v;
ctlr = e->ctlr;
if(ctlr->type == i82573 && (phy = phyread(ctlr, Phyier)) != ~0)
phywrite(ctlr, Phyier, phy | Lscie | Ancie | Spdie | Panie);
for(;;){
phy = phyread(ctlr, Physsr);
if(phy == ~0)
goto next;
i = (phy>>14) & 3;
switch(ctlr->type){
case i82563:
a = phyread(ctlr, Phyisr) & Ane;
break;
case i82571:
case i82572:
case i82575:
case i82576:
a = phyread(ctlr, Phylhr) & Anf;
i = (i-1) & 3;
break;
default:
a = 0;
break;
}
if(a)
phywrite(ctlr, Phyctl, phyread(ctlr, Phyctl) | Ran | Ean);
e->link = (phy & Rtlink) != 0;
if(e->link){
ctlr->speeds[i]++;
if (speedtab[i])
e->mbps = speedtab[i];
}
next:
ctlr->lim = 0;
i82563im(ctlr, Lsc);
ctlr->lsleep++;
sleep(&ctlr->lrendez, i82563lim, ctlr);
}
}
static void
i82563tproc(void *v)
{
Ether *e;
Ctlr *ctlr;
e = v;
ctlr = e->ctlr;
for(;;){
sleep(&ctlr->trendez, return0, 0);
i82563transmit(e);
}
}
static void
i82563attach(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
char name[KNAMELEN];
ctlr = edev->ctlr;
qlock(&ctlr->alock);
if(ctlr->attached){
qunlock(&ctlr->alock);
return;
}
ctlr->nrd = Nrd;
ctlr->ntd = Ntd;
if(waserror()){
while(ctlr->nrb > 0){
bp = i82563rballoc();
bp->free = nil;
freeb(bp);
ctlr->nrb--;
}
free(ctlr->tb);
ctlr->tb = nil;
free(ctlr->rb);
ctlr->rb = nil;
free(ctlr->tdba);
ctlr->tdba = nil;
free(ctlr->rdba);
ctlr->rdba = nil;
qunlock(&ctlr->alock);
nexterror();
}
if((ctlr->rdba = mallocalign(ctlr->nrd*sizeof(Rd), 128, 0, 0)) == nil ||
(ctlr->tdba = mallocalign(ctlr->ntd*sizeof(Td), 128, 0, 0)) == nil ||
(ctlr->rb = malloc(ctlr->nrd*sizeof(Block*))) == nil ||
(ctlr->tb = malloc(ctlr->ntd*sizeof(Block*))) == nil)
error(Enomem);
for(ctlr->nrb = 0; ctlr->nrb < Nrb; ctlr->nrb++){
if((bp = allocb(ctlr->rbsz + BY2PG)) == nil)
error(Enomem);
bp->free = i82563rbfree;
freeb(bp);
}
nrbfull = 0;
ctlr->edev = edev;
ctlr->attached = 1;
initmark(&ctlr->wmrb, Nrb, "rcv bufs unprocessed");
initmark(&ctlr->wmrd, Nrd-1, "rcv descrs processed at once");
initmark(&ctlr->wmtd, Ntd-1, "xmit descr queue len");
snprint(name, sizeof name, "#l%dl", edev->ctlrno);
kproc(name, i82563lproc, edev);
snprint(name, sizeof name, "#l%dr", edev->ctlrno);
kproc(name, i82563rproc, edev);
snprint(name, sizeof name, "#l%dt", edev->ctlrno);
kproc(name, i82563tproc, edev);
i82563txinit(ctlr);
qunlock(&ctlr->alock);
poperror();
}
static void
i82563interrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Ether *edev;
int icr, im, i;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
im = ctlr->im;
i = Nrd;
for(icr = csr32r(ctlr, Icr); icr & ctlr->im && i-- > 0;
icr = csr32r(ctlr, Icr)){
if(icr & Lsc){
im &= ~Lsc;
ctlr->lim = icr & Lsc;
wakeup(&ctlr->lrendez);
ctlr->lintr++;
}
if(icr & (Rxt0|Rxo|Rxdmt0|Rxseq|Ack)){
ctlr->rim = icr & (Rxt0|Rxo|Rxdmt0|Rxseq|Ack);
im &= ~(Rxt0|Rxo|Rxdmt0|Rxseq|Ack);
wakeup(&ctlr->rrendez);
ctlr->rintr++;
}
if(icr & Txdw){
im &= ~Txdw;
ctlr->tintr++;
wakeup(&ctlr->trendez);
}
}
ctlr->im = im;
csr32w(ctlr, Ims, im);
iunlock(&ctlr->imlock);
}
static void
i82575interrupt(Ureg*, void *)
{
Ctlr *ctlr;
for (ctlr = i82563ctlrhead; ctlr != nil && ctlr->edev != nil;
ctlr = ctlr->next)
i82563interrupt(nil, ctlr->edev);
}
static int
i82563detach0(Ctlr* ctlr)
{
int r, timeo;
csr32w(ctlr, Imc, ~0);
csr32w(ctlr, Rctl, 0);
csr32w(ctlr, Tctl, 0);
delay(10);
r = csr32r(ctlr, Ctrl);
if(ctlr->type == i82566 || ctlr->type == i82567 || ctlr->type == i82579)
r |= Phyrst;
csr32w(ctlr, Ctrl, Devrst | r);
delay(1);
for(timeo = 0; timeo < 1000; timeo++){
if(!(csr32r(ctlr, Ctrl) & Devrst))
break;
delay(1);
}
if(csr32r(ctlr, Ctrl) & Devrst)
return -1;
r = csr32r(ctlr, Ctrlext);
csr32w(ctlr, Ctrlext, r|Eerst);
delay(1);
for(timeo = 0; timeo < 1000; timeo++){
if(!(csr32r(ctlr, Ctrlext) & Eerst))
break;
delay(1);
}
if(csr32r(ctlr, Ctrlext) & Eerst)
return -1;
csr32w(ctlr, Imc, ~0);
delay(1);
for(timeo = 0; timeo < 1000; timeo++){
if(!csr32r(ctlr, Icr))
break;
delay(1);
}
if(csr32r(ctlr, Icr))
return -1;
if(ctlr->rbsz > 8192 && (ctlr->type == i82563 || ctlr->type == i82571 ||
ctlr->type == i82572)){
ctlr->pba = csr32r(ctlr, Pba);
r = ctlr->pba >> 16;
r += ctlr->pba & 0xffff;
r >>= 1;
csr32w(ctlr, Pba, r);
} else if(ctlr->type == i82573 && ctlr->rbsz > ETHERMAXTU)
csr32w(ctlr, Pba, 14);
ctlr->pba = csr32r(ctlr, Pba);
r = csr32r(ctlr, Ctrl);
csr32w(ctlr, Ctrl, Slu|r);
return 0;
}
static int
i82563detach(Ctlr* ctlr)
{
int r;
static Lock detlck;
ilock(&detlck);
r = i82563detach0(ctlr);
iunlock(&detlck);
return r;
}
static void
i82563shutdown(Ether* ether)
{
i82563detach(ether->ctlr);
}
static ushort
eeread(Ctlr *ctlr, int adr)
{
ulong n;
csr32w(ctlr, Eerd, EEstart | adr << 2);
for (n = 1000000; (csr32r(ctlr, Eerd) & EEdone) == 0 && n-- > 0; )
;
if (n == 0)
panic("i82563: eeread stuck");
return csr32r(ctlr, Eerd) >> 16;
}
static int
eeload(Ctlr *ctlr)
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
static int
fcycle(Ctlr *, Flash *f)
{
ushort s, i;
s = f->reg[Fsts];
if((s&Fvalid) == 0)
return -1;
f->reg[Fsts] |= Fcerr | Ael;
for(i = 0; i < 10; i++){
if((s&Scip) == 0)
return 0;
delay(1);
s = f->reg[Fsts];
}
return -1;
}
static int
fread(Ctlr *ctlr, Flash *f, int ladr)
{
ushort s;
ulong n;
delay(1);
if(fcycle(ctlr, f) == -1)
return -1;
f->reg[Fsts] |= Fdone;
f->reg32[Faddr] = ladr;
s = f->reg[Fctl];
s &= ~(0x1f << 8);
s |= (2-1) << 8;
s &= ~(2*Flcycle);
f->reg[Fctl] = s | Fgo;
for (n = 1000000; (f->reg[Fsts] & Fdone) == 0 && n-- > 0; )
;
if (n == 0)
panic("i82563: fread stuck");
if(f->reg[Fsts] & (Fcerr|Ael))
return -1;
return f->reg32[Fdata] & 0xffff;
}
static int
fload(Ctlr *ctlr)
{
ulong data, io, r, adr;
ushort sum;
Flash f;
io = ctlr->pcidev->mem[1].bar & ~0x0f;
f.reg = vmap(io, ctlr->pcidev->mem[1].size);
if(f.reg == nil)
return -1;
f.reg32 = (void*)f.reg;
f.base = f.reg32[Bfpr] & FMASK(0, 13);
f.lim = (f.reg32[Bfpr]>>16) & FMASK(0, 13);
if(csr32r(ctlr, Eec) & (1<<22))
f.base += (f.lim + 1 - f.base) >> 1;
r = f.base << 12;
sum = 0;
for (adr = 0; adr < 0x40; adr++) {
data = fread(ctlr, &f, r + adr*2);
if(data == -1)
break;
ctlr->eeprom[adr] = data;
sum += data;
}
vunmap(f.reg, ctlr->pcidev->mem[1].size);
return sum;
}
static int
i82563reset(Ctlr *ctlr)
{
int i, r;
if(i82563detach(ctlr))
return -1;
if(ctlr->type == i82566 || ctlr->type == i82567 ||
ctlr->type == i82577 || ctlr->type == i82579)
r = fload(ctlr);
else
r = eeload(ctlr);
if (r != 0 && r != 0xBABA){
print("%s: bad EEPROM checksum - %#.4ux\n",
tname[ctlr->type], r);
return -1;
}
for(i = 0; i < Eaddrlen/2; i++){
ctlr->ra[2*i]   = ctlr->eeprom[Ea+i];
ctlr->ra[2*i+1] = ctlr->eeprom[Ea+i] >> 8;
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
memset(ctlr->mta, 0, sizeof(ctlr->mta));
for(i = 0; i < 128; i++)
csr32w(ctlr, Mta + i*4, 0);
csr32w(ctlr, Fcal, 0x00C28001);
csr32w(ctlr, Fcah, 0x0100);
if(ctlr->type != i82579)
csr32w(ctlr, Fct, 0x8808);
csr32w(ctlr, Fcttv, 0x0100);
ctlr->fcrtl = ctlr->fcrth = 0;
csr32w(ctlr, Fcrtl, ctlr->fcrtl);
csr32w(ctlr, Fcrth, ctlr->fcrth);
return 0;
}
static void
i82563pci(void)
{
int type;
ulong io;
void *mem;
Pcidev *p;
Ctlr *ctlr;
p = nil;
while(p = pcimatch(p, 0x8086, 0)){
switch(p->did){
default:
continue;
case 0x1096:
case 0x10ba:
type = i82563;
break;
case 0x1049:
case 0x104a:
case 0x104b:
case 0x104d:
case 0x10bd:
case 0x294c:
type = i82566;
break;
case 0x10cd:
case 0x10ce:
case 0x10de:
case 0x10f5:
type = i82567;
break;
case 0x10a4:
case 0x105e:
type = i82571;
break;
case 0x107d:
case 0x107e:
case 0x107f:
case 0x10b9:
type = i82572;
break;
case 0x108b:
case 0x108c:
case 0x109a:
type = i82573;
break;
case 0x10d3:
type = i82574;
break;
case 0x10a7:
type = i82575;
break;
case 0x10c9:
case 0x10e6:
case 0x10e7:
type = i82576;
break;
case 0x10ea:
type = i82577;
break;
case 0x1502:
case 0x1503:
type = i82579;
break;
}
io = p->mem[0].bar & ~0x0F;
mem = vmap(io, p->mem[0].size);
if(mem == nil){
print("%s: can't map %.8lux\n", tname[type], io);
continue;
}
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil) {
vunmap(mem, p->mem[0].size);
error(Enomem);
}
ctlr->port = io;
ctlr->pcidev = p;
ctlr->type = type;
ctlr->rbsz = ETHERMAXTU;
ctlr->nic = mem;
if(i82563reset(ctlr)){
vunmap(mem, p->mem[0].size);
free(ctlr);
continue;
}
pcisetbme(p);
if(i82563ctlrhead != nil)
i82563ctlrtail->next = ctlr;
else
i82563ctlrhead = ctlr;
i82563ctlrtail = ctlr;
}
}
static int
pnp(Ether* edev, int type)
{
Ctlr *ctlr;
static int done;
if(!done) {
i82563pci();
done = 1;
}
for(ctlr = i82563ctlrhead; ctlr != nil; ctlr = ctlr->next){
if(ctlr->active)
continue;
if(type != Iany && ctlr->type != type)
continue;
if(edev->port == 0 || edev->port == ctlr->port){
ctlr->active = 1;
break;
}
}
if(ctlr == nil)
return -1;
edev->ctlr = ctlr;
ctlr->edev = edev;
edev->port = ctlr->port;
edev->irq = ctlr->pcidev->intl;
edev->tbdf = ctlr->pcidev->tbdf;
edev->mbps = 1000;
edev->maxmtu = ctlr->rbsz;
memmove(edev->ea, ctlr->ra, Eaddrlen);
edev->attach = i82563attach;
edev->transmit = i82563transmit;
edev->interrupt = (ctlr->type == i82575?
i82575interrupt: i82563interrupt);
edev->ifstat = i82563ifstat;
edev->ctl = i82563ctl;
edev->arg = edev;
edev->promiscuous = i82563promiscuous;
edev->shutdown = i82563shutdown;
edev->multicast = i82563multicast;
return 0;
}
static int
anypnp(Ether *e)
{
return pnp(e, Iany);
}
static int
i82563pnp(Ether *e)
{
return pnp(e, i82563);
}
static int
i82566pnp(Ether *e)
{
return pnp(e, i82566);
}
static int
i82571pnp(Ether *e)
{
return pnp(e, i82571);
}
static int
i82572pnp(Ether *e)
{
return pnp(e, i82572);
}
static int
i82573pnp(Ether *e)
{
return pnp(e, i82573);
}
static int
i82575pnp(Ether *e)
{
return pnp(e, i82575);
}
static int
i82579pnp(Ether *e)
{
return pnp(e, i82579);
}
void
ether82563link(void)
{
addethercard("i82563", i82563pnp);
addethercard("i82566", i82566pnp);
addethercard("i82571", i82571pnp);
addethercard("i82572", i82572pnp);
addethercard("i82573", i82573pnp);
addethercard("i82575", i82575pnp);
addethercard("i82579", i82579pnp);
addethercard("igbepcie", anypnp);
}