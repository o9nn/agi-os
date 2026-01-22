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
i82542		= (0x1000<<16)|0x8086,
i82543gc	= (0x1004<<16)|0x8086,
i82544ei	= (0x1008<<16)|0x8086,
i82544eif	= (0x1009<<16)|0x8086,
i82544gc	= (0x100d<<16)|0x8086,
i82540em	= (0x100E<<16)|0x8086,
i82540eplp	= (0x101E<<16)|0x8086,
i82545em	= (0x100F<<16)|0x8086,
i82545gmc	= (0x1026<<16)|0x8086,
i82547ei	= (0x1019<<16)|0x8086,
i82547gi	= (0x1075<<16)|0x8086,
i82541ei	= (0x1013<<16)|0x8086,
i82541gi	= (0x1076<<16)|0x8086,
i82541gi2	= (0x1077<<16)|0x8086,
i82541pi	= (0x107c<<16)|0x8086,
i82546gb	= (0x1079<<16)|0x8086,
i82546eb	= (0x1010<<16)|0x8086,
};
enum {
Ctrl		= 0x00000000,
Ctrldup		= 0x00000004,
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
Rxcw		= 0x00000180,
Tctl		= 0x00000400,
Tipg		= 0x00000410,
Tbt		= 0x00000448,
Ait		= 0x00000458,
Fcrtl		= 0x00002160,
Fcrth		= 0x00002168,
Rdfh		= 0x00002410,
Rdft		= 0x00002418,
Rdfhs		= 0x00002420,
Rdfts		= 0x00002428,
Rdfpc		= 0x00002430,
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
Tdfh		= 0x00003410,
Tdft		= 0x00003418,
Tdfhs		= 0x00003420,
Tdfts		= 0x00003428,
Tdfpc		= 0x00003430,
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
LspeedMASK	= 0x000000C0,
LspeedSHIFT	= 6,
Lspeed10	= 0x00000000,
Lspeed100	= 0x00000040,
Lspeed1000	= 0x00000080,
Mtxckok		= 0x00000400,
Pci66		= 0x00000800,
Bus64		= 0x00001000,
Pcixmode	= 0x00002000,
PcixspeedMASK	= 0x0000C000,
PcixspeedSHIFT	= 14,
Pcix66		= 0x00000000,
Pcix100		= 0x00004000,
Pcix133		= 0x00008000,
};
enum {
Fd		= 0x00000001,
AsdvMASK	= 0x00000300,
AsdvSHIFT	= 8,
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
Rxword		= 0x0000FFFF,
Rxnocarrier	= 0x04000000,
Rxinvalid	= 0x08000000,
Rxchange	= 0x10000000,
Rxconfig	= 0x20000000,
Rxsync		= 0x40000000,
Anc		= 0x80000000,
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
Mo47b36		= 0x00000000,
Mo46b35		= 0x00001000,
Mo45b34		= 0x00002000,
Mo43b32		= 0x00003000,
Bam		= 0x00008000,
BsizeMASK	= 0x00030000,
Bsize2048	= 0x00000000,
Bsize1024	= 0x00010000,
Bsize512	= 0x00020000,
Bsize256	= 0x00030000,
Bsize16384	= 0x00010000,
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
LthreshMASK	= 0xFE000000,
LthreshSHIFT	= 25,
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
enum {
DelayMASK	= 0x0000FFFF,
DelaySHIFT	= 0,
Fpd		= 0x80000000,
};
typedef struct Rd {
uint	addr[2];
ushort	length;
ushort	checksum;
uchar	status;
uchar	errors;
ushort	special;
} Rd;
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
typedef struct Td Td;
struct Td {
union {
uint	addr[2];
struct {
uchar	ipcss;
uchar	ipcso;
ushort	ipcse;
uchar	tucss;
uchar	tucso;
ushort	tucse;
};
};
uint	control;
uint	status;
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
Tdd		= 0x00000001,
Ec		= 0x00000002,
Lc		= 0x00000004,
Tu		= 0x00000008,
Iixsm		= 0x00000100,
Itxsm		= 0x00000200,
HdrlenMASK	= 0x0000FF00,
HdrlenSHIFT	= 8,
VlanMASK	= 0x0FFF0000,
VlanSHIFT	= 16,
Tcfi		= 0x10000000,
PriMASK		= 0xE0000000,
PriSHIFT	= 29,
MssMASK		= 0xFFFF0000,
MssSHIFT	= 16,
};
enum {
Rbsz		= 2048,
Nrd		= 128,
Nrb		= 512,
Ntd		= 32,
};
typedef struct Ctlr Ctlr;
struct Ctlr {
int	port;
Pcidev*	pcidev;
Ctlr*	next;
Ether*	edev;
int	active;
int	started;
int	id;
int	cls;
ushort	eeprom[0x40];
QLock	alock;
void*	alloc;
int	nrd;
int	ntd;
int	nrb;
int*	nic;
Lock	imlock;
int	im;
Mii*	mii;
Rendez	lrendez;
int	lim;
int	link;
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
uchar	ra[Eaddrlen];
ulong	mta[128];
Rendez	rrendez;
int	rim;
int	rdfree;
Rd*	rdba;
Block**	rb;
int	rdh;
int	rdt;
int	rdtr;
Lock	tlock;
int	tdfree;
Td*	tdba;
Block**	tb;
int	tdh;
int	tdt;
int	txcw;
int	fcrtl;
int	fcrth;
};
#define csr32r(c, r)	(*((c)->nic+((r)/4)))
#define csr32w(c, r, v)	(*((c)->nic+((r)/4)) = (v))
static Ctlr* igbectlrhead;
static Ctlr* igbectlrtail;
static Lock igberblock;
static Block* igberbpool;
static int nrbfull;
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
static long
igbeifstat(Ether* edev, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *p, *s, *e;
int i, l, r;
uvlong tuvl, ruvl;
ctlr = edev->ctlr;
qlock(&ctlr->slock);
p = malloc(READSTR);
if(p == nil) {
qunlock(&ctlr->slock);
error(Enomem);
}
l = 0;
for(i = 0; i < Nstatistics; i++){
r = csr32r(ctlr, Statistics+i*4);
if((s = statistics[i]) == nil)
continue;
switch(i){
case Gorcl:
case Gotcl:
case Torl:
case Totl:
ruvl = r;
ruvl += ((uvlong)csr32r(ctlr, Statistics+(i+1)*4))<<32;
tuvl = ruvl;
tuvl += ctlr->statistics[i];
tuvl += ((uvlong)ctlr->statistics[i+1])<<32;
if(tuvl == 0)
continue;
ctlr->statistics[i] = tuvl;
ctlr->statistics[i+1] = tuvl>>32;
l += snprint(p+l, READSTR-l, "%s: %llud %llud\n",
s, tuvl, ruvl);
i++;
break;
default:
ctlr->statistics[i] += r;
if(ctlr->statistics[i] == 0)
continue;
l += snprint(p+l, READSTR-l, "%s: %ud %ud\n",
s, ctlr->statistics[i], r);
break;
}
}
l += snprint(p+l, READSTR-l, "lintr: %ud %ud\n",
ctlr->lintr, ctlr->lsleep);
l += snprint(p+l, READSTR-l, "rintr: %ud %ud\n",
ctlr->rintr, ctlr->rsleep);
l += snprint(p+l, READSTR-l, "tintr: %ud %ud\n",
ctlr->tintr, ctlr->txdw);
l += snprint(p+l, READSTR-l, "ixcs: %ud %ud %ud\n",
ctlr->ixsm, ctlr->ipcs, ctlr->tcpcs);
l += snprint(p+l, READSTR-l, "rdtr: %ud\n", ctlr->rdtr);
l += snprint(p+l, READSTR-l, "Ctrlext: %08x\n", csr32r(ctlr, Ctrlext));
l += snprint(p+l, READSTR-l, "eeprom:");
for(i = 0; i < 0x40; i++){
if(i && ((i & 0x07) == 0))
l += snprint(p+l, READSTR-l, "\n       ");
l += snprint(p+l, READSTR-l, " %4.4uX", ctlr->eeprom[i]);
}
l += snprint(p+l, READSTR-l, "\n");
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
e = p + READSTR;
s = p + l + 1;
s = seprintmark(s, e, &ctlr->wmrb);
s = seprintmark(s, e, &ctlr->wmrd);
s = seprintmark(s, e, &ctlr->wmtd);
USED(s);
n = readstr(offset, a, n, p);
free(p);
qunlock(&ctlr->slock);
return n;
}
enum {
CMrdtr,
};
static Cmdtab igbectlmsg[] = {
CMrdtr,	"rdtr",	2,
};
static long
igbectl(Ether* edev, void* buf, long n)
{
int v;
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
ct = lookupcmd(cb, igbectlmsg, nelem(igbectlmsg));
switch(ct->index){
case CMrdtr:
v = strtol(cb->f[1], &p, 0);
if(v < 0 || p == cb->f[1] || v > 0xFFFF)
error(Ebadarg);
ctlr->rdtr = v;
csr32w(ctlr, Rdtr, Fpd|v);
break;
}
free(cb);
poperror();
return n;
}
static void
igbepromiscuous(void* arg, int on)
{
int rctl;
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
rctl = csr32r(ctlr, Rctl);
rctl &= ~MoMASK;
rctl |= Mo47b36;
if(on)
rctl |= Upe|Mpe;
else
rctl &= ~(Upe|Mpe);
csr32w(ctlr, Rctl, rctl|Mpe);
}
static void
igbemulticast(void* arg, uchar* addr, int add)
{
int bit, x;
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
x = addr[5]>>1;
bit = ((addr[5] & 1)<<4)|(addr[4]>>4);
if(add)
ctlr->mta[x] |= 1<<bit;
csr32w(ctlr, Mta+x*4, ctlr->mta[x]);
}
static Block*
igberballoc(void)
{
Block *bp;
ilock(&igberblock);
if((bp = igberbpool) != nil){
igberbpool = bp->next;
bp->next = nil;
_xinc(&bp->ref);
}
iunlock(&igberblock);
return bp;
}
static void
igberbfree(Block* bp)
{
bp->rp = bp->lim - Rbsz;
bp->wp = bp->rp;
bp->flag &= ~(Bipck | Budpck | Btcpck | Bpktck);
ilock(&igberblock);
bp->next = igberbpool;
igberbpool = bp;
nrbfull--;
iunlock(&igberblock);
}
static void
igbeim(Ctlr* ctlr, int im)
{
ilock(&ctlr->imlock);
ctlr->im |= im;
csr32w(ctlr, Ims, ctlr->im);
iunlock(&ctlr->imlock);
}
static int
igbelim(void* ctlr)
{
return ((Ctlr*)ctlr)->lim != 0;
}
static void
igbelproc(void* arg)
{
Ctlr *ctlr;
Ether *edev;
MiiPhy *phy;
int ctrl, r;
edev = arg;
ctlr = edev->ctlr;
for(;;){
if(ctlr->mii == nil || ctlr->mii->curphy == nil) {
sched();
continue;
}
if(miistatus(ctlr->mii) < 0)
goto enable;
phy = ctlr->mii->curphy;
ctrl = csr32r(ctlr, Ctrl);
switch(ctlr->id){
case i82543gc:
case i82544ei:
case i82544eif:
default:
if(!(ctrl & Asde)){
ctrl &= ~(SspeedMASK|Ilos|Fd);
ctrl |= Frcdplx|Frcspd;
if(phy->speed == 1000)
ctrl |= Sspeed1000;
else if(phy->speed == 100)
ctrl |= Sspeed100;
if(phy->fd)
ctrl |= Fd;
}
break;
case i82540em:
case i82540eplp:
case i82547gi:
case i82541gi:
case i82541gi2:
case i82541pi:
break;
}
r = csr32r(ctlr, Tctl);
r &= ~ColdMASK;
if(phy->fd)
r |= 64<<ColdSHIFT;
else
r |= 512<<ColdSHIFT;
csr32w(ctlr, Tctl, r);
if(phy->rfc)
ctrl |= Rfce;
if(phy->tfc)
ctrl |= Tfce;
csr32w(ctlr, Ctrl, ctrl);
enable:
ctlr->lim = 0;
igbeim(ctlr, Lsc);
ctlr->lsleep++;
sleep(&ctlr->lrendez, igbelim, ctlr);
}
}
static void
igbetxinit(Ctlr* ctlr)
{
int i, r;
Block *bp;
csr32w(ctlr, Tctl, (0x0F<<CtSHIFT)|Psp|(66<<ColdSHIFT));
switch(ctlr->id){
default:
r = 6;
break;
case i82543gc:
case i82544ei:
case i82544eif:
case i82544gc:
case i82540em:
case i82540eplp:
case i82541ei:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
case i82546gb:
case i82546eb:
case i82547ei:
case i82547gi:
r = 8;
break;
}
csr32w(ctlr, Tipg, (6<<20)|(8<<10)|r);
csr32w(ctlr, Ait, 0);
csr32w(ctlr, Txdmac, 0);
csr32w(ctlr, Tdbal, PCIWADDR(ctlr->tdba));
csr32w(ctlr, Tdbah, 0);
csr32w(ctlr, Tdlen, ctlr->ntd*sizeof(Td));
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
ctlr->tdfree = ctlr->ntd;
csr32w(ctlr, Tidv, 128);
r = (4<<WthreshSHIFT)|(4<<HthreshSHIFT)|(8<<PthreshSHIFT);
switch(ctlr->id){
default:
break;
case i82540em:
case i82540eplp:
case i82547gi:
case i82545em:
case i82545gmc:
case i82546gb:
case i82546eb:
case i82541gi:
case i82541gi2:
case i82541pi:
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
}
static void
igbetransmit(Ether* edev)
{
Td *td;
Block *bp;
Ctlr *ctlr;
int tdh, tdt;
ctlr = edev->ctlr;
ilock(&ctlr->tlock);
tdh = ctlr->tdh;
while(NEXT(tdh, ctlr->ntd) != csr32r(ctlr, Tdh)){
if((bp = ctlr->tb[tdh]) != nil){
ctlr->tb[tdh] = nil;
freeb(bp);
}
memset(&ctlr->tdba[tdh], 0, sizeof(Td));
tdh = NEXT(tdh, ctlr->ntd);
}
ctlr->tdh = tdh;
tdt = ctlr->tdt;
while(NEXT(tdt, ctlr->ntd) != tdh){
if((bp = qget(edev->oq)) == nil)
break;
td = &ctlr->tdba[tdt];
td->addr[0] = PCIWADDR(bp->rp);
td->control = ((BLEN(bp) & LenMASK)<<LenSHIFT);
td->control |= Dext|Ifcs|Teop|DtypeDD;
ctlr->tb[tdt] = bp;
notemark(&ctlr->wmtd, (tdt + Ntd - tdh) % Ntd);
tdt = NEXT(tdt, ctlr->ntd);
if(NEXT(tdt, ctlr->ntd) == tdh){
td->control |= Rs;
ctlr->txdw++;
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
igbeim(ctlr, Txdw);
break;
}
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
}
iunlock(&ctlr->tlock);
}
static void
igbereplenish(Ctlr* ctlr)
{
Rd *rd;
int rdt;
Block *bp;
rdt = ctlr->rdt;
while(NEXT(rdt, ctlr->nrd) != ctlr->rdh){
rd = &ctlr->rdba[rdt];
if(ctlr->rb[rdt] == nil){
bp = igberballoc();
if(bp == nil){
iprint("#l%d: igbereplenish: no available buffers\n",
ctlr->edev->ctlrno);
break;
}
ctlr->rb[rdt] = bp;
rd->addr[0] = PCIWADDR(bp->rp);
rd->addr[1] = 0;
}
coherence();
rd->status = 0;
rdt = NEXT(rdt, ctlr->nrd);
ctlr->rdfree++;
}
ctlr->rdt = rdt;
csr32w(ctlr, Rdt, rdt);
}
static void
igberxinit(Ctlr* ctlr)
{
int i;
Block *bp;
csr32w(ctlr, Rctl, Dpf|Bsize2048|Bam|RdtmsHALF|Mpe);
csr32w(ctlr, Rdbal, PCIWADDR(ctlr->rdba));
csr32w(ctlr, Rdbah, 0);
csr32w(ctlr, Rdlen, ctlr->nrd*sizeof(Rd));
ctlr->rdh = 0;
csr32w(ctlr, Rdh, 0);
ctlr->rdt = 0;
csr32w(ctlr, Rdt, 0);
ctlr->rdtr = 0;
csr32w(ctlr, Rdtr, Fpd|0);
for(i = 0; i < ctlr->nrd; i++){
if((bp = ctlr->rb[i]) != nil){
ctlr->rb[i] = nil;
freeb(bp);
}
}
igbereplenish(ctlr);
nrbfull = 0;
switch(ctlr->id){
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
case i82546gb:
case i82546eb:
case i82547gi:
csr32w(ctlr, Radv, 64);
break;
}
csr32w(ctlr, Rxdctl, (8<<WthreshSHIFT)|(8<<HthreshSHIFT)|4);
csr32w(ctlr, Rxcsum, ETHERHDRSIZE<<PcssSHIFT);
}
static int
igberim(void* ctlr)
{
return ((Ctlr*)ctlr)->rim != 0;
}
static void
igberproc(void* arg)
{
Rd *rd;
Block *bp;
Ctlr *ctlr;
int r, rdh, passed;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
igberxinit(ctlr);
r = csr32r(ctlr, Rctl);
r |= Ren;
csr32w(ctlr, Rctl, r);
for(;;){
ctlr->rim = 0;
igbeim(ctlr, Rxt0|Rxo|Rxdmt0|Rxseq);
ctlr->rsleep++;
sleep(&ctlr->rrendez, igberim, ctlr);
rdh = ctlr->rdh;
passed = 0;
for(;;){
rd = &ctlr->rdba[rdh];
if(!(rd->status & Rdd))
break;
rd->errors &= ~(Ipe | Tcpe);
if((rd->status & Reop) && rd->errors == 0){
bp = ctlr->rb[rdh];
ctlr->rb[rdh] = nil;
bp->wp += rd->length;
bp->next = nil;
if(0 && !(rd->status & Ixsm)){
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
ilock(&igberblock);
nrbfull++;
iunlock(&igberblock);
notemark(&ctlr->wmrb, nrbfull);
etheriq(edev, bp, 1);
passed++;
}
else if(ctlr->rb[rdh] != nil){
freeb(ctlr->rb[rdh]);
ctlr->rb[rdh] = nil;
}
memset(rd, 0, sizeof(Rd));
coherence();
ctlr->rdfree--;
rdh = NEXT(rdh, ctlr->nrd);
}
ctlr->rdh = rdh;
if(ctlr->rdfree < ctlr->nrd/2 || (ctlr->rim & Rxdmt0))
igbereplenish(ctlr);
notemark(&ctlr->wmrd, passed);
}
}
static void
igbeattach(Ether* edev)
{
Block *bp;
Ctlr *ctlr;
char name[KNAMELEN];
ctlr = edev->ctlr;
ctlr->edev = edev;
qlock(&ctlr->alock);
if(ctlr->alloc != nil){
qunlock(&ctlr->alock);
return;
}
ctlr->tb = nil;
ctlr->rb = nil;
ctlr->alloc = nil;
ctlr->nrb = 0;
if(waserror()){
while(ctlr->nrb > 0){
bp = igberballoc();
bp->free = nil;
freeb(bp);
ctlr->nrb--;
}
free(ctlr->tb);
ctlr->tb = nil;
free(ctlr->rb);
ctlr->rb = nil;
free(ctlr->alloc);
ctlr->alloc = nil;
qunlock(&ctlr->alock);
nexterror();
}
ctlr->nrd = ROUND(Nrd, 8);
ctlr->ntd = ROUND(Ntd, 8);
ctlr->alloc = malloc(ctlr->nrd*sizeof(Rd)+ctlr->ntd*sizeof(Td) + 127);
if(ctlr->alloc == nil) {
print("igbe: can't allocate ctlr->alloc\n");
error(Enomem);
}
ctlr->rdba = (Rd*)ROUNDUP((uintptr)ctlr->alloc, 128);
ctlr->tdba = (Td*)(ctlr->rdba+ctlr->nrd);
ctlr->rb = malloc(ctlr->nrd*sizeof(Block*));
ctlr->tb = malloc(ctlr->ntd*sizeof(Block*));
if (ctlr->rb == nil || ctlr->tb == nil) {
print("igbe: can't allocate ctlr->rb or ctlr->tb\n");
error(Enomem);
}
for(ctlr->nrb = 0; ctlr->nrb < Nrb; ctlr->nrb++){
if((bp = allocb(Rbsz)) == nil)
break;
bp->free = igberbfree;
freeb(bp);
}
initmark(&ctlr->wmrb, Nrb, "rcv bufs unprocessed");
initmark(&ctlr->wmrd, Nrd-1, "rcv descrs processed at once");
initmark(&ctlr->wmtd, Ntd-1, "xmit descr queue len");
snprint(name, KNAMELEN, "#l%dlproc", edev->ctlrno);
kproc(name, igbelproc, edev);
snprint(name, KNAMELEN, "#l%drproc", edev->ctlrno);
kproc(name, igberproc, edev);
igbetxinit(ctlr);
qunlock(&ctlr->alock);
poperror();
}
static void
igbeinterrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Ether *edev;
int icr, im, txdw;
edev = arg;
ctlr = edev->ctlr;
ilock(&ctlr->imlock);
csr32w(ctlr, Imc, ~0);
im = ctlr->im;
txdw = 0;
while((icr = csr32r(ctlr, Icr) & ctlr->im) != 0){
if(icr & Lsc){
im &= ~Lsc;
ctlr->lim = icr & Lsc;
wakeup(&ctlr->lrendez);
ctlr->lintr++;
}
if(icr & (Rxt0|Rxo|Rxdmt0|Rxseq)){
im &= ~(Rxt0|Rxo|Rxdmt0|Rxseq);
ctlr->rim = icr & (Rxt0|Rxo|Rxdmt0|Rxseq);
wakeup(&ctlr->rrendez);
ctlr->rintr++;
}
if(icr & Txdw){
im &= ~Txdw;
txdw++;
ctlr->tintr++;
}
}
ctlr->im = im;
csr32w(ctlr, Ims, im);
iunlock(&ctlr->imlock);
if(txdw)
igbetransmit(edev);
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
MiiPhy *phy;
int ctrl, p, r;
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
if(!(r & Mdro)) {
print("igbe: 82543gc Mdro not set\n");
return -1;
}
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
case i82544eif:
case i82544gc:
case i82540em:
case i82540eplp:
case i82547ei:
case i82547gi:
case i82541ei:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
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
free(ctlr->mii);
ctlr->mii = nil;
return -1;
}
USED(phy);
switch(ctlr->id){
case i82547gi:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
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
p = 0;
if(ctlr->txcw & TxcwPs)
p |= AnaP;
if(ctlr->txcw & TxcwAs)
p |= AnaAP;
miiane(ctlr->mii, ~0, p, ~0);
break;
}
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
case i82541ei:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
case i82546gb:
case i82546eb:
case i82547ei:
case i82547gi:
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
}
release:
if(areq)
csr32w(ctlr, Eecd, eecd & ~Areq);
return sum;
}
static int
igbedetach(Ctlr* ctlr)
{
int r, timeo;
csr32w(ctlr, Imc, ~0);
csr32w(ctlr, Rctl, 0);
csr32w(ctlr, Tctl, 0);
delay(10);
csr32w(ctlr, Ctrl, Devrst);
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
switch(ctlr->id){
default:
break;
case i82540em:
case i82540eplp:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
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
for(timeo = 0; timeo < 1000; timeo++){
if(!csr32r(ctlr, Icr))
break;
delay(1);
}
if(csr32r(ctlr, Icr))
return -1;
return 0;
}
static void
igbeshutdown(Ether* ether)
{
igbedetach(ether->ctlr);
}
static int
igbereset(Ctlr* ctlr)
{
int ctrl, i, pause, r, swdpio, txcw;
if(igbedetach(ctlr))
return -1;
if((r = at93c46r(ctlr)) != 0xBABA){
print("igbe: bad EEPROM checksum - 0x%4.4uX\n", r);
return -1;
}
if ((ctlr->id == i82546gb || ctlr->id == i82546eb) &&
BUSFNO(ctlr->pcidev->tbdf) == 1)
ctlr->eeprom[Ea+2] += 0x100;
if(ctlr->id == i82541gi && ctlr->eeprom[Ea] == 0xFFFF)
ctlr->eeprom[Ea] = 0xD000;
for(i = Ea; i < Eaddrlen/2; i++){
ctlr->ra[2*i] = ctlr->eeprom[i];
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
if(!(csr32r(ctlr, Status) & Tbimode) && igbemii(ctlr) < 0)
return -1;
return 0;
}
static void
igbepci(void)
{
int cls;
Pcidev *p;
Ctlr *ctlr;
void *mem;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
default:
continue;
case i82543gc:
case i82544ei:
case i82544eif:
case i82544gc:
case i82547ei:
case i82547gi:
case i82540em:
case i82540eplp:
case i82541ei:
case i82541gi:
case i82541gi2:
case i82541pi:
case i82545em:
case i82545gmc:
case i82546gb:
case i82546eb:
break;
}
mem = vmap(p->mem[0].bar & ~0x0F, p->mem[0].size);
if(mem == nil){
print("igbe: can't map %8.8luX\n", p->mem[0].bar);
continue;
}
cls = pcicfgr8(p, PciCLS);
switch(cls){
default:
print("igbe: p->cls %#ux, setting to 0x10\n", p->cls);
p->cls = 0x10;
pcicfgw8(p, PciCLS, p->cls);
break;
case 0x08:
case 0x10:
break;
}
ctlr = malloc(sizeof(Ctlr));
if(ctlr == nil) {
vunmap(mem, p->mem[0].size);
error(Enomem);
}
ctlr->port = p->mem[0].bar & ~0x0F;
ctlr->pcidev = p;
ctlr->id = (p->did<<16)|p->vid;
ctlr->cls = cls*4;
ctlr->nic = mem;
if(igbereset(ctlr)){
free(ctlr);
vunmap(mem, p->mem[0].size);
continue;
}
pcisetbme(p);
if(igbectlrhead != nil)
igbectlrtail->next = ctlr;
else
igbectlrhead = ctlr;
igbectlrtail = ctlr;
}
}
static int
igbepnp(Ether* edev)
{
Ctlr *ctlr;
if(igbectlrhead == nil)
igbepci();
for(ctlr = igbectlrhead; ctlr != nil; ctlr = ctlr->next){
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
memmove(edev->ea, ctlr->ra, Eaddrlen);
edev->attach = igbeattach;
edev->transmit = igbetransmit;
edev->interrupt = igbeinterrupt;
edev->ifstat = igbeifstat;
edev->ctl = igbectl;
edev->arg = edev;
edev->promiscuous = igbepromiscuous;
edev->shutdown = igbeshutdown;
edev->multicast = igbemulticast;
return 0;
}
void
etherigbelink(void)
{
addethercard("i82543", igbepnp);
addethercard("igbe", igbepnp);
}