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
Ctrl = 0x00000000,
Status = 0x00000008,
Eecd = 0x00000010,
Ctrlext = 0x00000018,
Mdic = 0x00000020,
Fcal = 0x00000028,
Fcah = 0x0000002C,
Fct = 0x00000030,
Icr = 0x000000C0,
Ics = 0x000000C8,
Ims = 0x000000D0,
Imc = 0x000000D8,
Rctl = 0x00000100,
Fcttv = 0x00000170,
Txcw = 0x00000178,
Rxcw = 0x00000180,
Tctl = 0x00000400,
Tipg = 0x00000410,
Tbt = 0x00000448,
Ait = 0x00000458,
Fcrtl = 0x00002160,
Fcrth = 0x00002168,
Rdfh = 0x00002410,
Rdft = 0x00002418,
Rdfhs = 0x00002420,
Rdfts = 0x00002428,
Rdfpc = 0x00002430,
Rdbal = 0x00002800,
Rdbah = 0x00002804,
Rdlen = 0x00002808,
Rdh = 0x00002810,
Rdt = 0x00002818,
Rdtr = 0x00002820,
Rxdctl = 0x00002828,
Txdmac = 0x00003000,
Ett = 0x00003008,
Tdfh = 0x00003410,
Tdft = 0x00003418,
Tdfhs = 0x00003420,
Tdfts = 0x00003428,
Tdfpc = 0x00003430,
Tdbal = 0x00003800,
Tdbah = 0x00003804,
Tdlen = 0x00003808,
Tdh = 0x00003810,
Tdt = 0x00003818,
Tidv = 0x00003820,
Txdctl = 0x00003828,
Statistics = 0x00004000,
Gorcl = 0x88/4,
Gotcl = 0x90/4,
Torl = 0xC0/4,
Totl = 0xC8/4,
Nstatistics = 64,
Rxcsum = 0x00005000,
Mta = 0x00005200,
Ral = 0x00005400,
Rah = 0x00005404,
};
enum {
Bem = 0x00000002,
Prior = 0x00000004,
Lrst = 0x00000008,
Asde = 0x00000020,
Slu = 0x00000040,
Ilos = 0x00000080,
Frcspd = 0x00000800,
Frcdplx = 0x00001000,
Swdpinslo = 0x003C0000,
Swdpin0 = 0x00040000,
Swdpin1 = 0x00080000,
Swdpin2 = 0x00100000,
Swdpin3 = 0x00200000,
Swdpiolo = 0x03C00000,
Swdpio0 = 0x00400000,
Swdpio1 = 0x00800000,
Swdpio2 = 0x01000000,
Swdpio3 = 0x02000000,
Devrst = 0x04000000,
Rfce = 0x08000000,
Tfce = 0x10000000,
Vme = 0x40000000,
};
enum {
Lu = 0x00000002,
Tckok = 0x00000004,
Rbcok = 0x00000008,
Txoff = 0x00000010,
Tbimode = 0x00000020,
SpeedMASK = 0x000000C0,
Speed10 = 0x00000000,
Speed100 = 0x00000040,
Speed1000 = 0x00000080,
Mtxckok = 0x00000400,
Pci66 = 0x00000800,
Bus64 = 0x00001000,
};
enum {
Fd = 0x00000001,
AsdvMASK = 0x00000300,
Asdv10 = 0x00000000,
Asdv100 = 0x00000100,
Asdv1000 = 0x00000200,
};
enum {
Sk = 0x00000001,
Cs = 0x00000002,
Di = 0x00000004,
Do = 0x00000008,
};
enum {
Gpien = 0x0000000F,
Swdpinshi = 0x000000F0,
Swdpiohi = 0x00000F00,
Asdchk = 0x00001000,
Eerst = 0x00002000,
Ips = 0x00004000,
Spdbyps = 0x00008000,
};
enum {
Ea = 0x00,
Cf = 0x03,
Pba = 0x08,
Icw1 = 0x0A,
Sid = 0x0B,
Svid = 0x0C,
Did = 0x0D,
Vid = 0x0E,
Icw2 = 0x0F,
};
enum {
MDIdMASK = 0x0000FFFF,
MDIdSHIFT = 0,
MDIrMASK = 0x001F0000,
MDIrSHIFT = 16,
MDIpMASK = 0x03E00000,
MDIpSHIFT = 21,
MDIwop = 0x04000000,
MDIrop = 0x08000000,
MDIready = 0x10000000,
MDIie = 0x20000000,
MDIe = 0x40000000,
};
enum {
Txdw = 0x00000001,
Txqe = 0x00000002,
Lsc = 0x00000004,
Rxseq = 0x00000008,
Rxdmt0 = 0x00000010,
Rxo = 0x00000040,
Rxt0 = 0x00000080,
Mdac = 0x00000200,
Rxcfg = 0x00000400,
Gpi0 = 0x00000800,
Gpi1 = 0x00001000,
Gpi2 = 0x00002000,
Gpi3 = 0x00004000,
};
enum {
Ane = 0x80000000,
Np = 0x00008000,
As = 0x00000100,
Ps = 0x00000080,
Hd = 0x00000040,
TxcwFd = 0x00000020,
};
enum {
Rxword = 0x0000FFFF,
Rxnocarrier = 0x04000000,
Rxinvalid = 0x08000000,
Rxchange = 0x10000000,
Rxconfig = 0x20000000,
Rxsync = 0x40000000,
Anc = 0x80000000,
};
enum {
Rrst = 0x00000001,
Ren = 0x00000002,
Sbp = 0x00000004,
Upe = 0x00000008,
Mpe = 0x00000010,
Lpe = 0x00000020,
LbmMASK = 0x000000C0,
LbmOFF = 0x00000000,
LbmTBI = 0x00000040,
LbmMII = 0x00000080,
LbmXCVR = 0x000000C0,
RdtmsMASK = 0x00000300,
RdtmsHALF = 0x00000000,
RdtmsQUARTER = 0x00000100,
RdtmsEIGHTH = 0x00000200,
MoMASK = 0x00003000,
Bam = 0x00008000,
BsizeMASK = 0x00030000,
Bsize2048 = 0x00000000,
Bsize1024 = 0x00010000,
Bsize512 = 0x00020000,
Bsize256 = 0x00030000,
Bsize16384 = 0x00010000,
Vfe = 0x00040000,
Cfien = 0x00080000,
Cfi = 0x00100000,
Dpf = 0x00400000,
Pmcf = 0x00800000,
Bsex = 0x02000000,
Secrc = 0x04000000,
};
enum {
Trst = 0x00000001,
Ten = 0x00000002,
Psp = 0x00000008,
CtMASK = 0x00000FF0,
CtSHIFT = 4,
ColdMASK = 0x003FF000,
ColdSHIFT = 12,
Swxoff = 0x00400000,
Pbe = 0x00800000,
Rtlc = 0x01000000,
Nrtu = 0x02000000,
};
enum {
PthreshMASK = 0x0000003F,
PthreshSHIFT = 0,
HthreshMASK = 0x00003F00,
HthreshSHIFT = 8,
WthreshMASK = 0x003F0000,
WthreshSHIFT = 16,
Gran = 0x00000000,
RxGran = 0x01000000,
};
enum {
PcssMASK = 0x000000FF,
PcssSHIFT = 0,
Ipofl = 0x00000100,
Tuofl = 0x00000200,
};
enum {
Fpd = 0x80000000,
};
typedef struct Rdesc {
uint addr[2];
ushort length;
ushort checksum;
uchar status;
uchar errors;
ushort special;
} Rdesc;
enum {
Rdd = 0x01,
Reop = 0x02,
Ixsm = 0x04,
Vp = 0x08,
Tcpcs = 0x20,
Ipcs = 0x40,
Pif = 0x80,
};
enum {
Ce = 0x01,
Se = 0x02,
Seq = 0x04,
Cxe = 0x10,
Tcpe = 0x20,
Ipe = 0x40,
Rxe = 0x80,
};
typedef struct Tdesc {
uint addr[2];
uint control;
uint status;
} Tdesc;
enum {
CsoMASK = 0x00000F00,
CsoSHIFT = 16,
Teop = 0x01000000,
Ifcs = 0x02000000,
Ic = 0x04000000,
Tse = 0x04000000,
Rs = 0x08000000,
Rps = 0x10000000,
Dext = 0x20000000,
Vle = 0x40000000,
Ide = 0x80000000,
};
enum {
Tdd = 0x00000001,
Ec = 0x00000002,
Lc = 0x00000004,
Tu = 0x00000008,
CssMASK = 0x0000FF00,
CssSHIFT = 8,
};
enum {
Nrdesc = 256,
Ntdesc = 256,
Nblocks = 4098,
SBLOCKSIZE = 2048,
JBLOCKSIZE = 16384,
NORMAL = 1,
JUMBO = 2,
};
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int port;
Pcidev* pcidev;
Ctlr* next;
int active;
int started;
int id;
ushort eeprom[0x40];
int* nic;
int im;
Lock slock;
uint statistics[Nstatistics];
Lock rdlock;
Rdesc* rdba;
Block* rb[Nrdesc];
int rdh;
int rdt;
Block** freehead;
Lock tdlock;
Tdesc* tdba;
Block* tb[Ntdesc];
int tdh;
int tdt;
int txstalled;
int txcw;
int fcrtl;
int fcrth;
ulong multimask[128];
} Ctlr;
static Ctlr* gc82543ctlrhead;
static Ctlr* gc82543ctlrtail;
static Lock freelistlock;
static Block* freeShortHead;
static Block* freeJumboHead;
#define csr32r(c, r) (*((c)->nic+((r)/4)))
#define csr32w(c, r, v) (*((c)->nic+((r)/4)) = (v))
static void gc82543watchdog(void* arg);
static void
gc82543attach(Ether* edev)
{
int ctl;
Ctlr *ctlr;
char name[KNAMELEN];
ctlr = edev->ctlr;
lock(&ctlr->slock);
if(ctlr->started == 0){
ctlr->started = 1;
snprint(name, KNAMELEN, "#l%d82543", edev->ctlrno);
kproc(name, gc82543watchdog, edev, 0);
}
unlock(&ctlr->slock);
ctl = csr32r(ctlr, Rctl)|Ren;
csr32w(ctlr, Rctl, ctl);
ctl = csr32r(ctlr, Tctl)|Ten;
csr32w(ctlr, Tctl, ctl);
csr32w(ctlr, Ims, ctlr->im);
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
static long
gc82543ifstat(Ether* edev, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *p, *s;
int i, l, r;
uvlong tuvl, ruvl;
ctlr = edev->ctlr;
lock(&ctlr->slock);
p = malloc(2*READSTR);
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
l += snprint(p+l, 2*READSTR-l, "%s: %llud %llud\n",
s, tuvl, ruvl);
i++;
break;
default:
ctlr->statistics[i] += r;
if(ctlr->statistics[i] == 0)
continue;
l += snprint(p+l, 2*READSTR-l, "%s: %ud %ud\n",
s, ctlr->statistics[i], r);
break;
}
}
l += snprint(p+l, 2*READSTR-l, "eeprom:");
for(i = 0; i < 0x40; i++){
if(i && ((i & 0x07) == 0))
l += snprint(p+l, 2*READSTR-l, "\n       ");
l += snprint(p+l, 2*READSTR-l, " %4.4uX", ctlr->eeprom[i]);
}
snprint(p+l, 2*READSTR-l, "\ntxstalled %d\n", ctlr->txstalled);
n = readstr(offset, a, n, p);
free(p);
unlock(&ctlr->slock);
return n;
}
static void
gc82543promiscuous(void* arg, int on)
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
gc82543multicast(void* arg, uchar* addr, int on)
{
int bit, x;
Ctlr *ctlr;
Ether *edev;
edev = arg;
ctlr = edev->ctlr;
x = addr[5]>>1;
bit = ((addr[5] & 1)<<4)|(addr[4]>>4);
if(on)
ctlr->multimask[x] |= 1<<bit;
else
ctlr->multimask[x] &= ~(1<<bit);
csr32w(ctlr, Mta+x*4, ctlr->multimask[x]);
}
static long
gc82543ctl(Ether* edev, void* buf, long n)
{
Cmdbuf *cb;
Ctlr *ctlr;
int ctrl, i, r;
ctlr = edev->ctlr;
if(ctlr == nil)
error(Enonexist);
lock(&ctlr->slock);
r = 0;
cb = parsecmd(buf, n);
if(cb->nf < 2)
r = -1;
else if(cistrcmp(cb->f[0], "auto") == 0){
ctrl = csr32r(ctlr, Ctrl);
if(cistrcmp(cb->f[1], "off") == 0){
csr32w(ctlr, Txcw, ctlr->txcw & ~Ane);
ctrl |= (Slu|Fd);
if(ctlr->txcw & As)
ctrl |= Rfce;
if(ctlr->txcw & Ps)
ctrl |= Tfce;
csr32w(ctlr, Ctrl, ctrl);
}
else if(cistrcmp(cb->f[1], "on") == 0){
csr32w(ctlr, Txcw, ctlr->txcw);
ctrl &= ~(Slu|Fd);
csr32w(ctlr, Ctrl, ctrl);
}
else
r = -1;
}
else if(cistrcmp(cb->f[0], "clear") == 0){
if(cistrcmp(cb->f[1], "stats") == 0){
for(i = 0; i < Nstatistics; i++)
ctlr->statistics[i] = 0;
}
else
r = -1;
}
else
r = -1;
unlock(&ctlr->slock);
free(cb);
return (r == 0) ? n : r;
}
static void
gc82543txinit(Ctlr* ctlr)
{
int i;
int tdsize;
Block *bp, **bpp;
tdsize = ROUND(Ntdesc*sizeof(Tdesc), 4096);
if(ctlr->tdba == nil)
ctlr->tdba = xspanalloc(tdsize, 32, 0);
for(i = 0; i < Ntdesc; i++){
bpp = &ctlr->tb[i];
bp = *bpp;
if(bp != nil){
*bpp = nil;
freeb(bp);
}
memset(&ctlr->tdba[i], 0, sizeof(Tdesc));
}
csr32w(ctlr, Tdbal, PCIWADDR(ctlr->tdba));
csr32w(ctlr, Tdbah, 0);
csr32w(ctlr, Tdlen, Ntdesc*sizeof(Tdesc));
ctlr->tdh = 0;
csr32w(ctlr, Tdh, ctlr->tdh);
ctlr->tdt = 0;
csr32w(ctlr, Tdt, ctlr->tdt);
csr32w(ctlr, Tipg, (6<<20)|(8<<10)|6);
csr32w(ctlr, Tidv, 128);
csr32w(ctlr, Ait, 0);
csr32w(ctlr, Txdmac, 0);
csr32w(ctlr, Txdctl, Gran|(4<<WthreshSHIFT)|(1<<HthreshSHIFT)|16);
csr32w(ctlr, Tctl, (0x0F<<CtSHIFT)|Psp|(6<<ColdSHIFT));
ctlr->im |= Txdw;
}
static void
gc82543transmit(Ether* edev)
{
Block *bp, **bpp;
Ctlr *ctlr;
Tdesc *tdesc;
int tdh, tdt, s;
ctlr = edev->ctlr;
ilock(&ctlr->tdlock);
tdh = ctlr->tdh;
for(;;){
tdesc = &ctlr->tdba[tdh];
if(!(tdesc->status & Tdd))
break;
memset(tdesc, 0, sizeof(Tdesc));
bpp = &ctlr->tb[tdh];
bp = *bpp;
if(bp != nil){
*bpp = nil;
freeb(bp);
}
tdh = NEXT(tdh, Ntdesc);
}
ctlr->tdh = tdh;
s = csr32r(ctlr, Status);
if((s & (Txoff|Lu)) == Lu){
tdt = ctlr->tdt;
while(NEXT(tdt, Ntdesc) != tdh){
if((bp = qget(edev->oq)) == nil)
break;
tdesc = &ctlr->tdba[tdt];
tdesc->addr[0] = PCIWADDR(bp->rp);
tdesc->control = Ide|Rs|Ifcs|Teop|BLEN(bp);
ctlr->tb[tdt] = bp;
tdt = NEXT(tdt, Ntdesc);
}
if(tdt != ctlr->tdt){
ctlr->tdt = tdt;
csr32w(ctlr, Tdt, tdt);
}
}
else
ctlr->txstalled++;
iunlock(&ctlr->tdlock);
}
static Block *
gc82543allocb(Ctlr* ctlr)
{
Block *bp;
ilock(&freelistlock);
if((bp = *(ctlr->freehead)) != nil){
*(ctlr->freehead) = bp->next;
bp->next = nil;
}
iunlock(&freelistlock);
return bp;
}
static void
gc82543replenish(Ctlr* ctlr)
{
int rdt;
Block *bp;
Rdesc *rdesc;
ilock(&ctlr->rdlock);
rdt = ctlr->rdt;
while(NEXT(rdt, Nrdesc) != ctlr->rdh){
rdesc = &ctlr->rdba[rdt];
if(ctlr->rb[rdt] == nil){
bp = gc82543allocb(ctlr);
if(bp == nil){
iprint("no available buffers\n");
break;
}
ctlr->rb[rdt] = bp;
rdesc->addr[0] = PCIWADDR(bp->rp);
rdesc->addr[1] = 0;
}
coherence();
rdesc->status = 0;
rdt = NEXT(rdt, Nrdesc);
}
ctlr->rdt = rdt;
csr32w(ctlr, Rdt, rdt);
iunlock(&ctlr->rdlock);
}
static void
gc82543rxinit(Ctlr* ctlr)
{
int rdsize, i;
csr32w(ctlr, Rctl, Dpf|Bsize2048|Bam|RdtmsHALF);
rdsize = ROUND(Nrdesc*sizeof(Rdesc), 4096);
if(ctlr->rdba == nil)
ctlr->rdba = xspanalloc(rdsize, 32, 0);
memset(ctlr->rdba, 0, rdsize);
ctlr->rdh = 0;
ctlr->rdt = 0;
csr32w(ctlr, Rdtr, Fpd|64);
csr32w(ctlr, Rdbal, PCIWADDR(ctlr->rdba));
csr32w(ctlr, Rdbah, 0);
csr32w(ctlr, Rdlen, Nrdesc*sizeof(Rdesc));
csr32w(ctlr, Rdh, 0);
csr32w(ctlr, Rdt, 0);
for(i = 0; i < Nrdesc; i++){
if(ctlr->rb[i] != nil){
freeb(ctlr->rb[i]);
ctlr->rb[i] = nil;
}
}
gc82543replenish(ctlr);
csr32w(ctlr, Rxdctl, RxGran|(8<<WthreshSHIFT)|(4<<HthreshSHIFT)|1);
ctlr->im |= Rxt0|Rxo|Rxdmt0|Rxseq;
}
static void
gc82543recv(Ether* edev, int icr)
{
Block *bp;
Ctlr *ctlr;
Rdesc *rdesc;
int rdh;
ctlr = edev->ctlr;
rdh = ctlr->rdh;
for(;;){
rdesc = &ctlr->rdba[rdh];
if(!(rdesc->status & Rdd))
break;
if((rdesc->status & Reop) && rdesc->errors == 0){
bp = ctlr->rb[rdh];
ctlr->rb[rdh] = nil;
bp->wp += rdesc->length;
bp->next = nil;
etheriq(edev, bp, 1);
}
if(ctlr->rb[rdh] != nil){
freeb(ctlr->rb[rdh]);
ctlr->rb[rdh] = nil;
}
memset(rdesc, 0, sizeof(Rdesc));
coherence();
rdh = NEXT(rdh, Nrdesc);
}
ctlr->rdh = rdh;
if(icr & Rxdmt0)
gc82543replenish(ctlr);
}
static void
freegc82543short(Block *bp)
{
ilock(&freelistlock);
bp->rp = bp->lim - ROUND(SBLOCKSIZE, BLOCKALIGN);
bp->wp = bp->rp;
bp->next = freeShortHead;
freeShortHead = bp;
iunlock(&freelistlock);
}
static void
freegc82532jumbo(Block *bp)
{
ilock(&freelistlock);
bp->rp = bp->lim - ROUND(JBLOCKSIZE, BLOCKALIGN);
bp->wp = bp->rp;
bp->next = freeJumboHead;
freeJumboHead = bp;
iunlock(&freelistlock);
}
static void
linkintr(Ctlr* ctlr)
{
int ctrl;
ctrl = csr32r(ctlr, Ctrl);
if((ctrl & Swdpin1) ||
((csr32r(ctlr, Rxcw) & Rxconfig) && !(csr32r(ctlr, Txcw) & Ane))){
csr32w(ctlr, Txcw, ctlr->txcw);
ctrl &= ~(Slu|Fd|Frcdplx);
csr32w(ctlr, Ctrl, ctrl);
}
}
static void
gc82543interrupt(Ureg*, void* arg)
{
Ctlr *ctlr;
Ether *edev;
int icr;
edev = arg;
ctlr = edev->ctlr;
while((icr = csr32r(ctlr, Icr) & ctlr->im) != 0){
if(icr & (Lsc|Rxseq))
linkintr(ctlr);
gc82543recv(edev, icr);
gc82543transmit(edev);
}
}
static int
gc82543init(Ether* edev)
{
int csr, i;
Block *bp;
Ctlr *ctlr;
ctlr = edev->ctlr;
ilock(&freelistlock);
if (ctlr->freehead == nil){
for(i = 0; i < Nblocks; i++){
bp = iallocb(SBLOCKSIZE);
if(bp != nil){
bp->next = freeShortHead;
bp->free = freegc82543short;
freeShortHead = bp;
}
else{
print("82543gc: no memory\n");
break;
}
}
ctlr->freehead = &freeShortHead;
}
iunlock(&freelistlock);
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
gc82543txinit(ctlr);
gc82543rxinit(ctlr);
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
if(lp != nil){
if(p != (lp+1) || loop != 7)
return -1;
lp = p;
loop = 15;
continue;
}
lp = p;
loop = 7;
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
microdelay(1);
}
if(loop >= 0)
return -1;
return r;
}
static int
at93c46r(Ctlr* ctlr)
{
ushort sum;
int addr, data;
sum = 0;
for(addr = 0; addr < 0x40; addr++){
if(at93c46io(ctlr, "S ICc :DCc;", (0x02<<6)|addr) != 0)
break;
data = at93c46io(ctlr, "::COc;", 0);
at93c46io(ctlr, "sic", 0);
ctlr->eeprom[addr] = data;
sum += data;
}
return sum;
}
static void
gc82543detach(Ctlr* ctlr)
{
csr32w(ctlr, Imc, ~0);
csr32w(ctlr, Rctl, 0);
csr32w(ctlr, Tctl, 0);
delay(10);
csr32w(ctlr, Ctrl, Devrst);
while(csr32r(ctlr, Ctrl) & Devrst)
;
csr32w(ctlr, Ctrlext, Eerst);
while(csr32r(ctlr, Ctrlext) & Eerst)
;
csr32w(ctlr, Imc, ~0);
while(csr32r(ctlr, Icr))
;
}
static void
gc82543checklink(Ctlr* ctlr)
{
int ctrl, status, rxcw;
ctrl = csr32r(ctlr, Ctrl);
status = csr32r(ctlr, Status);
rxcw = csr32r(ctlr, Rxcw);
if(!(status & Lu)){
if(!(ctrl & (Swdpin1|Slu)) && !(rxcw & Rxconfig)){
csr32w(ctlr, Txcw, ctlr->txcw & ~Ane);
ctrl |= (Slu|Fd);
if(ctlr->txcw & As)
ctrl |= Rfce;
if(ctlr->txcw & Ps)
ctrl |= Tfce;
csr32w(ctlr, Ctrl, ctrl);
}
}
else if((ctrl & Slu) && (rxcw & Rxconfig)){
csr32w(ctlr, Txcw, ctlr->txcw);
ctrl &= ~(Slu|Fd);
csr32w(ctlr, Ctrl, ctrl);
}
}
static void
gc82543shutdown(Ether* ether)
{
gc82543detach(ether->ctlr);
}
static int
gc82543reset(Ctlr* ctlr)
{
int ctl;
int te;
if(at93c46r(ctlr) != 0xBABA)
return -1;
gc82543detach(ctlr);
te = ctlr->eeprom[Icw2];
if((te & 0x3000) == 0){
ctlr->fcrtl = 0x00002000;
ctlr->fcrth = 0x00004000;
ctlr->txcw = Ane|TxcwFd;
}
else if((te & 0x3000) == 0x2000){
ctlr->fcrtl = 0;
ctlr->fcrth = 0;
ctlr->txcw = Ane|TxcwFd|As;
}
else{
ctlr->fcrtl = 0x00002000;
ctlr->fcrth = 0x00004000;
ctlr->txcw = Ane|TxcwFd|As|Ps;
}
csr32w(ctlr, Txcw, ctlr->txcw);
csr32w(ctlr, Ctrlext, (te & 0x00f0)<<4);
csr32w(ctlr, Tctl, csr32r(ctlr, Tctl)|(64<<ColdSHIFT));
te = ctlr->eeprom[Icw1];
ctl = ((te & 0x01E0)<<17)|(te & 0x0010)<<3;
csr32w(ctlr, Ctrl, ctl);
delay(10);
csr32w(ctlr, Fcal, 0x00C28001);
csr32w(ctlr, Fcah, 0x00000100);
csr32w(ctlr, Fct, 0x00008808);
csr32w(ctlr, Fcttv, 0x00000100);
csr32w(ctlr, Fcrtl, ctlr->fcrtl);
csr32w(ctlr, Fcrth, ctlr->fcrth);
ctlr->im = Lsc;
gc82543checklink(ctlr);
return 0;
}
static void
gc82543watchdog(void* arg)
{
Ether *edev;
Ctlr *ctlr;
edev = arg;
for(;;){
tsleep(&up->sleep, return0, 0, 1000);
ctlr = edev->ctlr;
if(ctlr == nil){
print("%s: exiting\n", up->text);
pexit("disabled", 0);
}
gc82543checklink(ctlr);
gc82543replenish(ctlr);
}
}
static void
gc82543pci(void)
{
int cls;
void *mem;
Pcidev *p;
Ctlr *ctlr;
p = nil;
while(p = pcimatch(p, 0, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
switch((p->did<<16)|p->vid){
case (0x1000<<16)|0x8086:
case (0x1004<<16)|0x8086:
case (0x1008<<16)|0x8086:
default:
continue;
case (0x1001<<16)|0x8086:
break;
}
mem = vmap(p->mem[0].bar & ~0x0F, p->mem[0].size);
if(mem == 0){
print("gc82543: can't map %8.8luX\n", p->mem[0].bar);
continue;
}
cls = pcicfgr8(p, PciCLS);
switch(cls){
case 0x00:
case 0xFF:
print("82543gc: unusable cache line size\n");
continue;
case 0x08:
break;
default:
print("82543gc: cache line size %d, expected 32\n",
cls*4);
}
ctlr = malloc(sizeof(Ctlr));
ctlr->port = p->mem[0].bar & ~0x0F;
ctlr->pcidev = p;
ctlr->id = (p->did<<16)|p->vid;
ctlr->nic = mem;
if(gc82543reset(ctlr)){
free(ctlr);
continue;
}
if(gc82543ctlrhead != nil)
gc82543ctlrtail->next = ctlr;
else
gc82543ctlrhead = ctlr;
gc82543ctlrtail = ctlr;
}
}
static int
gc82543pnp(Ether* edev)
{
int i;
Ctlr *ctlr;
uchar ea[Eaddrlen];
if(gc82543ctlrhead == nil)
gc82543pci();
for(ctlr = gc82543ctlrhead; ctlr != nil; ctlr = ctlr->next){
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
memset(ea, 0, Eaddrlen);
if(memcmp(ea, edev->ea, Eaddrlen) == 0){
for(i = Ea; i < Eaddrlen/2; i++){
edev->ea[2*i] = ctlr->eeprom[i];
edev->ea[2*i+1] = ctlr->eeprom[i]>>8;
}
}
gc82543init(edev);
edev->attach = gc82543attach;
edev->transmit = gc82543transmit;
edev->interrupt = gc82543interrupt;
edev->ifstat = gc82543ifstat;
edev->shutdown = gc82543shutdown;
edev->ctl = gc82543ctl;
edev->arg = edev;
edev->promiscuous = gc82543promiscuous;
edev->multicast = gc82543multicast;
return 0;
}
void
ether82543gclink(void)
{
addethercard("82543GC", gc82543pnp);
}