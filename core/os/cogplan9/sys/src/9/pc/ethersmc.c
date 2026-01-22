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
IoSize		= 0x10,
TxTimeout	= 150,
};
enum {
TupleFunce	= 0x22,
TfNodeId	= 0x04,
};
enum {
Tcr		= 0x0000,
Eph		= 0x0002,
Rcr		= 0x0004,
Counter		= 0x0006,
MemInfo		= 0x0008,
MemCfg		= 0x000A,
};
enum {
Config		= 0x0000,
BaseAddr	= 0x0002,
Addr0		= 0x0004,
Addr1		= 0x0006,
Addr2		= 0x0008,
General		= 0x000A,
Control		= 0x000C,
};
enum {
MmuCmd		= 0x0000,
PktNo		= 0x0002,
AllocRes	= 0x0003,
FifoPorts	= 0x0004,
Pointer		= 0x0006,
Data1		= 0x0008,
Interrupt	= 0x000C,
IntrMask	= 0x000D,
};
enum {
Mcast0		= 0x0000,
Mcast2		= 0x0002,
Mcast4		= 0x0004,
Mcast6		= 0x0006,
Revision	= 0x000A,
};
enum {
BankSelect	= 0x000E
};
enum {
BsrMask		= 0xFF00,
BsrId		= 0x3300,
};
enum {
TcrClear	= 0x0000,
TcrEnable	= 0x0001,
TcrLoop		= 0x0002,
TcrForceCol	= 0x0004,
TcrPadEn	= 0x0080,
TcrNoCrc	= 0x0100,
TcrMonCns	= 0x0400,
TcrFduplx	= 0x0800,
TcrStpSqet	= 0x1000,
TcrEphLoop	= 0x2000,
TcrNormal	= TcrEnable,
};
enum {
EphTxOk		= 0x0001,
Eph1Col		= 0x0002,
EphMCol		= 0x0004,
EphTxMcast	= 0x0008,
Eph16Col	= 0x0010,
EphSqet		= 0x0020,
EphTxBcast	= 0x0040,
EphDefr		= 0x0080,
EphLatCol	= 0x0200,
EphLostCarr	= 0x0400,
EphExcDefr	= 0x0800,
EphCntRol	= 0x1000,
EphRxOvrn	= 0x2000,
EphLinkOk	= 0x4000,
EphTxUnrn	= 0x8000,
};
enum {
RcrClear	= 0x0000,
RcrPromisc	= 0x0002,
RcrAllMcast	= 0x0004,
RcrEnable	= 0x0100,
RcrStripCrc	= 0x0200,
RcrSoftReset	= 0x8000,
RcrNormal	= RcrStripCrc | RcrEnable,
};
enum {
CntColMask	= 0x000F,
CntMColMask	= 0x00F0,
CntDtxMask	= 0x0F00,
CntExDtxMask	= 0xF000,
CntColShr	= 1,
CntMColShr	= 4,
CntDtxShr	= 8,
};
enum {
MirTotalMask	= 0x00FF,
MirFreeMask	= 0xFF00,
};
enum {
CfgIrqSel0	= 0x0002,
CfgIrqSel1	= 0x0004,
CfgDisLink	= 0x0040,
Cfg16Bit	= 0x0080,
CfgAuiSelect	= 0x0100,
CfgSetSqlch	= 0x0200,
CfgFullStep	= 0x0400,
CfgNoWait	= 0x1000,
CfgMiiSelect	= 0x8000,
};
enum {
CtlStore	= 0x0001,
CtlReload	= 0x0002,
CtlEeSelect	= 0x0004,
CtlTeEnable	= 0x0020,
CtlCrEnable	= 0x0040,
CtlLeEnable	= 0x0080,
CtlAutoRls	= 0x0800,
CtlPowerDn	= 0x2000,
};
enum {
McBusy		= 0x0001,
McAlloc		= 0x0020,
McReset		= 0x0040,
McRelease	= 0x0080,
McFreePkt	= 0x00A0,
McEnqueue	= 0x00C0,
McTxReset	= 0x00E0,
};
enum {
ArFailed	= 0x80,
};
enum {
FpTxEmpty	= 0x0080,
FpRxEmpty	= 0x8000,
FpTxMask	= 0x007F,
FpRxMask	= 0x7F00,
};
enum {
PtrRead		= 0x2000,
PtrAutoInc	= 0x4000,
PtrRcv		= 0x8000,
};
enum {
IntRcv		= 0x0001,
IntTxError	= 0x0002,
IntTxEmpty	= 0x0004,
IntAlloc	= 0x0008,
IntRxOvrn	= 0x0010,
IntEph		= 0x0020,
};
enum {
TsSuccess	= 0x0001,
Ts16Col		= 0x00A0,
TsLatCol	= 0x0200,
TsLostCar	= 0x0400,
};
enum {
RsMcast		= 0x0001,
RsTooShort	= 0x0400,
RsTooLong	= 0x0800,
RsOddFrame	= 0x1000,
RsBadCrc	= 0x2000,
RsAlgnErr	= 0x8000,
RsError		= RsAlgnErr | RsBadCrc | RsTooLong | RsTooShort,
};
enum {
RxLenMask	= 0x07FF,
HdrSize		= 6,
PageSize	= 256,
};
typedef struct Smc91xx Smc91xx;
struct Smc91xx {
Lock;
ushort rev;
int attached;
Block *txbp;
ulong txtime;
ulong rovrn;
ulong lcar;
ulong col;
ulong scol;
ulong mcol;
ulong lcol;
ulong dfr;
};
#define SELECT_BANK(x) outs(port + BankSelect, x)
static int
readnodeid(int slot, Ether* ether)
{
uchar data[Eaddrlen + 1];
int len;
len = sizeof(data);
if (pcmcistuple(slot, TupleFunce, TfNodeId, data, len) != len)
return -1;
if (data[0] != Eaddrlen)
return -1;
memmove(ether->ea, &data[1], Eaddrlen);
return 0;
}
static void
chipreset(Ether* ether)
{
int port;
int i;
port = ether->port;
SELECT_BANK(0);
outs(port + Rcr, RcrSoftReset);
delay(1);
outs(port + Rcr, RcrClear);
outs(port + Tcr, TcrClear);
SELECT_BANK(1);
outs(port + Control, CtlAutoRls | CtlTeEnable |
CtlCrEnable);
for(i = 0; i < 6; i++) {
outb(port + Addr0 +  i, ether->ea[i]);
}
SELECT_BANK(2);
outs(port + MmuCmd, McReset);
}
static void
chipenable(Ether* ether)
{
int port;
port = ether->port;
SELECT_BANK(0);
outs(port + Tcr, TcrNormal);
outs(port + Rcr, RcrNormal);
SELECT_BANK(2);
outb(port + IntrMask, IntEph | IntRxOvrn | IntRcv);
}
static void
attach(Ether *ether)
{
Smc91xx* ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
if (ctlr->attached) {
iunlock(ctlr);
return;
}
chipenable(ether);
ctlr->attached = 1;
iunlock(ctlr);
}
static void
txstart(Ether* ether)
{
int port;
Smc91xx* ctlr;
Block* bp;
int len, npages;
int pno;
port = ether->port;
ctlr = ether->ctlr;
if (ctlr->txbp) {
bp = ctlr->txbp;
ctlr->txbp = 0;
} else {
bp = qget(ether->oq);
if (bp == 0)
return;
len = BLEN(bp);
npages = (len + HdrSize) / PageSize;
outs(port + MmuCmd, McAlloc | npages);
}
pno = inb(port + AllocRes);
if (pno & ArFailed) {
outb(port + IntrMask, inb(port + IntrMask) | IntAlloc);
ctlr->txbp = bp;
ctlr->txtime = MACHP(0)->ticks;
return;
}
outb(port + PktNo, pno);
outs(port + Pointer, PtrAutoInc);
len = BLEN(bp);
outs(port + Data1, 0);
outb(port + Data1, (len + HdrSize) & 0xFF);
outb(port + Data1, (len + HdrSize) >> 8);
outss(port + Data1, bp->rp, len / 2);
if ((len & 1) == 0) {
outs(port + Data1, 0);
} else {
outb(port + Data1, bp->rp[len - 1]);
outb(port + Data1, 0x20);
}
outb(port + IntrMask, inb(port + IntrMask) |
IntTxError | IntTxEmpty);
outs(port + MmuCmd, McEnqueue);
freeb(bp);
}
static void
receive(Ether* ether)
{
int port;
Block* bp;
int pktno, status, len;
port = ether->port;
pktno = ins(port + FifoPorts);
if (pktno & FpRxEmpty) {
return;
}
outs(port + Pointer, PtrRead | PtrRcv | PtrAutoInc);
status = ins(port + Data1);
len = ins(port + Data1) & RxLenMask - HdrSize;
if (status & RsOddFrame)
len++;
if ((status & RsError) || (bp = iallocb(len)) == 0) {
if (status & RsAlgnErr)
ether->frames++;
if (status & (RsTooShort | RsTooLong))
ether->buffs++;
if (status & RsBadCrc)
ether->crcs++;
outs(port + MmuCmd, McRelease);
return;
}
inss(port + Data1, bp->rp, len / 2);
bp->wp = bp->rp + (len & ~1);
if (len & 1) {
*bp->wp = inb(port + Data1);
bp->wp++;
}
etheriq(ether, bp, 1);
ether->inpackets++;
outs(port + MmuCmd, McRelease);
}
static void
txerror(Ether* ether)
{
int port;
Smc91xx* ctlr;
int save_pkt;
int pktno, status;
port = ether->port;
ctlr = ether->ctlr;
save_pkt = inb(port + PktNo);
pktno = ins(port + FifoPorts) & FpTxMask;
outb(port + PktNo, pktno);
outs(port + Pointer, PtrAutoInc | PtrRead);
status = ins(port + Data1);
if (status & TsLostCar)
ctlr->lcar++;
if (status & TsLatCol)
ctlr->lcol++;
if (status & Ts16Col)
ctlr->scol++;
ether->oerrs++;
SELECT_BANK(0);
outs(port + Tcr, ins(port + Tcr) | TcrEnable);
SELECT_BANK(2);
outs(port + MmuCmd, McFreePkt);
outb(port + PktNo, save_pkt);
}
static void
eph_irq(Ether* ether)
{
int port;
Smc91xx* ctlr;
ushort status;
int n;
port = ether->port;
ctlr = ether->ctlr;
SELECT_BANK(0);
status = ins(port + Eph);
if (status & EphCntRol) {
n = ins(port + Counter);
ctlr->col += (n & CntColMask) >> CntColShr;
ctlr->mcol += (n & CntMColMask) >> CntMColShr;
ctlr->dfr += (n & CntDtxMask) >> CntDtxShr;
}
outs(port + Tcr, ins(port + Tcr) | TcrEnable);
SELECT_BANK(1);
outs(port + Control, CtlAutoRls);
outs(port + Control, CtlAutoRls | CtlTeEnable | CtlCrEnable);
SELECT_BANK(2);
}
static void
transmit(Ether* ether)
{
Smc91xx* ctlr;
int port, n;
ctlr = ether->ctlr;
port = ether->port;
ilock(ctlr);
if (ctlr->txbp) {
n = TK2MS(MACHP(0)->ticks - ctlr->txtime);
if (n > TxTimeout) {
chipreset(ether);
chipenable(ether);
freeb(ctlr->txbp);
ctlr->txbp = 0;
}
iunlock(ctlr);
return;
}
SELECT_BANK(2);
txstart(ether);
iunlock(ctlr);
}
static void
interrupt(Ureg*, void *arg)
{
int port;
Smc91xx* ctlr;
Ether* ether;
int save_bank;
int save_pointer;
int mask, status;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
ilock(ctlr);
save_bank = ins(port + BankSelect);
SELECT_BANK(2);
save_pointer = ins(port + Pointer);
mask = inb(port + IntrMask);
outb(port + IntrMask, 0);
while ((status = inb(port + Interrupt) & mask) != 0) {
if (status & IntRcv) {
receive(ether);
}
if (status & IntTxError) {
txerror(ether);
}
if (status & IntTxEmpty) {
outb(port + Interrupt, IntTxEmpty);
outb(port + IntrMask, mask & ~IntTxEmpty);
txstart(ether);
mask = inb(port + IntrMask);
}
if (status & IntAlloc) {
outb(port + IntrMask, mask & ~IntAlloc);
txstart(ether);
mask = inb(port + IntrMask);
}
if (status & IntRxOvrn) {
ctlr->rovrn++;
ether->misses++;
outb(port + Interrupt,IntRxOvrn);
}
if (status & IntEph)
eph_irq(ether);
}
outb(port + IntrMask, mask);
outs(port + Pointer, save_pointer);
outs(port + BankSelect, save_bank);
iunlock(ctlr);
}
static void
promiscuous(void* arg, int on)
{
int port;
Smc91xx *ctlr;
Ether* ether;
ushort x;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
ilock(ctlr);
SELECT_BANK(0);
x = ins(port + Rcr);
if (on)
x |= RcrPromisc;
else
x &= ~RcrPromisc;
outs(port + Rcr, x);
iunlock(ctlr);
}
static void
multicast(void* arg, uchar *addr, int on)
{
int port;
Smc91xx*ctlr;
Ether *ether;
ushort x;
USED(addr, on);
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
ilock(ctlr);
SELECT_BANK(0);
x = ins(port + Rcr);
if (ether->nmaddr)
x |= RcrAllMcast;
else
x &= ~RcrAllMcast;
outs(port + Rcr, x);
iunlock(ctlr);
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
static char *chiprev[] = {
[3] 	"92",
[5]	"95",
[7]	"100",
[8]	"100-FD",
[9]	"110",
};
Smc91xx* ctlr;
char *p, *s;
int r, len;
if (n == 0)
return 0;
ctlr = ether->ctlr;
p = malloc(READSTR);
if(p == nil)
error(Enomem);
s = 0;
if (ctlr->rev > 0) {
r = ctlr->rev >> 4;
if (r < nelem(chiprev))
s = chiprev[r];
if (r == 4) {
if ((ctlr->rev & 0x0F) >= 6)
s = "96";
else
s = "94";
}
}
len = snprint(p, READSTR, "rev: 91c%s\n", (s) ? s : "???");
len += snprint(p + len, READSTR - len, "rxovrn: %uld\n", ctlr->rovrn);
len += snprint(p + len, READSTR - len, "lcar: %uld\n", ctlr->lcar);
len += snprint(p + len, READSTR - len, "col: %uld\n", ctlr->col);
len += snprint(p + len, READSTR - len, "16col: %uld\n", ctlr->scol);
len += snprint(p + len, READSTR - len, "mcol: %uld\n", ctlr->mcol);
len += snprint(p + len, READSTR - len, "lcol: %uld\n", ctlr->lcol);
len += snprint(p + len, READSTR - len, "dfr: %uld\n", ctlr->dfr);
USED(len);
n = readstr(offset, a, n, p);
free(p);
return n;
}
static int
reset(Ether* ether)
{
int port;
int i, x;
char* type;
Smc91xx* ctlr;
int slot;
uchar ea[Eaddrlen];
if (ether->irq == 0)
ether->irq = 9;
if (ether->port == 0)
ether->port = 0x100;
type = "8020";
for(i = 0; i < ether->nopt; i++) {
if (cistrncmp(ether->opt[i], "id=", 3))
continue;
type = &ether->opt[i][3];
break;
}
if ((slot = pcmspecial(type, ether)) < 0)
return -1;
if (ioalloc(ether->port, IoSize, 0, "smc91cXX") < 0) {
pcmspecialclose(slot);
return -1;
}
ether->ctlr = malloc(sizeof(Smc91xx));
ctlr = ether->ctlr;
if (ctlr == 0) {
iofree(ether->port);
pcmspecialclose(slot);
return -1;
}
ilock(ctlr);
ctlr->rev = 0;
ctlr->txbp = nil;
ctlr->attached = 0;
ctlr->rovrn = 0;
ctlr->lcar = 0;
ctlr->col = 0;
ctlr->scol = 0;
ctlr->mcol = 0;
ctlr->lcol = 0;
ctlr->dfr = 0;
port = ether->port;
SELECT_BANK(1);
if ((ins(port + BankSelect) & BsrMask) != BsrId) {
outs(port + Control, 0);
delay(55);
}
outs(port + Config, ins(port + Config) | Cfg16Bit);
x = ins(port + BaseAddr);
if (((ins(port + BankSelect) & BsrMask) != BsrId) ||
((x >> 8) == (x & 0xFF))) {
iunlock(ctlr);
iofree(port);
pcmspecialclose(slot);
return -1;
}
SELECT_BANK(3);
ctlr->rev = ins(port + Revision) & 0xFF;
memset(ea, 0, Eaddrlen);
if (memcmp(ea, ether->ea, Eaddrlen) == 0) {
if (readnodeid(slot, ether) < 0) {
print("Smc91cXX: cannot find ethernet address\n");
iunlock(ctlr);
iofree(port);
pcmspecialclose(slot);
return -1;
}
}
chipreset(ether);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
ether->shutdown = chipreset;
ether->arg = ether;
iunlock(ctlr);
return 0;
}
void
ethersmclink(void)
{
addethercard("smc91cXX", reset);
}