#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/netif.h"
#include "etherif.h"
typedef struct Ctlr Ctlr;
enum {
IsaIOBase		= 0x08000000,
IsaMemBase	= 0xe0000000,
IOBase		= 0x300,
MemBase		= 0xc0000,
MEMORY = 0,
DORESET = 1,
DEBUG = 0,
};
#define	IOSHIFT	0
#define	IOREG(r) (IsaIOBase+((IOBase+(r))<<IOSHIFT))
#define	out16(port, val)	(*((ushort *)IOREG(port)) = (val))
#define	in16(port)			*((ushort *)IOREG(port))
#define	in8(port)			*((uchar *)IOREG(port))
#define	regIOw(reg, val)	 do {out16(PpPtr, (reg)|0x3000); out16(PpData, val);} while(0)
#define	regIOr(reg)		(out16(PpPtr, (reg)|0x3000), in16(PpData))
#define	regIOr1(reg)		(out16(PpPtr, (reg)|0x3000), in16(PpData1))
#define	REGW(reg, val)		*((ushort *)IsaMemBase + MemBase + (reg)) = (val)
#define	REGR(reg)			*((ushort *)IsaMemBase + MemBase + (reg))
enum {
RxTxData	= 0x00,
RxTxData1 = 0x02,
TxCmdIO = 0x04,
TxLenIO	= 0x06,
IsqIO	= 0x08,
PpPtr	= 0x0a,
PpData	= 0x0c,
PpData1	= 0x0e,
};
enum {
Ern		= 0x0000,
Pic		= 0x0002,
Iob		= 0x0020,
Intr		= 0x0022,
Mba		= 0x002c,
Ecr		= 0x0040,
Edw		= 0x0042,
Rbc		= 0x0050,
RxCfg	= 0x0102,
RxCtl	= 0x0104,
TxCfg	= 0x0106,
BufCfg	= 0x010a,
LineCtl	= 0x0112,
SelfCtl	= 0x0114,
BusCtl	= 0x0116,
TestCtl	= 0x0118,
Isq		= 0x0120,
RxEvent	= 0x0124,
TxEvent	= 0x0128,
BufEvent	= 0x012c,
RxMISS	= 0x0130,
TxCol	= 0x0132,
LineSt	= 0x0134,
SelfSt	= 0x0136,
BusSt	= 0x0138,
Tdr		= 0x013c,
TxCmd	= 0x0144,
TxLen	= 0x0146,
IndAddr	= 0x0158,
RxStatus	= 0x0400,
RxLen	= 0x0402,
RxFrame	= 0x0404,
TxFrame	= 0x0a00,
};
enum {
Addr			= 0x00ff,
Opcode		= 0x0300,
EEread	= 0x0200,
EEwrite	= 0x0100,
};
enum {
Regnum		= 0x003f,
IsqRxEvent	= 0x04,
IsqTxEvent	= 0x08,
IsqBufEvent	= 0x0c,
IsqRxMiss		= 0x10,
IsqTxCol		= 0x12,
RegContent 	= 0xffc0,
};
enum {
Skip_1		= 0x0040,
StreamE		= 0x0080,
RxOKiE		= 0x0100,
RxDMAonly	= 0x0200,
AutoRxDMAE	= 0x0400,
BufferCRC		= 0x0800,
CRCerroriE	= 0x1000,
RuntiE		= 0x2000,
ExtradataiE	= 0x4000,
};
enum {
IAHash		= 0x0040,
Dribblebits	= 0x0080,
RxOK		= 0x0100,
Hashed		= 0x0200,
IndividualAdr	= 0x0400,
Broadcast		= 0x0800,
CRCerror		= 0x1000,
Runt			= 0x2000,
Extradata		= 0x4000,
};
enum {
IAHashA		= 0x0040,
PromiscuousA	= 0x0080,
RxOKA		= 0x0100,
MulticastA	= 0x0200,
IndividualA	= 0x0400,
BroadcastA	= 0x0800,
CRCerrorA	= 0x1000,
RuntA		= 0x2000,
ExtradataA	= 0x4000,
};
enum {
LossofCRSiE	= 0x0040,
SQEerroriE	= 0x0080,
TxOKiE		= 0x0100,
OutofWindowiE	= 0x0200,
JabberiE		= 0x0400,
AnycolliE		= 0x0800,
Coll16iE		= 0x8000,
};
enum {
LossofCRS	= 0x0040,
SQEerror		= 0x0080,
TxOK		= 0x0100,
OutofWindow	= 0x0200,
Jabber		= 0x0400,
NTxCols		= 0x7800,
coll16		= 0x8000,
};
enum {
SWintX		= 0x0040,
RxDMAiE		= 0x0080,
Rdy4TxiE		= 0x0100,
TxUnderruniE	= 0x0200,
RxMissiE		= 0x0400,
Rx128iE		= 0x0800,
TxColOvfiE	= 0x1000,
MissOvfloiE	= 0x2000,
RxDestiE		= 0x8000,
};
enum {
SWint		= 0x0040,
RxDMAFrame	= 0x0080,
Rdy4Tx		= 0x0100,
TxUnderrun	= 0x0200,
RxMiss		= 0x0400,
Rx128		= 0x0800,
RxDest		= 0x8000,
};
enum {
MissCount	= 0xffc0,
};
enum {
ColCount	= 0xffc0,
};
enum {
SerRxOn		= 0x0040,
SerTxOn		= 0x0080,
Iface			= 0x0300,
ModBackoffE	= 0x0800,
PolarityDis	= 0x1000,
DefDis		= 0x2000,
LoRxSquelch	= 0x4000,
};
enum {
LinkOK		= 0x0080,
AUI			= 0x0100,
TenBT		= 0x0200,
PolarityOK	= 0x1000,
CRS			= 0x4000,
};
enum {
RESET		= 0x0040,
SWSuspend	= 0x0100,
HWSleepE		= 0x0200,
HWStandbyE	= 0x0400,
};
enum {
Active3V		= 0x0040,
INITD		= 0x0080,
SIBUSY		= 0x0100,
EepromPresent	= 0x0200,
EepromOK	= 0x0400,
ElPresent		= 0x0800,
EeSize		= 0x1000,
};
enum {
ResetRxDMA	= 0x0040,
UseSA		= 0x0200,
MemoryE		= 0x0400,
DMABurst		= 0x0800,
EnableIRQ		= 0x8000,
};
enum {
TxBidErr		= 0x0080,
Rdy4TxNOW	= 0x0100,
};
enum {
FDX			= 0x4000,
};
enum {
TxStart		= 0x00c0,
TxSt5	= 0x0000,
TxSt381	= 0x0040,
TxSt1021	= 0x0080,
TxStAll	= 0x00c0,
Force		= 0x0100,
Onecoll		= 0x0200,
InhibitCRC	= 0x1000,
TxPadDis		= 0x2000,
};
enum {
Edataoff	= 0x1C,
Edatalen	= 0x14,
};
struct Ctlr {
Lock;
Block*	waiting;
int	model;
int	rev;
ulong	collisions;
};
static void
regw(int reg, int val)
{
if(DEBUG)
print("r%4.4ux <- %4.4ux\n", reg, val);
if(MEMORY){
REGW(reg, val);
}else{
out16(PpPtr, reg);
out16(PpData, val);
}
}
static int
regr(int reg)
{
int v;
if(MEMORY)
return REGR(reg);
out16(PpPtr, reg);
v = in16(PpData);
if(DEBUG)
print("r%4.4ux = %4.4ux\n", reg, v);
return v;
}
static void
copypktin(void *ad, int len)
{
ushort *s, *d;
int ns;
if(!MEMORY){
d = ad;
for(ns = len>>1; --ns >= 0;)
*d++ = in16(RxTxData);
if(len & 1)
*(uchar*)d = in16(RxTxData);
return;
}
d = ad;
s = (ushort*)IsaMemBase + MemBase + RxFrame;
for(ns = len>>1; --ns >= 0;){
*d++ = *s;
s += 2;
}
if(len & 1)
*(uchar*)d = *s;
}
static void
copypktout(void *as, int len)
{
ushort *s, *d;
int ns;
if(!MEMORY){
s = as;
ns = (len+1)>>1;
while(--ns >= 0)
out16(RxTxData, *s++);
return;
}
s = as;
d = (ushort*)IsaMemBase + MemBase + TxFrame;
ns = (len+1)>>1;
while(--ns >= 0){
*d = *s++;
d += 2;
}
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
Ctlr *ctlr;
char *p;
int len;
if(n == 0)
return 0;
ctlr = ether->ctlr;
p = malloc(READSTR);
len = snprint(p, READSTR, "Overflow: %ud\n", ether->overflows);
len += snprint(p+len, READSTR-len, "CRC Error: %ud\n", ether->crcs);
snprint(p+len, READSTR-len, "Collision Seen: %lud\n", ctlr->collisions);
n = readstr(offset, a, n, p);
free(p);
return n;
}
static void
promiscuous(void* arg, int on)
{
USED(arg, on);
}
static void
attach(Ether *ether)
{
int reg;
USED(ether);
reg = regr(BusCtl);
regw(BusCtl, reg|EnableIRQ);
reg = regr(LineCtl);
regw(LineCtl, reg|SerRxOn|SerTxOn);
if(DEBUG){
iprint("bus=%4.4ux line=%4.4ux\n", regr(BusCtl), regr(LineCtl));
iprint("rc=%4.4ux tc=%4.4ux bc=%4.4ux\n", regr(RxCfg), regr(TxCfg), regr(BufCfg));
}
}
static void
txstart(Ether *ether, int dowait)
{
int len, status;
Ctlr *ctlr;
Block *b;
ctlr = ether->ctlr;
for(;;){
if((b = ctlr->waiting) == nil){
if((b = qget(ether->oq)) == nil)
break;
}else{
if(!dowait)
break;
ctlr->waiting = nil;
}
len = BLEN(b);
if(MEMORY){
regw(TxCmd, TxSt381);
regw(TxLen, len);
}else{
out16(TxCmdIO, TxStAll);
out16(TxLenIO, len);
}
status = regr(BusSt);
if((status & Rdy4TxNOW) == 0) {
ctlr->waiting = b;
break;
}
copypktout(b->rp, len);
freeb(b);
}
}
static void
transmit(Ether *ether)
{
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(ctlr);
txstart(ether, 0);
iunlock(ctlr);
}
static void
interrupt(Ureg*, void *arg)
{
Ether *ether;
Ctlr *ctlr;
int len, events, status;
Block *b;
ether = arg;
ctlr = ether->ctlr;
ilock(ctlr);
while((events = (MEMORY?regr(Isq):in16(IsqIO))) != 0) {
status = events&RegContent;
if(DEBUG)
iprint("status %4.4ux event %4.4ux\n", status, events);
switch(events&Regnum) {
case IsqBufEvent:
if(status&Rdy4Tx) {
if((b = ctlr->waiting) != nil){
ctlr->waiting = nil;
copypktout(b->rp, BLEN(b));
freeb(b);
}else
txstart(ether, 0);
}
break;
case IsqRxEvent:
if(status&RxOK) {
len = regr(RxLen);
if(DEBUG)
iprint("rxlen=%d\n", len);
if((b = iallocb(len)) != 0) {
copypktin(b->wp, len);
b->wp += len;
etheriq(ether, b, 1);
}
}
break;
case IsqTxEvent:
if(status&TxOK)
txstart(ether, 1);
break;
case IsqRxMiss:
ether->overflows++;
break;
case IsqTxCol:
ctlr->collisions++;
break;
}
}
iunlock(ctlr);
}
static int
eepromwait(void)
{
int i;
for(i=0; i<100000; i++)
if((regIOr(SelfSt) & SIBUSY) == 0)
return 0;
return -1;
}
static int
eepromrd(void *buf, int off, int n)
{
int i;
ushort *p;
p = buf;
n /= 2;
for(i=0; i<n; i++){
if(eepromwait() < 0)
return -1;
regIOw(Ecr, EEread | (off+i));
if(eepromwait() < 0)
return -1;
p[i] = regIOr(Edw);
}
return 0;
}
static int
reset(Ether* ether)
{
int i, reg, easet;
uchar ea[Eaddrlen];
ushort buf[Edatalen];
Ctlr *ctlr;
if(!MEMORY)
mmuphysmap(IsaIOBase, 64*1024);
delay(120);
if(0){
*(ushort*)IsaIOBase = 0xDEAD;
for(i=0; i<100; i++){
if(in16(PpPtr) == 0x3000)
break;
delay(1);
}
if(i>=100){
iprint("failed init: reg(0xA): %4.4ux, should be 0x3000\n", in16(PpPtr));
return -1;
}
}
iprint("8900: %4.4ux (selfst) %4.4ux (linest)\n", regIOr(SelfSt), regIOr(LineSt));
iprint("8900: %4.4ux %4.4ux\n", regIOr(Ern), regIOr(Pic));
if(regIOr(Ern) != 0x630e || (regIOr(Pic) & 0xe0ff) != 0)
return -1;
if(ether->ctlr == nil)
ether->ctlr = malloc(sizeof(Ctlr));
ctlr = ether->ctlr;
reg = regIOr(Pic);
ctlr->model = reg>>14;
ctlr->rev = (reg >> 8) & 0x1F;
ether->mbps = 10;
memset(ea, 0, Eaddrlen);
easet = memcmp(ea, ether->ea, Eaddrlen);
memset(buf, 0, sizeof(buf));
if(regIOr(SelfSt) & EepromPresent) {
if(eepromrd(buf, Edataoff, sizeof(buf)) >= 0){
for(i=0; i<3; i++){
ether->ea[2*i] = buf[i];
ether->ea[2*i+1] = buf[i] >> 8;
}
easet = 1;
}else
iprint("cs8900: can't read EEPROM\n");
}
if(!easet){
iprint("cs8900: ethernet address not configured\n");
return -1;
}
memmove(ea, ether->ea, Eaddrlen);
if(DORESET){
regIOw(SelfCtl, RESET);
delay(10);
i=in8(PpPtr); 	USED(i);
i=in8(PpPtr+1); USED(i);
i=in8(PpPtr); 	USED(i);
i=in8(PpPtr+1);	USED(i);
i=0;
for(;;) {
short st = regIOr(SelfSt);
if((st&SIBUSY) == 0 && st&INITD)
break;
if(i++ > 1000000)
panic("cs8900: initialisation failed");
}
}
if(MEMORY){
regIOw(Mba, MemBase & 0xffff);
regIOw(Mba+2, MemBase >> 16);
regIOw(BusCtl, MemoryE|UseSA);
}
reg = regr(LineCtl);
regw(LineCtl, reg&~Iface);
reg = regr(TestCtl);
if(ether->fullduplex)
regw(TestCtl, reg|FDX);
else
regw(TestCtl, reg&~FDX);
regw(BufCfg, Rdy4TxiE|TxUnderruniE);
regw(TxCfg, TxOKiE|AnycolliE|LossofCRSiE|Coll16iE);
regw(RxCfg, RxOKiE|CRCerroriE|RuntiE|ExtradataiE);
regw(RxCtl, RxOKA|IndividualA|BroadcastA);
for(i=0; i<Eaddrlen; i+=2)
regw(IndAddr+i, ea[i] | (ea[i+1] << 8));
regw(Intr, 0);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->arg = ether;
ether->promiscuous = promiscuous;
ether->itype = BusGPIOrising;
return 0;
}
void
ether8900link(void)
{
addethercard("CS8900",  reset);
}