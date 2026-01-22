#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
#define XCVRDEBUG if(0)print
enum {
IDport = 0x0110,
};
enum {
CommandR = 0x000E,
IntStatusR = 0x000E,
};
enum {
GlobalReset = 0x0000,
SelectRegisterWindow = 0x0001,
EnableDcConverter = 0x0002,
RxDisable = 0x0003,
RxEnable = 0x0004,
RxReset = 0x0005,
Stall = 0x0006,
TxDone = 0x0007,
RxDiscard = 0x0008,
TxEnable = 0x0009,
TxDisable = 0x000A,
TxReset = 0x000B,
RequestInterrupt = 0x000C,
AcknowledgeInterrupt = 0x000D,
SetInterruptEnable = 0x000E,
SetIndicationEnable = 0x000F,
SetRxFilter = 0x0010,
SetRxEarlyThresh = 0x0011,
SetTxAvailableThresh = 0x0012,
SetTxStartThresh = 0x0013,
StartDma = 0x0014,
StatisticsEnable = 0x0015,
StatisticsDisable = 0x0016,
DisableDcConverter = 0x0017,
SetTxReclaimThresh = 0x0018,
PowerUp = 0x001B,
PowerDownFull = 0x001C,
PowerAuto = 0x001D,
};
enum {
tpAuiReset = 0x0001,
endecReset = 0x0002,
networkReset = 0x0004,
fifoReset = 0x0008,
aismReset = 0x0010,
hostReset = 0x0020,
dmaReset = 0x0040,
vcoReset = 0x0080,
updnReset = 0x0100,
resetMask = 0x01FF,
};
enum {
upStall = 0x0000,
upUnStall = 0x0001,
dnStall = 0x0002,
dnUnStall = 0x0003,
};
enum {
receiveIndividual = 0x0001,
receiveMulticast = 0x0002,
receiveBroadcast = 0x0004,
receiveAllFrames = 0x0008,
};
enum {
Upload = 0x0000,
Download = 0x0001,
};
enum {
interruptLatch = 0x0001,
hostError = 0x0002,
txComplete = 0x0004,
txAvailable = 0x0008,
rxComplete = 0x0010,
rxEarly = 0x0020,
intRequested = 0x0040,
updateStats = 0x0080,
transferInt = 0x0100,
dnComplete = 0x0200,
upComplete = 0x0400,
busMasterInProgress = 0x0800,
commandInProgress = 0x1000,
interruptMask = 0x07FE,
};
#define COMMAND(port, cmd, a) outs((port)+CommandR, ((cmd)<<11)|(a))
#define STATUS(port) ins((port)+IntStatusR)
enum {
Wsetup = 0x0000,
ManufacturerID = 0x0000,
ProductID = 0x0002,
ConfigControl = 0x0004,
AddressConfig = 0x0006,
ResourceConfig = 0x0008,
EepromCommand = 0x000A,
EepromData = 0x000C,
autoSelect9 = 0x0080,
xcvrMask9 = 0xC000,
Ena = 0x0001,
base10TAvailable9 = 0x0200,
coaxAvailable9 = 0x1000,
auiAvailable9 = 0x2000,
_EepromReadRegister = 0x0080,
_EepromRead8bRegister = 0x0230,
EepromBusy = 0x8000,
};
static int EepromReadRegister = _EepromReadRegister;
#define EEPROMCMD(port, cmd, a) outs((port)+EepromCommand, (cmd)|(a))
#define EEPROMBUSY(port) (ins((port)+EepromCommand) & EepromBusy)
#define EEPROMDATA(port) ins((port)+EepromData)
enum {
Wop = 0x0001,
Fifo = 0x0000,
RxError = 0x0004,
RxStatus = 0x0008,
Timer = 0x000A,
TxStatus = 0x000B,
TxFree = 0x000C,
rxOverrun = 0x0001,
runtFrame = 0x0002,
alignmentError = 0x0004,
crcError = 0x0008,
oversizedFrame = 0x0010,
dribbleBits = 0x0080,
rxBytes = 0x1FFF,
rxBytes9 = 0x07FF,
rxError9 = 0x3800,
rxOverrun9 = 0x0000,
oversizedFrame9 = 0x0800,
dribbleBits9 = 0x1000,
runtFrame9 = 0x1800,
alignmentError9 = 0x2000,
crcError9 = 0x2800,
rxError = 0x4000,
rxIncomplete = 0x8000,
txStatusOverflow = 0x0004,
maxCollisions = 0x0008,
txUnderrun = 0x0010,
txJabber = 0x0020,
interruptRequested = 0x0040,
txStatusComplete = 0x0080,
};
enum {
Wstation = 0x0002,
ResetOp905B = 0x000C,
};
enum {
Wfifo = 0x0003,
InternalConfig = 0x0000,
OtherInt = 0x0004,
RomControl = 0x0006,
MacControl = 0x0006,
ResetOptions = 0x0008,
MediaOptions = 0x0008,
RxFree = 0x000A,
disableBadSsdDetect = 0x00000100,
ramLocation = 0x00000200,
ramPartition5to3 = 0x00000000,
ramPartition3to1 = 0x00010000,
ramPartition1to1 = 0x00020000,
ramPartition3to5 = 0x00030000,
ramPartitionMask = 0x00030000,
xcvr10BaseT = 0x00000000,
xcvrAui = 0x00100000,
xcvr10Base2 = 0x00300000,
xcvr100BaseTX = 0x00400000,
xcvr100BaseFX = 0x00500000,
xcvrMii = 0x00600000,
xcvrMask = 0x00700000,
autoSelect = 0x01000000,
deferExtendEnable = 0x0001,
deferTimerSelect = 0x001E,
fullDuplexEnable = 0x0020,
allowLargePackets = 0x0040,
extendAfterCollision = 0x0080,
flowControlEnable = 0x0100,
vltEnable = 0x0200,
baseT4Available = 0x0001,
baseTXAvailable = 0x0002,
baseFXAvailable = 0x0004,
base10TAvailable = 0x0008,
coaxAvailable = 0x0010,
auiAvailable = 0x0020,
miiConnector = 0x0040,
};
enum {
Wdiagnostic = 0x0004,
VcoDiagnostic = 0x0002,
FifoDiagnostic = 0x0004,
NetworkDiagnostic = 0x0006,
PhysicalMgmt = 0x0008,
MediaStatus = 0x000A,
BadSSD = 0x000C,
UpperBytesOk = 0x000D,
txOverrun = 0x0400,
rxUnderrun = 0x2000,
receiving = 0x8000,
mgmtClk = 0x0001,
mgmtData = 0x0002,
mgmtDir = 0x0004,
cat5LinkTestDefeat = 0x8000,
dataRate100 = 0x0002,
crcStripDisable = 0x0004,
enableSqeStats = 0x0008,
collisionDetect = 0x0010,
carrierSense = 0x0020,
jabberGuardEnable = 0x0040,
linkBeatEnable = 0x0080,
jabberDetect = 0x0200,
polarityReversed = 0x0400,
linkBeatDetect = 0x0800,
txInProg = 0x1000,
dcConverterEnabled = 0x4000,
auiDisable = 0x8000,
};
enum {
Wstate = 0x0005,
TxStartThresh = 0x0000,
TxAvailableThresh = 0x0002,
RxEarlyThresh = 0x0006,
RxFilter = 0x0008,
InterruptEnable = 0x000A,
IndicationEnable = 0x000C,
};
enum {
Wstatistics = 0x0006,
CarrierLost = 0x0000,
SqeErrors = 0x0001,
MultipleColls = 0x0002,
SingleCollFrames = 0x0003,
LateCollisions = 0x0004,
RxOverruns = 0x0005,
FramesXmittedOk = 0x0006,
FramesRcvdOk = 0x0007,
FramesDeferred = 0x0008,
UpperFramesOk = 0x0009,
BytesRcvdOk = 0x000A,
BytesXmittedOk = 0x000C,
};
enum {
Wmaster = 0x0007,
MasterAddress = 0x0000,
MasterLen = 0x0006,
MasterStatus = 0x000C,
masterAbort = 0x0001,
targetAbort = 0x0002,
targetRetry = 0x0004,
targetDisc = 0x0008,
masterDownload = 0x1000,
masterUpload = 0x4000,
masterInProgress = 0x8000,
masterMask = 0xD00F,
};
enum {
Timer905 = 0x001A,
TxStatus905 = 0x001B,
PktStatus = 0x0020,
DnListPtr = 0x0024,
FragAddr = 0x0028,
FragLen = 0x002C,
ListOffset = 0x002E,
TxFreeThresh = 0x002F,
UpPktStatus = 0x0030,
FreeTimer = 0x0034,
UpListPtr = 0x0038,
fragLast = 0x00000001,
dnCmplReq = 0x00000002,
dnStalled = 0x00000004,
upCompleteX = 0x00000008,
dnCompleteX = 0x00000010,
upRxEarlyEnable = 0x00000020,
armCountdown = 0x00000040,
dnInProg = 0x00000080,
counterSpeed = 0x00000010,
countdownMode = 0x00000020,
upPktLenMask = 0x00001FFF,
upStalled = 0x00002000,
upError = 0x00004000,
upPktComplete = 0x00008000,
upOverrun = 0x00010000,
upRuntFrame = 0x00020000,
upAlignmentError = 0x00040000,
upCRCError = 0x00080000,
upOversizedFrame = 0x00100000,
upDribbleBits = 0x00800000,
upOverflow = 0x01000000,
dnIndicate = 0x80000000,
updnLastFrag = 0x80000000,
Nup = 32,
Ndn = 64,
};
typedef struct Pd Pd;
typedef struct Pd {
ulong np;
ulong control;
ulong addr;
ulong len;
Pd* next;
void *vaddr;
} Pd;
typedef struct {
Lock wlock;
int attached;
int busmaster;
Block* rbp;
Block* txbp;
int txthreshold;
int txbusy;
int nup;
void* upbase;
Pd* upr;
Pd* uphead;
int ndn;
void* dnbase;
Pd* dnr;
Pd* dnhead;
Pd* dntail;
int dnq;
long interrupts;
long timer[2];
long stats[BytesRcvdOk+3];
int upqmax;
int upqmaxhw;
ulong upinterrupts;
ulong upqueued;
ulong upstalls;
int dnqmax;
int dnqmaxhw;
ulong dninterrupts;
ulong dnqueued;
int xcvr;
int rxstatus9;
int rxearly;
int ts;
int upenabled;
int dnenabled;
} Ctlr;
static void
init905(Ctlr* ctlr)
{
Pd *pd, *prev;
uchar *vaddr;
ctlr->upbase = malloc((ctlr->nup+1)*sizeof(Pd));
ctlr->upr = (Pd*)ROUNDUP((ulong)ctlr->upbase, 8);
vaddr = ialloc((ctlr->nup+1)*ROUNDUP(sizeof(Etherpkt)+4, 8), 8);
prev = ctlr->upr;
for(pd = &ctlr->upr[ctlr->nup-1]; pd >= ctlr->upr; pd--){
pd->np = PADDR(&prev->np);
pd->control = 0;
pd->vaddr = vaddr;
pd->addr = PADDR(vaddr);
vaddr += ROUNDUP(sizeof(Etherpkt)+4, 8);
pd->len = updnLastFrag|sizeof(Etherpkt);
pd->next = prev;
prev = pd;
}
ctlr->uphead = ctlr->upr;
ctlr->dnbase = malloc((ctlr->ndn+1)*sizeof(Pd));
ctlr->dnr = (Pd*)ROUNDUP((ulong)ctlr->dnbase, 8);
vaddr = ialloc((ctlr->ndn+1)*ROUNDUP(sizeof(Etherpkt)+4, 8), 8);
prev = ctlr->dnr;
for(pd = &ctlr->dnr[ctlr->ndn-1]; pd >= ctlr->dnr; pd--){
pd->next = prev;
pd->vaddr = vaddr;
pd->addr = PADDR(vaddr);
vaddr += ROUNDUP(sizeof(Etherpkt)+4, 8);
prev = pd;
}
ctlr->dnhead = ctlr->dnr;
ctlr->dntail = ctlr->dnr;
ctlr->dnq = 0;
}
static Block*
rbpalloc(Block* (*f)(int))
{
Block *bp;
ulong addr;
if(bp = f(ROUNDUP(sizeof(Etherpkt), 4) + 31)){
addr = (ulong)bp->base;
addr = ROUNDUP(addr, 32);
bp->rp = (uchar*)addr;
}
return bp;
}
static uchar*
startdma(Ether* ether, ulong address)
{
int port, status, w;
uchar *wp;
port = ether->port;
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wmaster);
wp = KADDR(inl(port+MasterAddress));
status = ins(port+MasterStatus);
if(status & (masterInProgress|targetAbort|masterAbort))
print("#l%d: BM status 0x%uX\n", ether->ctlrno, status);
outs(port+MasterStatus, masterMask);
outl(port+MasterAddress, address);
outs(port+MasterLen, sizeof(Etherpkt));
COMMAND(port, StartDma, Upload);
COMMAND(port, SelectRegisterWindow, w);
return wp;
}
static void
intrack3c575(ulong *cbfns)
{
cbfns[1] = 0x8000;
}
static void
attach(Ether* ether)
{
int port, x;
Ctlr *ctlr;
ctlr = ether->ctlr;
ilock(&ctlr->wlock);
if(ctlr->attached){
iunlock(&ctlr->wlock);
return;
}
port = ether->port;
COMMAND(port, SetRxFilter, receiveIndividual|receiveBroadcast);
x = interruptMask;
if(ctlr->busmaster == 1)
x &= ~(rxEarly|rxComplete);
else{
if(ctlr->dnenabled)
x &= ~transferInt;
if(ctlr->upenabled)
x &= ~(rxEarly|rxComplete);
}
COMMAND(port, SetIndicationEnable, x);
COMMAND(port, SetInterruptEnable, x);
COMMAND(port, RxEnable, 0);
COMMAND(port, TxEnable, 0);
if (ether->mem)
intrack3c575(KADDR(ether->mem));
if(ctlr->busmaster == 1)
startdma(ether, PADDR(ctlr->rbp->rp));
else{
if(ctlr->upenabled)
outl(port+UpListPtr, PADDR(&ctlr->uphead->np));
}
ctlr->attached = 1;
iunlock(&ctlr->wlock);
}
static void
statistics(Ether* ether)
{
int port, i, u, w;
Ctlr *ctlr;
port = ether->port;
ctlr = ether->ctlr;
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wstatistics);
STATUS(port);
for(i = 0; i < UpperFramesOk; i++)
ctlr->stats[i] += inb(port+i) & 0xFF;
u = inb(port+UpperFramesOk) & 0xFF;
ctlr->stats[FramesXmittedOk] += (u & 0x30)<<4;
ctlr->stats[FramesRcvdOk] += (u & 0x03)<<8;
ctlr->stats[BytesRcvdOk] += ins(port+BytesRcvdOk) & 0xFFFF;
ctlr->stats[BytesRcvdOk+1] += ins(port+BytesXmittedOk) & 0xFFFF;
switch(ctlr->xcvr){
case xcvrMii:
case xcvr100BaseTX:
case xcvr100BaseFX:
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
STATUS(port);
ctlr->stats[BytesRcvdOk+2] += inb(port+BadSSD);
break;
}
COMMAND(port, SelectRegisterWindow, w);
}
static void
txstart(Ether* ether)
{
int port, len;
Ctlr *ctlr;
RingBuf *tb;
port = ether->port;
ctlr = ether->ctlr;
for(tb = &ether->tb[ether->ti]; tb->owner == Interface; tb = &ether->tb[ether->ti]){
len = ROUNDUP(tb->len, 4);
if(len+4 <= ins(port+TxFree)){
outl(port+Fifo, tb->len);
outsl(port+Fifo, tb->pkt, len/4);
tb->owner = Host;
ether->ti = NEXT(ether->ti, ether->ntb);
}
else{
if(ctlr->txbusy == 0){
ctlr->txbusy = 1;
COMMAND(port, SetTxAvailableThresh, len>>ctlr->ts);
}
break;
}
}
}
static void
txstart905(Ether* ether)
{
Ctlr *ctlr;
int port, stalled, timeo;
RingBuf *tb;
Pd *pd;
ctlr = ether->ctlr;
port = ether->port;
pd = ctlr->dntail;
while(ctlr->dnq){
if(PADDR(&pd->np) == inl(port+DnListPtr))
break;
ctlr->dnq--;
pd = pd->next;
}
ctlr->dntail = pd;
stalled = 0;
while(ctlr->dnq < (ctlr->ndn-1)){
tb = &ether->tb[ether->ti];
if(tb->owner != Interface)
break;
pd = ctlr->dnhead->next;
pd->np = 0;
pd->control = dnIndicate|tb->len;
memmove(pd->vaddr, tb->pkt, tb->len);
pd->len = updnLastFrag|tb->len;
tb->owner = Host;
ether->ti = NEXT(ether->ti, ether->ntb);
if(stalled == 0 && ctlr->dnq && inl(port+DnListPtr)){
COMMAND(port, Stall, dnStall);
for(timeo = 100; (STATUS(port) & commandInProgress) && timeo; timeo--)
;
if(timeo == 0)
print("#l%d: dnstall %d\n", ether->ctlrno, timeo);
stalled = 1;
}
coherence();
ctlr->dnhead->np = PADDR(&pd->np);
ctlr->dnhead->control &= ~dnIndicate;
ctlr->dnhead = pd;
if(ctlr->dnq == 0)
ctlr->dntail = pd;
ctlr->dnq++;
ctlr->dnqueued++;
}
if(ctlr->dnq > ctlr->dnqmax)
ctlr->dnqmax = ctlr->dnq;
if(inl(port+DnListPtr) == 0 && ctlr->dnq)
outl(port+DnListPtr, PADDR(&ctlr->dnhead->np));
if(stalled)
COMMAND(port, Stall, dnUnStall);
}
static void
transmit(Ether* ether)
{
Ctlr *ctlr;
int port, w;
port = ether->port;
ctlr = ether->ctlr;
ilock(&ctlr->wlock);
if(ctlr->dnenabled)
txstart905(ether);
else{
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wop);
txstart(ether);
COMMAND(port, SelectRegisterWindow, w);
}
iunlock(&ctlr->wlock);
}
static void
receive905(Ether* ether)
{
Ctlr *ctlr;
int len, port, q;
Pd *pd;
RingBuf *rb;
ctlr = ether->ctlr;
port = ether->port;
if(inl(port+UpPktStatus) & upStalled)
ctlr->upstalls++;
q = 0;
for(pd = ctlr->uphead; pd->control & upPktComplete; pd = pd->next){
if(!(pd->control & upError)){
rb = &ether->rb[ether->ri];
if (rb->owner == Interface) {
len = pd->control & rxBytes;
rb->len = len;
memmove(rb->pkt, pd->vaddr, len);
rb->owner = Host;
ether->ri = NEXT(ether->ri, ether->nrb);
}
}
pd->control = 0;
COMMAND(port, Stall, upUnStall);
q++;
}
ctlr->uphead = pd;
ctlr->upqueued += q;
if(q > ctlr->upqmax)
ctlr->upqmax = q;
}
static void
receive(Ether* ether)
{
int len, port, rxstatus;
RingBuf *rb;
Ctlr *ctlr;
port = ether->port;
ctlr = ether->ctlr;
while(((rxstatus = ins(port+RxStatus)) & rxIncomplete) == 0){
if(ctlr->busmaster == 1 && (STATUS(port) & busMasterInProgress))
break;
if((rxstatus & rxError) == 0){
rb = &ether->rb[ether->ri];
if(rb->owner == Interface){
len = (rxstatus & rxBytes9);
rb->len = len;
insl(port+Fifo, rb->pkt, HOWMANY(len, 4));
rb->owner = Host;
ether->ri = NEXT(ether->ri, ether->nrb);
}else
if(debug) print("toss...");
}
else
if(debug) print("error...");
COMMAND(port, RxDiscard, 0);
while(STATUS(port) & commandInProgress)
;
}
}
static void
interrupt(Ureg*, void* arg)
{
Ether *ether;
int port, status, s, txstatus, w, x;
Ctlr *ctlr;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
ilock(&ctlr->wlock);
status = STATUS(port);
if(!(status & (interruptMask|interruptLatch))){
iunlock(&ctlr->wlock);
return;
}
w = (status>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wop);
ctlr->interrupts++;
if(ctlr->busmaster == 2)
ctlr->timer[0] += inb(port+Timer905) & 0xFF;
else
ctlr->timer[0] += inb(port+Timer) & 0xFF;
do{
if(status & hostError){
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+FifoDiagnostic);
COMMAND(port, SelectRegisterWindow, Wop);
print("#l%d: status 0x%uX, diag 0x%uX\n",
ether->ctlrno, status, x);
if(x & txOverrun){
if(ctlr->busmaster == 0)
COMMAND(port, TxReset, 0);
else
COMMAND(port, TxReset, (updnReset|dmaReset));
COMMAND(port, TxEnable, 0);
}
if(x & rxUnderrun){
COMMAND(port, SelectRegisterWindow, Wstate);
s = (port+RxFilter) & 0x000F;
COMMAND(port, SelectRegisterWindow, Wop);
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SetRxFilter, s);
COMMAND(port, SetRxEarlyThresh, ctlr->rxearly>>ctlr->ts);
COMMAND(port, RxEnable, 0);
}
status &= ~hostError;
}
if(status & (transferInt|rxComplete)){
receive(ether);
status &= ~(transferInt|rxComplete);
}
if(status & (upComplete)){
COMMAND(port, AcknowledgeInterrupt, upComplete);
receive905(ether);
status &= ~upComplete;
ctlr->upinterrupts++;
}
if(status & txComplete){
if(ctlr->busmaster == 2)
txstatus = port+TxStatus905;
else
txstatus = port+TxStatus;
s = 0;
do{
if(x = inb(txstatus))
outb(txstatus, 0);
s |= x;
}while(STATUS(port) & txComplete);
if(s & txUnderrun){
if(ctlr->dnenabled){
while(inl(port+PktStatus) & dnInProg)
;
}
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
while(ins(port+MediaStatus) & txInProg)
;
COMMAND(port, SelectRegisterWindow, Wop);
if(ctlr->txthreshold < ETHERMAXTU)
ctlr->txthreshold += ETHERMINTU;
}
if(s & (txJabber|txUnderrun|maxCollisions)){
if(ctlr->busmaster == 0)
COMMAND(port, TxReset, 0);
else
COMMAND(port, TxReset, (updnReset|dmaReset));
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SetTxStartThresh, ctlr->txthreshold>>ctlr->ts);
if(ctlr->busmaster == 2)
outl(port+TxFreeThresh, HOWMANY(ETHERMAXTU, 256));
if(ctlr->dnenabled)
status |= dnComplete;
}
if(s & ~(txStatusComplete|maxCollisions))
print("#l%d: txstatus 0x%uX, threshold %d\n",
ether->ctlrno, s, ctlr->txthreshold);
COMMAND(port, TxEnable, 0);
status &= ~txComplete;
status |= txAvailable;
}
if(status & txAvailable){
COMMAND(port, AcknowledgeInterrupt, txAvailable);
ctlr->txbusy = 0;
txstart(ether);
status &= ~txAvailable;
}
if(status & dnComplete){
COMMAND(port, AcknowledgeInterrupt, dnComplete);
txstart905(ether);
status &= ~dnComplete;
ctlr->dninterrupts++;
}
if(status & updateStats){
statistics(ether);
status &= ~updateStats;
}
if(status & rxEarly){
COMMAND(port, AcknowledgeInterrupt, rxEarly);
status &= ~rxEarly;
}
if(status & interruptMask)
panic("#l%d: interrupt mask 0x%uX\n", ether->ctlrno, status);
COMMAND(port, AcknowledgeInterrupt, interruptLatch);
if (ether->mem)
intrack3c575((ulong *)KADDR(ether->mem));
}while((status = STATUS(port)) & (interruptMask|interruptLatch));
if(ctlr->busmaster == 2)
ctlr->timer[1] += inb(port+Timer905) & 0xFF;
else
ctlr->timer[1] += inb(port+Timer) & 0xFF;
COMMAND(port, SelectRegisterWindow, w);
iunlock(&ctlr->wlock);
}
static void
txrxreset(int port)
{
COMMAND(port, TxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
}
typedef struct Adapter {
int port;
int irq;
int tbdf;
ulong cbfns;
} Adapter;
static Block* adapter;
static void
tcmadapter(int port, int irq, int tbdf, ulong cbfns)
{
Block *bp;
Adapter *ap;
bp = allocb(sizeof(Adapter));
ap = (Adapter*)bp->rp;
ap->port = port;
ap->irq = irq;
ap->tbdf = tbdf;
ap->cbfns = cbfns;
bp->next = adapter;
adapter = bp;
}
static void
idseq(void)
{
int i;
uchar al;
static int reset, untag;
if(reset == 0){
outb(IDport, 0);
outb(IDport, 0);
outb(IDport, 0xC0);
delay(20);
reset = 1;
}
outb(IDport, 0);
outb(IDport, 0);
for(al = 0xFF, i = 0; i < 255; i++){
outb(IDport, al);
if(al & 0x80){
al <<= 1;
al ^= 0xCF;
}
else
al <<= 1;
}
if(untag == 0){
outb(IDport, 0xD0);
untag = 1;
}
}
static ulong
activate(void)
{
int i;
ushort x, acr;
idseq();
outb(IDport, 0x87);
delay(20);
for(x = 0, i = 0; i < 16; i++){
delay(20);
x <<= 1;
x |= inb(IDport) & 0x01;
}
if(x != 0x6D50)
return 0;
outb(IDport, 0x88);
for(acr = 0, i = 0; i < 16; i++){
delay(20);
acr <<= 1;
acr |= inb(IDport) & 0x01;
}
return (acr & 0x1F)*0x10 + 0x200;
}
static void
tcm509isa(void)
{
int irq, port;
while(port = activate()){
outb(IDport, 0xD1);
if(port == 0x3F0)
continue;
outb(IDport, 0xFF);
delay(20);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port+ConfigControl, Ena);
txrxreset(port);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, BUSUNKNOWN, 0);
}
}
static void
tcm5XXeisa(void)
{
ushort x;
int irq, port, slot;
if(strncmp((char*)KADDR(0xFFFD9), "EISA", 4))
return;
for(slot = 1; slot < MaxEISA; slot++){
port = slot*0x1000;
if(ins(port+0xC80+ManufacturerID) != 0x6D50)
continue;
x = ins(port+0xC80+ProductID);
if((x & 0xF0FF) != 0x9050 && (x & 0xFF00) != 0x5900)
continue;
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port+ConfigControl, Ena);
txrxreset(port);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, BUSUNKNOWN, 0);
}
}
static void
tcm59Xpci(Ether *ether)
{
Pcidev *p;
int irq, port;
ulong bar;
p = nil;
while(p = pcimatch(p, 0x10B7, 0)){
if (p->did == 0x5157) {
EepromReadRegister = _EepromRead8bRegister;
bar = pcicfgr32(p, PciBAR2);
print("ether#%d: CardBus functions at %.8luX\n", ether->ctlrno, bar & ~KZERO);
}
else
bar = 0;
if(!(p->mem[0].bar & 0x01))
continue;
port = p->mem[0].bar & ~0x01;
irq = p->intl;
COMMAND(port, GlobalReset, 0);
while(STATUS(port) & commandInProgress)
;
tcmadapter(port, irq, p->tbdf, bar);
pcisetbme(p);
}
}
static char* tcmpcmcia[] = {
"3C589",
"3C562",
"589E",
nil,
};
static int
tcm5XXpcmcia(Ether* ether)
{
int i;
for(i = 0; tcmpcmcia[i] != nil; i++){
if(!cistrcmp(ether->type, tcmpcmcia[i])){
return ether->port;
}
}
return 0;
}
static void
setxcvr(int port, int xcvr, int is9)
{
int x;
if(is9){
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+AddressConfig) & ~xcvrMask9;
x |= (xcvr>>20)<<14;
outs(port+AddressConfig, x);
}
else{
COMMAND(port, SelectRegisterWindow, Wfifo);
x = inl(port+InternalConfig) & ~xcvrMask;
x |= xcvr;
outl(port+InternalConfig, x);
}
txrxreset(port);
}
static void
setfullduplex(int port)
{
int x;
COMMAND(port, SelectRegisterWindow, Wfifo);
x = ins(port+MacControl);
outs(port+MacControl, fullDuplexEnable|x);
txrxreset(port);
}
static int
miimdi(int port, int n)
{
int data, i;
data = 0;
for(i = n-1; i >= 0; i--){
if(ins(port) & mgmtData)
data |= (1<<i);
microdelay(1);
outs(port, mgmtClk);
microdelay(1);
outs(port, 0);
microdelay(1);
}
return data;
}
static void
miimdo(int port, int bits, int n)
{
int i, mdo;
for(i = n-1; i >= 0; i--){
if(bits & (1<<i))
mdo = mgmtDir|mgmtData;
else
mdo = mgmtDir;
outs(port, mdo);
microdelay(1);
outs(port, mdo|mgmtClk);
microdelay(1);
outs(port, mdo);
microdelay(1);
}
}
static int
miir(int port, int phyad, int regad)
{
int data, w;
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
port += PhysicalMgmt;
miimdo(port, 0xFFFFFFFF, 32);
miimdo(port, 0x1800|(phyad<<5)|regad, 14);
data = miimdi(port, 18);
port -= PhysicalMgmt;
COMMAND(port, SelectRegisterWindow, w);
if(data & 0x10000)
return -1;
return data & 0xFFFF;
}
static void
scanphy(int port)
{
int i, x;
for(i = 0; i < 32; i++){
if((x = miir(port, i, 2)) == -1 || x == 0)
continue;
x <<= 6;
x |= miir(port, i, 3)>>10;
XCVRDEBUG("phy%d: oui %uX reg1 %uX\n", i, x, miir(port, i, 1));
USED(x);
}
}
#ifdef notdef
static struct xxx {
int available;
int next;
} xxx[8] = {
{ base10TAvailable, 1, },
{ auiAvailable, 3, },
{ 0, -1, },
{ coaxAvailable, -1, },
{ baseTXAvailable, 5, },
{ baseFXAvailable, -1, },
{ miiConnector, -1, },
{ 0, -1, },
};
#endif
static struct {
char *name;
int avail;
int xcvr;
} media[] = {
"10BaseT", base10TAvailable, xcvr10BaseT,
"10Base2", coaxAvailable, xcvr10Base2,
"100BaseTX", baseTXAvailable, xcvr100BaseTX,
"100BaseFX", baseFXAvailable, xcvr100BaseFX,
"aui", auiAvailable, xcvrAui,
"mii", miiConnector, xcvrMii
};
static int
autoselect(int port, int xcvr, int is9)
{
int media, x;
USED(xcvr);
if(is9){
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+ConfigControl);
media = 0;
if(x & base10TAvailable9)
media |= base10TAvailable;
if(x & coaxAvailable9)
media |= coaxAvailable;
if(x & auiAvailable9)
media |= auiAvailable;
}
else{
COMMAND(port, SelectRegisterWindow, Wfifo);
media = ins(port+ResetOptions);
}
XCVRDEBUG("autoselect: media %uX\n", media);
if(media & miiConnector)
return xcvrMii;
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
XCVRDEBUG("autoselect: media status %uX\n", ins(port+MediaStatus));
if(media & baseTXAvailable){
setxcvr(port, xcvr100BaseTX, is9);
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~(dcConverterEnabled|jabberGuardEnable);
outs(port+MediaStatus, linkBeatEnable|x);
delay(10);
if(ins(port+MediaStatus) & linkBeatDetect)
return xcvr100BaseTX;
outs(port+MediaStatus, x);
}
if(media & base10TAvailable){
setxcvr(port, xcvr10BaseT, is9);
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~dcConverterEnabled;
outs(port+MediaStatus, linkBeatEnable|jabberGuardEnable|x);
delay(100);
XCVRDEBUG("autoselect: 10BaseT media status %uX\n", ins(port+MediaStatus));
if(ins(port+MediaStatus) & linkBeatDetect)
return xcvr10BaseT;
outs(port+MediaStatus, x);
}
return autoSelect;
}
static int
eepromdata(int port, int offset)
{
COMMAND(port, SelectRegisterWindow, Wsetup);
while(EEPROMBUSY(port))
;
EEPROMCMD(port, EepromReadRegister, offset);
while(EEPROMBUSY(port))
;
return EEPROMDATA(port);
}
int
elnk3reset(Ether* ether)
{
int anar, anlpar, phyaddr, phystat, timeo, xcvr;
int busmaster, did, i, j, port, rxearly, rxstatus9, x;
Block *bp, **bpp;
Adapter *ap;
uchar ea[Eaddrlen];
Ctlr *ctlr;
static int scandone;
char *p;
if(scandone == 0){
tcm59Xpci(ether);
tcm5XXeisa();
tcm509isa();
scandone = 1;
}
port = 0;
bpp = &adapter;
for(bp = *bpp; bp; bp = bp->next){
ap = (Adapter*)bp->rp;
if(ether->port == 0 || ether->port == ap->port){
port = ap->port;
ether->irq = ap->irq;
ether->tbdf = ap->tbdf;
ether->mem = ap->cbfns;
*bpp = bp->next;
freeb(bp);
break;
}
bpp = &bp->next;
}
if(port == 0 && (port = tcm5XXpcmcia(ether)) == 0)
return -1;
switch(did = eepromdata(port, 0x03)){
case 0x5157:
case 0x4500:
case 0x6056:
case 0x7646:
case 0x9055:
case 0x9200:
case 0x9000:
case 0x9001:
case 0x9005:
case 0x9050:
case 0x9051:
if(BUSTYPE(ether->tbdf) != BusPCI)
goto buggery;
busmaster = 2;
goto vortex;
case 0x5900:
case 0x5920:
case 0x5950:
case 0x5951:
case 0x5952:
case 0x5970:
case 0x5971:
case 0x5972:
busmaster = 1;
vortex:
COMMAND(port, SelectRegisterWindow, Wfifo);
xcvr = inl(port+InternalConfig) & (autoSelect|xcvrMask);
rxearly = 8188;
rxstatus9 = 0;
break;
buggery:
default:
busmaster = 0;
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+AddressConfig);
xcvr = ((x & xcvrMask9)>>14)<<20;
if(x & autoSelect9)
xcvr |= autoSelect;
rxearly = 2044;
rxstatus9 = 1;
break;
}
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
for(i = 0; i < Eaddrlen/2; i++){
x = eepromdata(port, i);
ether->ea[2*i] = x>>8;
ether->ea[2*i+1] = x;
}
}
COMMAND(port, SelectRegisterWindow, Wstation);
for(i = 0; i < Eaddrlen; i++)
outb(port+i, ether->ea[i]);
XCVRDEBUG("reset: xcvr %uX\n", xcvr);
for(i = 0; i < ether->nopt; i++){
if(cistrncmp(ether->opt[i], "media=", 6) != 0)
continue;
p = ether->opt[i]+6;
for(j = 0; j < nelem(media); j++)
if(cistrcmp(p, media[j].name) == 0)
xcvr = media[j].xcvr;
}
switch(did){
default:
if(xcvr & autoSelect)
xcvr = autoselect(port, xcvr, rxstatus9);
break;
case 0x4500:
case 0x5157:
case 0x6056:
case 0x7646:
case 0x9055:
case 0x9200:
xcvr = xcvrMii;
txrxreset(port);
XCVRDEBUG("905[BC] reset ops 0x%uX\n", ins(port+ResetOp905B));
if (did == 0x5157) {
ushort reset_opts;
COMMAND(port, SelectRegisterWindow, Wstation);
reset_opts = ins(port + ResetOp905B);
reset_opts |= 0x0010;
outs(port + ResetOp905B, reset_opts);
}
break;
}
XCVRDEBUG("autoselect returns: xcvr %uX, did 0x%uX\n", xcvr, did);
switch(xcvr){
case xcvrMii:
phyaddr = (did == 0x5157)? 0: 24;
for(i = 0; i < 7; i++)
XCVRDEBUG(" %2.2uX", miir(port, phyaddr, i));
XCVRDEBUG("\n");
for(timeo = 0; timeo < 30; timeo++){
phystat = miir(port, phyaddr, 0x01);
if(phystat & 0x20)
break;
XCVRDEBUG(" %2.2uX", phystat);
delay(100);
}
XCVRDEBUG(" %2.2uX", miir(port, phyaddr, 0x01));
XCVRDEBUG("\n");
anar = miir(port, phyaddr, 0x04);
anlpar = miir(port, phyaddr, 0x05) & 0x03E0;
anar &= anlpar;
miir(port, phyaddr, 0x00);
XCVRDEBUG("mii an: %uX anlp: %uX r0:%uX r1:%uX\n",
anar, anlpar, miir(port, phyaddr, 0x00),
miir(port, phyaddr, 0x01));
for(i = 0; i < ether->nopt; i++){
if(cistrcmp(ether->opt[i], "fullduplex") == 0)
anar |= 0x0100;
else if(cistrcmp(ether->opt[i], "100BASE-TXFD") == 0)
anar |= 0x0100;
else if(cistrcmp(ether->opt[i], "force100") == 0)
anar |= 0x0080;
}
XCVRDEBUG("mii anar: %uX\n", anar);
if(anar & 0x0100){
setfullduplex(port);
}
else if(anar & 0x0200){
}
else if(anar & 0x0080){
;
}
else if(anar & 0x0040)
setfullduplex(port);
else{
;
}
break;
case xcvr100BaseTX:
case xcvr100BaseFX:
COMMAND(port, SelectRegisterWindow, Wfifo);
x = inl(port+InternalConfig) & ~ramPartitionMask;
outl(port+InternalConfig, x|ramPartition1to1);
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~(dcConverterEnabled|jabberGuardEnable);
x |= linkBeatEnable;
outs(port+MediaStatus, x);
break;
case xcvr10BaseT:
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~dcConverterEnabled;
x |= linkBeatEnable|jabberGuardEnable;
outs(port+MediaStatus, x);
if((did & 0xFF00) == 0x5900)
busmaster = 0;
break;
case xcvr10Base2:
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~(linkBeatEnable|jabberGuardEnable);
outs(port+MediaStatus, x);
COMMAND(port, EnableDcConverter, 0);
delay(1);
break;
}
COMMAND(port, SelectRegisterWindow, Wop);
if(busmaster == 2)
x = port+TxStatus905;
else
x = port+TxStatus;
while(inb(x))
outb(x, 0);
ether->ctlr = malloc(sizeof(Ctlr));
ctlr = ether->ctlr;
ilock(&ctlr->wlock);
ctlr->xcvr = xcvr;
statistics(ether);
memset(ctlr->stats, 0, sizeof(ctlr->stats));
ctlr->busmaster = busmaster;
ctlr->xcvr = xcvr;
ctlr->rxstatus9 = rxstatus9;
ctlr->rxearly = rxearly;
if(rxearly >= 2048)
ctlr->ts = 2;
COMMAND(port, StatisticsEnable, 0);
if (ctlr->busmaster == 2) {
ctlr->dnenabled = 1;
ctlr->upenabled = 1;
x = eepromdata(port, 0x0F);
if(!(x & 0x01))
outl(port+PktStatus, upRxEarlyEnable);
ctlr->nup = Nup;
ctlr->ndn = Ndn;
init905(ctlr);
outl(port+TxFreeThresh, HOWMANY(ETHERMAXTU, 256));
}
ctlr->txthreshold = ETHERMAXTU/2;
COMMAND(port, SetTxStartThresh, ctlr->txthreshold>>ctlr->ts);
COMMAND(port, SetRxEarlyThresh, rxearly>>ctlr->ts);
iunlock(&ctlr->wlock);
ether->port = port;
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
return 0;
}