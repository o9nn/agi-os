#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"
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
EepromReadRegister = 0x0080,
EepromReadOffRegister = 0x00B0,
EepromRead8bRegister = 0x0230,
EepromBusy = 0x8000,
};
#define EEPROMCMD(port, cmd, a) outs((port)+EepromCommand, (cmd)|(a))
#define EEPROMBUSY(port) (ins((port)+EepromCommand) & EepromBusy)
#define EEPROMDATA(port) ins((port)+EepromData)
enum {
Wop = 0x0001,
Fifo = 0x0000,
RxError = 0x0004,
RxStatus = 0x0008,
TIMER = 0x000A,
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
deferTIMERSelect = 0x001E,
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
TIMER905 = 0x001A,
TxStatus905 = 0x001B,
PktStatus = 0x0020,
DnListPtr = 0x0024,
FragAddr = 0x0028,
FragLen = 0x002C,
ListOffset = 0x002E,
TxFreeThresh = 0x002F,
UpPktStatus = 0x0030,
FreeTIMER = 0x0034,
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
Block* bp;
} Pd;
typedef struct Ctlr Ctlr;
typedef struct Ctlr {
int port;
Pcidev* pcidev;
int irq;
Ctlr* next;
int active;
int did;
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
long bogusinterrupts;
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
int eepromcmd;
int rxstatus9;
int rxearly;
int ts;
int upenabled;
int dnenabled;
ulong cbfnpa;
ulong* cbfn;
} Ctlr;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
static void
init905(Ctlr* ctlr)
{
Block *bp;
Pd *pd, *prev;
ctlr->upbase = malloc((ctlr->nup+1)*sizeof(Pd));
ctlr->upr = (Pd*)ROUNDUP((ulong)ctlr->upbase, 8);
prev = ctlr->upr;
for(pd = &ctlr->upr[ctlr->nup-1]; pd >= ctlr->upr; pd--){
pd->np = PADDR(&prev->np);
pd->control = 0;
bp = iallocb(sizeof(Etherpkt));
if(bp == nil)
panic("can't allocate ethernet receive ring");
pd->addr = PADDR(bp->rp);
pd->len = updnLastFrag|sizeof(Etherpkt);
pd->next = prev;
prev = pd;
pd->bp = bp;
}
ctlr->uphead = ctlr->upr;
ctlr->dnbase = malloc((ctlr->ndn+1)*sizeof(Pd));
ctlr->dnr = (Pd*)ROUNDUP((ulong)ctlr->dnbase, 8);
prev = ctlr->dnr;
for(pd = &ctlr->dnr[ctlr->ndn-1]; pd >= ctlr->dnr; pd--){
pd->next = prev;
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
promiscuous(void* arg, int on)
{
int filter, port;
Ether *ether;
ether = (Ether*)arg;
port = ether->port;
filter = receiveBroadcast|receiveIndividual;
if(ether->nmaddr)
filter |= receiveMulticast;
if(on)
filter |= receiveAllFrames;
COMMAND(port, SetRxFilter, filter);
}
static void
multicast(void* arg, uchar *addr, int on)
{
int filter, port;
Ether *ether;
USED(addr, on);
ether = (Ether*)arg;
port = ether->port;
filter = receiveBroadcast|receiveIndividual;
if(ether->nmaddr)
filter |= receiveMulticast;
if(ether->prom)
filter |= receiveAllFrames;
COMMAND(port, SetRxFilter, filter);
}
static void
intrackcb(ulong *cbfn)
{
cbfn[1] = 0x8000;
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
promiscuous(ether, ether->prom);
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
if(ctlr->cbfn != nil)
intrackcb(ctlr->cbfn);
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
Block *bp;
port = ether->port;
ctlr = ether->ctlr;
for(;;){
if(ctlr->txbp){
bp = ctlr->txbp;
ctlr->txbp = 0;
}
else{
bp = qget(ether->oq);
if(bp == nil)
break;
}
len = ROUNDUP(BLEN(bp), 4);
if(len+4 <= ins(port+TxFree)){
outl(port+Fifo, BLEN(bp));
outsl(port+Fifo, bp->rp, len/4);
freeb(bp);
ether->outpackets++;
}
else{
ctlr->txbp = bp;
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
Block *bp;
Pd *pd;
ctlr = ether->ctlr;
port = ether->port;
pd = ctlr->dntail;
while(ctlr->dnq){
if(PADDR(&pd->np) == inl(port+DnListPtr))
break;
if(pd->bp){
freeb(pd->bp);
pd->bp = nil;
}
ctlr->dnq--;
pd = pd->next;
}
ctlr->dntail = pd;
stalled = 0;
while(ctlr->dnq < (ctlr->ndn-1)){
bp = qget(ether->oq);
if(bp == nil)
break;
pd = ctlr->dnhead->next;
pd->np = 0;
pd->control = dnIndicate|BLEN(bp);
pd->addr = PADDR(bp->rp);
pd->len = updnLastFrag|BLEN(bp);
pd->bp = bp;
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
Block *bp;
ctlr = ether->ctlr;
port = ether->port;
if(inl(port+UpPktStatus) & upStalled)
ctlr->upstalls++;
q = 0;
for(pd = ctlr->uphead; pd->control & upPktComplete; pd = pd->next){
if(pd->control & upError){
if(pd->control & upOverrun)
ether->overflows++;
if(pd->control & (upOversizedFrame|upRuntFrame))
ether->buffs++;
if(pd->control & upAlignmentError)
ether->frames++;
if(pd->control & upCRCError)
ether->crcs++;
}
else if(bp = iallocb(sizeof(Etherpkt)+4)){
len = pd->control & rxBytes;
pd->bp->wp = pd->bp->rp+len;
etheriq(ether, pd->bp, 1);
pd->bp = bp;
pd->addr = PADDR(bp->rp);
coherence();
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
int len, port, rxerror, rxstatus;
Ctlr *ctlr;
Block *bp;
port = ether->port;
ctlr = ether->ctlr;
while(((rxstatus = ins(port+RxStatus)) & rxIncomplete) == 0){
if(ctlr->busmaster == 1 && (STATUS(port) & busMasterInProgress))
break;
if(rxstatus & rxError){
if(ctlr->rxstatus9){
switch(rxstatus & rxError9){
case rxOverrun9:
ether->overflows++;
break;
case oversizedFrame9:
case runtFrame9:
ether->buffs++;
break;
case alignmentError9:
ether->frames++;
break;
case crcError9:
ether->crcs++;
break;
}
}
else{
rxerror = inb(port+RxError);
if(rxerror & rxOverrun)
ether->overflows++;
if(rxerror & (oversizedFrame|runtFrame))
ether->buffs++;
if(rxerror & alignmentError)
ether->frames++;
if(rxerror & crcError)
ether->crcs++;
}
}
if((rxstatus & rxError) || (bp = rbpalloc(iallocb)) == 0){
COMMAND(port, RxDiscard, 0);
while(STATUS(port) & commandInProgress)
;
if(ctlr->busmaster == 1)
startdma(ether, PADDR(ctlr->rbp->rp));
continue;
}
if(ctlr->busmaster == 0 || ctlr->busmaster == 2){
len = (rxstatus & rxBytes9);
ctlr->rbp->wp = ctlr->rbp->rp + len;
insl(port+Fifo, ctlr->rbp->rp, HOWMANY(len, 4));
}
COMMAND(port, RxDiscard, 0);
while(STATUS(port) & commandInProgress)
;
if(ctlr->busmaster == 1)
ctlr->rbp->wp = startdma(ether, PADDR(bp->rp));
etheriq(ether, ctlr->rbp, 1);
ctlr->rbp = bp;
}
}
static int
ejectable(int did)
{
switch (did) {
case 0x5157:
return 1;
default:
return 0;
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
ctlr->bogusinterrupts++;
iunlock(&ctlr->wlock);
return;
}
w = (status>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wop);
ctlr->interrupts++;
if(ctlr->busmaster == 2)
ctlr->timer[0] += inb(port+TIMER905) & 0xFF;
else
ctlr->timer[0] += inb(port+TIMER) & 0xFF;
do{
if(status & hostError){
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+FifoDiagnostic);
COMMAND(port, SelectRegisterWindow, Wop);
if (status == 0xFFFF && x == 0xFFFF && ejectable(ctlr->did)) {
print("#l%d: Card ejected?\n", ether->ctlrno);
iunlock(&ctlr->wlock);
return;
}
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
ether->oerrs++;
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
if(ctlr->cbfn != nil)
intrackcb(ctlr->cbfn);
}while((status = STATUS(port)) & (interruptMask|interruptLatch));
if(ctlr->busmaster == 2)
ctlr->timer[1] += inb(port+TIMER905) & 0xFF;
else
ctlr->timer[1] += inb(port+TIMER) & 0xFF;
COMMAND(port, SelectRegisterWindow, w);
iunlock(&ctlr->wlock);
}
static long
ifstat(Ether* ether, void* a, long n, ulong offset)
{
char *p;
int len;
Ctlr *ctlr;
if(n == 0)
return 0;
ctlr = ether->ctlr;
ilock(&ctlr->wlock);
statistics(ether);
iunlock(&ctlr->wlock);
p = malloc(READSTR);
len = snprint(p, READSTR, "interrupts: %lud\n", ctlr->interrupts);
len += snprint(p+len, READSTR-len, "bogusinterrupts: %lud\n", ctlr->bogusinterrupts);
len += snprint(p+len, READSTR-len, "timer: %lud %lud\n",
ctlr->timer[0], ctlr->timer[1]);
len += snprint(p+len, READSTR-len, "carrierlost: %lud\n",
ctlr->stats[CarrierLost]);
len += snprint(p+len, READSTR-len, "sqeerrors: %lud\n",
ctlr->stats[SqeErrors]);
len += snprint(p+len, READSTR-len, "multiplecolls: %lud\n",
ctlr->stats[MultipleColls]);
len += snprint(p+len, READSTR-len, "singlecollframes: %lud\n",
ctlr->stats[SingleCollFrames]);
len += snprint(p+len, READSTR-len, "latecollisions: %lud\n",
ctlr->stats[LateCollisions]);
len += snprint(p+len, READSTR-len, "rxoverruns: %lud\n",
ctlr->stats[RxOverruns]);
len += snprint(p+len, READSTR-len, "framesxmittedok: %lud\n",
ctlr->stats[FramesXmittedOk]);
len += snprint(p+len, READSTR-len, "framesrcvdok: %lud\n",
ctlr->stats[FramesRcvdOk]);
len += snprint(p+len, READSTR-len, "framesdeferred: %lud\n",
ctlr->stats[FramesDeferred]);
len += snprint(p+len, READSTR-len, "bytesrcvdok: %lud\n",
ctlr->stats[BytesRcvdOk]);
len += snprint(p+len, READSTR-len, "bytesxmittedok: %lud\n",
ctlr->stats[BytesRcvdOk+1]);
if(ctlr->upenabled){
if(ctlr->upqmax > ctlr->upqmaxhw)
ctlr->upqmaxhw = ctlr->upqmax;
len += snprint(p+len, READSTR-len, "up: q %lud i %lud m %d h %d s %lud\n",
ctlr->upqueued, ctlr->upinterrupts,
ctlr->upqmax, ctlr->upqmaxhw, ctlr->upstalls);
ctlr->upqmax = 0;
}
if(ctlr->dnenabled){
if(ctlr->dnqmax > ctlr->dnqmaxhw)
ctlr->dnqmaxhw = ctlr->dnqmax;
len += snprint(p+len, READSTR-len, "dn: q %lud i %lud m %d h %d\n",
ctlr->dnqueued, ctlr->dninterrupts, ctlr->dnqmax, ctlr->dnqmaxhw);
ctlr->dnqmax = 0;
}
snprint(p+len, READSTR-len, "badssd: %lud\n", ctlr->stats[BytesRcvdOk+2]);
n = readstr(offset, a, n, p);
free(p);
return n;
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
static Ctlr*
tcmadapter(int port, int irq, Pcidev* pcidev)
{
Ctlr *ctlr;
ctlr = malloc(sizeof(Ctlr));
ctlr->port = port;
ctlr->irq = irq;
ctlr->pcidev = pcidev;
ctlr->eepromcmd = EepromReadRegister;
if(ctlrhead != nil)
ctlrtail->next = ctlr;
else
ctlrhead = ctlr;
ctlrtail = ctlr;
return ctlr;
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
if(ioalloc(port, 0x10, 0, "tcm509isa") < 0){
print("tcm509isa: port 0x%uX in use\n", port);
continue;
}
outb(IDport, 0xD1);
if(port == 0x3F0){
iofree(port);
continue;
}
outb(IDport, 0xFF);
delay(20);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port+ConfigControl, Ena);
txrxreset(port);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, nil);
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
if(ioalloc(port, 0x1000, 0, "tcm5XXeisa") < 0){
print("tcm5XXeisa: port 0x%uX in use\n", port);
continue;
}
if(ins(port+0xC80+ManufacturerID) != 0x6D50){
iofree(port);
continue;
}
x = ins(port+0xC80+ProductID);
if((x & 0xF0FF) != 0x9050 && (x & 0xFF00) != 0x5900){
iofree(port);
continue;
}
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port+ConfigControl, Ena);
txrxreset(port);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, nil);
}
}
static void
tcm59Xpci(void)
{
Pcidev *p;
Ctlr *ctlr;
int irq, port;
p = nil;
while(p = pcimatch(p, 0x10B7, 0)){
if(p->ccrb != 0x02 || p->ccru != 0)
continue;
if(!(p->mem[0].bar & 0x01))
continue;
port = p->mem[0].bar & ~0x01;
if((port = ioalloc((port == 0)? -1: port, p->mem[0].size,
0, "tcm59Xpci")) < 0){
print("tcm59Xpci: port 0x%uX in use\n", port);
continue;
}
irq = p->intl;
txrxreset(port);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
ctlr = tcmadapter(port, irq, p);
switch(p->did){
default:
break;
case 0x5157:
ctlr->eepromcmd = EepromRead8bRegister;
ctlr->cbfnpa = p->mem[2].bar&~0x0F;
ctlr->cbfn = vmap(p->mem[2].bar&~0x0F, p->mem[2].size);
break;
case 0x6056:
ctlr->eepromcmd = EepromReadOffRegister;
ctlr->cbfnpa = p->mem[2].bar&~0x0F;
ctlr->cbfn = vmap(p->mem[2].bar&~0x0F, p->mem[2].size);
break;
}
pcisetbme(p);
}
}
static char* tcmpcmcia[] = {
"3C589",
"3C562",
"589E",
nil,
};
static Ctlr*
tcm5XXpcmcia(Ether* ether)
{
int i;
Ctlr *ctlr;
if(ether->type == nil)
return nil;
for(i = 0; tcmpcmcia[i] != nil; i++){
if(cistrcmp(ether->type, tcmpcmcia[i]))
continue;
ctlr = tcmadapter(ether->port, ether->irq, nil);
ctlr->active = 1;
return ctlr;
}
return nil;
}
static void
setxcvr(Ctlr* ctlr, int xcvr)
{
int port, x;
port = ctlr->port;
if(ctlr->rxstatus9){
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
static int
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
return i;
}
return 24;
}
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
autoselect(Ctlr* ctlr)
{
int media, port, x;
port = ctlr->port;
if(ctlr->rxstatus9){
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
setxcvr(ctlr, xcvr100BaseTX);
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~(dcConverterEnabled|jabberGuardEnable);
outs(port+MediaStatus, linkBeatEnable|x);
delay(10);
if(ins(port+MediaStatus) & linkBeatDetect)
return xcvr100BaseTX;
outs(port+MediaStatus, x);
}
if(media & base10TAvailable){
setxcvr(ctlr, xcvr10BaseT);
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
eepromdata(Ctlr* ctlr, int offset)
{
int port;
port = ctlr->port;
COMMAND(port, SelectRegisterWindow, Wsetup);
while(EEPROMBUSY(port))
;
EEPROMCMD(port, ctlr->eepromcmd, offset);
while(EEPROMBUSY(port))
;
return EEPROMDATA(port);
}
static void
resetctlr(Ctlr *ctlr)
{
int x, port = ctlr->port;
txrxreset(port);
x = ins(port+ResetOp905B);
XCVRDEBUG("905[BC] reset ops 0x%uX\n", x);
x &= ~0x4010;
if(ctlr->did == 0x5157){
x |= 0x0010;
outs(port+ResetOp905B, x);
}
if(ctlr->did == 0x6056){
x |= 0x4000;
outs(port+ResetOp905B, x);
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port, 0x0800);
}
}
static void
shutdown(Ether *ether)
{
print("etherelnk3 shutting down\n");
resetctlr(ether->ctlr);
}
int
etherelnk3reset(Ether* ether)
{
char *p;
Ctlr *ctlr;
uchar ea[Eaddrlen];
static int scandone;
int anar, anlpar, i, j, phyaddr, phystat, port, timeo, x;
if(scandone == 0){
tcm59Xpci();
tcm5XXeisa();
tcm509isa();
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
if(ctlr == nil && (ctlr = tcm5XXpcmcia(ether)) == 0)
return -1;
ether->ctlr = ctlr;
port = ctlr->port;
ether->port = port;
ether->irq = ctlr->irq;
if(ctlr->pcidev != nil)
ether->tbdf = ctlr->pcidev->tbdf;
else
ether->tbdf = BUSUNKNOWN;
switch(ctlr->did = eepromdata(ctlr, 0x03)){
case 0x5157:
case 0x6056:
case 0x4500:
case 0x7646:
case 0x9055:
case 0x9200:
case 0x9201:
case 0x9805:
case 0x9000:
case 0x9001:
case 0x9005:
case 0x9050:
case 0x9051:
if(BUSTYPE(ether->tbdf) != BusPCI)
goto buggery;
ctlr->busmaster = 2;
goto vortex;
case 0x5900:
case 0x5920:
case 0x5950:
case 0x5951:
case 0x5952:
case 0x5970:
case 0x5971:
case 0x5972:
ctlr->busmaster = 1;
vortex:
COMMAND(port, SelectRegisterWindow, Wfifo);
ctlr->xcvr = inl(port+InternalConfig) & (autoSelect|xcvrMask);
ctlr->rxearly = 8188;
ctlr->rxstatus9 = 0;
break;
buggery:
default:
ctlr->busmaster = 0;
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+AddressConfig);
ctlr->xcvr = ((x & xcvrMask9)>>14)<<20;
if(x & autoSelect9)
ctlr->xcvr |= autoSelect;
ctlr->rxearly = 2044;
ctlr->rxstatus9 = 1;
break;
}
if(ctlr->rxearly >= 2048)
ctlr->ts = 2;
memset(ea, 0, Eaddrlen);
if(memcmp(ea, ether->ea, Eaddrlen) == 0){
for(i = 0; i < Eaddrlen/2; i++){
x = eepromdata(ctlr, i);
ether->ea[2*i] = x>>8;
ether->ea[2*i+1] = x;
}
}
COMMAND(port, SelectRegisterWindow, Wstation);
for(i = 0; i < Eaddrlen; i++)
outb(port+i, ether->ea[i]);
XCVRDEBUG("reset: xcvr %uX\n", ctlr->xcvr);
for(i = 0; i < ether->nopt; i++){
if(cistrncmp(ether->opt[i], "media=", 6) != 0)
continue;
p = ether->opt[i]+6;
for(j = 0; j < nelem(media); j++)
if(cistrcmp(p, media[j].name) == 0)
ctlr->xcvr = media[j].xcvr;
}
switch(ctlr->did){
default:
if(ctlr->xcvr & autoSelect)
ctlr->xcvr = autoselect(ctlr);
break;
case 0x5157:
case 0x6056:
case 0x4500:
case 0x7646:
case 0x9055:
case 0x9200:
case 0x9201:
case 0x9805:
ctlr->xcvr = xcvrMii;
resetctlr(ctlr);
break;
}
XCVRDEBUG("xcvr selected: %uX, did 0x%uX\n", ctlr->xcvr, ctlr->did);
switch(ctlr->xcvr){
case xcvrMii:
if(ctlr->did == 0x5157)
phyaddr = 0;
else if(ctlr->did == 0x6056)
phyaddr = scanphy(port);
else
phyaddr = 24;
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
ether->mbps = 100;
setfullduplex(port);
}
else if(anar & 0x0200){
}
else if(anar & 0x0080)
ether->mbps = 100;
else if(anar & 0x0040)
setfullduplex(port);
else{
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
if(x & dataRate100)
ether->mbps = 100;
break;
case xcvr10BaseT:
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~dcConverterEnabled;
x |= linkBeatEnable|jabberGuardEnable;
outs(port+MediaStatus, x);
if((ctlr->did & 0xFF00) == 0x5900)
ctlr->busmaster = 0;
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
if(ctlr->busmaster == 2)
x = port+TxStatus905;
else
x = port+TxStatus;
while(inb(x))
outb(x, 0);
ilock(&ctlr->wlock);
statistics(ether);
memset(ctlr->stats, 0, sizeof(ctlr->stats));
COMMAND(port, StatisticsEnable, 0);
switch(ctlr->busmaster){
case 2:
ctlr->dnenabled = 1;
ctlr->upenabled = 1;
x = eepromdata(ctlr, 0x0F);
if(!(x & 0x01))
outl(port+PktStatus, upRxEarlyEnable);
if(ctlr->upenabled || ctlr->dnenabled){
ctlr->nup = Nup;
ctlr->ndn = Ndn;
init905(ctlr);
}
else {
ctlr->rbp = rbpalloc(iallocb);
if(ctlr->rbp == nil)
panic("can't reset ethernet: out of memory");
}
outl(port+TxFreeThresh, HOWMANY(ETHERMAXTU, 256));
break;
default:
ctlr->rbp = rbpalloc(iallocb);
if(ctlr->rbp == nil)
panic("can't reset ethernet: out of memory");
break;
}
ctlr->txthreshold = ETHERMAXTU/2;
COMMAND(port, SetTxStartThresh, ctlr->txthreshold>>ctlr->ts);
COMMAND(port, SetRxEarlyThresh, ctlr->rxearly>>ctlr->ts);
iunlock(&ctlr->wlock);
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
ether->ifstat = ifstat;
ether->promiscuous = promiscuous;
ether->multicast = multicast;
ether->shutdown = shutdown;
ether->arg = ether;
return 0;
}
void
etherelnk3link(void)
{
addethercard("elnk3", etherelnk3reset);
addethercard("3C509", etherelnk3reset);
addethercard("3C575", etherelnk3reset);
}