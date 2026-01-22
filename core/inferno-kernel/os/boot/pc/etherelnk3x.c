#include "u.h"
#include "lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "etherif.h"
enum {
IDport = 0x0110,
};
enum {
Command = 0x000E,
IntStatus = 0x000E,
};
enum {
GlobalReset = 0x0000,
SelectRegisterWindow = 0x0001,
EnableDcConverter = 0x0002,
RxDisable = 0x0003,
RxEnable = 0x0004,
RxReset = 0x0005,
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
resetMask = 0x00FF,
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
busMasterInProgress = 0x0800,
commandInProgress = 0x1000,
interruptMask = 0x01FE,
};
#define COMMAND(port, cmd, a) outs((port)+Command, ((cmd)<<11)|(a))
#define STATUS(port) ins((port)+IntStatus)
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
EepromReadRegister = 0x0080,
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
};
enum {
Wfifo = 0x0003,
InternalConfig = 0x0000,
OtherInt = 0x0004,
RomControl = 0x0006,
MacControl = 0x0006,
ResetOptions = 0x0008,
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
txOverrun = 0x0400,
rxUnderrun = 0x2000,
receiving = 0x8000,
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
TxAvalableThresh = 0x0002,
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
typedef struct {
int txthreshold;
} Ctlr;
static void
attach(Ether* ether)
{
int port, x;
port = ether->port;
x = receiveBroadcast|receiveIndividual;
COMMAND(port, SetRxFilter, x);
x = interruptMask|interruptLatch;
COMMAND(port, SetIndicationEnable, x);
COMMAND(port, SetInterruptEnable, x);
COMMAND(port, RxEnable, 0);
COMMAND(port, TxEnable, 0);
}
static void
transmit(Ether* ether)
{
int port, len;
RingBuf *tb;
port = ether->port;
for(tb = &ether->tb[ether->ti]; tb->owner == Interface; tb = &ether->tb[ether->ti]){
len = ROUNDUP(tb->len, 4);
if(len+4 <= ins(port+TxFree)){
outl(port+Fifo, tb->len);
outsl(port+Fifo, tb->pkt, len/4);
tb->owner = Host;
ether->ti = NEXT(ether->ti, ether->ntb);
}
else{
COMMAND(port, SetTxAvailableThresh, len);
break;
}
}
}
static void
receive(Ether* ether)
{
int len, port, rxstatus;
RingBuf *rb;
port = ether->port;
while(((rxstatus = ins(port+RxStatus)) & rxIncomplete) == 0){
if((rxstatus & rxError) == 0){
rb = &ether->rb[ether->ri];
if(rb->owner == Interface){
len = (rxstatus & rxBytes9);
rb->len = len;
insl(port+Fifo, rb->pkt, HOWMANY(len, 4));
rb->owner = Host;
ether->ri = NEXT(ether->ri, ether->nrb);
}
}
COMMAND(port, RxDiscard, 0);
while(STATUS(port) & commandInProgress)
;
}
}
static void
statistics(Ether* ether)
{
int i, port, w;
port = ether->port;
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wop);
COMMAND(port, SelectRegisterWindow, Wstatistics);
for(i = 0; i < 0x0A; i++)
inb(port+i);
ins(port+BytesRcvdOk);
ins(port+BytesXmittedOk);
COMMAND(port, SelectRegisterWindow, w);
}
static void
interrupt(Ureg*, void* arg)
{
Ether *ether;
int port, status, txstatus, w, x;
Ctlr *ctlr;
ether = arg;
port = ether->port;
ctlr = ether->ctlr;
w = (STATUS(port)>>13) & 0x07;
COMMAND(port, SelectRegisterWindow, Wop);
for(;;){
COMMAND(port, AcknowledgeInterrupt, interruptLatch);
status = STATUS(port);
if((status & interruptMask) == 0)
break;
if(status & hostError){
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+FifoDiagnostic);
COMMAND(port, SelectRegisterWindow, Wop);
print("elnk3#%d: status 0x%uX, diag 0x%uX\n",
ether->ctlrno, status, x);
if(x & txOverrun){
COMMAND(port, TxReset, 0);
COMMAND(port, TxEnable, 0);
}
if(x & rxUnderrun){
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, RxEnable, 0);
}
status &= ~hostError;
}
if(status & (transferInt|rxComplete)){
receive(ether);
status &= ~(transferInt|rxComplete);
}
if(status & txComplete){
txstatus = 0;
do{
if(x = inb(port+TxStatus))
outb(port+TxStatus, 0);
txstatus |= x;
}while(STATUS(port) & txComplete);
if(txstatus & txUnderrun){
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
while(ins(port+MediaStatus) & txInProg)
;
COMMAND(port, SelectRegisterWindow, Wop);
if(ctlr->txthreshold < ETHERMAXTU)
ctlr->txthreshold += ETHERMINTU;
}
if(txstatus & (txJabber|txUnderrun)){
COMMAND(port, TxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SetTxStartThresh, ctlr->txthreshold);
}
COMMAND(port, TxEnable, 0);
status &= ~txComplete;
status |= txAvailable;
}
if(status & txAvailable){
COMMAND(port, AcknowledgeInterrupt, txAvailable);
transmit(ether);
status &= ~txAvailable;
}
if(status & updateStats){
statistics(ether);
status &= ~updateStats;
}
if(status & interruptMask)
panic("elnk3#%d: interrupt mask 0x%uX\n", ether->ctlrno, status);
}
COMMAND(port, SelectRegisterWindow, w);
}
typedef struct Adapter {
int port;
int irq;
int tbdf;
} Adapter;
static Block* adapter;
static void
tcmadapter(int port, int irq, int tbdf)
{
Block *bp;
Adapter *ap;
bp = allocb(sizeof(Adapter));
ap = (Adapter*)bp->rp;
ap->port = port;
ap->irq = irq;
ap->tbdf = tbdf;
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
#ifdef notjustpcmcia
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
COMMAND(port, TxReset, 0);
COMMAND(port, RxReset, 0);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, BUSUNKNOWN);
}
}
static void
tcm5XXeisa(void)
{
ushort x;
int irq, port, slot;
if(strncmp((char*)(KZERO|0xFFFD9), "EISA", 4))
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
COMMAND(port, TxReset, 0);
COMMAND(port, RxReset, 0);
COMMAND(port, AcknowledgeInterrupt, 0xFF);
irq = (ins(port+ResourceConfig)>>12) & 0x0F;
tcmadapter(port, irq, BUSUNKNOWN);
}
}
static void
tcm59Xpci(void)
{
Pcidev *p;
int irq, port;
p = nil;
while(p = pcimatch(p, 0x10B7, 0)){
port = p->mem[0].bar & ~0x01;
irq = p->intl;
COMMAND(port, GlobalReset, 0);
while(STATUS(port) & commandInProgress)
;
tcmadapter(port, irq, p->tbdf);
}
}
#endif
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
if(!cistrcmp(ether->type, tcmpcmcia[i]))
return ether->port;
}
return 0;
}
static int
autoselect(int port, int rxstatus9)
{
int media, x;
media = auiAvailable|coaxAvailable|base10TAvailable;
if(rxstatus9 == 0){
COMMAND(port, SelectRegisterWindow, Wfifo);
media = ins(port+ResetOptions);
}
if(media & miiConnector)
return xcvrMii;
if(media & baseTXAvailable){
COMMAND(port, SelectRegisterWindow, Wfifo);
x = inl(port+InternalConfig) & ~xcvrMask;
x |= xcvr100BaseTX;
outl(port+InternalConfig, x);
COMMAND(port, TxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~(dcConverterEnabled|jabberGuardEnable);
outs(port+MediaStatus, linkBeatEnable|x);
delay(10);
{ int i, v;
for(i = 0; i < 10000; i++){
v = ins(port+MediaStatus);
if(v & linkBeatDetect){
print("count %d v %uX\n", i, v);
return xcvr100BaseTX;
}
delay(1);
}
print("count %d v %uX\n", i, ins(port+MediaStatus));
}
if(ins(port+MediaStatus) & linkBeatDetect)
return xcvr100BaseTX;
outs(port+MediaStatus, x);
}
if(media & base10TAvailable){
if(rxstatus9 == 0){
COMMAND(port, SelectRegisterWindow, Wfifo);
x = inl(port+InternalConfig) & ~xcvrMask;
x |= xcvr10BaseT;
outl(port+InternalConfig, x);
}
else{
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+AddressConfig) & ~xcvrMask9;
x |= (xcvr10BaseT>>20)<<14;
outs(port+AddressConfig, x);
}
COMMAND(port, TxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
x = ins(port+MediaStatus) & ~dcConverterEnabled;
outs(port+MediaStatus, linkBeatEnable|jabberGuardEnable|x);
delay(10);
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
int did, i, port, rxstatus9, x, xcvr;
Block *bp, **bpp;
Adapter *ap;
uchar ea[Eaddrlen];
Ctlr *ctlr;
#ifdef notjustpcmcia
static int scandone;
if(scandone == 0){
tcm59Xpci();
tcm5XXeisa();
tcm509isa();
scandone = 1;
}
#endif
port = 0;
bpp = &adapter;
for(bp = *bpp; bp; bp = bp->next){
ap = (Adapter*)bp->rp;
if(ether->port == 0 || ether->port == ap->port){
port = ap->port;
ether->irq = ap->irq;
ether->tbdf = ap->tbdf;
*bpp = bp->next;
freeb(bp);
break;
}
bpp = &bp->next;
}
if(port == 0 && (port = tcm5XXpcmcia(ether)) == 0)
return -1;
switch(did = eepromdata(port, 0x03)){
case 0x9000:
case 0x9001:
case 0x9050:
case 0x9051:
if(BUSTYPE(ether->tbdf) != BusPCI)
goto buggery;
goto vortex;
case 0x5900:
case 0x5920:
case 0x5950:
case 0x5951:
case 0x5952:
case 0x5970:
case 0x5971:
case 0x5972:
vortex:
COMMAND(port, SelectRegisterWindow, Wfifo);
xcvr = inl(port+InternalConfig) & (autoSelect|xcvrMask);
rxstatus9 = 0;
break;
buggery:
default:
COMMAND(port, SelectRegisterWindow, Wsetup);
x = ins(port+AddressConfig);
xcvr = ((x & xcvrMask9)>>14)<<20;
if(x & autoSelect9)
xcvr |= autoSelect;
rxstatus9 = 1;
break;
}
USED(did);
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
if(xcvr & autoSelect)
xcvr = autoselect(port, rxstatus9);
switch(xcvr){
case xcvrMii:
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
while(inb(port+TxStatus))
outb(port+TxStatus, 0);
ether->ctlr = malloc(sizeof(Ctlr));
ctlr = ether->ctlr;
memset(ctlr, 0, sizeof(Ctlr));
ctlr->txthreshold = ETHERMINTU;
COMMAND(port, SetTxStartThresh, ETHERMINTU);
COMMAND(port, SetRxEarlyThresh, ETHERMAXTU);
ether->port = port;
ether->attach = attach;
ether->transmit = transmit;
ether->interrupt = interrupt;
return 0;
}