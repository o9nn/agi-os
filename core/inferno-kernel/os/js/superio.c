#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
enum
{
SioFER =	0x0,
SioFAR =	0x1,
SioPTR =	0x2,
SioFCR =	0x3,
SioPCR =	0x4,
SioKRR =	0x5,
SioPMC =	0x6,
SioTUP =	0x7,
SioSID =	0x8,
SioASC =	0x9,
SioCS0CF0 =	0xA,
SioCS0CF1 =	0xB,
SioCS1CF0 =	0xC,
SioCS1CF1 = 	0xD,
PPTEnable = 	1<<0,
EnableUART1 = 	1<<1,
EnableUART2 = 	1<<2,
FDCEnable = 	1<<3,
FDC4 =	 	1<<4,
FDC2ndAddr = 	1<<5,
IDEEnable = 	1<<6,
IDE2ndAddr = 	1<<7,
PPTAddr =	3<<0,
UART1Addr = 	3<<2,
UART2Addr =	3<<4,
SelectCom3n4 = 	3<<6,
PWDN = 		1<<0,
ClkPWDN = 	1<<1,
PWDNSelect = 	1<<2,
IRQSelect = 	1<<3,
UART1Test = 	1<<4,
UART2Test = 	1<<5,
LockConfig = 	1<<6,
XtndPPTSelect =	1<<7,
MediaSense =	1<<0,
DatRateSelect =	1<<0,
IDENTSelect = 	1<<1,
PPTFloat = 	1<<3,
LogicalDrvXcg =	1<<4,
EnaZeroWait = 	1<<5,
EPPEnable =	1<<0,
EPPVersionSel =	1<<1,
ECPEnable = 	1<<2,
ECPClkFreeze = 	1<<3,
PPTIntPolar = 	1<<5,
PPTIntIOCtl = 	1<<6,
RTCRamMask =	1<<7,
KBCEnable =	1<<0,
KBCSpeedCtl = 	1<<1,
EnaProgAccess =	1<<2,
RTCEnable = 	1<<3,
RTCClkTst = 	1<<4,
RAMSEL = 	1<<5,
EnaChipSelect =	1<<6,
KBCClkSource =	1<<7,
IDETriStCtl =	1<<0,
FDCTriStCtl = 	1<<1,
UARTTriStCtl = 	1<<2,
SelectiveLock =	1<<5,
PPTriStEna = 	1<<6,
EPPToutIntEna =	1<<2,
IRQ5Select = 	1<<0,
DRATE0Select =	1<<0,
DRV2Select = 	1<<1,
DR23Select = 	1<<1,
EnhancedTDR = 	1<<2,
ECPCnfgABit3 = 	1<<5,
SystemOpMode0 =	1<<6,
SystemOpMode1 =	1<<7,
HA8 =		1<<0,
HA9 = 		1<<1,
HA10 = 		1<<2,
EnaCSWr = 	1<<4,
EnaCSRd =	1<<5,
CSAdrDcode =	1<<6,
CSSelectPin =	1<<7,
};
typedef struct SuperIO SuperIO;
struct SuperIO
{
ulong va;
uchar *index;
uchar *data;
uchar *mkctl;
uchar *mkdata;
};
static SuperIO sio;
static void printstatus(uchar status);
void
superioinit(ulong va, uchar *sindex, uchar *sdata, uchar *mkctl, uchar *mkdata)
{
sio.va = va;
sio.index = sindex;
sio.data = sdata;
sio.mkctl = mkctl;
sio.mkdata = mkdata;
}
ulong
superiova(void)
{
return sio.va;
}
enum
{
OBF =		1<<0,
IBF =		1<<1,
SysFlag =	1<<2,
LastWrWasCmd = 	1<<3,
KbdEnabled =	1<<4,
FromMouse = 	1<<5,
Timeout = 	1<<6,
ParityError = 	1<<7
};
uchar
superio_readctl(void)
{
return *sio.mkctl;
}
uchar
superio_readdata(void)
{
return *sio.mkdata;
}
void
superio_writectl(uchar val)
{
*sio.mkctl = val;
}
void
superio_writedata(uchar val)
{
*sio.mkdata = val;
}
static  void
printstatus(uchar status)
{
print("0x%2.2ux = <",status);
if(status & OBF) print("OBF|");
if(status & IBF) print("IBF|");
if(status & SysFlag) print("SysFlag|");
if(status & LastWrWasCmd) print("LastWrWasCmd|");
if(status & KbdEnabled) print("KbdEnabled|");
if(status & FromMouse) print("FromMouse|");
if(status & Timeout) print("Timeout|");
if(status & ParityError) print("ParityErr|");
print(">");
}
void
testit()
{
uchar status;
uchar val;
for(;;) {
status = *sio.mkctl;
if(status&OBF) {
printstatus(status);
val = *sio.mkdata;
print(", data = 0x%2.2ux\n",val);
}
}
}