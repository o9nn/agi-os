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
CommandR = 0x000E,
IntStatusR = 0x000E,
};
enum {
GlobalReset = 0x0000,
SelectRegisterWindow = 0x0001,
RxReset = 0x0005,
TxReset = 0x000B,
AcknowledgeInterrupt = 0x000D,
};
enum {
commandInProgress = 0x1000,
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
EepromBusy = 0x8000,
};
enum {
Wop = 0x0001,
};
enum {
Wfifo = 0x0003,
InternalConfig = 0x0000,
xcvr10BaseT = 0x00000000,
xcvr10Base2 = 0x00300000,
};
enum {
Wdiagnostic = 0x0004,
MediaStatus = 0x000A,
linkBeatDetect = 0x0800,
};
extern int etherelnk3reset(Ether*);
static char *tcmpcmcia[] = {
"3C589",
"3C562",
"589E",
nil,
};
static int
configASIC(Ether* ether, int port, int xcvr)
{
int x;
COMMAND(port, SelectRegisterWindow, Wsetup);
outs(port+ConfigControl, Ena);
outs(port + ResourceConfig, 0x3F00);
x = ins(port+AddressConfig) & ~xcvrMask9;
x |= (xcvr>>20)<<14;
outs(port+AddressConfig, x);
COMMAND(port, TxReset, 0);
while(STATUS(port) & commandInProgress)
;
COMMAND(port, RxReset, 0);
while(STATUS(port) & commandInProgress)
;
return etherelnk3reset(ether);
}
static int
reset(Ether* ether)
{
int i, t, slot;
char *type;
int port;
enum { WantAny, Want10BT, Want10B2 };
int want;
uchar ea[6];
char *p;
if(ether->irq == 0)
ether->irq = 10;
if(ether->port == 0)
ether->port = 0x240;
port = ether->port;
if(ioalloc(port, 0x10, 0, "3C589") < 0)
return -1;
type = nil;
slot = -1;
for(i = 0; tcmpcmcia[i] != nil; i++){
type = tcmpcmcia[i];
if((slot = pcmspecial(type, ether)) >= 0)
break;
}
if(slot < 0){
iofree(port);
return -1;
}
memset(ea, 0, sizeof ea);
if(memcmp(ea, ether->ea, 6) == 0 && strcmp(type, "3C562") == 0) {
if(pcmcistuple(slot, 0x88, -1, ea, 6) == 6) {
for(i = 0; i < 6; i += 2){
t = ea[i];
ea[i] = ea[i+1];
ea[i+1] = t;
}
memmove(ether->ea, ea, 6);
}
}
want = WantAny;
for(i = 0; i < ether->nopt; i++){
if(cistrncmp(ether->opt[i], "media=", 6) != 0)
continue;
p = ether->opt[i]+6;
if(cistrcmp(p, "10base2") == 0)
want = Want10B2;
else if(cistrcmp(p, "10baseT") == 0)
want = Want10BT;
}
if(want==WantAny || want==Want10BT){
if(configASIC(ether, port, xcvr10BaseT) < 0){
pcmspecialclose(slot);
iofree(port);
return -1;
}
delay(100);
COMMAND(port, SelectRegisterWindow, Wdiagnostic);
if((ins(port+MediaStatus)&linkBeatDetect) || want==Want10BT){
COMMAND(port, SelectRegisterWindow, Wop);
print("#l%d: xcvr10BaseT %s\n", ether->ctlrno, type);
return 0;
}
}
if(want==WantAny || want==Want10B2){
COMMAND(port, GlobalReset, 0);
if(configASIC(ether, port, xcvr10Base2) < 0){
pcmspecialclose(slot);
iofree(port);
return -1;
}
print("#l%d: xcvr10Base2 %s\n", ether->ctlrno, type);
return 0;
}
return -1;
}
void
ether589link(void)
{
addethercard("3C589", reset);
addethercard("3C562", reset);
addethercard("589E", reset);
}