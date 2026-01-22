#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"io.h"
enum {
L3DataSetupTime = 1,
L3DataHoldTime = 1,
L3ModeSetupTime = 1,
L3ModeHoldTime = 1,
L3ClockHighTime = 1,
L3ClockLowTime = 1,
L3HaltTime = 1,
};
static void
L3acquirepins(void)
{
GpioReg *g = GPIOREG;
int s;
s = splhi();
g->gpsr = (L3Mode | L3Clock | L3Data);
g->gpdr |=  (L3Mode | L3Clock | L3Data);
splx(s);
}
static void
L3releasepins(void)
{
GpioReg *g = GPIOREG;
int s;
s = splhi();
g->gpdr &= ~(L3Mode | L3Clock | L3Data);
splx(s);
}
void
L3init(void)
{
GpioReg *g = GPIOREG;
int s;
s = splhi();
g->gafr &= ~(L3Data | L3Clock | L3Mode);
splx(s);
L3releasepins();
}
static void
L3sendbyte(int data, int mode)
{
int i;
GpioReg *g = GPIOREG;
switch(mode) {
case 0:
g->gpcr = L3Mode;
break;
case 1:
break;
default:
g->gpcr = L3Mode;
microdelay(L3HaltTime);
g->gpsr = L3Mode;
break;
}
microdelay(L3ModeSetupTime);
for (i = 0; i < 8; i++){
microdelay(2);
g->gpcr = L3Clock;
if (data & (1<<i))
g->gpsr = L3Data;
else
g->gpcr = L3Data;
microdelay(L3ClockLowTime);
g->gpsr = L3Clock;
microdelay(L3ClockHighTime);
}
if (mode == 0)
g->gpsr = L3Mode;
microdelay(L3ModeHoldTime);
}
static int
L3getbyte(int mode)
{
int data = 0;
int i;
GpioReg *g = GPIOREG;
switch(mode) {
case 0:
break;
case 1:
break;
default:
g->gpcr = L3Mode;
microdelay(L3HaltTime);
g->gpsr = L3Mode;
break;
}
microdelay(L3ModeSetupTime);
for (i = 0; i < 8; i++){
g->gpcr = L3Clock;
microdelay(L3ClockLowTime);
if(g->gplr & L3Data)
data |= 1<<i;
g->gpsr = L3Clock;
microdelay(L3ClockHighTime);
}
microdelay(L3ModeHoldTime);
return data;
}
int
L3write(int addr, void *data, int len)
{
int mode = 0;
int bytes = len;
uchar *b;
L3acquirepins();
L3sendbyte(addr, mode++);
for(b = data; --len >= 0;)
L3sendbyte(*b++, mode++);
L3releasepins();
return bytes;
}
int
L3read(int addr, void *data, int len)
{
int mode = 0;
int bytes = len;
uchar *b;
int s;
L3acquirepins();
L3sendbyte(addr, mode++);
s = splhi();
GPIOREG->gpdr &= ~(L3Data);
splx(s);
for(b = data; --len >= 0;)
*b++ = L3getbyte(mode++);
L3releasepins();
return bytes;
}