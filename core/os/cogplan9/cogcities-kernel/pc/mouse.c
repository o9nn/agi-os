#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
#define	Image	IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
enum
{
Mouseother=	0,
Mouseserial=	1,
MousePS2=	2,
};
extern int mouseshifted;
static QLock mousectlqlock;
static int mousetype;
static int intellimouse;
static int packetsize;
static int resolution;
static int accelerated;
static int mousehwaccel;
static char mouseport[5];
enum
{
CMaccelerated,
CMhwaccel,
CMintellimouse,
CMlinear,
CMps2,
CMps2intellimouse,
CMres,
CMreset,
CMserial,
};
static Cmdtab mousectlmsg[] =
{
CMaccelerated,		"accelerated",		0,
CMhwaccel,		"hwaccel",		2,
CMintellimouse,		"intellimouse",		1,
CMlinear,		"linear",		1,
CMps2,			"ps2",			1,
CMps2intellimouse,	"ps2intellimouse",	1,
CMres,			"res",			0,
CMreset,		"reset",		1,
CMserial,		"serial",		0,
};
static void
ps2mouseputc(int c, int shift)
{
static short msg[4];
static int nb;
static uchar b[] = {0, 1, 4, 5, 2, 3, 6, 7, 0, 1, 2, 3, 2, 3, 6, 7 };
static ulong lasttick;
ulong m;
int buttons, dx, dy;
shift |= mouseshifted;
m = MACHP(0)->ticks;
if(TK2SEC(m - lasttick) > 2)
nb = 0;
lasttick = m;
if(nb==0 && (c&0xc8)!=0x08)
if(intellimouse && (c==0x00 || c==0x01 || c==0xFF)){
packetsize = 4;
return;
}
msg[nb] = c;
if(++nb == packetsize){
nb = 0;
if(msg[0] & 0x10)
msg[1] |= 0xFF00;
if(msg[0] & 0x20)
msg[2] |= 0xFF00;
buttons = b[(msg[0]&7) | (shift ? 8 : 0)];
if(intellimouse && packetsize==4){
if((msg[3]&0xc8) == 0x08){
packetsize = 3;
msg[0] = msg[3];
nb = 1;
}else{
if((msg[3] >> 3) & 1)
buttons |= 1<<3;
else if(msg[3] & 0x7)
buttons |= 1<<4;
}
}
dx = msg[1];
dy = -msg[2];
mousetrack(dx, dy, buttons, TK2MS(MACHP(0)->ticks));
}
return;
}
static void
ps2mouse(void)
{
if(mousetype == MousePS2)
return;
i8042auxenable(ps2mouseputc);
i8042auxcmd(0xEA);
i8042auxcmd(0xF4);
mousetype = MousePS2;
packetsize = 3;
mousehwaccel = 1;
}
static void
setaccelerated(int x)
{
accelerated = x;
if(mousehwaccel){
switch(mousetype){
case MousePS2:
i8042auxcmd(0xE7);
return;
}
}
mouseaccelerate(x);
}
static void
setlinear(void)
{
accelerated = 0;
if(mousehwaccel){
switch(mousetype){
case MousePS2:
i8042auxcmd(0xE6);
return;
}
}
mouseaccelerate(0);
}
static void
setres(int n)
{
resolution = n;
switch(mousetype){
case MousePS2:
i8042auxcmd(0xE8);
i8042auxcmd(n);
break;
}
}
static void
setintellimouse(void)
{
intellimouse = 1;
packetsize = 4;
switch(mousetype){
case MousePS2:
i8042auxcmd(0xF3);
i8042auxcmd(0xC8);
i8042auxcmd(0xF3);
i8042auxcmd(0x64);
i8042auxcmd(0xF3);
i8042auxcmd(0x50);
break;
case Mouseserial:
i8250setmouseputc(mouseport, m5mouseputc);
break;
}
}
static void
resetmouse(void)
{
packetsize = 3;
switch(mousetype){
case MousePS2:
i8042auxcmd(0xF6);
i8042auxcmd(0xEA);
i8042auxcmd(0xE8);
i8042auxcmd(3);
i8042auxcmd(0xF4);
break;
}
}
void
mousectl(Cmdbuf *cb)
{
Cmdtab *ct;
qlock(&mousectlqlock);
if(waserror()){
qunlock(&mousectlqlock);
nexterror();
}
ct = lookupcmd(cb, mousectlmsg, nelem(mousectlmsg));
switch(ct->index){
case CMaccelerated:
setaccelerated(cb->nf == 1 ? 1 : atoi(cb->f[1]));
break;
case CMintellimouse:
setintellimouse();
break;
case CMlinear:
setlinear();
break;
case CMps2:
intellimouse = 0;
ps2mouse();
break;
case CMps2intellimouse:
ps2mouse();
setintellimouse();
break;
case CMres:
if(cb->nf >= 2)
setres(atoi(cb->f[1]));
else
setres(1);
break;
case CMreset:
resetmouse();
if(accelerated)
setaccelerated(accelerated);
if(resolution)
setres(resolution);
if(intellimouse)
setintellimouse();
break;
case CMserial:
if(mousetype == Mouseserial)
error(Emouseset);
if(cb->nf > 2){
if(strcmp(cb->f[2], "M") == 0)
i8250mouse(cb->f[1], m3mouseputc, 0);
else if(strcmp(cb->f[2], "MI") == 0)
i8250mouse(cb->f[1], m5mouseputc, 0);
else
i8250mouse(cb->f[1], mouseputc, cb->nf == 1);
} else
i8250mouse(cb->f[1], mouseputc, cb->nf == 1);
mousetype = Mouseserial;
strncpy(mouseport, cb->f[1], sizeof(mouseport)-1);
packetsize = 3;
break;
case CMhwaccel:
if(strcmp(cb->f[1], "on")==0)
mousehwaccel = 1;
else if(strcmp(cb->f[1], "off")==0)
mousehwaccel = 0;
else
cmderror(cb, "bad mouse control message");
}
qunlock(&mousectlqlock);
poperror();
}