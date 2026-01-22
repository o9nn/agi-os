#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#include "io.h"
enum
{
Mouseother=	0,
Mouseserial=	1,
MousePS2=	2,
};
static int mousetype;
static void
ps2mouseputc(int c, int shift)
{
static short msg[3];
static int nb;
static uchar b[] = {0, 1, 4, 5, 2, 3, 6, 7, 0, 1, 2, 5, 2, 3, 6, 7 };
int buttons, dx, dy;
if(nb==0 && (c&0xc8)!=0x08)
return;
msg[nb] = c;
if(++nb == 3){
nb = 0;
if(msg[0] & 0x10)
msg[1] |= 0xFF00;
if(msg[0] & 0x20)
msg[2] |= 0xFF00;
buttons = b[(msg[0]&7) | (shift ? 8 : 0)];
dx = msg[1];
dy = -msg[2];
mousetrack(buttons, dx, dy, 1);
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
}
void
ps2mouselink(void)
{
ps2mouse();
}