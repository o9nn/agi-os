#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#define	Image	IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
enum {
Index		= 0x06,
Data		= 0x07,
};
static ushort dacxreg[4] = {
PaddrW, Pdata, Pixmask, PaddrR
};
static uchar
tvp3020io(uchar reg, uchar data)
{
uchar crt55;
crt55 = vgaxi(Crtx, 0x55) & 0xFC;
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
vgao(dacxreg[reg & 0x03], data);
return crt55;
}
static void
tvp3020xo(uchar index, uchar data)
{
uchar crt55;
crt55 = tvp3020io(Index, index);
vgao(dacxreg[Data & 0x03], data);
vgaxo(Crtx, 0x55, crt55);
}
static void
tvp3020disable(VGAscr*)
{
uchar r;
tvp3020xo(0x06, 0x10);
r = vgaxi(Crtx, 0x45) & ~0x20;
vgaxo(Crtx, 0x45, r);
r = vgaxi(Crtx, 0x55) & ~0x20;
vgaxo(Crtx, 0x55, r);
}
static void
tvp3020enable(VGAscr*)
{
uchar r;
tvp3020xo(0x06, 0x10);
tvp3020xo(0x20, Pwhite); tvp3020xo(0x21, Pwhite); tvp3020xo(0x22, Pwhite);
tvp3020xo(0x23, Pwhite); tvp3020xo(0x24, Pwhite); tvp3020xo(0x25, Pwhite);
tvp3020xo(0x26, Pblack); tvp3020xo(0x27, Pblack); tvp3020xo(0x28, Pblack);
r = vgaxi(Crtx, 0x55)|0x20;
vgaxo(Crtx, 0x55, r);
r = vgaxi(Crtx, 0x45)|0x20;
vgaxo(Crtx, 0x45, r);
}
static void
tvp3020load(VGAscr*, Cursor* curs)
{
uchar p, p0, p1;
int x, y;
tvp3020xo(0x06, 0x10);
tvp3020xo(0x08, 0x00);
tvp3020xo(0x09, 0x00);
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16){
p0 = curs->clr[x+y*2];
p1 = curs->set[x+y*2];
p = 0x00;
if(p1 & 0x10)
p |= 0x03;
else if(p0 & 0x10)
p |= 0x02;
if(p1 & 0x20)
p |= 0x0C;
else if(p0 & 0x20)
p |= 0x08;
if(p1 & 0x40)
p |= 0x30;
else if(p0 & 0x40)
p |= 0x20;
if(p1 & 0x80)
p |= 0xC0;
else if(p0 & 0x80)
p |= 0x80;
tvp3020xo(0x0A, p);
p = 0x00;
if(p1 & 0x01)
p |= 0x03;
else if(p0 & 0x01)
p |= 0x02;
if(p1 & 0x02)
p |= 0x0C;
else if(p0 & 0x02)
p |= 0x08;
if(p1 & 0x04)
p |= 0x30;
else if(p0 & 0x04)
p |= 0x20;
if(p1 & 0x08)
p |= 0xC0;
else if(p0 & 0x08)
p |= 0x80;
tvp3020xo(0x0A, p);
}
else{
tvp3020xo(0x0A, 0x00);
tvp3020xo(0x0A, 0x00);
}
}
}
tvp3020xo(0x04, -curs->offset.x);
tvp3020xo(0x05, -curs->offset.y);
tvp3020xo(0x06, 0x40|0x10);
}
static int
tvp3020move(VGAscr*, Point p)
{
tvp3020xo(0x00, p.x & 0xFF);
tvp3020xo(0x01, (p.x>>8) & 0x0F);
tvp3020xo(0x02, p.y & 0xFF);
tvp3020xo(0x03, (p.y>>8) & 0x0F);
return 0;
}
VGAcur vgatvp3020cur = {
"tvp3020hwgc",
tvp3020enable,
tvp3020disable,
tvp3020load,
tvp3020move,
};