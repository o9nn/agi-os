#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#define Image IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
enum {
IndexLo = 0x00,
IndexHi = 0x01,
Data = 0x02,
IndexCtl = 0x03,
};
enum {
CursorCtl = 0x30,
CursorXLo = 0x31,
CursorXHi = 0x32,
CursorYLo = 0x33,
CursorYHi = 0x34,
CursorHotX = 0x35,
CursorHotY = 0x36,
CursorR1 = 0x40,
CursorG1 = 0x41,
CursorB1 = 0x42,
CursorR2 = 0x43,
CursorG2 = 0x44,
CursorB2 = 0x45,
CursorR3 = 0x46,
CursorG3 = 0x47,
CursorB3 = 0x48,
CursorArray = 0x100,
};
static ushort dacxreg[4] = {
PaddrW, Pdata, Pixmask, PaddrR
};
static uchar
rgb524setrs2(void)
{
uchar rs2;
rs2 = vgaxi(Crtx, 0x55);
vgaxo(Crtx, 0x55, (rs2 & 0xFC)|0x01);
return rs2;
}
static void
rgb524xo(int index, uchar data)
{
vgao(dacxreg[IndexLo], index & 0xFF);
vgao(dacxreg[IndexHi], (index>>8) & 0xFF);
vgao(dacxreg[Data], data);
}
static void
rgb524disable(VGAscr*)
{
uchar rs2;
rs2 = rgb524setrs2();
rgb524xo(CursorCtl, 0x00);
vgaxo(Crtx, 0x55, rs2);
}
static void
rgb524enable(VGAscr*)
{
uchar rs2;
rs2 = rgb524setrs2();
rgb524xo(CursorCtl, 0x00);
rgb524xo(CursorR1, Pwhite); rgb524xo(CursorG1, Pwhite); rgb524xo(CursorB1, Pwhite);
rgb524xo(CursorR2, Pblack); rgb524xo(CursorG2, Pblack); rgb524xo(CursorB2, Pblack);
rgb524xo(CursorCtl, 0x23);
vgaxo(Crtx, 0x55, rs2);
}
static void
rgb524load(VGAscr*, Cursor* curs)
{
uchar p, p0, p1, rs2;
int x, y;
rs2 = rgb524setrs2();
rgb524xo(CursorCtl, 0x00);
vgao(dacxreg[IndexCtl], 0x01);
vgao(dacxreg[IndexLo], CursorArray & 0xFF);
vgao(dacxreg[IndexHi], (CursorArray>>8) & 0xFF);
for(y = 0; y < 32; y++){
for(x = 0; x < 32/8; x++){
if(x < 16/8 && y < 16){
p0 = curs->clr[x+y*2];
p1 = curs->set[x+y*2];
p = 0x00;
if(p1 & 0x80)
p |= 0xC0;
else if(p0 & 0x80)
p |= 0x80;
if(p1 & 0x40)
p |= 0x30;
else if(p0 & 0x40)
p |= 0x20;
if(p1 & 0x20)
p |= 0x0C;
else if(p0 & 0x20)
p |= 0x08;
if(p1 & 0x10)
p |= 0x03;
else if(p0 & 0x10)
p |= 0x02;
vgao(dacxreg[Data], p);
p = 0x00;
if(p1 & 0x08)
p |= 0xC0;
else if(p0 & 0x08)
p |= 0x80;
if(p1 & 0x04)
p |= 0x30;
else if(p0 & 0x04)
p |= 0x20;
if(p1 & 0x02)
p |= 0x0C;
else if(p0 & 0x02)
p |= 0x08;
if(p1 & 0x01)
p |= 0x03;
else if(p0 & 0x01)
p |= 0x02;
vgao(dacxreg[Data], p);
}
else{
vgao(dacxreg[Data], 0x00);
vgao(dacxreg[Data], 0x00);
}
}
}
rgb524xo(CursorHotX, -curs->offset.x);
rgb524xo(CursorHotY, -curs->offset.y);
rgb524xo(CursorCtl, 0x23);
vgaxo(Crtx, 0x55, rs2);
}
static int
rgb524move(VGAscr*, Point p)
{
uchar rs2;
rs2 = rgb524setrs2();
rgb524xo(CursorXLo, p.x & 0xFF);
rgb524xo(CursorXHi, (p.x>>8) & 0x0F);
rgb524xo(CursorYLo, p.y & 0xFF);
rgb524xo(CursorYHi, (p.y>>8) & 0x0F);
vgaxo(Crtx, 0x55, rs2);
return 0;
}
VGAcur vgargb524cur = {
"rgb524hwgc",
rgb524enable,
rgb524disable,
rgb524load,
rgb524move,
};