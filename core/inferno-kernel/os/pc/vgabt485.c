#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "../port/error.h"
#define Image IMAGE
#include <draw.h>
#include <memdraw.h>
#include <cursor.h>
#include "screen.h"
enum {
AddrW = 0x00,
Palette = 0x01,
Pmask = 0x02,
AddrR = 0x03,
ColorW = 0x04,
Color = 0x05,
Cmd0 = 0x06,
ColorR = 0x07,
Cmd1 = 0x08,
Cmd2 = 0x09,
Status = 0x0A,
Cmd3 = 0x1A,
Cram = 0x0B,
Cxlr = 0x0C,
Cxhr = 0x0D,
Cylr = 0x0E,
Cyhr = 0x0F,
Nreg = 0x10,
};
static ushort dacxreg[4] = {
PaddrW, Pdata, Pixmask, PaddrR
};
static uchar
bt485io(uchar reg)
{
uchar crt55, cr0;
crt55 = vgaxi(Crtx, 0x55) & 0xFC;
if((reg & 0x0F) == Status){
vgaxo(Crtx, 0x55, crt55|((Cmd0>>2) & 0x03));
cr0 = vgai(dacxreg[Cmd0 & 0x03])|0x80;
vgao(dacxreg[Cmd0 & 0x03], cr0);
vgaxo(Crtx, 0x55, crt55|((AddrW>>2) & 0x03));
vgao(dacxreg[AddrW & 0x03], (reg == Status) ? 0x00: 0x01);
}
return crt55;
}
static uchar
bt485i(uchar reg)
{
uchar crt55, r;
crt55 = bt485io(reg);
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
r = vgai(dacxreg[reg & 0x03]);
vgaxo(Crtx, 0x55, crt55);
return r;
}
static void
bt485o(uchar reg, uchar data)
{
uchar crt55;
crt55 = bt485io(reg);
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
vgao(dacxreg[reg & 0x03], data);
vgaxo(Crtx, 0x55, crt55);
}
static void
bt485disable(VGAscr*)
{
uchar r;
r = bt485i(Cmd2) & ~0x03;
bt485o(Cmd2, r);
r = vgaxi(Crtx, 0x45) & ~0x20;
vgaxo(Crtx, 0x45, r);
r = vgaxi(Crtx, 0x55) & ~0x20;
vgaxo(Crtx, 0x55, r);
}
static void
bt485enable(VGAscr*)
{
uchar r;
r = bt485i(Cmd2) & 0xFC;
bt485o(Cmd2, r);
bt485o(ColorW, 0x00);
bt485o(Color, Pwhite); bt485o(Color, Pwhite); bt485o(Color, Pwhite);
bt485o(Color, Pwhite); bt485o(Color, Pwhite); bt485o(Color, Pwhite);
bt485o(Color, Pblack); bt485o(Color, Pblack); bt485o(Color, Pblack);
bt485o(Color, Pblack); bt485o(Color, Pblack); bt485o(Color, Pblack);
r = vgaxi(Crtx, 0x55)|0x20;
vgaxo(Crtx, 0x55, r);
r = vgaxi(Crtx, 0x45)|0x20;
vgaxo(Crtx, 0x45, r);
}
static void
bt485load(VGAscr* scr, Cursor* curs)
{
uchar r;
int x, y;
r = bt485i(Cmd2) & 0xFC;
bt485o(Cmd2, r);
r = (bt485i(Cmd3) & 0xFC)|0x04;
bt485o(Cmd3, r);
bt485o(AddrW, 0x00);
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
bt485o(Cram, curs->clr[x+y*2]);
else
bt485o(Cram, 0x00);
}
}
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
bt485o(Cram, curs->set[x+y*2]);
else
bt485o(Cram, 0x00);
}
}
scr->offset.x = 64+curs->offset.x;
scr->offset.y = 64+curs->offset.y;
r = (bt485i(Cmd2) & 0xFC)|0x01;
bt485o(Cmd2, r);
}
static int
bt485move(VGAscr* scr, Point p)
{
int x, y;
x = p.x+scr->offset.x;
y = p.y+scr->offset.y;
bt485o(Cxlr, x & 0xFF);
bt485o(Cxhr, (x>>8) & 0x0F);
bt485o(Cylr, y & 0xFF);
bt485o(Cyhr, (y>>8) & 0x0F);
return 0;
}
VGAcur vgabt485cur = {
"bt485hwgc",
bt485enable,
bt485disable,
bt485load,
bt485move,
};