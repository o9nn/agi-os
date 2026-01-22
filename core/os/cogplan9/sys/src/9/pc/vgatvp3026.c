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
Index		= 0x00,
Data		= 0x0A,
CaddrW		= 0x04,
Cdata		= 0x05,
Cctl		= 0x09,
Cram		= 0x0B,
Cxlsb		= 0x0C,
Cxmsb		= 0x0D,
Cylsb		= 0x0E,
Cymsb		= 0x0F,
Icctl		= 0x06,
};
static ushort dacxreg[4] = {
PaddrW, Pdata, Pixmask, PaddrR
};
static uchar
tvp3026io(uchar reg, uchar data)
{
uchar crt55;
crt55 = vgaxi(Crtx, 0x55) & 0xFC;
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
vgao(dacxreg[reg & 0x03], data);
return crt55;
}
static void
tvp3026o(uchar reg, uchar data)
{
uchar crt55;
crt55 = tvp3026io(reg, data);
vgaxo(Crtx, 0x55, crt55);
}
void
tvp3026xo(uchar index, uchar data)
{
uchar crt55;
crt55 = tvp3026io(Index, index);
vgaxo(Crtx, 0x55, crt55|((Data>>2) & 0x03));
vgao(dacxreg[Data & 0x03], data);
vgaxo(Crtx, 0x55, crt55);
}
static void
tvp3026disable(VGAscr*)
{
tvp3026xo(Icctl, 0x90);
tvp3026o(Cctl, 0x00);
}
static void
tvp3026enable(VGAscr*)
{
tvp3026xo(Icctl, 0x90);
tvp3026o(Cctl, 0x00);
tvp3026o(CaddrW, 0x00);
tvp3026o(Cdata, Pwhite); tvp3026o(Cdata, Pwhite); tvp3026o(Cdata, Pwhite);
tvp3026o(Cdata, Pwhite); tvp3026o(Cdata, Pwhite); tvp3026o(Cdata, Pwhite);
tvp3026o(Cdata, Pblack); tvp3026o(Cdata, Pblack); tvp3026o(Cdata, Pblack);
tvp3026o(Cdata, Pblack); tvp3026o(Cdata, Pblack); tvp3026o(Cdata, Pblack);
tvp3026o(Cctl, 0x01);
}
static void
tvp3026load(VGAscr* scr, Cursor* curs)
{
int x, y;
tvp3026xo(Icctl, 0x90);
tvp3026o(Cctl, 0x00);
vgao(PaddrW, 0x00);
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
tvp3026o(Cram, curs->clr[x+y*2]);
else
tvp3026o(Cram, 0x00);
}
}
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
tvp3026o(Cram, curs->set[x+y*2]);
else
tvp3026o(Cram, 0x00);
}
}
scr->offset.x = 64+curs->offset.x;
scr->offset.y = 64+curs->offset.y;
tvp3026o(Cctl, 0x01);
}
static int
tvp3026move(VGAscr* scr, Point p)
{
int x, y;
x = p.x+scr->offset.x;
y = p.y+scr->offset.y;
tvp3026o(Cxlsb, x & 0xFF);
tvp3026o(Cxmsb, (x>>8) & 0x0F);
tvp3026o(Cylsb, y & 0xFF);
tvp3026o(Cymsb, (y>>8) & 0x0F);
return 0;
}
VGAcur vgatvp3026cur = {
"tvp3026hwgc",
tvp3026enable,
tvp3026disable,
tvp3026load,
tvp3026move,
};