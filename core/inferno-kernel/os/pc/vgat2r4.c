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
IndexLo = 0x10/4,
IndexHi = 0x14/4,
Data = 0x18/4,
IndexCtl = 0x1C/4,
Zoom = 0x54/4,
};
enum {
CursorSyncCtl = 0x03,
HsyncHi = 0x01,
HsyncLo = 0x02,
VsyncHi = 0x04,
VsyncLo = 0x08,
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
CursorMode32x32 = 0x23,
CursorMode64x64 = 0x27,
CursorMode = CursorMode32x32,
};
static ulong
t2r4linear(VGAscr* scr, int* size, int* align)
{
ulong aperture, oaperture;
int oapsize, wasupamem;
Pcidev *p;
oaperture = scr->aperture;
oapsize = scr->apsize;
wasupamem = scr->isupamem;
aperture = 0;
if(p = pcimatch(nil, 0x105D, 0)){
switch(p->did){
case 0x5348:
aperture = p->mem[0].bar & ~0x0F;
*size = p->mem[0].size;
break;
default:
break;
}
}
if(wasupamem){
if(oaperture == aperture)
return oaperture;
upafree(oaperture, oapsize);
}
scr->isupamem = 0;
aperture = upamalloc(aperture, *size, *align);
if(aperture == 0){
if(wasupamem && upamalloc(oaperture, oapsize, 0)){
aperture = oaperture;
scr->isupamem = 1;
}
else
scr->isupamem = 0;
}
else
scr->isupamem = 1;
return aperture;
}
static void
t2r4enable(VGAscr* scr)
{
Pcidev *p;
int size, align;
ulong aperture, mmio;
if(scr->mmio)
return;
if(p = pcimatch(nil, 0x105D, 0)){
switch(p->did){
case 0x5348:
break;
default:
return;
}
}
else
return;
mmio = upamalloc(p->mem[4].bar & ~0x0F, p->mem[4].size, 0);
if(mmio == 0)
return;
addvgaseg("t2r4mmio", mmio, p->mem[4].size);
scr->mmio = KADDR(mmio);
size = p->mem[0].size;
align = 0;
aperture = t2r4linear(scr, &size, &align);
if(aperture){
scr->aperture = aperture;
scr->apsize = size;
addvgaseg("t2r4screen", aperture, size);
}
}
static uchar
t2r4xi(VGAscr* scr, int index)
{
ulong *mmio;
mmio = scr->mmio;
mmio[IndexLo] = index & 0xFF;
mmio[IndexHi] = (index>>8) & 0xFF;
return mmio[Data];
}
static void
t2r4xo(VGAscr* scr, int index, uchar data)
{
ulong *mmio;
mmio = scr->mmio;
mmio[IndexLo] = index & 0xFF;
mmio[IndexHi] = (index>>8) & 0xFF;
mmio[Data] = data;
}
static void
t2r4curdisable(VGAscr* scr)
{
if(scr->mmio == 0)
return;
t2r4xo(scr, CursorCtl, 0x00);
}
static void
t2r4curload(VGAscr* scr, Cursor* curs)
{
uchar *data;
int size, x, y, zoom;
ulong clr, *mmio, pixels, set;
mmio = scr->mmio;
if(mmio == 0)
return;
t2r4xo(scr, CursorCtl, 0x00);
mmio[IndexCtl] = 0x01;
mmio[IndexLo] = CursorArray & 0xFF;
mmio[IndexHi] = (CursorArray>>8) & 0xFF;
if(mmio[Zoom] & 0x0F)
zoom = 32;
else
zoom = 16;
data = (uchar*)&mmio[Data];
for(y = 0; y < zoom; y++){
clr = (curs->clr[2*y]<<8)|curs->clr[y*2 + 1];
set = (curs->set[2*y]<<8)|curs->set[y*2 + 1];
pixels = 0;
for(x = 0; x < 16; x++){
if(set & (1<<x))
pixels |= 0x03<<(x*2);
else if(clr & (1<<x))
pixels |= 0x02<<(x*2);
}
*data = pixels>>24;
*data = pixels>>16;
*data = pixels>>8;
*data = pixels;
*data = 0x00;
*data = 0x00;
*data = 0x00;
*data = 0x00;
if(CursorMode == CursorMode32x32 && zoom == 16)
continue;
*data = pixels>>24;
*data = pixels>>16;
*data = pixels>>8;
*data = pixels;
*data = 0x00;
*data = 0x00;
*data = 0x00;
*data = 0x00;
}
if(CursorMode == CursorMode32x32)
size = 32;
else
size = 64;
while(y < size){
for(x = 0; x < size/8; x++){
*data = 0x00;
*data = 0x00;
}
y++;
}
mmio[IndexCtl] = 0x00;
t2r4xo(scr, CursorHotX, -curs->offset.x);
zoom = (scr->mmio[Zoom] & 0x0F)+1;
t2r4xo(scr, CursorHotY, -curs->offset.y*zoom);
t2r4xo(scr, CursorCtl, CursorMode);
}
static int
t2r4curmove(VGAscr* scr, Point p)
{
int y, zoom;
if(scr->mmio == 0)
return 1;
t2r4xo(scr, CursorXLo, p.x & 0xFF);
t2r4xo(scr, CursorXHi, (p.x>>8) & 0x0F);
zoom = (scr->mmio[Zoom] & 0x0F)+1;
y = p.y*zoom;
t2r4xo(scr, CursorYLo, y & 0xFF);
t2r4xo(scr, CursorYHi, (y>>8) & 0x0F);
return 0;
}
static void
t2r4curenable(VGAscr* scr)
{
t2r4enable(scr);
if(scr->mmio == 0)
return;
t2r4xo(scr, CursorCtl, 0x00);
t2r4xo(scr, CursorR1, Pwhite);
t2r4xo(scr, CursorG1, Pwhite);
t2r4xo(scr, CursorB1, Pwhite);
t2r4xo(scr, CursorR2, Pblack);
t2r4xo(scr, CursorG2, Pblack);
t2r4xo(scr, CursorB2, Pblack);
t2r4curload(scr, &arrow);
t2r4curmove(scr, ZP);
t2r4xo(scr, CursorCtl, CursorMode);
}
enum {
Flow = 0x08/4,
Busy = 0x0C/4,
BufCtl = 0x20/4,
DeSorg = 0x28/4,
DeDorg = 0x2C/4,
DeSptch = 0x40/4,
DeDptch = 0x44/4,
CmdOpc = 0x50/4,
CmdRop = 0x54/4,
CmdStyle = 0x58/4,
CmdPatrn = 0x5C/4,
CmdClp = 0x60/4,
CmdPf = 0x64/4,
Fore = 0x68/4,
Back = 0x6C/4,
Mask = 0x70/4,
DeKey = 0x74/4,
Lpat = 0x78/4,
Pctrl = 0x7C/4,
Clptl = 0x80/4,
Clpbr = 0x84/4,
XY0 = 0x88/4,
XY1 = 0x8C/4,
XY2 = 0x90/4,
XY3 = 0x94/4,
XY4 = 0x98/4,
Alpha = 0x128/4,
ACtl = 0x16C/4,
RBaseD = 0x4000/4,
};
static void
waitforfifo(VGAscr *scr)
{
int x;
ulong *d;
x = 0;
d = scr->mmio + RBaseD;
while((d[Busy]&1) && x++ < 1000000)
;
if(x >= 1000000)
iprint("busy %8lux\n", d[Busy]);
}
static void
waitforcmd(VGAscr *scr)
{
int x;
ulong *d;
x = 0;
d = scr->mmio + RBaseD;
while((d[Flow]&0x1B) && x++ < 1000000)
;
if(x >= 1000000)
iprint("flow %8lux\n", d[Flow]);
}
static void
waitformem(VGAscr *scr)
{
int x;
ulong *d;
x = 0;
d = scr->mmio + RBaseD;
while((d[Flow]&2)&& x++ < 1000000)
;
if(x >= 1000000)
iprint("mem %8lux\n", d[Busy]);
}
static int
t2r4hwscroll(VGAscr *scr, Rectangle r, Rectangle sr)
{
int ctl;
Point dp, sp;
ulong *d;
int depth;
if(r.min.y == sr.min.y){
depth = scr->gscreen->depth;
switch(depth){
case 32:
if(Dx(r) < 32)
return 0;
break;
case 16:
if(Dx(r) < 104)
return 0;
break;
}
}
waitformem(scr);
waitforfifo(scr);
d = scr->mmio + RBaseD;
ctl = 0;
if(r.min.x <= sr.min.x){
dp.x = r.min.x;
sp.x = sr.min.x;
}else{
ctl |= 2;
dp.x = r.max.x-1;
sp.x = sr.max.x-1;
}
if(r.min.y < sr.min.y){
dp.y = r.min.y;
sp.y = sr.min.y;
}else{
ctl |= 1;
dp.y = r.max.y-1;
sp.y = sr.max.y-1;
}
d[CmdOpc] = 0x1;
d[CmdRop] = 0xC;
d[CmdStyle] = 0;
d[CmdPatrn] = 0;
d[Fore] = 0;
d[Back] = 0;
d[XY3] = ctl;
d[XY0] = (sp.x<<16)|sp.y;
d[XY2] = (Dx(r)<<16)|Dy(r);
d[XY4] = 0;
d[XY1] = (dp.x<<16)|dp.y;
waitforcmd(scr);
return 1;
}
static int
t2r4hwfill(VGAscr *scr, Rectangle r, ulong sval)
{
ulong *d;
d = scr->mmio + RBaseD;
waitformem(scr);
waitforfifo(scr);
d[CmdOpc] = 0x1;
d[CmdRop] = 0xC;
d[CmdStyle] = 1;
d[CmdPatrn] = 0;
d[Fore] = sval;
d[Back] = sval;
d[XY3] = 0;
d[XY0] = (r.min.x<<16)|r.min.y;
d[XY2] = (Dx(r)<<16)|Dy(r);
d[XY4] = 0;
d[XY1] = (r.min.x<<16)|r.min.y;
waitforcmd(scr);
return 1;
}
static void
t2r4blank(VGAscr *scr, int blank)
{
uchar x;
x = t2r4xi(scr, CursorSyncCtl);
x &= ~0x0F;
if(blank)
x |= HsyncLo | VsyncLo;
t2r4xo(scr, CursorSyncCtl, x);
}
static void
t2r4drawinit(VGAscr *scr)
{
ulong pitch;
int depth;
int fmt;
ulong *d;
pitch = Dx(scr->gscreen->r);
depth = scr->gscreen->depth;
switch(scr->gscreen->chan){
case RGB16:
fmt = 3;
break;
case XRGB32:
fmt = 2;
break;
case RGB15:
fmt = 1;
break;
default:
scr->fill = nil;
scr->scroll = nil;
return;
}
d = scr->mmio + RBaseD;
d[BufCtl] = fmt<<24;
d[DeSorg] = 0;
d[DeDorg] = 0;
d[DeSptch] = (pitch*depth)/8;
d[DeDptch] = (pitch*depth)/8;
d[CmdClp] = 0;
d[Mask] = ~0;
d[DeKey] = 0;
d[Clptl] = 0;
d[Clpbr] = 0xFFF0FFF0;
d[Alpha] = 0;
d[ACtl] = 0;
scr->fill = t2r4hwfill;
scr->scroll = t2r4hwscroll;
scr->blank = t2r4blank;
hwblank = 1;
}
VGAdev vgat2r4dev = {
"t2r4",
t2r4enable,
nil,
nil,
t2r4linear,
t2r4drawinit,
};
VGAcur vgat2r4cur = {
"t2r4hwgc",
t2r4curenable,
t2r4curdisable,
t2r4curload,
t2r4curmove,
};