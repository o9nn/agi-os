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
MATROX		= 0x102B,
MGA2064		= 0x0519,
MGA2164		= 0x051B,
MGA2164AGP	= 0x051F
};
static Pcidev*
mgapcimatch(void)
{
Pcidev *p;
p = pcimatch(nil, MATROX, MGA2164AGP);
if(p == nil) {
p = pcimatch(nil, MATROX, MGA2164);
if(p == nil)
p = pcimatch(nil, MATROX, MGA2064);
}
return p;
}
static void
mga2164wenable(VGAscr* scr)
{
Pcidev *p;
if(scr->mmio)
return;
p = mgapcimatch();
if(p == nil)
return;
if(p->did == MGA2064){
scr->mmio = vmap(p->mem[0].bar&~0x0F, p->mem[0].size);
if(scr->mmio == nil)
return;
addvgaseg("mga2164wmmio", p->mem[0].bar&~0x0F, p->mem[0].size);
vgalinearaddr(scr, p->mem[1].bar&~0x0F, 8*MB);
}else{
scr->mmio = vmap(p->mem[1].bar&~0x0F, p->mem[1].size);
if(scr->mmio == nil)
return;
addvgaseg("mga2164wmmio", p->mem[1].bar&~0x0F, p->mem[1].size);
vgalinearaddr(scr, p->mem[0].bar&~0x0F, 16*MB);
}
if(scr->paddr)
addvgaseg("mga2164wscreen", scr->paddr, scr->apsize);
}
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
static void
tvp3026disable(VGAscr* scr)
{
uchar *tvp3026;
if(scr->mmio == 0)
return;
tvp3026 = (uchar*)scr->mmio+0x3C00;
*(tvp3026+Index) = Icctl;
*(tvp3026+Data) = 0x90;
*(tvp3026+Cctl) = 0x00;
}
static void
tvp3026load(VGAscr* scr, Cursor* curs)
{
int x, y;
uchar *tvp3026;
if(scr->mmio == 0)
return;
tvp3026 = (uchar*)scr->mmio+0x3C00;
tvp3026disable(scr);
*(tvp3026+Index) = 0;
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
*(tvp3026+Cram) = curs->clr[x+y*2];
else
*(tvp3026+Cram) = 0x00;
}
}
for(y = 0; y < 64; y++){
for(x = 0; x < 64/8; x++){
if(x < 16/8 && y < 16)
*(tvp3026+Cram) = curs->set[x+y*2];
else
*(tvp3026+Cram) = 0x00;
}
}
scr->offset.x = 64+curs->offset.x;
scr->offset.y = 64+curs->offset.y;
*(tvp3026+Cctl) = 0x01;
}
static int
tvp3026move(VGAscr* scr, Point p)
{
int x, y;
uchar *tvp3026;
if(scr->mmio == 0)
return 1;
tvp3026 = (uchar*)scr->mmio+0x3C00;
x = p.x+scr->offset.x;
y = p.y+scr->offset.y;
*(tvp3026+Cxlsb) = x & 0xFF;
*(tvp3026+Cxmsb) = (x>>8) & 0x0F;
*(tvp3026+Cylsb) = y & 0xFF;
*(tvp3026+Cymsb) = (y>>8) & 0x0F;
return 0;
}
static void
tvp3026enable(VGAscr* scr)
{
int i;
uchar *tvp3026;
if(scr->mmio == 0)
return;
tvp3026 = (uchar*)scr->mmio+0x3C00;
tvp3026disable(scr);
*(tvp3026+CaddrW) = 0x00;
for(i = 0; i < 6; i++)
*(tvp3026+Cdata) = Pwhite;
for(i = 0; i < 6; i++)
*(tvp3026+Cdata) = Pblack;
tvp3026load(scr, &arrow);
tvp3026move(scr, ZP);
*(tvp3026+Cctl) = 0x01;
}
VGAdev vgamga2164wdev = {
"mga2164w",
mga2164wenable,
0,
0,
0,
};
VGAcur vgamga2164wcur = {
"mga2164whwgc",
tvp3026enable,
tvp3026disable,
tvp3026load,
tvp3026move,
};