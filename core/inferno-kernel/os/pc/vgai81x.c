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
typedef struct
{
ushort ctl;
ushort pad;
ulong base;
ulong pos;
} CursorI81x;
enum {
Fbsize = 8*MB,
hwCur = 0x70080,
};
static Pcidev *
i81xpcimatch(void)
{
Pcidev *p;
p = nil;
while((p = pcimatch(p, 0x8086, 0)) != nil){
switch(p->did){
default:
continue;
case 0x7121:
case 0x7123:
case 0x7125:
case 0x1102:
case 0x1112:
case 0x1132:
case 0x3577:
return p;
}
}
return nil;
}
static ulong
i81xlinear(VGAscr* scr, int* size, int* align)
{
Pcidev *p;
int oapsize, wasupamem;
ulong aperture, oaperture, fbuf, fbend, *rp;
oaperture = scr->aperture;
oapsize = scr->apsize;
wasupamem = scr->isupamem;
aperture = 0;
p = i81xpcimatch();
if(p != nil) {
aperture = p->mem[0].bar & ~0x0F;
*size = p->mem[0].size;
if(*size > Fbsize)
*size = Fbsize;
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
if(oapsize == 0) {
fbuf = PADDR(xspanalloc(*size, BY2PG, 0));
fbend = PGROUND(fbuf+*size);
rp = KADDR(scr->io+0x10000);
while(fbuf < fbend) {
*rp++ = fbuf | (1<<0);
fbuf += BY2PG;
}
}
return aperture;
}
static void
i81xenable(VGAscr* scr)
{
Pcidev *p;
int align, size;
Mach *mach0;
ulong aperture, pgtbl, *rp, cursor, *pte;
if(scr->io)
return;
p = i81xpcimatch();
if(p == nil)
return;
scr->io = upamalloc(p->mem[1].bar & ~0x0F, p->mem[1].size, 0);
if(scr->io == 0)
return;
pgtbl = PADDR(xspanalloc(64*1024, BY2PG, 0));
rp = KADDR(scr->io+0x2020);
*rp = pgtbl | 1;
addvgaseg("i81xmmio", (ulong)scr->io, p->mem[0].size);
size = p->mem[0].size;
align = 0;
aperture = i81xlinear(scr, &size, &align);
if(aperture){
scr->aperture = aperture;
scr->apsize = size;
addvgaseg("i81xscreen", aperture, size);
}
cursor = (ulong)xspanalloc(BY2PG, BY2PG, 0);
mach0 = MACHP(0);
pte = mmuwalk(mach0->pdb, cursor, 2, 0);
if(pte == nil)
panic("i81x cursor");
*pte |= PTEUNCACHED;
scr->storage = PADDR(cursor);
}
static void
i81xcurdisable(VGAscr* scr)
{
CursorI81x *hwcurs;
if(scr->io == 0)
return;
hwcurs = KADDR(scr->io+hwCur);
hwcurs->ctl = (1<<4);
}
static void
i81xcurload(VGAscr* scr, Cursor* curs)
{
int y;
uchar *p;
CursorI81x *hwcurs;
if(scr->io == 0)
return;
hwcurs = KADDR(scr->io+hwCur);
hwcurs->ctl = (1<<4);
p = KADDR(scr->storage);
for(y = 0; y < 16; y += 2) {
*p++ = ~(curs->clr[2*y]|curs->set[2*y]);
*p++ = ~(curs->clr[2*y+1]|curs->set[2*y+1]);
p += 2;
*p++ = ~(curs->clr[2*y+2]|curs->set[2*y+2]);
*p++ = ~(curs->clr[2*y+3]|curs->set[2*y+3]);
p += 2;
*p++ = curs->set[2*y];
*p++ = curs->set[2*y+1];
p += 2;
*p++ = curs->set[2*y+2];
*p++ = curs->set[2*y+3];
p += 2;
}
scr->offset.x = curs->offset.x;
scr->offset.y = curs->offset.y;
hwcurs->ctl = (1<<4)|1;
}
static int
i81xcurmove(VGAscr* scr, Point p)
{
int x, y;
ulong pos;
CursorI81x *hwcurs;
if(scr->io == 0)
return 1;
hwcurs = KADDR(scr->io+hwCur);
x = p.x+scr->offset.x;
y = p.y+scr->offset.y;
pos = 0;
if(x < 0) {
pos |= (1<<15);
x = -x;
}
if(y < 0) {
pos |= (1<<31);
y = -y;
}
pos |= ((y&0x7ff)<<16)|(x&0x7ff);
hwcurs->pos = pos;
return 0;
}
static void
i81xcurenable(VGAscr* scr)
{
int i;
uchar *p;
CursorI81x *hwcurs;
i81xenable(scr);
if(scr->io == 0)
return;
hwcurs = KADDR(scr->io+hwCur);
hwcurs->base = scr->storage;
p = KADDR(scr->storage);
for(i = 0; i < 32/2; i++) {
memset(p, 0xff, 8);
memset(p+8, 0, 8);
p += 16;
}
i81xcurload(scr, &arrow);
i81xcurmove(scr, ZP);
}
VGAdev vgai81xdev = {
"i81x",
i81xenable,
nil,
nil,
i81xlinear,
};
VGAcur vgai81xcur = {
"i81xhwgc",
i81xcurenable,
i81xcurdisable,
i81xcurload,
i81xcurmove,
};