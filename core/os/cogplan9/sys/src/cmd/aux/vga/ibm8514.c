#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Subsys = 0x42E8,
Advfunc = 0x4AE8,
CurY = 0x82E8,
CurX = 0x86E8,
DestyAxstp = 0x8AE8,
DestxDiastp = 0x8EE8,
ErrTerm = 0x92E8,
MajAxisPcnt = 0x96E8,
GPstat = 0x9AE8,
Cmd = 0x9AE8,
ShortStroke = 0x9EE8,
BkgdColor = 0xA2E8,
FrgdColor = 0xA6E8,
WrtMask = 0xAAE8,
RdMask = 0xAEE8,
ColorCmp = 0xB2E8,
BkgdMix = 0xB6E8,
FrgdMix = 0xBAE8,
Multifunc = 0xBEE8,
PixTrans = 0xE2E8,
};
enum {
MinAxisPcnt = 0x0000,
ScissorsT = 0x1000,
ScissorsL = 0x2000,
ScissorsB = 0x3000,
ScissorsR = 0x4000,
MemCntl = 0x5000,
PixCntl = 0xA000,
MultMisc = 0xE000,
ReadSel = 0xF000,
};
static void
load(Vga* vga, Ctlr*)
{
outportw(Pixmask, 0x00);
outportw(Subsys, 0x8000|0x1000);
outportw(Subsys, 0x4000|0x1000);
outportw(Pixmask, 0xFF);
outportw(FrgdMix, 0x47);
outportw(BkgdMix, 0x07);
outportw(Multifunc, ScissorsT|0x000);
outportw(Multifunc, ScissorsL|0x000);
outportw(Multifunc, ScissorsB|(vga->vmz/vga->mode->x-1));
outportw(Multifunc, ScissorsR|(vga->mode->x-1));
outportw(WrtMask, 0xFFFF);
outportw(Multifunc, PixCntl|0x0000);
}
static void
dump(Vga*, Ctlr* ctlr)
{
printitem(ctlr->name, "Advfunc");
Bprint(&stdout, "%9.4uX\n", inportw(Advfunc));
printitem(ctlr->name, "Subsys");
Bprint(&stdout, "%9.4uX\n", inportw(Subsys));
}
Ctlr ibm8514 = {
"ibm8514",
0,
0,
0,
load,
dump,
};