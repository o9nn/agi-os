#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Cr0		= 0x00,
Mid		= 0x01,
Did		= 0x02,
Cr1		= 0x03,
Reserve1	= 0x04,
Reserve2	= 0x06,
Reserve3	= 0x08,
Reserve4	= 0x0A,
IstartX		= 0x0C,
IstartY		= 0x0E,
IendX		= 0x10,
IendY		= 0x12,
RatioX		= 0x14,
RatioY		= 0x16,
OffsetX		= 0x18,
OffsetY		= 0x1A,
TestR		= 0x1C,
TestG		= 0x1D,
TestB		= 0x1E,
Nir		= 0x1F,
};
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hpclk2x8|Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong grade, pclk;
char *p;
grade = 110000000;
if(p = strrchr(ctlr->name, '-'))
grade = strtoul(p+1, 0, 0) * 1000000;
if(grade == 170000000)
pclk = 135000000;
else
pclk = grade;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->ctlr && (vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8 && vga->f[0] >= 60000000){
vga->f[0] /= 2;
resyncinit(vga, ctlr, Upclk2x8, 0);
}
if(vga->f[0] > pclk)
error("%s: invalid pclk - %ld\n", ctlr->name, vga->f[0]);
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
uchar mode, x;
attdaco(Cr0, 0x08);
mode = 0x00;
if(ctlr->flag & Upclk2x8)
mode = 0x20;
if(vga->mode->z == 8 && 0)
mode |= 0x02;
x = attdaci(Cr1) & 0x80;
attdaco(Cr1, x);
attdaco(Cr0, mode);
ctlr->flag |= Fload;
}
static void
dump(Vga*, Ctlr* ctlr)
{
int i;
printitem(ctlr->name, "");
for(i = 0; i < Nir; i++)
printreg(attdaci(i));
}
Ctlr w30c516 = {
"w30c516",
0,
options,
init,
load,
dump,
};