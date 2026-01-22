#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
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
Cram = 0x0B,
Cxlr = 0x0C,
Cxhr = 0x0D,
Cylr = 0x0E,
Cyhr = 0x0F,
Nreg = 0x10,
Cmd3 = 0x1A,
Cmd4 = 0x2A,
};
static uchar
bt485io(uchar reg)
{
uchar crt55, cr0;
if(reg >= Nreg && (reg & 0x0F) != Status)
error("%s: bad reg - 0x%X\n", bt485.name, reg);
crt55 = vgaxi(Crtx, 0x55) & 0xFC;
if((reg & 0x0F) == Status){
vgaxo(Crtx, 0x55, crt55|((Cmd0>>2) & 0x03));
cr0 = vgai(dacxreg[Cmd0 & 0x03])|0x80;
vgao(dacxreg[Cmd0 & 0x03], cr0);
vgaxo(Crtx, 0x55, crt55|((AddrW>>2) & 0x03));
vgao(dacxreg[AddrW & 0x03], (reg>>4) & 0x0F);
}
return crt55;
}
uchar
bt485i(uchar reg)
{
uchar crt55, r;
crt55 = bt485io(reg);
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
r = vgai(dacxreg[reg & 0x03]);
vgaxo(Crtx, 0x55, crt55);
return r;
}
void
bt485o(uchar reg, uchar data)
{
uchar crt55;
crt55 = bt485io(reg);
vgaxo(Crtx, 0x55, crt55|((reg>>2) & 0x03));
vgao(dacxreg[reg & 0x03], data);
vgaxo(Crtx, 0x55, crt55);
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hsid32|Hclk2|Hextsid|Henhanced|Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong grade;
char *p;
grade = 110000000;
if(p = strrchr(ctlr->name, '-'))
grade = strtoul(p+1, 0, 0) * 1000000;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] > grade)
error("%s: invalid pclk - %ld\n", ctlr->name, vga->f[0]);
if((ctlr->flag & Uclk2) == 0 && vga->f[0] > 67500000){
vga->f[0] /= 2;
resyncinit(vga, ctlr, Uclk2, 0);
}
ctlr->flag |= Finit;
}
static void
load(Vga*, Ctlr* ctlr)
{
uchar x;
x = bt485i(Cmd0);
bt485o(Cmd0, x|0x01);
x = bt485i(Cmd2);
if(ctlr->flag & Uenhanced)
x |= 0x10;
else
x &= ~0x10;
bt485o(Cmd2, x);
x = bt485i(Cmd3);
if(ctlr->flag & Uclk2)
x |= 0x08;
else
x &= ~0x08;
bt485o(Cmd3, x);
x = bt485i(Cmd2);
if(ctlr->flag & Uenhanced){
bt485o(Cmd1, 0x40);
x |= 0x20;
}
else{
bt485o(Cmd1, 0x00);
x &= ~0x20;
}
bt485o(Cmd2, x);
x = bt485i(Cmd4);
if(ctlr->flag & Uenhanced)
x |= 0x01;
else
x &= ~0x01;
bt485o(Cmd4, x);
x = bt485i(Cmd0) & ~0x01;
x &= ~0x02;
bt485o(Cmd0, x);
ctlr->flag |= Fload;
}
static void
dump(Vga*, Ctlr* ctlr)
{
int i;
printitem(ctlr->name, "direct");
for(i = 0; i < 0x10; i++)
printreg(bt485i(i));
printitem(ctlr->name, "indirect");
printreg(bt485i(Cmd3));
printreg(bt485i(Cmd4));
}
Ctlr bt485 = {
"bt485",
0,
options,
init,
load,
dump,
};