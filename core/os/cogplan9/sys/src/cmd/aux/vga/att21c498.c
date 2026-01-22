#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Cr0		= 0x00,
Midr		= 0x01,
Didr		= 0x02,
Rtest		= 0x03,
Gtest		= 0x04,
Btest		= 0x05,
Nir		= 0x06,
};
static void
attdacio(uchar reg)
{
int i;
inportb(PaddrW);
for(i = 0; i < 4+reg; i++)
inportb(Pixmask);
}
uchar
attdaci(uchar reg)
{
uchar r;
attdacio(reg);
r = inportb(Pixmask);
inportb(PaddrW);
return r;
}
void
attdaco(uchar reg, uchar data)
{
attdacio(reg);
outportb(Pixmask, data);
inportb(PaddrW);
}
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
if(vga->ctlr && ((vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8))
pclk = grade;
else{
if(grade == 110000000)
pclk = 80000000;
else
pclk = 110000000;
}
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->ctlr && (vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8 && vga->f[0] > 80000000){
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
x = attdaci(Cr0);
attdaco(Cr0, x|0x04);
mode = 0x00;
if(ctlr->flag & Upclk2x8)
mode = 0x20;
if(vga->mode->z == 8 && 0)
mode |= 0x02;
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
Ctlr att21c498 = {
"att21c498",
0,
options,
init,
load,
dump,
};