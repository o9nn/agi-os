#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
typedef struct {
uchar	old[3];
int	pcr;
int	x;
int	y;
} Cyber938x;
static void
pcrrw(void)
{
int i;
inportb(PaddrW);
for(i = 0; i < 4; i++)
inportb(Pixmask);
}
static int
pcrr(void)
{
int pcr;
pcrrw();
pcr = inportb(Pixmask);
inportb(PaddrW);
return pcr;
}
static void
pcrw(int pcr)
{
pcrrw();
outportb(Pixmask, pcr);
inportb(PaddrW);
}
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int i;
Cyber938x *cyber;
if(vga->private == nil)
vga->private = alloc(sizeof(Cyber938x));
cyber = vga->private;
vgaxo(Seqx, 0x0B, 0);
cyber->old[0] = vgaxi(Seqx, 0x0C);
cyber->old[1] = vgaxi(Seqx, 0x0D);
cyber->old[2] = vgaxi(Seqx, 0x0E);
vga->sequencer[0x0B] = vgaxi(Seqx, 0x0B);
cyber->pcr = pcrr();
for(i = 0x08; i < 0x20; i++){
if(i == 0x0B)
continue;
vga->sequencer[i] = vgaxi(Seqx, i);
}
for(i = 0x19; i < 0x60; i++)
vga->crt[i] = vgaxi(Crtx, i);
vga->crt[0xCF] = vgaxi(Crtx, 0xCF);
for(i = 0x09; i < 0x60; i++)
vga->graphics[i] = vgaxi(Grx, i);
vga->vma = 4*1024*1024;
switch(vga->crt[0x1F] & 0x07){
case 0:
case 4:
vga->vma = 8*1024*1024;
vga->vmz = 8*1024*1024;
break;
case 1:
case 5:
vga->vmz = 4*1024*1024;
break;
case 2:
case 6:
vga->vmz = 768*1024;
break;
case 7:
vga->vmz = 2*1024*1024;
break;
case 3:
default:
vga->vmz = 1024*1024;
break;
}
switch((vga->graphics[0x52]>>4) & 0x03){
case 0:
cyber->x = 1280;
cyber->y = 1024;
break;
case 1:
cyber->x = 640;
cyber->y = 480;
break;
case 2:
cyber->x = 1024;
cyber->y = 768;
break;
case 3:
cyber->x = 800;
cyber->y = 600;
break;
}
ctlr->flag |= Fsnarf;
}
static void
options(Vga* vga, Ctlr* ctlr)
{
USED(vga);
ctlr->flag |= Hlinear|Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
Cyber938x *cyber;
cyber = vga->private;
vga->crt[0x1E] = 0x80;
vga->crt[0x2A] |= 0x40;
if(vga->linear && (ctlr->flag & Hlinear))
ctlr->flag |= Ulinear;
vga->graphics[0x31] &= 0x8F;
if(vga->mode->y > 768)
vga->graphics[0x31] |= 0x30;
else if(vga->mode->y > 600)
vga->graphics[0x31] |= 0x20;
else if(vga->mode->y > 480)
vga->graphics[0x31] |= 0x10;
switch(vga->mode->z){
case 8:
cyber->pcr = 0;
vga->crt[0x38] = 0x00;
break;
case 16:
cyber->pcr = 0x30;
vga->crt[0x38] = 0x05;
vga->crt[0x29] |= 0x10;
break;
case 24:
cyber->pcr = 0xD0;
vga->crt[0x38] = 0x29;
break;
default:
error("depth %d not supported\n", vga->mode->z);
}
switch(vga->sequencer[0x09]){
default:
error("unknown Cyber revision 0x%uX\n", vga->sequencer[0x09]);
break;
case 0x21:
vga->attribute[0x11] = 0;
vga->graphics[0x0F] |= 0x07;
break;
case 0x42:
if(!(vga->graphics[0x42] & 0x80)
&& (vga->graphics[0x43] & 0x20)){
if(vga->mode->x == 640)
vga->graphics[0x30] |= 0x81;
else
vga->graphics[0x30] &= ~0x81;
}
case 0x3:
case 0x33:
case 0x34:
case 0x35:
case 0x3A:
vga->graphics[0x0F] |= 0x07;
break;
case 0x41:
vga->graphics[0x0F] |= 0x17;
break;
case 0x4A:
case 0x5D:
case 0x6A:
case 0x7A:
vga->crt[0x2F] = 0x3F;
vga->graphics[0x0F] |= 0x17;
if(vga->mode->x == 1024)
vga->graphics[0x30] = 0x18;
break;
}
vga->graphics[0x52] &= ~0x01;
vga->graphics[0x53] &= ~0x01;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
Cyber938x *cyber;
cyber = vga->private;
outportw(0x3C4, 0x000B);
outportb(0x3C4, 0x0B);
inportb(0x3C5);
pcrw(cyber->pcr);
outportw(0x3C4, (0xC2 << 8) | 0x0E);
vgaxo(Grx, 0x52, vga->graphics[0x52]);
vgaxo(Grx, 0x53, vga->graphics[0x53]);
vgaxo(Grx, 0x30, vga->graphics[0x30]);
vgaxo(Grx, 0x31, vga->graphics[0x31]);
vgaxo(Crtx, 0x1E, vga->crt[0x1E]);
if(ctlr->flag & Ulinear)
vga->crt[0x21] |= 0x20;
vgaxo(Crtx, 0x21, vga->crt[0x21]);
vgaxo(Crtx, 0x29, vga->crt[0x29]);
vgaxo(Crtx, 0x2A, vga->crt[0x2A]);
vgaxo(Crtx, 0x2F, vga->crt[0x2F]);
vgaxo(Crtx, 0x38, vga->crt[0x38]);
vgaxo(Grx, 0x0F, vga->graphics[0x0F]);
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
char *name;
Cyber938x *cyber;
int i, k, m, n, vclka, vclkb;
name = ctlr->name;
cyber = vga->private;
printitem(name, "Seq08");
for(i = 0x08; i < 0x20; i++)
printreg(vga->sequencer[i]);
printitem(name, "Old Seq0C");
for(i = 0; i < 3; i++)
printreg(cyber->old[i]);
printitem(name, "pcr");
printreg(cyber->pcr);
printitem(name, "Crt19");
for(i = 0x19; i < 0x20; i++)
printreg(vga->crt[i]);
printitem(name, "Crt20");
for(i = 0x20; i < 0x60; i++)
printreg(vga->crt[i]);
printitem(name, "CrtCF");
printreg(vga->crt[0xCF]);
printitem(name, "Gr09");
for(i = 0x09; i < 0x10; i++)
printreg(vga->graphics[i]);
printitem(name, "Gr10");
for(i = 0x10; i < 0x60; i++)
printreg(vga->graphics[i]);
if(vga->sequencer[0x09] >= 0x4A){
vclka = vga->sequencer[0x18];
vclkb = vga->sequencer[0x19];
}
else{
vclka = inportb(0x43C8);
vclkb = inportb(0x43C9);
printitem(name, "mclk");
printreg(inportb(0x43C6));
printreg(inportb(0x43C7));
}
printitem(name, "vclk");
printreg(vclka);
printreg(vclkb);
k = vclkb>>6;
m = vclkb & 0x3F;
n = vclka;
Bprint(&stdout, "%18d\n", (((n+8)*RefFreq)*(1<<k))/(m+2));
printitem(name, "lcd size");
Bprint(&stdout, "%9ud %8ud\n", cyber->x, cyber->y);
}
Ctlr cyber938x = {
"cyber938x",
snarf,
options,
init,
load,
dump,
};
Ctlr cyber938xhwgc = {
"cyber938xhwgc",
0,
0,
0,
0,
0,
};