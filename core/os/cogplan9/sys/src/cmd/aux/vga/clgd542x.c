#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
typedef struct {
uchar id;
ulong vclk;
} Gd542x;
static Gd542x family[] = {
{ 0x88, 75000000, },
{ 0x8C, 80000000, },
{ 0x94, 80000000, },
{ 0x90, 80000000, },
{ 0x98, 80000000, },
{ 0x9C, 86000000, },
{ 0xA0, 86000000, },
{ 0xA8, 86000000, },
{ 0xAC, 135000000, },
{ 0xB8, 135000000, },
{ 0xBC, 135000000, },
{ 0x30, 80000000, },
{ 0x00, },
};
static Gd542x*
identify(Vga* vga, Ctlr* ctlr)
{
Gd542x *gd542x;
uchar id;
id = vga->crt[0x27] & ~0x03;
for(gd542x = &family[0]; gd542x->id; gd542x++){
if(gd542x->id == id)
return gd542x;
}
error("%s: unknown chip id - 0x%2.2X\n", ctlr->name, vga->crt[0x27]);
return 0;
}
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int i;
Gd542x *gd542x;
vgaxo(Seqx, 0x06, 0x12);
for(i = 0x06; i < 0x20; i++)
vga->sequencer[i] = vgaxi(Seqx, i);
for(i = 0x09; i < 0x3A; i++)
vga->graphics[i] = vgaxi(Grx, i);
for(i = 0x19; i < 0x1E; i++)
vga->crt[i] = vgaxi(Crtx, i);
vga->crt[0x27] = vgaxi(Crtx, 0x27);
for(i = 0; i < 4; i++)
vgai(Pixmask);
vga->crt[0x28] = vgai(Pixmask);
i = 0;
switch(vga->crt[0x27] & ~0x03){
case 0x88:
case 0x8C:
case 0x94:
case 0x80:
case 0x90:
case 0x98:
case 0x9C:
i = (vga->sequencer[0x0A]>>3) & 0x03;
break;
case 0xA0:
case 0xA8:
case 0xAC:
case 0xB8:
case 0x30:
i = (vga->sequencer[0x0F]>>3) & 0x03;
if(vga->sequencer[0x0F] & 0x80)
i++;
if(((vga->sequencer[0x17]>>3) & 0x07) == 0x04)
ctlr->flag |= Hlinear;
break;
case 0xBC:
i = 2;
if((vga->sequencer[0x0F] & 0x18) == 0x18){
i <<= 1;
if(vga->sequencer[0x0F] & 0x80)
i <<= 2;
}
if(vga->sequencer[0x17] & 0x80)
i <<= 1;
ctlr->flag |= Hlinear;
break;
default:
break;
}
if(vga->linear && (ctlr->flag & Hlinear)){
vga->vmz = 16*1024*1024;
vga->vma = 16*1024*1024;
ctlr->flag |= Ulinear;
}
else
vga->vmz = (256<<i)*1024;
gd542x = identify(vga, ctlr);
if(vga->f[1] == 0 || vga->f[1] > gd542x->vclk)
vga->f[1] = gd542x->vclk;
ctlr->flag |= Fsnarf;
}
void
clgd54xxclock(Vga* vga, Ctlr* ctlr)
{
int f;
ulong d, dmin, fmin, n, nmin, p;
trace("%s->init->clgd54xxclock\n", ctlr->name);
fmin = vga->f[0];
nmin = 69;
dmin = 24;
if(vga->f[0] >= 40000000)
p = 0;
else
p = 1;
for(n = 1; n < 128; n++){
for(d = 1; d < 32; d++){
f = vga->f[0] - (RefFreq*n)/(d*(1+p));
if(f < 0)
f = -f;
if(f <= fmin){
fmin = f;
nmin = n;
dmin = d;
}
}
}
vga->f[0] = (RefFreq*nmin)/(dmin*(1+p));
vga->d[0] = dmin;
vga->n[0] = nmin;
vga->p[0] = p;
}
void
init(Vga* vga, Ctlr* ctlr)
{
Mode *mode;
Gd542x *gd542x;
ushort x;
mode = vga->mode;
gd542x = identify(vga, ctlr);
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] > gd542x->vclk)
error("%s: pclk %lud too high (> %lud)\n",
ctlr->name, vga->f[0], gd542x->vclk);
if(mode->z > 8)
error("%s: depth %d not supported\n", ctlr->name, mode->z);
clgd54xxclock(vga, ctlr);
vga->misc |= 0x0C;
vga->sequencer[0x0E] = vga->n[0];
vga->sequencer[0x1E] = (vga->d[0]<<1)|vga->p[0];
vga->sequencer[0x07] = 0x00;
if(mode->z == 8)
vga->sequencer[0x07] |= 0x01;
if(vga->f[0] >= 42000000)
vga->sequencer[0x0F] |= 0x20;
else
vga->sequencer[0x0F] &= ~0x20;
vga->sequencer[0x16] = (vga->sequencer[0x16] & 0xF0)|0x08;
vga->crt[0x1A] = 0x00;
x = mode->ehb>>3;
if(x & 0x40)
vga->crt[0x1A] |= 0x10;
if(x & 0x80)
vga->crt[0x1A] |= 0x20;
if(vga->crt[0x16] & 0x100)
vga->crt[0x1A] |= 0x40;
if(vga->crt[0x16] & 0x200)
vga->crt[0x1A] |= 0x80;
vga->crt[0x1B] = 0x22;
if(vga->crt[0x13] & 0x100)
vga->crt[0x1B] |= 0x10;
vga->graphics[0x0B] = 0x00;
if(vga->vmz > 1024*1024)
vga->graphics[0x0B] |= 0x20;
if(mode->interlace == 'v'){
vga->crt[0x19] = vga->crt[0x00]/2;
vga->crt[0x1A] |= 0x01;
}
}
static void
load(Vga* vga, Ctlr* ctlr)
{
vgaxo(Seqx, 0x0E, vga->sequencer[0x0E]);
vgaxo(Seqx, 0x1E, vga->sequencer[0x1E]);
if(ctlr->flag & Ulinear)
vga->sequencer[0x07] |= 0xE0;
vgaxo(Seqx, 0x07, vga->sequencer[0x07]);
vgaxo(Seqx, 0x0F, vga->sequencer[0x0F]);
vgaxo(Seqx, 0x16, vga->sequencer[0x16]);
if(vga->mode->interlace == 'v')
vgaxo(Crtx, 0x19, vga->crt[0x19]);
vgaxo(Crtx, 0x1A, vga->crt[0x1A]);
vgaxo(Crtx, 0x1B, vga->crt[0x1B]);
vgaxo(Grx, 0x0B, vga->graphics[0x0B]);
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
char *name;
name = ctlr->name;
printitem(name, "Seq06");
for(i = 0x06; i < 0x20; i++)
printreg(vga->sequencer[i]);
printitem(name, "Crt19");
for(i = 0x19; i < 0x1E; i++)
printreg(vga->crt[i]);
printitem(name, "Gr09");
for(i = 0x09; i < 0x3A; i++)
printreg(vga->graphics[i]);
printitem(name, "Id Hdr");
printreg(vga->crt[0x27]);
printreg(vga->crt[0x28]);
}
Ctlr clgd542x = {
"clgd542x",
snarf,
0,
init,
load,
dump,
};
Ctlr clgd542xhwgc = {
"clgd542xhwgc",
0,
0,
0,
0,
0,
};