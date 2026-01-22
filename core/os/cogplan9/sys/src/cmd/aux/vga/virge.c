#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static uchar savage4mem[] = { 2, 4, 8, 12, 16, 32, 0, 32};
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int i, id;
char *p;
vgaxo(Seqx, 0x08, 0x06);
s3generic.snarf(vga, ctlr);
for(i = 0x08; i < 0x4F; i++)
vga->sequencer[i] = vgaxi(Seqx, i);
vga->crt[0x2D] = vgaxi(Crtx, 0x2D);
vga->crt[0x2E] = vgaxi(Crtx, 0x2E);
vga->crt[0x2F] = vgaxi(Crtx, 0x2F);
for(i = 0x70; i < 0x99; i++)
vga->crt[i] = vgaxi(Crtx, i);
id = (vga->crt[0x2D]<<8)|vga->crt[0x2E];
switch(id){
default:
trace("Unknown ViRGE/Trio64+ - 0x%4.4uX\n",
(vga->crt[0x2D]<<8)|vga->crt[0x2E]);
case 0x8810:
case 0x8811:
vga->r[1] = 3;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 135000000;
trace("Trio64+\n");
break;
case 0x8812:
vga->r[1] = 3;
vga->m[1] = 127;
vga->n[1] = 63;
vga->f[1] = 135000000;
trace("Aurora64V+\n");
break;
case 0x8901:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 170000000;
trace("Trio64V2\n");
break;
case 0x5631:
vga->r[1] = 3;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 135000000;
vga->apz = 64*1024*1024;
trace("ViRGE\n");
break;
case 0x8A01:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 170000000;
trace("ViRGE/[DG]X\n");
break;
case 0x8A10:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 170000000;
vga->apz = 64*1024*1024;
trace("ViRGE/GX2\n");
switch((vga->crt[0x36]>>6) & 0x03){
case 0x01:
vga->vmz = 4*1024*1024;
break;
case 0x03:
vga->vmz = 2*1024*1024;
break;
}
break;
case 0x883D:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 31;
vga->f[1] = 220000000;
vga->apz = 64*1024*1024;
trace("ViRGE/VX\n");
vga->vmz = (2*(((vga->crt[0x36]>>5) & 0x03)+1)) * 1*1024*1024;
break;
case 0x8C10:
case 0x8C12:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 127;
vga->f[1] = 135000000;
for(i = 0x50; i < 0x70; i++)
vga->sequencer[i] = vgaxi(Seqx, i);
vga->apz = 128*1024*1024;
vga->vmz = savage4mem[vga->crt[0x36]>>5] * 1024 * 1024;
trace("Savage4/IX-MV\n");
break;
case 0x8C2E:
vga->r[1] = 4;
vga->m[1] = 127;
vga->n[1] = 127;
vga->f[1] = 135000000;
for(i = 0x50; i < 0x70; i++)
vga->sequencer[i] = vgaxi(Seqx, i);
vga->apz = 64*1024*1024;
vga->vmz = savage4mem[vga->crt[0x36]>>5] * 1024 * 1024;
trace("SuperSavage/IXC16\n");
break;
case 0x8A22:
case 0x8A25:
case 0x8A26:
case 0x8D04:
vga->r[1] = 4;
vga->m[1] = 511;
vga->n[1] = 127;
vga->f[1] = 300000000;
vga->apz = 128*1024*1024;
vga->vmz = savage4mem[vga->crt[0x36]>>5] * 1024 * 1024;
trace("Savage4\n");
break;
}
if(p = strrchr(ctlr->name, '-'))
vga->f[1] = strtoul(p+1, 0, 0) * 1000000;
ctlr->flag |= Fsnarf;
}
static void
options(Vga *vga, Ctlr* ctlr)
{
int id;
id = (vga->crt[0x2D]<<8)|(vga->crt[0x2E]);
switch(id){
case 0x8C2E:
case 0x8C10:
case 0x8C12:
case 0x8A22:
case 0x8A25:
case 0x8A26:
case 0x8D04:
if(vga->virtx%16)
vga->virtx = (vga->virtx+15)&~15;
break;
}
ctlr->flag |= Hlinear|Hpclk2x8|Henhanced|Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
char *p, *val;
Mode *mode;
ulong pclk, x;
int id, noclockset, width;
id = (vga->crt[0x2D]<<8)|vga->crt[0x2E];
mode = vga->mode;
if((id == 0x8A01) && ((mode->z+7)/8 == 2)){
resyncinit(vga, ctlr, Uenhanced, 0);
vga->crt[0x00] = ((mode->ht * 2) >> 3) - 5;
vga->crt[0x01] = ((mode->x * 2) >> 3) - 1;
vga->crt[0x02] = ((mode->shb * 2) >> 3) - 1;
x = (mode->ehb * 2) >> 3;
vga->crt[0x03] = 0x80 | (x & 0x1F);
vga->crt[0x04] = (mode->shs * 2) >> 3;
vga->crt[0x05] = ((mode->ehs * 2) >> 3) & 0x1F;
if(x & 0x20)
vga->crt[0x05] |= 0x80;
}
s3generic.init(vga, ctlr);
noclockset = 0;
if(p = strrchr(ctlr->name, '-'))
vga->f[1] = strtoul(p+1, 0, 0) * 1000000;
pclk = vga->f[1];
if(vga->mode->z > 8)
width = vga->virtx*(vga->mode->z/8);
else
width = vga->virtx*(8/vga->mode->z);
switch(id){
case 0x8810:
case 0x8811:
if((ctlr->flag & Hpclk2x8) && vga->mode->z == 8){
if(vga->f[0] > 80000000)
ctlr->flag |= Upclk2x8;
}
else
pclk = 80000000;
vga->crt[0x67] &= ~0xF2;
if(ctlr->flag & Upclk2x8){
vga->sequencer[0x15] |= 0x10;
vga->sequencer[0x18] |= 0x80;
vga->crt[0x67] |= 0x10;
}
if((vga->crt[0x36] & 0x03) == 0x01)
vga->crt[0x58] &= ~0x08;
vga->crt[0x60] = 0xFF;
if(vga->mode->z > 8)
error("trio64: depth %d not supported\n", vga->mode->z);
break;
case 0x8901:
vga->crt[0x90] = 0;
vga->crt[0x91] = 0;
break;
case 0x8A10:
vga->crt[0x90] = 0;
vga->crt[0x31] |= 0x08;
vga->crt[0x13] = (width>>3) & 0xFF;
vga->crt[0x51] &= ~0x30;
vga->crt[0x51] |= (width>>7) & 0x30;
vga->crt[0x85] = 0x0F;
case 0x5631:
case 0x8A01:
if(id == 0x8A01){
x = mode->x * ((mode->z + 7) / 8);
x = (x + 7) / 8;
vga->crt[0x91] = x & 0xFF;
vga->crt[0x90] = (x >> 8) | 0x80;
}
case 0x883D:
vga->crt[0x60] &= 0x0F;
case 0x8812:
if(id == 0x8812)
noclockset = 1;
vga->crt[0x65] = 0;
vga->crt[0x66] = 0x89;
vga->crt[0x67] = 0;
if(id == 0x883D){
vga->crt[0x36] &= ~0x0C;
if(vga->mode->x > 800 && vga->mode->z == 8)
vga->crt[0x67] = 0x10;
else
vga->crt[0x67] = 0;
vga->crt[0x66] = 0x90;
vga->crt[0x58] &= ~0x88;
vga->crt[0x58] |= 0x40;
if(vga->mode->x > 640 && vga->mode->z >= 8)
vga->crt[0x63] |= 0x01;
else
vga->crt[0x63] &= ~0x01;
}
switch(vga->mode->z){
case 1:
case 2:
case 4:
case 8:
default:
vga->crt[0x67] |= 0x00;
break;
case 15:
vga->crt[0x67] |= 0x30;
break;
case 16:
vga->crt[0x67] |= 0x50;
break;
case 24:
if(id == 0x8A10)
vga->crt[0x67] |= 0x70;
else
vga->crt[0x67] |= 0xD0;
break;
case 32:
if(id != 0x8A10)
error("32-bit mode only supported on the GX/2\n");
vga->crt[0x67] |= 0xD0;
break;
}
vga->crt[0x53] &= ~0x18;
vga->crt[0x53] |= 0x08;
break;
case 0x8C2E:
case 0x8C10:
case 0x8C12:
x = width/8 ;
vga->crt[0x91] = x;
vga->crt[0x90] &= ~0x07;
vga->crt[0x90] |= (x>>8) & 0x07;
case 0x8D04:
x = mode->x * ((mode->z + 7) / 8);
x = (x + 7) / 8;
vga->crt[0x91] = x & 0xFF;
vga->crt[0x90] = (x >> 8) | 0x80;
case 0x8A22:
case 0x8A25:
case 0x8A26:
vga->crt[0x66] = 0x89;
vga->crt[0x67] = 0;
vga->crt[0x85] = 0x02;
vga->crt[0x31] |= 0x08;
vga->crt[0x13] = (width>>3) & 0xFF;
vga->crt[0x51] &= ~0x30;
vga->crt[0x51] |= (width>>7) & 0x30;
vga->crt[0x50] = 0xC1;
switch(vga->mode->z){
default:
error("%d-bit mode not supported on savage 4\n", vga->mode->z);
case 8:
vga->crt[0x67] |= 0x00;
vga->crt[0x50] |= 0<<4;
break;
case 15:
vga->crt[0x67] |= 0x20;
vga->crt[0x50] |= 1<<4;
break;
case 16:
vga->crt[0x67] |= 0x40;
vga->crt[0x50] |= 1<<4;
if(id == 0x8C12 || id == 0x8C2E || id == 0x8C10)
vga->crt[0x67] |= 0x10;
break;
case 32:
vga->crt[0x67] |= 0xD0;
vga->crt[0x50] |= 3<<4;
break;
}
break;
}
if(val = dbattr(vga->mode->attr, "noclockset")){
if((noclockset = strtol(val, &p, 0)) == 0 && p == val)
error("%s: invalid 'noclockset' attr\n", ctlr->name);
}
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
vga->misc &= ~0x0C;
if(vga->f[0] == VgaFreq0){
;
}
else if(vga->f[0] == VgaFreq1)
vga->misc |= 0x04;
else if(noclockset){
vga->misc |= 0x0C;
}
else{
if(vga->f[0] > pclk)
error("%s: invalid pclk - %lud\n",
ctlr->name, vga->f[0]);
trio64clock(vga, ctlr);
switch(id){
case 0x8A10:
vga->sequencer[0x12] = (vga->r[0]<<6)|vga->n[0];
if(vga->r[0] & 0x04)
vga->sequencer[0x29] |= 0x01;
else
vga->sequencer[0x29] &= ~0x01;
break;
case 0x8C2E:
case 0x8C10:
case 0x8C12:
case 0x8A22:
case 0x8A25:
case 0x8A26:
case 0x8D04:
vga->sequencer[0x12] = (vga->r[0]<<6)|(vga->n[0] & 0x3F);
vga->sequencer[0x39] &= ~0x01;
vga->sequencer[0x29] &= ~0x1C;
if(vga->r[0] & 0x04)
vga->sequencer[0x29] |= (1<<2);
if(vga->m[0] & 0x100)
vga->sequencer[0x29] |= (1<<3);
if(vga->n[0] & 0x40)
vga->sequencer[0x29] |= (1<<4);
break;
default:
vga->sequencer[0x12] = (vga->r[0]<<5)|vga->n[0];
break;
}
vga->sequencer[0x13] = vga->m[0];
vga->misc |= 0x0C;
}
vga->sequencer[0x15] &= ~0x31;
vga->sequencer[0x15] |= 0x02;
vga->sequencer[0x18] &= ~0x80;
x = (vga->crt[0]+vga->crt[4]+1)/2;
vga->crt[0x3B] = x;
if(x & 0x100)
vga->crt[0x5D] |= 0x40;
if(vga->mode->x <= 800)
vga->crt[0x54] = 0xE8;
else if(vga->mode->x <= 1024 && id != 0x8C12 && id != 0x8C2E)
vga->crt[0x54] = 0xA8;
else
vga->crt[0x54] = 0x00;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
int id;
ushort advfunc;
s3generic.load(vga, ctlr);
vgaxo(Seqx, 0x12, vga->sequencer[0x12]);
vgaxo(Seqx, 0x13, vga->sequencer[0x13]);
id = (vga->crt[0x2D]<<8)|vga->crt[0x2E];
switch(id){
case 0x883D:
vgaxo(Crtx, 0x36, vga->crt[0x36]);
break;
case 0x8A10:
vgaxo(Seqx, 0x29, vga->sequencer[0x29]);
break;
case 0x8C2E:
case 0x8C12:
vgaxo(Crtx, 0x90, vga->crt[0x90]);
vgaxo(Crtx, 0x91, vga->crt[0x91]);
case 0x8A22:
case 0x8A25:
case 0x8A26:
case 0x8D04:
vgaxo(Seqx, 0x29, vga->sequencer[0x29]);
vgaxo(Seqx, 0x39, vga->sequencer[0x39]);
break;
}
if((vga->misc & 0x0C) == 0x0C)
vgaxo(Seqx, 0x15, vga->sequencer[0x15]|0x20);
vgaxo(Seqx, 0x15, vga->sequencer[0x15]);
vgaxo(Seqx, 0x18, vga->sequencer[0x18]);
vgaxo(Crtx, 0x60, vga->crt[0x60]);
vgaxo(Crtx, 0x63, vga->crt[0x63]);
vgaxo(Crtx, 0x65, vga->crt[0x65]);
vgaxo(Crtx, 0x66, vga->crt[0x66]);
vgaxo(Crtx, 0x67, vga->crt[0x67]);
switch(id){
case 0x8810:
case 0x8811:
advfunc = 0x0000;
if(ctlr->flag & Uenhanced)
advfunc = 0x0001;
outportw(0x4AE8, advfunc);
break;
case 0x8901:
case 0x8A01:
vgaxo(Crtx, 0x90, vga->crt[0x90]);
vgaxo(Crtx, 0x91, vga->crt[0x91]);
break;
case 0x8A10:
vgaxo(Crtx, 0x90, vga->crt[0x90]);
vgaxo(Crtx, 0x31, vga->crt[0x31]);
vgaxo(Crtx, 0x13, vga->crt[0x13]);
vgaxo(Crtx, 0x51, vga->crt[0x51]);
vgaxo(Crtx, 0x85, vga->crt[0x85]);
break;
case 0x8D04:
vgaxo(Crtx, 0x90, vga->crt[0x90]);
vgaxo(Crtx, 0x91, vga->crt[0x91]);
case 0x8C2E:
case 0x8C12:
case 0x8A22:
case 0x8A25:
case 0x8A26:
vgaxo(Crtx, 0x31, vga->crt[0x31]);
vgaxo(Crtx, 0x13, vga->crt[0x13]);
vgaxo(Crtx, 0x51, vga->crt[0x51]);
vgaxo(Crtx, 0x85, vga->crt[0x85]);
vgaxo(Crtx, 0x50, vga->crt[0x50]);
break;
}
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i, id;
ulong dclk, m, n, r;
s3generic.dump(vga, ctlr);
printitem(ctlr->name, "Crt70");
for(i = 0x70; i < 0x99; i++)
printreg(vga->crt[i]);
printitem(ctlr->name, "Seq08");
for(i = 0x08; i < 0x10; i++)
printreg(vga->sequencer[i]);
printitem(ctlr->name, "Seq10");
for(i = 0x10; i < 0x50; i++)
printreg(vga->sequencer[i]);
id = (vga->crt[0x2D]<<8)|vga->crt[0x2E];
switch(id){
default:
break;
case 0x8812:
case 0x8C2E:
case 0x8C12:
printitem(ctlr->name, "Seq50");
for(i = 0x50; i < 0x70; i++)
printreg(vga->sequencer[i]);
break;
}
printitem(ctlr->name, "Crt2D");
printreg(vga->crt[0x2D]);
printreg(vga->crt[0x2E]);
printreg(vga->crt[0x2F]);
m = vga->sequencer[0x13] & vga->m[1];
n = vga->sequencer[0x12] & vga->n[1];
r = (vga->sequencer[0x12]>>5) & 0x03;
switch(id){
case 0x8812:
r = (vga->sequencer[0x12]>>6) & 0x03;
break;
case 0x8A01:
r = (vga->sequencer[0x12]>>5) & 0x07;
break;
case 0x8A10:
r = (vga->sequencer[0x12]>>6) & 0x03;
r |= (vga->sequencer[0x29] & 0x01)<<2;
break;
case 0x8C2E:
case 0x8C12:
case 0x8A22:
case 0x8A25:
case 0x8A26:
case 0x8D04:
m = vga->sequencer[0x13] & 0xFF;
if(vga->sequencer[0x29] & (1<<3))
m |= 0x100;
if(vga->sequencer[0x29] & (1<<4))
n |= 0x40;
r = (vga->sequencer[0x12]>>6) & 0x03;
r |= (vga->sequencer[0x29] & (1<<2));
break;
}
dclk = (m+2)*RefFreq;
dclk /= (n+2)*(1<<r);
printitem(ctlr->name, "dclk m n r");
Bprint(&stdout, "%9ld %8ld       - %8ld %8ld\n", dclk, m, n, r);
m = vga->sequencer[0x11] & 0x7F;
n = vga->sequencer[0x10] & 0x1F;
r = (vga->sequencer[0x10]>>5) & 0x03;
dclk = (m+2)*RefFreq;
dclk /= (n+2)*(1<<r);
printitem(ctlr->name, "mclk m n r");
Bprint(&stdout, "%9ld %8ld       - %8ld %8ld\n", dclk, m, n, r);
}
Ctlr virge = {
"virge",
snarf,
options,
init,
load,
dump,
};