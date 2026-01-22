#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int i;
vgaxo(Seqx, 0x08, 0x06);
for(i = 0x08; i < 0x19; i++)
vga->sequencer[i] = vgaxi(Seqx, i);
vga->crt[0x2D] = vgaxi(Crtx, 0x2D);
vga->crt[0x2E] = vgaxi(Crtx, 0x2E);
vga->crt[0x2F] = vgaxi(Crtx, 0x2F);
s3generic.snarf(vga, ctlr);
ctlr->flag |= Fsnarf;
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hlinear|Hpclk2x8|Henhanced|Foptions;
}
void
trio64clock(Vga* vga, Ctlr* ctlr)
{
int d;
ulong f, fmax, fmin, n, m, r;
double trouble;
for(r = 0; r <= vga->r[1]; r++){
f = vga->f[0]*(1<<r);
if(vga->f[1] < f && f <= vga->f[1]*2)
vga->r[0] = r;
}
if(vga->r[0] > vga->r[1])
error("%s: pclk %lud out of range\n", ctlr->name, vga->f[0]);
vga->d[0] = vga->f[0]+1;
for(n = 1; n <= vga->n[1]; n++){
trouble = vga->f[0]*(n+2)*(1<<vga->r[0]);
trouble /= RefFreq;
m = (trouble+0.5) - 2;
if(m > vga->m[1])
continue;
trouble = (m+2)*RefFreq;
trouble /= (n+2)*(1<<vga->r[0]);
f = trouble+0.5;
d = vga->f[0] - f;
if(d < 0)
d = -d;
if(d <= vga->d[0]){
vga->m[0] = m;
vga->n[0] = n;
vga->d[0] = d;
}
}
trouble = vga->f[0]*1.005;
fmax = trouble;
trouble = vga->f[0]*0.995;
fmin = trouble;
trouble = (vga->m[0]+2)*RefFreq;
trouble /= (vga->n[0]+2)*(1<<vga->r[0]);
f = trouble+0.5;
if(fmin >= f || f >= fmax)
error("%s: pclk %lud out of range\n", ctlr->name, vga->f[0]);
}
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong pclk, x;
s3generic.init(vga, ctlr);
if(vga->mode->z > 8)
error("depth %d not supported\n", vga->mode->z);
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
vga->misc &= ~0x0C;
if(vga->f[0] == VgaFreq0){
;
}
else if(vga->f[0] == VgaFreq1)
vga->misc |= 0x04;
else{
if((ctlr->flag & Hpclk2x8) && vga->mode->z == 8){
pclk = 135000000;
if(vga->f[0] > 80000000)
ctlr->flag |= Upclk2x8;
}
else
pclk = 80000000;
if(vga->f[0] > pclk)
error("%s: invalid pclk - %lud\n",
ctlr->name, vga->f[0]);
vga->f[1] = 135000000;
vga->r[1] = 3;
vga->n[1] = 31;
vga->m[1] = 127;
trio64clock(vga, ctlr);
vga->sequencer[0x12] = (vga->r[0]<<5)|vga->n[0];
vga->sequencer[0x13] = vga->m[0];
vga->misc |= 0x0C;
}
vga->sequencer[0x15] &= ~0x31;
vga->sequencer[0x15] |= 0x02;
vga->sequencer[0x18] &= ~0x80;
vga->crt[0x67] &= ~0xF2;
if(ctlr->flag & Upclk2x8){
vga->sequencer[0x15] |= 0x10;
vga->sequencer[0x18] |= 0x80;
vga->crt[0x67] |= 0x10;
}
if((vga->crt[0x36] & 0x03) == 0x01)
vga->crt[0x58] &= ~0x08;
x = vga->crt[0]-5;
vga->crt[0x3B] = x;
if(x & 0x100)
vga->crt[0x5D] |= 0x40;
vga->crt[0x60] = 0xFF;
if(vga->mode->x <= 800)
vga->crt[0x54] = 0xE8;
else if(vga->mode->x <= 1024)
vga->crt[0x54] = 0xA8;
else
vga->crt[0x54] = 0x00;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
ushort advfunc;
s3generic.load(vga, ctlr);
vgaxo(Crtx, 0x60, vga->crt[0x60]);
vgaxo(Crtx, 0x67, vga->crt[0x67]);
vgaxo(Seqx, 0x12, vga->sequencer[0x12]);
vgaxo(Seqx, 0x13, vga->sequencer[0x13]);
if((vga->misc & 0x0C) == 0x0C)
vgaxo(Seqx, 0x15, vga->sequencer[0x15]|0x20);
vgaxo(Seqx, 0x15, vga->sequencer[0x15]);
vgaxo(Seqx, 0x18, vga->sequencer[0x18]);
advfunc = 0x0000;
if(ctlr->flag & Uenhanced)
advfunc = 0x0001;
outportw(0x4AE8, advfunc);
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
ulong dclk, m, n, r;
s3generic.dump(vga, ctlr);
printitem(ctlr->name, "Seq08");
for(i = 0x08; i < 0x19; i++)
printreg(vga->sequencer[i]);
printitem(ctlr->name, "Crt2D");
printreg(vga->crt[0x2D]);
printreg(vga->crt[0x2E]);
printreg(vga->crt[0x2F]);
n = vga->sequencer[0x12] & 0x1F;
r = (vga->sequencer[0x12]>>5) & 0x03;
m = vga->sequencer[0x13] & 0x7F;
dclk = (m+2)*RefFreq;
dclk /= (n+2)*(1<<r);
printitem(ctlr->name, "dclk m n r");
Bprint(&stdout, "%9ld %8ld       - %8ld %8ld\n", dclk, m, n, r);
}
Ctlr trio64 = {
"trio64",
snarf,
options,
init,
load,
dump,
};