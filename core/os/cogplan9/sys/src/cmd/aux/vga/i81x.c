#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
typedef struct {
Pcidev* pci;
uchar* mmio;
ulong clk[6];
ulong lcd[9];
ulong pixconf;
} I81x;
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int f, i;
uchar *mmio;
ulong *rp;
Pcidev *p;
I81x *i81x;
if(vga->private == nil){
vga->private = alloc(sizeof(I81x));
p = nil;
while((p = pcimatch(p, 0x8086, 0)) != nil) {
switch(p->did) {
default:
continue;
case 0x7121:
case 0x7123:
case 0x7125:
case 0x1102:
case 0x1112:
case 0x1132:
case 0x3577:
vga->f[1] = 230000000;
break;
}
break;
}
if(p == nil)
error("%s: Intel 81x graphics function not found\n", ctlr->name);
if((f = open("#v/vgactl", OWRITE)) < 0)
error("%s: can't open vgactl\n", ctlr->name);
if(write(f, "type i81x", 9) != 9)
error("%s: can't set type\n", ctlr->name);
close(f);
mmio = segattach(0, "i81xmmio", 0, p->mem[1].size);
if(mmio == (void*)-1)
error("%s: can't attach mmio segment\n", ctlr->name);
i81x = vga->private;
i81x->pci = p;
i81x->mmio = mmio;
}
i81x = vga->private;
vga->vma = vga->vmz = i81x->pci->mem[0].size;
vga->apz = i81x->pci->mem[0].size;
ctlr->flag |= Hlinear;
vga->graphics[0x10] = vgaxi(Grx, 0x10);
vga->attribute[0x11] = vgaxi(Attrx, 0x11);
for(i=0; i < 0x19; i++)
vga->crt[i] = vgaxi(Crtx, i);
for(i=0x30; i <= 0x82; i++)
vga->crt[i] = vgaxi(Crtx, i);
rp = (ulong*)(i81x->mmio+0x06000);
for(i = 0; i < nelem(i81x->clk); i++)
i81x->clk[i] = *rp++;
rp = (ulong*)(i81x->mmio+0x60000);
for(i = 0; i < nelem(i81x->lcd); i++)
i81x->lcd[i] = *rp++;
rp = (ulong*)(i81x->mmio+0x70008);
i81x->pixconf = *rp;
ctlr->flag |= Fsnarf;
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hlinear|Foptions;
}
static void
i81xdclk(I81x *i81x, Vga *vga)
{
int m, n, post, mtp, ntp;
double md, freq, error;
freq = vga->mode->deffrequency/1000000.0;
if (freq == 0)
sysfatal("i81xdclk: deffrequency %d becomes freq 0.0",
vga->mode->deffrequency);
post = log(600.0/freq)/log(2.0);
for(ntp=3;;ntp++) {
md = freq*(1<<post)/(24.0/(double)ntp)/4.0;
mtp = (int)(md+0.5);
if(mtp<3) mtp=3;
error = 1.0-freq/(md/(ntp*(1<<post))*4*24.0);
if((fabs(error) < 0.001) || ((ntp > 30) && (fabs(error) < 0.005)))
break;
}
m = vga->m[1] = mtp-2;
n = vga->n[1] = ntp-2;
vga->r[1] = post;
i81x->clk[2] = ((n & 0x3FF)<<16) | (m & 0x3FF);
i81x->clk[4] = (i81x->clk[4] & ~0x700000) | ((post & 0x07)<<20);
vga->mode->frequency = (m+2)/((n+2)*(1<<post))*4*24*1000000;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
I81x *i81x;
int vt, vde, vrs, vre;
ulong *rp;
i81x = vga->private;
i81x->clk[0] = 0x00030013;
i81x->clk[1] = 0x00100053;
rp = (ulong*)i81x->mmio+0x6010;
i81x->clk[4] = *rp;
i81x->clk[4] |= 0x4040;
vga->misc = vgai(MiscR);
switch(vga->virtx) {
case 640:
vga->misc &= ~0x0A;
break;
case 720:
vga->misc = (vga->misc & ~0x08) | (1<<2);
break;
case 800:
case 1024:
case 1152:
case 1280:
case 1376:
vga->misc = vga->misc | (2<<2) & ~0x02;
i81xdclk(i81x, vga);
break;
default:
error("%s: Only 800, 1024, 1152, 1280, 1376 resolutions are supported\n", ctlr->name);
}
i81x->pixconf = (1<<12)|(1<<0);
i81x->pixconf &= 0xFFFFFBFF;
switch(vga->mode->z) {
case 8:
i81x->pixconf |= (2<<16);
break;
case 16:
i81x->pixconf |= (5<<16);
break;
case 24:
i81x->pixconf |= (6<<16);
break;
case 32:
i81x->pixconf |= (7<<16);
break;
default:
error("%s: depth %d not supported\n", ctlr->name, vga->mode->z);
}
vga->attribute[0x11] = 0;
if(vga->linear && (ctlr->flag & Hlinear)) {
vga->graphics[0x10] = 0x0A;
ctlr->flag |= Ulinear;
}
vt = vga->mode->vt;
vde = vga->virty;
vrs = vga->mode->vrs;
vre = vga->mode->vre+6;
if(vga->mode->interlace == 'v') {
vt /= 2;
vde /= 2;
vrs /= 2;
vre /= 2;
}
vga->crt[8] = 0;
vga->crt[9] = 0;
vga->crt[7] = 0;
vga->crt[0x18] = 0;
vga->crt[0x42] = vga->pci->mem[0].bar>>24 & 0xFF;
vga->crt[0x40] = vga->pci->mem[0].bar>>18 & 0x3F | 0x80;
vga->crt[0x0C] = vga->pci->mem[0].bar>>10 & 0xFF;
vga->crt[0x0D] = (vga->pci->mem[0].bar >>2 + 1)& 0xFF;
vga->crt[0x14] = 0x0;
vga->crt[0x17] = 0x80;
vga->crt[0x41] = (vga->crt[0x13]>>8) & 0x0F;
vga->crt[0] = ((vga->mode->ht>>3)-6) & 0xFF;
vga->crt[0x35] = vga->mode->ht>>12 & 0x01;
vga->crt[1] = (vga->virtx-1)>>3 & 0xFF;
vga->crt[2] = ((vga->mode->shb>>3)-1) & 0xFF;
vga->crt[3] = (vga->mode->shb - vga->virtx)>>3 & 0x1F;
vga->crt[5] = ((vga->mode->shb - vga->virtx)>>3 & 0x20) <<2;
vga->crt[0x39] = ((vga->mode->shb - vga->virtx)>>3 & 0x40) >>6;
vga->crt[4] = vga->mode->shb>>3 & 0xFF;
vga->crt[5] |= vga->mode->ehb>>3 & 0x1F;
vga->crt[6] = (vt - 2) & 0xFF;
vga->crt[0x30] = (vt - 2)>>8 & 0x0F;
vga->crt[0x11] = (vre - vrs - 2) & 0x0F;
vga->crt[0x16] = (vre - vrs) & 0xFF;
vga->crt[0x12] = (vde-1) & 0xFF;
vga->crt[0x31] = (vde-1)>>8 & 0x0f;
vga->crt[0x10] = (vrs-1) & 0xFF;
vga->crt[0x32] = (vrs-1)>>8 & 0x0F;
vga->crt[0x15] = vrs & 0xFF;
vga->crt[0x33] = vrs>>8 & 0x0F;
if(vga->mode->interlace == 'v')
vga->crt[0x70] = vrs | 0x80;
else
vga->crt[0x70] = 0;
vga->crt[0x80] = 1;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
int i;
ulong *rp;
I81x *i81x;
char *p;
i81x = vga->private;
vgaxo(Attrx, 0x11, vga->attribute[0x11]);
vgaxo(Crtx, 0x80, vga->crt[0x80]);
vgaxo(Grx, 0x10, vga->graphics[0x10]);
vgao(MiscW, vga->misc);
for(i=0; i <= 0x18; i++)
vgaxo(Crtx, i, vga->crt[i]);
for(i=0x30; i <= 0x82; i++)
vgaxo(Crtx, i, vga->crt[i]);
vga->crt[0x40] |= 0x80;
vgaxo(Crtx, 0x40, vga->crt[0x40]);
rp = (ulong*)(i81x->mmio+0x06000);
for(i=0; i < nelem(i81x->clk); i++)
*rp++ = i81x->clk[i];
rp = (ulong*)(i81x->mmio+0x60000);
for(i = 0; i < nelem(i81x->lcd); i++)
*rp++ = i81x->lcd[i];
rp = (ulong*)(i81x->mmio+0x70008);
*rp = i81x->pixconf | (1<<8);
p = (char*)(i81x->mmio+Pixmask);
*p = 0xff;
p = (char*)(i81x->mmio+PaddrW);
*p = 0x04;
p = (char*)(i81x->mmio+Pdata);
*p = 0xff;
*p = 0xff;
*p = 0xff;
*p = 0x00;
*p = 0x00;
*p = 0x00;
*rp = i81x->pixconf;
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
Pcidev *p;
I81x *i81x;
char *name;
name = ctlr->name;
i81x = vga->private;
printitem(name, "Crt30");
for(i = 0x30; i <= 0x39; i++)
printreg(vga->crt[i]);
printitem(name, "Crt40");
for(i = 0x40; i <= 0x42; i++)
printreg(vga->crt[i]);
printitem(name, "Crt70");
for(i = 0x70; i <= 0x79; i++)
printreg(vga->crt[i]);
printitem(name, "Crt80");
for(i = 0x80; i <= 0x82; i++)
printreg(vga->crt[i]);
printitem(name, "Graphics10");
for(i = 0x10; i <= 0x1f; i++)
printreg(vga->graphics[i]);
printitem(name, "clk");
for(i = 0; i < nelem(i81x->clk); i++)
printreg(i81x->clk[i]);
printitem(name, "lcd");
for(i = 0; i < nelem(i81x->lcd); i++)
printreg(i81x->lcd[i]);
printitem(name, "pixconf");
printreg(i81x->pixconf);
p = i81x->pci;
printitem(name, "mem[0]");
Bprint(&stdout, "base %lux size %d\n", p->mem[0].bar & ~0x0F, p->mem[0].size);
printitem(name, "mem[1]");
Bprint(&stdout, "base %lux size %d\n", p->mem[1].bar & ~0x0F, p->mem[1].size);
}
Ctlr i81x = {
"i81x",
snarf,
options,
init,
load,
dump,
};
Ctlr i81xhwgc = {
"i81xhwgc",
0,
0,
0,
0,
0,
};