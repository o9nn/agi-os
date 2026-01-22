#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
typedef struct {
Pcidev*	pci;
ulong	io;
uchar*	mmio;
ulong	ioreg[13];
ulong	g[25];
ulong	w[25];
} T2r4;
enum {
IntVcnt		= 0x20/4,
IntHcnt		= 0x24/4,
DbAdr		= 0x28/4,
DbPtch		= 0x2C/4,
CrtHac		= 0x30/4,
CrtHbl		= 0x34/4,
CrtHfp		= 0x38/4,
CrtHs		= 0x3C/4,
CrtVac		= 0x40/4,
CrtVbl		= 0x44/4,
CrtVfp		= 0x48/4,
CrtVs		= 0x4C/4,
CrtLcnt		= 0x50/4,
CrtZoom		= 0x54/4,
Crt1con		= 0x58/4,
Crt2con		= 0x5C/4,
DbAdr2		= 0x60/4,
};
enum {
Mw0Ctrl		= 0x00/4,
Mw0Ad		= 0x04/4,
Mw0Sz		= 0x08/4,
Mw0Org		= 0x10/4,
Mw0Mask		= 0x24/4,
Mw1Ctlr		= 0x28/4,
Mw1Ad		= 0x2C/4,
Mw1Sz		= 0x30/4,
Mw1Org		= 0x38/4,
Mw1Mask		= 0x4C/4,
MwcFcnt		= 0x50/4,
MwcFlsh		= 0x54/4,
YuvLi		= 0x58/4,
YuvLa		= 0x5C/4,
MwCtrl		= 0x60/4,
};
enum {
IndexLo		= 4,
IndexHi		= 5,
Data		= 6,
IndexCtl	= 7,
};
static uchar
_rgb524xi(Vga* vga, int index)
{
ulong *mmio;
mmio = (ulong*)((T2r4*)vga->private)->mmio;
mmio[IndexLo] = index & 0xFF;
mmio[IndexHi] = (index>>8) & 0xFF;
return mmio[Data];
}
static void
_rgb524xo(Vga* vga, int index, uchar data)
{
ulong *mmio;
mmio = (ulong*)((T2r4*)vga->private)->mmio;
mmio[IndexLo] = index & 0xFF;
mmio[IndexHi] = (index>>8) & 0xFF;
mmio[Data] = data;
}
static void
snarf(Vga* vga, Ctlr* ctlr)
{
ulong *mmio;
int f, i, x;
Pcidev *p;
T2r4 *t2r4;
ulong *rp;
if(vga->private == nil){
vga->private = alloc(sizeof(T2r4));
if((p = pcimatch(0, 0x105D, 0)) == nil)
error("%s: not found\n", ctlr->name);
switch(p->did){
case 0x5348:
break;
default:
error("%s: not found\n", ctlr->name);
}
if((f = open("#v/vgactl", OWRITE)) < 0)
error("%s: can't open vgactl\n", ctlr->name);
if(write(f, "type t2r4", 9) != 9)
error("%s: can't set type\n", ctlr->name);
close(f);
mmio = segattach(0, "t2r4mmio", 0, p->mem[4].size);
if(mmio == (void*)-1)
error("%s: can't attach mmio segment\n", ctlr->name);
t2r4 = vga->private;
t2r4->pci = p;
t2r4->io = p->mem[5].bar & ~0x0F;
t2r4->mmio = (uchar*)mmio;
}
t2r4 = vga->private;
for(i = 0; i < nelem(t2r4->ioreg); i++)
t2r4->ioreg[i] = inportl(t2r4->io+(i*4));
x = t2r4->ioreg[7] & 0xFF00001F;
outportl(t2r4->io+0x1C, x|0x00331F10);
x = t2r4->ioreg[8] & 0xFF0FFFFF;
outportl(t2r4->io+0x20, x|0x00100000);
x = inportl(t2r4->io+0x30) & 0xFF;
outportl(t2r4->io+0x30, x|0x82);
rp = (ulong*)t2r4->mmio;
for(i = 0; i < nelem(t2r4->g); i++)
t2r4->g[i] = *rp++;
rp = (ulong*)(t2r4->mmio+8192);
for(i = 0; i < nelem(t2r4->w); i++)
t2r4->w[i] = *rp++;
vga->vma = vga->vmz = t2r4->pci->mem[0].size;
ctlr->flag |= Hlinear;
if(vga->ramdac && strncmp(vga->ramdac->name, "rgb524mn", 8) == 0){
rgb524mnxi = _rgb524xi;
rgb524mnxo = _rgb524xo;
}
ctlr->flag |= Fsnarf;
}
static void
options(Vga* vga, Ctlr* ctlr)
{
USED(vga);
ctlr->flag |= Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
char *val;
T2r4 *t2r4;
int crtclocks, zoom;
t2r4 = vga->private;
crtclocks = 64/vga->mode->z;
zoom = 1;
if((val = dbattr(vga->mode->attr, "zoom")) && strtol(val, 0, 0))
zoom = 2;
t2r4->g[DbAdr] = 0;
switch(vga->mode->z){
case 8:
t2r4->g[DbPtch] = vga->mode->x*1;
break;
case 16:
t2r4->g[DbPtch] = vga->mode->x*2;
break;
case 32:
t2r4->g[DbPtch] = vga->mode->x*4;
break;
}
t2r4->g[CrtHac] = vga->mode->x/crtclocks;
t2r4->g[CrtHbl] = (vga->mode->ht-vga->mode->x)/crtclocks;
if(vga->mode->shs == 0)
vga->mode->shs = vga->mode->shb;
t2r4->g[CrtHfp] = (vga->mode->shs-vga->mode->x)/crtclocks;
if(vga->mode->ehs == 0)
vga->mode->ehs = vga->mode->ehb;
t2r4->g[CrtHs] = (vga->mode->ehs-vga->mode->shs)/crtclocks;
t2r4->g[CrtVac] = vga->mode->y * zoom;
t2r4->g[CrtVbl] = (vga->mode->vt-vga->mode->y) * zoom;
t2r4->g[CrtVfp] = (vga->mode->vrs-vga->mode->y) * zoom;
t2r4->g[CrtVs] = (vga->mode->vre-vga->mode->vrs) * zoom;
t2r4->g[CrtZoom] = 0;
t2r4->g[Crt1con] &= 0x00000100;
t2r4->g[Crt1con] |= 0x70;
t2r4->g[Crt2con] = 0x20000100;
if(zoom == 2)
t2r4->g[CrtZoom] = 1;
t2r4->w[Mw0Ctrl] = 0;
t2r4->w[Mw0Sz] = 0x0D;
t2r4->w[Mw0Org] = 0;
t2r4->w[Mw0Mask] = 0xFFFFFFFF;
if(vga->linear && (ctlr->flag & Hlinear))
ctlr->flag |= Ulinear;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
T2r4 *t2r4;
ulong *g, *w;
t2r4 = vga->private;
g = (ulong*)t2r4->mmio;
g[DbAdr] = t2r4->g[DbAdr];
g[DbPtch] = t2r4->g[DbPtch];
g[CrtHac] = t2r4->g[CrtHac];
g[CrtHbl] = t2r4->g[CrtHbl];
g[CrtHfp] = t2r4->g[CrtHfp];
g[CrtHs] = t2r4->g[CrtHs];
g[CrtVac] = t2r4->g[CrtVac];
g[CrtVbl] = t2r4->g[CrtVbl];
g[CrtVfp] = t2r4->g[CrtVfp];
g[CrtVs] = t2r4->g[CrtVs];
g[Crt1con] = t2r4->g[Crt1con];
g[Crt2con] = t2r4->g[Crt2con];
g[CrtZoom] = t2r4->g[CrtZoom];
w = (ulong*)(t2r4->mmio+8192);
w[Mw0Ctrl] = t2r4->w[Mw0Ctrl];
w[Mw0Sz] = t2r4->w[Mw0Sz];
w[Mw0Org] = t2r4->w[Mw0Org];
w[Mw0Mask] = t2r4->w[Mw0Mask];
if(t2r4->g[CrtZoom])
outportl(t2r4->io+0x30, 0xA2);
else
outportl(t2r4->io+0x30, 0x82);
outportl(t2r4->io+0x24, 0x211BF030);
sleep(500);
outportl(t2r4->io+0x24, 0xA11BF030);
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
T2r4 *t2r4;
t2r4 = vga->private;
Bprint(&stdout, "\n");
Bprint(&stdout, "%s ioreg\t\t%8.8luX\n", ctlr->name, t2r4->io);
Bprint(&stdout, "%s rbase_g\t%8.8luX\n", ctlr->name, t2r4->ioreg[0]);
Bprint(&stdout, "%s rbase_w\t%8.8luX\n", ctlr->name, t2r4->ioreg[1]);
Bprint(&stdout, "%s rbase_d\t%8.8luX\n", ctlr->name, t2r4->ioreg[2]);
Bprint(&stdout, "%s rbase_i\t%8.8luX\n", ctlr->name, t2r4->ioreg[4]);
Bprint(&stdout, "%s rbase_e\t%8.8luX\n", ctlr->name, t2r4->ioreg[5]);
Bprint(&stdout, "%s id\t\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[6]);
Bprint(&stdout, "%s config1\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[7]);
Bprint(&stdout, "%s config2\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[8]);
Bprint(&stdout, "%s sgram\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[9]);
Bprint(&stdout, "%s softsw\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[10]);
Bprint(&stdout, "%s ddc\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[11]);
Bprint(&stdout, "%s vgactl\t\t%8.8luX\n", ctlr->name, t2r4->ioreg[12]);
Bprint(&stdout, "\n");
for(i = 0; i < nelem(t2r4->g); i++)
Bprint(&stdout, "%s g reg0x%2.2uX\t\t%8.8luX\n",
ctlr->name, i*4, t2r4->g[i]);
Bprint(&stdout, "\n");
for(i = 0; i < nelem(t2r4->w); i++)
Bprint(&stdout, "%s w reg0x%2.2uX\t\t%8.8luX\n",
ctlr->name, i*4, t2r4->w[i]);
}
Ctlr t2r4 = {
"t2r4",
snarf,
options,
init,
load,
dump,
};
Ctlr t2r4hwgc = {
"t2r4hwgc",
0,
0,
0,
0,
0,
};