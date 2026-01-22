#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Advfunc		= 0x4AE8,
Clocksel	= 0x4AEE,
Misc		= 0x36EE,
Membndry	= 0x42EE,
Memcfg		= 0x5EEE,
};
typedef struct {
ushort	advfunc;
ushort	clocksel;
ushort	misc;
ushort	membndry;
ushort	memcfg;
} Mach32;
typedef struct {
ulong	frequency;
uchar	b8;
uchar	b9;
uchar	be;
uchar	misc;
} Clock;
static Clock clocks[] = {
{  VgaFreq0,	0x40, 0x02, 0x00, 0x00, },
{  32000000,	0x00, 0x00, 0x10, 0x04, },
{  40000000,	0x00, 0x02, 0x10, 0x00, },
{  44900000,	0x00, 0x02, 0x00, 0x0C, },
{  65000000,	0x00, 0x02, 0x10, 0x0C, },
{  75000000,	0x00, 0x02, 0x10, 0x08, },
{	  0, },
};
static ulong atix;
static uchar
atixi(uchar index)
{
outportb(atix, index);
return inportb(atix+1);
}
static void
atixo(uchar index, uchar data)
{
outportw(atix, (data<<8)|index);
}
static void
atixinit(Vga* vga, Ctlr*)
{
uchar b;
Mach32 *mach32;
atix = 0x1CE;
if((b = atixi(0xB8)) & 0x3F)
atixo(0xB8, b & 0xC0);
b = atixi(0xAB);
atixo(0xAB, b & ~0x18);
atixo(0xB4, 0x00);
b = atixi(0xB9);
atixo(0xB9, b & ~0x80);
b = atixi(0xBE);
atixo(0xBE, b|0x09);
if(vga->private == 0)
vga->private = alloc(sizeof(mach32));
}
static void
snarf(Vga* vga, Ctlr* ctlr)
{
int i;
Mach32 *mach32;
atixinit(vga, ctlr);
for(i = 0xA0; i < 0xC0; i++)
vga->crt[i] = atixi(i);
mach32 = vga->private;
mach32->advfunc = inportw(Advfunc);
mach32->clocksel = inportw(Clocksel);
mach32->misc = inportw(Misc);
mach32->membndry = inportw(Membndry);
mach32->memcfg = inportw(Memcfg);
switch((mach32->misc>>2) & 0x03){
case 0:
vga->vmz = 512*1024;
break;
case 1:
vga->vmz = 1024*1024;
break;
case 2:
vga->vmz = 2*1024*1024;
break;
case 3:
vga->vmz = 4*1024*1024;
break;
}
ctlr->flag |= Fsnarf;
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
Clock *clockp;
Mode *mode;
mode = vga->mode;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
for(clockp = clocks; clockp->frequency; clockp++){
if(clockp->frequency > vga->f[0]+100000)
continue;
if(clockp->frequency > vga->f[0]-100000)
break;
}
if(clockp->frequency == 0)
error("%s: no suitable clock for %lud\n",
ctlr->name, vga->f[0]);
vga->crt[0xB0] &= 0xDA;
vga->crt[0xB1] &= 0x87;
vga->crt[0xB5] &= 0x7E;
vga->crt[0xB6] &= 0xE2;
vga->crt[0xB3] &= 0xAF;
vga->crt[0xA6] &= 0xFE;
vga->crt[0xA7] &= 0xF4;
if(mode->z == 8){
vga->graphics[0x05] = 0x00;
vga->attribute[0x10] &= ~0x40;
vga->crt[0x13] = (mode->x/8)/2;
vga->crt[0x14] = 0x00;
vga->crt[0x17] = 0xE3;
vga->crt[0xB0] |= 0x20;
vga->crt[0xB6] |= 0x04;
}
vga->attribute[0x11] = 0x00;
vga->crt[0xB6] |= 0x01;
vga->crt[0xBE] &= ~0x04;
vga->crt[0xB9] &= 0xFD;
vga->crt[0xB8] &= 0x3F;
vga->crt[0xBE] &= 0xE5;
vga->crt[0xB8] |= clockp->b8;
vga->crt[0xB9] |= clockp->b9;
vga->crt[0xBE] |= clockp->be;
vga->misc |= clockp->misc;
if(vga->mode->interlace == 'v')
vga->crt[0xBE] |= 0x02;
vga->crt[0xBD] &= ~0x04;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
ushort x;
outportw(Clocksel, 0x0000);
x = inportw(Memcfg) & ~0x0003;
outportw(Memcfg, x);
outportw(Membndry, 0x0000);
atixo(0xB0, vga->crt[0xB0]);
atixo(0xB1, vga->crt[0xB1]);
atixo(0xB5, vga->crt[0xB5]);
atixo(0xB6, vga->crt[0xB6]);
atixo(0xB3, vga->crt[0xB3]);
atixo(0xA6, vga->crt[0xA6]);
atixo(0xA7, vga->crt[0xA7]);
atixo(0xB8, vga->crt[0xB8]);
atixo(0xB9, vga->crt[0xB9]);
atixo(0xBE, vga->crt[0xBE]);
vgao(MiscW, vga->misc);
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
Mach32 *mach32;
printitem(ctlr->name, "ATIX");
for(i = 0xA0; i < 0xC0; i++)
printreg(vga->crt[i]);
if((mach32 = vga->private) == 0)
return;
printitem(ctlr->name, "ADVFUNC");
Bprint(&stdout, "%.4ux\n", mach32->advfunc);
printitem(ctlr->name, "CLOCKSEL");
Bprint(&stdout, "%.4ux\n", mach32->clocksel);
printitem(ctlr->name, "MISC");
Bprint(&stdout, "%.4ux\n", mach32->misc);
printitem(ctlr->name, "MEMBNDRY");
Bprint(&stdout, "%.4ux\n", mach32->membndry);
printitem(ctlr->name, "MEMCFG");
Bprint(&stdout, "%.4ux\n", mach32->memcfg);
}
Ctlr mach32 = {
"mach32",
snarf,
options,
init,
load,
dump,
};