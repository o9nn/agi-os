#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum
{
X= 0x3D6,
D= 0x3D7,
};
static int misc[] = { 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0xE, 0x28, 0x29,
0x70, 0x72, 0x73, 0x7D, 0x7F, -1};
static int map[] = { 0x7, 0x8, 0xB, 0xC, 0x10, 0x11, -1};
static int flags[] = { 0xF, 0x2B, 0x44, 0x45, -1};
static int compat[] = { 0x14, 0x15, 0x1F, 0x7E, -1};
static int clock[] = { 0x30, 0x31, 0x32, 0x33, -1};
static int mm[] = { 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, -1};
static int alt[] = { 0x0D, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E,
0x24, 0x25, 0x26, 0x64, 0x65, 0x66, 0x67, -1};
static int flat[] = { 0x2C, 0x2D, 0x2E, 0x2F, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54,
0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E,
0x5F, 0x60, 0x61, 0x62, 0x63, 0x68, 0x6C, 0x6E, 0x6F, -1};
typedef struct Group Group;
struct Group {
char *name;
int *x;
};
static Group group[] =
{
{ "misc", misc, },
{ "map", map, },
{ "compatability", compat, },
{ "clock", clock, },
{ "multimedia", mm, },
{ "alternate", alt, },
{ "flat-panel", flat, },
{ 0 },
};
static uchar greg[256];
static uchar
ctxi(uchar index)
{
outportb(X, index);
return inportb(D);
}
static void
ctxo(uchar index, uchar data)
{
outportb(X, index);
outportb(D, data);
}
static void
snarf(Vga*, Ctlr* ctlr)
{
Group *g;
int *xp;
ctlr->flag |= Fsnarf;
for(g = group; g->name; g++)
for(xp = g->x; *xp >= 0; xp++)
greg[*xp] = ctxi(*xp);
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hlinear|Foptions;
}
static int
setclock(Vga* vga)
{
ulong fvco, t;
ulong m, n;
ulong bestm, bestn, diff, bestdiff, lastdiff;
ulong p;
if(vga->mode->frequency > 220000000)
return -1;
vga->misc &= ~(3<<2);
vga->feature &= ~3;
greg[0x33] &= ~0x20;
greg[0x33] &= ~0x80;
vga->misc |= (2<<2);
vga->feature |= 2;
fvco = vga->mode->frequency;
if(fvco == 0)
return -1;
p = 0;
while(fvco < 100000000){
fvco *= 2;
p++;
}
m = (1<<31)/(4*RefFreq);
if(m > 127)
m = 127;
bestdiff = 1<<31;
bestm = 3;
bestn = 3;
for(; m > 2; m--){
lastdiff = 1<<31;
for(n = 3; n < 128; n++){
t = (RefFreq*4*m)/n;
diff = abs(fvco-t);
if(diff < bestdiff){
bestdiff = diff;
bestm = m;
bestn = n;
} else {
if(diff > lastdiff)
break;
}
lastdiff = diff;
}
}
greg[0x31] = bestm - 2;
greg[0x32] = bestn - 2;
greg[0x30] = (p<<1) | 1;
return 0;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
int x;
greg[0x15] = 0;
if(vga->mode->z > 8)
error("depth %d not supported\n", vga->mode->z);
if(vga->mode->z == 8){
if(vga->linear && (ctlr->flag & Hlinear))
ctlr->flag |= Ulinear;
vga->vmz = 1024*1024;
vga->vmb = 1024*1024;
greg[0x04] = (1<<2);
greg[0x0b] = 0x15;
greg[0x28] = 0x90;
vga->sequencer[0x04] = 0x0A;
vga->graphics[0x05] = 0x00;
vga->attribute[0x10] &= ~(1<<6);
vga->crt[0x14] = 0x00;
vga->crt[0x17] = 0xe3;
} else {
greg[0x04] = (1<<2);
greg[0x0b] = 0x01;
greg[0x28] = 0x80;
}
x = 0;
if(vga->mode->vt & (1<<10))
x |= (1<<0);
if(vga->mode->vrs & (1<<10))
x |= (1<<2);
greg[0x16] = x;
x = 0;
if(vga->mode->ht & (1<<(8+3)))
x |= (1<<0);
if(vga->mode->shb & (1<<(8+3)))
x |= (1<<4);
if(vga->mode->ehb & (1<<(6+3)))
x |= (1<<5);
greg[0x17] = x;
if(vga->mode->y > 480)
vga->misc &= 0x3F;
setclock(vga);
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
Group *g;
int *xp;
ctxo(0x15, greg[0x15]);
ctxo(0x33, greg[0x33]);
if(ctlr->flag & Ulinear){
greg[0x8] = vga->vmb>>20;
ctxo(0x08, greg[0x08]);
}
for(g = group; g->name; g++)
for(xp = g->x; *xp >= 0; xp++)
ctxo(*xp, greg[*xp]);
ctlr->flag |= Fload;
}
static void
dump(Vga*, Ctlr* ctlr)
{
Group *g;
int *xp;
char *name;
int lastx;
char item[32];
name = ctlr->name;
for(g = group; g->name; g++){
lastx = -2;
for(xp = g->x; *xp >= 0; xp++){
if(*xp != lastx+1){
sprint(item, "%s %2.2ux:", g->name, *xp);
printitem(name, item);
}
lastx = *xp;
printreg(greg[*xp]);
}
}
}
Ctlr ct65540 = {
"ct65540",
snarf,
options,
init,
load,
dump,
};
Ctlr ct65545 = {
"ct65545",
snarf,
options,
init,
load,
dump,
};
Ctlr ct65545hwgc = {
"ct65545hwgc",
0,
0,
0,
0,
0,
};