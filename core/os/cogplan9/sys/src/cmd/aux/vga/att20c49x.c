#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Cr0 = 0x00,
};
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong pclk;
char *p;
pclk = 55000000;
if(p = strrchr(ctlr->name, '-'))
pclk = strtoul(p+1, 0, 0) * 1000000;
if(vga->f == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] > pclk)
error("%s: invalid pclk - %ld\n", ctlr->name, vga->f[0]);
}
static void
load(Vga* vga, Ctlr* ctlr)
{
uchar mode, x;
if(ctlr->name[8] == '1'){
x = attdaci(Cr0);
attdaco(Cr0, x|0x04);
}
mode = 0x00;
if(vga->mode->z == 8 && ctlr->name[8] == '1' && 0)
mode |= 0x02;
attdaco(Cr0, mode);
}
static void
dump(Vga*, Ctlr* ctlr)
{
printitem(ctlr->name, "Cr0");
printreg(attdaci(Cr0));
}
Ctlr att20c490 = {
"att20c490",
0,
0,
init,
load,
dump,
};
Ctlr att20c491 = {
"att20c491",
0,
0,
init,
load,
dump,
};
Ctlr att20c492 = {
"att20c492",
0,
0,
init,
load,
dump,
};