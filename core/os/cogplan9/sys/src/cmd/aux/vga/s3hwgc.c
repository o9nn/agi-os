#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static void
init(Vga* vga, Ctlr* ctlr)
{
ctlr->flag |= Finit;
if(cflag)
return;
if(vga->ctlr == 0 || (vga->ctlr->flag & Henhanced) == 0 || vga->mode->z < 8){
cflag = 1;
return;
}
resyncinit(vga, ctlr, Uenhanced, 0);
}
static void
load(Vga* vga, Ctlr* ctlr)
{
ctlr->flag |= Fload;
if(cflag)
return;
if(vga->ctlr == 0 || (vga->ctlr->flag & Uenhanced) == 0 || vga->mode->z < 8)
cflag = 1;
}
Ctlr bt485hwgc = {
"bt485hwgc",
0,
0,
0,
0,
0,
};
Ctlr rgb524hwgc = {
"rgb524hwgc",
0,
0,
0,
0,
0,
};
Ctlr s3hwgc = {
"s3hwgc",
0,
0,
init,
load,
0,
};
Ctlr tvp3020hwgc = {
"tvp3020hwgc",
0,
0,
0,
0,
0,
};
Ctlr tvp3026hwgc = {
"tvp3026hwgc",
0,
0,
0,
0,
0,
};