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
if(vga->ctlr == 0 || strncmp(vga->ctlr->name, "et4000-w32", 10))
cflag = 1;
if(vga->mode->z != 8 || (ctlr->flag & Upclk2x8))
cflag = 1;
}
Ctlr et4000hwgc = {
"et4000hwgc",
0,
0,
init,
0,
0,
};