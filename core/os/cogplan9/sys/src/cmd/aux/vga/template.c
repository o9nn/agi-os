#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static void
snarf(Vga* vga, Ctlr* ctlr)
{
USED(vga);
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
USED(vga);
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
USED(vga);
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
USED(vga, ctlr);
}
Ctlr xxx = {
"xxx",
snarf,
options,
init,
load,
dump,
};