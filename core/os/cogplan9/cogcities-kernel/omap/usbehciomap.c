#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/usb.h"
#include "../port/portusbehci.h"
#include "usbehci.h"
static Ctlr* ctlrs[Nhcis];
static void
ehcireset(Ctlr *ctlr)
{
Eopio *opio;
int i;
ilock(ctlr);
dprint("ehci %#p reset\n", ctlr->capio);
opio = ctlr->opio;
ehcirun(ctlr, 0);
if((ctlr->capio->capparms & C64) != 0){
dprint("ehci: 64 bits\n");
opio->seg = 0;
}
if(ehcidebugcapio != ctlr->capio){
opio->cmd |= Chcreset;
coherence();
for(i = 0; i < 100; i++){
if((opio->cmd & Chcreset) == 0)
break;
delay(1);
}
if(i == 100)
print("ehci %#p controller reset timed out\n", ctlr->capio);
}
opio->cmd &= ~Citcmask;
opio->cmd |= 1 << Citcshift;
coherence();
switch(opio->cmd & Cflsmask){
case Cfls1024:
ctlr->nframes = 1024;
break;
case Cfls512:
ctlr->nframes = 512;
break;
case Cfls256:
ctlr->nframes = 256;
break;
default:
panic("ehci: unknown fls %ld", opio->cmd & Cflsmask);
}
coherence();
dprint("ehci: %d frames\n", ctlr->nframes);
iunlock(ctlr);
}
static void
setdebug(Hci*, int d)
{
ehcidebug = d;
}
static void
shutdown(Hci *hp)
{
int i;
Ctlr *ctlr;
Eopio *opio;
ctlr = hp->aux;
ilock(ctlr);
opio = ctlr->opio;
opio->cmd |= Chcreset;
coherence();
for(i = 0; i < 100; i++){
if((opio->cmd & Chcreset) == 0)
break;
delay(1);
}
if(i >= 100)
print("ehci %#p controller reset timed out\n", ctlr->capio);
delay(100);
ehcirun(ctlr, 0);
opio->frbase = 0;
coherence();
iunlock(ctlr);
}
enum {
Control = 1<<31,
Write = 2<<22,
Read = 3<<22,
Portsh = 24,
Regaddrsh = 16,
Eregaddrsh = 8,
Funcctlreg = 4,
Ifcctlreg = 7,
Phystppullupoff = 0x90,
Phyrstport2 = 147,
};
static void
wrulpi(Eopio *opio, int port, int reg, uchar data)
{
opio->insn[5] = Control | port << Portsh | Write | reg << Regaddrsh |
data;
coherence();
while (!(opio->insn[5] & Control))
;
}
static int
reset(Hci *hp)
{
Ctlr *ctlr;
Ecapio *capio;
Eopio *opio;
Uhh *uhh;
static int beenhere;
if (beenhere)
return -1;
beenhere = 1;
if(getconf("*nousbehci") != nil || probeaddr(PHYSEHCI) < 0)
return -1;
ctlr = smalloc(sizeof(Ctlr));
ctlr->capio = capio = (Ecapio *)PHYSEHCI;
ctlr->opio = opio = (Eopio*)((uintptr)capio + (capio->cap & 0xff));
hp->aux = ctlr;
hp->port = (uintptr)ctlr->capio;
hp->irq = 77;
hp->nports = capio->parms & Cnports;
ddprint("echi: %s, ncc %lud npcc %lud\n",
capio->parms & 0x10000 ? "leds" : "no leds",
(capio->parms >> 12) & 0xf, (capio->parms >> 8) & 0xf);
ddprint("ehci: routing %s, %sport power ctl, %d ports\n",
capio->parms & 0x40 ? "explicit" : "automatic",
capio->parms & 0x10 ? "" : "no ", hp->nports);
ehcireset(ctlr);
ehcimeminit(ctlr);
opio->insn[4] |= 1<<5;
coherence();
uhh = (Uhh *)PHYSUHH;
if (uhh->hostconfig & P1ulpi_bypass) {
iprint("usbehci: bypassing ulpi on port 1!\n");
opio->insn[5] &= ~(MASK(4) << 13);
opio->insn[5] |= 1 << 13;
coherence();
} else {
wrulpi(opio, 1, Ifcctlreg, Phystppullupoff);
wrulpi(opio, 1, Funcctlreg, 0x40);
}
ehcilinkage(hp);
hp->shutdown = shutdown;
hp->debug = setdebug;
intrenable(78, hp->interrupt, hp, UNKNOWN, "usbtll");
intrenable(92, hp->interrupt, hp, UNKNOWN, "usb otg");
intrenable(93, hp->interrupt, hp, UNKNOWN, "usb otg dma");
return 0;
}
void
usbehcilink(void)
{
addhcitype("ehci", reset);
}