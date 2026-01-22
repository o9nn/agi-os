#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Command		= 0x00,
IndexLO		= 0x01,
IndexHI		= 0x02,
Index		= 0x03,
CompanyID	= 0x00,
DeviceID	= 0x01,
Pmode		= 0x03,
Smode		= 0x04,
Pipeline	= 0x05,
Sreset		= 0x06,
Power		= 0x07,
Nindex		= 0x08,
};
static void
pixmask(void)
{
inportb(PaddrW);
}
static void
commandrw(void)
{
int i;
pixmask();
for(i = 0; i < 4; i++)
inportb(Pixmask);
}
static uchar
commandr(void)
{
uchar command;
commandrw();
command = inportb(Pixmask);
pixmask();
return command;
}
static void
commandw(uchar command)
{
commandrw();
outportb(Pixmask, command);
pixmask();
}
static void
indexrw(uchar index)
{
uchar command;
command = commandr();
commandw(command|0x10);
commandrw();
inportb(Pixmask);
outportb(Pixmask, index & 0xFF);
outportb(Pixmask, (index>>8) & 0xFF);
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hpclk2x8|Foptions;
}
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong pclk;
if(vga->ctlr && ((vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8))
pclk = 135000000;
else
pclk = 110000000;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] < 16000000 || vga->f[0] > pclk)
error("%s: invalid pclk - %ld\n", ctlr->name, vga->f[0]);
if(vga->ctlr && (vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8 && vga->f[0] >= 110000000){
vga->f[0] /= 2;
resyncinit(vga, ctlr, Upclk2x8, 0);
}
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
uchar command, mode, pipeline;
command = 0x00;
mode = 0x00;
pipeline = 0x02;
if(ctlr->flag & Upclk2x8){
command = 0x08;
mode = 0x05;
pipeline = 0x02;
if(vga->f[0] < 16000000)
pipeline = 0x00;
else if(vga->f[0] < 32000000)
pipeline = 0x01;
}
indexrw(Pmode);
outportb(Pixmask, mode);
outportb(Pixmask, mode);
outportb(Pixmask, pipeline);
sleep(1);
commandw(command);
ctlr->flag |= Fload;
}
static void
dump(Vga*, Ctlr* ctlr)
{
int i;
printitem(ctlr->name, "command");
printreg(commandr());
printitem(ctlr->name, "index");
indexrw(CompanyID);
for(i = 0; i < Nindex; i++)
printreg(inportb(Pixmask));
pixmask();
}
Ctlr stg1702 = {
"stg1702",
0,
options,
init,
load,
dump,
};