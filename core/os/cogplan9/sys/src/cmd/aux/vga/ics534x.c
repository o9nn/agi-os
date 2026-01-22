#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static uchar
setrs2(Vga* vga, Ctlr* ctlr)
{
uchar rs2;
rs2 = 0;
if(strncmp(vga->ctlr->name, "et4000-w32", 10) == 0){
rs2 = vgaxi(Crtx, 0x31);
vgaxo(Crtx, 0x31, 0x40|rs2);
}
else if(strncmp(vga->ctlr->name, "ark2000pv", 9) == 0){
rs2 = vgaxi(Seqx, 0x1C);
vgaxo(Seqx, 0x1C, 0x80|rs2);
}
else
error("%s: not configured for %s\n", vga->ctlr->name, ctlr->name);
return rs2;
}
static void
restorers2(Vga* vga, uchar rs2)
{
if(strncmp(vga->ctlr->name, "et4000-w32", 10) == 0)
vgaxo(Crtx, 0x31, rs2);
else if(strncmp(vga->ctlr->name, "ark2000pv", 9) == 0)
vgaxo(Seqx, 0x1C, rs2);
}
static void
options(Vga*, Ctlr* ctlr)
{
ctlr->flag |= Hpclk2x8|Foptions;
}
static void
clock(Vga* vga, Ctlr* ctlr)
{
ulong f, m, n, r;
double fmin, fmax, t, tok;
if(ctlr->flag & Upclk2x8)
vga->r[0] = 2;
else{
vga->r[0] = 4;
for(r = 0; r <= 3; r++){
f = vga->f[0]*(1<<r);
if(60000000 < f && f <= 270000000)
vga->r[0] = r;
}
if(vga->r[0] > 3)
error("%s: pclk %lud out of range\n",
ctlr->name, vga->f[0]);
}
fmin = vga->f[0]*0.995;
fmax = vga->f[0]*1.005;
tok = 0.0;
for(n = 31; n >= 1; n--){
t = RefFreq/(n+2);
if(600000 >= t || t > 8000000)
continue;
t = vga->f[0]*(n+2)*(1<<vga->r[0]);
t /= RefFreq;
m = (t+0.5) - 2;
if(m > 127)
continue;
t = (m+2)*RefFreq;
t /= (n+2)*(1<<vga->r[0]);
if(fmin <= t && t < fmax){
vga->m[0] = m;
vga->n[0] = n;
tok = t;
}
}
if(tok == 0.0)
error("%s: pclk %lud out of range\n", ctlr->name, vga->f[0]);
}
static void
init(Vga* vga, Ctlr* ctlr)
{
ulong pclk;
char *p;
pclk = 80000000;
if(p = strrchr(ctlr->name, '-'))
pclk = strtoul(p+1, 0, 0) * 1000000;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] > pclk)
error("%s: invalid pclk - %ld\n", ctlr->name, vga->f[0]);
if(vga->ctlr && (vga->ctlr->flag & Hpclk2x8) && vga->mode->z == 8 && vga->f[0] >= pclk/2){
vga->f[0] /= 2;
resyncinit(vga, ctlr, Upclk2x8, 0);
}
vga->misc &= ~0x0C;
if(vga->f[0] == VgaFreq0)
vga->i[0] = 0;
else if(vga->f[0] == VgaFreq1){
vga->misc |= 0x04;
vga->i[0] = 1;
}
else{
clock(vga, ctlr);
vga->i[0] = 0x07;
}
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
uchar rs2, mode, pll;
rs2 = setrs2(vga, ctlr);
mode = 0x00;
outportb(Pixmask, 0x01);
if(ctlr->flag & Upclk2x8)
mode = 0x10;
outportb(PaddrR, 0x0E);
pll = inportb(Pdata) & 0x10;
if(vga->i[0] == 0x07){
outportb(PaddrW, vga->i[0]);
outportb(Pdata, vga->m[0]);
outportb(Pdata, (vga->r[0]<<5)|vga->n[0]);
pll |= 0x27;
}
outportb(PaddrW, 0x0E);
outportb(Pdata, pll);
outportb(Pixmask, mode);
restorers2(vga, rs2);
ctlr->flag |= Fload;
}
static void
dump(Vga* vga, Ctlr* ctlr)
{
int i;
uchar rs2, m, n;
char buf[32];
ulong f;
rs2 = setrs2(vga, ctlr);
printitem(ctlr->name, "command");
printreg(inportb(Pixmask));
outportb(PaddrR, 0x00);
for(i = 0; i < 0x0E; i++){
sprint(buf, "f%X m n", i);
printitem(ctlr->name, buf);
m = inportb(Pdata);
printreg(m);
n = inportb(Pdata);
printreg(n);
f = 14318180*(m+2);
f /= (n & 0x1F)+2;
f /= 1<<((n>>5) & 0x03);
Bprint(&stdout, "%12lud", f);
}
printitem(ctlr->name, "control");
printreg(inportb(Pdata));
restorers2(vga, rs2);
}
Ctlr ics534x = {
"ics534x",
0,
options,
init,
load,
dump,
};