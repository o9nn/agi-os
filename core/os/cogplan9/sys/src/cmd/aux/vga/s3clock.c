#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
static void
setcrt42(Vga* vga, Ctlr* ctlr, uchar index)
{
trace("%s->clock->setcrt42\n", ctlr->name);
vgao(MiscW, vga->misc & ~0x0C);
outportb(Crtx+1, 0x00);
vga->crt[0x42] = (vga->crt[0x42] & 0xF0)|index;
vgao(MiscW, vga->misc);
vgaxo(Crtx, 0x42, vga->crt[0x42]);
}
static void
icd2061aload(Vga* vga, Ctlr* ctlr)
{
ulong sdata;
int i;
uchar crt42;
trace("%s->clock->icd2061aload\n", ctlr->name);
sdata = (2<<21)|(vga->i[0]<<17)|((vga->n[0])<<10)|(vga->p[0]<<7)|vga->d[0];
vgao(MiscW, vga->misc & ~0x0C);
crt42 = vgaxi(Crtx, 0x42) & 0xF0;
outportb(Crtx+1, 0x00);
vgaxo(Seqx, 0x00, 0x00);
vgao(MiscW, vga->misc|0x0C);
vgaxo(Seqx, 0x00, 0x03);
outportb(Crtx+1, crt42|0x00);
outportb(Crtx+1, crt42|0x02);
for(i = 0; i < 5; i++){
outportb(Crtx+1, crt42|0x03);
outportb(Crtx+1, crt42|0x02);
}
outportb(Crtx+1, crt42|0x00);
outportb(Crtx+1, crt42|0x01);
outportb(Crtx+1, crt42|0x00);
outportb(Crtx+1, crt42|0x01);
for(i = 0; i < 24; i++){
if(sdata & 0x01){
outportb(Crtx+1, crt42|0x01);
outportb(Crtx+1, crt42|0x00);
outportb(Crtx+1, crt42|0x02);
outportb(Crtx+1, crt42|0x03);
}
else {
outportb(Crtx+1, crt42|0x03);
outportb(Crtx+1, crt42|0x02);
outportb(Crtx+1, crt42|0x00);
outportb(Crtx+1, crt42|0x01);
}
sdata >>= 1;
}
outportb(Crtx+1, crt42|0x03);
outportb(Crtx+1, crt42|0x02);
outportb(Crtx+1, crt42|0x03);
setcrt42(vga, ctlr, 0x02);
}
static void
ch9294load(Vga* vga, Ctlr* ctlr)
{
trace("%s->clock->ch9294load\n", ctlr->name);
setcrt42(vga, ctlr, vga->i[0]);
}
static void
tvp3025load(Vga* vga, Ctlr* ctlr)
{
uchar crt5c, x;
trace("%s->clock->tvp3025load\n", ctlr->name);
crt5c = vgaxi(Crtx, 0x5C);
vgaxo(Crtx, 0x5C, crt5c & ~0x20);
tvp3020xo(0x2C, 0x00);
tvp3020xo(0x2D, vga->d[0]);
tvp3020xo(0x2D, vga->n[0]);
tvp3020xo(0x2D, 0x08|vga->p[0]);
tvp3020xo(0x2F, 0x01);
tvp3020xo(0x2F, 0x01);
tvp3020xo(0x2F, vga->p[0]);
x = 0x54;
if(vga->ctlr && (vga->ctlr->flag & Uenhanced))
x = 0xC4;
tvp3020xo(0x1E, x);
vgaxo(Crtx, 0x5C, crt5c);
vgao(MiscW, vga->misc);
ctlr->flag |= Fload;
}
static void
tvp3026load(Vga* vga, Ctlr* ctlr)
{
trace("%s->clock->tvp3026load\n", ctlr->name);
if((vga->misc & 0x0C) != 0x0C && vga->mode->z == 1){
tvp3026xo(0x1A, 0x07);
tvp3026xo(0x18, 0x80);
tvp3026xo(0x19, 0x98);
tvp3026xo(0x2C, 0x2A);
tvp3026xo(0x2F, 0x00);
tvp3026xo(0x2D, 0x00);
tvp3026xo(0x39, 0x18);
setcrt42(vga, ctlr, 0);
}
else if(vga->mode->z == 8){
tvp3026xo(0x1A, 0x05);
tvp3026xo(0x18, 0x80);
tvp3026xo(0x19, 0x4C);
tvp3026xo(0x2C, 0x2A);
tvp3026xo(0x2F, 0x00);
tvp3026xo(0x2D, 0x00);
tvp3026xo(0x2C, 0x00);
tvp3026xo(0x2D, 0xC0|vga->n[0]);
tvp3026xo(0x2D, vga->m[0] & 0x3F);
tvp3026xo(0x2D, 0xB0|vga->p[0]);
while(!(tvp3026xi(0x2D) & 0x40))
;
tvp3026xo(0x39, 0x38|vga->q[1]);
tvp3026xo(0x2C, 0x00);
tvp3026xo(0x2F, 0xC0|vga->n[1]);
tvp3026xo(0x2F, vga->m[1]);
tvp3026xo(0x2F, 0xF0|vga->p[1]);
while(!(tvp3026xi(0x2F) & 0x40))
;
setcrt42(vga, ctlr, 3);
}
ctlr->flag |= Fload;
}
static struct {
char*	name;
void	(*load)(Vga*, Ctlr*);
} clocks[] = {
{ "icd2061a",		icd2061aload, },
{ "ch9294",		ch9294load, },
{ "tvp3025clock",	tvp3025load, },
{ "tvp3026clock",	tvp3026load, },
{ 0 },
};
static void
init(Vga* vga, Ctlr* ctlr)
{
char name[Namelen+1], *p;
int i;
if(vga->clock == 0)
return;
strncpy(name, vga->clock->name, Namelen);
name[Namelen] = 0;
if(p = strchr(name, '-'))
*p = 0;
for(i = 0; clocks[i].name; i++){
if(strcmp(clocks[i].name, name) == 0)
break;
}
if(clocks[i].name == 0)
error("%s: unknown clock \"%s\"\n", ctlr->name, vga->clock->name);
if(vga->clock->init && (vga->clock->flag & Finit) == 0)
(*vga->clock->init)(vga, vga->clock);
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->f[0] != VgaFreq0 && vga->f[0] != VgaFreq1)
vga->misc |= 0x0C;
ctlr->flag |= Finit;
}
static void
load(Vga* vga, Ctlr* ctlr)
{
char name[Namelen+1], *p;
int i;
if(vga->clock == 0 || (vga->clock->flag & Fload))
return;
strncpy(name, vga->clock->name, Namelen);
name[Namelen] = 0;
if(p = strchr(name, '-'))
*p = 0;
for(i = 0; clocks[i].name; i++){
if(strcmp(clocks[i].name, name))
continue;
clocks[i].load(vga, ctlr);
if(strcmp(clocks[i].name, "icd2061a") == 0){
clocks[i].load(vga, ctlr);
clocks[i].load(vga, ctlr);
}
ctlr->flag |= Fload;
return;
}
}
Ctlr s3clock = {
"s3clock",
0,
0,
init,
load,
0,
};