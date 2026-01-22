#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
#define SCALE(f)	((f)/1)
static void
init(Vga* vga, Ctlr* ctlr)
{
int f;
ulong d, dmax, fmin, fvco, n, nmax, p;
if(ctlr->flag & Finit)
return;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
fmin = vga->f[0];
vga->d[0] = 6;
vga->n[0] = 5;
vga->p[0] = 2;
dmax = (RefFreq/1000000)-2;
for(d = 1; d < dmax; d++){
nmax = ((220000000+RefFreq)*(d+2))/(RefFreq*8) - 2;
for(n = 1; n < nmax; n++){
fvco = SCALE(RefFreq)*((n+2)*8)/(d+2);
if(fvco < SCALE(110000000) || fvco > SCALE(220000000))
continue;
for(p = 1; p < 4; p++){
f = SCALE(vga->f[0]) - (fvco>>p);
if(f < 0)
f = -f;
if(f < fmin){
fmin = f;
vga->d[0] = d;
vga->n[0] = n;
vga->p[0] = p;
}
}
}
}
ctlr->flag |= Finit;
}
Ctlr tvp3025clock = {
"tvp3025clock",
0,
0,
init,
0,
0,
};