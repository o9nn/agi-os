#include <u.h>
#include <libc.h>
#include <bio.h>
#include "pci.h"
#include "vga.h"
enum {
Prescale	= 2,
NIndex		= 14,
};
static ulong index[NIndex] = {
50000000,
51000000,
53200000,
58500000,
60700000,
64400000,
66800000,
73500000,
75600000,
80900000,
83200000,
91500000,
100000000,
120000000,
};
static void
init(Vga* vga, Ctlr* ctlr)
{
int f;
ulong d, dmax, fmin, n;
if(ctlr->flag & Finit)
return;
if(vga->f[0] == 0)
vga->f[0] = vga->mode->frequency;
if(vga->mode->z > 8)
error("depth %d not supported\n", vga->mode->z);
for(vga->p[0] = 0; vga->f[0] <= 50000000; vga->p[0]++)
vga->f[0] <<= 1;
for(vga->i[0] = NIndex-1; vga->f[0] < index[vga->i[0]] && vga->i[0]; vga->i[0]--)
;
d = RefFreq/1000000 > 3 ? RefFreq/1000000: 3;
dmax = RefFreq/200000 < 129 ? RefFreq/200000: 129;
vga->d[0] = d;
vga->n[0] = 4;
for(fmin = vga->f[0]; d <= dmax; d++){
for(n = 4; n <= 130; n++){
f = vga->f[0] - (Prescale*RefFreq*n/d);
if(f < 0)
f = -f;
if(f < fmin){
fmin = f;
vga->d[0] = d;
vga->n[0] = n;
}
}
}
vga->f[0] = (Prescale*RefFreq*vga->n[0]/vga->d[0]);
vga->d[0] -= 2;
vga->n[0] -= 3;
ctlr->flag |= Finit;
}
Ctlr icd2061a = {
"icd2061a",
0,
0,
init,
0,
0,
};