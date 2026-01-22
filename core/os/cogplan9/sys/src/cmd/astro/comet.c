#include "astro.h"
#define	MAXE	(.999)
void
comet(void)
{
double pturbl, pturbb, pturbr;
double lograd;
double dele, enom, vnom, nd, sl;
struct	elem
{
double	t;
double	q;
double	e;
double	i;
double	w;
double	o;
} elem;
elem=(struct elem)
{
etdate(2002, 3, 18.9784),
0.5070601,
0.990111,
28.12106,
34.6666,
93.1206,
};
ecc = elem.e;
if(ecc > MAXE)
ecc = MAXE;
incl = elem.i * radian;
node = (elem.o + 0.4593) * radian;
argp = (elem.w + elem.o + 0.4066) * radian;
mrad = elem.q / (1-ecc);
motion = .01720209895 * sqrt(1/(mrad*mrad*mrad))/radian;
anom = (eday - (elem.t - 2415020)) * motion * radian;
enom = anom + ecc*sin(anom);
do {
dele = (anom - enom + ecc * sin(enom)) /
(1 - ecc*cos(enom));
enom += dele;
} while(fabs(dele) > converge);
vnom = 2*atan2(
sqrt((1+ecc)/(1-ecc))*sin(enom/2),
cos(enom/2));
rad = mrad*(1-ecc*cos(enom));
lambda = vnom + argp;
pturbl = 0;
lambda += pturbl*radsec;
pturbb = 0;
pturbr = 0;
nd = lambda - node;
lambda = node + atan2(sin(nd)*cos(incl),cos(nd));
sl = sin(incl)*sin(nd) + pturbb*radsec;
beta = atan2(sl, sqrt(1-sl*sl));
lograd = pturbr*2.30258509;
rad *= 1 + lograd;
motion *= radian*mrad*mrad/(rad*rad);
semi = 0;
mag = 5.47 + 6.1/2.303*log(rad);
helio();
geo();
}