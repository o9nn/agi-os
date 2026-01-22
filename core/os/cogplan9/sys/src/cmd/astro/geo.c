#include "astro.h"
void
geo(void)
{
double sel, saz, caz;
double f;
double sa, ca, sd;
lha = gst - alpha - wlong;
decl = delta;
sa = cos(decl)*sin(lha);
ca = cos(decl)*cos(lha) - erad*cos(glat)*sin(hp);
sd = sin(decl) - erad*sin(glat)*sin(hp);
lha = atan2(sa, ca);
decl2 = atan2(sd, sqrt(sa*sa+ca*ca));
f = sqrt(sa*sa+ca*ca+sd*sd);
semi2 = semi/f;
ra = gst - lha - wlong;
ra = pinorm(ra);
sel = sin(nlat)*sin(decl2) + cos(nlat)*cos(decl2)*cos(lha);
el = atan2(sel, pyth(sel));
saz = sin(lha)*cos(decl2);
caz = cos(nlat)*sin(decl2) - sin(nlat)*cos(decl2)*cos(lha);
az = pi + atan2(saz, -caz);
az /= radian;
el /= radian;
}