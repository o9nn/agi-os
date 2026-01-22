#include <u.h>
#include <libc.h>
#include "map.h"
#define TFUZZ 0.00001
static struct place tpole[4];
static double tpoleinit[4][2] = {
1.,	0.,
1.,	180.,
-1.,	90.,
-1.,	-90.
};
static struct tproj {
double tlat,tlon;
double ttwist;
double trot;
struct place projpl;
struct coord projtw;
struct coord postrot;
} tproj[4][4] = {
{	{0.},
{90.,	0.,	90.,	-90.},
{0.,	45.,	-45.,	150.},
{0.,	-45.,	-135.,	30.}
},
{	{90.,	0.,	-90.,	90.},
{0.},
{0.,	135.,	-135.,	-150.},
{0.,	-135.,	-45.,	-30.}
},
{	{0.,	45.,	135.,	-30.},
{0.,	135.,	45.,	-150.},
{0.},
{-90.,	0.,	180.,	90.}
},
{	{0.,	-45.,	45.,	-150.},
{0.,	-135.,	135.,	-30.},
{-90.,	0.,	0.,	90.},
{0.}
}};
static double tx[4] = {
0.,	0.,	-1.,	1.
};
static double ty[4] = {
0.,	2.,	-1.,	-1.
};
static double root3;
static double rt3inv;
static double two_rt3;
static double tkc,tk,tcon;
static double f0r,f0i,fpir,fpii;
static void
twhichp(struct place *g, int *p, int *q)
{
int i,j,k;
double cosdist[4];
struct place *tp;
for(i=0;i<4;i++) {
tp = &tpole[i];
cosdist[i] = g->nlat.s*tp->nlat.s +
g->nlat.c*tp->nlat.c*(
g->wlon.s*tp->wlon.s +
g->wlon.c*tp->wlon.c);
}
j = 0;
for(i=1;i<4;i++)
if(cosdist[i] > cosdist[j])
j = i;
*p = j;
k = j==0?1:0;
for(i=0;i<4;i++)
if(i!=j&&cosdist[i]>cosdist[k])
k = i;
*q = k;
}
int
Xtetra(struct place *place, double *x, double *y)
{
int i,j;
struct place pl;
register struct tproj *tpp;
double vr, vi;
double br, bi;
double zr,zi,z2r,z2i,z4r,z4i,sr,si,tr,ti;
twhichp(place,&i,&j);
copyplace(place,&pl);
norm(&pl,&tproj[i][j].projpl,&tproj[i][j].projtw);
Xstereographic(&pl,&vr,&vi);
zr = vr/2;
zi = vi/2;
if(zr<=TFUZZ)
zr = TFUZZ;
csq(zr,zi,&z2r,&z2i);
csq(z2r,z2i,&z4r,&z4i);
z2r *= two_rt3;
z2i *= two_rt3;
cdiv(z4r+z2r-1,z4i+z2i,z4r-z2r-1,z4i-z2i,&sr,&si);
csqrt(sr-1,si,&tr,&ti);
cdiv(tcon*tr,tcon*ti,root3+1-sr,-si,&br,&bi);
if(br<0) {
br = -br;
bi = -bi;
if(!elco2(br,bi,tk,1.,1.,&vr,&vi))
return 0;
vr = fpir - vr;
vi = fpii - vi;
} else
if(!elco2(br,bi,tk,1.,1.,&vr,&vi))
return 0;
if(si>=0) {
tr = f0r - vi;
ti = f0i + vr;
} else {
tr = f0r + vi;
ti = f0i - vr;
}
tpp = &tproj[i][j];
*x = tr*tpp->postrot.c +
ti*tpp->postrot.s + tx[i];
*y = ti*tpp->postrot.c -
tr*tpp->postrot.s + ty[i];
return(1);
}
int
tetracut(struct place *g, struct place *og, double *cutlon)
{
int i,j,k;
if((g->nlat.s<=-rt3inv&&og->nlat.s<=-rt3inv) &&
(ckcut(g,og,*cutlon=0.)==2||ckcut(g,og,*cutlon=PI)==2))
return(2);
twhichp(g,&i,&k);
twhichp(og,&j,&k);
if(i==j||i==0||j==0)
return(1);
return(0);
}
proj
tetra(void)
{
register i;
int j;
register struct place *tp;
register struct tproj *tpp;
double t;
root3 = sqrt(3.);
rt3inv = 1/root3;
two_rt3 = 2*root3;
tkc = sqrt(.5-.25*root3);
tk = sqrt(.5+.25*root3);
tcon = 2*sqrt(root3);
elco2(tcon/(root3-1),0.,tkc,1.,1.,&f0r,&f0i);
elco2(1.e15,0.,tk,1.,1.,&fpir,&fpii);
fpir *= 2;
fpii *= 2;
for(i=0;i<4;i++) {
tx[i] *= f0r*root3;
ty[i] *= f0r;
tp = &tpole[i];
t = tp->nlat.s = tpoleinit[i][0]/root3;
tp->nlat.c = sqrt(1 - t*t);
tp->nlat.l = atan2(tp->nlat.s,tp->nlat.c);
deg2rad(tpoleinit[i][1],&tp->wlon);
for(j=0;j<4;j++) {
tpp = &tproj[i][j];
latlon(tpp->tlat,tpp->tlon,&tpp->projpl);
deg2rad(tpp->ttwist,&tpp->projtw);
deg2rad(tpp->trot,&tpp->postrot);
}
}
return(Xtetra);
}