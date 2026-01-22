#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "util.h"
#include "l3side.h"
#include "quantize.h"
#include "reservoir.h"
#include "quantize_pvt.h"
#include "lame-analysis.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#undef MAXQUANTERROR
typedef union {
float f;
int i;
} fi_union;
#define MAGIC_FLOAT (65536*(128))
#define MAGIC_INT 0x4b000000
#ifdef TAKEHIRO_IEEE754_HACK
#ifdef MAXQUANTERROR
#define DUFFBLOCK() do { \
xp = xr34[0] * sfpow34_p1; \
xe = xr34[0] * sfpow34_eq; \
xm = xr34[0] * sfpow34_m1; \
if (xm > IXMAX_VAL) \
return -1; \
xp += MAGIC_FLOAT; \
xe += MAGIC_FLOAT; \
xm += MAGIC_FLOAT; \
fi[0].f = xp; \
fi[1].f = xe; \
fi[2].f = xm; \
fi[0].f = xp + (adj43asm - MAGIC_INT)[fi[0].i]; \
fi[1].f = xe + (adj43asm - MAGIC_INT)[fi[1].i]; \
fi[2].f = xm + (adj43asm - MAGIC_INT)[fi[2].i]; \
fi[0].i -= MAGIC_INT; \
fi[1].i -= MAGIC_INT; \
fi[2].i -= MAGIC_INT; \
x0 = fabs(xr[0]); \
xp = x0 - pow43[fi[0].i] * sfpow_p1; \
xe = x0 - pow43[fi[1].i] * sfpow_eq; \
xm = x0 - pow43[fi[2].i] * sfpow_m1; \
xp *= xp; \
xe *= xe; \
xm *= xm; \
xfsf_eq = Max(xfsf_eq, xe); \
xfsf_p1 = Max(xfsf_p1, xp); \
xfsf_m1 = Max(xfsf_m1, xm); \
++xr; \
++xr34; \
} while(0)
#else
#define DUFFBLOCK() do { \
xp = xr34[0] * sfpow34_p1; \
xe = xr34[0] * sfpow34_eq; \
xm = xr34[0] * sfpow34_m1; \
if (xm > IXMAX_VAL) \
return -1; \
xp += MAGIC_FLOAT; \
xe += MAGIC_FLOAT; \
xm += MAGIC_FLOAT; \
fi[0].f = xp; \
fi[1].f = xe; \
fi[2].f = xm; \
fi[0].f = xp + (adj43asm - MAGIC_INT)[fi[0].i]; \
fi[1].f = xe + (adj43asm - MAGIC_INT)[fi[1].i]; \
fi[2].f = xm + (adj43asm - MAGIC_INT)[fi[2].i]; \
fi[0].i -= MAGIC_INT; \
fi[1].i -= MAGIC_INT; \
fi[2].i -= MAGIC_INT; \
x0 = fabs(xr[0]); \
xp = x0 - pow43[fi[0].i] * sfpow_p1; \
xe = x0 - pow43[fi[1].i] * sfpow_eq; \
xm = x0 - pow43[fi[2].i] * sfpow_m1; \
xfsf_p1 += xp * xp; \
xfsf_eq += xe * xe; \
xfsf_m1 += xm * xm; \
++xr; \
++xr34; \
} while(0)
#endif
#else
# define QUANTFAC(rx) adj43[rx]
# define ROUNDFAC 0.4054
# define XRPOW_FTOI(src,dest) ((dest) = (int)(src))
#endif
static FLOAT8
calc_sfb_noise(const FLOAT8 *xr, const FLOAT8 *xr34, const int bw, const int sf)
{
int j;
fi_union fi;
FLOAT8 temp;
FLOAT8 xfsf=0;
FLOAT8 sfpow,sfpow34;
sfpow = POW20(sf+210);
sfpow34 = IPOW20(sf+210);
for ( j=0; j < bw ; ++j) {
#if 0
int ix;
if (xr34[j]*sfpow34 > IXMAX_VAL) return -1;
ix=floor( xr34[j]*sfpow34);
temp = fabs(xr[j])- pow43[ix]*sfpow;
temp *= temp;
if (ix < IXMAX_VAL) {
temp2 = fabs(xr[j])- pow43[ix+1]*sfpow;
temp2 *=temp2;
if (temp2<temp) {
temp=temp2;
++ix;
}
}
#else
if (xr34[j]*sfpow34 > IXMAX_VAL) return -1;
#ifdef TAKEHIRO_IEEE754_HACK
temp = sfpow34*xr34[j];
temp += MAGIC_FLOAT;
fi.f = temp;
fi.f = temp + (adj43asm - MAGIC_INT)[fi.i];
fi.i -= MAGIC_INT;
#else
temp = xr34[j]*sfpow34;
XRPOW_FTOI(temp, fi.i);
XRPOW_FTOI(temp + QUANTFAC(fi.i), fi.i);
#endif
temp = fabs(xr[j])- pow43[fi.i]*sfpow;
temp *= temp;
#endif
#ifdef MAXQUANTERROR
xfsf = Max(xfsf,temp);
#else
xfsf += temp;
#endif
}
#ifdef MAXQUANTERROR
return xfsf;
#else
return xfsf;
#endif
}
static FLOAT8
calc_sfb_noise_ave(const FLOAT8 *xr, const FLOAT8 *xr34, const int bw, const int sf)
{
double xp;
double xe;
double xm;
#ifdef TAKEHIRO_IEEE754_HACK
double x0;
#endif
int xx[3], j;
fi_union *fi = (fi_union *)xx;
FLOAT8 sfpow34_eq, sfpow34_p1, sfpow34_m1;
FLOAT8 sfpow_eq, sfpow_p1, sfpow_m1;
FLOAT8 xfsf_eq = 0, xfsf_p1 = 0, xfsf_m1 = 0;
sfpow_eq = POW20(sf + 210);
sfpow_m1 = sfpow_eq * .8408964153;
sfpow_p1 = sfpow_eq * 1.189207115;
sfpow34_eq = IPOW20(sf + 210);
sfpow34_m1 = sfpow34_eq * 1.13878863476;
sfpow34_p1 = sfpow34_eq * 0.878126080187;
#ifdef TAKEHIRO_IEEE754_HACK
j = (bw+3) / 4;
switch (bw % 4) {
default:
case 0: do{ DUFFBLOCK();
case 3: DUFFBLOCK();
case 2: DUFFBLOCK();
case 1: DUFFBLOCK(); } while (--j);
}
#else
for (j = 0; j < bw; ++j) {
if (xr34[j]*sfpow34_m1 > IXMAX_VAL) return -1;
xe = xr34[j]*sfpow34_eq;
XRPOW_FTOI(xe, fi[0].i);
XRPOW_FTOI(xe + QUANTFAC(fi[0].i), fi[0].i);
xe = fabs(xr[j])- pow43[fi[0].i]*sfpow_eq;
xe *= xe;
xp = xr34[j]*sfpow34_p1;
XRPOW_FTOI(xp, fi[0].i);
XRPOW_FTOI(xp + QUANTFAC(fi[0].i), fi[0].i);
xp = fabs(xr[j])- pow43[fi[0].i]*sfpow_p1;
xp *= xp;
xm = xr34[j]*sfpow34_m1;
XRPOW_FTOI(xm, fi[0].i);
XRPOW_FTOI(xm + QUANTFAC(fi[0].i), fi[0].i);
xm = fabs(xr[j])- pow43[fi[0].i]*sfpow_m1;
xm *= xm;
#ifdef MAXQUANTERROR
xfsf_eq = Max(xfsf,xm);
xfsf_p1 = Max(xfsf_p1,xp);
xfsf_m1 = Max(xfsf_m1,xm);
#else
xfsf_eq += xe;
xfsf_p1 += xp;
xfsf_m1 += xm;
#endif
}
#endif
if (xfsf_eq < xfsf_p1)
xfsf_eq = xfsf_p1;
if (xfsf_eq < xfsf_m1)
xfsf_eq = xfsf_m1;
#ifdef MAXQUANTERROR
return xfsf_eq;
#else
return xfsf_eq;
#endif
}
static int
find_scalefac(const FLOAT8 *xr, const FLOAT8 *xr34, const int sfb,
const FLOAT8 l3_xmin, const int bw)
{
FLOAT8 xfsf;
int i,sf,sf_ok,delsf;
sf = -82;
delsf = 128;
sf_ok=10000;
for (i=0; i<7; i++) {
delsf /= 2;
xfsf = calc_sfb_noise(xr,xr34,bw,sf);
if (xfsf < 0) {
sf += delsf;
}else{
if (sf_ok==10000) sf_ok=sf;
if (xfsf > l3_xmin) {
sf -= delsf;
}else{
sf_ok = sf;
sf += delsf;
}
}
}
assert(sf_ok!=10000);
#if 0
assert(delsf==1);
#endif
return sf;
}
static int
find_scalefac_ave(const FLOAT8 *xr, const FLOAT8 *xr34, const int sfb,
const FLOAT8 l3_xmin, const int bw)
{
FLOAT8 xfsf;
int i,sf,sf_ok,delsf;
sf = -82;
delsf = 128;
sf_ok=10000;
for (i=0; i<7; i++) {
delsf /= 2;
xfsf = calc_sfb_noise_ave(xr,xr34,bw,sf);
if (xfsf < 0) {
sf += delsf;
}else{
if (sf_ok==10000) sf_ok=sf;
if (xfsf > l3_xmin) {
sf -= delsf;
}else{
sf_ok = sf;
sf += delsf;
}
}
}
assert(sf_ok!=10000);
#if 0
assert(delsf==1);
#endif
return sf;
}
static const int max_range_short[SBPSY_s] =
{15, 15, 15, 15, 15, 15, 7, 7, 7, 7, 7, 7 };
static const int max_range_long[SBPSY_l] =
{15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7};
static const int max_range_short_lsf[SBPSY_s] =
{15, 15, 15, 15, 15, 15, 7, 7, 7, 7, 7, 7 };
static const int max_range_long_lsf[SBPSY_l] =
{15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7};
static const int max_range_long_lsf_pretab[SBPSY_l] =
{ 7,7,7,7,7,7, 3,3,3,3,3, 0,0,0,0, 0,0,0, 0,0,0 };
static int
compute_scalefacs_short_lsf (
int sf[SBPSY_s][3],gr_info *cod_info, int scalefac[SBPSY_s][3],int sbg[3])
{
int maxrange, maxrange1, maxrange2, maxover;
int sfb, i;
int ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
maxover = 0;
maxrange1 = max_range_short_lsf[0];
maxrange2 = max_range_short_lsf[6];
for (i=0; i<3; ++i) {
int maxsf1 = 0, maxsf2 = 0, minsf = 1000;
for (sfb = 0; sfb < SBPSY_s; sfb++) {
if (sfb < 6) {
if (maxsf1 < -sf[sfb][i])
maxsf1 = -sf[sfb][i];
} else {
if (maxsf2 < -sf[sfb][i])
maxsf2 = -sf[sfb][i];
}
if (minsf > -sf[sfb][i])
minsf = -sf[sfb][i];
}
maxsf1 = Max (maxsf1-maxrange1*ifqstep, maxsf2-maxrange2*ifqstep);
sbg[i] = 0;
if (minsf > 0)
sbg[i] = floor (.125*minsf + .001);
if (maxsf1 > 0)
sbg[i] = Max (sbg[i], (maxsf1/8 + (maxsf1 % 8 != 0)));
if (sbg[i] > 7)
sbg[i] = 7;
for (sfb = 0; sfb < SBPSY_s; sfb++) {
sf[sfb][i] += 8*sbg[i];
if (sf[sfb][i] < 0) {
maxrange = sfb < 6 ? maxrange1 : maxrange2;
scalefac[sfb][i]
= -sf[sfb][i]/ifqstep + (-sf[sfb][i]%ifqstep != 0);
if (scalefac[sfb][i] > maxrange)
scalefac[sfb][i] = maxrange;
if (maxover < -(sf[sfb][i] + scalefac[sfb][i]*ifqstep))
maxover = -(sf[sfb][i] + scalefac[sfb][i]*ifqstep);
}
}
}
return maxover;
}
static int
compute_scalefacs_long_lsf (
int sf [SBPSY_l],
const gr_info * const cod_info,
int scalefac [SBPSY_l] )
{
const int * max_range = max_range_long_lsf;
int ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
int sfb;
int maxover;
if (cod_info->preflag) {
max_range = max_range_long_lsf_pretab;
for (sfb = 11; sfb < SBPSY_l; sfb++)
sf[sfb] += pretab[sfb] * ifqstep;
}
maxover = 0;
for (sfb = 0; sfb < SBPSY_l; sfb++) {
if (sf[sfb] < 0) {
scalefac[sfb] = -sf[sfb]/ifqstep + (-sf[sfb] % ifqstep != 0);
if (scalefac[sfb] > max_range[sfb])
scalefac[sfb] = max_range[sfb];
if (-(sf[sfb] + scalefac[sfb]*ifqstep) > maxover) {
maxover = -(sf[sfb] + scalefac[sfb]*ifqstep);
}
}
}
return maxover;
}
static int
compute_scalefacs_short(int sf[SBPSY_s][3],gr_info *cod_info,
int scalefac[SBPSY_s][3],int sbg[3])
{
int maxrange,maxrange1,maxrange2,maxover;
int sfb,i;
int ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
maxover=0;
maxrange1 = 15;
maxrange2 = 7;
for (i=0; i<3; ++i) {
int maxsf1=0,maxsf2=0,minsf=1000;
for ( sfb = 0; sfb < SBPSY_s; sfb++ ) {
if (sfb < 6) {
if (-sf[sfb][i]>maxsf1) maxsf1 = -sf[sfb][i];
} else {
if (-sf[sfb][i]>maxsf2) maxsf2 = -sf[sfb][i];
}
if (-sf[sfb][i]<minsf) minsf = -sf[sfb][i];
}
maxsf1 = Max(maxsf1-maxrange1*ifqstep,maxsf2-maxrange2*ifqstep);
sbg[i]=0;
if (minsf >0 ) sbg[i] = floor(.125*minsf + .001);
if (maxsf1 > 0) sbg[i] = Max(sbg[i],(maxsf1/8 + (maxsf1 % 8 != 0)));
if (sbg[i] > 7) sbg[i]=7;
for ( sfb = 0; sfb < SBPSY_s; sfb++ ) {
sf[sfb][i] += 8*sbg[i];
if (sf[sfb][i] < 0) {
maxrange = sfb < 6 ? maxrange1 : maxrange2;
scalefac[sfb][i]= -sf[sfb][i]/ifqstep + (-sf[sfb][i]%ifqstep != 0);
if (scalefac[sfb][i]>maxrange) scalefac[sfb][i]=maxrange;
if (-(sf[sfb][i] + scalefac[sfb][i]*ifqstep) >maxover) {
maxover=-(sf[sfb][i] + scalefac[sfb][i]*ifqstep);
}
}
}
}
return maxover;
}
static int
compute_scalefacs_long(int sf[SBPSY_l],gr_info *cod_info,int scalefac[SBPSY_l])
{
int sfb;
int maxover;
int ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
if (cod_info->preflag)
for ( sfb = 11; sfb < SBPSY_l; sfb++ )
sf[sfb] += pretab[sfb]*ifqstep;
maxover=0;
for ( sfb = 0; sfb < SBPSY_l; sfb++ ) {
if (sf[sfb]<0) {
scalefac[sfb]= -sf[sfb]/ifqstep + (-sf[sfb] % ifqstep != 0);
if (scalefac[sfb] > max_range_long[sfb]) scalefac[sfb]=max_range_long[sfb];
if ( -(sf[sfb] + scalefac[sfb]*ifqstep) > maxover) {
maxover = -(sf[sfb] + scalefac[sfb]*ifqstep);
}
}
}
return maxover;
}
static int
VBR_quantize_granule(
lame_global_flags *gfp,
FLOAT8 xr34[576],
int l3_enc[576],
const III_psy_ratio * const ratio,
III_scalefac_t * const scalefac,
const int gr,
const int ch)
{
lame_internal_flags *gfc=gfp->internal_flags;
int status;
gr_info *cod_info;
III_side_info_t * l3_side;
l3_side = &gfc->l3_side;
cod_info = &l3_side->gr[gr].ch[ch].tt;
if ( gfc->is_mpeg1 )
status=scale_bitcount(scalefac, cod_info);
else
status=scale_bitcount_lsf(gfc,scalefac, cod_info);
if (status!=0) {
return -1;
}
cod_info->part2_3_length = count_bits(gfc,l3_enc,xr34,cod_info);
if (cod_info->part2_3_length >= LARGE_BITS) return -2;
cod_info->part2_3_length += cod_info->part2_length;
if (gfc->use_best_huffman==1) {
best_huffman_divide(gfc, gr, ch, cod_info, l3_enc);
}
return 0;
}
static const int MAX_SF_DELTA = 4;
static int
short_block_vbr_sf (
const lame_internal_flags * const gfc,
const III_psy_xmin * const l3_xmin,
const FLOAT8 xr34_orig[576],
const FLOAT8 xr34 [576],
III_scalefac_t * const vbrsf )
{
int j, sfb, b;
int vbrmax = -10000;
for (j = 0, sfb = 0; sfb < SBMAX_s; sfb++) {
for (b = 0; b < 3; b++) {
const int start = gfc->scalefac_band.s[ sfb ];
const int end = gfc->scalefac_band.s[ sfb+1 ];
const int width = end - start;
vbrsf->s[sfb][b] = find_scalefac_ave (&xr34[j], &xr34_orig[j],
sfb, l3_xmin->s[sfb][b], width);
j += width;
}
}
for (sfb = 0; sfb < SBMAX_s; sfb++) {
for (b = 0; b < 3; b++) {
if (sfb > 0)
if (vbrsf->s[sfb][b] > vbrsf->s[sfb-1][b]+MAX_SF_DELTA)
vbrsf->s[sfb][b] = vbrsf->s[sfb-1][b]+MAX_SF_DELTA;
if (sfb < SBMAX_s-1)
if (vbrsf->s[sfb][b] > vbrsf->s[sfb+1][b]+MAX_SF_DELTA)
vbrsf->s[sfb][b] = vbrsf->s[sfb+1][b]+MAX_SF_DELTA;
if (vbrmax < vbrsf->s[sfb][b])
vbrmax = vbrsf->s[sfb][b];
}
}
return vbrmax;
}
static int
long_block_vbr_sf (
const lame_internal_flags * const gfc,
const III_psy_xmin * const l3_xmin,
const FLOAT8 xr34_orig[576],
const FLOAT8 xr34 [576],
III_scalefac_t * const vbrsf )
{
int sfb;
int vbrmax = -10000;
for (sfb = 0; sfb < SBMAX_l; sfb++) {
const int start = gfc->scalefac_band.l[ sfb ];
const int end = gfc->scalefac_band.l[ sfb+1 ];
const int width = end - start;
vbrsf->l[sfb] = find_scalefac_ave (&xr34[start], &xr34_orig[start],
sfb, l3_xmin->l[sfb], width);
}
for (sfb = 0; sfb < SBMAX_l; sfb++) {
if (sfb > 0)
if (vbrsf->l[sfb] > vbrsf->l[sfb-1]+MAX_SF_DELTA)
vbrsf->l[sfb] = vbrsf->l[sfb-1]+MAX_SF_DELTA;
if (sfb < SBMAX_l-1)
if (vbrsf->l[sfb] > vbrsf->l[sfb+1]+MAX_SF_DELTA)
vbrsf->l[sfb] = vbrsf->l[sfb+1]+MAX_SF_DELTA;
if (vbrmax < vbrsf->l[sfb])
vbrmax = vbrsf->l[sfb];
}
return vbrmax;
}
static int
short_block_sf (
const lame_internal_flags * const gfc,
const III_psy_xmin * const l3_xmin,
const FLOAT8 xr34_orig[576],
const FLOAT8 xr34 [576],
III_scalefac_t * const vbrsf )
{
int j, sfb, b;
int vbrmean, vbrmin, vbrmax;
int sf_cache[SBMAX_s];
for (j = 0, sfb = 0; sfb < SBMAX_s; sfb++) {
for (b = 0; b < 3; b++) {
const int start = gfc->scalefac_band.s[ sfb ];
const int end = gfc->scalefac_band.s[ sfb+1 ];
const int width = end - start;
if (0 == gfc->noise_shaping_amp) {
vbrsf->s[sfb][b] = find_scalefac (&xr34[j], &xr34_orig[j], sfb,
l3_xmin->s[sfb][b], width);
}
else {
vbrsf->s[sfb][b] = find_scalefac_ave (&xr34[j], &xr34_orig[j],
sfb, l3_xmin->s[sfb][b], width);
}
j += width;
}
}
vbrmax = -10000;
for (b = 0; b < 3; b++) {
for (sfb = 0; sfb < SBMAX_s; sfb++)
sf_cache[sfb] = vbrsf->s[sfb][b];
vbrmean = select_kth_int (sf_cache, SBMAX_s, (SBMAX_s+1)/2);
vbrmin = 10000;
for (sfb = 0; sfb < SBMAX_s; sfb++) {
if (vbrmin > vbrsf->s[sfb][b])
vbrmin = vbrsf->s[sfb][b];
}
vbrsf->s[SBPSY_s][b] = Min (vbrsf->s[SBPSY_s][b], vbrmean);
vbrsf->s[SBPSY_s][b] = Max (vbrsf->s[SBPSY_s][b], vbrmin-(vbrmean-vbrmin));
for (sfb = 0; sfb < SBMAX_s; sfb++) {
if (vbrsf->s[sfb][b] > vbrmean+(vbrmean-vbrmin))
vbrsf->s[sfb][b] = vbrmean+(vbrmean-vbrmin);
}
for (sfb = 0; sfb < SBMAX_s; sfb++) {
if (vbrmax < vbrsf->s[sfb][b])
vbrmax = vbrsf->s[sfb][b];
}
}
return vbrmax;
}
static int
long_block_sf (
const lame_internal_flags * const gfc,
const III_psy_xmin * const l3_xmin,
const FLOAT8 xr34_orig[576],
const FLOAT8 xr34 [576],
III_scalefac_t * const vbrsf )
{
int sfb;
int vbrmean, vbrmin, vbrmax;
int sf_cache[SBMAX_l];
for (sfb = 0; sfb < SBMAX_l; sfb++) {
const int start = gfc->scalefac_band.l[ sfb ];
const int end = gfc->scalefac_band.l[ sfb+1 ];
const int width = end - start;
if (0 == gfc->noise_shaping_amp) {
vbrsf->l[sfb] = find_scalefac (&xr34[start], &xr34_orig[start],
sfb, l3_xmin->l[sfb], width);
}
else {
vbrsf->l[sfb] = find_scalefac_ave (&xr34[start], &xr34_orig[start],
sfb, l3_xmin->l[sfb], width);
}
}
for (sfb = 0; sfb < SBMAX_l; sfb++)
sf_cache[sfb] = vbrsf->l[sfb];
vbrmean = select_kth_int (sf_cache, SBMAX_l, (SBMAX_l+1)/2);
vbrmin = +10000;
for (sfb = 0; sfb < SBMAX_l; sfb++) {
if (vbrmin > vbrsf->l[sfb])
vbrmin = vbrsf->l[sfb];
}
vbrsf->l[SBPSY_l] = Min (vbrsf->l[SBPSY_l], vbrmean);
vbrsf->l[SBPSY_l] = Max (vbrsf->l[SBPSY_l], vbrmin-(vbrmean-vbrmin));
for (sfb = 0; sfb < SBMAX_l; sfb++) {
if (vbrsf->l[sfb] > vbrmean+(vbrmean-vbrmin))
vbrsf->l[sfb] = vbrmean+(vbrmean-vbrmin);
}
vbrmax = -10000;
for (sfb = 0; sfb < SBMAX_l; sfb++) {
if (vbrmax < vbrsf->l[sfb])
vbrmax = vbrsf->l[sfb];
}
return vbrmax;
}
static void
short_block_scalefacs (
lame_global_flags *gfp,
gr_info * const cod_info,
III_scalefac_t * const scalefac,
III_scalefac_t * const vbrsf,
int * const VBRmax )
{
lame_internal_flags *gfc=gfp->internal_flags;
const int * max_range;
int sfb, b;
int maxover, maxover0, maxover1, mover;
int v0, v1;
int minsfb;
int vbrmax = *VBRmax;
max_range = gfc->is_mpeg1 ? max_range_short : max_range_short_lsf;
maxover0 = 0;
maxover1 = 0;
for (sfb = 0; sfb < SBPSY_s; sfb++) {
for (b = 0; b < 3; b++) {
v0 = (vbrmax - vbrsf->s[sfb][b]) - (4*14 + 2*max_range[sfb]);
v1 = (vbrmax - vbrsf->s[sfb][b]) - (4*14 + 4*max_range[sfb]);
if (maxover0 < v0)
maxover0 = v0;
if (maxover1 < v1)
maxover1 = v1;
}
}
if (gfc->noise_shaping == 2)
mover = Min (maxover0, maxover1);
else
mover = maxover0;
vbrmax -= mover;
maxover0 -= mover;
maxover1 -= mover;
if (maxover0 == 0)
cod_info->scalefac_scale = 0;
else if (maxover1 == 0)
cod_info->scalefac_scale = 1;
cod_info->global_gain = vbrmax + 210;
assert(cod_info->global_gain < 256);
if (vbr_mtrh == gfp->VBR && cod_info->global_gain > 1) {
cod_info->global_gain -= 1;
}
if (cod_info->global_gain > 255)
cod_info->global_gain = 255;
for (sfb = 0; sfb < SBPSY_s; sfb++) {
for (b = 0; b < 3; b++) {
vbrsf->s[sfb][b] -= vbrmax;
}
}
if ( gfc->is_mpeg1 )
maxover = compute_scalefacs_short (vbrsf->s, cod_info, scalefac->s,
cod_info->subblock_gain);
else
maxover = compute_scalefacs_short_lsf (vbrsf->s, cod_info, scalefac->s,
cod_info->subblock_gain);
assert (maxover <= 0);
minsfb = 999;
for (b = 0; b < 3; b++)
if (minsfb > cod_info->subblock_gain[b])
minsfb = cod_info->subblock_gain[b];
if (minsfb > cod_info->global_gain/8)
minsfb = cod_info->global_gain/8;
vbrmax -= 8*minsfb;
cod_info->global_gain -= 8*minsfb;
for (b = 0; b < 3; b++)
cod_info->subblock_gain[b] -= minsfb;
*VBRmax = vbrmax;
}
static void
long_block_scalefacs (
lame_global_flags *gfp,
gr_info * const cod_info,
III_scalefac_t * const scalefac,
III_scalefac_t * const vbrsf,
int * const VBRmax )
{
lame_internal_flags *gfc=gfp->internal_flags;
const int * max_range;
const int * max_rangep;
int sfb;
int maxover, maxover0, maxover1, maxover0p, maxover1p, mover;
int v0, v1, v0p, v1p;
int vbrmax = *VBRmax;
max_range = gfc->is_mpeg1 ? max_range_long : max_range_long_lsf;
max_rangep = gfc->is_mpeg1 ? max_range_long : max_range_long_lsf_pretab;
maxover0 = 0;
maxover1 = 0;
maxover0p = 0;
maxover1p = 0;
for ( sfb = 0; sfb < SBPSY_l; sfb++ ) {
v0 = (vbrmax - vbrsf->l[sfb]) - 2*max_range[sfb];
v1 = (vbrmax - vbrsf->l[sfb]) - 4*max_range[sfb];
v0p = (vbrmax - vbrsf->l[sfb]) - 2*(max_rangep[sfb]+pretab[sfb]);
v1p = (vbrmax - vbrsf->l[sfb]) - 4*(max_rangep[sfb]+pretab[sfb]);
if (maxover0 < v0)
maxover0 = v0;
if (maxover1 < v1)
maxover1 = v1;
if (maxover0p < v0p)
maxover0p = v0p;
if (maxover1p < v1p)
maxover1p = v1p;
}
mover = Min (maxover0, maxover0p);
if (gfc->noise_shaping == 2) {
mover = Min (mover, maxover1);
mover = Min (mover, maxover1p);
}
vbrmax -= mover;
maxover0 -= mover;
maxover0p -= mover;
maxover1 -= mover;
maxover1p -= mover;
if (maxover0 <= 0) {
cod_info->scalefac_scale = 0;
cod_info->preflag = 0;
vbrmax -= maxover0;
} else if (maxover0p <= 0) {
cod_info->scalefac_scale = 0;
cod_info->preflag = 1;
vbrmax -= maxover0p;
} else if (maxover1 == 0) {
cod_info->scalefac_scale = 1;
cod_info->preflag = 0;
} else if (maxover1p == 0) {
cod_info->scalefac_scale = 1;
cod_info->preflag = 1;
} else {
assert(0);
}
cod_info->global_gain = vbrmax + 210;
assert (cod_info->global_gain < 256);
if (vbr_mtrh == gfp->VBR && cod_info->global_gain > 1) {
cod_info->global_gain -= 1;
}
if (cod_info->global_gain > 255)
cod_info->global_gain = 255;
for (sfb = 0; sfb < SBPSY_l; sfb++)
vbrsf->l[sfb] -= vbrmax;
if ( gfc->is_mpeg1 == 1 )
maxover = compute_scalefacs_long (vbrsf->l, cod_info, scalefac->l);
else
maxover = compute_scalefacs_long_lsf (vbrsf->l, cod_info, scalefac->l);
assert (maxover <= 0);
*VBRmax = vbrmax;
}
static FLOAT8 calc_fac ( const int ifac )
{
if (ifac+210 < Q_MAX)
return 1/IPOW20 (ifac+210);
else
return pow (2.0, 0.75*ifac/4.0);
}
static void
short_block_xr34 (
const lame_internal_flags * const gfc,
const gr_info * const cod_info,
const III_scalefac_t * const scalefac,
const FLOAT8 xr34_orig[576],
FLOAT8 xr34 [576] )
{
int sfb, l, j, b;
int ifac, ifqstep, start, end;
FLOAT8 fac;
ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
for ( j = 0, sfb = 0; sfb < SBMAX_s; sfb++ ) {
start = gfc->scalefac_band.s[ sfb ];
end = gfc->scalefac_band.s[ sfb+1 ];
for (b = 0; b < 3; b++) {
ifac = 8*cod_info->subblock_gain[b]+ifqstep*scalefac->s[sfb][b];
fac = calc_fac( ifac );
l = (end-start+7) / 8;
switch ((end-start) % 8) {
default:
case 0: do{ xr34[j] = xr34_orig[j]*fac; j++;
case 7: xr34[j] = xr34_orig[j]*fac; j++;
case 6: xr34[j] = xr34_orig[j]*fac; j++;
case 5: xr34[j] = xr34_orig[j]*fac; j++;
case 4: xr34[j] = xr34_orig[j]*fac; j++;
case 3: xr34[j] = xr34_orig[j]*fac; j++;
case 2: xr34[j] = xr34_orig[j]*fac; j++;
case 1: xr34[j] = xr34_orig[j]*fac; j++; } while (--l);
}
}
}
}
static void
long_block_xr34 (
const lame_internal_flags * const gfc,
const gr_info * const cod_info,
const III_scalefac_t * const scalefac,
const FLOAT8 xr34_orig[576],
FLOAT8 xr34 [576] )
{
int sfb, l, j;
int ifac, ifqstep, start, end;
FLOAT8 fac;
ifqstep = ( cod_info->scalefac_scale == 0 ) ? 2 : 4;
for ( sfb = 0; sfb < SBMAX_l; sfb++ ) {
ifac = ifqstep*scalefac->l[sfb];
if (cod_info->preflag)
ifac += ifqstep*pretab[sfb];
fac = calc_fac( ifac );
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
j = start;
l = (end-start+7) / 8;
switch ((end-start) % 8) {
default:
case 0: do{ xr34[j] = xr34_orig[j]*fac; j++;
case 7: xr34[j] = xr34_orig[j]*fac; j++;
case 6: xr34[j] = xr34_orig[j]*fac; j++;
case 5: xr34[j] = xr34_orig[j]*fac; j++;
case 4: xr34[j] = xr34_orig[j]*fac; j++;
case 3: xr34[j] = xr34_orig[j]*fac; j++;
case 2: xr34[j] = xr34_orig[j]*fac; j++;
case 1: xr34[j] = xr34_orig[j]*fac; j++; } while (--l);
}
}
}
static int
VBR_noise_shaping (
lame_global_flags *gfp,
FLOAT8 xr [576],
FLOAT8 xr34orig [576],
III_psy_ratio *ratio,
int l3_enc [576],
int digital_silence,
int minbits,
int maxbits,
III_scalefac_t *scalefac,
III_psy_xmin *l3_xmin,
int gr,
int ch )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_scalefac_t save_sf;
III_scalefac_t vbrsf;
gr_info *cod_info;
FLOAT8 xr34[576];
int shortblock;
int vbrmax;
int global_gain_adjust = 0;
cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
shortblock = (cod_info->block_type == SHORT_TYPE);
if (shortblock)
vbrmax = short_block_vbr_sf (gfc, l3_xmin, xr34orig, xr, &vbrsf);
else
vbrmax = long_block_vbr_sf (gfc, l3_xmin, xr34orig, xr, &vbrsf);
memcpy (&save_sf, &vbrsf, sizeof(III_scalefac_t));
do {
memset (scalefac, 0, sizeof(III_scalefac_t));
if (shortblock) {
short_block_scalefacs (gfp, cod_info, scalefac, &vbrsf, &vbrmax);
short_block_xr34 (gfc, cod_info, scalefac, xr34orig, xr34);
} else {
long_block_scalefacs (gfp, cod_info, scalefac, &vbrsf, &vbrmax);
long_block_xr34 (gfc, cod_info, scalefac, xr34orig, xr34);
}
VBR_quantize_granule (gfp, xr34, l3_enc, ratio, scalefac, gr, ch);
if (cod_info->part2_3_length < minbits) {
if (digital_silence) break;
if (vbrmax+210 == 0) break;
--vbrmax;
--global_gain_adjust;
memcpy (&vbrsf, &save_sf, sizeof(III_scalefac_t));
}
} while (cod_info->part2_3_length < minbits);
while (cod_info->part2_3_length > Min (maxbits, MAX_BITS)) {
++cod_info->global_gain;
if (cod_info->global_gain > 255)
ERRORF (gfc,"%ld impossible to encode ??? frame! bits=%d\n",
-1, cod_info->part2_3_length);
VBR_quantize_granule (gfp, xr34, l3_enc, ratio, scalefac, gr, ch);
++global_gain_adjust;
}
return global_gain_adjust;
}
int
VBR_noise_shaping2 (
lame_global_flags *gfp,
FLOAT8 xr [576],
FLOAT8 xr34orig [576],
III_psy_ratio * const ratio,
int l3_enc [576],
int digital_silence,
int minbits,
int maxbits,
III_scalefac_t * const scalefac,
III_psy_xmin * const l3_xmin,
int gr,
int ch )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_scalefac_t vbrsf;
gr_info *cod_info;
FLOAT8 xr34[576];
int shortblock, ret, bits, huffbits;
int vbrmax, best_huffman = gfc->use_best_huffman;
cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
shortblock = (cod_info->block_type == SHORT_TYPE);
if (shortblock) {
vbrmax = short_block_sf (gfc, l3_xmin, xr34orig, xr, &vbrsf);
short_block_scalefacs (gfp, cod_info, scalefac, &vbrsf, &vbrmax);
short_block_xr34 (gfc, cod_info, scalefac, xr34orig, xr34);
} else {
vbrmax = long_block_sf (gfc, l3_xmin, xr34orig, xr, &vbrsf);
long_block_scalefacs (gfp, cod_info, scalefac, &vbrsf, &vbrmax);
long_block_xr34 (gfc, cod_info, scalefac, xr34orig, xr34);
}
gfc->use_best_huffman = 0;
ret = VBR_quantize_granule (gfp, xr34, l3_enc, ratio, scalefac, gr, ch);
gfc->use_best_huffman = best_huffman;
if (ret == -1)
return -1;
if (cod_info->part2_3_length < minbits) {
huffbits = minbits - cod_info->part2_length;
bits = bin_search_StepSize (gfc, cod_info, huffbits,
gfc->OldValue[ch], xr34, l3_enc);
gfc->OldValue[ch] = cod_info->global_gain;
cod_info->part2_3_length = bits + cod_info->part2_length;
}
if (cod_info->part2_3_length > maxbits) {
huffbits = maxbits - cod_info->part2_length;
bits = bin_search_StepSize (gfc, cod_info, huffbits,
gfc->OldValue[ch], xr34, l3_enc);
gfc->OldValue[ch] = cod_info->global_gain;
cod_info->part2_3_length = bits;
if (bits > huffbits) {
bits = inner_loop (gfc, cod_info, huffbits, xr34, l3_enc);
cod_info->part2_3_length = bits;
}
if (bits >= LARGE_BITS)
return -2;
cod_info->part2_3_length += cod_info->part2_length;
}
if (cod_info->part2_length >= LARGE_BITS)
return -2;
assert (cod_info->global_gain < 256);
return 0;
}
void
VBR_quantize(lame_global_flags *gfp,
FLOAT8 pe[2][2], FLOAT8 ms_ener_ratio[2],
FLOAT8 xr[2][2][576], III_psy_ratio ratio[2][2],
int l3_enc[2][2][576],
III_scalefac_t scalefac[2][2])
{
lame_internal_flags *gfc=gfp->internal_flags;
III_psy_xmin l3_xmin[2][2];
int minbits,maxbits,max_frame_bits,totbits,gr,ch,i,bits_ok;
int bitsPerFrame,mean_bits;
int analog_silence;
FLOAT8 qadjust;
III_side_info_t * l3_side;
gr_info *cod_info;
int digital_silence[2][2];
FLOAT8 masking_lower_db=0;
FLOAT8 xr34[2][2][576];
static const FLOAT8 dbQ[10]=
{-6.06,-4.4,-2.9,-1.57, -0.4, 0.61, 1.45, 2.13, 2.65, 3.0};
qadjust=0;
l3_side = &gfc->l3_side;
assert( gfp->VBR_q <= 9 );
assert( gfp->VBR_q >= 0 );
analog_silence=1;
for (gr = 0; gr < gfc->mode_gr; gr++) {
if (gfc->mode_ext==MPG_MD_MS_LR) {
ms_convert(xr[gr],xr[gr]);
}
for (ch = 0; ch < gfc->channels_out; ch++) {
int over_ath;
cod_info = &l3_side->gr[gr].ch[ch].tt;
cod_info->part2_3_length=LARGE_BITS;
if (cod_info->block_type == SHORT_TYPE) {
cod_info->sfb_lmax = 0;
cod_info->sfb_smin = 0;
} else {
cod_info->sfb_lmax = SBPSY_l;
cod_info->sfb_smin = SBPSY_s;
if (cod_info->mixed_block_flag) {
cod_info->sfb_lmax = 8;
cod_info->sfb_smin = 3;
}
}
masking_lower_db = dbQ[gfp->VBR_q];
if (pe[gr][ch]>750) {
masking_lower_db -= Min(10,4*(pe[gr][ch]-750.)/750.);
}
gfc->masking_lower = pow(10.0,masking_lower_db/10);
over_ath = calc_xmin(gfp,xr[gr][ch],&ratio[gr][ch],cod_info,&l3_xmin[gr][ch]);
if (over_ath) {
analog_silence = 0;
}
digital_silence[gr][ch] = 1;
for(i=0;i<576;i++) {
FLOAT8 temp=fabs(xr[gr][ch][i]);
xr34[gr][ch][i]=sqrt(sqrt(temp)*temp);
digital_silence[gr][ch] &= temp < 1E-20;
}
}
}
if (analog_silence) {
gfc->bitrate_index=1;
} else {
gfc->bitrate_index=gfc->VBR_min_bitrate;
}
getframebits(gfp, &bitsPerFrame, &mean_bits);
minbits = (mean_bits/gfc->channels_out);
gfc->bitrate_index=gfc->VBR_max_bitrate;
getframebits(gfp, &bitsPerFrame, &mean_bits);
max_frame_bits = ResvFrameBegin(gfp, l3_side, mean_bits, bitsPerFrame);
maxbits=2.5*(mean_bits/gfc->channels_out);
{
int bit_rate = gfp->out_samplerate*16*gfc->channels_out/(1000.0*gfp->compression_ratio);
bitsPerFrame = (bit_rate*gfp->framesize*1000)/gfp->out_samplerate;
mean_bits = (bitsPerFrame - 8*gfc->sideinfo_len) / gfc->mode_gr;
}
minbits = Max(minbits,125);
minbits=Max(minbits,.40*(mean_bits/gfc->channels_out));
maxbits=Min(maxbits,2.5*(mean_bits/gfc->channels_out));
do {
totbits=0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
int minbits_lr[2];
minbits_lr[0]=minbits;
minbits_lr[1]=minbits;
#if 0
if (gfc->mode_ext==MPG_MD_MS_LR) {
FLOAT8 fac;
fac = .33*(.5-ms_ener_ratio[gr])/.5;
if (fac<0) fac=0;
if (fac>.5) fac=.5;
minbits_lr[0] = (1+fac)*minbits;
minbits_lr[1] = Max(125,(1-fac)*minbits);
}
#endif
for (ch = 0; ch < gfc->channels_out; ch++) {
int adjusted,shortblock;
cod_info = &l3_side->gr[gr].ch[ch].tt;
if (cod_info->part2_3_length > (max_frame_bits/(2*gfc->channels_out*gfc->mode_gr))) {
shortblock = (cod_info->block_type == SHORT_TYPE);
if (qadjust!=0 ) {
masking_lower_db = dbQ[gfp->VBR_q] + qadjust;
if (pe[gr][ch]>750)
masking_lower_db -= Min(10,4*(pe[gr][ch]-750.)/750.);
gfc->masking_lower = pow(10.0,masking_lower_db/10);
calc_xmin( gfp, xr[gr][ch], ratio[gr]+ch, cod_info, l3_xmin[gr]+ch);
}
adjusted = VBR_noise_shaping (gfp,xr[gr][ch],xr34[gr][ch],
ratio[gr]+ch,l3_enc[gr][ch],
digital_silence[gr][ch],
minbits_lr[ch],
maxbits,scalefac[gr]+ch,
l3_xmin[gr]+ch,gr,ch);
if (adjusted>10) {
}
}
totbits += cod_info->part2_3_length;
}
}
bits_ok=1;
if (totbits>max_frame_bits) {
qadjust += Max(.125,Min(1,(totbits-max_frame_bits)/300.0));
minbits = Max(125,minbits*0.975);
maxbits = Max(minbits,maxbits*0.975);
bits_ok=0;
}
} while (!bits_ok);
totbits=0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
best_scalefac_store(gfc, gr, ch, l3_enc, l3_side, scalefac);
totbits += l3_side->gr[gr].ch[ch].tt.part2_3_length;
}
}
if (analog_silence && !gfp->VBR_hard_min) {
gfc->bitrate_index = 1;
} else {
gfc->bitrate_index = gfc->VBR_min_bitrate;
}
for( ; gfc->bitrate_index < gfc->VBR_max_bitrate; gfc->bitrate_index++ ) {
getframebits (gfp, &bitsPerFrame, &mean_bits);
maxbits = ResvFrameBegin(gfp, l3_side, mean_bits, bitsPerFrame);
if (totbits <= maxbits) break;
}
if (gfc->bitrate_index == gfc->VBR_max_bitrate) {
getframebits (gfp, &bitsPerFrame, &mean_bits);
maxbits = ResvFrameBegin(gfp, l3_side, mean_bits, bitsPerFrame);
}
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
cod_info = &l3_side->gr[gr].ch[ch].tt;
ResvAdjust (gfc, cod_info, l3_side, mean_bits);
for ( i = 0; i < 576; i++) {
if (xr[gr][ch][i] < 0) l3_enc[gr][ch][i] *= -1;
}
}
}
ResvFrameEnd (gfc, l3_side, mean_bits);
}