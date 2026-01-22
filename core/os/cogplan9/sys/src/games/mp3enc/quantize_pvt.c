#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "util.h"
#include "lame-analysis.h"
#include "tables.h"
#include "reservoir.h"
#include "quantize_pvt.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#define NSATHSCALE 100
const char slen1_tab [16] = { 0, 0, 0, 0, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4 };
const char slen2_tab [16] = { 0, 1, 2, 3, 0, 1, 2, 3, 1, 2, 3, 1, 2, 3, 2, 3 };
const int nr_of_sfb_block [6] [3] [4] =
{
{
{6, 5, 5, 5},
{9, 9, 9, 9},
{6, 9, 9, 9}
},
{
{6, 5, 7, 3},
{9, 9, 12, 6},
{6, 9, 12, 6}
},
{
{11, 10, 0, 0},
{18, 18, 0, 0},
{15,18,0,0}
},
{
{7, 7, 7, 0},
{12, 12, 12, 0},
{6, 15, 12, 0}
},
{
{6, 6, 6, 3},
{12, 9, 9, 6},
{6, 12, 9, 6}
},
{
{8, 8, 5, 0},
{15,12,9,0},
{6,18,9,0}
}
};
const char pretab [SBMAX_l] =
{
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
1, 1, 1, 1, 2, 2, 3, 3, 3, 2, 0
};
const scalefac_struct sfBandIndex[9] =
{
{
{0,6,12,18,24,30,36,44,54,66,80,96,116,140,168,200,238,284,336,396,464,522,576},
{0,4,8,12,18,24,32,42,56,74,100,132,174,192}
},
{
{0,6,12,18,24,30,36,44,54,66,80,96,114,136,162,194,232,278, 332, 394,464,540,576},
{0,4,8,12,18,26,36,48,62,80,104,136,180,192}
},
{
{0,6,12,18,24,30,36,44,54,66,80,96,116,140,168,200,238,284,336,396,464,522,576},
{0,4,8,12,18,26,36,48,62,80,104,134,174,192}
},
{
{0,4,8,12,16,20,24,30,36,44,52,62,74,90,110,134,162,196,238,288,342,418,576},
{0,4,8,12,16,22,30,40,52,66,84,106,136,192}
},
{
{0,4,8,12,16,20,24,30,36,42,50,60,72,88,106,128,156,190,230,276,330,384,576},
{0,4,8,12,16,22,28,38,50,64,80,100,126,192}
},
{
{0,4,8,12,16,20,24,30,36,44,54,66,82,102,126,156,194,240,296,364,448,550,576},
{0,4,8,12,16,22,30,42,58,78,104,138,180,192}
},
{
{0,6,12,18,24,30,36,44,54,66,80,96,116,140,168,200,238,284,336,396,464,522,576},
{0/3,12/3,24/3,36/3,54/3,78/3,108/3,144/3,186/3,240/3,312/3,402/3,522/3,576/3}
},
{
{0,6,12,18,24,30,36,44,54,66,80,96,116,140,168,200,238,284,336,396,464,522,576},
{0/3,12/3,24/3,36/3,54/3,78/3,108/3,144/3,186/3,240/3,312/3,402/3,522/3,576/3}
},
{
{0,12,24,36,48,60,72,88,108,132,160,192,232,280,336,400,476,566,568,570,572,574,576},
{0/3,24/3,48/3,72/3,108/3,156/3,216/3,288/3,372/3,480/3,486/3,492/3,498/3,576/3}
}
};
FLOAT8 pow20[Q_MAX];
FLOAT8 ipow20[Q_MAX];
FLOAT8 pow43[PRECALC_SIZE];
FLOAT8 adj43asm[PRECALC_SIZE];
FLOAT8 adj43[PRECALC_SIZE];
void
iteration_init( lame_global_flags *gfp)
{
lame_internal_flags *gfc=gfp->internal_flags;
III_side_info_t * const l3_side = &gfc->l3_side;
int i;
if ( gfc->iteration_init_init==0 ) {
gfc->iteration_init_init=1;
l3_side->main_data_begin = 0;
compute_ath(gfp,gfc->ATH->l,gfc->ATH->s);
pow43[0] = 0.0;
for(i=1;i<PRECALC_SIZE;i++)
pow43[i] = pow((FLOAT8)i, 4.0/3.0);
adj43asm[0] = 0.0;
for (i = 1; i < PRECALC_SIZE; i++)
adj43asm[i] = i - 0.5 - pow(0.5 * (pow43[i - 1] + pow43[i]),0.75);
for (i = 0; i < PRECALC_SIZE-1; i++)
adj43[i] = (i + 1) - pow(0.5 * (pow43[i] + pow43[i + 1]), 0.75);
adj43[i] = 0.5;
for (i = 0; i < Q_MAX; i++) {
ipow20[i] = pow(2.0, (double)(i - 210) * -0.1875);
pow20[i] = pow(2.0, (double)(i - 210) * 0.25);
}
huffman_init(gfc);
}
}
FLOAT8 ATHmdct( lame_global_flags *gfp, FLOAT8 f )
{
lame_internal_flags *gfc = gfp->internal_flags;
FLOAT8 ath;
ath = ATHformula( f , gfp );
if (gfc->nsPsy.use) {
ath -= NSATHSCALE;
} else {
ath -= 114;
}
ath -= gfp->ATHlower;
ath = pow( 10.0, ath/10.0 );
return ath;
}
void compute_ath( lame_global_flags *gfp, FLOAT8 ATH_l[], FLOAT8 ATH_s[] )
{
lame_internal_flags *gfc = gfp->internal_flags;
int sfb, i, start, end;
FLOAT8 ATH_f;
FLOAT8 samp_freq = gfp->out_samplerate;
for (sfb = 0; sfb < SBMAX_l; sfb++) {
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
ATH_l[sfb]=1e99;
for (i = start ; i < end; i++) {
FLOAT8 freq = i*samp_freq/(2*576);
ATH_f = ATHmdct( gfp, freq );
ATH_l[sfb] = Min( ATH_l[sfb], ATH_f );
}
}
for (sfb = 0; sfb < SBMAX_s; sfb++){
start = gfc->scalefac_band.s[ sfb ];
end = gfc->scalefac_band.s[ sfb+1 ];
ATH_s[sfb] = 1e99;
for (i = start ; i < end; i++) {
FLOAT8 freq = i*samp_freq/(2*192);
ATH_f = ATHmdct( gfp, freq );
ATH_s[sfb] = Min( ATH_s[sfb], ATH_f );
}
}
if (gfp->noATH) {
for (sfb = 0; sfb < SBMAX_l; sfb++) {
ATH_l[sfb] = 1E-37;
}
for (sfb = 0; sfb < SBMAX_s; sfb++) {
ATH_s[sfb] = 1E-37;
}
}
}
void ms_convert(FLOAT8 dst[2][576], FLOAT8 src[2][576])
{
FLOAT8 l;
FLOAT8 r;
int i;
for (i = 0; i < 576; ++i) {
l = src[0][i];
r = src[1][i];
dst[0][i] = (l+r) * (FLOAT8)(SQRT2*0.5);
dst[1][i] = (l-r) * (FLOAT8)(SQRT2*0.5);
}
}
int on_pe(lame_global_flags *gfp,FLOAT8 pe[2][2],III_side_info_t *l3_side,
int targ_bits[2],int mean_bits, int gr)
{
lame_internal_flags *gfc=gfp->internal_flags;
gr_info *cod_info;
int extra_bits,tbits,bits;
int add_bits[2];
int ch;
int max_bits;
ResvMaxBits (gfp, mean_bits, &tbits, &extra_bits);
max_bits=tbits+extra_bits;
bits=0;
for (ch=0 ; ch < gfc->channels_out ; ch ++) {
cod_info = &l3_side->gr[gr].ch[ch].tt;
targ_bits[ch]=Min(MAX_BITS, tbits/gfc->channels_out);
if (gfc->nsPsy.use) {
add_bits[ch] = targ_bits[ch]*pe[gr][ch]/700.0-targ_bits[ch];
} else {
add_bits[ch]=(pe[gr][ch]-750)/1.4;
if (cod_info->block_type==SHORT_TYPE) {
if (add_bits[ch]<mean_bits/4) add_bits[ch]=mean_bits/4;
}
if (add_bits[ch] > .75*mean_bits) add_bits[ch]=mean_bits*.75;
if (add_bits[ch] < 0) add_bits[ch]=0;
if ((targ_bits[ch]+add_bits[ch]) > MAX_BITS)
add_bits[ch]=Max(0, MAX_BITS-targ_bits[ch]);
}
bits += add_bits[ch];
}
if (bits > extra_bits)
for (ch=0 ; ch < gfc->channels_out ; ch ++) {
add_bits[ch] = (extra_bits*add_bits[ch])/bits;
}
for (ch=0 ; ch < gfc->channels_out ; ch ++) {
targ_bits[ch] = targ_bits[ch] + add_bits[ch];
extra_bits -= add_bits[ch];
}
return max_bits;
}
void reduce_side(int targ_bits[2],FLOAT8 ms_ener_ratio,int mean_bits,int max_bits)
{
int move_bits;
FLOAT fac;
fac = .33*(.5-ms_ener_ratio)/.5;
if (fac<0) fac=0;
if (fac>.5) fac=.5;
move_bits = fac*.5*(targ_bits[0]+targ_bits[1]);
if (move_bits > MAX_BITS - targ_bits[0]) {
move_bits = MAX_BITS - targ_bits[0];
}
if (move_bits<0) move_bits=0;
if (targ_bits[1] >= 125) {
if (targ_bits[1]-move_bits > 125) {
if (targ_bits[0] < mean_bits)
targ_bits[0] += move_bits;
targ_bits[1] -= move_bits;
} else {
targ_bits[0] += targ_bits[1] - 125;
targ_bits[1] = 125;
}
}
move_bits=targ_bits[0]+targ_bits[1];
if (move_bits > max_bits) {
targ_bits[0]=(max_bits*targ_bits[0])/move_bits;
targ_bits[1]=(max_bits*targ_bits[1])/move_bits;
}
}
#if 0
FLOAT8 dreinorm (FLOAT8 a, FLOAT8 b, FLOAT8 c)
{
return pow(pow(a,3.)+pow(b,3.)+pow(c,3.),1./3.);
}
#endif
int calc_xmin(
lame_global_flags *gfp,
const FLOAT8 xr [576],
const III_psy_ratio * const ratio,
const gr_info * const cod_info,
III_psy_xmin * const l3_xmin )
{
lame_internal_flags *gfc=gfp->internal_flags;
int sfb,j,start, end, bw,l, b, ath_over=0;
FLOAT8 en0, xmin, ener;
if (cod_info->block_type==SHORT_TYPE) {
for ( j=0, sfb = 0; sfb < SBMAX_s; sfb++ ) {
start = gfc->scalefac_band.s[ sfb ];
end = gfc->scalefac_band.s[ sfb + 1 ];
bw = end - start;
for ( b = 0; b < 3; b++ ) {
for (en0 = 0.0, l = start; l < end; l++) {
ener = xr[j++];
ener = ener * ener;
en0 += ener;
}
en0 /= bw;
if (gfp->ATHonly || gfp->ATHshort) {
xmin = gfc->ATH->adjust * gfc->ATH->s[sfb];
} else {
xmin = ratio->en.s[sfb][b];
if (xmin > 0.0)
xmin = en0 * ratio->thm.s[sfb][b] * gfc->masking_lower / xmin;
xmin = Max(gfc->ATH->adjust * gfc->ATH->s[sfb], xmin);
}
l3_xmin->s[sfb][b] = xmin * bw;
if (gfc->nsPsy.use) {
if (sfb <= 5) {
l3_xmin->s[sfb][b] *= gfc->nsPsy.bass;
} else if (sfb <= 10) {
l3_xmin->s[sfb][b] *= gfc->nsPsy.alto;
} else {
l3_xmin->s[sfb][b] *= gfc->nsPsy.treble;
}
}
if (en0 > gfc->ATH->adjust * gfc->ATH->s[sfb]) ath_over++;
if (gfc->nsPsy.use && (gfp->VBR == vbr_off || gfp->VBR == vbr_abr) && gfp->quality <= 1)
l3_xmin->s[sfb][b] *= 0.001;
}
}
if (gfp->useTemporal) {
for (sfb = 0; sfb < SBMAX_s; sfb++ ) {
for ( b = 1; b < 3; b++ ) {
xmin = l3_xmin->s[sfb][b] * (1.0 - gfc->decay)
+ l3_xmin->s[sfb][b-1] * gfc->decay;
if (l3_xmin->s[sfb][b] < xmin)
l3_xmin->s[sfb][b] = xmin;
}
}
}
}else{
if (gfc->nsPsy.use) {
for ( sfb = 0; sfb < SBMAX_l; sfb++ ){
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
for (en0 = 0.0, l = start; l < end; l++ ) {
ener = xr[l] * xr[l];
en0 += ener;
}
if (gfp->ATHonly) {
xmin=gfc->ATH->adjust * gfc->ATH->l[sfb];
} else {
xmin = ratio->en.l[sfb];
if (xmin > 0.0)
xmin = en0 * ratio->thm.l[sfb] * gfc->masking_lower / xmin;
xmin=Max(gfc->ATH->adjust * gfc->ATH->l[sfb], xmin);
}
l3_xmin->l[sfb]=xmin;
if (sfb <= 6) {
l3_xmin->l[sfb] *= gfc->nsPsy.bass;
} else if (sfb <= 13) {
l3_xmin->l[sfb] *= gfc->nsPsy.alto;
} else {
l3_xmin->l[sfb] *= gfc->nsPsy.treble;
}
if (en0 > gfc->ATH->adjust * gfc->ATH->l[sfb]) ath_over++;
if ((gfp->VBR == vbr_off || gfp->VBR == vbr_abr) && gfp->quality <= 1)
l3_xmin->l[sfb] *= 0.001;
}
} else {
for ( sfb = 0; sfb < SBMAX_l; sfb++ ){
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
bw = end - start;
for (en0 = 0.0, l = start; l < end; l++ ) {
ener = xr[l] * xr[l];
en0 += ener;
}
en0 /= bw;
if (gfp->ATHonly) {
xmin=gfc->ATH->adjust * gfc->ATH->l[sfb];
} else {
xmin = ratio->en.l[sfb];
if (xmin > 0.0)
xmin = en0 * ratio->thm.l[sfb] * gfc->masking_lower / xmin;
xmin=Max(gfc->ATH->adjust * gfc->ATH->l[sfb], xmin);
}
l3_xmin->l[sfb]=xmin*bw;
if (en0 > gfc->ATH->adjust * gfc->ATH->l[sfb]) ath_over++;
}
}
}
return ath_over;
}
double penalties ( double noise )
{
return log ( 0.368 + 0.632 * noise * noise * noise );
}
int calc_noise(
const lame_internal_flags * const gfc,
const FLOAT8 xr [576],
const int ix [576],
const gr_info * const cod_info,
const III_psy_xmin * const l3_xmin,
const III_scalefac_t * const scalefac,
III_psy_xmin * xfsf,
calc_noise_result * const res )
{
int sfb,start, end, j,l, i, over=0;
FLOAT8 sum;
int count=0;
FLOAT8 noise,noise_db;
FLOAT8 over_noise = 1;
FLOAT8 over_noise_db = 0;
FLOAT8 tot_noise = 1;
FLOAT8 tot_noise_db = 0;
FLOAT8 max_noise = 1E-20;
double klemm_noise = 1E-37;
if (cod_info->block_type == SHORT_TYPE) {
int max_index = gfc->sfb21_extra ? SBMAX_s : SBPSY_s;
for ( j=0, sfb = 0; sfb < max_index; sfb++ ) {
start = gfc->scalefac_band.s[ sfb ];
end = gfc->scalefac_band.s[ sfb+1 ];
for ( i = 0; i < 3; i++ ) {
FLOAT8 step;
int s;
s = (scalefac->s[sfb][i] << (cod_info->scalefac_scale + 1))
+ cod_info->subblock_gain[i] * 8;
s = cod_info->global_gain - s;
assert(s<Q_MAX);
assert(s>=0);
step = POW20(s);
sum = 0.0;
l = start;
do {
FLOAT8 temp;
temp = pow43[ix[j]];
temp *= step;
temp -= fabs(xr[j]);
++j;
sum += temp * temp;
l++;
} while (l < end);
noise = xfsf->s[sfb][i] = sum / l3_xmin->s[sfb][i];
max_noise = Max(max_noise,noise);
klemm_noise += penalties (noise);
noise_db=10*log10(Max(noise,1E-20));
tot_noise_db += noise_db;
if (noise > 1) {
over++;
over_noise_db += noise_db;
}
count++;
}
}
}else{
int max_index = gfc->sfb21_extra ? SBMAX_l : SBPSY_l;
for ( sfb = 0; sfb < max_index; sfb++ ) {
FLOAT8 step;
int s = scalefac->l[sfb];
if (cod_info->preflag)
s += pretab[sfb];
s = cod_info->global_gain - (s << (cod_info->scalefac_scale + 1));
assert(s<Q_MAX);
assert(s>=0);
step = POW20(s);
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
for ( sum = 0.0, l = start; l < end; l++ ) {
FLOAT8 temp;
temp = fabs(xr[l]) - pow43[ix[l]] * step;
sum += temp * temp;
}
noise = xfsf->l[sfb] = sum / l3_xmin->l[sfb];
max_noise=Max(max_noise,noise);
klemm_noise += penalties (noise);
noise_db=10*log10(Max(noise,1E-20));
tot_noise_db += noise_db;
if (noise > 1) {
over++;
over_noise_db += noise_db;
}
count++;
}
}
res->tot_count = count;
res->over_count = over;
res->tot_noise = tot_noise_db;
res->over_noise = over_noise_db;
res->max_noise = 10.*log10(Max(1e-20,max_noise ));
res->klemm_noise = 10.*log10(Max(1e-20,klemm_noise));
return over;
}
static
void set_pinfo (
lame_global_flags *gfp,
const gr_info * const cod_info,
const III_psy_ratio * const ratio,
const III_scalefac_t * const scalefac,
const FLOAT8 xr[576],
const int l3_enc[576],
const int gr,
const int ch )
{
lame_internal_flags *gfc=gfp->internal_flags;
int sfb;
int j,i,l,start,end,bw;
FLOAT8 en0,en1;
FLOAT ifqstep = ( cod_info->scalefac_scale == 0 ) ? .5 : 1.0;
III_psy_xmin l3_xmin;
calc_noise_result noise;
III_psy_xmin xfsf;
calc_xmin (gfp,xr, ratio, cod_info, &l3_xmin);
calc_noise (gfc, xr, l3_enc, cod_info, &l3_xmin, scalefac, &xfsf, &noise);
if (cod_info->block_type == SHORT_TYPE) {
for (j=0, sfb = 0; sfb < SBMAX_s; sfb++ ) {
start = gfc->scalefac_band.s[ sfb ];
end = gfc->scalefac_band.s[ sfb + 1 ];
bw = end - start;
for ( i = 0; i < 3; i++ ) {
for ( en0 = 0.0, l = start; l < end; l++ ) {
en0 += xr[j] * xr[j];
++j;
}
en0=Max(en0/bw,1e-20);
#if 0
{
double tot1,tot2;
if (sfb<SBMAX_s-1) {
if (sfb==0) {
tot1=0;
tot2=0;
}
tot1 += en0;
tot2 += ratio->en.s[sfb][i];
DEBUGF("%i %i sfb=%i mdct=%f fft=%f  fft-mdct=%f db \n",
gr,ch,sfb,
10*log10(Max(1e-25,ratio->en.s[sfb][i])),
10*log10(Max(1e-25,en0)),
10*log10((Max(1e-25,en0)/Max(1e-25,ratio->en.s[sfb][i]))));
if (sfb==SBMAX_s-2) {
DEBUGF("%i %i toti %f %f ratio=%f db \n",gr,ch,
10*log10(Max(1e-25,tot2)),
10*log(Max(1e-25,tot1)),
10*log10(Max(1e-25,tot1)/(Max(1e-25,tot2))));
}
}
}
#endif
en1=1e15;
gfc->pinfo->xfsf_s[gr][ch][3*sfb+i]
= en1*xfsf.s[sfb][i]*l3_xmin.s[sfb][i]/bw;
gfc->pinfo->en_s[gr][ch][3*sfb+i] = en1*en0;
if (ratio->en.s[sfb][i]>0)
en0 = en0/ratio->en.s[sfb][i];
else
en0=0;
if (gfp->ATHonly || gfp->ATHshort)
en0=0;
gfc->pinfo->thr_s[gr][ch][3*sfb+i] =
en1*Max(en0*ratio->thm.s[sfb][i],gfc->ATH->s[sfb]);
if (sfb < SBPSY_s) {
gfc->pinfo->LAMEsfb_s[gr][ch][3*sfb+i]=
-ifqstep*scalefac->s[sfb][i];
} else {
gfc->pinfo->LAMEsfb_s[gr][ch][3*sfb+i]=0;
}
gfc->pinfo->LAMEsfb_s[gr][ch][3*sfb+i] -=
2*cod_info->subblock_gain[i];
}
}
} else {
for ( sfb = 0; sfb < SBMAX_l; sfb++ ) {
start = gfc->scalefac_band.l[ sfb ];
end = gfc->scalefac_band.l[ sfb+1 ];
bw = end - start;
for ( en0 = 0.0, l = start; l < end; l++ )
en0 += xr[l] * xr[l];
if (!gfc->nsPsy.use) en0/=bw;
#if 0
{
double tot1,tot2;
if (sfb==0) {
tot1=0;
tot2=0;
}
tot1 += en0;
tot2 += ratio->en.l[sfb];
DEBUGF("%i %i sfb=%i mdct=%f fft=%f  fft-mdct=%f db \n",
gr,ch,sfb,
10*log10(Max(1e-25,ratio->en.l[sfb])),
10*log10(Max(1e-25,en0)),
10*log10((Max(1e-25,en0)/Max(1e-25,ratio->en.l[sfb]))));
if (sfb==SBMAX_l-1) {
DEBUGF("%i %i toti %f %f ratio=%f db \n",
gr,ch,
10*log10(Max(1e-25,tot2)),
10*log(Max(1e-25,tot1)),
10*log10(Max(1e-25,tot1)/(Max(1e-25,tot2))));
}
}
#endif
en1=1e15;
gfc->pinfo->xfsf[gr][ch][sfb] = en1*xfsf.l[sfb]*l3_xmin.l[sfb]/bw;
gfc->pinfo->en[gr][ch][sfb] = en1*en0;
if (ratio->en.l[sfb]>0)
en0 = en0/ratio->en.l[sfb];
else
en0=0;
if (gfp->ATHonly)
en0=0;
gfc->pinfo->thr[gr][ch][sfb] =
en1*Max(en0*ratio->thm.l[sfb],gfc->ATH->l[sfb]);
if (sfb<SBPSY_l) {
if (scalefac->l[sfb]<0)
gfc->pinfo->LAMEsfb[gr][ch][sfb] =
gfc->pinfo->LAMEsfb[0][ch][sfb];
else
gfc->pinfo->LAMEsfb[gr][ch][sfb] = -ifqstep*scalefac->l[sfb];
}else{
gfc->pinfo->LAMEsfb[gr][ch][sfb] = 0;
}
if (cod_info->preflag && sfb>=11)
gfc->pinfo->LAMEsfb[gr][ch][sfb] -= ifqstep*pretab[sfb];
}
}
gfc->pinfo->LAMEqss [gr][ch] = cod_info->global_gain;
gfc->pinfo->LAMEmainbits[gr][ch] = cod_info->part2_3_length;
gfc->pinfo->LAMEsfbits [gr][ch] = cod_info->part2_length;
gfc->pinfo->over [gr][ch] = noise.over_count;
gfc->pinfo->max_noise [gr][ch] = noise.max_noise;
gfc->pinfo->over_noise[gr][ch] = noise.over_noise;
gfc->pinfo->tot_noise [gr][ch] = noise.tot_noise;
}
void set_frame_pinfo(
lame_global_flags *gfp,
FLOAT8 xr [2][2][576],
III_psy_ratio ratio [2][2],
int l3_enc [2][2][576],
III_scalefac_t scalefac [2][2] )
{
lame_internal_flags *gfc=gfp->internal_flags;
unsigned int gr, ch, sfb;
int act_l3enc[576];
III_scalefac_t act_scalefac [2];
int scsfi[2] = {0,0};
gfc->masking_lower = 1.0;
for (ch = 0; ch < gfc->channels_out; ch ++) {
for (sfb = 0; sfb < SBMAX_l; sfb ++) {
if (scalefac[1][ch].l[sfb] == -1) {
act_scalefac[ch].l[sfb] = scalefac[0][ch].l[sfb];
scsfi[ch] = 1;
} else {
act_scalefac[ch].l[sfb] = scalefac[1][ch].l[sfb];
}
}
}
for (gr = 0; gr < gfc->mode_gr; gr ++) {
for (ch = 0; ch < gfc->channels_out; ch ++) {
int i;
gr_info *cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
for ( i = 0; i < 576; i++) {
if (xr[gr][ch][i] < 0)
act_l3enc[i] = -l3_enc[gr][ch][i];
else
act_l3enc[i] = +l3_enc[gr][ch][i];
}
if (gr == 1 && scsfi[ch])
set_pinfo (gfp, cod_info, &ratio[gr][ch], &act_scalefac[ch],
xr[gr][ch], act_l3enc, gr, ch);
else
set_pinfo (gfp, cod_info, &ratio[gr][ch], &scalefac[gr][ch],
xr[gr][ch], act_l3enc, gr, ch);
}
}
}
#ifdef TAKEHIRO_IEEE754_HACK
typedef union {
float f;
int i;
} fi_union;
#define MAGIC_FLOAT (65536*(128))
#define MAGIC_INT 0x4b000000
void quantize_xrpow(const FLOAT8 xp[576], int pi[576], FLOAT8 istep)
{
int j;
fi_union *fi;
fi = (fi_union *)pi;
for (j = 576 / 4 - 1; j >= 0; --j) {
double x0 = istep * xp[0];
double x1 = istep * xp[1];
double x2 = istep * xp[2];
double x3 = istep * xp[3];
x0 += MAGIC_FLOAT; fi[0].f = x0;
x1 += MAGIC_FLOAT; fi[1].f = x1;
x2 += MAGIC_FLOAT; fi[2].f = x2;
x3 += MAGIC_FLOAT; fi[3].f = x3;
fi[0].f = x0 + (adj43asm - MAGIC_INT)[fi[0].i];
fi[1].f = x1 + (adj43asm - MAGIC_INT)[fi[1].i];
fi[2].f = x2 + (adj43asm - MAGIC_INT)[fi[2].i];
fi[3].f = x3 + (adj43asm - MAGIC_INT)[fi[3].i];
fi[0].i -= MAGIC_INT;
fi[1].i -= MAGIC_INT;
fi[2].i -= MAGIC_INT;
fi[3].i -= MAGIC_INT;
fi += 4;
xp += 4;
}
}
# define ROUNDFAC -0.0946
void quantize_xrpow_ISO(const FLOAT8 xp[576], int pi[576], FLOAT8 istep)
{
int j;
fi_union *fi;
fi = (fi_union *)pi;
for (j=576/4 - 1;j>=0;j--) {
fi[0].f = istep * xp[0] + (ROUNDFAC + MAGIC_FLOAT);
fi[1].f = istep * xp[1] + (ROUNDFAC + MAGIC_FLOAT);
fi[2].f = istep * xp[2] + (ROUNDFAC + MAGIC_FLOAT);
fi[3].f = istep * xp[3] + (ROUNDFAC + MAGIC_FLOAT);
fi[0].i -= MAGIC_INT;
fi[1].i -= MAGIC_INT;
fi[2].i -= MAGIC_INT;
fi[3].i -= MAGIC_INT;
fi+=4;
xp+=4;
}
}
#else
#define XRPOW_FTOI(src,dest) ((dest) = (int)(src))
#define QUANTFAC(rx) adj43[rx]
#define ROUNDFAC 0.4054
void quantize_xrpow(const FLOAT8 xr[576], int ix[576], FLOAT8 istep) {
int j;
for ( j = 576/8; j > 0; --j) {
FLOAT8 x1, x2, x3, x4, x5, x6, x7, x8;
int rx1, rx2, rx3, rx4, rx5, rx6, rx7, rx8;
x1 = *xr++ * istep;
x2 = *xr++ * istep;
XRPOW_FTOI(x1, rx1);
x3 = *xr++ * istep;
XRPOW_FTOI(x2, rx2);
x4 = *xr++ * istep;
XRPOW_FTOI(x3, rx3);
x5 = *xr++ * istep;
XRPOW_FTOI(x4, rx4);
x6 = *xr++ * istep;
XRPOW_FTOI(x5, rx5);
x7 = *xr++ * istep;
XRPOW_FTOI(x6, rx6);
x8 = *xr++ * istep;
XRPOW_FTOI(x7, rx7);
x1 += QUANTFAC(rx1);
XRPOW_FTOI(x8, rx8);
x2 += QUANTFAC(rx2);
XRPOW_FTOI(x1,*ix++);
x3 += QUANTFAC(rx3);
XRPOW_FTOI(x2,*ix++);
x4 += QUANTFAC(rx4);
XRPOW_FTOI(x3,*ix++);
x5 += QUANTFAC(rx5);
XRPOW_FTOI(x4,*ix++);
x6 += QUANTFAC(rx6);
XRPOW_FTOI(x5,*ix++);
x7 += QUANTFAC(rx7);
XRPOW_FTOI(x6,*ix++);
x8 += QUANTFAC(rx8);
XRPOW_FTOI(x7,*ix++);
XRPOW_FTOI(x8,*ix++);
}
}
void quantize_xrpow_ISO( const FLOAT8 xr[576], int ix[576], FLOAT8 istep )
{
const FLOAT8 compareval0 = (1.0 - 0.4054)/istep;
int j;
for (j=576;j>0;j--) {
if (compareval0 > *xr) {
*(ix++) = 0;
xr++;
} else {
XRPOW_FTOI( istep*(*(xr++)) + ROUNDFAC , *(ix++) );
}
}
}
#endif