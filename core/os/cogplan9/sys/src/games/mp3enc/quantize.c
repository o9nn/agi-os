#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <math.h>
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
static int
init_outer_loop(
gr_info *const cod_info,
III_scalefac_t *const scalefac,
const int is_mpeg1,
const FLOAT8 xr[576],
FLOAT8 xrpow[576] )
{
FLOAT8 tmp, sum = 0;
int i;
cod_info->part2_3_length = 0;
cod_info->big_values = 0;
cod_info->count1 = 0;
cod_info->global_gain = 210;
cod_info->scalefac_compress = 0;
cod_info->table_select [0] = 0;
cod_info->table_select [1] = 0;
cod_info->table_select [2] = 0;
cod_info->subblock_gain[0] = 0;
cod_info->subblock_gain[1] = 0;
cod_info->subblock_gain[2] = 0;
cod_info->region0_count = 0;
cod_info->region1_count = 0;
cod_info->preflag = 0;
cod_info->scalefac_scale = 0;
cod_info->count1table_select = 0;
cod_info->part2_length = 0;
if (cod_info->block_type == SHORT_TYPE) {
cod_info->sfb_lmax = 0;
cod_info->sfb_smin = 0;
if (cod_info->mixed_block_flag) {
cod_info->sfb_lmax = is_mpeg1 ? 8 : 6;
cod_info->sfb_smin = 3;
}
} else {
cod_info->sfb_lmax = SBPSY_l;
cod_info->sfb_smin = SBPSY_s;
}
cod_info->count1bits = 0;
cod_info->sfb_partition_table = nr_of_sfb_block[0][0];
cod_info->slen[0] = 0;
cod_info->slen[1] = 0;
cod_info->slen[2] = 0;
cod_info->slen[3] = 0;
memset(scalefac, 0, sizeof(III_scalefac_t));
for (i = 0; i < 576; ++i) {
tmp = fabs (xr[i]);
sum += tmp;
xrpow[i] = sqrt (tmp * sqrt(tmp));
}
return sum > (FLOAT8)1E-20;
}
typedef enum {
BINSEARCH_NONE,
BINSEARCH_UP,
BINSEARCH_DOWN
} binsearchDirection_t;
int
bin_search_StepSize(
lame_internal_flags * const gfc,
gr_info * const cod_info,
const int desired_rate,
const int start,
const FLOAT8 xrpow [576],
int l3enc [576] )
{
int nBits;
int CurrentStep;
int flag_GoneOver = 0;
int StepSize = start;
binsearchDirection_t Direction = BINSEARCH_NONE;
assert(gfc->CurrentStep);
CurrentStep = gfc->CurrentStep;
do {
cod_info->global_gain = StepSize;
nBits = count_bits(gfc,l3enc,xrpow,cod_info);
if (CurrentStep == 1) break;
if (flag_GoneOver) CurrentStep /= 2;
if (nBits > desired_rate) {
if (Direction == BINSEARCH_DOWN && !flag_GoneOver) {
flag_GoneOver = 1;
CurrentStep /= 2;
}
Direction = BINSEARCH_UP;
StepSize += CurrentStep;
if (StepSize > 255) break;
}
else if (nBits < desired_rate) {
if (Direction == BINSEARCH_UP && !flag_GoneOver) {
flag_GoneOver = 1;
CurrentStep /= 2;
}
Direction = BINSEARCH_DOWN;
StepSize -= CurrentStep;
if (StepSize < 0) break;
}
else break;
} while (1);
CurrentStep = start - StepSize;
gfc->CurrentStep = CurrentStep/4 != 0 ? 4 : 2;
return nBits;
}
int
inner_loop(
lame_internal_flags * const gfc,
gr_info * const cod_info,
const int max_bits,
const FLOAT8 xrpow [576],
int l3enc [576] )
{
int bits;
assert(max_bits >= 0);
bits=count_bits(gfc,l3enc,xrpow,cod_info);
while (bits > max_bits) {
cod_info->global_gain++;
bits = count_bits (gfc, l3enc, xrpow, cod_info);
}
return bits;
}
inline
static int
loop_break(
const gr_info * const cod_info,
const III_scalefac_t * const scalefac )
{
unsigned int i, sfb;
for (sfb = 0; sfb < cod_info->sfb_lmax; sfb++)
if (scalefac->l[sfb] == 0)
return 0;
for (sfb = cod_info->sfb_smin; sfb < SBPSY_s; sfb++)
for (i = 0; i < 3; i++)
if (scalefac->s[sfb][i] == 0 && cod_info->subblock_gain[i] == 0)
return 0;
return 1;
}
inline
static int
quant_compare(
const int experimentalX,
const calc_noise_result * const best,
const calc_noise_result * const calc )
{
int better;
switch (experimentalX) {
default:
case 0:
better = calc->over_count < best->over_count
|| ( calc->over_count == best->over_count &&
calc->over_noise < best->over_noise )
|| ( calc->over_count == best->over_count &&
calc->over_noise == best->over_noise &&
calc->tot_noise < best->tot_noise );
break;
case 1:
better = calc->max_noise < best->max_noise;
break;
case 2:
better = calc->tot_noise < best->tot_noise;
break;
case 3:
better = calc->tot_noise < best->tot_noise &&
calc->max_noise < best->max_noise+2;
break;
case 4:
better = ( calc->max_noise <= 0 &&
best->max_noise > 2 )
|| ( calc->max_noise <= 0 &&
best->max_noise < 0 &&
best->max_noise > calc->max_noise-2 &&
calc->tot_noise < best->tot_noise )
|| ( calc->max_noise <= 0 &&
best->max_noise > 0 &&
best->max_noise > calc->max_noise-2 &&
calc->tot_noise < best->tot_noise+best->over_noise )
|| ( calc->max_noise > 0 &&
best->max_noise > -0.5 &&
best->max_noise > calc->max_noise-1 &&
calc->tot_noise+calc->over_noise < best->tot_noise+best->over_noise )
|| ( calc->max_noise > 0 &&
best->max_noise > -1 &&
best->max_noise > calc->max_noise-1.5 &&
calc->tot_noise+calc->over_noise+calc->over_noise < best->tot_noise+best->over_noise+best->over_noise );
break;
case 5:
better = calc->over_noise < best->over_noise
|| ( calc->over_noise == best->over_noise &&
calc->tot_noise < best->tot_noise );
break;
case 6:
better = calc->over_noise < best->over_noise
|| ( calc->over_noise == best->over_noise &&
( calc->max_noise < best->max_noise
|| ( calc->max_noise == best->max_noise &&
calc->tot_noise <= best->tot_noise )
));
break;
case 7:
better = calc->over_count < best->over_count
|| calc->over_noise < best->over_noise;
break;
case 8:
better = calc->klemm_noise < best->klemm_noise;
break;
}
return better;
}
static void
amp_scalefac_bands(
lame_global_flags *gfp,
const gr_info *const cod_info,
III_scalefac_t *const scalefac,
III_psy_xmin *distort,
FLOAT8 xrpow[576] )
{
lame_internal_flags *gfc=gfp->internal_flags;
int start, end, l,i,j,sfb;
FLOAT8 ifqstep34, trigger;
if (cod_info->scalefac_scale == 0) {
ifqstep34 = 1.29683955465100964055;
} else {
ifqstep34 = 1.68179283050742922612;
}
trigger = 0;
for (sfb = 0; sfb < cod_info->sfb_lmax; sfb++) {
if (trigger < distort->l[sfb])
trigger = distort->l[sfb];
}
for (sfb = cod_info->sfb_smin; sfb < SBPSY_s; sfb++) {
for (i = 0; i < 3; i++ ) {
if (trigger < distort->s[sfb][i])
trigger = distort->s[sfb][i];
}
}
switch (gfc->noise_shaping_amp) {
case 2:
break;
case 1:
if (trigger>1.0)
trigger = pow(trigger, .5);
else
trigger *= .95;
break;
case 0:
default:
if (trigger>1.0)
trigger=1.0;
else
trigger *= .95;
break;
}
for (sfb = 0; sfb < cod_info->sfb_lmax; sfb++ ) {
start = gfc->scalefac_band.l[sfb];
end = gfc->scalefac_band.l[sfb+1];
if (distort->l[sfb]>=trigger ) {
scalefac->l[sfb]++;
for ( l = start; l < end; l++ )
xrpow[l] *= ifqstep34;
if (gfc->noise_shaping_amp==2) goto done;
}
}
for ( j=0,sfb = cod_info->sfb_smin; sfb < SBPSY_s; sfb++ ) {
start = gfc->scalefac_band.s[sfb];
end = gfc->scalefac_band.s[sfb+1];
for ( i = 0; i < 3; i++ ) {
int j2 = j;
if ( distort->s[sfb][i]>=trigger) {
scalefac->s[sfb][i]++;
for (l = start; l < end; l++)
xrpow[j2++] *= ifqstep34;
if (gfc->noise_shaping_amp==2) goto done;
}
j += end-start;
}
}
done:
return;
}
static void
inc_scalefac_scale (
const lame_internal_flags * const gfc,
gr_info * const cod_info,
III_scalefac_t * const scalefac,
FLOAT8 xrpow[576] )
{
int start, end, l,i,j;
int sfb;
const FLOAT8 ifqstep34 = 1.29683955465100964055;
for (sfb = 0; sfb < cod_info->sfb_lmax; sfb++) {
int s = scalefac->l[sfb] + (cod_info->preflag ? pretab[sfb] : 0);
if (s & 1) {
s++;
start = gfc->scalefac_band.l[sfb];
end = gfc->scalefac_band.l[sfb+1];
for (l = start; l < end; l++)
xrpow[l] *= ifqstep34;
}
scalefac->l[sfb] = s >> 1;
cod_info->preflag = 0;
}
for (j = 0, sfb = cod_info->sfb_smin; sfb < SBPSY_s; sfb++) {
start = gfc->scalefac_band.s[sfb];
end = gfc->scalefac_band.s[sfb+1];
for (i = 0; i < 3; i++) {
int j2 = j;
if (scalefac->s[sfb][i] & 1) {
scalefac->s[sfb][i]++;
for (l = start; l < end; l++)
xrpow[j2++] *= ifqstep34;
}
scalefac->s[sfb][i] >>= 1;
j += end-start;
}
}
cod_info->scalefac_scale = 1;
}
static int
inc_subblock_gain (
const lame_internal_flags * const gfc,
gr_info * const cod_info,
III_scalefac_t * const scalefac,
FLOAT8 xrpow[576] )
{
int window;
for (window = 0; window < 3; window++) {
int s1, s2, l;
int sfb;
s1 = s2 = 0;
for (sfb = cod_info->sfb_smin; sfb < 6; sfb++) {
if (s1 < scalefac->s[sfb][window])
s1 = scalefac->s[sfb][window];
}
for (; sfb < SBPSY_s; sfb++) {
if (s2 < scalefac->s[sfb][window])
s2 = scalefac->s[sfb][window];
}
if (s1 < 16 && s2 < 8)
continue;
if (cod_info->subblock_gain[window] >= 7)
return 1;
cod_info->subblock_gain[window]++;
for (sfb = cod_info->sfb_smin; sfb < SBMAX_s; sfb++) {
int i, width;
int s = scalefac->s[sfb][window];
FLOAT8 amp;
if (s < 0)
continue;
s = s - (4 >> cod_info->scalefac_scale);
if (s >= 0) {
scalefac->s[sfb][window] = s;
continue;
}
scalefac->s[sfb][window] = 0;
width = gfc->scalefac_band.s[sfb] - gfc->scalefac_band.s[sfb+1];
i = gfc->scalefac_band.s[sfb] * 3 + width * window;
amp = IPOW20(210 + (s << (cod_info->scalefac_scale + 1)));
for (l = 0; l < width; l++) {
xrpow[l] *= amp;
}
}
}
return 0;
}
inline
static int
balance_noise (
lame_global_flags *const gfp,
gr_info * const cod_info,
III_scalefac_t * const scalefac,
III_psy_xmin *distort,
FLOAT8 xrpow[576] )
{
lame_internal_flags *const gfc = (lame_internal_flags *)gfp->internal_flags;
int status;
amp_scalefac_bands ( gfp, cod_info, scalefac, distort, xrpow);
status = loop_break (cod_info, scalefac);
if (status)
return 0;
if (gfc->is_mpeg1)
status = scale_bitcount (scalefac, cod_info);
else
status = scale_bitcount_lsf (gfc, scalefac, cod_info);
if (!status)
return 1;
if (gfc->noise_shaping > 1) {
if (!cod_info->scalefac_scale) {
inc_scalefac_scale (gfc, cod_info, scalefac, xrpow);
status = 0;
} else {
if (cod_info->block_type == SHORT_TYPE ) {
status = inc_subblock_gain (gfc, cod_info, scalefac, xrpow)
|| loop_break (cod_info, scalefac);
}
}
}
if (!status) {
if (gfc->is_mpeg1 == 1)
status = scale_bitcount (scalefac, cod_info);
else
status = scale_bitcount_lsf (gfc, scalefac, cod_info);
}
return !status;
}
static int
outer_loop (
lame_global_flags *gfp,
gr_info * const cod_info,
const FLOAT8 xr[576],
const III_psy_xmin * const l3_xmin,
III_scalefac_t * const scalefac,
FLOAT8 xrpow[576],
int l3enc[576],
const int ch,
const int targ_bits )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_scalefac_t save_scalefac;
gr_info save_cod_info;
FLOAT8 save_xrpow[576];
III_psy_xmin distort;
calc_noise_result noise_info;
calc_noise_result best_noise_info;
int l3_enc_w[576];
int iteration = 0;
int bits_found = 0;
int huff_bits;
int real_bits;
int better;
int over=0;
int notdone = 1;
int copy = 0;
int age = 0;
noise_info.over_count = 100;
noise_info.tot_count = 100;
noise_info.max_noise = 0;
noise_info.tot_noise = 0;
noise_info.over_noise = 0;
best_noise_info.over_count = 100;
bits_found = bin_search_StepSize (gfc, cod_info, targ_bits,
gfc->OldValue[ch], xrpow, l3_enc_w);
gfc->OldValue[ch] = cod_info->global_gain;
do {
iteration ++;
huff_bits = targ_bits - cod_info->part2_length;
if (huff_bits < 0) {
assert(iteration != 1);
break;
}
if (iteration == 1) {
if (bits_found > huff_bits) {
cod_info->global_gain++;
real_bits = inner_loop (gfc, cod_info, huff_bits, xrpow,
l3_enc_w);
} else {
real_bits = bits_found;
}
} else {
real_bits = inner_loop (gfc, cod_info, huff_bits, xrpow,
l3_enc_w);
}
cod_info->part2_3_length = real_bits;
if (gfc->noise_shaping)
over = calc_noise (gfc, xr, l3_enc_w, cod_info, l3_xmin,
scalefac, &distort, &noise_info);
else {
best_noise_info = noise_info;
over = 0;
copy = 0;
memcpy(l3enc, l3_enc_w, sizeof(int)*576);
break;
}
if (iteration == 1)
better = 1;
else
better = quant_compare (gfp->experimentalX,
&best_noise_info, &noise_info);
if (better) {
copy = 0;
best_noise_info = noise_info;
memcpy(l3enc, l3_enc_w, sizeof(int)*576);
age = 0;
}
else
age ++;
if (0==gfc->noise_shaping_stop &&
0==gfp->experimentalX &&
(over == 0 || best_noise_info.over_count == 0) )
break;
if (age > 3 && best_noise_info.over_count == 0)
break;
if (gfc->sfb21_extra) {
if (cod_info->block_type == SHORT_TYPE) {
if (distort.s[SBMAX_s-1][0] > 1 ||
distort.s[SBMAX_s-1][1] > 1 ||
distort.s[SBMAX_s-1][2] > 1) break;
} else {
if (distort.l[SBMAX_l-1] > 1) break;
}
}
if (better) {
copy = 1;
save_scalefac = *scalefac;
save_cod_info = *cod_info;
if (gfp->VBR == vbr_rh || gfp->VBR == vbr_mtrh) {
memcpy(save_xrpow, xrpow, sizeof(FLOAT8)*576);
}
}
notdone = balance_noise (gfp, cod_info, scalefac, &distort, xrpow);
if (notdone == 0)
break;
}
while (1);
if (copy) {
*cod_info = save_cod_info;
*scalefac = save_scalefac;
if (gfp->VBR == vbr_rh || gfp->VBR == vbr_mtrh)
memcpy(xrpow, save_xrpow, sizeof(FLOAT8)*576);
}
cod_info->part2_3_length += cod_info->part2_length;
assert (cod_info->global_gain < 256);
return best_noise_info.over_count;
}
static void
iteration_finish (
lame_internal_flags *gfc,
FLOAT8 xr [2][2][576],
int l3_enc [2][2][576],
III_psy_ratio ratio [2][2],
III_scalefac_t scalefac[2][2],
const int mean_bits )
{
III_side_info_t *l3_side = &gfc->l3_side;
int gr, ch, i;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
gr_info *cod_info = &l3_side->gr[gr].ch[ch].tt;
best_scalefac_store (gfc, gr, ch, l3_enc, l3_side, scalefac);
if (gfc->use_best_huffman == 1)
best_huffman_divide (gfc, gr, ch, cod_info, l3_enc[gr][ch]);
ResvAdjust (gfc, cod_info, l3_side, mean_bits);
for (i = 0; i < 576; i++) {
if (xr[gr][ch][i] < 0) l3_enc[gr][ch][i] *= -1;
}
}
}
ResvFrameEnd (gfc, l3_side, mean_bits);
}
static void
VBR_encode_granule (
lame_global_flags *gfp,
gr_info * const cod_info,
FLOAT8 xr[576],
const III_psy_xmin * const l3_xmin,
III_scalefac_t * const scalefac,
FLOAT8 xrpow[576],
int l3_enc[576],
const int ch,
int min_bits,
int max_bits )
{
gr_info bst_cod_info;
III_scalefac_t bst_scalefac;
FLOAT8 bst_xrpow [576];
int bst_l3_enc[576];
int Max_bits = max_bits;
int real_bits = max_bits+1;
int this_bits = min_bits+(max_bits-min_bits)/2;
int dbits, over;
assert(Max_bits <= MAX_BITS);
bst_cod_info = *cod_info;
memset(&bst_scalefac, 0, sizeof(III_scalefac_t));
memcpy(&bst_xrpow, xrpow, sizeof(FLOAT8)*576);
do {
assert(this_bits >= min_bits);
assert(this_bits <= max_bits);
over = outer_loop ( gfp, cod_info, xr, l3_xmin, scalefac,
xrpow, l3_enc, ch, this_bits );
if (over <= 0) {
real_bits = cod_info->part2_3_length;
bst_cod_info = *cod_info;
bst_scalefac = *scalefac;
memcpy(bst_xrpow, xrpow, sizeof(FLOAT8)*576);
memcpy(bst_l3_enc, l3_enc, sizeof(int)*576);
max_bits = real_bits-32;
dbits = max_bits-min_bits;
this_bits = min_bits+dbits/2;
}
else {
min_bits = this_bits+32;
dbits = max_bits-min_bits;
this_bits = min_bits+dbits/2;
if (dbits>8) {
*cod_info = bst_cod_info;
*scalefac = bst_scalefac;
memcpy(xrpow, bst_xrpow, sizeof(FLOAT8)*576);
}
}
} while (dbits>8);
if (real_bits <= Max_bits) {
*cod_info = bst_cod_info;
*scalefac = bst_scalefac;
memcpy(l3_enc, bst_l3_enc, sizeof(int)*576);
}
assert(cod_info->part2_3_length <= Max_bits);
}
static void
get_framebits (
lame_global_flags *gfp,
int * const analog_mean_bits,
int * const min_mean_bits,
int frameBits[15] )
{
lame_internal_flags *gfc=gfp->internal_flags;
int bitsPerFrame, mean_bits, i;
III_side_info_t *l3_side = &gfc->l3_side;
gfc->bitrate_index = gfc->VBR_min_bitrate;
getframebits (gfp, &bitsPerFrame, &mean_bits);
*min_mean_bits = mean_bits / gfc->channels_out;
gfc->bitrate_index = 1;
getframebits (gfp, &bitsPerFrame, &mean_bits);
*analog_mean_bits = mean_bits / gfc->channels_out;
for (i = 1; i <= gfc->VBR_max_bitrate; i++) {
gfc->bitrate_index = i;
getframebits (gfp, &bitsPerFrame, &mean_bits);
frameBits[i] = ResvFrameBegin (gfp, l3_side, mean_bits, bitsPerFrame);
}
}
inline
static int
calc_min_bits (
lame_global_flags *gfp,
const gr_info * const cod_info,
const int pe,
const FLOAT8 ms_ener_ratio,
const int bands,
const int mch_bits,
const int analog_mean_bits,
const int min_mean_bits,
const int analog_silence,
const int ch )
{
lame_internal_flags *gfc=gfp->internal_flags;
int min_bits, min_pe_bits;
if (gfc->nsPsy.use) return 1;
min_bits = Max (125, min_mean_bits);
if (gfc->mode_ext == MPG_MD_MS_LR && ch == 1)
min_bits = Max (min_bits, mch_bits/5);
if (cod_info->block_type == SHORT_TYPE)
min_pe_bits = (pe-350) * bands/39;
else
min_pe_bits = (pe-350) * bands/22;
if (gfc->mode_ext == MPG_MD_MS_LR && ch == 1) {
FLOAT8 fac = .33 * (.5 - ms_ener_ratio) / .5;
min_pe_bits = (int)(min_pe_bits * ((1-fac)/(1+fac)));
}
min_pe_bits = Min (min_pe_bits, (1820 * gfp->out_samplerate / 44100));
if (analog_silence && !gfp->VBR_hard_min)
min_bits = analog_mean_bits;
else
min_bits = Max (min_bits, min_pe_bits);
return min_bits;
}
inline
static int
calc_max_bits (
const lame_internal_flags * const gfc,
const int frameBits[15],
const int min_bits )
{
int max_bits;
max_bits = frameBits[gfc->VBR_max_bitrate];
max_bits /= gfc->channels_out * gfc->mode_gr;
max_bits = Min (1200 + max_bits, MAX_BITS - 195 * (gfc->channels_out - 1));
max_bits = Max (max_bits, min_bits);
return max_bits;
}
static int
VBR_prepare (
lame_global_flags *gfp,
FLOAT8 pe [2][2],
FLOAT8 ms_ener_ratio [2],
FLOAT8 xr [2][2][576],
III_psy_ratio ratio [2][2],
III_psy_xmin l3_xmin [2][2],
int frameBits [16],
int *analog_mean_bits,
int *min_mean_bits,
int min_bits [2][2],
int max_bits [2][2],
int bands [2][2] )
{
lame_internal_flags *gfc=gfp->internal_flags;
static const FLOAT8 dbQ[10]={-2.,-1.0,-.66,-.33,0.,0.33,.66,1.0,1.33,1.66};
static const FLOAT8 dbQns[10]={- 4,- 3,-2,-1,0,0.7,1.4,2.1,2.8,3.5};
static const FLOAT8 dbQmtrh[10]=
{ -4., -3., -2., -1., 0., 0.5, 1., 1.5, 2., 2.5 };
FLOAT8 masking_lower_db, adjust = 0.0;
int gr, ch;
int used_bits = 0, bits;
int analog_silence = 1;
assert( gfp->VBR_q <= 9 );
assert( gfp->VBR_q >= 0 );
get_framebits (gfp, analog_mean_bits, min_mean_bits, frameBits);
for (gr = 0; gr < gfc->mode_gr; gr++) {
if (gfc->mode_ext == MPG_MD_MS_LR)
ms_convert (xr[gr], xr[gr]);
for (ch = 0; ch < gfc->channels_out; ch++) {
gr_info *cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
if (cod_info->block_type == SHORT_TYPE)
adjust = 5/(1+exp(3.5-pe[gr][ch]/300.))-0.14;
else
adjust = 2/(1+exp(3.5-pe[gr][ch]/300.))-0.05;
if (vbr_mtrh == gfp->VBR) {
masking_lower_db = dbQmtrh[gfp->VBR_q] - adjust;
}
else if (gfc->nsPsy.use && gfp->ATHtype == 0) {
masking_lower_db = dbQns[gfp->VBR_q] - adjust;
}
else {
masking_lower_db = dbQ[gfp->VBR_q] - adjust;
}
gfc->masking_lower = pow (10.0, masking_lower_db * 0.1);
bands[gr][ch] = calc_xmin (gfp, xr[gr][ch], ratio[gr]+ch,
cod_info, l3_xmin[gr]+ch);
if (bands[gr][ch])
analog_silence = 0;
min_bits[gr][ch] = calc_min_bits (gfp, cod_info, (int)pe[gr][ch],
ms_ener_ratio[gr], bands[gr][ch],
0, *analog_mean_bits,
*min_mean_bits, analog_silence, ch);
max_bits[gr][ch] = calc_max_bits (gfc, frameBits, min_bits[gr][ch]);
used_bits += min_bits[gr][ch];
}
}
*min_mean_bits = Max(125, *min_mean_bits);
bits = 0.8*frameBits[gfc->VBR_max_bitrate];
if (used_bits >= bits)
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
min_bits[gr][ch] *= bits;
min_bits[gr][ch] /= used_bits;
if (min_bits[gr][ch] < *min_mean_bits)
min_bits[gr][ch] = *min_mean_bits;
max_bits[gr][ch] *= bits;
max_bits[gr][ch] /= used_bits;
if (max_bits[gr][ch] < *min_mean_bits)
max_bits[gr][ch] = *min_mean_bits;
}
}
return analog_silence;
}
void
VBR_iteration_loop (
lame_global_flags *gfp,
FLOAT8 pe [2][2],
FLOAT8 ms_ener_ratio[2],
FLOAT8 xr [2][2][576],
III_psy_ratio ratio [2][2],
int l3_enc [2][2][576],
III_scalefac_t scalefac [2][2] )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_psy_xmin l3_xmin[2][2];
FLOAT8 xrpow[576];
int bands[2][2];
int frameBits[15];
int bitsPerFrame;
int save_bits[2][2];
int used_bits, used_bits2;
int bits;
int min_bits[2][2], max_bits[2][2];
int analog_mean_bits, min_mean_bits;
int mean_bits;
int ch, num_chan, gr, analog_silence;
int reduce_s_ch, sfb21_extra;
gr_info *cod_info;
III_side_info_t *l3_side = &gfc->l3_side;
if (gfc->mode_ext == MPG_MD_MS_LR && gfp->quality >= 5) {
reduce_s_ch = 1;
num_chan = 1;
} else {
reduce_s_ch = 0;
num_chan = gfc->channels_out;
}
analog_silence = VBR_prepare (gfp, pe, ms_ener_ratio, xr, ratio,
l3_xmin, frameBits, &analog_mean_bits,
&min_mean_bits, min_bits, max_bits, bands);
do {
used_bits = 0;
used_bits2 = 0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < num_chan; ch++) {
int ret;
cod_info = &l3_side->gr[gr].ch[ch].tt;
ret = init_outer_loop(cod_info, &scalefac[gr][ch], gfc->is_mpeg1,
xr[gr][ch], xrpow);
if (ret == 0) {
memset(l3_enc[gr][ch], 0, sizeof(int)*576);
save_bits[gr][ch] = 0;
continue;
}
#if 0
if (gfc->mode_ext == MPG_MD_MS_LR && ch == 1)
min_bits[gr][ch] = Max (min_bits[gr][ch], save_bits[gr][0]/5);
#endif
if (gfp->VBR == vbr_mtrh) {
ret = VBR_noise_shaping2 (gfp, xr[gr][ch], xrpow,
&ratio[gr][ch], l3_enc[gr][ch], 0,
min_bits[gr][ch], max_bits[gr][ch],
&scalefac[gr][ch],
&l3_xmin[gr][ch], gr, ch );
if (ret < 0)
cod_info->part2_3_length = 100000;
}
else
VBR_encode_granule (gfp, cod_info, xr[gr][ch], &l3_xmin[gr][ch],
&scalefac[gr][ch], xrpow, l3_enc[gr][ch],
ch, min_bits[gr][ch], max_bits[gr][ch] );
used_bits += cod_info->part2_3_length;
save_bits[gr][ch] = Min(MAX_BITS, cod_info->part2_3_length);
used_bits2 += Min(MAX_BITS, cod_info->part2_3_length);
}
}
if (reduce_s_ch) {
for (gr = 0; gr < gfc->mode_gr; gr++) {
FLOAT8 fac = .33*(.5-ms_ener_ratio[gr])/.5;
save_bits[gr][1] = (int)(((1-fac)/(1+fac)) * save_bits[gr][0]);
save_bits[gr][1] = Max (analog_mean_bits, save_bits[gr][1]);
used_bits += save_bits[gr][1];
}
}
if (analog_silence && !gfp->VBR_hard_min)
gfc->bitrate_index = 1;
else
gfc->bitrate_index = gfc->VBR_min_bitrate;
for( ; gfc->bitrate_index < gfc->VBR_max_bitrate; gfc->bitrate_index++) {
if (used_bits <= frameBits[gfc->bitrate_index]) break;
}
getframebits (gfp, &bitsPerFrame, &mean_bits);
bits = ResvFrameBegin (gfp, l3_side, mean_bits, bitsPerFrame);
if (used_bits > bits){
if(gfp->VBR == vbr_mtrh) {
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
max_bits[gr][ch] = save_bits[gr][ch];
max_bits[gr][ch] *= frameBits[gfc->bitrate_index];
max_bits[gr][ch] /= used_bits2;
max_bits[gr][ch] = Max(min_bits[gr][ch],max_bits[gr][ch]);
}
}
}
else {
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
int sfb;
cod_info = &l3_side->gr[gr].ch[ch].tt;
if (cod_info->block_type == SHORT_TYPE) {
for (sfb = 0; sfb < SBMAX_s; sfb++) {
l3_xmin[gr][ch].s[sfb][0] *= 1.+.029*sfb*sfb/SBMAX_s/SBMAX_s;
l3_xmin[gr][ch].s[sfb][1] *= 1.+.029*sfb*sfb/SBMAX_s/SBMAX_s;
l3_xmin[gr][ch].s[sfb][2] *= 1.+.029*sfb*sfb/SBMAX_s/SBMAX_s;
}
}
else {
for (sfb = 0; sfb < SBMAX_l; sfb++)
l3_xmin[gr][ch].l[sfb] *= 1.+.029*sfb*sfb/SBMAX_l/SBMAX_l;
}
max_bits[gr][ch] = Max(min_mean_bits, 0.9*max_bits[gr][ch]);
}
}
}
}
} while (used_bits > bits);
sfb21_extra = gfc->sfb21_extra;
gfc->sfb21_extra = 0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
int ret;
cod_info = &l3_side->gr[gr].ch[ch].tt;
if (used_bits <= bits && ! (reduce_s_ch && ch == 1))
continue;
if (used_bits > bits) {
save_bits[gr][ch] *= frameBits[gfc->bitrate_index];
save_bits[gr][ch] /= used_bits;
}
ret = init_outer_loop(cod_info, &scalefac[gr][ch], gfc->is_mpeg1,
xr[gr][ch], xrpow);
if (ret == 0)
{
memset(l3_enc[gr][ch], 0, sizeof(int)*576);
}
else {
outer_loop (gfp, cod_info, xr[gr][ch], &l3_xmin[gr][ch],
&scalefac[gr][ch], xrpow, l3_enc[gr][ch], ch,
save_bits[gr][ch]);
}
}
}
gfc->sfb21_extra = sfb21_extra;
iteration_finish (gfc, xr, l3_enc, ratio, scalefac, mean_bits);
}
static void
calc_target_bits (
lame_global_flags * gfp,
FLOAT8 pe [2][2],
FLOAT8 ms_ener_ratio [2],
int targ_bits [2][2],
int *analog_silence_bits,
int *max_frame_bits )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_side_info_t *l3_side = &gfc->l3_side;
FLOAT8 res_factor;
int gr, ch, totbits, mean_bits, bitsPerFrame;
gfc->bitrate_index = gfc->VBR_max_bitrate;
getframebits (gfp, &bitsPerFrame, &mean_bits);
*max_frame_bits = ResvFrameBegin (gfp, l3_side, mean_bits, bitsPerFrame);
gfc->bitrate_index = 1;
getframebits (gfp, &bitsPerFrame, &mean_bits);
*analog_silence_bits = mean_bits / gfc->channels_out;
mean_bits = gfp->VBR_mean_bitrate_kbps * gfp->framesize * 1000;
mean_bits /= gfp->out_samplerate;
mean_bits -= gfc->sideinfo_len*8;
mean_bits /= gfc->mode_gr;
res_factor = .90 + .10 * (11.0 - gfp->compression_ratio) / (11.0 - 5.5);
if (res_factor < .90)
res_factor = .90;
if (res_factor > 1.00)
res_factor = 1.00;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
targ_bits[gr][ch] = res_factor * (mean_bits / gfc->channels_out);
if (pe[gr][ch] > 700) {
int add_bits = (pe[gr][ch] - 700) / 1.4;
gr_info *cod_info = &l3_side->gr[gr].ch[ch].tt;
targ_bits[gr][ch] = res_factor * (mean_bits / gfc->channels_out);
if (cod_info->block_type == SHORT_TYPE) {
if (add_bits < mean_bits/4)
add_bits = mean_bits/4;
}
if (add_bits > mean_bits*3/4)
add_bits = mean_bits*3/4;
else
if (add_bits < 0)
add_bits = 0;
targ_bits[gr][ch] += add_bits;
}
}
}
if (gfc->mode_ext == MPG_MD_MS_LR)
for (gr = 0; gr < gfc->mode_gr; gr++) {
reduce_side (targ_bits[gr], ms_ener_ratio[gr], mean_bits,
MAX_BITS);
}
totbits=0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
if (targ_bits[gr][ch] > MAX_BITS)
targ_bits[gr][ch] = MAX_BITS;
totbits += targ_bits[gr][ch];
}
}
if (totbits > *max_frame_bits) {
for(gr = 0; gr < gfc->mode_gr; gr++) {
for(ch = 0; ch < gfc->channels_out; ch++) {
targ_bits[gr][ch] *= *max_frame_bits;
targ_bits[gr][ch] /= totbits;
}
}
}
}
void
ABR_iteration_loop(
lame_global_flags *gfp,
FLOAT8 pe [2][2],
FLOAT8 ms_ener_ratio[2],
FLOAT8 xr [2][2][576],
III_psy_ratio ratio [2][2],
int l3_enc [2][2][576],
III_scalefac_t scalefac [2][2] )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_psy_xmin l3_xmin;
FLOAT8 xrpow[576];
int targ_bits[2][2];
int bitsPerFrame, mean_bits, totbits, max_frame_bits;
int ch, gr, ath_over, ret;
int analog_silence_bits;
gr_info *cod_info = NULL;
III_side_info_t *l3_side = &gfc->l3_side;
calc_target_bits (gfp, pe, ms_ener_ratio, targ_bits,
&analog_silence_bits, &max_frame_bits);
totbits=0;
for (gr = 0; gr < gfc->mode_gr; gr++) {
if (gfc->mode_ext == MPG_MD_MS_LR)
ms_convert (xr[gr], xr[gr]);
for (ch = 0; ch < gfc->channels_out; ch++) {
cod_info = &l3_side->gr[gr].ch[ch].tt;
ret = init_outer_loop(cod_info, &scalefac[gr][ch], gfc->is_mpeg1,
xr[gr][ch], xrpow);
if (ret == 0) {
memset(l3_enc[gr][ch], 0, sizeof(int)*576);
}
else {
ath_over = calc_xmin (gfp, xr[gr][ch], &ratio[gr][ch],
cod_info, &l3_xmin);
if (0 == ath_over)
targ_bits[gr][ch] = analog_silence_bits;
outer_loop (gfp, cod_info, xr[gr][ch], &l3_xmin,
&scalefac[gr][ch], xrpow, l3_enc[gr][ch],
ch, targ_bits[gr][ch]);
}
totbits += cod_info->part2_3_length;
}
}
for (gfc->bitrate_index = gfc->VBR_min_bitrate ;
gfc->bitrate_index <= gfc->VBR_max_bitrate;
gfc->bitrate_index++ ) {
getframebits (gfp, &bitsPerFrame, &mean_bits);
max_frame_bits = ResvFrameBegin (gfp, l3_side, mean_bits, bitsPerFrame);
if (totbits <= max_frame_bits) break;
}
assert (gfc->bitrate_index <= gfc->VBR_max_bitrate);
iteration_finish (gfc, xr, l3_enc, ratio, scalefac, mean_bits);
}
void
iteration_loop(
lame_global_flags *gfp,
FLOAT8 pe [2][2],
FLOAT8 ms_ener_ratio[2],
FLOAT8 xr [2][2][576],
III_psy_ratio ratio [2][2],
int l3_enc [2][2][576],
III_scalefac_t scalefac [2][2] )
{
lame_internal_flags *gfc=gfp->internal_flags;
III_psy_xmin l3_xmin[2];
FLOAT8 xrpow[576];
int targ_bits[2];
int bitsPerFrame;
int mean_bits, max_bits, bit_rate;
int gr, ch, i;
III_side_info_t *l3_side = &gfc->l3_side;
gr_info *cod_info;
bit_rate = bitrate_table [gfp->version] [gfc->bitrate_index];
getframebits (gfp, &bitsPerFrame, &mean_bits);
ResvFrameBegin (gfp, l3_side, mean_bits, bitsPerFrame );
for (gr = 0; gr < gfc->mode_gr; gr++) {
max_bits = on_pe (gfp, pe, l3_side, targ_bits, mean_bits, gr);
if (gfc->mode_ext == MPG_MD_MS_LR) {
ms_convert (xr[gr], xr[gr]);
reduce_side (targ_bits, ms_ener_ratio[gr], mean_bits, max_bits);
}
for (ch=0 ; ch < gfc->channels_out ; ch ++) {
cod_info = &l3_side->gr[gr].ch[ch].tt;
i = init_outer_loop(cod_info, &scalefac[gr][ch], gfc->is_mpeg1,
xr[gr][ch], xrpow);
if (i == 0) {
memset(l3_enc[gr][ch], 0, sizeof(int)*576);
}
else {
calc_xmin (gfp, xr[gr][ch], &ratio[gr][ch], cod_info,
&l3_xmin[ch]);
outer_loop (gfp, cod_info, xr[gr][ch], &l3_xmin[ch],
&scalefac[gr][ch], xrpow, l3_enc[gr][ch],
ch, targ_bits[ch]);
}
assert (cod_info->part2_3_length <= MAX_BITS);
best_scalefac_store (gfc, gr, ch, l3_enc, l3_side, scalefac);
if (gfc->use_best_huffman == 1)
best_huffman_divide (gfc, gr, ch, cod_info, l3_enc[gr][ch]);
#undef NORES_TEST
#ifndef NORES_TEST
ResvAdjust (gfc, cod_info, l3_side, mean_bits);
#endif
for (i = 0; i < 576; i++) {
if (xr[gr][ch][i] < 0) l3_enc[gr][ch][i] *= -1;
}
}
}
#ifdef NORES_TEST
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
cod_info = &l3_side->gr[gr].ch[ch].tt;
ResvAdjust (gfc, cod_info, l3_side, mean_bits);
}
}
#endif
ResvFrameEnd (gfc, l3_side, mean_bits);
}