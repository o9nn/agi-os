#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "util.h"
#include "l3side.h"
#include "tables.h"
#include "quantize_pvt.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
static const struct
{
const int region0_count;
const int region1_count;
} subdv_table[ 23 ] =
{
{0, 0},
{0, 0},
{0, 0},
{0, 0},
{0, 0},
{0, 1},
{1, 1},
{1, 1},
{1, 2},
{2, 2},
{2, 3},
{2, 3},
{3, 4},
{3, 4},
{3, 4},
{4, 5},
{4, 5},
{4, 6},
{5, 6},
{5, 6},
{5, 7},
{6, 7},
{6, 7},
};
int
ix_max(const int *ix, const int *end)
{
int max1 = 0, max2 = 0;
do {
int x1 = *ix++;
int x2 = *ix++;
if (max1 < x1)
max1 = x1;
if (max2 < x2)
max2 = x2;
} while (ix < end);
if (max1 < max2)
max1 = max2;
return max1;
}
int
count_bit_ESC(
const int *       ix,
const int * const end,
int         t1,
const int         t2,
int * const s )
{
int linbits = ht[t1].xlen * 65536 + ht[t2].xlen;
int sum = 0, sum2;
do {
int x = *ix++;
int y = *ix++;
if (x != 0) {
if (x > 14) {
x = 15;
sum += linbits;
}
x *= 16;
}
if (y != 0) {
if (y > 14) {
y = 15;
sum += linbits;
}
x += y;
}
sum += largetbl[x];
} while (ix < end);
sum2 = sum & 0xffff;
sum >>= 16;
if (sum > sum2) {
sum = sum2;
t1 = t2;
}
*s += sum;
return t1;
}
inline static int
count_bit_noESC(const int * ix, const int * const end, int * const s)
{
int	sum1 = 0;
const char *hlen1 = ht[1].hlen;
do {
int x = ix[0] * 2 + ix[1];
ix += 2;
sum1 += hlen1[x];
} while (ix < end);
*s += sum1;
return 1;
}
inline static int
count_bit_noESC_from2(
const int *       ix,
const int * const end,
int         t1,
int * const s )
{
unsigned int sum = 0, sum2;
const int xlen = ht[t1].xlen;
const unsigned int *hlen;
if (t1 == 2)
hlen = table23;
else
hlen = table56;
do {
int x = ix[0] * xlen + ix[1];
ix += 2;
sum += hlen[x];
} while (ix < end);
sum2 = sum & 0xffff;
sum >>= 16;
if (sum > sum2) {
sum = sum2;
t1++;
}
*s += sum;
return t1;
}
inline static int
count_bit_noESC_from3(
const int *       ix,
const int * const end,
int         t1,
int * const s )
{
int	sum1 = 0;
int	sum2 = 0;
int	sum3 = 0;
const int xlen = ht[t1].xlen;
const char *hlen1 = ht[t1].hlen;
const char *hlen2 = ht[t1+1].hlen;
const char *hlen3 = ht[t1+2].hlen;
int t;
do {
int x = ix[0] * xlen + ix[1];
ix += 2;
sum1 += hlen1[x];
sum2 += hlen2[x];
sum3 += hlen3[x];
} while (ix < end);
t = t1;
if (sum1 > sum2) {
sum1 = sum2;
t++;
}
if (sum1 > sum3) {
sum1 = sum3;
t = t1+2;
}
*s += sum1;
return t;
}
static int
choose_table_nonMMX(
const int *       ix,
const int * const end,
int * const s )
{
int max;
int choice, choice2;
static const int huf_tbl_noESC[] = {
1, 2, 5, 7, 7,10,10,13,13,13,13,13,13,13,13
};
max = ix_max(ix, end);
switch (max) {
case 0:
return max;
case 1:
return count_bit_noESC(ix, end, s);
case 2:
case 3:
return count_bit_noESC_from2(ix, end, huf_tbl_noESC[max - 1], s);
case 4: case 5: case 6:
case 7: case 8: case 9:
case 10: case 11: case 12:
case 13: case 14: case 15:
return count_bit_noESC_from3(ix, end, huf_tbl_noESC[max - 1], s);
default:
if (max > IXMAX_VAL) {
*s = LARGE_BITS;
return -1;
}
max -= 15;
for (choice2 = 24; choice2 < 32; choice2++) {
if (ht[choice2].linmax >= max) {
break;
}
}
for (choice = choice2 - 8; choice < 24; choice++) {
if (ht[choice].linmax >= max) {
break;
}
}
return count_bit_ESC(ix, end, choice, choice2, s);
}
}
int count_bits_long(lame_internal_flags * const gfc, const int ix[576], gr_info * const gi)
{
int i, a1, a2;
int bits = 0;
i=576;
for (; i > 1; i -= 2)
if (ix[i - 1] | ix[i - 2])
break;
gi->count1 = i;
a1 = a2 = 0;
for (; i > 3; i -= 4) {
int p;
if ((unsigned int)(ix[i-1] | ix[i-2] | ix[i-3] | ix[i-4]) > 1)
break;
p = ((ix[i-4] * 2 + ix[i-3]) * 2 + ix[i-2]) * 2 + ix[i-1];
a1 += t32l[p];
a2 += t33l[p];
}
bits = a1;
gi->count1table_select = 0;
if (a1 > a2) {
bits = a2;
gi->count1table_select = 1;
}
gi->count1bits = bits;
gi->big_values = i;
if (i == 0)
return bits;
if (gi->block_type == SHORT_TYPE) {
a1=3*gfc->scalefac_band.s[3];
if (a1 > gi->big_values) a1 = gi->big_values;
a2 = gi->big_values;
}else if (gi->block_type == NORM_TYPE) {
assert(i <= 576);
a1 = gi->region0_count = gfc->bv_scf[i-2];
a2 = gi->region1_count = gfc->bv_scf[i-1];
a2 = gfc->scalefac_band.l[a1 + a2 + 2];
a1 = gfc->scalefac_band.l[a1 + 1];
if (a2 < i)
gi->table_select[2] = gfc->choose_table(ix + a2, ix + i, &bits);
} else {
gi->region0_count = 7;
gi->region1_count = SBMAX_l -1 - 7 - 1;
a1 = gfc->scalefac_band.l[7 + 1];
a2 = i;
if (a1 > a2) {
a1 = a2;
}
}
a1 = Min(a1,i);
a2 = Min(a2,i);
if (0 < a1)
gi->table_select[0] = gfc->choose_table(ix, ix + a1, &bits);
if (a1 < a2)
gi->table_select[1] = gfc->choose_table(ix + a1, ix + a2, &bits);
return bits;
}
int count_bits(
lame_internal_flags * const gfc,
int     * const ix,
const FLOAT8  * const xr,
gr_info * const cod_info)
{
int bits=0,i;
FLOAT8 w = (IXMAX_VAL) / IPOW20(cod_info->global_gain);
for ( i = 0; i < 576; i++ )  {
if (xr[i] > w)
return LARGE_BITS;
}
if (gfc->quantization)
quantize_xrpow(xr, ix, IPOW20(cod_info->global_gain));
else
quantize_xrpow_ISO(xr, ix, IPOW20(cod_info->global_gain));
bits=count_bits_long(gfc, ix, cod_info);
return bits;
}
inline static void
recalc_divide_init(
const lame_internal_flags * const gfc,
gr_info         cod_info,
int     * const ix,
int             r01_bits[],
int             r01_div [],
int             r0_tbl  [],
int             r1_tbl  [] )
{
int r0, r1, bigv, r0t, r1t, bits;
bigv = cod_info.big_values;
for (r0 = 0; r0 <= 7 + 15; r0++) {
r01_bits[r0] = LARGE_BITS;
}
for (r0 = 0; r0 < 16; r0++) {
int a1 = gfc->scalefac_band.l[r0 + 1], r0bits;
if (a1 >= bigv)
break;
r0bits = cod_info.part2_length;
r0t = gfc->choose_table(ix, ix + a1, &r0bits);
for (r1 = 0; r1 < 8; r1++) {
int a2 = gfc->scalefac_band.l[r0 + r1 + 2];
if (a2 >= bigv)
break;
bits = r0bits;
r1t = gfc->choose_table(ix + a1, ix + a2, &bits);
if (r01_bits[r0 + r1] > bits) {
r01_bits[r0 + r1] = bits;
r01_div[r0 + r1] = r0;
r0_tbl[r0 + r1] = r0t;
r1_tbl[r0 + r1] = r1t;
}
}
}
}
inline static void
recalc_divide_sub(
const lame_internal_flags * const gfc,
const gr_info         cod_info2,
gr_info * const gi,
const int     * const ix,
const int             r01_bits[],
const int             r01_div [],
const int             r0_tbl  [],
const int             r1_tbl  [] )
{
int bits, r2, a2, bigv, r2t;
bigv = cod_info2.big_values;
for (r2 = 2; r2 < SBMAX_l + 1; r2++) {
a2 = gfc->scalefac_band.l[r2];
if (a2 >= bigv)
break;
bits = r01_bits[r2 - 2] + cod_info2.count1bits;
if (gi->part2_3_length <= bits)
break;
r2t = gfc->choose_table(ix + a2, ix + bigv, &bits);
if (gi->part2_3_length <= bits)
continue;
memcpy(gi, &cod_info2, sizeof(gr_info));
gi->part2_3_length = bits;
gi->region0_count = r01_div[r2 - 2];
gi->region1_count = r2 - 2 - r01_div[r2 - 2];
gi->table_select[0] = r0_tbl[r2 - 2];
gi->table_select[1] = r1_tbl[r2 - 2];
gi->table_select[2] = r2t;
}
}
void best_huffman_divide(
const lame_internal_flags * const gfc,
const int             gr,
const int             ch,
gr_info * const gi,
int     * const ix )
{
int i, a1, a2;
gr_info cod_info2;
int r01_bits[7 + 15 + 1];
int r01_div[7 + 15 + 1];
int r0_tbl[7 + 15 + 1];
int r1_tbl[7 + 15 + 1];
if (gi->block_type == SHORT_TYPE && gfc->mode_gr==1)
return;
memcpy(&cod_info2, gi, sizeof(gr_info));
if (gi->block_type == NORM_TYPE) {
recalc_divide_init(gfc, cod_info2, ix, r01_bits,r01_div,r0_tbl,r1_tbl);
recalc_divide_sub(gfc, cod_info2, gi, ix, r01_bits,r01_div,r0_tbl,r1_tbl);
}
i = cod_info2.big_values;
if (i == 0 || (unsigned int)(ix[i-2] | ix[i-1]) > 1)
return;
i = gi->count1 + 2;
if (i > 576)
return;
memcpy(&cod_info2, gi, sizeof(gr_info));
cod_info2.count1 = i;
a1 = a2 = 0;
assert(i <= 576);
for (; i > cod_info2.big_values; i -= 4) {
int p = ((ix[i-4] * 2 + ix[i-3]) * 2 + ix[i-2]) * 2 + ix[i-1];
a1 += t32l[p];
a2 += t33l[p];
}
cod_info2.big_values = i;
cod_info2.count1table_select = 0;
if (a1 > a2) {
a1 = a2;
cod_info2.count1table_select = 1;
}
cod_info2.count1bits = a1;
cod_info2.part2_3_length = a1 + cod_info2.part2_length;
if (cod_info2.block_type == NORM_TYPE)
recalc_divide_sub(gfc, cod_info2, gi, ix, r01_bits,r01_div,r0_tbl,r1_tbl);
else {
a1 = gfc->scalefac_band.l[7 + 1];
if (a1 > i) {
a1 = i;
}
if (a1 > 0)
cod_info2.table_select[0] =
gfc->choose_table(ix, ix + a1, (int *)&cod_info2.part2_3_length);
if (i > a1)
cod_info2.table_select[1] =
gfc->choose_table(ix + a1, ix + i, (int *)&cod_info2.part2_3_length);
if (gi->part2_3_length > cod_info2.part2_3_length)
memcpy(gi, &cod_info2, sizeof(gr_info));
}
}
static const int slen1_n[16] = { 1, 1, 1, 1, 8, 2, 2, 2, 4, 4, 4, 8, 8, 8,16,16 };
static const int slen2_n[16] = { 1, 2, 4, 8, 1, 2, 4, 8, 2, 4, 8, 2, 4, 8, 4, 8 };
void
scfsi_calc(int ch,
III_side_info_t *l3_side,
III_scalefac_t scalefac[2][2])
{
int i, s1, s2, c1, c2;
int sfb;
gr_info *gi = &l3_side->gr[1].ch[ch].tt;
static const int scfsi_band[5] = { 0, 6, 11, 16, 21 };
#if 0
static const int slen1_n[16] = { 0, 1, 1, 1, 8, 2, 2, 2, 4, 4, 4, 8, 8, 8,16,16 };
static const int slen2_n[16] = { 0, 2, 4, 8, 1, 2, 4, 8, 2, 4, 8, 2, 4, 8, 4, 8 };
#endif
for (i = 0; i < 4; i++)
l3_side->scfsi[ch][i] = 0;
for (i = 0; i < (sizeof(scfsi_band) / sizeof(int)) - 1; i++) {
for (sfb = scfsi_band[i]; sfb < scfsi_band[i + 1]; sfb++) {
if (scalefac[0][ch].l[sfb] != scalefac[1][ch].l[sfb])
break;
}
if (sfb == scfsi_band[i + 1]) {
for (sfb = scfsi_band[i]; sfb < scfsi_band[i + 1]; sfb++) {
scalefac[1][ch].l[sfb] = -1;
}
l3_side->scfsi[ch][i] = 1;
}
}
s1 = c1 = 0;
for (sfb = 0; sfb < 11; sfb++) {
if (scalefac[1][ch].l[sfb] < 0)
continue;
c1++;
if (s1 < scalefac[1][ch].l[sfb])
s1 = scalefac[1][ch].l[sfb];
}
s2 = c2 = 0;
for (; sfb < SBPSY_l; sfb++) {
if (scalefac[1][ch].l[sfb] < 0)
continue;
c2++;
if (s2 < scalefac[1][ch].l[sfb])
s2 = scalefac[1][ch].l[sfb];
}
for (i = 0; i < 16; i++) {
if (s1 < slen1_n[i] && s2 < slen2_n[i]) {
int c = slen1_tab[i] * c1 + slen2_tab[i] * c2;
if (gi->part2_length > c) {
gi->part2_length = c;
gi->scalefac_compress = i;
}
}
}
}
void best_scalefac_store(
const lame_internal_flags *gfc,
const int             gr,
const int             ch,
int             l3_enc[2][2][576],
III_side_info_t * const l3_side,
III_scalefac_t          scalefac[2][2] )
{
gr_info *gi = &l3_side->gr[gr].ch[ch].tt;
int sfb,i,j,j2,l,start,end;
for ( sfb = 0; sfb < gi->sfb_lmax; sfb++ ) {
if (scalefac[gr][ch].l[sfb]>0) {
start = gfc->scalefac_band.l[ sfb ];
end   = gfc->scalefac_band.l[ sfb+1 ];
for ( l = start; l < end; l++ ) if (l3_enc[gr][ch][l]!=0) break;
if (l==end) scalefac[gr][ch].l[sfb]=0;
}
}
for ( j=0, sfb = gi->sfb_smin; sfb < SBPSY_s; sfb++ ) {
start = gfc->scalefac_band.s[ sfb ];
end   = gfc->scalefac_band.s[ sfb+1 ];
for ( i = 0; i < 3; i++ ) {
if (scalefac[gr][ch].s[sfb][i]>0) {
j2 = j;
for ( l = start; l < end; l++ )
if (l3_enc[gr][ch][j2++ ]!=0) break;
if (l==end) scalefac[gr][ch].s[sfb][i]=0;
}
j += end-start;
}
}
gi->part2_3_length -= gi->part2_length;
if (!gi->scalefac_scale && !gi->preflag) {
int b, s = 0;
for (sfb = 0; sfb < gi->sfb_lmax; sfb++) {
s |= scalefac[gr][ch].l[sfb];
}
for (sfb = gi->sfb_smin; sfb < SBPSY_s; sfb++) {
for (b = 0; b < 3; b++) {
s |= scalefac[gr][ch].s[sfb][b];
}
}
if (!(s & 1) && s != 0) {
for (sfb = 0; sfb < gi->sfb_lmax; sfb++) {
scalefac[gr][ch].l[sfb] /= 2;
}
for (sfb = gi->sfb_smin; sfb < SBPSY_s; sfb++) {
for (b = 0; b < 3; b++) {
scalefac[gr][ch].s[sfb][b] /= 2;
}
}
gi->scalefac_scale = 1;
gi->part2_length = 99999999;
if (gfc->mode_gr == 2) {
scale_bitcount(&scalefac[gr][ch], gi);
} else {
scale_bitcount_lsf(gfc,&scalefac[gr][ch], gi);
}
}
}
for ( i = 0; i < 4; i++ )
l3_side->scfsi[ch][i] = 0;
if (gfc->mode_gr==2 && gr == 1
&& l3_side->gr[0].ch[ch].tt.block_type != SHORT_TYPE
&& l3_side->gr[1].ch[ch].tt.block_type != SHORT_TYPE) {
scfsi_calc(ch, l3_side, scalefac);
}
gi->part2_3_length += gi->part2_length;
}
static const int scale_short[16] = {
0, 18, 36, 54, 54, 36, 54, 72, 54, 72, 90, 72, 90, 108, 108, 126 };
static const int scale_mixed[16] = {
0, 18, 36, 54, 51, 35, 53, 71, 52, 70, 88, 69, 87, 105, 104, 122 };
static const int scale_long[16] = {
0, 10, 20, 30, 33, 21, 31, 41, 32, 42, 52, 43, 53, 63, 64, 74 };
int scale_bitcount(
III_scalefac_t * const scalefac, gr_info * const cod_info)
{
int i, k, sfb, max_slen1 = 0, max_slen2 = 0, ep = 2;
const int *tab;
if ( cod_info->block_type == SHORT_TYPE ) {
tab = scale_short;
if (cod_info->mixed_block_flag) {
tab = scale_mixed;
for ( sfb = 0 ; sfb < cod_info->sfb_lmax; sfb++ )
if (max_slen1 < scalefac->l[sfb])
max_slen1 = scalefac->l[sfb];
}
for ( i = 0; i < 3; i++ ) {
for ( sfb = cod_info->sfb_smin; sfb < 6; sfb++ )
if (max_slen1 < scalefac->s[sfb][i])
max_slen1 = scalefac->s[sfb][i];
for (sfb = 6; sfb < SBPSY_s; sfb++ )
if (max_slen2 < scalefac->s[sfb][i])
max_slen2 = scalefac->s[sfb][i];
}
}
else
{
tab = scale_long;
for ( sfb = 0; sfb < 11; sfb++ )
if ( scalefac->l[sfb] > max_slen1 )
max_slen1 = scalefac->l[sfb];
if (!cod_info->preflag) {
for ( sfb = 11; sfb < SBPSY_l; sfb++ )
if (scalefac->l[sfb] < pretab[sfb])
break;
if (sfb == SBPSY_l) {
cod_info->preflag = 1;
for ( sfb = 11; sfb < SBPSY_l; sfb++ )
scalefac->l[sfb] -= pretab[sfb];
}
}
for ( sfb = 11; sfb < SBPSY_l; sfb++ )
if ( scalefac->l[sfb] > max_slen2 )
max_slen2 = scalefac->l[sfb];
}
cod_info->part2_length = LARGE_BITS;
for ( k = 0; k < 16; k++ )
{
if ( (max_slen1 < slen1_n[k]) && (max_slen2 < slen2_n[k]) &&
(cod_info->part2_length > tab[k])) {
cod_info->part2_length=tab[k];
cod_info->scalefac_compress=k;
ep=0;
}
}
return ep;
}
static const int max_range_sfac_tab[6][4] =
{
{ 15, 15, 7,  7},
{ 15, 15, 7,  0},
{ 7,  3,  0,  0},
{ 15, 31, 31, 0},
{ 7,  7,  7,  0},
{ 3,  3,  0,  0}
};
int scale_bitcount_lsf(const lame_internal_flags *gfc,
const III_scalefac_t * const scalefac, gr_info * const cod_info)
{
int table_number, row_in_table, partition, nr_sfb, window, over;
int i, sfb, max_sfac[ 4 ];
const int *partition_table;
if ( cod_info->preflag )
table_number = 2;
else
table_number = 0;
for ( i = 0; i < 4; i++ )
max_sfac[i] = 0;
if ( cod_info->block_type == SHORT_TYPE )
{
row_in_table = 1;
partition_table = &nr_of_sfb_block[table_number][row_in_table][0];
for ( sfb = 0, partition = 0; partition < 4; partition++ )
{
nr_sfb = partition_table[ partition ] / 3;
for ( i = 0; i < nr_sfb; i++, sfb++ )
for ( window = 0; window < 3; window++ )
if ( scalefac->s[sfb][window] > max_sfac[partition] )
max_sfac[partition] = scalefac->s[sfb][window];
}
}
else
{
row_in_table = 0;
partition_table = &nr_of_sfb_block[table_number][row_in_table][0];
for ( sfb = 0, partition = 0; partition < 4; partition++ )
{
nr_sfb = partition_table[ partition ];
for ( i = 0; i < nr_sfb; i++, sfb++ )
if ( scalefac->l[sfb] > max_sfac[partition] )
max_sfac[partition] = scalefac->l[sfb];
}
}
for ( over = 0, partition = 0; partition < 4; partition++ )
{
if ( max_sfac[partition] > max_range_sfac_tab[table_number][partition] )
over++;
}
if ( !over )
{
static const int log2tab[] = { 0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4 };
int slen1, slen2, slen3, slen4;
cod_info->sfb_partition_table = nr_of_sfb_block[table_number][row_in_table];
for ( partition = 0; partition < 4; partition++ )
cod_info->slen[partition] = log2tab[max_sfac[partition]];
slen1 = cod_info->slen[ 0 ];
slen2 = cod_info->slen[ 1 ];
slen3 = cod_info->slen[ 2 ];
slen4 = cod_info->slen[ 3 ];
switch ( table_number )
{
case 0:
cod_info->scalefac_compress = (((slen1 * 5) + slen2) << 4)
+ (slen3 << 2)
+ slen4;
break;
case 1:
cod_info->scalefac_compress = 400
+ (((slen1 * 5) + slen2) << 2)
+ slen3;
break;
case 2:
cod_info->scalefac_compress = 500 + (slen1 * 3) + slen2;
break;
default:
ERRORF(gfc,"intensity stereo not implemented yet\n" );
break;
}
}
#ifdef DEBUG
if ( over )
ERRORF(gfc, "---WARNING !! Amplification of some bands over limits\n" );
#endif
if (!over) {
assert( cod_info->sfb_partition_table );
cod_info->part2_length=0;
for ( partition = 0; partition < 4; partition++ )
cod_info->part2_length += cod_info->slen[partition] * cod_info->sfb_partition_table[partition];
}
return over;
}
void huffman_init(lame_internal_flags * const gfc)
{
int i;
gfc->choose_table = choose_table_nonMMX;
#ifdef MMX_choose_table
if (gfc->CPU_features.MMX) {
extern int choose_table_MMX(const int *ix, const int *end, int *s);
gfc->choose_table = choose_table_MMX;
}
#endif
for (i = 2; i <= 576; i += 2) {
int scfb_anz = 0, index;
while (gfc->scalefac_band.l[++scfb_anz] < i)
;
index = subdv_table[scfb_anz].region0_count;
while (gfc->scalefac_band.l[index + 1] > i)
index--;
if (index < 0) {
index = subdv_table[scfb_anz].region0_count;
}
gfc->bv_scf[i-2] = index;
index = subdv_table[scfb_anz].region1_count;
while (gfc->scalefac_band.l[index + gfc->bv_scf[i-2] + 2] > i)
index--;
if (index < 0) {
index = subdv_table[scfb_anz].region1_count;
}
gfc->bv_scf[i-1] = index;
}
}