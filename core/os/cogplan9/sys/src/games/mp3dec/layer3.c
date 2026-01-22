# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif
# include "global.h"
# include "fixed.h"
# include "bit.h"
# include "stream.h"
# include "frame.h"
# include "huffman.h"
# include "layer3.h"
#define CHAR_BIT 8
enum {
count1table_select = 0x01,
scalefac_scale     = 0x02,
preflag	     = 0x04,
mixed_block_flag   = 0x08
};
enum {
I_STEREO  = 0x1,
MS_STEREO = 0x2
};
struct sideinfo {
unsigned int main_data_begin;
unsigned int private_bits;
unsigned char scfsi[2];
struct granule {
struct channel {
unsigned short part2_3_length;
unsigned short big_values;
unsigned short global_gain;
unsigned short scalefac_compress;
unsigned char flags;
unsigned char block_type;
unsigned char table_select[3];
unsigned char subblock_gain[3];
unsigned char region0_count;
unsigned char region1_count;
unsigned char scalefac[39];
} ch[2];
} gr[2];
};
static
struct {
unsigned char slen1;
unsigned char slen2;
} const sflen_table[16] = {
{ 0, 0 }, { 0, 1 }, { 0, 2 }, { 0, 3 },
{ 3, 0 }, { 1, 1 }, { 1, 2 }, { 1, 3 },
{ 2, 1 }, { 2, 2 }, { 2, 3 }, { 3, 1 },
{ 3, 2 }, { 3, 3 }, { 4, 2 }, { 4, 3 }
};
static
unsigned char const nsfb_table[6][3][4] = {
{ {  6,  5,  5, 5 },
{  9,  9,  9, 9 },
{  6,  9,  9, 9 } },
{ {  6,  5,  7, 3 },
{  9,  9, 12, 6 },
{  6,  9, 12, 6 } },
{ { 11, 10,  0, 0 },
{ 18, 18,  0, 0 },
{ 15, 18,  0, 0 } },
{ {  7,  7,  7, 0 },
{ 12, 12, 12, 0 },
{  6, 15, 12, 0 } },
{ {  6,  6,  6, 3 },
{ 12,  9,  9, 6 },
{  6, 12,  9, 6 } },
{ {  8,  8,  5, 0 },
{ 15, 12,  9, 0 },
{  6, 18,  9, 0 } }
};
static
unsigned char const sfb_48000_long[] = {
4,  4,  4,  4,  4,  4,  6,  6,  6,   8,  10,
12, 16, 18, 22, 28, 34, 40, 46, 54,  54, 192
};
static
unsigned char const sfb_44100_long[] = {
4,  4,  4,  4,  4,  4,  6,  6,  8,   8,  10,
12, 16, 20, 24, 28, 34, 42, 50, 54,  76, 158
};
static
unsigned char const sfb_32000_long[] = {
4,  4,  4,  4,  4,  4,  6,  6,  8,  10,  12,
16, 20, 24, 30, 38, 46, 56, 68, 84, 102,  26
};
static
unsigned char const sfb_48000_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  6,
6,  6,  6,  6,  6, 10, 10, 10, 12, 12, 12, 14, 14,
14, 16, 16, 16, 20, 20, 20, 26, 26, 26, 66, 66, 66
};
static
unsigned char const sfb_44100_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  6,
6,  6,  8,  8,  8, 10, 10, 10, 12, 12, 12, 14, 14,
14, 18, 18, 18, 22, 22, 22, 30, 30, 30, 56, 56, 56
};
static
unsigned char const sfb_32000_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  6,
6,  6,  8,  8,  8, 12, 12, 12, 16, 16, 16, 20, 20,
20, 26, 26, 26, 34, 34, 34, 42, 42, 42, 12, 12, 12
};
static
unsigned char const sfb_48000_mixed[] = {
4,  4,  4,  4,  4,  4,  6,  6,
4,  4,  4,  6,  6,  6,  6,  6,  6, 10,
10, 10, 12, 12, 12, 14, 14, 14, 16, 16,
16, 20, 20, 20, 26, 26, 26, 66, 66, 66
};
static
unsigned char const sfb_44100_mixed[] = {
4,  4,  4,  4,  4,  4,  6,  6,
4,  4,  4,  6,  6,  6,  8,  8,  8, 10,
10, 10, 12, 12, 12, 14, 14, 14, 18, 18,
18, 22, 22, 22, 30, 30, 30, 56, 56, 56
};
static
unsigned char const sfb_32000_mixed[] = {
4,  4,  4,  4,  4,  4,  6,  6,
4,  4,  4,  6,  6,  6,  8,  8,  8, 12,
12, 12, 16, 16, 16, 20, 20, 20, 26, 26,
26, 34, 34, 34, 42, 42, 42, 12, 12, 12
};
static
unsigned char const sfb_24000_long[] = {
6,  6,  6,  6,  6,  6,  8, 10, 12,  14,  16,
18, 22, 26, 32, 38, 46, 54, 62, 70,  76,  36
};
static
unsigned char const sfb_22050_long[] = {
6,  6,  6,  6,  6,  6,  8, 10, 12,  14,  16,
20, 24, 28, 32, 38, 46, 52, 60, 68,  58,  54
};
# define sfb_16000_long  sfb_22050_long
static
unsigned char const sfb_24000_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  6,  6,  6,  8,
8,  8, 10, 10, 10, 12, 12, 12, 14, 14, 14, 18, 18,
18, 24, 24, 24, 32, 32, 32, 44, 44, 44, 12, 12, 12
};
static
unsigned char const sfb_22050_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  6,  6,  6,  6,
6,  6,  8,  8,  8, 10, 10, 10, 14, 14, 14, 18, 18,
18, 26, 26, 26, 32, 32, 32, 42, 42, 42, 18, 18, 18
};
static
unsigned char const sfb_16000_short[] = {
4,  4,  4,  4,  4,  4,  4,  4,  4,  6,  6,  6,  8,
8,  8, 10, 10, 10, 12, 12, 12, 14, 14, 14, 18, 18,
18, 24, 24, 24, 30, 30, 30, 40, 40, 40, 18, 18, 18
};
static
unsigned char const sfb_24000_mixed[] = {
6,  6,  6,  6,  6,  6,
6,  6,  6,  8,  8,  8, 10, 10, 10, 12,
12, 12, 14, 14, 14, 18, 18, 18, 24, 24,
24, 32, 32, 32, 44, 44, 44, 12, 12, 12
};
static
unsigned char const sfb_22050_mixed[] = {
6,  6,  6,  6,  6,  6,
6,  6,  6,  6,  6,  6,  8,  8,  8, 10,
10, 10, 14, 14, 14, 18, 18, 18, 26, 26,
26, 32, 32, 32, 42, 42, 42, 18, 18, 18
};
static
unsigned char const sfb_16000_mixed[] = {
6,  6,  6,  6,  6,  6,
6,  6,  6,  8,  8,  8, 10, 10, 10, 12,
12, 12, 14, 14, 14, 18, 18, 18, 24, 24,
24, 30, 30, 30, 40, 40, 40, 18, 18, 18
};
# define sfb_12000_long  sfb_16000_long
# define sfb_11025_long  sfb_12000_long
static
unsigned char const sfb_8000_long[] = {
12, 12, 12, 12, 12, 12, 16, 20, 24,  28,  32,
40, 48, 56, 64, 76, 90,  2,  2,  2,   2,   2
};
# define sfb_12000_short  sfb_16000_short
# define sfb_11025_short  sfb_12000_short
static
unsigned char const sfb_8000_short[] = {
8,  8,  8,  8,  8,  8,  8,  8,  8, 12, 12, 12, 16,
16, 16, 20, 20, 20, 24, 24, 24, 28, 28, 28, 36, 36,
36,  2,  2,  2,  2,  2,  2,  2,  2,  2, 26, 26, 26
};
# define sfb_12000_mixed  sfb_16000_mixed
# define sfb_11025_mixed  sfb_12000_mixed
static
unsigned char const sfb_8000_mixed[] = {
12, 12, 12,
4,  4,  4,  8,  8,  8, 12, 12, 12, 16, 16, 16,
20, 20, 20, 24, 24, 24, 28, 28, 28, 36, 36, 36,
2,  2,  2,  2,  2,  2,  2,  2,  2, 26, 26, 26
};
static
struct {
unsigned char const *l;
unsigned char const *s;
unsigned char const *m;
} const sfbwidth_table[9] = {
{ sfb_48000_long, sfb_48000_short, sfb_48000_mixed },
{ sfb_44100_long, sfb_44100_short, sfb_44100_mixed },
{ sfb_32000_long, sfb_32000_short, sfb_32000_mixed },
{ sfb_24000_long, sfb_24000_short, sfb_24000_mixed },
{ sfb_22050_long, sfb_22050_short, sfb_22050_mixed },
{ sfb_16000_long, sfb_16000_short, sfb_16000_mixed },
{ sfb_12000_long, sfb_12000_short, sfb_12000_mixed },
{ sfb_11025_long, sfb_11025_short, sfb_11025_mixed },
{  sfb_8000_long,  sfb_8000_short,  sfb_8000_mixed }
};
static
unsigned char const pretab[22] = {
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 2, 0
};
static
struct fixedfloat {
unsigned long mantissa;
unsigned short exponent;
} const rq_table[8207] = {
# include "rq_table.dat"
};
static
mad_fixed_t const root_table[7] = {
MAD_F(0x09837f05) ,
MAD_F(0x0b504f33) ,
MAD_F(0x0d744fcd) ,
MAD_F(0x10000000) ,
MAD_F(0x1306fe0a) ,
MAD_F(0x16a09e66) ,
MAD_F(0x1ae89f99)
};
static
mad_fixed_t const cs[8] = {
+MAD_F(0x0db84a81) , +MAD_F(0x0e1b9d7f) ,
+MAD_F(0x0f31adcf) , +MAD_F(0x0fbba815) ,
+MAD_F(0x0feda417) , +MAD_F(0x0ffc8fc8) ,
+MAD_F(0x0fff964c) , +MAD_F(0x0ffff8d3)
};
static
mad_fixed_t const ca[8] = {
-MAD_F(0x083b5fe7) , -MAD_F(0x078c36d2) ,
-MAD_F(0x05039814) , -MAD_F(0x02e91dd1) ,
-MAD_F(0x0183603a) , -MAD_F(0x00a7cb87) ,
-MAD_F(0x003a2847) , -MAD_F(0x000f27b4)
};
static
mad_fixed_t const imdct_s[6][6] = {
# include "imdct_s.dat"
};
# if !defined(ASO_IMDCT)
static
mad_fixed_t const window_l[36] = {
MAD_F(0x00b2aa3e) , MAD_F(0x0216a2a2) ,
MAD_F(0x03768962) , MAD_F(0x04cfb0e2) ,
MAD_F(0x061f78aa) , MAD_F(0x07635284) ,
MAD_F(0x0898c779) , MAD_F(0x09bd7ca0) ,
MAD_F(0x0acf37ad) , MAD_F(0x0bcbe352) ,
MAD_F(0x0cb19346) , MAD_F(0x0d7e8807) ,
MAD_F(0x0e313245) , MAD_F(0x0ec835e8) ,
MAD_F(0x0f426cb5) , MAD_F(0x0f9ee890) ,
MAD_F(0x0fdcf549) , MAD_F(0x0ffc19fd) ,
MAD_F(0x0ffc19fd) , MAD_F(0x0fdcf549) ,
MAD_F(0x0f9ee890) , MAD_F(0x0f426cb5) ,
MAD_F(0x0ec835e8) , MAD_F(0x0e313245) ,
MAD_F(0x0d7e8807) , MAD_F(0x0cb19346) ,
MAD_F(0x0bcbe352) , MAD_F(0x0acf37ad) ,
MAD_F(0x09bd7ca0) , MAD_F(0x0898c779) ,
MAD_F(0x07635284) , MAD_F(0x061f78aa) ,
MAD_F(0x04cfb0e2) , MAD_F(0x03768962) ,
MAD_F(0x0216a2a2) , MAD_F(0x00b2aa3e) ,
};
# endif
static
mad_fixed_t const window_s[12] = {
MAD_F(0x0216a2a2) , MAD_F(0x061f78aa) ,
MAD_F(0x09bd7ca0) , MAD_F(0x0cb19346) ,
MAD_F(0x0ec835e8) , MAD_F(0x0fdcf549) ,
MAD_F(0x0fdcf549) , MAD_F(0x0ec835e8) ,
MAD_F(0x0cb19346) , MAD_F(0x09bd7ca0) ,
MAD_F(0x061f78aa) , MAD_F(0x0216a2a2) ,
};
static
mad_fixed_t const is_table[7] = {
MAD_F(0x00000000) ,
MAD_F(0x0361962f) ,
MAD_F(0x05db3d74) ,
MAD_F(0x08000000) ,
MAD_F(0x0a24c28c) ,
MAD_F(0x0c9e69d1) ,
MAD_F(0x10000000)
};
static
mad_fixed_t const is_lsf_table[2][15] = {
{
MAD_F(0x0d744fcd) ,
MAD_F(0x0b504f33) ,
MAD_F(0x09837f05) ,
MAD_F(0x08000000) ,
MAD_F(0x06ba27e6) ,
MAD_F(0x05a8279a) ,
MAD_F(0x04c1bf83) ,
MAD_F(0x04000000) ,
MAD_F(0x035d13f3) ,
MAD_F(0x02d413cd) ,
MAD_F(0x0260dfc1) ,
MAD_F(0x02000000) ,
MAD_F(0x01ae89fa) ,
MAD_F(0x016a09e6) ,
MAD_F(0x01306fe1)
}, {
MAD_F(0x0b504f33) ,
MAD_F(0x08000000) ,
MAD_F(0x05a8279a) ,
MAD_F(0x04000000) ,
MAD_F(0x02d413cd) ,
MAD_F(0x02000000) ,
MAD_F(0x016a09e6) ,
MAD_F(0x01000000) ,
MAD_F(0x00b504f3) ,
MAD_F(0x00800000) ,
MAD_F(0x005a827a) ,
MAD_F(0x00400000) ,
MAD_F(0x002d413d) ,
MAD_F(0x00200000) ,
MAD_F(0x0016a09e)
}
};
static
enum mad_error III_sideinfo(struct mad_bitptr *ptr, unsigned int nch,
int lsf, struct sideinfo *si,
unsigned int *data_bitlen,
unsigned int *priv_bitlen)
{
unsigned int ngr, gr, ch, i;
enum mad_error result = MAD_ERROR_NONE;
*data_bitlen = 0;
*priv_bitlen = lsf ? ((nch == 1) ? 1 : 2) : ((nch == 1) ? 5 : 3);
si->main_data_begin = mad_bit_read(ptr, lsf ? 8 : 9);
si->private_bits    = mad_bit_read(ptr, *priv_bitlen);
ngr = 1;
if (!lsf) {
ngr = 2;
for (ch = 0; ch < nch; ++ch)
si->scfsi[ch] = mad_bit_read(ptr, 4);
}
for (gr = 0; gr < ngr; ++gr) {
struct granule *granule = &si->gr[gr];
for (ch = 0; ch < nch; ++ch) {
struct channel *channel = &granule->ch[ch];
channel->part2_3_length    = mad_bit_read(ptr, 12);
channel->big_values        = mad_bit_read(ptr, 9);
channel->global_gain       = mad_bit_read(ptr, 8);
channel->scalefac_compress = mad_bit_read(ptr, lsf ? 9 : 4);
*data_bitlen += channel->part2_3_length;
if (channel->big_values > 288 && result == 0)
result = MAD_ERROR_BADBIGVALUES;
channel->flags = 0;
if (mad_bit_read(ptr, 1)) {
channel->block_type = mad_bit_read(ptr, 2);
if (channel->block_type == 0 && result == 0)
result = MAD_ERROR_BADBLOCKTYPE;
if (!lsf && channel->block_type == 2 && si->scfsi[ch] && result == 0)
result = MAD_ERROR_BADSCFSI;
channel->region0_count = 7;
channel->region1_count = 36;
if (mad_bit_read(ptr, 1))
channel->flags |= mixed_block_flag;
else if (channel->block_type == 2)
channel->region0_count = 8;
for (i = 0; i < 2; ++i)
channel->table_select[i] = mad_bit_read(ptr, 5);
# if defined(DEBUG)
channel->table_select[2] = 4;
# endif
for (i = 0; i < 3; ++i)
channel->subblock_gain[i] = mad_bit_read(ptr, 3);
}
else {
channel->block_type = 0;
for (i = 0; i < 3; ++i)
channel->table_select[i] = mad_bit_read(ptr, 5);
channel->region0_count = mad_bit_read(ptr, 4);
channel->region1_count = mad_bit_read(ptr, 3);
}
channel->flags |= mad_bit_read(ptr, lsf ? 2 : 3);
}
}
return result;
}
static
unsigned int III_scalefactors_lsf(struct mad_bitptr *ptr,
struct channel *channel,
struct channel *gr1ch, int mode_extension)
{
struct mad_bitptr start;
unsigned int scalefac_compress, index, slen[4], part, n, i;
unsigned char const *nsfb;
start = *ptr;
scalefac_compress = channel->scalefac_compress;
index = (channel->block_type == 2) ?
((channel->flags & mixed_block_flag) ? 2 : 1) : 0;
if (!((mode_extension & I_STEREO) && gr1ch)) {
if (scalefac_compress < 400) {
slen[0] = (scalefac_compress >> 4) / 5;
slen[1] = (scalefac_compress >> 4) % 5;
slen[2] = (scalefac_compress % 16) >> 2;
slen[3] =  scalefac_compress %  4;
nsfb = nsfb_table[0][index];
}
else if (scalefac_compress < 500) {
scalefac_compress -= 400;
slen[0] = (scalefac_compress >> 2) / 5;
slen[1] = (scalefac_compress >> 2) % 5;
slen[2] =  scalefac_compress %  4;
slen[3] = 0;
nsfb = nsfb_table[1][index];
}
else {
scalefac_compress -= 500;
slen[0] = scalefac_compress / 3;
slen[1] = scalefac_compress % 3;
slen[2] = 0;
slen[3] = 0;
channel->flags |= preflag;
nsfb = nsfb_table[2][index];
}
n = 0;
for (part = 0; part < 4; ++part) {
for (i = 0; i < nsfb[part]; ++i)
channel->scalefac[n++] = mad_bit_read(ptr, slen[part]);
}
while (n < 39)
channel->scalefac[n++] = 0;
}
else {
scalefac_compress >>= 1;
if (scalefac_compress < 180) {
slen[0] =  scalefac_compress / 36;
slen[1] = (scalefac_compress % 36) / 6;
slen[2] = (scalefac_compress % 36) % 6;
slen[3] = 0;
nsfb = nsfb_table[3][index];
}
else if (scalefac_compress < 244) {
scalefac_compress -= 180;
slen[0] = (scalefac_compress % 64) >> 4;
slen[1] = (scalefac_compress % 16) >> 2;
slen[2] =  scalefac_compress %  4;
slen[3] = 0;
nsfb = nsfb_table[4][index];
}
else {
scalefac_compress -= 244;
slen[0] = scalefac_compress / 3;
slen[1] = scalefac_compress % 3;
slen[2] = 0;
slen[3] = 0;
nsfb = nsfb_table[5][index];
}
n = 0;
for (part = 0; part < 4; ++part) {
unsigned int max, is_pos;
max = (1 << slen[part]) - 1;
for (i = 0; i < nsfb[part]; ++i) {
is_pos = mad_bit_read(ptr, slen[part]);
channel->scalefac[n] = is_pos;
gr1ch->scalefac[n++] = (is_pos == max);
}
}
while (n < 39) {
channel->scalefac[n] = 0;
gr1ch->scalefac[n++] = 0;
}
}
return mad_bit_length(&start, ptr);
}
static
unsigned int III_scalefactors(struct mad_bitptr *ptr, struct channel *channel,
struct channel const *gr0ch, unsigned int scfsi)
{
struct mad_bitptr start;
unsigned int slen1, slen2, sfbi;
start = *ptr;
slen1 = sflen_table[channel->scalefac_compress].slen1;
slen2 = sflen_table[channel->scalefac_compress].slen2;
if (channel->block_type == 2) {
unsigned int nsfb;
sfbi = 0;
nsfb = (channel->flags & mixed_block_flag) ? 8 + 3 * 3 : 6 * 3;
while (nsfb--)
channel->scalefac[sfbi++] = mad_bit_read(ptr, slen1);
nsfb = 6 * 3;
while (nsfb--)
channel->scalefac[sfbi++] = mad_bit_read(ptr, slen2);
nsfb = 1 * 3;
while (nsfb--)
channel->scalefac[sfbi++] = 0;
}
else {
if (scfsi & 0x8) {
for (sfbi = 0; sfbi < 6; ++sfbi)
channel->scalefac[sfbi] = gr0ch->scalefac[sfbi];
}
else {
for (sfbi = 0; sfbi < 6; ++sfbi)
channel->scalefac[sfbi] = mad_bit_read(ptr, slen1);
}
if (scfsi & 0x4) {
for (sfbi = 6; sfbi < 11; ++sfbi)
channel->scalefac[sfbi] = gr0ch->scalefac[sfbi];
}
else {
for (sfbi = 6; sfbi < 11; ++sfbi)
channel->scalefac[sfbi] = mad_bit_read(ptr, slen1);
}
if (scfsi & 0x2) {
for (sfbi = 11; sfbi < 16; ++sfbi)
channel->scalefac[sfbi] = gr0ch->scalefac[sfbi];
}
else {
for (sfbi = 11; sfbi < 16; ++sfbi)
channel->scalefac[sfbi] = mad_bit_read(ptr, slen2);
}
if (scfsi & 0x1) {
for (sfbi = 16; sfbi < 21; ++sfbi)
channel->scalefac[sfbi] = gr0ch->scalefac[sfbi];
}
else {
for (sfbi = 16; sfbi < 21; ++sfbi)
channel->scalefac[sfbi] = mad_bit_read(ptr, slen2);
}
channel->scalefac[21] = 0;
}
return mad_bit_length(&start, ptr);
}
static
void III_exponents(struct channel const *channel,
unsigned char const *sfbwidth, signed int exponents[39])
{
signed int gain;
unsigned int scalefac_multiplier, sfbi;
gain = (signed int) channel->global_gain - 210;
scalefac_multiplier = (channel->flags & scalefac_scale) ? 2 : 1;
if (channel->block_type == 2) {
unsigned int l;
signed int gain0, gain1, gain2;
sfbi = l = 0;
if (channel->flags & mixed_block_flag) {
unsigned int premask;
premask = (channel->flags & preflag) ? ~0 : 0;
while (l < 36) {
exponents[sfbi] = gain -
(signed int) ((channel->scalefac[sfbi] + (pretab[sfbi] & premask)) <<
scalefac_multiplier);
l += sfbwidth[sfbi++];
}
}
gain0 = gain - 8 * (signed int) channel->subblock_gain[0];
gain1 = gain - 8 * (signed int) channel->subblock_gain[1];
gain2 = gain - 8 * (signed int) channel->subblock_gain[2];
while (l < 576) {
exponents[sfbi + 0] = gain0 -
(signed int) (channel->scalefac[sfbi + 0] << scalefac_multiplier);
exponents[sfbi + 1] = gain1 -
(signed int) (channel->scalefac[sfbi + 1] << scalefac_multiplier);
exponents[sfbi + 2] = gain2 -
(signed int) (channel->scalefac[sfbi + 2] << scalefac_multiplier);
l    += 3 * sfbwidth[sfbi];
sfbi += 3;
}
}
else {
if (channel->flags & preflag) {
for (sfbi = 0; sfbi < 22; ++sfbi) {
exponents[sfbi] = gain -
(signed int) ((channel->scalefac[sfbi] + pretab[sfbi]) <<
scalefac_multiplier);
}
}
else {
for (sfbi = 0; sfbi < 22; ++sfbi) {
exponents[sfbi] = gain -
(signed int) (channel->scalefac[sfbi] << scalefac_multiplier);
}
}
}
}
static
mad_fixed_t III_requantize(unsigned int value, signed int exp)
{
mad_fixed_t requantized;
signed int frac;
struct fixedfloat const *power;
frac = exp % 4;
exp /= 4;
power = &rq_table[value];
requantized = power->mantissa;
exp += power->exponent;
if (exp < 0) {
if (-exp >= sizeof(mad_fixed_t) * CHAR_BIT) {
requantized = 0;
}
else {
requantized += 1L << (-exp - 1);
requantized >>= -exp;
}
}
else {
if (exp >= 5) {
# if defined(DEBUG)
fprintf(stderr, "requantize overflow (%f * 2^%d)\n",
mad_f_todouble(requantized), exp);
# endif
requantized = MAD_F_MAX;
}
else
requantized <<= exp;
}
return frac ? mad_f_mul(requantized, root_table[3 + frac]) : requantized;
}
# define MASK(cache, sz, bits)	\
(((cache) >> ((sz) - (bits))) & ((1 << (bits)) - 1))
# define MASK1BIT(cache, sz)  \
((cache) & (1 << ((sz) - 1)))
static
enum mad_error III_huffdecode(struct mad_bitptr *ptr, mad_fixed_t xr[576],
struct channel *channel,
unsigned char const *sfbwidth,
unsigned int part2_length)
{
signed int exponents[39], exp;
signed int const *expptr;
struct mad_bitptr peek;
signed int bits_left, cachesz;
register mad_fixed_t *xrptr;
mad_fixed_t const *sfbound;
register unsigned long bitcache;
bits_left = (signed) channel->part2_3_length - (signed) part2_length;
if (bits_left < 0)
return MAD_ERROR_BADPART3LEN;
III_exponents(channel, sfbwidth, exponents);
peek = *ptr;
mad_bit_skip(ptr, bits_left);
cachesz  = mad_bit_bitsleft(&peek);
cachesz += ((32 - 1 - 24) + (24 - cachesz)) & ~7;
bitcache   = mad_bit_read(&peek, cachesz);
bits_left -= cachesz;
xrptr = &xr[0];
{
unsigned int region, rcount;
struct hufftable const *entry;
struct huffpair const *table;
unsigned int linbits, startbits, big_values, reqhits;
mad_fixed_t reqcache[16];
sfbound = xrptr + *sfbwidth++;
rcount  = channel->region0_count + 1;
entry     = &mad_huff_pair_table[channel->table_select[region = 0]];
table     = entry->table;
linbits   = entry->linbits;
startbits = entry->startbits;
if (table == 0)
return MAD_ERROR_BADHUFFTABLE;
expptr  = &exponents[0];
exp     = *expptr++;
reqhits = 0;
big_values = channel->big_values;
while (big_values-- && cachesz + bits_left > 0) {
struct huffpair const *pair;
unsigned int clumpsz, value;
register mad_fixed_t requantized;
if (xrptr == sfbound) {
sfbound += *sfbwidth++;
if (--rcount == 0) {
if (region == 0)
rcount = channel->region1_count + 1;
else
rcount = 0;
entry     = &mad_huff_pair_table[channel->table_select[++region]];
table     = entry->table;
linbits   = entry->linbits;
startbits = entry->startbits;
if (table == 0)
return MAD_ERROR_BADHUFFTABLE;
}
if (exp != *expptr) {
exp = *expptr;
reqhits = 0;
}
++expptr;
}
if (cachesz < 21) {
unsigned int bits;
bits       = ((32 - 1 - 21) + (21 - cachesz)) & ~7;
bitcache   = (bitcache << bits) | mad_bit_read(&peek, bits);
cachesz   += bits;
bits_left -= bits;
}
clumpsz = startbits;
pair    = &table[MASK(bitcache, cachesz, clumpsz)];
while (!pair->final) {
cachesz -= clumpsz;
clumpsz = pair->ptr.bits;
pair    = &table[pair->ptr.offset + MASK(bitcache, cachesz, clumpsz)];
}
cachesz -= pair->value.hlen;
if (linbits) {
value = pair->value.x;
switch (value) {
case 0:
xrptr[0] = 0;
break;
case 15:
if (cachesz < linbits + 2) {
bitcache   = (bitcache << 16) | mad_bit_read(&peek, 16);
cachesz   += 16;
bits_left -= 16;
}
value += MASK(bitcache, cachesz, linbits);
cachesz -= linbits;
requantized = III_requantize(value, exp);
goto x_final;
default:
if (reqhits & (1 << value))
requantized = reqcache[value];
else {
reqhits |= (1 << value);
requantized = reqcache[value] = III_requantize(value, exp);
}
x_final:
xrptr[0] = MASK1BIT(bitcache, cachesz--) ?
-requantized : requantized;
}
value = pair->value.y;
switch (value) {
case 0:
xrptr[1] = 0;
break;
case 15:
if (cachesz < linbits + 1) {
bitcache   = (bitcache << 16) | mad_bit_read(&peek, 16);
cachesz   += 16;
bits_left -= 16;
}
value += MASK(bitcache, cachesz, linbits);
cachesz -= linbits;
requantized = III_requantize(value, exp);
goto y_final;
default:
if (reqhits & (1 << value))
requantized = reqcache[value];
else {
reqhits |= (1 << value);
requantized = reqcache[value] = III_requantize(value, exp);
}
y_final:
xrptr[1] = MASK1BIT(bitcache, cachesz--) ?
-requantized : requantized;
}
}
else {
value = pair->value.x;
if (value == 0)
xrptr[0] = 0;
else {
if (reqhits & (1 << value))
requantized = reqcache[value];
else {
reqhits |= (1 << value);
requantized = reqcache[value] = III_requantize(value, exp);
}
xrptr[0] = MASK1BIT(bitcache, cachesz--) ?
-requantized : requantized;
}
value = pair->value.y;
if (value == 0)
xrptr[1] = 0;
else {
if (reqhits & (1 << value))
requantized = reqcache[value];
else {
reqhits |= (1 << value);
requantized = reqcache[value] = III_requantize(value, exp);
}
xrptr[1] = MASK1BIT(bitcache, cachesz--) ?
-requantized : requantized;
}
}
xrptr += 2;
}
}
if (cachesz + bits_left < 0)
return MAD_ERROR_BADHUFFDATA;
{
struct huffquad const *table;
register mad_fixed_t requantized;
table = mad_huff_quad_table[channel->flags & count1table_select];
requantized = III_requantize(1, exp);
while (cachesz + bits_left > 0 && xrptr <= &xr[572]) {
struct huffquad const *quad;
if (cachesz < 10) {
bitcache   = (bitcache << 16) | mad_bit_read(&peek, 16);
cachesz   += 16;
bits_left -= 16;
}
quad = &table[MASK(bitcache, cachesz, 4)];
if (!quad->final) {
cachesz -= 4;
quad = &table[quad->ptr.offset +
MASK(bitcache, cachesz, quad->ptr.bits)];
}
cachesz -= quad->value.hlen;
if (xrptr == sfbound) {
sfbound += *sfbwidth++;
if (exp != *expptr) {
exp = *expptr;
requantized = III_requantize(1, exp);
}
++expptr;
}
xrptr[0] = quad->value.v ?
(MASK1BIT(bitcache, cachesz--) ? -requantized : requantized) : 0;
xrptr[1] = quad->value.w ?
(MASK1BIT(bitcache, cachesz--) ? -requantized : requantized) : 0;
xrptr += 2;
if (xrptr == sfbound) {
sfbound += *sfbwidth++;
if (exp != *expptr) {
exp = *expptr;
requantized = III_requantize(1, exp);
}
++expptr;
}
xrptr[0] = quad->value.x ?
(MASK1BIT(bitcache, cachesz--) ? -requantized : requantized) : 0;
xrptr[1] = quad->value.y ?
(MASK1BIT(bitcache, cachesz--) ? -requantized : requantized) : 0;
xrptr += 2;
}
if (cachesz + bits_left < 0) {
# if 0 && defined(DEBUG)
fprintf(stderr, "huffman count1 overrun (%d bits)\n",
-(cachesz + bits_left));
# endif
xrptr -= 4;
}
}
assert(-bits_left <= MAD_BUFFER_GUARD * CHAR_BIT);
# if 0 && defined(DEBUG)
if (bits_left < 0)
fprintf(stderr, "read %d bits too many\n", -bits_left);
else if (cachesz + bits_left > 0)
fprintf(stderr, "%d stuffing bits\n", cachesz + bits_left);
# endif
while (xrptr < &xr[576]) {
xrptr[0] = 0;
xrptr[1] = 0;
xrptr += 2;
}
return MAD_ERROR_NONE;
}
# undef MASK
# undef MASK1BIT
static
void III_reorder(mad_fixed_t xr[576], struct channel const *channel,
unsigned char const sfbwidth[39])
{
mad_fixed_t tmp[32][3][6];
unsigned int sb, l, f, w, sbw[3], sw[3];
sb = 0;
if (channel->flags & mixed_block_flag) {
sb = 2;
l = 0;
while (l < 36)
l += *sfbwidth++;
}
for (w = 0; w < 3; ++w) {
sbw[w] = sb;
sw[w]  = 0;
}
f = *sfbwidth++;
w = 0;
for (l = 18 * sb; l < 576; ++l) {
if (f-- == 0) {
f = *sfbwidth++ - 1;
w = (w + 1) % 3;
}
tmp[sbw[w]][w][sw[w]++] = xr[l];
if (sw[w] == 6) {
sw[w] = 0;
++sbw[w];
}
}
memcpy(&xr[18 * sb], &tmp[sb], (576 - 18 * sb) * sizeof(mad_fixed_t));
}
static
enum mad_error III_stereo(mad_fixed_t xr[2][576],
struct granule const *granule,
struct mad_header *header,
unsigned char const *sfbwidth)
{
short modes[39];
unsigned int sfbi, l, n, i;
if (granule->ch[0].block_type !=
granule->ch[1].block_type ||
(granule->ch[0].flags & mixed_block_flag) !=
(granule->ch[1].flags & mixed_block_flag))
return MAD_ERROR_BADSTEREO;
for (i = 0; i < 39; ++i)
modes[i] = header->mode_extension;
if (header->mode_extension & I_STEREO) {
struct channel const *right_ch = &granule->ch[1];
mad_fixed_t const *right_xr = xr[1];
unsigned int is_pos;
header->flags |= MAD_FLAG_I_STEREO;
if (right_ch->block_type == 2) {
unsigned int lower, start, max, bound[3], w;
lower = start = max = bound[0] = bound[1] = bound[2] = 0;
sfbi = l = 0;
if (right_ch->flags & mixed_block_flag) {
while (l < 36) {
n = sfbwidth[sfbi++];
for (i = 0; i < n; ++i) {
if (right_xr[i]) {
lower = sfbi;
break;
}
}
right_xr += n;
l += n;
}
start = sfbi;
}
w = 0;
while (l < 576) {
n = sfbwidth[sfbi++];
for (i = 0; i < n; ++i) {
if (right_xr[i]) {
max = bound[w] = sfbi;
break;
}
}
right_xr += n;
l += n;
w = (w + 1) % 3;
}
if (max)
lower = start;
for (i = 0; i < lower; ++i)
modes[i] = header->mode_extension & ~I_STEREO;
w = 0;
for (i = start; i < max; ++i) {
if (i < bound[w])
modes[i] = header->mode_extension & ~I_STEREO;
w = (w + 1) % 3;
}
}
else {
unsigned int bound;
bound = 0;
for (sfbi = l = 0; l < 576; l += n) {
n = sfbwidth[sfbi++];
for (i = 0; i < n; ++i) {
if (right_xr[i]) {
bound = sfbi;
break;
}
}
right_xr += n;
}
for (i = 0; i < bound; ++i)
modes[i] = header->mode_extension & ~I_STEREO;
}
if (header->flags & MAD_FLAG_LSF_EXT) {
unsigned char const *illegal_pos = granule[1].ch[1].scalefac;
mad_fixed_t const *lsf_scale;
lsf_scale = is_lsf_table[right_ch->scalefac_compress & 0x1];
for (sfbi = l = 0; l < 576; ++sfbi, l += n) {
n = sfbwidth[sfbi];
if (!(modes[sfbi] & I_STEREO))
continue;
if (illegal_pos[sfbi]) {
modes[sfbi] &= ~I_STEREO;
continue;
}
is_pos = right_ch->scalefac[sfbi];
for (i = 0; i < n; ++i) {
register mad_fixed_t left;
left = xr[0][l + i];
if (is_pos == 0)
xr[1][l + i] = left;
else {
register mad_fixed_t opposite;
opposite = mad_f_mul(left, lsf_scale[(is_pos - 1) / 2]);
if (is_pos & 1) {
xr[0][l + i] = opposite;
xr[1][l + i] = left;
}
else
xr[1][l + i] = opposite;
}
}
}
}
else {
for (sfbi = l = 0; l < 576; ++sfbi, l += n) {
n = sfbwidth[sfbi];
if (!(modes[sfbi] & I_STEREO))
continue;
is_pos = right_ch->scalefac[sfbi];
if (is_pos >= 7) {
modes[sfbi] &= ~I_STEREO;
continue;
}
for (i = 0; i < n; ++i) {
register mad_fixed_t left;
left = xr[0][l + i];
xr[0][l + i] = mad_f_mul(left, is_table[    is_pos]);
xr[1][l + i] = mad_f_mul(left, is_table[6 - is_pos]);
}
}
}
}
if (header->mode_extension & MS_STEREO) {
register mad_fixed_t invsqrt2;
header->flags |= MAD_FLAG_MS_STEREO;
invsqrt2 = root_table[3 + -2];
for (sfbi = l = 0; l < 576; ++sfbi, l += n) {
n = sfbwidth[sfbi];
if (modes[sfbi] != MS_STEREO)
continue;
for (i = 0; i < n; ++i) {
register mad_fixed_t m, s;
m = xr[0][l + i];
s = xr[1][l + i];
xr[0][l + i] = mad_f_mul(m + s, invsqrt2);
xr[1][l + i] = mad_f_mul(m - s, invsqrt2);
}
}
}
return MAD_ERROR_NONE;
}
static
void III_aliasreduce(mad_fixed_t xr[576], int lines)
{
mad_fixed_t const *bound;
int i;
bound = &xr[lines];
for (xr += 18; xr < bound; xr += 18) {
for (i = 0; i < 8; ++i) {
register mad_fixed_t a, b;
register mad_fixed64hi_t hi;
register mad_fixed64lo_t lo;
a = xr[-1 - i];
b = xr[     i];
# if defined(ASO_ZEROCHECK)
if (a | b) {
# endif
MAD_F_ML0(hi, lo,  a, cs[i]);
MAD_F_MLA(hi, lo, -b, ca[i]);
xr[-1 - i] = MAD_F_MLZ(hi, lo);
MAD_F_ML0(hi, lo,  b, cs[i]);
MAD_F_MLA(hi, lo,  a, ca[i]);
xr[     i] = MAD_F_MLZ(hi, lo);
# if defined(ASO_ZEROCHECK)
}
# endif
}
}
}
# if defined(ASO_IMDCT)
void III_imdct_l(mad_fixed_t const [18], mad_fixed_t [36], unsigned int);
# else
#  if 1
static
void fastsdct(mad_fixed_t const x[9], mad_fixed_t y[18])
{
mad_fixed_t a0,  a1,  a2,  a3,  a4,  a5,  a6,  a7,  a8,  a9,  a10, a11, a12;
mad_fixed_t a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25;
mad_fixed_t m0,  m1,  m2,  m3,  m4,  m5,  m6,  m7;
enum {
c0 =  MAD_F(0x1f838b8d),
c1 =  MAD_F(0x1bb67ae8),
c2 =  MAD_F(0x18836fa3),
c3 =  MAD_F(0x1491b752),
c4 =  MAD_F(0x0af1d43a),
c5 =  MAD_F(0x058e86a0),
c6 = -MAD_F(0x1e11f642)
};
a0 = x[3] + x[5];
a1 = x[3] - x[5];
a2 = x[6] + x[2];
a3 = x[6] - x[2];
a4 = x[1] + x[7];
a5 = x[1] - x[7];
a6 = x[8] + x[0];
a7 = x[8] - x[0];
a8  = a0  + a2;
a9  = a0  - a2;
a10 = a0  - a6;
a11 = a2  - a6;
a12 = a8  + a6;
a13 = a1  - a3;
a14 = a13 + a7;
a15 = a3  + a7;
a16 = a1  - a7;
a17 = a1  + a3;
m0 = mad_f_mul(a17, -c3);
m1 = mad_f_mul(a16, -c0);
m2 = mad_f_mul(a15, -c4);
m3 = mad_f_mul(a14, -c1);
m4 = mad_f_mul(a5,  -c1);
m5 = mad_f_mul(a11, -c6);
m6 = mad_f_mul(a10, -c5);
m7 = mad_f_mul(a9,  -c2);
a18 =     x[4] + a4;
a19 = 2 * x[4] - a4;
a20 = a19 + m5;
a21 = a19 - m5;
a22 = a19 + m6;
a23 = m4  + m2;
a24 = m4  - m2;
a25 = m4  + m1;
y[ 0] = a18 + a12;
y[ 2] = m0  - a25;
y[ 4] = m7  - a20;
y[ 6] = m3;
y[ 8] = a21 - m6;
y[10] = a24 - m1;
y[12] = a12 - 2 * a18;
y[14] = a23 + m0;
y[16] = a22 + m7;
}
static inline
void sdctII(mad_fixed_t const x[18], mad_fixed_t X[18])
{
mad_fixed_t tmp[9];
int i;
static mad_fixed_t const scale[9] = {
MAD_F(0x1fe0d3b4), MAD_F(0x1ee8dd47), MAD_F(0x1d007930),
MAD_F(0x1a367e59), MAD_F(0x16a09e66), MAD_F(0x125abcf8),
MAD_F(0x0d8616bc), MAD_F(0x08483ee1), MAD_F(0x02c9fad7)
};
for (i = 0; i < 9; i += 3) {
tmp[i + 0] = x[i + 0] + x[18 - (i + 0) - 1];
tmp[i + 1] = x[i + 1] + x[18 - (i + 1) - 1];
tmp[i + 2] = x[i + 2] + x[18 - (i + 2) - 1];
}
fastsdct(tmp, &X[0]);
for (i = 0; i < 9; i += 3) {
tmp[i + 0] = mad_f_mul(x[i + 0] - x[18 - (i + 0) - 1], scale[i + 0]);
tmp[i + 1] = mad_f_mul(x[i + 1] - x[18 - (i + 1) - 1], scale[i + 1]);
tmp[i + 2] = mad_f_mul(x[i + 2] - x[18 - (i + 2) - 1], scale[i + 2]);
}
fastsdct(tmp, &X[1]);
for (i = 3; i < 18; i += 8) {
X[i + 0] -= X[(i + 0) - 2];
X[i + 2] -= X[(i + 2) - 2];
X[i + 4] -= X[(i + 4) - 2];
X[i + 6] -= X[(i + 6) - 2];
}
}
static inline
void dctIV(mad_fixed_t const y[18], mad_fixed_t X[18])
{
mad_fixed_t tmp[18];
int i;
static mad_fixed_t const scale[18] = {
MAD_F(0x1ff833fa), MAD_F(0x1fb9ea93), MAD_F(0x1f3dd120),
MAD_F(0x1e84d969), MAD_F(0x1d906bcf), MAD_F(0x1c62648b),
MAD_F(0x1afd100f), MAD_F(0x1963268b), MAD_F(0x1797c6a4),
MAD_F(0x159e6f5b), MAD_F(0x137af940), MAD_F(0x11318ef3),
MAD_F(0x0ec6a507), MAD_F(0x0c3ef153), MAD_F(0x099f61c5),
MAD_F(0x06ed12c5), MAD_F(0x042d4544), MAD_F(0x0165547c)
};
for (i = 0; i < 18; i += 3) {
tmp[i + 0] = mad_f_mul(y[i + 0], scale[i + 0]);
tmp[i + 1] = mad_f_mul(y[i + 1], scale[i + 1]);
tmp[i + 2] = mad_f_mul(y[i + 2], scale[i + 2]);
}
sdctII(tmp, X);
X[0] /= 2;
for (i = 1; i < 17; i += 4) {
X[i + 0] = X[i + 0] / 2 - X[(i + 0) - 1];
X[i + 1] = X[i + 1] / 2 - X[(i + 1) - 1];
X[i + 2] = X[i + 2] / 2 - X[(i + 2) - 1];
X[i + 3] = X[i + 3] / 2 - X[(i + 3) - 1];
}
X[17] = X[17] / 2 - X[16];
}
static inline
void imdct36(mad_fixed_t const x[18], mad_fixed_t y[36])
{
mad_fixed_t tmp[18];
int i;
dctIV(x, tmp);
for (i =  0; i <  9; i += 3) {
y[i + 0] =  tmp[9 + (i + 0)];
y[i + 1] =  tmp[9 + (i + 1)];
y[i + 2] =  tmp[9 + (i + 2)];
}
for (i =  9; i < 27; i += 3) {
y[i + 0] = -tmp[36 - (9 + (i + 0)) - 1];
y[i + 1] = -tmp[36 - (9 + (i + 1)) - 1];
y[i + 2] = -tmp[36 - (9 + (i + 2)) - 1];
}
for (i = 27; i < 36; i += 3) {
y[i + 0] = -tmp[(i + 0) - 27];
y[i + 1] = -tmp[(i + 1) - 27];
y[i + 2] = -tmp[(i + 2) - 27];
}
}
#  else
static inline
void imdct36(mad_fixed_t const X[18], mad_fixed_t x[36])
{
mad_fixed_t t0, t1, t2,  t3,  t4,  t5,  t6,  t7;
mad_fixed_t t8, t9, t10, t11, t12, t13, t14, t15;
register mad_fixed64hi_t hi;
register mad_fixed64lo_t lo;
MAD_F_ML0(hi, lo, X[4],  MAD_F(0x0ec835e8));
MAD_F_MLA(hi, lo, X[13], MAD_F(0x061f78aa));
t6 = MAD_F_MLZ(hi, lo);
MAD_F_MLA(hi, lo, (t14 = X[1] - X[10]), -MAD_F(0x061f78aa));
MAD_F_MLA(hi, lo, (t15 = X[7] + X[16]), -MAD_F(0x0ec835e8));
t0 = MAD_F_MLZ(hi, lo);
MAD_F_MLA(hi, lo, (t8  = X[0] - X[11] - X[12]),  MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, (t9  = X[2] - X[9]  - X[14]),  MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, (t10 = X[3] - X[8]  - X[15]), -MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, (t11 = X[5] - X[6]  - X[17]), -MAD_F(0x0fdcf549));
x[7]  = MAD_F_MLZ(hi, lo);
x[10] = -x[7];
MAD_F_ML0(hi, lo, t8,  -MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, t9,   MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, t10,  MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, t11, -MAD_F(0x09bd7ca0));
x[19] = x[34] = MAD_F_MLZ(hi, lo) - t0;
t12 = X[0] - X[3] + X[8] - X[11] - X[12] + X[15];
t13 = X[2] + X[5] - X[6] - X[9]  - X[14] - X[17];
MAD_F_ML0(hi, lo, t12, -MAD_F(0x0ec835e8));
MAD_F_MLA(hi, lo, t13,  MAD_F(0x061f78aa));
x[22] = x[31] = MAD_F_MLZ(hi, lo) + t0;
MAD_F_ML0(hi, lo, X[1],  -MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, X[7],   MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, X[10], -MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, X[16],  MAD_F(0x0cb19346));
t1 = MAD_F_MLZ(hi, lo) + t6;
MAD_F_ML0(hi, lo, X[0],   MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[11],  MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0f9ee890));
x[6]  = MAD_F_MLZ(hi, lo) + t1;
x[11] = -x[6];
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[2],  -MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[17],  MAD_F(0x04cfb0e2));
x[23] = x[30] = MAD_F_MLZ(hi, lo) + t1;
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[11],  MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0acf37ad));
x[18] = x[35] = MAD_F_MLZ(hi, lo) - t1;
MAD_F_ML0(hi, lo, X[4],   MAD_F(0x061f78aa));
MAD_F_MLA(hi, lo, X[13], -MAD_F(0x0ec835e8));
t7 = MAD_F_MLZ(hi, lo);
MAD_F_MLA(hi, lo, X[1],  -MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, X[7],   MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, X[10],  MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, X[16], -MAD_F(0x09bd7ca0));
t2 = MAD_F_MLZ(hi, lo);
MAD_F_MLA(hi, lo, X[0],   MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[12],  MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[17],  MAD_F(0x0f426cb5));
x[5]  = MAD_F_MLZ(hi, lo);
x[12] = -x[5];
MAD_F_ML0(hi, lo, X[0],   MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[2],  -MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[11],  MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0bcbe352));
x[0]  = MAD_F_MLZ(hi, lo) + t2;
x[17] = -x[0];
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[2],  -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[14], -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x03768962));
x[24] = x[29] = MAD_F_MLZ(hi, lo) + t2;
MAD_F_ML0(hi, lo, X[1],  -MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, X[7],  -MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, X[10],  MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, X[16],  MAD_F(0x0fdcf549));
t3 = MAD_F_MLZ(hi, lo) + t7;
MAD_F_ML0(hi, lo, X[0],   MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[12],  MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0ffc19fd));
x[8] = MAD_F_MLZ(hi, lo) + t3;
x[9] = -x[8];
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[14], -MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[17],  MAD_F(0x07635284));
x[21] = x[32] = MAD_F_MLZ(hi, lo) + t3;
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[12],  MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0898c779));
x[20] = x[33] = MAD_F_MLZ(hi, lo) - t3;
MAD_F_ML0(hi, lo, t14, -MAD_F(0x0ec835e8));
MAD_F_MLA(hi, lo, t15,  MAD_F(0x061f78aa));
t4 = MAD_F_MLZ(hi, lo) - t7;
MAD_F_ML0(hi, lo, t12, MAD_F(0x061f78aa));
MAD_F_MLA(hi, lo, t13, MAD_F(0x0ec835e8));
x[4]  = MAD_F_MLZ(hi, lo) + t4;
x[13] = -x[4];
MAD_F_ML0(hi, lo, t8,   MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, t9,  -MAD_F(0x0216a2a2));
MAD_F_MLA(hi, lo, t10,  MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, t11, -MAD_F(0x0cb19346));
x[1]  = MAD_F_MLZ(hi, lo) + t4;
x[16] = -x[1];
MAD_F_ML0(hi, lo, t8,  -MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, t9,  -MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, t10, -MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, t11, -MAD_F(0x0216a2a2));
x[25] = x[28] = MAD_F_MLZ(hi, lo) + t4;
MAD_F_ML0(hi, lo, X[1],  -MAD_F(0x0fdcf549));
MAD_F_MLA(hi, lo, X[7],  -MAD_F(0x0cb19346));
MAD_F_MLA(hi, lo, X[10], -MAD_F(0x09bd7ca0));
MAD_F_MLA(hi, lo, X[16], -MAD_F(0x0216a2a2));
t5 = MAD_F_MLZ(hi, lo) - t6;
MAD_F_ML0(hi, lo, X[0],   MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[6],   MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[9],   MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[12],  MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[14], -MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[15],  MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x0d7e8807));
x[2]  = MAD_F_MLZ(hi, lo) + t5;
x[15] = -x[2];
MAD_F_ML0(hi, lo, X[0],   MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[2],   MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[3],   MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[5],   MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x00b2aa3e));
MAD_F_MLA(hi, lo, X[8],   MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[11],  MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[14],  MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[17],  MAD_F(0x0e313245));
x[3]  = MAD_F_MLZ(hi, lo) + t5;
x[14] = -x[3];
MAD_F_ML0(hi, lo, X[0],  -MAD_F(0x0ffc19fd));
MAD_F_MLA(hi, lo, X[2],  -MAD_F(0x0f9ee890));
MAD_F_MLA(hi, lo, X[3],  -MAD_F(0x0f426cb5));
MAD_F_MLA(hi, lo, X[5],  -MAD_F(0x0e313245));
MAD_F_MLA(hi, lo, X[6],  -MAD_F(0x0d7e8807));
MAD_F_MLA(hi, lo, X[8],  -MAD_F(0x0bcbe352));
MAD_F_MLA(hi, lo, X[9],  -MAD_F(0x0acf37ad));
MAD_F_MLA(hi, lo, X[11], -MAD_F(0x0898c779));
MAD_F_MLA(hi, lo, X[12], -MAD_F(0x07635284));
MAD_F_MLA(hi, lo, X[14], -MAD_F(0x04cfb0e2));
MAD_F_MLA(hi, lo, X[15], -MAD_F(0x03768962));
MAD_F_MLA(hi, lo, X[17], -MAD_F(0x00b2aa3e));
x[26] = x[27] = MAD_F_MLZ(hi, lo) + t5;
}
#  endif
static
void III_imdct_l(mad_fixed_t const X[18], mad_fixed_t z[36],
unsigned int block_type)
{
unsigned int i;
imdct36(X, z);
switch (block_type) {
case 0:
# if defined(ASO_INTERLEAVE1)
{
register mad_fixed_t tmp1, tmp2;
tmp1 = window_l[0];
tmp2 = window_l[1];
for (i = 0; i < 34; i += 2) {
z[i + 0] = mad_f_mul(z[i + 0], tmp1);
tmp1 = window_l[i + 2];
z[i + 1] = mad_f_mul(z[i + 1], tmp2);
tmp2 = window_l[i + 3];
}
z[34] = mad_f_mul(z[34], tmp1);
z[35] = mad_f_mul(z[35], tmp2);
}
# elif defined(ASO_INTERLEAVE2)
{
register mad_fixed_t tmp1, tmp2;
tmp1 = z[0];
tmp2 = window_l[0];
for (i = 0; i < 35; ++i) {
z[i] = mad_f_mul(tmp1, tmp2);
tmp1 = z[i + 1];
tmp2 = window_l[i + 1];
}
z[35] = mad_f_mul(tmp1, tmp2);
}
# elif 1
for (i = 0; i < 36; i += 4) {
z[i + 0] = mad_f_mul(z[i + 0], window_l[i + 0]);
z[i + 1] = mad_f_mul(z[i + 1], window_l[i + 1]);
z[i + 2] = mad_f_mul(z[i + 2], window_l[i + 2]);
z[i + 3] = mad_f_mul(z[i + 3], window_l[i + 3]);
}
# else
for (i =  0; i < 36; ++i) z[i] = mad_f_mul(z[i], window_l[i]);
# endif
break;
case 1:
for (i =  0; i < 18; i += 3) {
z[i + 0] = mad_f_mul(z[i + 0], window_l[i + 0]);
z[i + 1] = mad_f_mul(z[i + 1], window_l[i + 1]);
z[i + 2] = mad_f_mul(z[i + 2], window_l[i + 2]);
}
for (i = 24; i < 30; ++i) z[i] = mad_f_mul(z[i], window_s[i - 18]);
for (i = 30; i < 36; ++i) z[i] = 0;
break;
case 3:
for (i =  0; i <  6; ++i) z[i] = 0;
for (i =  6; i < 12; ++i) z[i] = mad_f_mul(z[i], window_s[i - 6]);
for (i = 18; i < 36; i += 3) {
z[i + 0] = mad_f_mul(z[i + 0], window_l[i + 0]);
z[i + 1] = mad_f_mul(z[i + 1], window_l[i + 1]);
z[i + 2] = mad_f_mul(z[i + 2], window_l[i + 2]);
}
break;
}
}
# endif
static
void III_imdct_s(mad_fixed_t const X[18], mad_fixed_t z[36])
{
mad_fixed_t y[36], *yptr;
mad_fixed_t const *wptr;
int w, i;
register mad_fixed64hi_t hi;
register mad_fixed64lo_t lo;
yptr = &y[0];
for (w = 0; w < 3; ++w) {
register mad_fixed_t const (*s)[6];
s = imdct_s;
for (i = 0; i < 3; ++i) {
MAD_F_ML0(hi, lo, X[0], (*s)[0]);
MAD_F_MLA(hi, lo, X[1], (*s)[1]);
MAD_F_MLA(hi, lo, X[2], (*s)[2]);
MAD_F_MLA(hi, lo, X[3], (*s)[3]);
MAD_F_MLA(hi, lo, X[4], (*s)[4]);
MAD_F_MLA(hi, lo, X[5], (*s)[5]);
yptr[i + 0] = MAD_F_MLZ(hi, lo);
yptr[5 - i] = -yptr[i + 0];
++s;
MAD_F_ML0(hi, lo, X[0], (*s)[0]);
MAD_F_MLA(hi, lo, X[1], (*s)[1]);
MAD_F_MLA(hi, lo, X[2], (*s)[2]);
MAD_F_MLA(hi, lo, X[3], (*s)[3]);
MAD_F_MLA(hi, lo, X[4], (*s)[4]);
MAD_F_MLA(hi, lo, X[5], (*s)[5]);
yptr[ i + 6] = MAD_F_MLZ(hi, lo);
yptr[11 - i] = yptr[i + 6];
++s;
}
yptr += 12;
X    += 6;
}
yptr = &y[0];
wptr = &window_s[0];
for (i = 0; i < 6; ++i) {
z[i +  0] = 0;
z[i +  6] = mad_f_mul(yptr[ 0 + 0], wptr[0]);
MAD_F_ML0(hi, lo, yptr[ 0 + 6], wptr[6]);
MAD_F_MLA(hi, lo, yptr[12 + 0], wptr[0]);
z[i + 12] = MAD_F_MLZ(hi, lo);
MAD_F_ML0(hi, lo, yptr[12 + 6], wptr[6]);
MAD_F_MLA(hi, lo, yptr[24 + 0], wptr[0]);
z[i + 18] = MAD_F_MLZ(hi, lo);
z[i + 24] = mad_f_mul(yptr[24 + 6], wptr[6]);
z[i + 30] = 0;
++yptr;
++wptr;
}
}
static
void III_overlap(mad_fixed_t const output[36], mad_fixed_t overlap[18],
mad_fixed_t sample[18][32], unsigned int sb)
{
unsigned int i;
# if defined(ASO_INTERLEAVE2)
{
register mad_fixed_t tmp1, tmp2;
tmp1 = overlap[0];
tmp2 = overlap[1];
for (i = 0; i < 16; i += 2) {
sample[i + 0][sb] = output[i + 0 +  0] + tmp1;
overlap[i + 0]    = output[i + 0 + 18];
tmp1 = overlap[i + 2];
sample[i + 1][sb] = output[i + 1 +  0] + tmp2;
overlap[i + 1]    = output[i + 1 + 18];
tmp2 = overlap[i + 3];
}
sample[16][sb] = output[16 +  0] + tmp1;
overlap[16]    = output[16 + 18];
sample[17][sb] = output[17 +  0] + tmp2;
overlap[17]    = output[17 + 18];
}
# elif 0
for (i = 0; i < 18; i += 2) {
sample[i + 0][sb] = output[i + 0 +  0] + overlap[i + 0];
overlap[i + 0]    = output[i + 0 + 18];
sample[i + 1][sb] = output[i + 1 +  0] + overlap[i + 1];
overlap[i + 1]    = output[i + 1 + 18];
}
# else
for (i = 0; i < 18; ++i) {
sample[i][sb] = output[i +  0] + overlap[i];
overlap[i]    = output[i + 18];
}
# endif
}
static inline
void III_overlap_z(mad_fixed_t overlap[18],
mad_fixed_t sample[18][32], unsigned int sb)
{
unsigned int i;
# if defined(ASO_INTERLEAVE2)
{
register mad_fixed_t tmp1, tmp2;
tmp1 = overlap[0];
tmp2 = overlap[1];
for (i = 0; i < 16; i += 2) {
sample[i + 0][sb] = tmp1;
overlap[i + 0]    = 0;
tmp1 = overlap[i + 2];
sample[i + 1][sb] = tmp2;
overlap[i + 1]    = 0;
tmp2 = overlap[i + 3];
}
sample[16][sb] = tmp1;
overlap[16]    = 0;
sample[17][sb] = tmp2;
overlap[17]    = 0;
}
# else
for (i = 0; i < 18; ++i) {
sample[i][sb] = overlap[i];
overlap[i]    = 0;
}
# endif
}
static
void III_freqinver(mad_fixed_t sample[18][32], unsigned int sb)
{
unsigned int i;
# if 1 || defined(ASO_INTERLEAVE1) || defined(ASO_INTERLEAVE2)
{
register mad_fixed_t tmp1, tmp2;
tmp1 = sample[1][sb];
tmp2 = sample[3][sb];
for (i = 1; i < 13; i += 4) {
sample[i + 0][sb] = -tmp1;
tmp1 = sample[i + 4][sb];
sample[i + 2][sb] = -tmp2;
tmp2 = sample[i + 6][sb];
}
sample[13][sb] = -tmp1;
tmp1 = sample[17][sb];
sample[15][sb] = -tmp2;
sample[17][sb] = -tmp1;
}
# else
for (i = 1; i < 18; i += 2)
sample[i][sb] = -sample[i][sb];
# endif
}
static
enum mad_error III_decode(struct mad_bitptr *ptr, struct mad_frame *frame,
struct sideinfo *si, unsigned int nch)
{
struct mad_header *header = &frame->header;
unsigned int sfreqi, ngr, gr;
{
unsigned int sfreq;
sfreq = header->samplerate;
if (header->flags & MAD_FLAG_MPEG_2_5_EXT)
sfreq *= 2;
sfreqi = ((sfreq >>  7) & 0x000f) +
((sfreq >> 15) & 0x0001) - 8;
if (header->flags & MAD_FLAG_MPEG_2_5_EXT)
sfreqi += 3;
}
ngr = (header->flags & MAD_FLAG_LSF_EXT) ? 1 : 2;
for (gr = 0; gr < ngr; ++gr) {
struct granule *granule = &si->gr[gr];
unsigned char const *sfbwidth[2];
mad_fixed_t xr[2][576];
unsigned int ch;
enum mad_error error;
for (ch = 0; ch < nch; ++ch) {
struct channel *channel = &granule->ch[ch];
unsigned int part2_length;
sfbwidth[ch] = sfbwidth_table[sfreqi].l;
if (channel->block_type == 2) {
sfbwidth[ch] = (channel->flags & mixed_block_flag) ?
sfbwidth_table[sfreqi].m : sfbwidth_table[sfreqi].s;
}
if (header->flags & MAD_FLAG_LSF_EXT) {
part2_length = III_scalefactors_lsf(ptr, channel,
ch == 0 ? 0 : &si->gr[1].ch[1],
header->mode_extension);
}
else {
part2_length = III_scalefactors(ptr, channel, &si->gr[0].ch[ch],
gr == 0 ? 0 : si->scfsi[ch]);
}
error = III_huffdecode(ptr, xr[ch], channel, sfbwidth[ch], part2_length);
if (error)
return error;
}
if (header->mode == MAD_MODE_JOINT_STEREO && header->mode_extension) {
error = III_stereo(xr, granule, header, sfbwidth[0]);
if (error)
return error;
}
for (ch = 0; ch < nch; ++ch) {
struct channel const *channel = &granule->ch[ch];
mad_fixed_t (*sample)[32] = &frame->sbsample[ch][18 * gr];
unsigned int sb, l, i, sblimit;
mad_fixed_t output[36];
if (channel->block_type == 2) {
III_reorder(xr[ch], channel, sfbwidth[ch]);
# if !defined(OPT_STRICT)
if (channel->flags & mixed_block_flag)
III_aliasreduce(xr[ch], 36);
# endif
}
else
III_aliasreduce(xr[ch], 576);
l = 0;
if (channel->block_type != 2 || (channel->flags & mixed_block_flag)) {
unsigned int block_type;
block_type = channel->block_type;
if (channel->flags & mixed_block_flag)
block_type = 0;
for (sb = 0; sb < 2; ++sb, l += 18) {
III_imdct_l(&xr[ch][l], output, block_type);
III_overlap(output, (*frame->overlap)[ch][sb], sample, sb);
}
}
else {
for (sb = 0; sb < 2; ++sb, l += 18) {
III_imdct_s(&xr[ch][l], output);
III_overlap(output, (*frame->overlap)[ch][sb], sample, sb);
}
}
III_freqinver(sample, 1);
i = 576;
while (i > 36 && xr[ch][i - 1] == 0)
--i;
sblimit = 32 - (576 - i) / 18;
if (channel->block_type != 2) {
for (sb = 2; sb < sblimit; ++sb, l += 18) {
III_imdct_l(&xr[ch][l], output, channel->block_type);
III_overlap(output, (*frame->overlap)[ch][sb], sample, sb);
if (sb & 1)
III_freqinver(sample, sb);
}
}
else {
for (sb = 2; sb < sblimit; ++sb, l += 18) {
III_imdct_s(&xr[ch][l], output);
III_overlap(output, (*frame->overlap)[ch][sb], sample, sb);
if (sb & 1)
III_freqinver(sample, sb);
}
}
for (sb = sblimit; sb < 32; ++sb) {
III_overlap_z((*frame->overlap)[ch][sb], sample, sb);
if (sb & 1)
III_freqinver(sample, sb);
}
}
}
return MAD_ERROR_NONE;
}
int mad_layer_III(struct mad_stream *stream, struct mad_frame *frame)
{
struct mad_header *header = &frame->header;
unsigned int nch, priv_bitlen, next_md_begin = 0;
unsigned int si_len, data_bitlen, md_len;
unsigned int frame_space, frame_used, frame_free;
struct mad_bitptr ptr;
struct sideinfo si;
enum mad_error error;
int result = 0;
if (stream->main_data == 0) {
stream->main_data = malloc(MAD_BUFFER_MDLEN);
if (stream->main_data == 0) {
stream->error = MAD_ERROR_NOMEM;
return -1;
}
}
if (frame->overlap == 0) {
frame->overlap = calloc(2 * 32 * 18, sizeof(mad_fixed_t));
if (frame->overlap == 0) {
stream->error = MAD_ERROR_NOMEM;
return -1;
}
}
nch = MAD_NCHANNELS(header);
si_len = (header->flags & MAD_FLAG_LSF_EXT) ?
(nch == 1 ? 9 : 17) : (nch == 1 ? 17 : 32);
if (stream->next_frame - mad_bit_nextbyte(&stream->ptr) <
(signed int) si_len) {
stream->error = MAD_ERROR_BADFRAMELEN;
stream->md_len = 0;
return -1;
}
if (header->flags & MAD_FLAG_PROTECTION) {
header->crc_check =
mad_bit_crc(stream->ptr, si_len * CHAR_BIT, header->crc_check);
if (header->crc_check != header->crc_target &&
!(frame->options & MAD_OPTION_IGNORECRC)) {
stream->error = MAD_ERROR_BADCRC;
result = -1;
}
}
error = III_sideinfo(&stream->ptr, nch, header->flags & MAD_FLAG_LSF_EXT,
&si, &data_bitlen, &priv_bitlen);
if (error && result == 0) {
stream->error = error;
result = -1;
}
header->flags        |= priv_bitlen;
header->private_bits |= si.private_bits;
{
struct mad_bitptr peek;
unsigned long header;
mad_bit_init(&peek, stream->next_frame);
header = mad_bit_read(&peek, 32);
if ((header & 0xffe60000L)  == 0xffe20000L) {
if (!(header & 0x00010000L))
mad_bit_skip(&peek, 16);
next_md_begin =
mad_bit_read(&peek, (header & 0x00080000L)  ? 9 : 8);
}
mad_bit_finish(&peek);
}
frame_space = stream->next_frame - mad_bit_nextbyte(&stream->ptr);
if (next_md_begin > si.main_data_begin + frame_space)
next_md_begin = 0;
md_len = si.main_data_begin + frame_space - next_md_begin;
frame_used = 0;
if (si.main_data_begin == 0) {
ptr = stream->ptr;
stream->md_len = 0;
frame_used = md_len;
}
else {
if (si.main_data_begin > stream->md_len) {
if (result == 0) {
stream->error = MAD_ERROR_BADDATAPTR;
result = -1;
}
}
else {
mad_bit_init(&ptr,
*stream->main_data + stream->md_len - si.main_data_begin);
if (md_len > si.main_data_begin) {
assert(stream->md_len + md_len -
si.main_data_begin <= MAD_BUFFER_MDLEN);
memcpy(*stream->main_data + stream->md_len,
mad_bit_nextbyte(&stream->ptr),
frame_used = md_len - si.main_data_begin);
stream->md_len += frame_used;
}
}
}
frame_free = frame_space - frame_used;
if (result == 0) {
error = III_decode(&ptr, frame, &si, nch);
if (error) {
stream->error = error;
result = -1;
}
stream->anc_ptr    = ptr;
stream->anc_bitlen = md_len * CHAR_BIT - data_bitlen;
}
# if 0 && defined(DEBUG)
fprintf(stderr,
"main_data_begin:%u, md_len:%u, frame_free:%u, "
"data_bitlen:%u, anc_bitlen: %u\n",
si.main_data_begin, md_len, frame_free,
data_bitlen, stream->anc_bitlen);
# endif
if (frame_free >= next_md_begin) {
memcpy(*stream->main_data,
stream->next_frame - next_md_begin, next_md_begin);
stream->md_len = next_md_begin;
}
else {
if (md_len < si.main_data_begin) {
unsigned int extra;
extra = si.main_data_begin - md_len;
if (extra + frame_free > next_md_begin)
extra = next_md_begin - frame_free;
if (extra < stream->md_len) {
memmove(*stream->main_data,
*stream->main_data + stream->md_len - extra, extra);
stream->md_len = extra;
}
}
else
stream->md_len = 0;
memcpy(*stream->main_data + stream->md_len,
stream->next_frame - frame_free, frame_free);
stream->md_len += frame_free;
}
return result;
}