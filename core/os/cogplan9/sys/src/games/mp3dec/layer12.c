# ifdef HAVE_CONFIG_H
# include "config.h"
# endif
# include "global.h"
# ifdef HAVE_LIMITS_H
# include <limits.h>
# else
# define CHAR_BIT 8
# endif
# include "fixed.h"
# include "bit.h"
# include "stream.h"
# include "frame.h"
# include "layer12.h"
static
mad_fixed_t const sf_table[64] = {
# include "sf_table.dat"
};
static
mad_fixed_t const linear_table[14] = {
MAD_F(0x15555555),
MAD_F(0x12492492),
MAD_F(0x11111111),
MAD_F(0x10842108),
MAD_F(0x10410410),
MAD_F(0x10204081),
MAD_F(0x10101010),
MAD_F(0x10080402),
MAD_F(0x10040100),
MAD_F(0x10020040),
MAD_F(0x10010010),
MAD_F(0x10008004),
MAD_F(0x10004001),
MAD_F(0x10002000)
};
static
mad_fixed_t I_sample(struct mad_bitptr *ptr, unsigned int nb)
{
mad_fixed_t sample;
sample = mad_bit_read(ptr, nb);
sample ^= 1 << (nb - 1);
sample |= -(sample & (1 << (nb - 1)));
sample <<= MAD_F_FRACBITS - (nb - 1);
sample += MAD_F_ONE >> (nb - 1);
return mad_f_mul(sample, linear_table[nb - 2]);
}
int mad_layer_I(struct mad_stream *stream, struct mad_frame *frame)
{
struct mad_header *header = &frame->header;
unsigned int nch, bound, ch, s, sb, nb;
unsigned char allocation[2][32], scalefactor[2][32];
nch = MAD_NCHANNELS(header);
bound = 32;
if (header->mode == MAD_MODE_JOINT_STEREO) {
header->flags |= MAD_FLAG_I_STEREO;
bound = 4 + header->mode_extension * 4;
}
if (header->flags & MAD_FLAG_PROTECTION) {
header->crc_check =
mad_bit_crc(stream->ptr, 4 * (bound * nch + (32 - bound)),
header->crc_check);
if (header->crc_check != header->crc_target &&
!(frame->options & MAD_OPTION_IGNORECRC)) {
stream->error = MAD_ERROR_BADCRC;
return -1;
}
}
for (sb = 0; sb < bound; ++sb) {
for (ch = 0; ch < nch; ++ch) {
nb = mad_bit_read(&stream->ptr, 4);
if (nb == 15) {
stream->error = MAD_ERROR_BADBITALLOC;
return -1;
}
allocation[ch][sb] = nb ? nb + 1 : 0;
}
}
for (sb = bound; sb < 32; ++sb) {
nb = mad_bit_read(&stream->ptr, 4);
if (nb == 15) {
stream->error = MAD_ERROR_BADBITALLOC;
return -1;
}
allocation[0][sb] =
allocation[1][sb] = nb ? nb + 1 : 0;
}
for (sb = 0; sb < 32; ++sb) {
for (ch = 0; ch < nch; ++ch) {
if (allocation[ch][sb]) {
scalefactor[ch][sb] = mad_bit_read(&stream->ptr, 6);
# if defined(OPT_STRICT)
if (scalefactor[ch][sb] == 63) {
stream->error = MAD_ERROR_BADSCALEFACTOR;
return -1;
}
# endif
}
}
}
for (s = 0; s < 12; ++s) {
for (sb = 0; sb < bound; ++sb) {
for (ch = 0; ch < nch; ++ch) {
nb = allocation[ch][sb];
frame->sbsample[ch][s][sb] = nb ?
mad_f_mul(I_sample(&stream->ptr, nb),
sf_table[scalefactor[ch][sb]]) : 0;
}
}
for (sb = bound; sb < 32; ++sb) {
if ((nb = allocation[0][sb])) {
mad_fixed_t sample;
sample = I_sample(&stream->ptr, nb);
for (ch = 0; ch < nch; ++ch) {
frame->sbsample[ch][s][sb] =
mad_f_mul(sample, sf_table[scalefactor[ch][sb]]);
}
}
else {
for (ch = 0; ch < nch; ++ch)
frame->sbsample[ch][s][sb] = 0;
}
}
}
return 0;
}
static
struct {
unsigned int sblimit;
unsigned char const offsets[30];
} const sbquant_table[5] = {
{ 27, { 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3,
3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0 } },
{ 30, { 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 3, 3, 3, 3, 3,
3, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0 } },
{ 8, { 5, 5, 2, 2, 2, 2, 2, 2 } },
{ 12, { 5, 5, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 } },
{ 30, { 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 } }
};
static
struct {
unsigned short nbal;
unsigned short offset;
} const bitalloc_table[8] = {
{ 2, 0 },
{ 2, 3 },
{ 3, 3 },
{ 3, 1 },
{ 4, 2 },
{ 4, 3 },
{ 4, 4 },
{ 4, 5 }
};
static
unsigned char const offset_table[6][15] = {
{ 0, 1, 16 },
{ 0, 1, 2, 3, 4, 5, 16 },
{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 },
{ 0, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 },
{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 16 },
{ 0, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 }
};
static
struct quantclass {
unsigned short nlevels;
unsigned char group;
unsigned char bits;
mad_fixed_t C;
mad_fixed_t D;
} const qc_table[17] = {
# include "qc_table.dat"
};
static
void II_samples(struct mad_bitptr *ptr,
struct quantclass const *quantclass,
mad_fixed_t output[3])
{
unsigned int nb, s, sample[3];
if ((nb = quantclass->group)) {
unsigned int c, nlevels;
c = mad_bit_read(ptr, quantclass->bits);
nlevels = quantclass->nlevels;
for (s = 0; s < 3; ++s) {
sample[s] = c % nlevels;
c /= nlevels;
}
}
else {
nb = quantclass->bits;
for (s = 0; s < 3; ++s)
sample[s] = mad_bit_read(ptr, nb);
}
for (s = 0; s < 3; ++s) {
mad_fixed_t requantized;
requantized = sample[s] ^ (1 << (nb - 1));
requantized |= -(requantized & (1 << (nb - 1)));
requantized <<= MAD_F_FRACBITS - (nb - 1);
output[s] = mad_f_mul(requantized + quantclass->D, quantclass->C);
}
}
int mad_layer_II(struct mad_stream *stream, struct mad_frame *frame)
{
struct mad_header *header = &frame->header;
struct mad_bitptr start;
unsigned int index, sblimit, nbal, nch, bound, gr, ch, s, sb;
unsigned char const *offsets;
unsigned char allocation[2][32], scfsi[2][32], scalefactor[2][32][3];
mad_fixed_t samples[3];
nch = MAD_NCHANNELS(header);
if (header->flags & MAD_FLAG_LSF_EXT)
index = 4;
else if (header->flags & MAD_FLAG_FREEFORMAT)
goto freeformat;
else {
unsigned long bitrate_per_channel;
bitrate_per_channel = header->bitrate;
if (nch == 2) {
bitrate_per_channel /= 2;
# if defined(OPT_STRICT)
if (bitrate_per_channel <= 28000 || bitrate_per_channel == 40000) {
stream->error = MAD_ERROR_BADMODE;
return -1;
}
# endif
}
else {
if (bitrate_per_channel > 192000) {
stream->error = MAD_ERROR_BADMODE;
return -1;
}
}
if (bitrate_per_channel <= 48000)
index = (header->samplerate == 32000) ? 3 : 2;
else if (bitrate_per_channel <= 80000)
index = 0;
else {
freeformat:
index = (header->samplerate == 48000) ? 0 : 1;
}
}
sblimit = sbquant_table[index].sblimit;
offsets = sbquant_table[index].offsets;
bound = 32;
if (header->mode == MAD_MODE_JOINT_STEREO) {
header->flags |= MAD_FLAG_I_STEREO;
bound = 4 + header->mode_extension * 4;
}
if (bound > sblimit)
bound = sblimit;
start = stream->ptr;
for (sb = 0; sb < bound; ++sb) {
nbal = bitalloc_table[offsets[sb]].nbal;
for (ch = 0; ch < nch; ++ch)
allocation[ch][sb] = mad_bit_read(&stream->ptr, nbal);
}
for (sb = bound; sb < sblimit; ++sb) {
nbal = bitalloc_table[offsets[sb]].nbal;
allocation[0][sb] =
allocation[1][sb] = mad_bit_read(&stream->ptr, nbal);
}
for (sb = 0; sb < sblimit; ++sb) {
for (ch = 0; ch < nch; ++ch) {
if (allocation[ch][sb])
scfsi[ch][sb] = mad_bit_read(&stream->ptr, 2);
}
}
if (header->flags & MAD_FLAG_PROTECTION) {
header->crc_check =
mad_bit_crc(start, mad_bit_length(&start, &stream->ptr),
header->crc_check);
if (header->crc_check != header->crc_target &&
!(frame->options & MAD_OPTION_IGNORECRC)) {
stream->error = MAD_ERROR_BADCRC;
return -1;
}
}
for (sb = 0; sb < sblimit; ++sb) {
for (ch = 0; ch < nch; ++ch) {
if (allocation[ch][sb]) {
scalefactor[ch][sb][0] = mad_bit_read(&stream->ptr, 6);
switch (scfsi[ch][sb]) {
case 2:
scalefactor[ch][sb][2] =
scalefactor[ch][sb][1] =
scalefactor[ch][sb][0];
break;
case 0:
scalefactor[ch][sb][1] = mad_bit_read(&stream->ptr, 6);
case 1:
case 3:
scalefactor[ch][sb][2] = mad_bit_read(&stream->ptr, 6);
}
if (scfsi[ch][sb] & 1)
scalefactor[ch][sb][1] = scalefactor[ch][sb][scfsi[ch][sb] - 1];
# if defined(OPT_STRICT)
if (scalefactor[ch][sb][0] == 63 ||
scalefactor[ch][sb][1] == 63 ||
scalefactor[ch][sb][2] == 63) {
stream->error = MAD_ERROR_BADSCALEFACTOR;
return -1;
}
# endif
}
}
}
for (gr = 0; gr < 12; ++gr) {
for (sb = 0; sb < bound; ++sb) {
for (ch = 0; ch < nch; ++ch) {
if ((index = allocation[ch][sb])) {
index = offset_table[bitalloc_table[offsets[sb]].offset][index - 1];
II_samples(&stream->ptr, &qc_table[index], samples);
for (s = 0; s < 3; ++s) {
frame->sbsample[ch][3 * gr + s][sb] =
mad_f_mul(samples[s], sf_table[scalefactor[ch][sb][gr / 4]]);
}
}
else {
for (s = 0; s < 3; ++s)
frame->sbsample[ch][3 * gr + s][sb] = 0;
}
}
}
for (sb = bound; sb < sblimit; ++sb) {
if ((index = allocation[0][sb])) {
index = offset_table[bitalloc_table[offsets[sb]].offset][index - 1];
II_samples(&stream->ptr, &qc_table[index], samples);
for (ch = 0; ch < nch; ++ch) {
for (s = 0; s < 3; ++s) {
frame->sbsample[ch][3 * gr + s][sb] =
mad_f_mul(samples[s], sf_table[scalefactor[ch][sb][gr / 4]]);
}
}
}
else {
for (ch = 0; ch < nch; ++ch) {
for (s = 0; s < 3; ++s)
frame->sbsample[ch][3 * gr + s][sb] = 0;
}
}
}
for (ch = 0; ch < nch; ++ch) {
for (s = 0; s < 3; ++s) {
for (sb = sblimit; sb < 32; ++sb)
frame->sbsample[ch][3 * gr + s][sb] = 0;
}
}
}
return 0;
}