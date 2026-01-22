#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "lame-analysis.h"
#include "lame.h"
#include "util.h"
#include "bitstream.h"
#include "version.h"
#include "tables.h"
#include "quantize_pvt.h"
#include "VbrTag.h"
#if defined(__FreeBSD__) && !defined(__alpha__)
#include <floatingpoint.h>
#endif
#ifdef __riscos__
#include "asmstuff.h"
#endif
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
static void
lame_init_params_ppflt_lowpass(FLOAT8 amp_lowpass[32], FLOAT lowpass1,
FLOAT lowpass2, int *lowpass_band,
int *minband, int *maxband)
{
int     band;
FLOAT8  freq;
for (band = 0; band <= 31; band++) {
freq = band / 31.0;
amp_lowpass[band] = 1;
if (freq >= lowpass2) {
*lowpass_band = Min(*lowpass_band, band);
amp_lowpass[band] = 0;
}
if (lowpass1 < freq && freq < lowpass2) {
*minband = Min(*minband, band);
*maxband = Max(*maxband, band);
amp_lowpass[band] = cos((PI / 2) *
(lowpass1 - freq) / (lowpass2 - lowpass1));
}
}
}
static void
lame_init_params_ppflt(lame_global_flags * gfp)
{
lame_internal_flags *gfc = gfp->internal_flags;
int     band, maxband, minband;
FLOAT8  freq;
if (gfc->lowpass1 > 0) {
minband = 999;
maxband = -1;
lame_init_params_ppflt_lowpass(gfc->amp_lowpass,
gfc->lowpass1, gfc->lowpass2,
&gfc->lowpass_band, &minband, &maxband);
if (minband == 999) {
gfc->lowpass1 = (gfc->lowpass_band - .75) / 31.0;
}
else {
gfc->lowpass1 = (minband - .75) / 31.0;
}
gfc->lowpass2 = gfc->lowpass_band / 31.0;
gfc->lowpass_start_band = minband;
gfc->lowpass_end_band = maxband;
for (band = minband; band <= maxband; band++) {
freq = band / 31.0;
gfc->amp_lowpass[band] =
cos((PI / 2) * (gfc->lowpass1 - freq) /
(gfc->lowpass2 - gfc->lowpass1));
}
}
else {
gfc->lowpass_start_band = 0;
gfc->lowpass_end_band = -1;
}
if (gfc->highpass2 > 0) {
if (gfc->highpass2 < .9 * (.75 / 31.0)) {
gfc->highpass1 = 0;
gfc->highpass2 = 0;
MSGF(gfc, "Warning: highpass filter disabled.  "
"highpass frequency too small\n");
}
}
if (gfc->highpass2 > 0) {
minband = 999;
maxband = -1;
for (band = 0; band <= 31; band++) {
freq = band / 31.0;
gfc->amp_highpass[band] = 1;
if (freq <= gfc->highpass1) {
gfc->highpass_band = Max(gfc->highpass_band, band);
gfc->amp_highpass[band] = 0;
}
if (gfc->highpass1 < freq && freq < gfc->highpass2) {
minband = Min(minband, band);
maxband = Max(maxband, band);
gfc->amp_highpass[band] =
cos((PI / 2) *
(gfc->highpass2 - freq) /
(gfc->highpass2 - gfc->highpass1));
}
}
gfc->highpass1 = gfc->highpass_band / 31.0;
if (maxband == -1) {
gfc->highpass2 = (gfc->highpass_band + .75) / 31.0;
}
else {
gfc->highpass2 = (maxband + .75) / 31.0;
}
gfc->highpass_start_band = minband;
gfc->highpass_end_band = maxband;
for (band = minband; band <= maxband; band++) {
freq = band / 31.0;
gfc->amp_highpass[band] =
cos((PI / 2) * (gfc->highpass2 - freq) /
(gfc->highpass2 - gfc->highpass1));
}
}
else {
gfc->highpass_start_band = 0;
gfc->highpass_end_band = -1;
}
}
static void
optimum_bandwidth(double *const lowerlimit,
double *const upperlimit,
const unsigned bitrate,
const int samplefreq,
const double channels, lame_global_flags * gfp)
{
double  f_low;
double  f_high;
double  br;
assert(bitrate >= 8000 && bitrate <= 320000);
assert(samplefreq >= 8000 && samplefreq <= 48000);
assert(channels == 1 || (channels >= 2 && channels <= 3));
if (samplefreq >= 32000)
br =
bitrate - (channels ==
1 ? (17 + 4) * 8 : (32 + 4) * 8) * samplefreq / 1152;
else
br =
bitrate - (channels ==
1 ? (9 + 4) * 8 : (17 + 4) * 8) * samplefreq / 576;
if (channels >= 2.)
br /= 1.75 + 0.25 * (channels - 2.);
br *= 0.5;
f_low = br / log10(br * 4.425e-3);
#if 0
{
double  br_sw = (128000 - (32 + 4) * 8 * 44100 / 1152) / 1.75 * 0.5;
double  f_low_sw = br_sw / log10(br_sw * 4.425e-3);
while (f_low > f_low_sw) {
double  dATH = ATHformula(f_low, gfp) - ATHformula(f_low_sw, gfp);
double  dNMR = br / f_low - br_sw / f_low_sw;
if (dATH / 4.0 < dNMR * 6.0206 / 1.25)
break;
f_low -= 25.;
}
}
#endif
if (f_low <= 16000)
f_high = 16000. * 20. / f_low;
else if (f_low <= 18000)
f_high = 180. - 0.01 * f_low;
else
f_high = 0.;
if (lowerlimit != NULL)
*lowerlimit = f_low  ;
if (upperlimit != NULL)
*upperlimit = f_high;
}
static int
optimum_samplefreq(int lowpassfreq, int input_samplefreq)
{
if (input_samplefreq <= 8000 * 1.03 || lowpassfreq <= 3622)
return 8000;
if (input_samplefreq <= 11025 * 1.03 || lowpassfreq <= 4991)
return 11025;
if (input_samplefreq <= 12000 * 1.03 || lowpassfreq <= 5620)
return 12000;
if (input_samplefreq <= 16000 * 1.03 || lowpassfreq <= 7244)
return 16000;
if (input_samplefreq <= 22050 * 1.03 || lowpassfreq <= 9982)
return 22050;
if (input_samplefreq <= 24000 * 1.03 || lowpassfreq <= 11240)
return 24000;
if (input_samplefreq <= 32000 * 1.03 || lowpassfreq <= 15264)
return 32000;
if (input_samplefreq <= 44100 * 1.03)
return 44100;
return 48000;
}
void
lame_init_qval(lame_global_flags * gfp)
{
lame_internal_flags *gfc = gfp->internal_flags;
switch (gfp->quality) {
case 9:
gfc->filter_type = 0;
gfc->psymodel = 0;
gfc->quantization = 0;
gfc->noise_shaping = 0;
gfc->noise_shaping_amp = 0;
gfc->noise_shaping_stop = 0;
gfc->use_best_huffman = 0;
break;
case 8:
gfp->quality = 7;
case 7:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 0;
gfc->noise_shaping = 0;
gfc->noise_shaping_amp = 0;
gfc->noise_shaping_stop = 0;
gfc->use_best_huffman = 0;
break;
case 6:
gfp->quality = 5;
case 5:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 0;
gfc->noise_shaping = 1;
gfc->noise_shaping_amp = 0;
gfc->noise_shaping_stop = 0;
gfc->use_best_huffman = 0;
break;
case 4:
gfp->quality = 3;
case 3:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 1;
gfc->noise_shaping = 1;
gfc->noise_shaping_amp = 0;
gfc->noise_shaping_stop = 0;
gfc->use_best_huffman = 1;
break;
case 2:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 1;
gfc->noise_shaping = 1;
gfc->noise_shaping_amp = 1;
gfc->noise_shaping_stop = 1;
gfc->use_best_huffman = 1;
break;
case 1:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 1;
gfc->noise_shaping = 1;
gfc->noise_shaping_amp = 2;
gfc->noise_shaping_stop = 1;
gfc->use_best_huffman = 1;
break;
case 0:
gfc->filter_type = 0;
gfc->psymodel = 1;
gfc->quantization = 1;
gfc->noise_shaping = 1;
gfc->noise_shaping_amp = 2;
gfc->noise_shaping_stop = 1;
gfc->use_best_huffman = 1;
}
if (gfp->experimentalZ) {
gfc->noise_shaping = 2;
}
if (gfp->exp_nspsytune & 1) {
if (gfp->quality <= 2)
gfc->noise_shaping = 2;
}
}
int
lame_init_params(lame_global_flags * const gfp)
{
int     i;
int     j;
lame_internal_flags *gfc = gfp->internal_flags;
gfc->gfp = gfp;
gfc->Class_ID = 0;
gfc->report.msgf   = gfp->report.msgf;
gfc->report.debugf = gfp->report.debugf;
gfc->report.errorf = gfp->report.errorf;
gfc->CPU_features.i387 = has_i387();
gfc->CPU_features.AMD_3DNow = has_3DNow();
gfc->CPU_features.MMX = has_MMX();
gfc->CPU_features.SIMD = has_SIMD();
gfc->CPU_features.SIMD2 = has_SIMD2();
if (NULL == gfc->ATH)
gfc->ATH = calloc(1, sizeof(ATH_t));
if (NULL == gfc->ATH)
return -2;
#ifdef KLEMM_44
init_scalar_functions(gfc);
#endif
gfc->channels_in = gfp->num_channels;
if (gfc->channels_in == 1)
gfp->mode = MONO;
gfc->channels_out = (gfp->mode == MONO) ? 1 : 2;
gfc->mode_ext = MPG_MD_LR_LR;
if (gfp->mode == MONO)
gfp->force_ms = 0;
if (gfp->VBR != vbr_off) {
gfp->free_format = 0;
}
if (gfp->VBR == vbr_off && gfp->brate == 0) {
if (gfp->compression_ratio == 0)
gfp->compression_ratio = 11.025;
}
if (gfp->VBR == vbr_off && gfp->brate == 0) {
if (gfp->compression_ratio == 0)
gfp->compression_ratio = 11.025;
}
if (gfp->VBR == vbr_off && gfp->compression_ratio > 0) {
if (gfp->out_samplerate == 0)
gfp->out_samplerate = map2MP3Frequency(0.97 * gfp->in_samplerate);
gfp->brate = gfp->out_samplerate * 16 * gfc->channels_out / (1.e3 *
gfp->compression_ratio);
gfc->samplerate_index = SmpFrqIndex(gfp->out_samplerate, &gfp->version);
if (!gfp->free_format)
gfp->brate =
FindNearestBitrate(gfp->brate, gfp->version,
gfp->out_samplerate);
}
if (gfp->VBR != vbr_off && gfp->brate >= 320)
gfp->VBR = vbr_off;
if (gfp->out_samplerate == 0) {
gfp->out_samplerate = map2MP3Frequency(0.97 * gfp->in_samplerate);
if (gfp->VBR == vbr_off && gfp->brate > 0) {
gfp->compression_ratio = gfp->out_samplerate * 16 *
gfc->channels_out / (1.e3 * gfp->brate);
if (gfp->compression_ratio > 13.)
gfp->out_samplerate = map2MP3Frequency((10. * 1.e3 *
gfp->brate) / (16 * gfc->channels_out));
}
if (gfp->VBR == vbr_abr) {
gfp->compression_ratio = gfp->out_samplerate * 16 *
gfc->channels_out / (1.e3 * gfp->VBR_mean_bitrate_kbps);
if (gfp->compression_ratio > 13.)
gfp->out_samplerate =
map2MP3Frequency((10. * 1.e3 * gfp->VBR_mean_bitrate_kbps) /
(16 * gfc->channels_out));
}
}
if (gfp->ogg) {
gfp->framesize = 1024;
gfp->encoder_delay = ENCDELAY;
gfc->coding = coding_Ogg_Vorbis;
}
else {
gfc->mode_gr = gfp->out_samplerate <= 24000 ? 1 : 2;
gfp->framesize = 576 * gfc->mode_gr;
gfp->encoder_delay = ENCDELAY;
gfc->coding = coding_MPEG_Layer_3;
}
gfc->frame_size = gfp->framesize;
gfc->resample_ratio = (double) gfp->in_samplerate / gfp->out_samplerate;
switch (gfp->VBR) {
case vbr_mt:
case vbr_rh:
case vbr_mtrh:
{
FLOAT8  cmp[] = { 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 };
gfp->compression_ratio = cmp[gfp->VBR_q];
}
break;
case vbr_abr:
gfp->compression_ratio = gfp->out_samplerate * 16 * gfc->channels_out /
(1.e3 * gfp->VBR_mean_bitrate_kbps);
break;
default:
gfp->compression_ratio =
gfp->out_samplerate * 16 * gfc->channels_out / (1.e3 * gfp->brate);
break;
}
if (gfp->mode == NOT_SET) {
if (gfp->compression_ratio < 8)
gfp->mode = STEREO;
else
gfp->mode = JOINT_STEREO;
}
if (gfp->mode_automs) {
if (gfp->mode != MONO && gfp->compression_ratio < 6.6)
gfp->mode = STEREO;
}
if (gfp->allow_diff_short == -1) {
if (gfp->mode == STEREO)
gfp->allow_diff_short = 1;
}
if (gfp->lowpassfreq == 0) {
double  lowpass;
double  highpass;
double  channels;
switch (gfp->mode) {
case MONO:
channels = 1.;
break;
case JOINT_STEREO:
channels = 2. + 0.00;
break;
case DUAL_CHANNEL:
case STEREO:
channels = 3.;
break;
default:
channels = 1.;
assert(0);
break;
}
optimum_bandwidth(&lowpass,
&highpass,
gfp->out_samplerate * 16 * gfc->channels_out /
gfp->compression_ratio, gfp->out_samplerate, channels,
gfp);
if (lowpass < 0.5 * gfp->out_samplerate) {
gfc->lowpass1 = gfc->lowpass2 =
lowpass / (0.5 * gfp->out_samplerate);
}
if (0 && gfp->out_samplerate !=
optimum_samplefreq(lowpass, gfp->in_samplerate)) {
MSGF(gfc,
"I would suggest to use %u Hz instead of %u Hz sample frequency\n",
optimum_samplefreq(lowpass, gfp->in_samplerate),
gfp->out_samplerate);
}
fflush(stderr);
}
if (gfp->highpassfreq > 0) {
gfc->highpass1 = 2. * gfp->highpassfreq / gfp->out_samplerate;
if (gfp->highpasswidth >= 0)
gfc->highpass2 = 2. * (gfp->highpassfreq + gfp->highpasswidth) /
gfp->out_samplerate;
else
gfc->highpass2 =
(1 + 0.00) * 2. * gfp->highpassfreq / gfp->out_samplerate;
}
if (gfp->lowpassfreq > 0) {
gfc->lowpass2 = 2. * gfp->lowpassfreq / gfp->out_samplerate;
if (gfp->lowpasswidth >= 0) {
gfc->lowpass1 = 2. * (gfp->lowpassfreq - gfp->lowpasswidth) /
gfp->out_samplerate;
if (gfc->lowpass1 < 0)
gfc->lowpass1 = 0;
}
else {
gfc->lowpass1 =
(1 - 0.00) * 2. * gfp->lowpassfreq / gfp->out_samplerate;
}
}
lame_init_params_ppflt(gfp);
gfc->samplerate_index = SmpFrqIndex(gfp->out_samplerate, &gfp->version);
if (gfc->samplerate_index < 0)
return -1;
if (gfp->VBR == vbr_off) {
if (gfp->free_format)
gfc->bitrate_index = 0;
else {
gfc->bitrate_index = BitrateIndex(gfp->brate, gfp->version,
gfp->out_samplerate);
if (gfc->bitrate_index < 0)
return -1;
}
}
else {
gfc->VBR_min_bitrate = 1;
gfc->VBR_max_bitrate = 14;
if (gfp->VBR_min_bitrate_kbps)
if (
(gfc->VBR_min_bitrate =
BitrateIndex(gfp->VBR_min_bitrate_kbps, gfp->version,
gfp->out_samplerate)) < 0) return -1;
if (gfp->VBR_max_bitrate_kbps)
if (
(gfc->VBR_max_bitrate =
BitrateIndex(gfp->VBR_max_bitrate_kbps, gfp->version,
gfp->out_samplerate)) < 0) return -1;
gfp->VBR_min_bitrate_kbps =
bitrate_table[gfp->version][gfc->VBR_min_bitrate];
gfp->VBR_max_bitrate_kbps =
bitrate_table[gfp->version][gfc->VBR_max_bitrate];
gfp->VBR_mean_bitrate_kbps =
Min(bitrate_table[gfp->version][gfc->VBR_max_bitrate],
gfp->VBR_mean_bitrate_kbps);
gfp->VBR_mean_bitrate_kbps =
Max(bitrate_table[gfp->version][gfc->VBR_min_bitrate],
gfp->VBR_mean_bitrate_kbps);
}
if (gfp->VBR == vbr_off)
gfp->bWriteVbrTag = 0;
if (gfp->ogg)
gfp->bWriteVbrTag = 0;
if (gfp->analysis)
gfp->bWriteVbrTag = 0;
if (gfc->pinfo != NULL)
gfp->bWriteVbrTag = 0;
init_bit_stream_w(gfc);
j = gfc->samplerate_index + (3 * gfp->version) + 6 * (gfp->out_samplerate <
16000);
for (i = 0; i < SBMAX_l + 1; i++)
gfc->scalefac_band.l[i] = sfBandIndex[j].l[i];
for (i = 0; i < SBMAX_s + 1; i++)
gfc->scalefac_band.s[i] = sfBandIndex[j].s[i];
if (gfp->version == 1)
gfc->sideinfo_len = (gfc->channels_out == 1) ? 4 + 17 : 4 + 32;
else
gfc->sideinfo_len = (gfc->channels_out == 1) ? 4 + 9 : 4 + 17;
if (gfp->error_protection)
gfc->sideinfo_len += 2;
if (!gfp->ogg)
id3tag_write_v2(gfp);
if (gfp->bWriteVbrTag)
InitVbrTag(gfp);
if (gfp->version == 1)
gfc->is_mpeg1 = 1;
else
gfc->is_mpeg1 = 0;
gfp->totalframes =
2 + gfp->num_samples / (gfc->resample_ratio * gfp->framesize);
gfc->Class_ID = LAME_ID;
if (gfp->exp_nspsytune & 1) {
int     i;
gfc->nsPsy.use = 1;
gfc->nsPsy.safejoint = (gfp->exp_nspsytune & 2) != 0;
for (i = 0; i < 19; i++)
gfc->nsPsy.pefirbuf[i] = 700;
if (gfp->VBR == vbr_mtrh || gfp->VBR == vbr_mt) {
ERRORF(gfc, "\n**** nspsytune doesn't support --vbr-new **** \n\n");
gfp->VBR = vbr_rh;
}
if (gfp->ATHtype == -1)
gfp->ATHtype = 0;
gfc->nsPsy.bass = gfc->nsPsy.alto = gfc->nsPsy.treble = 0;
i = (gfp->exp_nspsytune >> 2) & 63;
if (i >= 32)
i -= 64;
gfc->nsPsy.bass = pow(10, i / 4.0 / 10.0);
i = (gfp->exp_nspsytune >> 8) & 63;
if (i >= 32)
i -= 64;
gfc->nsPsy.alto = pow(10, i / 4.0 / 10.0);
i = (gfp->exp_nspsytune >> 14) & 63;
if (i >= 32)
i -= 64;
gfc->nsPsy.treble = pow(10, i / 4.0 / 10.0);
}
switch (gfp->VBR) {
case vbr_mtrh:
if (gfp->quality < 0)
gfp->quality = 1;
if (gfp->cwlimit <= 0)
gfp->cwlimit = 0.454 * gfp->out_samplerate;
case vbr_mt:
if (gfp->ATHtype == -1)
gfp->ATHtype = 2;
case vbr_rh:
if (gfp->ATHtype == -1)
gfp->ATHtype = 2;
gfc->ATH->use_adjust = 1;
gfc->sfb21_extra = (gfp->out_samplerate > 44000);
if (gfp->quality > 5)
gfp->quality = 5;
if (gfp->quality < 0)
gfp->quality = 2;
gfp->allow_diff_short = 1;
break;
default:
gfc->ATH->use_adjust = 0;
if (gfp->ATHtype == -1)
gfp->ATHtype = 2;
gfc->sfb21_extra = 0;
if (gfp->quality < 0)
gfp->quality = 5;
break;
}
lame_init_qval(gfp);
#ifdef KLEMM_44
gfc->mfbuf[0] = (sample_t *) calloc(sizeof(sample_t), MFSIZE);
gfc->mfbuf[1] = (sample_t *) calloc(sizeof(sample_t), MFSIZE);
gfc->sampfreq_in = unround_samplefrequency(gfp->in_samplerate);
gfc->sampfreq_out = gfp->out_samplerate;
gfc->resample_in = resample_open(gfc->sampfreq_in, gfc->sampfreq_out,
-1 .0  , 32);
#endif
return 0;
}
void
lame_print_config(const lame_global_flags * gfp)
{
lame_internal_flags *gfc = gfp->internal_flags;
double  out_samplerate = gfp->out_samplerate;
double  in_samplerate = gfp->out_samplerate * gfc->resample_ratio;
MSGF(gfc, "mp3enc (from lame version %s (%s))\n", get_lame_version(), get_lame_url());
if (gfc->CPU_features.MMX
|| gfc->CPU_features.AMD_3DNow
|| gfc->CPU_features.SIMD || gfc->CPU_features.SIMD2) {
MSGF(gfc, "CPU features:");
if (gfc->CPU_features.i387)
MSGF(gfc, " i387");
if (gfc->CPU_features.MMX)
#ifdef MMX_choose_table
MSGF(gfc, ", MMX (ASM used)");
#else
MSGF(gfc, ", MMX");
#endif
if (gfc->CPU_features.AMD_3DNow)
MSGF(gfc, ", 3DNow!");
if (gfc->CPU_features.SIMD)
MSGF(gfc, ", SIMD");
if (gfc->CPU_features.SIMD2)
MSGF(gfc, ", SIMD2");
MSGF(gfc, "\n");
}
if (gfp->num_channels == 2 && gfc->channels_out == 1  ) {
MSGF
(gfc,
"Autoconverting from stereo to mono. Setting encoding to mono mode.\n");
}
if (gfc->resample_ratio != 1.) {
MSGF(gfc, "Resampling:  input %g kHz  output %g kHz\n",
1.e-3 * in_samplerate, 1.e-3 * out_samplerate);
}
if (gfc->filter_type == 0) {
if (gfc->highpass2 > 0.)
MSGF
(gfc,
"Using polyphase highpass filter, transition band: %5.0f Hz - %5.0f Hz\n",
0.5 * gfc->highpass1 * out_samplerate,
0.5 * gfc->highpass2 * out_samplerate);
if (gfc->lowpass1 > 0.) {
MSGF
(gfc,
"Using polyphase lowpass  filter, transition band: %5.0f Hz - %5.0f Hz\n",
0.5 * gfc->lowpass1 * out_samplerate,
0.5 * gfc->lowpass2 * out_samplerate);
}
else {
MSGF(gfc, "polyphase lowpass filter disabled\n");
}
}
else {
MSGF(gfc, "polyphase filters disabled\n");
}
if (gfp->free_format) {
MSGF(gfc,
"Warning: many decoders cannot handle free format bitstreams\n");
if (gfp->brate > 320) {
MSGF
(gfc,
"Warning: many decoders cannot handle free format bitrates >320 kbps (see documentation)\n");
}
}
}
int
lame_encode_frame(lame_global_flags * gfp,
sample_t inbuf_l[], sample_t inbuf_r[],
unsigned char *mp3buf, int mp3buf_size)
{
int     ret;
if (gfp->ogg) {
#ifdef HAVE_VORBIS
ret = lame_encode_ogg_frame(gfp, inbuf_l, inbuf_r, mp3buf, mp3buf_size);
#else
return -5;
#endif
}
else {
ret = lame_encode_mp3_frame(gfp, inbuf_l, inbuf_r, mp3buf, mp3buf_size);
}
gfp->frameNum++;
if (gfp->totalframes < gfp->frameNum)
gfp->totalframes = gfp->frameNum;
return ret;
}
int
lame_encode_buffer_sample_t(lame_global_flags * gfp,
sample_t buffer_l[],
sample_t buffer_r[],
int nsamples, unsigned char *mp3buf, const int mp3buf_size)
{
lame_internal_flags *gfc = gfp->internal_flags;
int     mp3size = 0, ret, i, ch, mf_needed;
sample_t *mfbuf[2];
sample_t *in_buffer[2];
if (gfc->Class_ID != LAME_ID)
return -3;
if (nsamples == 0)
return 0;
in_buffer[0]=buffer_l;
in_buffer[1]=buffer_r;
#if ENCDELAY < MDCTDELAY
# error ENCDELAY is less than MDCTDELAY, see encoder.h
#endif
#if FFTOFFSET > BLKSIZE
# error FFTOFFSET is greater than BLKSIZE, see encoder.h
#endif
mf_needed = BLKSIZE + gfp->framesize - FFTOFFSET;
mf_needed = Max(mf_needed, 286 + 576 * (1 + gfc->mode_gr));
assert(MFSIZE >= mf_needed);
mfbuf[0] = gfc->mfbuf[0];
mfbuf[1] = gfc->mfbuf[1];
if (gfp->num_channels == 2 && gfc->channels_out == 1) {
for (i = 0; i < nsamples; ++i) {
in_buffer[0][i] =
0.5 * ((FLOAT8) in_buffer[0][i] + in_buffer[1][i]);
in_buffer[1][i] = 0.0;
}
}
while (nsamples > 0) {
int     n_in = 0;
int     n_out = 0;
fill_buffer(gfp, mfbuf, in_buffer, nsamples, &n_in, &n_out);
nsamples -= n_in;
in_buffer[0] += n_in;
if (gfc->channels_out == 2)
in_buffer[1] += n_in;
gfc->mf_size += n_out;
assert(gfc->mf_size <= MFSIZE);
gfc->mf_samples_to_encode += n_out;
if (gfc->mf_size >= mf_needed) {
ret =
lame_encode_frame(gfp, mfbuf[0], mfbuf[1], mp3buf, mp3buf_size);
if (ret < 0)
goto retr;
mp3buf += ret;
mp3size += ret;
gfc->mf_size -= gfp->framesize;
gfc->mf_samples_to_encode -= gfp->framesize;
for (ch = 0; ch < gfc->channels_out; ch++)
for (i = 0; i < gfc->mf_size; i++)
mfbuf[ch][i] = mfbuf[ch][i + gfp->framesize];
}
}
assert(nsamples == 0);
ret = mp3size;
retr:
return ret;
}
int
lame_encode_buffer(lame_global_flags * gfp,
const short int buffer_l[],
const short int buffer_r[],
int nsamples, unsigned char *mp3buf, const int mp3buf_size)
{
lame_internal_flags *gfc = gfp->internal_flags;
int     ret, i;
sample_t *in_buffer[2];
if (gfc->Class_ID != LAME_ID)
return -3;
if (nsamples == 0)
return 0;
in_buffer[0] = calloc(sizeof(sample_t), nsamples);
in_buffer[1] = calloc(sizeof(sample_t), nsamples);
if (in_buffer[0] == NULL || in_buffer[1] == NULL) {
ERRORF(gfc, "Error: can't allocate in_buffer buffer\n");
return -2;
}
for (i = 0; i < nsamples; i++) {
in_buffer[0][i] = buffer_l[i];
in_buffer[1][i] = buffer_r[i];
}
ret = lame_encode_buffer_sample_t(gfp,in_buffer[0],in_buffer[1],
nsamples, mp3buf, mp3buf_size);
free(in_buffer[0]);
free(in_buffer[1]);
return ret;
}
int
lame_encode_buffer_float(lame_global_flags * gfp,
const float buffer_l[],
const float buffer_r[],
int nsamples, unsigned char *mp3buf, const int mp3buf_size)
{
lame_internal_flags *gfc = gfp->internal_flags;
int     ret, i;
sample_t *in_buffer[2];
if (gfc->Class_ID != LAME_ID)
return -3;
if (nsamples == 0)
return 0;
in_buffer[0] = calloc(sizeof(sample_t), nsamples);
in_buffer[1] = calloc(sizeof(sample_t), nsamples);
if (in_buffer[0] == NULL || in_buffer[1] == NULL) {
ERRORF(gfc, "Error: can't allocate in_buffer buffer\n");
return -2;
}
for (i = 0; i < nsamples; i++) {
in_buffer[0][i] = buffer_l[i];
in_buffer[1][i] = buffer_r[i];
}
ret = lame_encode_buffer_sample_t(gfp,in_buffer[0],in_buffer[1],
nsamples, mp3buf, mp3buf_size);
free(in_buffer[0]);
free(in_buffer[1]);
return ret;
}
int
lame_encode_buffer_long(lame_global_flags * gfp,
const long buffer_l[],
const long buffer_r[],
int nsamples, unsigned char *mp3buf, const int mp3buf_size)
{
lame_internal_flags *gfc = gfp->internal_flags;
int     ret, i;
sample_t *in_buffer[2];
if (gfc->Class_ID != LAME_ID)
return -3;
if (nsamples == 0)
return 0;
in_buffer[0] = calloc(sizeof(sample_t), nsamples);
in_buffer[1] = calloc(sizeof(sample_t), nsamples);
if (in_buffer[0] == NULL || in_buffer[1] == NULL) {
ERRORF(gfc, "Error: can't allocate in_buffer buffer\n");
return -2;
}
for (i = 0; i < nsamples; i++) {
in_buffer[0][i] = buffer_l[i];
in_buffer[1][i] = buffer_r[i];
}
ret = lame_encode_buffer_sample_t(gfp,in_buffer[0],in_buffer[1],
nsamples, mp3buf, mp3buf_size);
free(in_buffer[0]);
free(in_buffer[1]);
return ret;
}
int
lame_encode_buffer_interleaved(lame_global_flags * gfp,
short int buffer[],
int nsamples,
unsigned char *mp3buf, int mp3buf_size)
{
int     ret, i;
short int *buffer_l;
short int *buffer_r;
buffer_l = malloc(sizeof(short int) * nsamples);
buffer_r = malloc(sizeof(short int) * nsamples);
if (buffer_l == NULL || buffer_r == NULL) {
return -2;
}
for (i = 0; i < nsamples; i++) {
buffer_l[i] = buffer[2 * i];
buffer_r[i] = buffer[2 * i + 1];
}
ret =
lame_encode_buffer(gfp, buffer_l, buffer_r, nsamples, mp3buf,
mp3buf_size);
free(buffer_l);
free(buffer_r);
return ret;
}
int
lame_encode(lame_global_flags * const gfp,
const short int in_buffer[2][1152],
unsigned char *const mp3buf, const int size)
{
lame_internal_flags *gfc = gfp->internal_flags;
if (gfc->Class_ID != LAME_ID)
return -3;
return lame_encode_buffer(gfp, in_buffer[0], in_buffer[1], gfp->framesize,
mp3buf, size);
}
int
lame_encode_flush(lame_global_flags * gfp,
unsigned char *mp3buffer, int mp3buffer_size)
{
short int buffer[2][1152];
int     imp3 = 0, mp3count, mp3buffer_size_remaining;
lame_internal_flags *gfc = gfp->internal_flags;
memset(buffer, 0, sizeof(buffer));
mp3count = 0;
while (gfc->mf_samples_to_encode > 0) {
mp3buffer_size_remaining = mp3buffer_size - mp3count;
if (mp3buffer_size == 0)
mp3buffer_size_remaining = 0;
imp3 = lame_encode_buffer(gfp, buffer[0], buffer[1], gfp->framesize,
mp3buffer, mp3buffer_size_remaining);
gfc->mf_samples_to_encode -= gfp->framesize;
if (imp3 < 0) {
return imp3;
}
mp3buffer += imp3;
mp3count += imp3;
}
mp3buffer_size_remaining = mp3buffer_size - mp3count;
if (mp3buffer_size == 0)
mp3buffer_size_remaining = 0;
if (gfp->ogg) {
#ifdef HAVE_VORBIS
imp3 = lame_encode_ogg_finish(gfp, mp3buffer, mp3buffer_size_remaining);
#endif
}
else {
flush_bitstream(gfp);
id3tag_write_v1(gfp);
imp3 = copy_buffer(mp3buffer, mp3buffer_size_remaining, &gfc->bs);
}
if (imp3 < 0) {
return imp3;
}
mp3count += imp3;
return mp3count;
}
int
lame_close(lame_global_flags * gfp)
{
lame_internal_flags *gfc = gfp->internal_flags;
if (gfc->Class_ID != LAME_ID)
return -3;
gfc->Class_ID = 0;
freegfc(gfc);
gfp->internal_flags = NULL;
if (gfp->lame_allocated_gfp)
free(gfp);
return 0;
}
int
lame_encode_finish(lame_global_flags * gfp,
unsigned char *mp3buffer, int mp3buffer_size)
{
int     ret = lame_encode_flush(gfp, mp3buffer, mp3buffer_size);
lame_close(gfp);
return ret;
}
void
lame_mp3_tags_fid(lame_global_flags * gfp, FILE * fpStream)
{
if (gfp->bWriteVbrTag && (gfp->VBR != vbr_off)) {
int     nQuality = ((9-gfp->VBR_q) * 100) / 9;
if (fpStream && !fseek(fpStream, 0, SEEK_SET))
PutVbrTag(gfp, fpStream, nQuality);
}
}
lame_global_flags *
lame_init(void)
{
lame_global_flags *gfp;
int     ret;
gfp = calloc(1, sizeof(lame_global_flags));
if (gfp == NULL)
return NULL;
ret = lame_init_old(gfp);
if (ret != 0) {
free(gfp);
return NULL;
}
gfp->lame_allocated_gfp = 1;
return gfp;
}
int
lame_init_old(lame_global_flags * gfp)
{
lame_internal_flags *gfc;
disable_FPE();
memset(gfp, 0, sizeof(lame_global_flags));
if (NULL ==
(gfc = gfp->internal_flags =
calloc(1, sizeof(lame_internal_flags)))) return -1;
gfp->mode = NOT_SET;
gfp->original = 1;
gfp->in_samplerate = 1000 * 44.1;
gfp->num_channels = 2;
gfp->num_samples = MAX_U_32_NUM;
gfp->bWriteVbrTag = 1;
gfp->quality = -1;
gfp->allow_diff_short = -1;
gfp->lowpassfreq = 0;
gfp->highpassfreq = 0;
gfp->lowpasswidth = -1;
gfp->highpasswidth = -1;
gfp->padding_type = 2;
gfp->VBR = vbr_off;
gfp->VBR_q = 4;
gfp->VBR_mean_bitrate_kbps = 128;
gfp->VBR_min_bitrate_kbps = 0;
gfp->VBR_max_bitrate_kbps = 0;
gfp->VBR_hard_min = 0;
gfc->resample_ratio = 1;
gfc->lowpass_band = 32;
gfc->highpass_band = -1;
gfc->VBR_min_bitrate = 1;
gfc->VBR_max_bitrate = 13;
gfc->OldValue[0] = 180;
gfc->OldValue[1] = 180;
gfc->CurrentStep = 4;
gfc->masking_lower = 1;
gfp->ATHtype = -1;
gfp->useTemporal = 1;
gfc->mf_samples_to_encode = ENCDELAY + 288;
gfc->mf_size = ENCDELAY - MDCTDELAY;
#ifdef KLEMM_44
gfc->last_ampl = gfc->ampl = +1.0;
#endif
return 0;
}
void
lame_bitrate_hist(const lame_global_flags * const gfp, int bitrate_count[14])
{
const lame_internal_flags *gfc;
int     i;
if (NULL == bitrate_count)
return;
if (NULL == gfp)
return;
gfc = gfp->internal_flags;
if (NULL == gfc)
return;
for (i = 0; i < 14; i++)
bitrate_count[i] = gfc->bitrate_stereoMode_Hist[i + 1][4];
}
void
lame_bitrate_kbps(const lame_global_flags * const gfp, int bitrate_kbps[14])
{
const lame_internal_flags *gfc;
int     i;
if (NULL == bitrate_kbps)
return;
if (NULL == gfp)
return;
gfc = gfp->internal_flags;
if (NULL == gfc)
return;
for (i = 0; i < 14; i++)
bitrate_kbps[i] = bitrate_table[gfp->version][i + 1];
}
void
lame_stereo_mode_hist(const lame_global_flags * const gfp, int stmode_count[4])
{
const lame_internal_flags *gfc;
int     i;
if (NULL == stmode_count)
return;
if (NULL == gfp)
return;
gfc = gfp->internal_flags;
if (NULL == gfc)
return;
for (i = 0; i < 4; i++) {
int     j, sum = 0;
for (j = 0; j < 14; j++)
sum += gfc->bitrate_stereoMode_Hist[j + 1][i];
stmode_count[i] = sum;
}
}
void
lame_bitrate_stereo_mode_hist(const lame_global_flags * const gfp,
int bitrate_stmode_count[14][4])
{
const lame_internal_flags *gfc;
int     i;
int     j;
if (NULL == bitrate_stmode_count)
return;
if (NULL == gfp)
return;
gfc = gfp->internal_flags;
if (NULL == gfc)
return;
for (j = 0; j < 14; j++)
for (i = 0; i < 4; i++)
bitrate_stmode_count[j][i] = gfc->bitrate_stereoMode_Hist[j + 1][i];
}