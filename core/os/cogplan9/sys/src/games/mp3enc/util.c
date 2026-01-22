#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#define PRECOMPUTE
#include "util.h"
#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#if defined(__FreeBSD__) && !defined(__alpha__)
# include <machine/floatingpoint.h>
#endif
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
void freegfc ( lame_internal_flags* const gfc )
{
int i;
#ifdef KLEMM_44
if (gfc->resample_in != NULL) {
resample_close(gfc->resample_in);
gfc->resample_in = NULL;
}
free(gfc->mfbuf[0]);
free(gfc->mfbuf[1]);
#endif
for ( i = 0 ; i <= 2*BPC; i++ )
if ( gfc->blackfilt[i] != NULL ) {
free ( gfc->blackfilt[i] );
gfc->blackfilt[i] = NULL;
}
if ( gfc->inbuf_old[0] ) {
free ( gfc->inbuf_old[0] );
gfc->inbuf_old[0] = NULL;
}
if ( gfc->inbuf_old[1] ) {
free ( gfc->inbuf_old[1] );
gfc->inbuf_old[1] = NULL;
}
if ( gfc->bs.buf != NULL ) {
free ( gfc->bs.buf );
gfc->bs.buf = NULL;
}
if ( gfc->VBR_seek_table.bag ) {
free ( gfc->VBR_seek_table.bag );
}
if ( gfc->ATH ) {
free ( gfc->ATH );
}
free ( gfc );
}
FLOAT8 ATHformula_old(FLOAT8 f)
{
FLOAT8 ath;
f /= 1000;
f = Max(0.01, f);
f = Min(18.0, f);
ath = 3.640 * pow(f,-0.8)
- 6.500 * exp(-0.6*pow(f-3.3,2.0))
+ 0.001 * pow(f,4.0);
return ath;
}
FLOAT8 ATHformula_GB(FLOAT8 f)
{
FLOAT8 ath;
f /= 1000;
f = Max(0.01, f);
f = Min(18.0, f);
ath = 3.640 * pow(f,-0.8)
- 6.800 * exp(-0.6*pow(f-3.4,2.0))
+ 6.000 * exp(-0.15*pow(f-8.7,2.0))
+ 0.6* 0.001 * pow(f,4.0);
return ath;
}
FLOAT8 ATHformula_GBtweak(FLOAT8 f)
{
FLOAT8 ath;
f /= 1000;
f = Max(0.01, f);
f = Min(18.0, f);
ath = 3.640 * pow(f,-0.8)
- 6.800 * exp(-0.6*pow(f-3.4,2.0))
+ 6.000 * exp(-0.15*pow(f-8.7,2.0))
+ 0.57* 0.001 * pow(f,4.0)
+ 6;
return ath;
}
FLOAT8 ATHformula_Frank( FLOAT8 freq )
{
static FLOAT tab [] = {
96.69, 96.69, 96.26, 95.12,
93.53, 91.13, 88.82, 86.76,
84.69, 82.43, 79.97, 77.48,
74.92, 72.39, 70.00, 67.62,
65.29, 63.02, 60.84, 59.00,
57.17, 55.34, 53.51, 51.67,
50.04, 48.12, 46.38, 44.66,
43.10, 41.73, 40.50, 39.22,
37.23, 35.77, 34.51, 32.81,
31.32, 30.36, 29.02, 27.60,
26.58, 25.91, 24.41, 23.01,
22.12, 21.25, 20.18, 19.00,
17.70, 16.82, 15.94, 15.12,
14.30, 13.41, 12.60, 11.98,
11.36, 10.57, 9.98, 9.43,
8.87, 8.46, 7.44, 7.12,
6.93, 6.68, 6.37, 6.06,
5.80, 5.55, 5.29, 5.02,
4.75, 4.48, 4.22, 3.98,
3.75, 3.51, 3.27, 3.22,
3.12, 3.01, 2.91, 2.68,
2.46, 2.15, 1.82, 1.46,
1.07, 0.61, 0.13, -0.35,
-0.96, -1.56, -1.79, -2.35,
-2.95, -3.50, -4.01, -4.21,
-4.46, -4.99, -5.32, -5.35,
-5.13, -4.76, -4.31, -3.13,
-1.79, 0.08, 2.03, 4.03,
5.80, 7.36, 8.81, 10.22,
11.54, 12.51, 13.48, 14.21,
14.79, 13.99, 12.85, 11.93,
12.87, 15.19, 19.14, 23.69,
33.52, 48.65, 59.42, 61.77,
63.85, 66.04, 68.33, 70.09,
70.66, 71.27, 71.91, 72.60,
};
FLOAT8 freq_log;
unsigned index;
if ( freq < 10. ) freq = 10.;
if ( freq > 29853. ) freq = 29853.;
freq_log = 40. * log10 (0.1 * freq);
index = (unsigned) freq_log;
assert ( index < sizeof(tab)/sizeof(*tab) );
return tab [index] * (1 + index - freq_log) + tab [index+1] * (freq_log - index);
}
FLOAT8 ATHformula(FLOAT8 f,lame_global_flags *gfp)
{
switch(gfp->ATHtype)
{
case 0:
return ATHformula_old(f);
case 1:
return ATHformula_Frank(f);
case 2:
return ATHformula_GB(f);
case 3:
return ATHformula_GBtweak(f);
}
return ATHformula_Frank(f);
}
FLOAT8 freq2bark(FLOAT8 freq)
{
if (freq<0) freq=0;
freq = freq * 0.001;
return 13.0*atan(.76*freq) + 3.5*atan(freq*freq/(7.5*7.5));
}
FLOAT8 freq2cbw(FLOAT8 freq)
{
freq = freq * 0.001;
return 25+75*pow(1+1.4*(freq*freq),0.69);
}
void getframebits(lame_global_flags *gfp, int *bitsPerFrame, int *mean_bits)
{
lame_internal_flags *gfc=gfp->internal_flags;
int whole_SpF;
int bit_rate;
if (gfc->bitrate_index)
bit_rate = bitrate_table[gfp->version][gfc->bitrate_index];
else
bit_rate = gfp->brate;
assert ( bit_rate <= 550 );
whole_SpF = (gfp->version+1)*72000*bit_rate / gfp->out_samplerate;
*bitsPerFrame = 8 * (whole_SpF + gfc->padding);
*mean_bits = (*bitsPerFrame - 8*gfc->sideinfo_len) / gfc->mode_gr;
}
#define ABS(A) (((A)>0) ? (A) : -(A))
int FindNearestBitrate(
int bRate,
int version,
int samplerate)
{
int bitrate = 0;
int i;
for ( i = 1; i <= 14; i++ )
if ( ABS (bitrate_table[version][i] - bRate) < ABS (bitrate - bRate) )
bitrate = bitrate_table [version] [i];
return bitrate;
}
int map2MP3Frequency(int freq)
{
if (freq <= 8000) return 8000;
if (freq <= 11025) return 11025;
if (freq <= 12000) return 12000;
if (freq <= 16000) return 16000;
if (freq <= 22050) return 22050;
if (freq <= 24000) return 24000;
if (freq <= 32000) return 32000;
if (freq <= 44100) return 44100;
return 48000;
}
int BitrateIndex(
int bRate,
int version,
int samplerate)
{
int i;
for ( i = 0; i <= 14; i++)
if ( bitrate_table [version] [i] == bRate )
return i;
return -1;
}
int SmpFrqIndex ( int sample_freq, int* const version )
{
switch ( sample_freq ) {
case 44100: *version = 1; return 0;
case 48000: *version = 1; return 1;
case 32000: *version = 1; return 2;
case 22050: *version = 0; return 0;
case 24000: *version = 0; return 1;
case 16000: *version = 0; return 2;
case 11025: *version = 0; return 0;
case 12000: *version = 0; return 1;
case 8000: *version = 0; return 2;
default: *version = 0; return -1;
}
}
void freorder(int scalefac_band[],FLOAT8 ix_orig[576]) {
int i,sfb, window, j=0;
FLOAT8 ix[576];
for (sfb = 0; sfb < SBMAX_s; sfb++) {
int start = scalefac_band[sfb];
int end = scalefac_band[sfb + 1];
for (window = 0; window < 3; window++) {
for (i = start; i < end; ++i) {
ix[j++] = ix_orig[3*i+window];
}
}
}
memcpy(ix_orig,ix,576*sizeof(FLOAT8));
}
#ifndef KLEMM_44
inline static FLOAT8 blackman(FLOAT8 x,FLOAT8 fcn,int l)
{
FLOAT8 bkwn,x2;
FLOAT8 wcn = (PI * fcn);
x /= l;
if (x<0) x=0;
if (x>1) x=1;
x2 = x - .5;
bkwn = 0.42 - 0.5*cos(2*x*PI) + 0.08*cos(4*x*PI);
if (fabs(x2)<1e-9) return wcn/PI;
else
return ( bkwn*sin(l*wcn*x2) / (PI*l*x2) );
}
int gcd ( int i, int j )
{
return j ? gcd(j, i % j) : i;
}
void fill_buffer(lame_global_flags *gfp,
sample_t *mfbuf[2],
sample_t *in_buffer[2],
int nsamples, int *n_in, int *n_out)
{
lame_internal_flags *gfc = gfp->internal_flags;
int ch,i;
if (gfc->resample_ratio != 1.0) {
for (ch = 0; ch < gfc->channels_out; ch++) {
*n_out =
fill_buffer_resample(gfp, &mfbuf[ch][gfc->mf_size],
gfp->framesize, in_buffer[ch],
nsamples, n_in, ch);
}
}
else {
*n_out = Min(gfp->framesize, nsamples);
*n_in = *n_out;
for (i = 0; i < *n_out; ++i) {
mfbuf[0][gfc->mf_size + i] = in_buffer[0][i];
if (gfc->channels_out == 2)
mfbuf[1][gfc->mf_size + i] = in_buffer[1][i];
}
}
if (gfp->scale != 0) {
for (i=0 ; i<*n_out; ++i) {
mfbuf[0][gfc->mf_size+i] *= gfp->scale;
if (gfc->channels_out == 2)
mfbuf[1][gfc->mf_size + i] *= gfp->scale;
}
}
}
int fill_buffer_resample(
lame_global_flags *gfp,
sample_t *outbuf,
int desired_len,
sample_t *inbuf,
int len,
int *num_used,
int ch)
{
lame_internal_flags *gfc=gfp->internal_flags;
int BLACKSIZE;
FLOAT8 offset,xvalue;
int i,j=0,k;
int filter_l;
FLOAT8 fcn,intratio;
FLOAT *inbuf_old;
int bpc;
bpc = gfp->out_samplerate/gcd(gfp->out_samplerate,gfp->in_samplerate);
if (bpc>BPC) bpc = BPC;
intratio=( fabs(gfc->resample_ratio - floor(.5+gfc->resample_ratio)) < .0001 );
fcn = 1.00/gfc->resample_ratio;
if (fcn>1.00) fcn=1.00;
filter_l = gfp->quality < 7 ? 31 : 7;
filter_l = 31;
if (0==filter_l % 2 ) --filter_l;
filter_l += intratio;
BLACKSIZE = filter_l+1;
if ( gfc->fill_buffer_resample_init == 0 ) {
gfc->inbuf_old[0]=calloc(BLACKSIZE,sizeof(gfc->inbuf_old[0][0]));
gfc->inbuf_old[1]=calloc(BLACKSIZE,sizeof(gfc->inbuf_old[0][0]));
for (i=0; i<=2*bpc; ++i)
gfc->blackfilt[i]=calloc(BLACKSIZE,sizeof(gfc->blackfilt[0][0]));
gfc->itime[0]=0;
gfc->itime[1]=0;
for ( j = 0; j <= 2*bpc; j++ ) {
FLOAT8 sum = 0.;
offset = (j-bpc) / (2.*bpc);
for ( i = 0; i <= filter_l; i++ )
sum +=
gfc->blackfilt[j][i] = blackman(i-offset,fcn,filter_l);
for ( i = 0; i <= filter_l; i++ )
gfc->blackfilt[j][i] /= sum;
}
gfc->fill_buffer_resample_init = 1;
}
inbuf_old=gfc->inbuf_old[ch];
for (k=0;k<desired_len;k++) {
FLOAT time0;
int joff;
time0 = k*gfc->resample_ratio;
j = floor( time0 -gfc->itime[ch] );
if ((filter_l + j - filter_l/2) >= len) break;
offset = ( time0 -gfc->itime[ch] - (j + .5*(filter_l%2)));
assert(fabs(offset)<=.500001);
joff = floor((offset*2*bpc) + bpc +.5);
xvalue = 0.;
for (i=0 ; i<=filter_l ; ++i) {
int j2 = i+j-filter_l/2;
int y;
assert(j2<len);
assert(j2+BLACKSIZE >= 0);
y = (j2<0) ? inbuf_old[BLACKSIZE+j2] : inbuf[j2];
#define PRECOMPUTE
#ifdef PRECOMPUTE
xvalue += y*gfc->blackfilt[joff][i];
#else
xvalue += y*blackman(i-offset,fcn,filter_l);
#endif
}
outbuf[k]=xvalue;
}
*num_used = Min(len,filter_l+j-filter_l/2);
gfc->itime[ch] += *num_used - k*gfc->resample_ratio;
if (*num_used >= BLACKSIZE) {
for (i=0;i<BLACKSIZE;i++)
inbuf_old[i]=inbuf[*num_used + i -BLACKSIZE];
}else{
int n_shift = BLACKSIZE-*num_used;
for (i=0; i<n_shift; ++i )
inbuf_old[i] = inbuf_old[i+ *num_used];
for (j=0; i<BLACKSIZE; ++i, ++j )
inbuf_old[i] = inbuf[j];
assert(j==*num_used);
}
return k;
}
#endif
void lame_debugf (const lame_internal_flags *gfc, const char* format, ... )
{
va_list args;
va_start ( args, format );
if ( gfc->report.debugf != NULL ) {
gfc->report.debugf( format, args );
} else {
(void) vfprintf ( stderr, format, args );
fflush ( stderr );
}
va_end ( args );
}
void lame_msgf (const lame_internal_flags *gfc, const char* format, ... )
{
va_list args;
va_start ( args, format );
if ( gfc->report.msgf != NULL ) {
gfc->report.msgf( format, args );
} else {
(void) vfprintf ( stderr, format, args );
fflush ( stderr );
}
va_end ( args );
}
void lame_errorf (const lame_internal_flags *gfc, const char* format, ... )
{
va_list args;
va_start ( args, format );
if ( gfc->report.errorf != NULL ) {
gfc->report.errorf( format, args );
} else {
(void) vfprintf ( stderr, format, args );
fflush ( stderr );
}
va_end ( args );
}
int has_i387 ( void )
{
#ifdef HAVE_NASM
return 1;
#else
return 0;
#endif
}
int has_MMX ( void )
{
#ifdef HAVE_NASM
extern int has_MMX_nasm ( void );
return has_MMX_nasm ();
#else
return 0;
#endif
}
int has_3DNow ( void )
{
#ifdef HAVE_NASM
extern int has_3DNow_nasm ( void );
return has_3DNow_nasm ();
#else
return 0;
#endif
}
int has_SIMD ( void )
{
#ifdef HAVE_NASM
extern int has_SIMD_nasm ( void );
return has_SIMD_nasm ();
#else
return 0;
#endif
}
int has_SIMD2 ( void )
{
#ifdef HAVE_NASM
extern int has_SIMD2_nasm ( void );
return has_SIMD2_nasm ();
#else
return 0;
#endif
}
void updateStats( lame_internal_flags * const gfc )
{
assert ( gfc->bitrate_index < 16u );
assert ( gfc->mode_ext < 4u );
gfc->bitrate_stereoMode_Hist [gfc->bitrate_index] [4] ++;
if (gfc->channels_out == 2)
gfc->bitrate_stereoMode_Hist [gfc->bitrate_index] [gfc->mode_ext]++;
}
int select_kth_int(int a[], int N, int k)
{
int i, j, l, r, v, w;
l = 0;
r = N-1;
while (r > l) {
v = a[r];
i = l-1;
j = r;
for (;;) {
while (a[++i] < v) ;
while (a[--j] > v) ;
if (i >= j)
break;
w = a[i];
a[i] = a[j];
a[j] = w;
}
w = a[i];
a[i] = a[r];
a[r] = w;
if (i >= k)
r = i-1;
if (i <= k)
l = i+1;
}
return a[k];
}
void disable_FPE(void) {
#if defined(__FreeBSD__) && !defined(__alpha__)
{
fp_except_t mask;
mask = fpgetmask();
fpsetmask(mask & ~(FP_X_INV | FP_X_DZ));
}
#endif
#if defined(__riscos__) && !defined(ABORTFP)
DisableFPETraps(_FPE_IVO | _FPE_DVZ | _FPE_OFL);
#endif
#if defined(ABORTFP)
#if defined(_MSC_VER)
{
#include <float.h>
unsigned int mask;
mask = _controlfp(0, 0);
mask &= ~(_EM_OVERFLOW | _EM_UNDERFLOW | _EM_ZERODIVIDE | _EM_INVALID);
mask = _controlfp(mask, _MCW_EM);
}
#elif defined(__CYGWIN__)
# define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
# define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))
# define _EM_INEXACT 0x00000020
# define _EM_UNDERFLOW 0x00000010
# define _EM_OVERFLOW 0x00000008
# define _EM_ZERODIVIDE 0x00000004
# define _EM_INVALID 0x00000001
{
unsigned int mask;
_FPU_GETCW(mask);
mask &= ~(_EM_OVERFLOW | _EM_ZERODIVIDE | _EM_INVALID);
_FPU_SETCW(mask);
}
# elif defined(__linux__)
{
# include <fpu_control.h>
# ifndef _FPU_GETCW
# define _FPU_GETCW(cw) __asm__ ("fnstcw %0" : "=m" (*&cw))
# endif
# ifndef _FPU_SETCW
# define _FPU_SETCW(cw) __asm__ ("fldcw %0" : : "m" (*&cw))
# endif
unsigned int mask;
_FPU_GETCW(mask);
mask &= ~(_FPU_MASK_IM | _FPU_MASK_ZM | _FPU_MASK_OM);
_FPU_SETCW(mask);
}
#endif
#endif
}