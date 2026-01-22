#ifndef LAME_LAME_H
#define LAME_LAME_H
#include <stdio.h>
#include <stdarg.h>
#if defined(__cplusplus)
extern "C" {
#endif
#if defined(WIN32)
#undef CDECL
#define CDECL _cdecl
#else
#define CDECL
#endif
typedef enum vbr_mode_e {
vbr_off=0,
vbr_mt,
vbr_rh,
vbr_abr,
vbr_mtrh,
vbr_default=vbr_rh
} vbr_mode;
typedef enum MPEG_mode_e {
STEREO=0,
JOINT_STEREO,
DUAL_CHANNEL,
MONO,
NOT_SET,
MAX_INDICATOR
} MPEG_mode;
typedef struct {
unsigned long num_samples;
int num_channels;
int in_samplerate;
int out_samplerate;
float scale;
int analysis;
int bWriteVbrTag;
int disable_waveheader;
int decode_only;
int ogg;
int quality;
MPEG_mode mode;
int mode_fixed;
int mode_automs;
int force_ms;
int free_format;
int brate;
float compression_ratio;
int copyright;
int original;
int error_protection;
int padding_type;
int extension;
int strict_ISO;
int disable_reservoir;
int experimentalX;
int experimentalY;
int experimentalZ;
int exp_nspsytune;
vbr_mode VBR;
int VBR_q;
int VBR_mean_bitrate_kbps;
int VBR_min_bitrate_kbps;
int VBR_max_bitrate_kbps;
int VBR_hard_min;
int lowpassfreq;
int highpassfreq;
int lowpasswidth;
int highpasswidth;
int ATHonly;
int ATHshort;
int noATH;
int ATHtype;
float ATHlower;
int cwlimit;
int allow_diff_short;
int useTemporal;
int no_short_blocks;
int emphasis;
struct {
void (*msgf) (const char *format, va_list ap);
void (*debugf)(const char *format, va_list ap);
void (*errorf)(const char *format, va_list ap);
} report;
int version;
int encoder_delay;
int framesize;
int frameNum;
int totalframes;
int lame_allocated_gfp;
void *internal_flags;
int nZeroStreamSize;
int TotalFrameSize;
int* pVbrFrames;
int nVbrNumFrames;
int nVbrFrameBufferSize;
} lame_global_flags;
typedef lame_global_flags lame_t;
lame_global_flags * CDECL lame_init(void);
int CDECL lame_init_old(lame_global_flags *);
int CDECL lame_set_num_samples(lame_global_flags *, unsigned long);
unsigned long CDECL lame_get_num_samples(const lame_global_flags *);
int CDECL lame_set_in_samplerate(lame_global_flags *, int);
int CDECL lame_get_in_samplerate(const lame_global_flags *);
int CDECL lame_set_num_channels(lame_global_flags *, int);
int CDECL lame_get_num_channels(const lame_global_flags *);
int CDECL lame_set_scale(lame_global_flags *, float);
float CDECL lame_get_scale(const lame_global_flags *);
int CDECL lame_set_out_samplerate(lame_global_flags *, int);
int CDECL lame_get_out_samplerate(const lame_global_flags *);
int CDECL lame_set_analysis(lame_global_flags *, int);
int CDECL lame_get_analysis(const lame_global_flags *);
int CDECL lame_set_bWriteVbrTag(lame_global_flags *, int);
int CDECL lame_get_bWriteVbrTag(const lame_global_flags *);
int CDECL lame_set_disable_waveheader(lame_global_flags *, int);
int CDECL lame_get_disable_waveheader(const lame_global_flags *);
int CDECL lame_set_decode_only(lame_global_flags *, int);
int CDECL lame_get_decode_only(const lame_global_flags *);
int CDECL lame_set_ogg(lame_global_flags *, int);
int CDECL lame_get_ogg(const lame_global_flags *);
int CDECL lame_set_quality(lame_global_flags *, int);
int CDECL lame_get_quality(const lame_global_flags *);
int CDECL lame_set_mode(lame_global_flags *, MPEG_mode);
MPEG_mode CDECL lame_get_mode(const lame_global_flags *);
int CDECL lame_set_mode_automs(lame_global_flags *, int);
int CDECL lame_get_mode_automs(const lame_global_flags *);
int CDECL lame_set_force_ms(lame_global_flags *, int);
int CDECL lame_get_force_ms(const lame_global_flags *);
int CDECL lame_set_free_format(lame_global_flags *, int);
int CDECL lame_get_free_format(const lame_global_flags *);
int CDECL lame_set_errorf(lame_global_flags *,
void (*func)(const char *, va_list));
int CDECL lame_set_debugf(lame_global_flags *,
void (*func)(const char *, va_list));
int CDECL lame_set_msgf (lame_global_flags *,
void (*func)(const char *, va_list));
int CDECL lame_set_brate(lame_global_flags *, int);
int CDECL lame_get_brate(const lame_global_flags *);
int CDECL lame_set_compression_ratio(lame_global_flags *, float);
float CDECL lame_get_compression_ratio(const lame_global_flags *);
int CDECL lame_set_copyright(lame_global_flags *, int);
int CDECL lame_get_copyright(const lame_global_flags *);
int CDECL lame_set_original(lame_global_flags *, int);
int CDECL lame_get_original(const lame_global_flags *);
int CDECL lame_set_error_protection(lame_global_flags *, int);
int CDECL lame_get_error_protection(const lame_global_flags *);
int CDECL lame_set_padding_type(lame_global_flags *, int);
int CDECL lame_get_padding_type(const lame_global_flags *);
int CDECL lame_set_extension(lame_global_flags *, int);
int CDECL lame_get_extension(const lame_global_flags *);
int CDECL lame_set_strict_ISO(lame_global_flags *, int);
int CDECL lame_get_strict_ISO(const lame_global_flags *);
int CDECL lame_set_disable_reservoir(lame_global_flags *, int);
int CDECL lame_get_disable_reservoir(const lame_global_flags *);
int CDECL lame_set_experimentalX(lame_global_flags *, int);
int CDECL lame_get_experimentalX(const lame_global_flags *);
int CDECL lame_set_experimentalY(lame_global_flags *, int);
int CDECL lame_get_experimentalY(const lame_global_flags *);
int CDECL lame_set_experimentalZ(lame_global_flags *, int);
int CDECL lame_get_experimentalZ(const lame_global_flags *);
int CDECL lame_set_exp_nspsytune(lame_global_flags *, int);
int CDECL lame_get_exp_nspsytune(const lame_global_flags *);
int CDECL lame_set_VBR(lame_global_flags *, vbr_mode);
vbr_mode CDECL lame_get_exp_VBR(const lame_global_flags *);
int CDECL lame_set_VBR_q(lame_global_flags *, int);
int CDECL lame_get_VBR_q(const lame_global_flags *);
int CDECL lame_set_VBR_mean_bitrate_kbps(lame_global_flags *, int);
int CDECL lame_get_VBR_mean_bitrate_kbps(const lame_global_flags *);
int CDECL lame_set_VBR_min_bitrate_kbps(lame_global_flags *, int);
int CDECL lame_get_VBR_min_bitrate_kbps(const lame_global_flags *);
int CDECL lame_set_VBR_max_bitrate_kbps(lame_global_flags *, int);
int CDECL lame_get_VBR_max_bitrate_kbps(const lame_global_flags *);
int CDECL lame_set_VBR_hard_min(lame_global_flags *, int);
int CDECL lame_get_VBR_hard_min(const lame_global_flags *);
int CDECL lame_set_lowpassfreq(lame_global_flags *, int);
int CDECL lame_get_lowpassfreq(const lame_global_flags *);
int CDECL lame_set_lowpasswidth(lame_global_flags *, int);
int CDECL lame_get_lowpasswidth(const lame_global_flags *);
int CDECL lame_set_highpassfreq(lame_global_flags *, int);
int CDECL lame_get_highpassfreq(const lame_global_flags *);
int CDECL lame_set_highpasswidth(lame_global_flags *, int);
int CDECL lame_get_highpasswidth(const lame_global_flags *);
int CDECL lame_set_ATHonly(lame_global_flags *, int);
int CDECL lame_get_ATHonly(const lame_global_flags *);
int CDECL lame_set_ATHshort(lame_global_flags *, int);
int CDECL lame_get_ATHshort(const lame_global_flags *);
int CDECL lame_set_noATH(lame_global_flags *, int);
int CDECL lame_get_noATH(const lame_global_flags *);
int CDECL lame_set_ATHtype(lame_global_flags *, int);
int CDECL lame_get_ATHtype(const lame_global_flags *);
int CDECL lame_set_ATHlower(lame_global_flags *, float);
float CDECL lame_get_ATHlower(const lame_global_flags *);
int CDECL lame_set_cwlimit(lame_global_flags *, int);
int CDECL lame_get_cwlimit(const lame_global_flags *);
int CDECL lame_set_allow_diff_short(lame_global_flags *, int);
int CDECL lame_get_allow_diff_short(const lame_global_flags *);
int CDECL lame_set_useTemporal(lame_global_flags *, int);
int CDECL lame_get_useTemporal(const lame_global_flags *);
int CDECL lame_set_no_short_blocks(lame_global_flags *, int);
int CDECL lame_get_no_short_blocks(const lame_global_flags *);
int CDECL lame_set_emphasis(lame_global_flags *, int);
int CDECL lame_get_emphasis(const lame_global_flags *);
int CDECL lame_get_version(const lame_global_flags *);
int CDECL lame_get_encoder_delay(const lame_global_flags *);
int CDECL lame_get_framesize(const lame_global_flags *);
int CDECL lame_get_frameNum(const lame_global_flags *);
int CDECL lame_get_totalframes(const lame_global_flags *);
int CDECL lame_init_params(lame_global_flags *);
const char* CDECL get_lame_version ( void );
const char* CDECL get_lame_short_version ( void );
const char* CDECL get_psy_version ( void );
const char* CDECL get_mp3x_version ( void );
const char* CDECL get_lame_url ( void );
typedef struct {
int major;
int minor;
int alpha;
int beta;
int psy_major;
int psy_minor;
int psy_alpha;
int psy_beta;
const char *features;
} lame_version_t;
void CDECL get_lame_version_numerical ( lame_version_t *const );
void CDECL lame_print_config(const lame_global_flags* gfp);
int CDECL lame_encode_buffer (
lame_global_flags* gfp,
const short int buffer_l [],
const short int buffer_r [],
const int nsamples,
unsigned char* mp3buf,
const int mp3buf_size );
int CDECL lame_encode_buffer_interleaved(
lame_global_flags* gfp,
short int pcm[],
int num_samples,
unsigned char* mp3buf,
int mp3buf_size );
int CDECL lame_encode_buffer_float(
lame_global_flags* gfp,
const float buffer_l [],
const float buffer_r [],
const int nsamples,
unsigned char* mp3buf,
const int mp3buf_size );
int CDECL lame_encode_buffer_long(
lame_global_flags* gfp,
const long buffer_l [],
const long buffer_r [],
const int nsamples,
unsigned char* mp3buf,
const int mp3buf_size );
int CDECL lame_encode_flush(
lame_global_flags * gfp,
unsigned char* mp3buf,
int size);
#ifdef KLEMM_44
int CDECL lame_encode_pcm (
lame_t* const lame,
octetstream_t* os,
const void* pcm,
size_t len,
uint32_t flags );
int CDECL lame_encode_pcm_flush (
lame_t* const lame,
octetstream_t* const os );
#endif
void CDECL lame_bitrate_hist(
const lame_global_flags *const gfp,
int bitrate_count[14] );
void CDECL lame_bitrate_kbps(
const lame_global_flags *const gfp,
int bitrate_kbps [14] );
void CDECL lame_stereo_mode_hist(
const lame_global_flags *const gfp,
int stereo_mode_count[4] );
void CDECL lame_bitrate_stereo_mode_hist (
const lame_global_flags* gfp,
int bitrate_stmode_count [14] [4] );
void CDECL lame_mp3_tags_fid(lame_global_flags *,FILE* fid);
int CDECL lame_close (lame_global_flags *);
int CDECL lame_encode_finish(
lame_global_flags* gfp,
unsigned char* mp3buf,
int size );
typedef struct {
int header_parsed;
int stereo;
int samplerate;
int bitrate;
int mode;
int mode_ext;
int framesize;
unsigned long nsamp;
int totalframes;
int framenum;
} mp3data_struct;
int CDECL lame_decode_init(void);
int CDECL lame_decode(
unsigned char * mp3buf,
int len,
short pcm_l[],
short pcm_r[] );
int CDECL lame_decode_headers(
unsigned char* mp3buf,
int len,
short pcm_l[],
short pcm_r[],
mp3data_struct* mp3data );
int CDECL lame_decode1(
unsigned char* mp3buf,
int len,
short pcm_l[],
short pcm_r[] );
int CDECL lame_decode1_headers(
unsigned char* mp3buf,
int len,
short pcm_l[],
short pcm_r[],
mp3data_struct* mp3data );
#if 0
#define NUMTOCENTRIES 100
typedef struct
{
int h_id;
int samprate;
int flags;
int frames;
int bytes;
int vbr_scale;
unsigned char toc[NUMTOCENTRIES];
int headersize;
} VBRTAGDATA;
int CDECL GetVbrTag(
VBRTAGDATA* pTagData,
unsigned char* buf);
#endif
extern void id3tag_genre_list(
void (*handler)(int, const char *, void *),
void* cookie);
extern void id3tag_init (lame_global_flags *gfp);
extern void id3tag_add_v2 (lame_global_flags *gfp);
extern void id3tag_v1_only (lame_global_flags *gfp);
extern void id3tag_v2_only (lame_global_flags *gfp);
extern void id3tag_space_v1 (lame_global_flags *gfp);
extern void id3tag_pad_v2 (lame_global_flags *gfp);
extern void id3tag_set_title(
lame_global_flags* gfp,
const char* title );
extern void id3tag_set_artist(
lame_global_flags* gfp,
const char* artist );
extern void id3tag_set_album(
lame_global_flags* gfp,
const char* album );
extern void id3tag_set_year(
lame_global_flags* gfp,
const char* year );
extern void id3tag_set_comment(
lame_global_flags* gfp,
const char* comment );
extern void id3tag_set_track(
lame_global_flags* gfp,
const char* track );
extern int id3tag_set_genre(
lame_global_flags* gfp,
const char* genre );
extern const int bitrate_table [3] [16];
extern const int samplerate_table [3] [ 4];
#define LAME_MAXMP3BUFFER 16384
#if defined(__cplusplus)
}
#endif
#endif