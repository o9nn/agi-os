#ifndef LAME_UTIL_H
#define LAME_UTIL_H
#ifdef HUGE_VAL
#ifndef fabs
#define fabs(x) ((double)((x) < 0? -(x): (x)))
#endif
#endif
#include "machine.h"
#include "encoder.h"
#include "lame.h"
#include "lame-analysis.h"
#include "id3tag.h"
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif
#ifdef UINT_MAX
# define MAX_U_32_NUM UINT_MAX
#else
# define MAX_U_32_NUM 0xFFFFFFFF
#endif
#ifndef PI
# ifdef M_PI
# define PI M_PI
# else
# define PI 3.14159265358979323846
# endif
#endif
#ifdef M_LN2
# define LOG2 M_LN2
#else
# define LOG2 0.69314718055994530942
#endif
#ifdef M_LN10
# define LOG10 M_LN10
#else
# define LOG10 2.30258509299404568402
#endif
#ifdef M_SQRT2
# define SQRT2 M_SQRT2
#else
# define SQRT2 1.41421356237309504880
#endif
#define HAN_SIZE 512
#define CRC16_POLYNOMIAL 0x8005
#define MAX_BITS 4095
#define BUFFER_SIZE LAME_MAXMP3BUFFER
#define Min(A, B) ((A) < (B) ? (A) : (B))
#define Max(A, B) ((A) > (B) ? (A) : (B))
typedef struct bit_stream_struc {
unsigned char *buf;
int buf_size;
int totbit;
int buf_byte_idx;
int buf_bit_idx;
} Bit_stream_struc;
#include "l3side.h"
typedef struct {
int use;
int safejoint;
FLOAT last_en_subshort[4][9];
FLOAT last_attack_intensity[4][9];
FLOAT last_thm[4][SBMAX_s][3];
int last_attacks[4][3];
FLOAT pe_l[4],pe_s[4];
FLOAT pefirbuf[19];
FLOAT bass,alto,treble;
} nsPsy_t;
typedef struct
{
int sum;
int seen;
int want;
int pos;
int size;
int *bag;
} VBR_seek_info_t;
typedef struct
{
int use_adjust;
FLOAT8 adjust;
FLOAT8 adjust_limit;
FLOAT8 decay;
FLOAT8 l[SBMAX_l];
FLOAT8 s[SBMAX_s];
FLOAT8 cb[CBANDS];
} ATH_t;
typedef enum {
coding_MPEG_Layer_1 = 1,
coding_MPEG_Layer_2 = 2,
coding_MPEG_Layer_3 = 3,
coding_MPEG_AAC = 4,
coding_Ogg_Vorbis = 5,
coding_MPEG_plus = 6
} coding_t;
#define MAX_CHANNELS 2
typedef struct {
unsigned long Class_ID;
long double sample_freq_in;
long double sample_freq_out;
float lowpass_freq;
int scale_in;
int scale_out;
int taps;
sample_t** fir;
void* firfree;
unsigned char* src_step;
sample_t* in_old [MAX_CHANNELS];
unsigned fir_stepper [MAX_CHANNELS];
int inp_stepper [MAX_CHANNELS];
} resample_t;
typedef struct {
#define LAME_ID 0xFFF88E3B
unsigned long Class_ID;
struct {
void (*msgf) (const char *format, va_list ap);
void (*debugf)(const char *format, va_list ap);
void (*errorf)(const char *format, va_list ap);
} report;
int lame_encode_frame_init;
int iteration_init_init;
int fill_buffer_resample_init;
int psymodel_init;
int padding;
int mode_gr;
int channels_in;
int channels_out;
resample_t* resample_in;
resample_t* resample_out;
long double samplefreq_in;
long double samplefreq_out;
#ifndef MFSIZE
# define MFSIZE ( 3*1152 + ENCDELAY - MDCTDELAY )
#endif
#ifdef KLEMM_44
sample_t* mfbuf [MAX_CHANNELS];
#else
sample_t mfbuf [2] [MFSIZE];
#endif
size_t frame_size;
lame_global_flags* gfp;
coding_t coding;
unsigned long frame_count;
int mf_samples_to_encode;
int mf_size;
float ampl;
float last_ampl;
int VBR_min_bitrate;
int VBR_max_bitrate;
float resample_ratio;
int bitrate_index;
int samplerate_index;
int mode_ext;
float lowpass1,lowpass2;
float highpass1,highpass2;
int lowpass_band;
int highpass_band;
int lowpass_start_band;
int lowpass_end_band;
int highpass_start_band;
int highpass_end_band;
int filter_type;
int quantization;
int noise_shaping;
int noise_shaping_amp;
int psymodel;
int noise_shaping_stop;
int use_best_huffman;
Bit_stream_struc bs;
III_side_info_t l3_side;
FLOAT8 ms_ratio[2];
int frac_SpF;
int slot_lag;
struct id3tag_spec tag_spec;
int OldValue[2];
int CurrentStep;
FLOAT8 decay;
FLOAT8 masking_lower;
char bv_scf[576];
int sfb21_extra;
int is_mpeg1;
#ifndef KLEMM_44
#define BPC 320
sample_t *inbuf_old [2];
sample_t *blackfilt [2*BPC+1];
FLOAT8 itime[2];
#endif
int sideinfo_len;
FLOAT8 sb_sample[2][2][18][SBLIMIT];
FLOAT8 amp_lowpass[32];
FLOAT8 amp_highpass[32];
#define MAX_HEADER_BUF 256
#define MAX_HEADER_LEN 40
struct {
int write_timing;
int ptr;
char buf[MAX_HEADER_LEN];
} header[MAX_HEADER_BUF];
int h_ptr;
int w_ptr;
int ancillary_flag;
int ResvSize;
int ResvMax;
scalefac_struct scalefac_band;
FLOAT8 minval[CBANDS];
FLOAT8 nb_1[4][CBANDS], nb_2[4][CBANDS];
FLOAT8 s3_s[CBANDS][CBANDS];
FLOAT8 s3_l[CBANDS][CBANDS];
III_psy_xmin thm[4];
III_psy_xmin en[4];
int cw_upper_index;
int cw_lower_index;
FLOAT ax_sav[4][2][HBLKSIZE];
FLOAT bx_sav[4][2][HBLKSIZE];
FLOAT rx_sav[4][2][HBLKSIZE];
FLOAT cw[HBLKSIZE];
FLOAT wsamp_L[2][BLKSIZE];
FLOAT energy[HBLKSIZE];
FLOAT wsamp_S[2][3][BLKSIZE_s];
FLOAT energy_s[3][HBLKSIZE_s];
FLOAT tot_ener[4];
FLOAT window[BLKSIZE], window_s[BLKSIZE_s/2];
FLOAT8 w1_l[SBMAX_l], w2_l[SBMAX_l];
FLOAT8 w1_s[SBMAX_s], w2_s[SBMAX_s];
FLOAT8 mld_l[SBMAX_l],mld_s[SBMAX_s];
int bu_l[SBMAX_l],bo_l[SBMAX_l] ;
int bu_s[SBMAX_s],bo_s[SBMAX_s] ;
int npart_l,npart_s;
int npart_l_orig,npart_s_orig;
int s3ind[CBANDS][2];
int s3ind_s[CBANDS][2];
FLOAT8 SNR_s[CBANDS];
int numlines_s[CBANDS];
int numlines_l[CBANDS];
FLOAT energy_save[4][HBLKSIZE];
FLOAT8 pe_save[4];
FLOAT8 ers_save[4];
int bitrate_stereoMode_Hist [16] [4+1];
FLOAT8 pe[4];
FLOAT8 ms_ratio_s_old,ms_ratio_l_old;
FLOAT8 ms_ener_ratio_old;
int blocktype_old[2];
plotting_data *pinfo;
struct {
unsigned int i387 : 1;
unsigned int MMX : 1;
unsigned int AMD_3DNow : 1;
unsigned int SIMD : 1;
unsigned int SIMD2 : 1;
} CPU_features;
int (*choose_table)(const int *ix, const int *end, int *s);
nsPsy_t nsPsy;
unsigned crcvalue;
VBR_seek_info_t VBR_seek_table;
ATH_t *ATH;
} lame_internal_flags;
void freegfc(lame_internal_flags *gfc);
extern int BitrateIndex(int, int,int);
extern int FindNearestBitrate(int,int,int);
extern int map2MP3Frequency(int freq);
extern int SmpFrqIndex(int, int*);
extern FLOAT8 ATHformula(FLOAT8 f,lame_global_flags *gfp);
extern FLOAT8 freq2bark(FLOAT8 freq);
extern FLOAT8 freq2cbw(FLOAT8 freq);
extern void freorder(int scalefac_band[],FLOAT8 ix_orig[576]);
void disable_FPE(void);
extern void
getframebits(lame_global_flags *gfp, int *bitsPerFrame, int *mean_bits);
void fill_buffer(lame_global_flags *gfp,
sample_t *mfbuf[2],
sample_t *in_buffer[2],
int nsamples, int *n_in, int *n_out);
int fill_buffer_resample (
lame_global_flags *gfp,
sample_t* outbuf,
int desired_len,
sample_t* inbuf,
int len,
int* num_used,
int channels );
extern int has_i387 ( void );
extern int has_MMX ( void );
extern int has_3DNow ( void );
extern int has_SIMD ( void );
extern int has_SIMD2 ( void );
extern void updateStats (lame_internal_flags *gfc);
extern void lame_errorf(const lame_internal_flags *gfc, const char *, ...);
extern void lame_debugf(const lame_internal_flags *gfc, const char *, ...);
extern void lame_msgf (const lame_internal_flags *gfc, const char *, ...);
#define DEBUGF lame_debugf
#define ERRORF lame_errorf
#define MSGF lame_msgf
int select_kth_int(int b[], int N, int k);
#endif