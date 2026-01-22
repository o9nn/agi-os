#include <limits.h>
#include "lame.h"
#include "util.h"
#define ENCDELAY      576
#define MDCTDELAY     48
#define BLKSIZE       1024
#define HBLKSIZE      (BLKSIZE/2 + 1)
#define BLKSIZE_s     256
#define HBLKSIZE_s    (BLKSIZE_s/2 + 1)
#define MAX_TABLES    1002
#define TAPS          32
#define WINDOW_SIZE   15.5
#define WINDOW        hanning
#define inline        __inline
#define MIN(a,b)      ((a) < (b) ? (a) : (b))
#define MAX(a,b)      ((a) > (b) ? (a) : (b))
#ifndef M_PIl
# define M_PIl          3.1415926535897932384626433832795029L
#endif
#define SIN             sin
#define COS             cos
#define RESAMPLE_ID     0x52455341LU
#define PSYCHO_ID       0x50535943LU
#define BITSTREAM_ID    0x42495453LU
typedef float         float32_t;
typedef double        float64_t;
typedef long double   float80_t;
typedef long double   float_t;
typedef long double   double_t;
typedef long double   longdouble_t;
typedef float_t (*scalar_t)  ( const sample_t* p, const sample_t* q );
typedef float_t (*scalarn_t) ( const sample_t* p, const sample_t* q, size_t len );
#define OUT             const
#define INOUT
#define IN
#define OUTTR
#ifndef __LOC__
# define _STR2(x)      #x
# define _STR1(x)      _STR2(x)
# define __LOC__       __FILE__ "(" _STR1(__LINE__) ") : warning: "
#endif
#if CHAR_BIT != 8
# pragma message ( __LOC__ "Machines with CHAR_BIT != 8 not yet supported" )
# pragma error
#endif
#define  LAME_INTERLEAVED       0x10000000
#define  LAME_CHAINED           0x20000000
#define  LAME_INDIRECT          0x30000000
#define  LAME_NATIVE_ENDIAN     0x00000000
#define  LAME_OPPOSITE_ENDIAN   0x01000000
#define  LAME_LITTLE_ENDIAN     0x02000000
#define  LAME_BIG_ENDIAN        0x03000000
#define  LAME_SILENCE           0x00010000
#define  LAME_UINT8             0x00020000
#define  LAME_INT8              0x00030000
#define  LAME_UINT16            0x00040000
#define  LAME_INT16             0x00050000
#define  LAME_UINT24            0x00060000
#define  LAME_INT24             0x00070000
#define  LAME_UINT32            0x00080000
#define  LAME_INT32             0x00090000
#define  LAME_FLOAT32           0x00140000
#define  LAME_FLOAT64           0x00180000
#define  LAME_FLOAT80_ALIGN2    0x001A0000
#define  LAME_FLOAT80_ALIGN4    0x001C0000
#define  LAME_FLOAT80_ALIGN8    0x00200000
#define  LAME_SHORT             0x00210000
#define  LAME_INT               0x00220000
#define  LAME_LONG              0x00230000
#define  LAME_FLOAT             0x00240000
#define  LAME_DOUBLE            0x00250000
#define  LAME_LONGDOUBLE        0x00260000
#define  LAME_ALAW              0x00310000
#define  LAME_ULAW              0x00320000
#define LAME_MONO               0x00000001
#define LAME_STEREO             0x00000002
#define LAME_STEREO_2_CHANNELS  0x00000002
#define LAME_STEREO_3_CHANNELS  0x00000003
#define LAME_STEREO_4_CHANNELS  0x00000004
#define LAME_STEREO_5_CHANNELS  0x00000005
#define LAME_STEREO_6_CHANNELS  0x00000006
#define LAME_STEREO_7_CHANNELS  0x00000007
#define LAME_STEREO_65535_CHANNELS\
0x0000FFFF
extern scalar_t   scalar4;
extern scalar_t   scalar8;
extern scalar_t   scalar12;
extern scalar_t   scalar16;
extern scalar_t   scalar20;
extern scalar_t   scalar24;
extern scalar_t   scalar32;
extern scalarn_t  scalar4n;
extern scalarn_t  scalar1n;
float_t  scalar04_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar08_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar12_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar16_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar20_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar24_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar32_float32_i387  ( const float32_t* p, const float32_t* q );
float_t  scalar4n_float32_i387  ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar1n_float32_i387  ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar04_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar08_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar12_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar16_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar20_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar24_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar32_float32_3DNow ( const float32_t* p, const float32_t* q );
float_t  scalar4n_float32_3DNow ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar1n_float32_3DNow ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar04_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar08_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar12_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar16_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar20_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar24_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar32_float32_SIMD  ( const float32_t* p, const float32_t* q );
float_t  scalar4n_float32_SIMD  ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar1n_float32_SIMD  ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar04_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar08_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar12_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar16_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar20_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar24_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar32_float32       ( const float32_t* p, const float32_t* q );
float_t  scalar4n_float32       ( const float32_t* p, const float32_t* q, const size_t len );
float_t  scalar1n_float32       ( const float32_t* p, const float32_t* q, const size_t len );
resample_t*  resample_open (
OUT long double  sampfreq_in,
OUT long double  sampfreq_out,
OUT double       lowpass_freq,
OUT int          quality );
int  resample_buffer (
INOUT resample_t *const   r,
IN    sample_t   *const   out,
INOUT size_t     *const   out_req_len,
OUT   sample_t   *const   in,
INOUT size_t     *const   in_avail_len,
OUT   size_t              channel );
int  resample_close ( INOUT resample_t* const r );
void         init_scalar_functions   ( OUT lame_t* const lame );
long double  unround_samplefrequency ( OUT long double freq );
#if 0
int  lame_encode_mp3_frame (
INOUT lame_global_flags*,
OUTTR sample_t * inbuf_l,
OUTTR sample_t * inbuf_r,
IN    uint8_t  * mp3buf,
OUT   size_t     mp3buf_size );
#endif
int  lame_encode_ogg_frame (
INOUT lame_global_flags*,
OUT   sample_t * inbuf_l,
OUT   sample_t * inbuf_r,
IN    uint8_t  * mp3buf,
OUT   size_t     mp3buf_size );