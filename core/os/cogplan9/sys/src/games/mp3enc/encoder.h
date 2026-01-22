#ifndef LAME_ENCODER_H
#define LAME_ENCODER_H
#define ENCDELAY      576
#define MDCTDELAY     48
#define FFTOFFSET     (224+MDCTDELAY)
#define DECDELAY      528
#define SBLIMIT       32
#define CBANDS        64
#define SBPSY_l       21
#define SBPSY_s       12
#define SBMAX_l       22
#define SBMAX_s       13
#define BLKSIZE       1024
#define HBLKSIZE      (BLKSIZE/2 + 1)
#define BLKSIZE_s     256
#define HBLKSIZE_s    (BLKSIZE_s/2 + 1)
#define NORM_TYPE     0
#define START_TYPE    1
#define SHORT_TYPE    2
#define STOP_TYPE     3
#define MPG_MD_LR_LR  0
#define MPG_MD_LR_I   1
#define MPG_MD_MS_LR  2
#define MPG_MD_MS_I   3
#include "machine.h"
#include "lame.h"
int  lame_encode_mp3_frame (
lame_global_flags*  const gfp,
sample_t*           inbuf_l,
sample_t*           inbuf_r,
unsigned char*      mp3buf,
int                 mp3buf_size );
#endif