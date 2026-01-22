#ifndef LAME_BITSTREAM_H
#define LAME_BITSTREAM_H
#include "util.h"
int format_bitstream(lame_global_flags *gfp, int i,
int              l3_enc[2][2][576],
III_scalefac_t   scalefac[2][2] );
void flush_bitstream(lame_global_flags *gfp);
void add_dummy_byte ( lame_global_flags* const gfp, unsigned char val );
int  copy_buffer(unsigned char *buffer,int buffer_size,Bit_stream_struc *bs);
void init_bit_stream_w(lame_internal_flags *gfc);
void main_CRC_init (void);
#endif