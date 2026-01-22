#ifndef LAME_FFT_H
#define LAME_FFT_H
#include "encoder.h"
void fft_long(lame_internal_flags* gfc, FLOAT x_real[BLKSIZE],
int chn, const sample_t *data[2] );
void fft_short(lame_internal_flags* gfc, FLOAT x_real[3][BLKSIZE_s],
int chn, const sample_t *data[2] );
void init_fft(lame_internal_flags* gfc );
#endif