#ifndef LAME_BRHIST_H
#define LAME_BRHIST_H
#if defined(_WIN32) && !defined(__CYGWIN__)
# include <windows.h>
#endif
#include "lame.h"
int brhist_init ( const lame_global_flags *gf, const int bitrate_kbps_min, const int bitrate_kbps_max );
void brhist_disp ( const lame_global_flags *gf );
void brhist_disp_total ( const lame_global_flags *gf );
void brhist_jump_back ( void );
typedef struct {
FILE* Console_fp;
FILE* Error_fp;
FILE* Report_fp;
#if defined(_WIN32) && !defined(__CYGWIN__)
HANDLE Console_Handle;
#endif
int disp_width;
int disp_height;
char str_up [10];
char str_clreoln [10];
char str_emph [10];
char str_norm [10];
char Console_buff [1024];
} Console_IO_t;
extern Console_IO_t Console_IO;
#endif