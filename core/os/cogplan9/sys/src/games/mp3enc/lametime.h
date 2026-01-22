#ifndef LAME_LAMETIME_H
#define LAME_LAMETIME_H
#include <sys/types.h>
#include "lame.h"
extern double  GetCPUTime  ( void );
extern double  GetRealTime ( void );
extern int     lame_set_stream_binary_mode ( FILE* const fp );
extern off_t   lame_get_file_size          ( const char* const filename );
#endif