#define _BSD_EXTENSION
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include "lametime.h"
double GetCPUTime ( void )
{
return clock () / (double) CLOCKS_PER_SEC;
}
double GetRealTime ( void )
{
struct timeval  t;
if ( 0 != gettimeofday (&t, NULL) )
assert (0);
return t.tv_sec + 1.e-6 * t.tv_usec;
}
int  lame_set_stream_binary_mode ( FILE* const fp )
{
return 0;
}
off_t  lame_get_file_size ( const char* const filename )
{
struct stat       sb;
if ( 0 == stat ( filename, &sb ) )
return sb.st_size;
return (off_t) -1;
}