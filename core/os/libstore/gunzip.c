#include <errno.h>
extern void do_gunzip (void);
static error_t
DO_UNZIP (void)
{
do_gunzip ();
return 0;
}
#define UNZIP gunzip
#include "unzipstore.c"