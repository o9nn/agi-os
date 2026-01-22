#include <errno.h>
extern void do_bunzip2 (void);
static error_t
DO_UNZIP (void)
{
do_bunzip2 ();
return 0;
}
#define UNZIP		bunzip2
#include "unzipstore.c"