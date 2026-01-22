#include "iohelp.h"
#include <stdlib.h>
void __attribute__ ((weak))
iohelp_fetch_shared_data (void *foo)
{
abort ();
}
void __attribute__ ((weak))
iohelp_put_shared_data (void *foo)
{
abort ();
}