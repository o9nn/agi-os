#ifndef gdevmeds_INCLUDED
#  define gdevmeds_INCLUDED
#include "gdevprn.h"
int select_medium(gx_device_printer *pdev, const char **available,
int default_index);
#endif