#ifndef zht2_INCLUDED
#  define zht2_INCLUDED
#include "gscspace.h"
int gs_get_colorname_string(const gs_memory_t *mem,
gs_separation_name colorname_index,
unsigned char **ppstr,
unsigned int *pname_size);
#endif