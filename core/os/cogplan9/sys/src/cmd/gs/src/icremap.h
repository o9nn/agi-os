#ifndef icremap_INCLUDED
#  define icremap_INCLUDED
#include "gsccolor.h"
#ifndef int_remap_color_info_DEFINED
#  define int_remap_color_info_DEFINED
typedef struct int_remap_color_info_s int_remap_color_info_t;
#endif
struct int_remap_color_info_s {
op_proc_t proc;
float tint[GS_CLIENT_COLOR_MAX_COMPONENTS];
};
#define private_st_int_remap_color_info() \
gs_private_st_simple(st_int_remap_color_info, int_remap_color_info_t,\
"int_remap_color_info_t")
#endif