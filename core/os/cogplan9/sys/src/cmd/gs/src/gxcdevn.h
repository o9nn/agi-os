#ifndef gxcdevn_INCLUDED
#  define gxcdevn_INCLUDED
#include "gsrefct.h"
#include "gxcindex.h"
#ifndef gs_device_n_map_DEFINED
#  define gs_device_n_map_DEFINED
typedef struct gs_device_n_map_s gs_device_n_map;
#endif
struct gs_device_n_map_s {
rc_header rc;
int (*tint_transform)(const float *in, float *out,
const gs_imager_state *pis, void *data);
void *tint_transform_data;
bool cache_valid;
float tint[GS_CLIENT_COLOR_MAX_COMPONENTS];
frac conc[GX_DEVICE_COLOR_MAX_COMPONENTS];
};
#define private_st_device_n_map() \
gs_private_st_ptrs1(st_device_n_map, gs_device_n_map, "gs_device_n_map",\
device_n_map_enum_ptrs, device_n_map_reloc_ptrs, tint_transform_data)
int alloc_device_n_map(gs_device_n_map ** ppmap, gs_memory_t * mem,
client_name_t cname);
bool using_alt_color_space(const gs_state * pgs);
#endif