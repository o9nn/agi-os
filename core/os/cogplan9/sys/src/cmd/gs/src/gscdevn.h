#ifndef gscdevn_INCLUDED
# define gscdevn_INCLUDED
#include "gscspace.h"
int gs_build_DeviceN(
gs_color_space *pcspace,
uint num_components,
const gs_color_space *palt_cspace,
gs_memory_t *pmem
);
int gs_cspace_build_DeviceN(
gs_color_space **ppcspace,
gs_separation_name *psnames,
uint num_components,
const gs_color_space *palt_cspace,
gs_memory_t *pmem
);
extern int gs_cspace_set_devn_proc(
gs_color_space * pcspace,
int (*proc)(const float *,
float *,
const gs_imager_state *,
void *
),
void *proc_data
);
#ifndef gs_function_DEFINED
typedef struct gs_function_s gs_function_t;
# define gs_function_DEFINED
#endif
int gs_cspace_set_devn_function(gs_color_space *pcspace,
gs_function_t *pfn);
gs_function_t *gs_cspace_get_devn_function(const gs_color_space *pcspace);
int map_devn_using_function(const float *in, float *out,
const gs_imager_state *pis, void *data);
int gx_serialize_device_n_map(const gs_color_space * pcs, gs_device_n_map * m, stream * s);
#endif