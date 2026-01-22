#ifndef gscsepr_INCLUDED
# define gscsepr_INCLUDED
#include "gscspace.h"
extern int gs_cspace_build_Separation(
gs_color_space ** ppcspace,
gs_separation_name sname,
const gs_color_space * palt_cspace,
int cache_size,
gs_memory_t * pmem
);
extern int gs_build_Separation(
gs_color_space * pcspace,
const gs_color_space * palt_cspace,
gs_memory_t * pmem
);
#ifndef gs_function_DEFINED
typedef struct gs_function_s gs_function_t;
# define gs_function_DEFINED
#endif
int gs_cspace_set_sepr_proc(gs_color_space * pcspace,
int (*proc)(const float *,
float *,
const gs_imager_state *,
void *
),
void *proc_data
);
int gs_cspace_set_sepr_function(const gs_color_space *pcspace,
gs_function_t *pfn);
gs_function_t *gs_cspace_get_sepr_function(const gs_color_space *pcspace);
#endif