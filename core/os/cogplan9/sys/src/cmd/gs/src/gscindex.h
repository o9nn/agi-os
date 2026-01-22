#ifndef gscindex_INCLUDED
# define gscindex_INCLUDED
#include "gscspace.h"
extern int gs_cspace_build_Indexed(
gs_color_space ** ppcspace,
const gs_color_space * pbase_cspace,
uint num_entries,
const gs_const_string * ptbl,
gs_memory_t * pmem
);
extern int gs_cspace_indexed_num_entries(
const gs_color_space * pcspace
);
extern float *gs_cspace_indexed_value_array(
const gs_color_space * pcspace
);
extern int gs_cspace_indexed_set_proc(
gs_color_space * pcspace,
int (*proc) (const gs_indexed_params *, int, float *)
);
int gs_cspace_indexed_lookup(const gs_indexed_params *, int,
gs_client_color *);
#endif