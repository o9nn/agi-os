#ifndef gxtmap_INCLUDED
#  define gxtmap_INCLUDED
typedef struct gx_transfer_map_s gx_transfer_map;
typedef float (*gs_mapping_proc) (floatp, const gx_transfer_map *);
typedef float (*gs_mapping_closure_proc_t) (floatp value,
const gx_transfer_map * pmap,
const void *proc_data);
typedef struct gs_mapping_closure_s {
gs_mapping_closure_proc_t proc;
const void *data;
} gs_mapping_closure_t;
#endif