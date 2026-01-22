#ifndef icsmap_INCLUDED
# define icsmap_INCLUDED
#define num_csme 5
# define csme_num_components (-4)
# define csme_map (-3)
# define csme_proc (-2)
# define csme_hival (-1)
# define csme_index 0
int zcs_begin_map(i_ctx_t *i_ctx_p, gs_indexed_map ** pmap,
const ref * pproc, int num_entries,
const gs_direct_color_space * base_space,
op_proc_t map1);
#endif