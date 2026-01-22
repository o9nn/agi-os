#ifndef icie_INCLUDED
# define icie_INCLUDED
int dict_ranges_param(const gs_memory_t *mem,
const ref * pdref, const char *kstr, int count,
gs_range * prange);
int dict_range3_param(const gs_memory_t *mem,
const ref *pdref, const char *kstr,
gs_range3 *prange3);
int dict_matrix3_param(const gs_memory_t *mem, const ref *pdref,
const char *kstr,
gs_matrix3 *pmat3);
int dict_proc_array_param(const gs_memory_t *mem, const ref *pdict,
const char *kstr,
uint count, ref * pparray);
int dict_proc3_param(const gs_memory_t *mem, const ref *pdref,
const char *kstr, ref proc3[3]);
int cie_points_param(const gs_memory_t *mem,
const ref * pdref, gs_cie_wb * pwb);
int cie_table_param(const ref * ptable, gx_color_lookup_table * pclt,
gs_memory_t * mem);
int cie_set_finish(i_ctx_t *, gs_color_space *,
const ref_cie_procs *, int, int);
int cie_cache_push_finish(i_ctx_t *i_ctx_p, op_proc_t finish_proc,
gs_ref_memory_t * imem, void *data);
int cie_prepare_cache(i_ctx_t *i_ctx_p, const gs_range * domain,
const ref * proc, cie_cache_floats * pcache,
void *container, gs_ref_memory_t * imem,
client_name_t cname);
int cie_prepare_caches_4(i_ctx_t *i_ctx_p, const gs_range * domains,
const ref * procs,
cie_cache_floats * pc0,
cie_cache_floats * pc1,
cie_cache_floats * pc2,
cie_cache_floats * pc3 ,
void *container,
gs_ref_memory_t * imem, client_name_t cname);
#define cie_prepare_cache3(p,d3,p3,c3,pcie,imem,cname)\
cie_prepare_caches_4(p, (d3)->ranges, p3,\
&(c3)->floats, &(c3)[1].floats, &(c3)[2].floats,\
NULL, pcie, imem, cname)
#define cie_prepare_cache4(p,d4,p4,c4,pcie,imem,cname)\
cie_prepare_caches_4(p, (d4)->ranges, p4,\
&(c4)->floats, &(c4)[1].floats, &(c4)[2].floats,\
&(c4)[3].floats, pcie, imem, cname)
int cie_cache_joint(i_ctx_t *, const ref_cie_render_procs *,
const gs_cie_common *, gs_state *);
#endif