#ifndef gxfunc_INCLUDED
# define gxfunc_INCLUDED
#include "gsfunc.h"
#include "gsstruct.h"
extern_st(st_function);
#define public_st_function() \
gs_public_st_ptrs2(st_function, gs_function_t, "gs_function_t",\
function_enum_ptrs, function_reloc_ptrs, params.Domain, params.Range)
void fn_common_free_params(gs_function_params_t * params, gs_memory_t * mem);
void fn_common_free(gs_function_t * pfn, bool free_params, gs_memory_t * mem);
int fn_check_mnDR(const gs_function_params_t * params, int m, int n);
FN_GET_INFO_PROC(gs_function_get_info_default);
int fn_common_get_params(const gs_function_t *pfn, gs_param_list *plist);
void *fn_copy_values(const void *pvalues, int count, int size,
gs_memory_t *mem);
int fn_scale_pairs(const float **ppvalues, const float *pvalues, int npairs,
const gs_range_t *pranges, gs_memory_t *mem);
int fn_common_scale(gs_function_t *psfn, const gs_function_t *pfn,
const gs_range_t *pranges, gs_memory_t *mem);
int fn_common_serialize(const gs_function_t * pfn, stream *s);
#endif