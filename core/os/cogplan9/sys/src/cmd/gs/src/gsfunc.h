#ifndef gsfunc_INCLUDED
# define gsfunc_INCLUDED
#include "gstypes.h"
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
typedef int gs_function_type_t;
#define gs_function_params_common\
int m; \
const float *Domain; \
int n; \
const float *Range
#ifndef gs_data_source_DEFINED
# define gs_data_source_DEFINED
typedef struct gs_data_source_s gs_data_source_t;
#endif
#ifndef gs_param_list_DEFINED
# define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif
typedef struct gs_function_params_s {
gs_function_params_common;
} gs_function_params_t;
#ifndef gs_function_DEFINED
typedef struct gs_function_s gs_function_t;
# define gs_function_DEFINED
#endif
typedef struct gs_function_info_s {
const gs_data_source_t *DataSource;
ulong data_size;
const gs_function_t *const *Functions;
int num_Functions;
} gs_function_info_t;
#define FN_EVALUATE_PROC(proc)\
int proc(const gs_function_t * pfn, const float *in, float *out)
typedef FN_EVALUATE_PROC((*fn_evaluate_proc_t));
#define FN_IS_MONOTONIC_PROC(proc)\
int proc(const gs_function_t * pfn, const float *lower,\
const float *upper, uint *mask)
typedef FN_IS_MONOTONIC_PROC((*fn_is_monotonic_proc_t));
#define FN_GET_INFO_PROC(proc)\
void proc(const gs_function_t *pfn, gs_function_info_t *pfi)
typedef FN_GET_INFO_PROC((*fn_get_info_proc_t));
#define FN_GET_PARAMS_PROC(proc)\
int proc(const gs_function_t *pfn, gs_param_list *plist)
typedef FN_GET_PARAMS_PROC((*fn_get_params_proc_t));
#define FN_MAKE_SCALED_PROC(proc)\
int proc(const gs_function_t *pfn, gs_function_t **ppsfn,\
const gs_range_t *pranges, gs_memory_t *mem)
typedef FN_MAKE_SCALED_PROC((*fn_make_scaled_proc_t));
#define FN_FREE_PARAMS_PROC(proc)\
void proc(gs_function_params_t * params, gs_memory_t * mem)
typedef FN_FREE_PARAMS_PROC((*fn_free_params_proc_t));
#define FN_FREE_PROC(proc)\
void proc(gs_function_t * pfn, bool free_params, gs_memory_t * mem)
typedef FN_FREE_PROC((*fn_free_proc_t));
#define FN_SERIALIZE_PROC(proc)\
int proc(const gs_function_t * pfn, stream *s)
typedef FN_SERIALIZE_PROC((*fn_serialize_proc_t));
typedef struct gs_function_procs_s {
fn_evaluate_proc_t evaluate;
fn_is_monotonic_proc_t is_monotonic;
fn_get_info_proc_t get_info;
fn_get_params_proc_t get_params;
fn_make_scaled_proc_t make_scaled;
fn_free_params_proc_t free_params;
fn_free_proc_t free;
fn_serialize_proc_t serialize;
} gs_function_procs_t;
typedef struct gs_function_head_s {
gs_function_type_t type;
gs_function_procs_t procs;
} gs_function_head_t;
struct gs_function_s {
gs_function_head_t head;
gs_function_params_t params;
};
#define FunctionType(pfn) ((pfn)->head.type)
int alloc_function_array(uint count, gs_function_t *** pFunctions,
gs_memory_t *mem);
#define gs_function_evaluate(pfn, in, out)\
((pfn)->head.procs.evaluate)(pfn, in, out)
#define gs_function_is_monotonic(pfn, lower, upper, mask)\
((pfn)->head.procs.is_monotonic)(pfn, lower, upper, mask)
#define gs_function_get_info(pfn, pfi)\
((pfn)->head.procs.get_info(pfn, pfi))
#define gs_function_get_params(pfn, plist)\
((pfn)->head.procs.get_params(pfn, plist))
#define gs_function_make_scaled(pfn, ppsfn, pranges, mem)\
((pfn)->head.procs.make_scaled(pfn, ppsfn, pranges, mem))
#define gs_function_free_params(pfn, mem)\
((pfn)->head.procs.free_params(&(pfn)->params, mem))
#define gs_function_free(pfn, free_params, mem)\
((pfn)->head.procs.free(pfn, free_params, mem))
#define gs_function_serialize(pfn, s)\
((pfn)->head.procs.serialize(pfn, s))
#endif