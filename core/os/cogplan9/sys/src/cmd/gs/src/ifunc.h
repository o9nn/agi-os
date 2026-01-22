#ifndef ifunc_INCLUDED
# define ifunc_INCLUDED
#include "gsfunc.h"
#define build_function_proc(proc)\
int proc(i_ctx_t *i_ctx_p, const ref *op, const gs_function_params_t *params, int depth,\
gs_function_t **ppfn, gs_memory_t *mem)
typedef build_function_proc((*build_function_proc_t));
typedef struct build_function_type_s {
int type;
build_function_proc_t proc;
} build_function_type_t;
extern const build_function_type_t build_function_type_table[];
extern const uint build_function_type_table_count;
int fn_build_function(i_ctx_t *i_ctx_p, const ref * op, gs_function_t ** ppfn,
gs_memory_t *mem);
int fn_build_sub_function(i_ctx_t *i_ctx_p, const ref * op, gs_function_t ** ppfn,
int depth, gs_memory_t *mem);
int fn_build_float_array(const ref * op, const char *kstr, bool required,
bool even, const float **pparray,
gs_memory_t *mem);
int
fn_build_float_array_forced(const ref * op, const char *kstr, bool required,
const float **pparray, gs_memory_t *mem);
gs_function_t *ref_function(const ref *op);
int zexecfunction(i_ctx_t *);
#endif