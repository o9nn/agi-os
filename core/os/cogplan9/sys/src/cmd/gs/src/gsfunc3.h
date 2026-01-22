#ifndef gsfunc3_INCLUDED
# define gsfunc3_INCLUDED
#include "gsfunc.h"
#include "gsdsrc.h"
enum {
function_type_ExponentialInterpolation = 2,
function_type_1InputStitching = 3,
function_type_ArrayedOutput = -1
};
typedef struct gs_function_ElIn_params_s {
gs_function_params_common;
const float *C0;
const float *C1;
float N;
} gs_function_ElIn_params_t;
#define private_st_function_ElIn() \
gs_private_st_suffix_add2(st_function_ElIn, gs_function_ElIn_t,\
"gs_function_ElIn_t", function_ElIn_enum_ptrs, function_ElIn_reloc_ptrs,\
st_function, params.C0, params.C1)
typedef struct gs_function_1ItSg_params_s {
gs_function_params_common;
int k;
const gs_function_t *const *Functions;
const float *Bounds;
const float *Encode;
} gs_function_1ItSg_params_t;
#define private_st_function_1ItSg() \
gs_private_st_suffix_add3(st_function_1ItSg, gs_function_1ItSg_t,\
"gs_function_1ItSg_t", function_1ItSg_enum_ptrs, function_1ItSg_reloc_ptrs,\
st_function, params.Functions, params.Bounds, params.Encode)
typedef struct gs_function_AdOt_params_s {
gs_function_params_common;
const gs_function_t *const *Functions;
} gs_function_AdOt_params_t;
#define private_st_function_AdOt() \
gs_private_st_suffix_add1(st_function_AdOt, gs_function_AdOt_t,\
"gs_function_AdOt_t", function_AdOt_enum_ptrs, function_AdOt_reloc_ptrs,\
st_function, params.Functions)
int gs_function_ElIn_init(gs_function_t ** ppfn,
const gs_function_ElIn_params_t * params,
gs_memory_t * mem);
int gs_function_1ItSg_init(gs_function_t ** ppfn,
const gs_function_1ItSg_params_t * params,
gs_memory_t * mem);
int gs_function_AdOt_init(gs_function_t ** ppfn,
const gs_function_AdOt_params_t * params,
gs_memory_t * mem);
void gs_function_ElIn_free_params(gs_function_ElIn_params_t * params,
gs_memory_t * mem);
void gs_function_1ItSg_free_params(gs_function_1ItSg_params_t * params,
gs_memory_t * mem);
void gs_function_AdOt_free_params(gs_function_AdOt_params_t * params,
gs_memory_t * mem);
#endif