#ifndef gsfunc0_INCLUDED
#  define gsfunc0_INCLUDED
#include "gsfunc.h"
#include "gsdsrc.h"
#define function_type_Sampled 0
typedef struct gs_function_Sd_params_s {
gs_function_params_common;
int Order;
gs_data_source_t DataSource;
int BitsPerSample;
const float *Encode;
const float *Decode;
const int *Size;
double *pole;
int *array_step;
int *stream_step;
int array_size;
} gs_function_Sd_params_t;
#define private_st_function_Sd()	\
gs_private_st_composite(st_function_Sd, gs_function_Sd_t,\
"gs_function_Sd_t", function_Sd_enum_ptrs, function_Sd_reloc_ptrs)
int gs_function_Sd_init(gs_function_t ** ppfn,
const gs_function_Sd_params_t * params,
gs_memory_t * mem);
void gs_function_Sd_free_params(gs_function_Sd_params_t * params,
gs_memory_t * mem);
#endif