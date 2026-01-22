#ifndef gsropc_INCLUDED
#  define gsropc_INCLUDED
#include "gscompt.h"
#include "gsropt.h"
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
typedef struct gs_composite_rop_params_s {
gs_logical_operation_t log_op;
const gx_device_color *texture;
} gs_composite_rop_params_t;
int gs_create_composite_rop(gs_composite_t ** ppcte,
const gs_composite_rop_params_t * params,
gs_memory_t * mem);
#endif