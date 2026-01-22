#ifndef gxropc_INCLUDED
#  define gxropc_INCLUDED
#include "gsropc.h"
#include "gxcomp.h"
typedef struct gs_composite_rop_s {
gs_composite_common;
gs_composite_rop_params_t params;
} gs_composite_rop_t;
#define private_st_composite_rop() \
gs_private_st_ptrs1(st_composite_rop, gs_composite_rop_t,\
"gs_composite_rop_t", composite_rop_enum_ptrs, composite_rop_reloc_ptrs,\
params.texture)
void gx_init_composite_rop(gs_composite_rop_t * pcte,
const gs_composite_rop_params_t * params);
#endif