#ifndef gscrdp_INCLUDED
#  define gscrdp_INCLUDED
#include "gscie.h"
#include "gsparam.h"
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
int param_write_cie_render1(gs_param_list * plist, gs_param_name key,
gs_cie_render * pcrd,
gs_memory_t * mem);
int param_put_cie_render1(gs_param_list * plist, gs_cie_render * pcrd,
gs_memory_t * mem);
int gs_cie_render1_param_initialize(gs_cie_render * pcrd,
gs_param_list * plist,
gs_param_name key,
gx_device * dev);
int param_get_cie_render1(gs_cie_render * pcrd,
gs_param_list * plist,
gx_device * dev);
#define GX_DEVICE_CRD1_TYPE 101
#endif