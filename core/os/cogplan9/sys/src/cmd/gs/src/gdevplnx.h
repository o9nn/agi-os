#ifndef gdevplnx_INCLUDED
#  define gdevplnx_INCLUDED
#include "gxrplane.h"
typedef struct gx_device_plane_extract_s {
gx_device_forward_common;
gx_device *plane_dev;
gx_render_plane_t plane;
gx_color_index plane_white;
uint plane_mask;
bool plane_dev_is_memory;
bool any_marks;
} gx_device_plane_extract;
extern_st(st_device_plane_extract);
#define public_st_device_plane_extract()	\
gs_public_st_complex_only(st_device_plane_extract, gx_device_plane_extract,\
"gx_device_plane_extract", 0, device_plane_extract_enum_ptrs,\
device_plane_extract_reloc_ptrs, gx_device_finalize)
int plane_device_init(gx_device_plane_extract *edev, gx_device *target,
gx_device *plane_dev,
const gx_render_plane_t *render_plane, bool clear);
#endif