#ifndef gsptype2_INCLUDED
#  define gsptype2_INCLUDED
#include "gspcolor.h"
#include "gsdcolor.h"
#include "gxfixed.h"
#ifndef gs_shading_t_DEFINED
#  define gs_shading_t_DEFINED
typedef struct gs_shading_s gs_shading_t;
#endif
typedef struct gs_pattern2_template_s {
gs_pattern_template_common;
const gs_shading_t *Shading;
} gs_pattern2_template_t;
#define private_st_pattern2_template() \
gs_private_st_suffix_add1(st_pattern2_template,\
gs_pattern2_template_t, "gs_pattern2_template_t",\
pattern2_template_enum_ptrs, pattern2_template_reloc_ptrs,\
st_pattern_template, Shading)
#define st_pattern2_template_max_ptrs (st_pattern_template_max_ptrs + 1)
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
typedef struct gs_pattern2_instance_s {
gs_pattern_instance_common;
gs_pattern2_template_t template;
bool shfill;
} gs_pattern2_instance_t;
#define private_st_pattern2_instance() \
gs_private_st_composite(st_pattern2_instance, gs_pattern2_instance_t,\
"gs_pattern2_instance_t", pattern2_instance_enum_ptrs,\
pattern2_instance_reloc_ptrs)
#ifndef gx_path_DEFINED
#define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef gx_device_DEFINED
#define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
extern const gx_device_color_type_t gx_dc_pattern2;
#define gx_dc_type_pattern2 (&gx_dc_pattern2)
void gs_pattern2_init(gs_pattern2_template_t *);
bool gx_dc_is_pattern2_color(const gx_device_color *pdevc);
int gx_dc_pattern2_fill_path(const gx_device_color * pdevc,
gx_path * ppath, gs_fixed_rect * rect,
gx_device * dev);
int gs_pattern2_set_shfill(gs_client_color * pcc);
int gx_dc_pattern2_shade_bbox_transform2fixed(const gs_rect * rect,
const gs_imager_state * pis, gs_fixed_rect * rfixed);
int gx_dc_pattern2_get_bbox(const gx_device_color * pdevc, gs_fixed_rect *bbox);
bool gx_dc_pattern2_can_overlap(const gx_device_color *pdevc);
bool gx_dc_pattern2_has_background(const gx_device_color *pdevc);
#endif