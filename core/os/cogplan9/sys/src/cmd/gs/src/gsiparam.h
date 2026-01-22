#ifndef gsiparam_INCLUDED
# define gsiparam_INCLUDED
#include "gsccolor.h"
#include "gsmatrix.h"
#include "gsstype.h"
#ifndef gx_image_type_DEFINED
# define gx_image_type_DEFINED
typedef struct gx_image_type_s gx_image_type_t;
#endif
#define gs_image_common\
const gx_image_type_t *type;\
\
gs_matrix ImageMatrix
typedef struct gs_image_common_s {
gs_image_common;
} gs_image_common_t;
#define public_st_gs_image_common() \
gs_public_st_simple(st_gs_image_common, gs_image_common_t,\
"gs_image_common_t")
#define GS_IMAGE_MAX_COLOR_COMPONENTS GS_CLIENT_COLOR_MAX_COMPONENTS
#define GS_IMAGE_MAX_COMPONENTS (GS_IMAGE_MAX_COLOR_COMPONENTS + 1)
#define gs_image_max_components GS_IMAGE_MAX_COMPONENTS
#define GS_IMAGE_MAX_PLANES (GS_IMAGE_MAX_COMPONENTS * 8)
#define gs_image_max_planes GS_IMAGE_MAX_PLANES
#define gs_data_image_common\
gs_image_common;\
\
int Width;\
\
int Height;\
\
int BitsPerComponent;\
\
float Decode[GS_IMAGE_MAX_COMPONENTS * 2];\
\
bool Interpolate
typedef struct gs_data_image_s {
gs_data_image_common;
} gs_data_image_t;
#define public_st_gs_data_image() \
gs_public_st_simple(st_gs_data_image, gs_data_image_t,\
"gs_data_image_t")
typedef enum {
gs_image_format_chunky = 0,
gs_image_format_component_planar = 1,
gs_image_format_bit_planar = 2
} gs_image_format_t;
#ifndef gs_color_space_DEFINED
# define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif
#define gs_pixel_image_common\
gs_data_image_common;\
\
gs_image_format_t format;\
\
const gs_color_space *ColorSpace;\
\
bool CombineWithColor
typedef struct gs_pixel_image_s {
gs_pixel_image_common;
} gs_pixel_image_t;
extern_st(st_gs_pixel_image);
#define public_st_gs_pixel_image() \
gs_public_st_ptrs1(st_gs_pixel_image, gs_pixel_image_t,\
"gs_data_image_t", pixel_image_enum_ptrs, pixel_image_reloc_ptrs,\
ColorSpace)
typedef enum {
gs_image_alpha_none = 0,
gs_image_alpha_first,
gs_image_alpha_last
} gs_image_alpha_t;
typedef struct gs_image1_s {
gs_pixel_image_common;
bool ImageMask;
bool adjust;
gs_image_alpha_t Alpha;
} gs_image1_t;
extern_st(st_gs_image1);
#define public_st_gs_image1() \
gs_public_st_suffix_add0(st_gs_image1, gs_image1_t, "gs_image1_t",\
image1_enum_ptrs, image1_reloc_ptrs, st_gs_pixel_image)
typedef gs_image1_t gs_image_t;
void
gs_image_common_t_init(gs_image_common_t * pic),
gs_data_image_t_init(gs_data_image_t * pim, int num_components),
gs_pixel_image_t_init(gs_pixel_image_t * pim,
const gs_color_space * color_space);
void gs_image_t_init_adjust(gs_image_t * pim, const gs_color_space * pcs,
bool adjust);
#define gs_image_t_init(pim, pcs)\
gs_image_t_init_adjust(pim, pcs, true)
void gs_image_t_init_mask_adjust(gs_image_t * pim, bool write_1s,
bool adjust);
#define gs_image_t_init_mask(pim, write_1s)\
gs_image_t_init_mask_adjust(pim, write_1s, true)
#if 0
int gx_map_image_color(gx_device * dev,
const gs_image_t * pim,
const gx_color_rendering_info * pcri,
const uint components[GS_IMAGE_MAX_COMPONENTS],
gx_drawing_color * pdcolor);
#endif
#endif