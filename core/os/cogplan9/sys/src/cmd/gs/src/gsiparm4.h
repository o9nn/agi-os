#ifndef gsiparm4_INCLUDED
# define gsiparm4_INCLUDED
#include "gsiparam.h"
typedef struct gs_image4_s {
gs_pixel_image_common;
bool MaskColor_is_range;
uint MaskColor[GS_IMAGE_MAX_COMPONENTS * 2];
} gs_image4_t;
#define private_st_gs_image4() \
extern_st(st_gs_pixel_image);\
gs_private_st_suffix_add0(st_gs_image4, gs_image4_t, "gs_image4_t",\
image4_enum_ptrs, image4_reloc_ptrs, st_gs_pixel_image)
void gs_image4_t_init(gs_image4_t * pim, const gs_color_space * color_space);
#endif