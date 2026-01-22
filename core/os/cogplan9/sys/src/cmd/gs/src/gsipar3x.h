#ifndef gsipar3x_INCLUDED
#  define gsipar3x_INCLUDED
#include "gsiparam.h"
#include "gsiparm3.h"
#define IMAGE3X_IMAGETYPE 103
typedef struct gs_image3x_mask_s {
int InterleaveType;
float Matte[GS_CLIENT_COLOR_MAX_COMPONENTS];
bool has_Matte;
gs_data_image_t MaskDict;
} gs_image3x_mask_t;
typedef struct gs_image3x_s {
gs_pixel_image_common;
gs_image3x_mask_t Opacity, Shape;
} gs_image3x_t;
#define private_st_gs_image3x()	\
gs_private_st_suffix_add0(st_gs_image3x, gs_image3x_t, "gs_image3x_t",\
image3x_enum_ptrs, image3x_reloc_ptrs, st_gs_pixel_image)
void gs_image3x_t_init(gs_image3x_t *pim, const gs_color_space *color_space);
#endif