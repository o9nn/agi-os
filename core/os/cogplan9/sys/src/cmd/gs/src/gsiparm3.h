#ifndef gsiparm3_INCLUDED
#  define gsiparm3_INCLUDED
#include "gsiparam.h"
typedef enum {
interleave_chunky = 1,
interleave_scan_lines = 2,
interleave_separate_source = 3
} gs_image3_interleave_type_t;
typedef struct gs_image3_s {
gs_pixel_image_common;
int InterleaveType;
gs_data_image_t MaskDict;
} gs_image3_t;
#define private_st_gs_image3()	\
gs_private_st_suffix_add0(st_gs_image3, gs_image3_t, "gs_image3_t",\
image3_enum_ptrs, image3_reloc_ptrs, st_gs_pixel_image)
void gs_image3_t_init(gs_image3_t * pim, const gs_color_space * color_space,
gs_image3_interleave_type_t interleave_type);
#endif