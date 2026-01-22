#ifndef gsiparm2_INCLUDED
#  define gsiparm2_INCLUDED
#include "gsiparam.h"
#ifndef gx_path_DEFINED
#  define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
typedef struct gs_image2_s {
gs_image_common;
gs_state *DataSource;
float XOrigin, YOrigin;
float Width, Height;
gx_path *UnpaintedPath;
bool PixelCopy;
} gs_image2_t;
#define private_st_gs_image2()	\
extern_st(st_gs_image_common);\
gs_private_st_suffix_add2(st_gs_image2, gs_image2_t, "gs_image2_t",\
image2_enum_ptrs, image2_reloc_ptrs, st_gs_image_common,\
DataSource, UnpaintedPath)
void gs_image2_t_init(gs_image2_t * pim);
#endif