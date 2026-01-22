#ifndef gxoprect_INCLUDED
#define gxoprect_INCLUDED
extern int gx_overprint_generic_fill_rectangle(
gx_device * tdev,
gx_color_index drawn_comps,
int x,
int y,
int w,
int h,
gx_color_index color,
gs_memory_t * mem );
extern int gx_overprint_sep_fill_rectangle_1(
gx_device * tdev,
gx_color_index retain_mask,
int x,
int y,
int w,
int h,
gx_color_index color,
gs_memory_t * mem );
extern int gx_overprint_sep_fill_rectangle_2(
gx_device * tdev,
gx_color_index retain_mask,
int x,
int y,
int w,
int h,
gx_color_index color,
gs_memory_t * mem );
#endif