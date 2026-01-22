#ifndef gxclip_INCLUDED
#  define gxclip_INCLUDED
typedef struct clip_callback_data_s {
gx_device *tdev;
int x, y, w, h;
gx_color_index color[2];
const byte *data;
int sourcex;
uint raster;
int depth;
const gx_drawing_color *pdcolor;
gs_logical_operation_t lop;
const gx_clip_path *pcpath;
const gx_strip_bitmap *tiles;
gs_int_point phase;
const gx_color_index *scolors;
const gx_strip_bitmap *textures;
const gx_color_index *tcolors;
} clip_callback_data_t;
int
clip_call_fill_rectangle(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_copy_mono(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_copy_color(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_copy_alpha(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_fill_mask(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_strip_tile_rectangle(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec),
clip_call_strip_copy_rop(clip_callback_data_t * pccd,
int xc, int yc, int xec, int yec);
#endif