#ifndef gxdevcli_INCLUDED
#  define gxdevcli_INCLUDED
#include "std.h"
#include "stdint_.h"
#include "gscompt.h"
#include "gsdcolor.h"
#include "gsmatrix.h"
#include "gsiparam.h"
#include "gsrefct.h"
#include "gsropt.h"
#include "gsstruct.h"
#include "gstparam.h"
#include "gsxfont.h"
#include "gxbitmap.h"
#include "gxcindex.h"
#include "gxcvalue.h"
#include "gxfixed.h"
#include "gxtext.h"
#include "gxcmap.h"
#ifndef gx_device_DEFINED
#  define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
#ifndef gx_path_DEFINED
#  define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef gx_clip_path_DEFINED
#  define gx_clip_path_DEFINED
typedef struct gx_clip_path_s gx_clip_path;
#endif
#ifndef gx_fill_params_DEFINED
#  define gx_fill_params_DEFINED
typedef struct gx_fill_params_s gx_fill_params;
#endif
#ifndef gx_stroke_params_DEFINED
#  define gx_stroke_params_DEFINED
typedef struct gx_stroke_params_s gx_stroke_params;
#endif
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gx_image_enum_common_t_DEFINED
#  define gx_image_enum_common_t_DEFINED
typedef struct gx_image_enum_common_s gx_image_enum_common_t;
#endif
#ifndef gs_pattern1_instance_t_DEFINED
#  define gs_pattern1_instance_t_DEFINED
typedef struct gs_pattern1_instance_s gs_pattern1_instance_t;
#endif
typedef gx_device_color gx_drawing_color;
typedef enum {
go_text,
go_graphics
} graphics_object_type;
typedef struct gs_fixed_edge_s {
gs_fixed_point start;
gs_fixed_point end;
} gs_fixed_edge;
#ifndef gs_get_bits_params_DEFINED
#  define gs_get_bits_params_DEFINED
typedef struct gs_get_bits_params_s gs_get_bits_params_t;
#endif
typedef struct gx_device_anti_alias_info_s {
int text_bits;
int graphics_bits;
} gx_device_anti_alias_info;
typedef int32_t frac31;
typedef struct gs_linear_color_edge_s {
gs_fixed_point start;
gs_fixed_point end;
const frac31 *c0, *c1;
fixed clip_x;
} gs_linear_color_edge;
typedef enum {
GX_CINFO_UNKNOWN_SEP_LIN = -1,
GX_CINFO_SEP_LIN_NONE = 0,
GX_CINFO_SEP_LIN
} gx_color_enc_sep_lin_t;
typedef enum {
GX_CINFO_POLARITY_UNKNOWN = -1,
GX_CINFO_POLARITY_SUBTRACTIVE = 0,
GX_CINFO_POLARITY_ADDITIVE
} gx_color_polarity_t;
typedef enum {
GX_CINFO_OPMODE_UNKNOWN = -1,
GX_CINFO_OPMODE_NOT = 0,
GX_CINFO_OPMODE
} gx_cm_opmode_t;
#define GX_CINFO_COMP_NO_INDEX 0xff
#define GX_CINFO_COMP_INDEX_UNKNOWN 0xfe
typedef struct gx_device_color_info_s {
int max_components;
int num_components;
gx_color_polarity_t polarity;
byte depth;
byte gray_index;
uint max_gray;
uint max_color;
uint dither_grays;
uint dither_colors;
gx_device_anti_alias_info anti_alias;
gx_color_enc_sep_lin_t separable_and_linear;
byte                   comp_shift[GX_DEVICE_COLOR_MAX_COMPONENTS];
byte                   comp_bits[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index         comp_mask[GX_DEVICE_COLOR_MAX_COMPONENTS];
const char * cm_name;
gx_cm_opmode_t opmode;
gx_color_index process_comps;
} gx_device_color_info;
#define dci_extended_alpha_values(mcmp, nc, p, d, gi, mg, \
mc, dg, dc, ta, ga, sl, cn)   \
{mcmp , \
nc , \
p , \
d , \
gi , \
mg , \
mc , \
dg , \
dc , \
{ ta, ga } , \
sl , \
{ 0 } , \
{ 0 } , \
{ 0 } , \
cn ,\
GX_CINFO_OPMODE_UNKNOWN ,\
0  }
#define gx_device_has_color(dev)                           \
( (dev)->color_info.num_components > 1 ||                \
(dev)->color_info.gray_index == GX_CINFO_COMP_NO_INDEX )
#define dci_std_cm_name(nc)                 \
( (nc) == 1 ? "DeviceGray"              \
: ((nc) == 3 ? "DeviceRGB"  \
: "DeviceCMYK") )
#define dci_std_polarity(nc)                    \
( (nc) == 4 ? GX_CINFO_POLARITY_SUBTRACTIVE \
: GX_CINFO_POLARITY_ADDITIVE )
#define dci_std_gray_index(nc)    \
((nc) == 3 ? GX_CINFO_COMP_NO_INDEX : (nc) - 1)
#define dci_alpha_values(nc, depth, mg, mc, dg, dc, ta, ga) \
dci_extended_alpha_values(nc, nc,			    \
dci_std_polarity(nc),         \
depth,                        \
dci_std_gray_index(nc),       \
mg, mc, dg, dc, ta, ga,       \
GX_CINFO_UNKNOWN_SEP_LIN,     \
dci_std_cm_name(nc) )
#define dci_std_color_depth(color_bits)   \
((color_bits) == 1 ? 1 : ((color_bits) + 7) & ~7)
#define dci_std_color_num_components(color_bits)      \
( (color_bits) <= 1 ? 1                           \
: ((color_bits) % 3 == 0 ||     \
(color_bits) == 4     ||     \
(color_bits) == 8       ) ? 3 : 4 )
#define dci_std_gray_bits(nc, color_bits)    \
((color_bits) - ((nc) - 1) * ((color_bits) / (nc)))
#define dci_std_color_bits(nc, color_bits)                        \
( (nc) == 3                                                   \
? (color_bits) / (nc)                                     \
: ( (nc) == 1                                             \
? 0                                                 \
: ((color_bits) - dci_std_gray_bits(nc, color_bits))\
/ ((nc) == 1 ? (1) : (nc) - 1) ) )
#define dci_std_color_max_gray(nc, color_bits)            \
( (nc) == 3                                           \
? 0                                               \
: (1 << dci_std_gray_bits(nc, color_bits)) - 1 )
#define dci_std_color_max_color(nc, color_bits)               \
( (nc) == 1                                               \
? 0                                                   \
: (1 << dci_std_color_bits(nc, color_bits)) - 1 )
#define dci_std_color_(nc, color_bits)                        \
dci_values( nc,                                           \
dci_std_color_depth(color_bits),              \
dci_std_color_max_gray(nc, color_bits),       \
dci_std_color_max_color(nc, color_bits),      \
dci_std_color_max_gray(nc, color_bits) + 1,   \
dci_std_color_max_color(nc, color_bits) + 1 )
#define dci_std_color(color_bits)                             \
dci_std_color_( dci_std_color_num_components(color_bits), \
color_bits )
#define dci_values(nc,depth,mg,mc,dg,dc)\
dci_alpha_values(nc, depth, mg, mc, dg, dc, 1, 1)
#define dci_black_and_white dci_std_color(1)
#define dci_black_and_white_() dci_black_and_white
#define dci_color(depth,maxv,dither)\
dci_values(3, depth, maxv, maxv, dither, dither)
#define get_process_color_model_name(dev) \
((dev)->color_info.cm_name)
typedef struct gx_device_procs_s gx_device_procs;
typedef struct gx_page_device_procs_s {
#define dev_page_proc_install(proc)\
int proc(gx_device *dev, gs_state *pgs)
dev_page_proc_install((*install));
#define dev_page_proc_begin_page(proc)\
int proc(gx_device *dev, gs_state *pgs)
dev_page_proc_begin_page((*begin_page));
#define dev_page_proc_end_page(proc)\
int proc(gx_device *dev, int reason, gs_state *pgs)
dev_page_proc_end_page((*end_page));
} gx_page_device_procs;
dev_page_proc_install(gx_default_install);
dev_page_proc_begin_page(gx_default_begin_page);
dev_page_proc_end_page(gx_default_end_page);
typedef struct gx_device_cached_colors_s {
gx_color_index black, white;
} gx_device_cached_colors_t;
#define gx_device_common\
int params_size;		\
\
const gx_device_procs *static_procs;	\
\
const char *dname;		\
gs_memory_t *memory;		\
gs_memory_type_ptr_t stype;	\
\
bool stype_is_dynamic;		\
\
void (*finalize)(gx_device *);  \
\
rc_header rc;			\
\
bool retained;			\
bool is_open;			\
int max_fill_band;		\
\
\
gx_device_color_info color_info;	\
gx_device_cached_colors_t cached_colors;\
int width;			\
int height;			\
int TrayOrientation;            \
float MediaSize[2];		\
float ImagingBBox[4];		\
bool ImagingBBox_set;\
float HWResolution[2];		\
float MarginsHWResolution[2];	\
float Margins[2];		\
\
\
float HWMargins[4];		\
\
long PageCount;			\
long ShowpageCount;		\
int NumCopies;\
bool NumCopies_set;\
bool IgnoreNumCopies;		\
bool UseCIEColor;		\
bool LockSafetyParams;		\
gx_page_device_procs page_procs;	\
\
gx_device_procs procs
#define x_pixels_per_inch HWResolution[0]
#define y_pixels_per_inch HWResolution[1]
#define offset_margin_values(x, y, left, bot, right, top)\
{x, y}, {left, bot, right, top}
#define margin_values(left, bot, right, top)\
offset_margin_values(0, 0, left, bot, right, top)
#define no_margins margin_values(0, 0, 0, 0)
#define no_margins_() no_margins
#define dev_x_offset(dev) ((dev)->Margins[0] / (dev)->MarginsHWResolution[0])
#define dev_y_offset(dev) ((dev)->Margins[1] / (dev)->MarginsHWResolution[1])
#define dev_y_offset_points(dev) (dev_y_offset(dev) * 72.0)
#define dev_l_margin(dev) ((dev)->HWMargins[0] / 72.0)
#define dev_b_margin(dev) ((dev)->HWMargins[1] / 72.0)
#define dev_b_margin_points(dev) ((dev)->HWMargins[1])
#define dev_r_margin(dev) ((dev)->HWMargins[2] / 72.0)
#define dev_t_margin(dev) ((dev)->HWMargins[3] / 72.0)
#define dev_t_margin_points(dev) ((dev)->HWMargins[3])
#define open_init_closed() 0 , 0
#define open_init_open() 1 , 0
#define dev_proc(dev, p) ((dev)->procs.p)
#define set_dev_proc(dev, p, proc) ((dev)->procs.p = (proc))
#define fill_dev_proc(dev, p, dproc)\
if ( dev_proc(dev, p) == 0 ) set_dev_proc(dev, p, dproc)
#define assign_dev_procs(todev, fromdev)\
((todev)->procs = (fromdev)->procs)
#ifndef gs_param_list_DEFINED
#  define gs_param_list_DEFINED
typedef struct gs_param_list_s gs_param_list;
#endif
#define dev_t_proc_open_device(proc, dev_t)\
int proc(dev_t *dev)
#define dev_proc_open_device(proc)\
dev_t_proc_open_device(proc, gx_device)
#define dev_t_proc_get_initial_matrix(proc, dev_t)\
void proc(dev_t *dev, gs_matrix *pmat)
#define dev_proc_get_initial_matrix(proc)\
dev_t_proc_get_initial_matrix(proc, gx_device)
#define dev_t_proc_sync_output(proc, dev_t)\
int proc(dev_t *dev)
#define dev_proc_sync_output(proc)\
dev_t_proc_sync_output(proc, gx_device)
#define dev_t_proc_output_page(proc, dev_t)\
int proc(dev_t *dev, int num_copies, int flush)
#define dev_proc_output_page(proc)\
dev_t_proc_output_page(proc, gx_device)
#define dev_t_proc_close_device(proc, dev_t)\
int proc(dev_t *dev)
#define dev_proc_close_device(proc)\
dev_t_proc_close_device(proc, gx_device)
#define dev_t_proc_map_rgb_color(proc, dev_t)\
gx_color_index proc(dev_t *dev, const gx_color_value cv[])
#define dev_proc_map_rgb_color(proc)\
dev_t_proc_map_rgb_color(proc, gx_device)
#define dev_t_proc_map_color_rgb(proc, dev_t)\
int proc(dev_t *dev,\
gx_color_index color, gx_color_value rgb[3])
#define dev_proc_map_color_rgb(proc)\
dev_t_proc_map_color_rgb(proc, gx_device)
#define dev_t_proc_fill_rectangle(proc, dev_t)\
int proc(dev_t *dev,\
int x, int y, int width, int height, gx_color_index color)
#define dev_proc_fill_rectangle(proc)\
dev_t_proc_fill_rectangle(proc, gx_device)
#define dev_t_proc_tile_rectangle(proc, dev_t)\
int proc(dev_t *dev,\
const gx_tile_bitmap *tile, int x, int y, int width, int height,\
gx_color_index color0, gx_color_index color1,\
int phase_x, int phase_y)
#define dev_proc_tile_rectangle(proc)\
dev_t_proc_tile_rectangle(proc, gx_device)
#define dev_t_proc_copy_mono(proc, dev_t)\
int proc(dev_t *dev,\
const byte *data, int data_x, int raster, gx_bitmap_id id,\
int x, int y, int width, int height,\
gx_color_index color0, gx_color_index color1)
#define dev_proc_copy_mono(proc)\
dev_t_proc_copy_mono(proc, gx_device)
#define dev_t_proc_copy_color(proc, dev_t)\
int proc(dev_t *dev,\
const byte *data, int data_x, int raster, gx_bitmap_id id,\
int x, int y, int width, int height)
#define dev_proc_copy_color(proc)\
dev_t_proc_copy_color(proc, gx_device)
#define dev_t_proc_draw_line(proc, dev_t)\
int proc(dev_t *dev,\
int x0, int y0, int x1, int y1, gx_color_index color)
#define dev_proc_draw_line(proc)\
dev_t_proc_draw_line(proc, gx_device)
#define dev_t_proc_get_bits(proc, dev_t)\
int proc(dev_t *dev,\
int y, byte *data, byte **actual_data)
#define dev_proc_get_bits(proc)\
dev_t_proc_get_bits(proc, gx_device)
#define dev_t_proc_get_params(proc, dev_t)\
int proc(dev_t *dev, gs_param_list *plist)
#define dev_proc_get_params(proc)\
dev_t_proc_get_params(proc, gx_device)
#define dev_t_proc_put_params(proc, dev_t)\
int proc(dev_t *dev, gs_param_list *plist)
#define dev_proc_put_params(proc)\
dev_t_proc_put_params(proc, gx_device)
#define dev_t_proc_map_cmyk_color(proc, dev_t)\
gx_color_index proc(dev_t *dev, const gx_color_value cv[])
#define dev_proc_map_cmyk_color(proc)\
dev_t_proc_map_cmyk_color(proc, gx_device)
#define dev_t_proc_get_xfont_procs(proc, dev_t)\
const gx_xfont_procs *proc(dev_t *dev)
#define dev_proc_get_xfont_procs(proc)\
dev_t_proc_get_xfont_procs(proc, gx_device)
#define dev_t_proc_get_xfont_device(proc, dev_t)\
gx_device *proc(dev_t *dev)
#define dev_proc_get_xfont_device(proc)\
dev_t_proc_get_xfont_device(proc, gx_device)
#define dev_t_proc_map_rgb_alpha_color(proc, dev_t)\
gx_color_index proc(dev_t *dev,\
gx_color_value red, gx_color_value green, gx_color_value blue,\
gx_color_value alpha)
#define dev_proc_map_rgb_alpha_color(proc)\
dev_t_proc_map_rgb_alpha_color(proc, gx_device)
#define dev_t_proc_get_page_device(proc, dev_t)\
gx_device *proc(dev_t *dev)
#define dev_proc_get_page_device(proc)\
dev_t_proc_get_page_device(proc, gx_device)
#define dev_t_proc_get_alpha_bits(proc, dev_t)\
int proc(dev_t *dev, graphics_object_type type)
#define dev_proc_get_alpha_bits(proc)\
dev_t_proc_get_alpha_bits(proc, gx_device)
#define dev_t_proc_copy_alpha(proc, dev_t)\
int proc(dev_t *dev, const byte *data, int data_x,\
int raster, gx_bitmap_id id, int x, int y, int width, int height,\
gx_color_index color, int depth)
#define dev_proc_copy_alpha(proc)\
dev_t_proc_copy_alpha(proc, gx_device)
#define dev_t_proc_get_band(proc, dev_t)\
int proc(dev_t *dev, int y, int *band_start)
#define dev_proc_get_band(proc)\
dev_t_proc_get_band(proc, gx_device)
#define dev_t_proc_copy_rop(proc, dev_t)\
int proc(dev_t *dev,\
const byte *sdata, int sourcex, uint sraster, gx_bitmap_id id,\
const gx_color_index *scolors,\
const gx_tile_bitmap *texture, const gx_color_index *tcolors,\
int x, int y, int width, int height,\
int phase_x, int phase_y, gs_logical_operation_t lop)
#define dev_proc_copy_rop(proc)\
dev_t_proc_copy_rop(proc, gx_device)
#define dev_t_proc_fill_path(proc, dev_t)\
int proc(dev_t *dev,\
const gs_imager_state *pis, gx_path *ppath,\
const gx_fill_params *params,\
const gx_drawing_color *pdcolor, const gx_clip_path *pcpath)
#define dev_proc_fill_path(proc)\
dev_t_proc_fill_path(proc, gx_device)
#define dev_t_proc_stroke_path(proc, dev_t)\
int proc(dev_t *dev,\
const gs_imager_state *pis, gx_path *ppath,\
const gx_stroke_params *params,\
const gx_drawing_color *pdcolor, const gx_clip_path *pcpath)
#define dev_proc_stroke_path(proc)\
dev_t_proc_stroke_path(proc, gx_device)
#define dev_t_proc_fill_mask(proc, dev_t)\
int proc(dev_t *dev,\
const byte *data, int data_x, int raster, gx_bitmap_id id,\
int x, int y, int width, int height,\
const gx_drawing_color *pdcolor, int depth,\
gs_logical_operation_t lop, const gx_clip_path *pcpath)
#define dev_proc_fill_mask(proc)\
dev_t_proc_fill_mask(proc, gx_device)
#define dev_t_proc_fill_trapezoid(proc, dev_t)\
int proc(dev_t *dev,\
const gs_fixed_edge *left, const gs_fixed_edge *right,\
fixed ybot, fixed ytop, bool swap_axes,\
const gx_drawing_color *pdcolor, gs_logical_operation_t lop)
#define dev_proc_fill_trapezoid(proc)\
dev_t_proc_fill_trapezoid(proc, gx_device)
#define dev_t_proc_fill_parallelogram(proc, dev_t)\
int proc(dev_t *dev,\
fixed px, fixed py, fixed ax, fixed ay, fixed bx, fixed by,\
const gx_drawing_color *pdcolor, gs_logical_operation_t lop)
#define dev_proc_fill_parallelogram(proc)\
dev_t_proc_fill_parallelogram(proc, gx_device)
#define dev_t_proc_fill_triangle(proc, dev_t)\
int proc(dev_t *dev,\
fixed px, fixed py, fixed ax, fixed ay, fixed bx, fixed by,\
const gx_drawing_color *pdcolor, gs_logical_operation_t lop)
#define dev_proc_fill_triangle(proc)\
dev_t_proc_fill_triangle(proc, gx_device)
#define dev_t_proc_draw_thin_line(proc, dev_t)\
int proc(dev_t *dev,\
fixed fx0, fixed fy0, fixed fx1, fixed fy1,\
const gx_drawing_color *pdcolor, gs_logical_operation_t lop)
#define dev_proc_draw_thin_line(proc)\
dev_t_proc_draw_thin_line(proc, gx_device)
#define dev_t_proc_begin_image(proc, dev_t)\
int proc(dev_t *dev,\
const gs_imager_state *pis, const gs_image_t *pim,\
gs_image_format_t format, const gs_int_rect *prect,\
const gx_drawing_color *pdcolor, const gx_clip_path *pcpath,\
gs_memory_t *memory, gx_image_enum_common_t **pinfo)
#define dev_proc_begin_image(proc)\
dev_t_proc_begin_image(proc, gx_device)
#define dev_t_proc_image_data(proc, dev_t)\
int proc(dev_t *dev,\
gx_image_enum_common_t *info, const byte **planes, int data_x,\
uint raster, int height)
#define dev_proc_image_data(proc)\
dev_t_proc_image_data(proc, gx_device)
#define dev_t_proc_end_image(proc, dev_t)\
int proc(dev_t *dev,\
gx_image_enum_common_t *info, bool draw_last)
#define dev_proc_end_image(proc)\
dev_t_proc_end_image(proc, gx_device)
#define dev_t_proc_strip_tile_rectangle(proc, dev_t)\
int proc(dev_t *dev,\
const gx_strip_bitmap *tiles, int x, int y, int width, int height,\
gx_color_index color0, gx_color_index color1,\
int phase_x, int phase_y)
#define dev_proc_strip_tile_rectangle(proc)\
dev_t_proc_strip_tile_rectangle(proc, gx_device)
#define dev_t_proc_strip_copy_rop(proc, dev_t)\
int proc(dev_t *dev,\
const byte *sdata, int sourcex, uint sraster, gx_bitmap_id id,\
const gx_color_index *scolors,\
const gx_strip_bitmap *textures, const gx_color_index *tcolors,\
int x, int y, int width, int height,\
int phase_x, int phase_y, gs_logical_operation_t lop)
#define dev_proc_strip_copy_rop(proc)\
dev_t_proc_strip_copy_rop(proc, gx_device)
#define dev_t_proc_get_clipping_box(proc, dev_t)\
void proc(dev_t *dev, gs_fixed_rect *pbox)
#define dev_proc_get_clipping_box(proc)\
dev_t_proc_get_clipping_box(proc, gx_device)
#define dev_t_proc_begin_typed_image(proc, dev_t)\
int proc(dev_t *dev,\
const gs_imager_state *pis, const gs_matrix *pmat,\
const gs_image_common_t *pim, const gs_int_rect *prect,\
const gx_drawing_color *pdcolor, const gx_clip_path *pcpath,\
gs_memory_t *memory, gx_image_enum_common_t **pinfo)
#define dev_proc_begin_typed_image(proc)\
dev_t_proc_begin_typed_image(proc, gx_device)
#define dev_t_proc_get_bits_rectangle(proc, dev_t)\
int proc(dev_t *dev, const gs_int_rect *prect,\
gs_get_bits_params_t *params, gs_int_rect **unread)
#define dev_proc_get_bits_rectangle(proc)\
dev_t_proc_get_bits_rectangle(proc, gx_device)
#define dev_t_proc_map_color_rgb_alpha(proc, dev_t)\
int proc(dev_t *dev,\
gx_color_index color, gx_color_value rgba[4])
#define dev_proc_map_color_rgb_alpha(proc)\
dev_t_proc_map_color_rgb_alpha(proc, gx_device)
#define dev_t_proc_create_compositor(proc, dev_t)\
int proc(dev_t *dev,\
gx_device **pcdev, const gs_composite_t *pcte,\
gs_imager_state *pis, gs_memory_t *memory)
#define dev_proc_create_compositor(proc)\
dev_t_proc_create_compositor(proc, gx_device)\
#define dev_t_proc_get_hardware_params(proc, dev_t)\
int proc(dev_t *dev, gs_param_list *plist)
#define dev_proc_get_hardware_params(proc)\
dev_t_proc_get_hardware_params(proc, gx_device)
#define dev_t_proc_finish_copydevice(proc, dev_t)\
int proc(dev_t *dev, const gx_device *from_dev)
#define dev_proc_finish_copydevice(proc)\
dev_t_proc_finish_copydevice(proc, gx_device)
#define dev_t_proc_begin_transparency_group(proc, dev_t)\
int proc(gx_device *dev,\
const gs_transparency_group_params_t *ptgp,\
const gs_rect *pbbox,\
gs_imager_state *pis,\
gs_transparency_state_t **ppts,\
gs_memory_t *mem)
#define dev_proc_begin_transparency_group(proc)\
dev_t_proc_begin_transparency_group(proc, gx_device)
#define dev_t_proc_end_transparency_group(proc, dev_t)\
int proc(gx_device *dev,\
gs_imager_state *pis,\
gs_transparency_state_t **ppts)
#define dev_proc_end_transparency_group(proc)\
dev_t_proc_end_transparency_group(proc, gx_device)
#define dev_t_proc_begin_transparency_mask(proc, dev_t)\
int proc(gx_device *dev,\
const gx_transparency_mask_params_t *ptmp,\
const gs_rect *pbbox,\
gs_imager_state *pis,\
gs_transparency_state_t **ppts,\
gs_memory_t *mem)
#define dev_proc_begin_transparency_mask(proc)\
dev_t_proc_begin_transparency_mask(proc, gx_device)
#define dev_t_proc_end_transparency_mask(proc, dev_t)\
int proc(gx_device *dev,\
gs_transparency_mask_t **pptm)
#define dev_proc_end_transparency_mask(proc)\
dev_t_proc_end_transparency_mask(proc, gx_device)
#define dev_t_proc_discard_transparency_layer(proc, dev_t)\
int proc(gx_device *dev,\
gs_transparency_state_t **ppts)
#define dev_proc_discard_transparency_layer(proc)\
dev_t_proc_discard_transparency_layer(proc, gx_device)
typedef enum {
pattern_manage__can_accum,
pattern_manage__start_accum,
pattern_manage__finish_accum,
pattern_manage__load,
pattern_manage__shading_area
} pattern_manage_t;
#define dev_t_proc_pattern_manage(proc, dev_t)\
int proc(gx_device *pdev, gx_bitmap_id id,\
gs_pattern1_instance_t *pinst, pattern_manage_t function)
#define dev_proc_pattern_manage(proc)\
dev_t_proc_pattern_manage(proc, gx_device)
#define dev_t_proc_fill_rectangle_hl_color(proc, dev_t)\
int proc(dev_t *dev, const gs_fixed_rect *rect, \
const gs_imager_state *pis, const gx_drawing_color *pdcolor, \
const gx_clip_path *pcpath)
#define dev_proc_fill_rectangle_hl_color(proc)\
dev_t_proc_fill_rectangle_hl_color(proc, gx_device)
#define dev_t_proc_include_color_space(proc, dev_t)\
int proc(dev_t *dev, gs_color_space *cspace, const byte *res_name, int name_length)
#define dev_proc_include_color_space(proc)\
dev_t_proc_include_color_space(proc, gx_device)
typedef struct gs_fill_attributes_s {
const gs_fixed_rect *clip;
bool swap_axes;
const gx_device_halftone *ht;
gs_logical_operation_t lop;
fixed ystart, yend;
} gs_fill_attributes;
#define dev_t_proc_fill_linear_color_scanline(proc, dev_t)\
int proc(dev_t *dev, const gs_fill_attributes *fa,\
int i, int j, int w, \
const frac31 *c0, \
const int32_t *c0_f, \
const int32_t *cg_num, \
int32_t cg_den )
#define dev_proc_fill_linear_color_scanline(proc)\
dev_t_proc_fill_linear_color_scanline(proc, gx_device)
#define dev_t_proc_fill_linear_color_trapezoid(proc, dev_t)\
int proc(dev_t *dev, const gs_fill_attributes *fa,\
const gs_fixed_point *p0, const gs_fixed_point *p1,\
const gs_fixed_point *p2, const gs_fixed_point *p3,\
const frac31 *c0, const frac31 *c1,\
const frac31 *c2, const frac31 *c3)
#define dev_proc_fill_linear_color_trapezoid(proc)\
dev_t_proc_fill_linear_color_trapezoid(proc, gx_device)
#define dev_t_proc_fill_linear_color_triangle(proc, dev_t)\
int proc(dev_t *dev, const gs_fill_attributes *fa,\
const gs_fixed_point *p0, const gs_fixed_point *p1,\
const gs_fixed_point *p2,\
const frac31 *c0, const frac31 *c1, const frac31 *c2)
#define dev_proc_fill_linear_color_triangle(proc)\
dev_t_proc_fill_linear_color_triangle(proc, gx_device)
#define dev_t_proc_update_spot_equivalent_colors(proc, dev_t)\
int proc(dev_t *dev, const gs_state * pgs)
#define dev_proc_update_spot_equivalent_colors(proc)\
dev_t_proc_update_spot_equivalent_colors(proc, gx_device)
#define gx_device_proc_struct(dev_t)\
{	dev_t_proc_open_device((*open_device), dev_t);\
dev_t_proc_get_initial_matrix((*get_initial_matrix), dev_t);\
dev_t_proc_sync_output((*sync_output), dev_t);\
dev_t_proc_output_page((*output_page), dev_t);\
dev_t_proc_close_device((*close_device), dev_t);\
dev_t_proc_map_rgb_color((*map_rgb_color), dev_t);\
dev_t_proc_map_color_rgb((*map_color_rgb), dev_t);\
dev_t_proc_fill_rectangle((*fill_rectangle), dev_t);\
dev_t_proc_tile_rectangle((*tile_rectangle), dev_t);\
dev_t_proc_copy_mono((*copy_mono), dev_t);\
dev_t_proc_copy_color((*copy_color), dev_t);\
dev_t_proc_draw_line((*obsolete_draw_line), dev_t);\
dev_t_proc_get_bits((*get_bits), dev_t);\
dev_t_proc_get_params((*get_params), dev_t);\
dev_t_proc_put_params((*put_params), dev_t);\
dev_t_proc_map_cmyk_color((*map_cmyk_color), dev_t);\
dev_t_proc_get_xfont_procs((*get_xfont_procs), dev_t);\
dev_t_proc_get_xfont_device((*get_xfont_device), dev_t);\
dev_t_proc_map_rgb_alpha_color((*map_rgb_alpha_color), dev_t);\
dev_t_proc_get_page_device((*get_page_device), dev_t);\
dev_t_proc_get_alpha_bits((*get_alpha_bits), dev_t);\
dev_t_proc_copy_alpha((*copy_alpha), dev_t);\
dev_t_proc_get_band((*get_band), dev_t);\
dev_t_proc_copy_rop((*copy_rop), dev_t);\
dev_t_proc_fill_path((*fill_path), dev_t);\
dev_t_proc_stroke_path((*stroke_path), dev_t);\
dev_t_proc_fill_mask((*fill_mask), dev_t);\
dev_t_proc_fill_trapezoid((*fill_trapezoid), dev_t);\
dev_t_proc_fill_parallelogram((*fill_parallelogram), dev_t);\
dev_t_proc_fill_triangle((*fill_triangle), dev_t);\
dev_t_proc_draw_thin_line((*draw_thin_line), dev_t);\
dev_t_proc_begin_image((*begin_image), dev_t);\
dev_t_proc_image_data((*image_data), dev_t);\
dev_t_proc_end_image((*end_image), dev_t);\
dev_t_proc_strip_tile_rectangle((*strip_tile_rectangle), dev_t);\
dev_t_proc_strip_copy_rop((*strip_copy_rop), dev_t);\
dev_t_proc_get_clipping_box((*get_clipping_box), dev_t);\
dev_t_proc_begin_typed_image((*begin_typed_image), dev_t);\
dev_t_proc_get_bits_rectangle((*get_bits_rectangle), dev_t);\
dev_t_proc_map_color_rgb_alpha((*map_color_rgb_alpha), dev_t);\
dev_t_proc_create_compositor((*create_compositor), dev_t);\
dev_t_proc_get_hardware_params((*get_hardware_params), dev_t);\
dev_t_proc_text_begin((*text_begin), dev_t);\
dev_t_proc_finish_copydevice((*finish_copydevice), dev_t);\
dev_t_proc_begin_transparency_group((*begin_transparency_group), dev_t);\
dev_t_proc_end_transparency_group((*end_transparency_group), dev_t);\
dev_t_proc_begin_transparency_mask((*begin_transparency_mask), dev_t);\
dev_t_proc_end_transparency_mask((*end_transparency_mask), dev_t);\
dev_t_proc_discard_transparency_layer((*discard_transparency_layer), dev_t);\
dev_t_proc_get_color_mapping_procs((*get_color_mapping_procs), dev_t); \
dev_t_proc_get_color_comp_index((*get_color_comp_index), dev_t); \
dev_t_proc_encode_color((*encode_color), dev_t); \
dev_t_proc_decode_color((*decode_color), dev_t); \
dev_t_proc_pattern_manage((*pattern_manage), dev_t); \
dev_t_proc_fill_rectangle_hl_color((*fill_rectangle_hl_color), dev_t); \
dev_t_proc_include_color_space((*include_color_space), dev_t); \
dev_t_proc_fill_linear_color_scanline((*fill_linear_color_scanline), dev_t); \
dev_t_proc_fill_linear_color_trapezoid((*fill_linear_color_trapezoid), dev_t); \
dev_t_proc_fill_linear_color_triangle((*fill_linear_color_triangle), dev_t); \
dev_t_proc_update_spot_equivalent_colors((*update_spot_equivalent_colors), dev_t); \
}
typedef struct gx_image_plane_s {
const byte *data;
int data_x;
uint raster;
} gx_image_plane_t;
#define gx_device_begin_image(dev, pis, pim, format, prect, pdcolor, pcpath, memory, pinfo)\
((*dev_proc(dev, begin_image))\
(dev, pis, pim, format, prect, pdcolor, pcpath, memory, pinfo))
#define gx_device_begin_typed_image(dev, pis, pmat, pim, prect, pdcolor, pcpath, memory, pinfo)\
((*dev_proc(dev, begin_typed_image))\
(dev, pis, pmat, pim, prect, pdcolor, pcpath, memory, pinfo))
int gx_image_data(gx_image_enum_common_t *info, const byte **planes,
int data_x, uint raster, int height);
int gx_image_plane_data(gx_image_enum_common_t *info,
const gx_image_plane_t *planes, int height);
int gx_image_plane_data_rows(gx_image_enum_common_t *info,
const gx_image_plane_t *planes, int height,
int *rows_used);
int gx_image_flush(gx_image_enum_common_t *info);
bool gx_image_planes_wanted(const gx_image_enum_common_t *info, byte *wanted);
int gx_image_end(gx_image_enum_common_t *info, bool draw_last);
#define gx_device_image_data(dev, info, planes, data_x, raster, height)\
gx_image_data(info, planes, data_x, raster, height)
#define gx_device_image_plane_data(dev, info, planes, height)\
gx_image_plane_data(info, planes, height)
#define gx_device_end_image(dev, info, draw_last)\
gx_image_end(info, draw_last)
#define gx_device_get_alpha_bits(dev, type)\
gx_default_get_alpha_bits(dev, type)
struct gx_device_procs_s gx_device_proc_struct(gx_device);
dev_proc_copy_mono(gx_copy_mono_unaligned);
dev_proc_copy_color(gx_copy_color_unaligned);
dev_proc_copy_alpha(gx_copy_alpha_unaligned);
struct gx_device_s {
gx_device_common;
};
extern_st(st_device);
struct_proc_finalize(gx_device_finalize);
#define public_st_device()	\
gs_public_st_complex_only(st_device, gx_device, "gx_device",\
0, gs_no_struct_enum_ptrs, gs_no_struct_reloc_ptrs, gx_device_finalize)
#define st_device_max_ptrs 0
gx_device *gx_device_enum_ptr(gx_device *);
gx_device *gx_device_reloc_ptr(gx_device *, gc_state_t *);
typedef dev_proc_map_rgb_color((*dev_proc_map_rgb_color_t));
typedef dev_proc_map_color_rgb((*dev_proc_map_color_rgb_t));
#define gx_device_forward_common\
gx_device_common;\
gx_device *target
typedef struct gx_device_forward_s {
gx_device_forward_common;
} gx_device_forward;
extern_st(st_device_forward);
#define public_st_device_forward()	\
gs_public_st_complex_only(st_device_forward, gx_device_forward,\
"gx_device_forward", 0, device_forward_enum_ptrs,\
device_forward_reloc_ptrs, gx_device_finalize)
#define st_device_forward_max_ptrs (st_device_max_ptrs + 1)
#ifndef gx_device_null_DEFINED
#  define gx_device_null_DEFINED
typedef struct gx_device_null_s gx_device_null;
#endif
struct gx_device_null_s {
gx_device_forward_common;
};
extern const gx_device_null gs_null_device;
#define gx_device_is_null(dev)\
((dev)->dname == gs_null_device.dname)
extern_st(st_device_null);
#define public_st_device_null()	\
gs_public_st_complex_only(st_device_null, gx_device_null,\
"gx_device_null", 0, device_forward_enum_ptrs,\
device_forward_reloc_ptrs, gx_device_finalize)
#define st_device_null_max_ptrs st_device_forward_max_ptrs
void gx_device_init(gx_device * dev, const gx_device * proto,
gs_memory_t * mem, bool internal);
void gs_make_null_device(gx_device_null *dev_null, gx_device *target,
gs_memory_t *mem);
bool gs_is_null_device(gx_device *dev);
void gx_device_set_target(gx_device_forward *fdev, gx_device *target);
void gx_device_retain(gx_device *dev, bool retained);
uint gx_device_raster(const gx_device * dev, bool pad_to_word);
int gx_device_adjust_resolution(gx_device * dev, int actual_width, int actual_height, int fit);
void gx_device_set_margins(gx_device * dev, const float *margins  ,
bool move_origin);
void gx_device_set_width_height(gx_device * dev, int width, int height);
void gx_device_set_resolution(gx_device * dev, floatp x_dpi, floatp y_dpi);
void gx_device_set_media_size(gx_device * dev, floatp media_width, floatp media_height);
#define gx_device_set_page_size(dev, w, h)\
gx_device_set_media_size(dev, w, h)
void gx_set_device_only(gs_state *, gx_device *);
int gs_closedevice(gx_device *);
void gx_device_free_local(gx_device *);
#define dev_type_proc_initialize(proc)\
int proc(gx_device *)
typedef struct gx_device_type_s {
gs_memory_type_ptr_t stype;
dev_type_proc_initialize((*initialize));
} gx_device_type;
#define device_type(dtname, stype, initproc)\
private dev_type_proc_initialize(initproc);\
const gx_device_type dtname = { &stype, initproc }
#endif