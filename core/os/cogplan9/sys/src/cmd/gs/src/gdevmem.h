#ifndef gdevmem_INCLUDED
#  define gdevmem_INCLUDED
#include "gxbitops.h"
#define declare_scan_ptr(ptr)\
DECLARE_SCAN_PTR_VARS(ptr, chunk *, draster)
#define DECLARE_SCAN_PTR_VARS(ptr, ptype, draster)\
register ptype ptr;\
uint draster
#define setup_rect(ptr)\
SETUP_RECT_VARS(ptr, chunk *, draster)
#define SETUP_RECT_VARS(ptr, ptype, draster)\
draster = mdev->raster;\
ptr = (ptype)(scan_line_base(mdev, y) +\
(x_to_byte(x) & -chunk_align_bytes))
dev_proc_get_initial_matrix(mem_get_initial_matrix);
dev_proc_close_device(mem_close);
#define declare_mem_map_procs(map_rgb_color, map_color_rgb)\
private dev_proc_map_rgb_color(map_rgb_color);\
private dev_proc_map_color_rgb(map_color_rgb)
#define declare_mem_procs(copy_mono, copy_color, fill_rectangle)\
private dev_proc_copy_mono(copy_mono);\
private dev_proc_copy_color(copy_color);\
private dev_proc_fill_rectangle(fill_rectangle)
dev_proc_draw_thin_line(mem_draw_thin_line);
dev_proc_open_device(mem_open);
dev_proc_get_bits_rectangle(mem_get_bits_rectangle);
#if arch_is_big_endian
#  define mem_word_get_bits_rectangle mem_get_bits_rectangle
#else
dev_proc_get_bits_rectangle(mem_word_get_bits_rectangle);
#endif
dev_proc_map_rgb_color(mem_mapped_map_rgb_color);
dev_proc_map_color_rgb(mem_mapped_map_color_rgb);
dev_proc_strip_copy_rop(mem_default_strip_copy_rop);
#define max_value_gray(rgb_depth, gray_depth)\
(gray_depth ? (1 << gray_depth) - 1 : max_value_rgb(rgb_depth, 0))
#define max_value_rgb(rgb_depth, gray_depth)\
(rgb_depth >= 8 ? 255 : rgb_depth == 4 ? 15 : rgb_depth == 2 ? 3 :\
rgb_depth == 1 ? 1 : (1 << gray_depth) - 1)
#define mem_full_alpha_device(name, rgb_depth, gray_depth, open, map_rgb_color, map_color_rgb, copy_mono, copy_color, fill_rectangle, map_cmyk_color, copy_alpha, strip_tile_rectangle, strip_copy_rop, get_bits_rectangle)\
{	std_device_dci_body(gx_device_memory, 0, name,\
0, 0, 72, 72,\
(rgb_depth ? 3 : 0) + (gray_depth ? 1 : 0),	\
rgb_depth + gray_depth,	\
max_value_gray(rgb_depth, gray_depth),	\
max_value_rgb(rgb_depth, gray_depth),	\
max_value_gray(rgb_depth, gray_depth) + 1, \
max_value_rgb(rgb_depth, gray_depth) + 1 \
),\
{	open,			\
mem_get_initial_matrix,\
gx_default_sync_output,\
gx_default_output_page,\
mem_close,\
map_rgb_color,		\
map_color_rgb,		\
fill_rectangle,		\
gx_default_tile_rectangle,\
copy_mono,		\
copy_color,		\
gx_default_draw_line,\
gx_default_get_bits,\
gx_default_get_params,\
gx_default_put_params,\
map_cmyk_color,		\
gx_forward_get_xfont_procs,\
gx_forward_get_xfont_device,\
gx_default_map_rgb_alpha_color,\
gx_forward_get_page_device,\
gx_default_get_alpha_bits,	\
copy_alpha,		\
gx_default_get_band,\
gx_default_copy_rop,\
gx_default_fill_path,\
gx_default_stroke_path,\
gx_default_fill_mask,\
gx_default_fill_trapezoid,\
gx_default_fill_parallelogram,\
gx_default_fill_triangle,\
mem_draw_thin_line,	\
gx_default_begin_image,\
gx_default_image_data,\
gx_default_end_image,\
strip_tile_rectangle,	\
strip_copy_rop,		\
gx_default_get_clipping_box,\
gx_default_begin_typed_image,\
get_bits_rectangle,	\
gx_default_map_color_rgb_alpha,\
gx_default_create_compositor,\
gx_default_get_hardware_params,\
gx_default_text_begin,\
gx_default_finish_copydevice\
},\
0,			\
mem_device_init_private	\
}
#define mem_full_device(name, rgb_depth, gray_depth, open, map_rgb_color, map_color_rgb, copy_mono, copy_color, fill_rectangle, map_cmyk_color, strip_tile_rectangle, strip_copy_rop, get_bits_rectangle)\
mem_full_alpha_device(name, rgb_depth, gray_depth, open, map_rgb_color,\
map_color_rgb, copy_mono, copy_color, fill_rectangle,\
map_cmyk_color, gx_default_copy_alpha,\
strip_tile_rectangle, strip_copy_rop,\
get_bits_rectangle)
#define mem_device(name, rgb_depth, gray_depth, map_rgb_color, map_color_rgb, copy_mono, copy_color, fill_rectangle, strip_copy_rop)\
mem_full_device(name, rgb_depth, gray_depth, mem_open, map_rgb_color,\
map_color_rgb, copy_mono, copy_color, fill_rectangle,\
gx_default_map_cmyk_color, gx_default_strip_tile_rectangle,\
strip_copy_rop, mem_get_bits_rectangle)
void mem_swap_byte_rect(byte *, uint, int, int, int, bool);
#define mem_copy_byte_rect(mdev, base, sourcex, sraster, x, y, w, h)\
bytes_copy_rectangle(scan_line_base(mdev, y) + x_to_byte(x),\
(mdev)->raster,\
base + x_to_byte(sourcex), sraster,\
x_to_byte(w), h)
extern const gx_device_memory mem_mono_device;
extern const gx_device_memory mem_mapped2_device;
extern const gx_device_memory mem_mapped4_device;
extern const gx_device_memory mem_mapped8_device;
extern const gx_device_memory mem_true16_device;
extern const gx_device_memory mem_true24_device;
extern const gx_device_memory mem_true32_device;
extern const gx_device_memory mem_true40_device;
extern const gx_device_memory mem_true48_device;
extern const gx_device_memory mem_true56_device;
extern const gx_device_memory mem_true64_device;
extern const gx_device_memory mem_planar_device;
dev_proc_strip_copy_rop(mem_mono_strip_copy_rop);
dev_proc_strip_copy_rop(mem_gray_strip_copy_rop);
dev_proc_strip_copy_rop(mem_gray8_rgb24_strip_copy_rop);
#if arch_is_big_endian
#  define mem_mono_word_device mem_mono_device
#  define mem_mapped2_word_device mem_mapped2_device
#  define mem_mapped4_word_device mem_mapped4_device
#  define mem_mapped8_word_device mem_mapped8_device
#  define mem_true24_word_device mem_true24_device
#  define mem_true32_word_device mem_true32_device
#  define mem_true40_word_device mem_true40_device
#  define mem_true48_word_device mem_true48_device
#  define mem_true56_word_device mem_true56_device
#  define mem_true64_word_device mem_true64_device
#else
extern const gx_device_memory mem_mono_word_device;
extern const gx_device_memory mem_mapped2_word_device;
extern const gx_device_memory mem_mapped4_word_device;
extern const gx_device_memory mem_mapped8_word_device;
extern const gx_device_memory mem_true24_word_device;
extern const gx_device_memory mem_true32_word_device;
extern const gx_device_memory mem_true40_word_device;
extern const gx_device_memory mem_true48_word_device;
extern const gx_device_memory mem_true56_word_device;
extern const gx_device_memory mem_true64_word_device;
#endif
extern const gs_const_string mem_mono_b_w_palette;
extern const gs_const_string mem_mono_w_b_palette;
#endif