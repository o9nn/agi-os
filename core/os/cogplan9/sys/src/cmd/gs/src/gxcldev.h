#ifndef gxcldev_INCLUDED
#  define gxcldev_INCLUDED
#include "gxclist.h"
#include "gsropt.h"
#include "gxht.h"
#include "gxtmap.h"
#include "gxdht.h"
#include "strimpl.h"
#include "scfx.h"
#include "srlx.h"
#include "gsdcolor.h"
#define cmd_compress_rle 1
#define cmd_compress_cfe 2
#define cmd_mask_compress_any\
((1 << cmd_compress_rle) | (1 << cmd_compress_cfe))
void clist_rle_init(stream_RLE_state *ss);
void clist_rld_init(stream_RLD_state *ss);
void clist_cfe_init(stream_CFE_state *ss, int width, gs_memory_t *mem);
void clist_cfd_init(stream_CFD_state *ss, int width, int height,
gs_memory_t *mem);
typedef enum {
cmd_op_misc = 0x00,
cmd_opv_end_run = 0x00,
cmd_opv_set_tile_size = 0x01,
cmd_opv_set_tile_phase = 0x02,
cmd_opv_set_tile_bits = 0x03,
cmd_opv_set_bits = 0x04,
cmd_opv_set_tile_color = 0x05,
cmd_opv_set_misc = 0x06,
#define cmd_set_misc_lop (0 << 6)
#define cmd_set_misc_data_x (1 << 6)
#define cmd_set_misc_map (2 << 6)
#define cmd_set_misc_halftone (3 << 6)
cmd_opv_enable_lop = 0x07,
cmd_opv_disable_lop = 0x08,
cmd_opv_end_page = 0x0b,
cmd_opv_delta_color0 = 0x0c,
cmd_opv_delta_color1 = 0x0d,
cmd_opv_set_copy_color = 0x0e,
cmd_opv_set_copy_alpha = 0x0f,
cmd_op_set_color0 = 0x10,
#define cmd_no_color_index 15
cmd_op_set_color1 = 0x20,
cmd_op_fill_rect = 0x30,
cmd_op_fill_rect_short = 0x40,
cmd_op_fill_rect_tiny = 0x50,
cmd_op_tile_rect = 0x60,
cmd_op_tile_rect_short = 0x70,
cmd_op_tile_rect_tiny = 0x80,
cmd_op_copy_mono = 0x90,
#define cmd_copy_ht_color 4
#define cmd_copy_use_tile 8
cmd_op_copy_color_alpha = 0xa0,
cmd_op_delta_tile_index = 0xb0,
cmd_op_set_tile_index = 0xc0
} gx_cmd_op;
#define cmd_op_name_strings\
"(misc)", "set_color[0]", "set_color[1]", "fill_rect",\
"fill_rect_short", "fill_rect_tiny", "tile_rect", "tile_rect_short",\
"tile_rect_tiny", "copy_mono", "copy_color_alpha", "delta_tile_index",\
"set_tile_index", "(misc2)", "(segment)", "(path)"
#define cmd_misc_op_name_strings\
"end_run", "set_tile_size", "set_tile_phase", "set_tile_bits",\
"set_bits", "set_tile_color", "set_misc", "enable_lop",\
"disable_lop", "set_ht_order", "set_ht_data", "end_page",\
"delta2_color0", "delta2_color1", "set_copy_color", "set_copy_alpha",
#ifdef DEBUG
extern const char *const cmd_op_names[16];
extern const char *const *const cmd_sub_op_names[16];
#endif
#define cmd_max_intsize(siz)\
(((siz) * 8 + 6) / 7)
#define cmd_largest_size\
(2 + (1 + cmd_max_dash) * sizeof(float))
typedef struct {
int x, y, width, height;
} gx_cmd_rect;
typedef struct {
byte dx, dwidth, dy, dheight;
} gx_cmd_rect_short;
#define cmd_min_short (-128)
#define cmd_max_short 127
#define cmd_min_dw_tiny (-4)
#define cmd_max_dw_tiny 3
typedef struct {
unsigned dx:4;
unsigned dy:4;
} gx_cmd_rect_tiny;
#define cmd_min_dxy_tiny (-8)
#define cmd_max_dxy_tiny 7
#define cmd_depth_to_code(d)    ((d) > 0xf ? 0x10 | ((d) >> 3) : (d))
#define cmd_code_to_depth(v)    \
(((v) & 0x10) != 0 ? ((v) & 0xf) << 3 : (v) & 0xf)
#define cmd_max_short_width_bytes 6
#define cmd_max_short_width_bits (cmd_max_short_width_bytes * 8)
uint clist_bitmap_bytes(uint width_bits, uint height,
int compression_mask,
uint * width_bytes, uint * raster);
typedef struct cmd_block_s {
int band_min, band_max;
#define cmd_band_end (-1)
long pos;
} cmd_block;
struct gx_clist_state_s {
gx_color_index colors[2];
gx_device_color_saved sdc;
uint tile_index;
gx_bitmap_id tile_id;
#define cls_has_tile_id(cldev, pcls, tid, offset_temp)\
((pcls)->tile_id == (tid) &&\
(offset_temp = cldev->tile_table[(pcls)->tile_index].offset) != 0 &&\
((tile_slot *)(cldev->data + offset_temp))->id == (tid))
gs_int_point tile_phase;
gx_color_index tile_colors[2];
gx_cmd_rect rect;
gs_logical_operation_t lop;
short lop_enabled;
short clip_enabled;
bool color_is_alpha;
uint known;
#define tile_params_known (1<<15)
#define begin_image_known (1<<14)
#define initial_known 0x3fff
cmd_list list;
ulong cost;
gx_colors_used_t colors_used;
};
#define cls_initial_values\
{ gx_no_color_index, gx_no_color_index },\
{ gx_dc_type_none },\
0, gx_no_bitmap_id,\
{ 0, 0 }, { gx_no_color_index, gx_no_color_index },\
{ 0, 0, 0, 0 }, lop_default, 0, 0, 0, initial_known,\
{ 0, 0 }, 0, { 0 }
#define cbuf_size 4096
dev_proc_fill_rectangle(clist_fill_rectangle);
dev_proc_copy_mono(clist_copy_mono);
dev_proc_copy_color(clist_copy_color);
dev_proc_copy_alpha(clist_copy_alpha);
dev_proc_strip_tile_rectangle(clist_strip_tile_rectangle);
dev_proc_strip_copy_rop(clist_strip_copy_rop);
dev_proc_fill_mask(clist_fill_mask);
dev_proc_begin_typed_image(clist_begin_typed_image);
dev_proc_create_compositor(clist_create_compositor);
dev_proc_get_bits_rectangle(clist_get_bits_rectangle);
int clist_VMerror_recover(gx_device_clist_writer *, int);
int clist_VMerror_recover_flush(gx_device_clist_writer *, int);
int cmd_put_params(gx_device_clist_writer *, gs_param_list *);
#ifdef DEBUG
int cmd_count_op(int op, uint size);
void cmd_uncount_op(int op, uint size);
void cmd_print_stats(void);
#  define cmd_count_add1(v) (v++)
#else
#  define cmd_count_op(op, size) (op)
#  define cmd_uncount_op(op, size) DO_NOTHING
#  define cmd_count_add1(v) DO_NOTHING
#endif
byte *cmd_put_list_op(gx_device_clist_writer * cldev, cmd_list * pcl, uint size);
#ifdef DEBUG
byte *cmd_put_op(gx_device_clist_writer * cldev, gx_clist_state * pcls, uint size);
#else
#  define cmd_put_op(cldev, pcls, size)\
cmd_put_list_op(cldev, &(pcls)->list, size)
#endif
#define set_cmd_put_op(dp, cldev, pcls, op, csize)\
( (dp = cmd_put_op(cldev, pcls, csize)) == 0 ?\
(cldev)->error_code :\
(*dp = cmd_count_op(op, csize), 0) )
byte *cmd_put_range_op(gx_device_clist_writer * cldev, int band_min,
int band_max, uint size);
#define cmd_put_all_op(cldev, size)\
cmd_put_range_op(cldev, 0, (cldev)->nbands - 1, size)
#define set_cmd_put_range_op(dp, cldev, op, bmin, bmax, csize)\
( (dp = cmd_put_range_op(cldev, bmin, bmax, csize)) == 0 ?\
(cldev)->error_code :\
(*dp = cmd_count_op(op, csize), 0) )
#define set_cmd_put_all_op(dp, cldev, op, csize)\
set_cmd_put_range_op(dp, cldev, op, 0, (cldev)->nbands - 1, csize)
#define cmd_shorten_list_op(cldev, pcls, delta)\
((pcls)->tail->size -= (delta), (cldev)->cnext -= (delta))
#define cmd_shorten_op(cldev, pcls, delta)\
cmd_shorten_list_op(cldev, &(pcls)->list, delta)
int cmd_write_buffer(gx_device_clist_writer * cldev, byte cmd_end);
int clist_end_page(gx_device_clist_writer *);
int cmd_size_w(uint);
#define w1byte(w) (!((w) & ~0x7f))
#define w2byte(w) (!((w) & ~0x3fff))
#define cmd_sizew(w)\
(w1byte(w) ? 1 : w2byte(w) ? 2 : cmd_size_w((uint)(w)))
#define cmd_size2w(wx,wy)\
(w1byte((wx) | (wy)) ? 2 :\
cmd_size_w((uint)(wx)) + cmd_size_w((uint)(wy)))
#define cmd_sizexy(xy) cmd_size2w((xy).x, (xy).y)
#define cmd_sizew_max ((sizeof(uint) * 8 + 6) / 7)
byte *cmd_put_w(uint, byte *);
#define cmd_putw(w,dp)\
(w1byte(w) ? (*dp = w, ++dp) :\
w2byte(w) ? (*dp = (w) | 0x80, dp[1] = (w) >> 7, dp += 2) :\
(dp = cmd_put_w((uint)(w), dp)))
#define cmd_put2w(wx,wy,dp)\
(w1byte((wx) | (wy)) ? (dp[0] = (wx), dp[1] = (wy), dp += 2) :\
(dp = cmd_put_w((uint)(wy), cmd_put_w((uint)(wx), dp))))
#define cmd_putxy(xy,dp) cmd_put2w((xy).x, (xy).y, dp)
typedef struct {
byte set_op;
byte delta_op;
bool tile_color;
} clist_select_color_t;
extern const clist_select_color_t
clist_select_color0, clist_select_color1, clist_select_tile_color0,
clist_select_tile_color1;
int cmd_put_color(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const clist_select_color_t * select,
gx_color_index color, gx_color_index * pcolor);
extern const gx_color_index cmd_delta_offsets[];
#define cmd_set_color0(dev, pcls, color0)\
cmd_put_color(dev, pcls, &clist_select_color0, color0, &(pcls)->colors[0])
#define cmd_set_color1(dev, pcls, color1)\
cmd_put_color(dev, pcls, &clist_select_color1, color1, &(pcls)->colors[1])
int cmd_set_tile_colors(gx_device_clist_writer *cldev, gx_clist_state * pcls,
gx_color_index color0, gx_color_index color1);
int cmd_set_tile_phase(gx_device_clist_writer *cldev, gx_clist_state * pcls,
int px, int py);
int cmd_put_enable_lop(gx_device_clist_writer *, gx_clist_state *, int);
#define cmd_do_enable_lop(cldev, pcls, enable)\
( (pcls)->lop_enabled == ((enable) ^ 1) &&\
cmd_put_enable_lop(cldev, pcls, enable) < 0 ?\
(cldev)->error_code : 0 )
#define cmd_enable_lop(cldev, pcls)\
cmd_do_enable_lop(cldev, pcls, 1)
#define cmd_disable_lop(cldev, pcls)\
cmd_do_enable_lop(cldev, pcls, 0)
int cmd_put_enable_clip(gx_device_clist_writer *, gx_clist_state *, int);
#define cmd_do_enable_clip(cldev, pcls, enable)\
( (pcls)->clip_enabled == ((enable) ^ 1) &&\
cmd_put_enable_clip(cldev, pcls, enable) < 0 ?\
(cldev)->error_code : 0 )
#define cmd_enable_clip(cldev, pcls)\
cmd_do_enable_clip(cldev, pcls, 1)
#define cmd_disable_clip(cldev, pcls)\
cmd_do_enable_clip(cldev, pcls, 0)
int cmd_set_lop(gx_device_clist_writer *, gx_clist_state *,
gs_logical_operation_t);
int cmd_update_lop(gx_device_clist_writer *, gx_clist_state *,
gs_logical_operation_t);
#define FOR_RECTS_NO_ERROR\
BEGIN\
int yend = y + height;\
int band_height = cdev->page_band_height;\
\
\
if (cdev->permanent_error < 0)\
return (cdev->permanent_error);\
do {\
int band = y / band_height;\
gx_clist_state *pcls = cdev->states + band;\
int band_end = (band + 1) * band_height;\
\
height = min(band_end, yend) - y;\
#define FOR_RECTS\
BEGIN\
int yend = y + height;\
int band_height = cdev->page_band_height;\
int band_code;\
\
if (cdev->permanent_error < 0)\
return (cdev->permanent_error);\
do {\
int band = y / band_height;\
gx_clist_state *pcls = cdev->states + band;\
int band_end = (band + 1) * band_height;\
\
height = min(band_end, yend) - y;\
retry_rect:\
;
#define NEST_RECT    ++cdev->driver_call_nesting;
#define UNNEST_RECT  --cdev->driver_call_nesting
#define ERROR_RECT(code_value)\
BEGIN\
band_code = (code_value);\
if (1) goto error_in_rect;\
END
#define TRY_RECT\
BEGIN\
do
#define HANDLE_RECT_UNLESS(codevar, unless_clause)\
while (codevar < 0 &&\
(codevar = clist_VMerror_recover(cdev, codevar)) >= 0\
);\
if (codevar < 0 && !(unless_clause))\
ERROR_RECT(codevar);\
END
#define HANDLE_RECT(codevar)\
HANDLE_RECT_UNLESS(codevar, 0)
#define END_RECTS_ON_ERROR(retry_cleanup, is_error, after_recovering)\
continue;\
error_in_rect:\
if (cdev->error_is_retryable) {\
retry_cleanup;\
if ((is_error) &&\
cdev->driver_call_nesting == 0 &&\
(band_code =\
clist_VMerror_recover_flush(cdev, band_code)) >= 0 &&\
(after_recovering)\
)\
goto retry_rect;\
}\
if (1) return band_code;\
} while ((y += height) < yend);\
END
#define END_RECTS END_RECTS_ON_ERROR(DO_NOTHING, 1, 1)
#define END_RECTS_NO_ERROR\
} while ((y += height) < yend);\
END
int cmd_write_rect_cmd(gx_device_clist_writer * cldev, gx_clist_state * pcls,
int op, int x, int y, int width, int height);
#define decompress_elsewhere 0x100
#define decompress_spread 0x200
int cmd_put_bits(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const byte * data, uint width_bits, uint height,
uint raster, int op_size, int compression_mask,
byte ** pdp, uint * psize);
typedef enum {
cmd_map_transfer = 0,
cmd_map_transfer_0,
cmd_map_transfer_1,
cmd_map_transfer_2,
cmd_map_transfer_3,
cmd_map_black_generation,
cmd_map_undercolor_removal
} cmd_map_index;
typedef enum {
cmd_map_none = 0,
cmd_map_identity,
cmd_map_other
} cmd_map_contents;
int cmd_put_color_map(gx_device_clist_writer * cldev,
cmd_map_index map_index, int comp_num,
const gx_transfer_map * map, gs_id * pid);
int clist_change_tile(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const gx_strip_bitmap * tiles, int depth);
int clist_change_bits(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const gx_strip_bitmap * tiles, int depth);
int cmd_put_color_mapping(gx_device_clist_writer * cldev,
const gs_imager_state * pis);
int cmd_put_halftone(gx_device_clist_writer * cldev,
const gx_device_halftone * pdht);
typedef enum {
playback_action_render,
playback_action_setup
} clist_playback_action;
int clist_playback_band(clist_playback_action action,
gx_device_clist_reader *cdev,
stream *s, gx_device *target,
int x0, int y0, gs_memory_t *mem);
#endif