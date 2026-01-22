#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsrect.h"
#include "gsstruct.h"
#include "gxarith.h"
#include "gxdevice.h"
#include "gxgetbit.h"
#include "gxdevmem.h"
#include "gdevmem.h"
#include "gstrans.h"
public_st_device_memory();
private
ENUM_PTRS_WITH(device_memory_enum_ptrs, gx_device_memory *mptr)
{
return ENUM_USING(st_device_forward, vptr, sizeof(gx_device_forward), index - 3);
}
case 0: ENUM_RETURN((mptr->foreign_bits ? NULL : (void *)mptr->base));
case 1: ENUM_RETURN((mptr->foreign_line_pointers ? NULL : (void *)mptr->line_ptrs));
ENUM_STRING_PTR(2, gx_device_memory, palette);
ENUM_PTRS_END
private
RELOC_PTRS_WITH(device_memory_reloc_ptrs, gx_device_memory *mptr)
{
if (!mptr->foreign_bits) {
byte *base_old = mptr->base;
long reloc;
int y;
RELOC_PTR(gx_device_memory, base);
reloc = base_old - mptr->base;
for (y = 0; y < mptr->height; y++)
mptr->line_ptrs[y] -= reloc;
mptr->line_ptrs = (byte **) ((byte *) mptr->line_ptrs - reloc);
} else if (!mptr->foreign_line_pointers) {
RELOC_PTR(gx_device_memory, line_ptrs);
}
RELOC_CONST_STRING_PTR(gx_device_memory, palette);
RELOC_USING(st_device_forward, vptr, sizeof(gx_device_forward));
}
RELOC_PTRS_END
private const byte b_w_palette_string[6] = {
0xff, 0xff, 0xff, 0, 0, 0
};
const gs_const_string mem_mono_b_w_palette = {
b_w_palette_string, 6
};
private const byte w_b_palette_string[6] = {
0, 0, 0, 0xff, 0xff, 0xff
};
const gs_const_string mem_mono_w_b_palette = {
w_b_palette_string, 6
};
private const gx_device_memory *const mem_devices[65] = {
0, &mem_mono_device, &mem_mapped2_device, 0, &mem_mapped4_device,
0, 0, 0, &mem_mapped8_device,
0, 0, 0, 0, 0, 0, 0, &mem_true16_device,
0, 0, 0, 0, 0, 0, 0, &mem_true24_device,
0, 0, 0, 0, 0, 0, 0, &mem_true32_device,
0, 0, 0, 0, 0, 0, 0, &mem_true40_device,
0, 0, 0, 0, 0, 0, 0, &mem_true48_device,
0, 0, 0, 0, 0, 0, 0, &mem_true56_device,
0, 0, 0, 0, 0, 0, 0, &mem_true64_device
};
const gx_device_memory *
gdev_mem_device_for_bits(int bits_per_pixel)
{
return ((uint)bits_per_pixel > 64 ? (const gx_device_memory *)0 :
mem_devices[bits_per_pixel]);
}
private const gx_device_memory *const mem_word_devices[65] = {
0, &mem_mono_device, &mem_mapped2_word_device, 0, &mem_mapped4_word_device,
0, 0, 0, &mem_mapped8_word_device,
0, 0, 0, 0, 0, 0, 0, 0 ,
0, 0, 0, 0, 0, 0, 0, &mem_true24_word_device,
0, 0, 0, 0, 0, 0, 0, &mem_true32_word_device,
0, 0, 0, 0, 0, 0, 0, &mem_true40_word_device,
0, 0, 0, 0, 0, 0, 0, &mem_true48_word_device,
0, 0, 0, 0, 0, 0, 0, &mem_true56_word_device,
0, 0, 0, 0, 0, 0, 0, &mem_true64_word_device
};
const gx_device_memory *
gdev_mem_word_device_for_bits(int bits_per_pixel)
{
return ((uint)bits_per_pixel > 64 ? (const gx_device_memory *)0 :
mem_word_devices[bits_per_pixel]);
}
bool
gs_device_is_memory(const gx_device * dev)
{
int bits_per_pixel = dev->color_info.depth;
const gx_device_memory *mdproto;
if ((uint)bits_per_pixel > 64)
return false;
mdproto = mem_devices[bits_per_pixel];
if (mdproto != 0 && dev_proc(dev, draw_thin_line) == dev_proc(mdproto, draw_thin_line))
return true;
mdproto = mem_word_devices[bits_per_pixel];
return (mdproto != 0 && dev_proc(dev, draw_thin_line) == dev_proc(mdproto, draw_thin_line));
}
void
gs_make_mem_device(gx_device_memory * dev, const gx_device_memory * mdproto,
gs_memory_t * mem, int page_device, gx_device * target)
{
gx_device_init((gx_device *) dev, (const gx_device *)mdproto,
mem, true);
dev->stype = &st_device_memory;
switch (page_device) {
case -1:
set_dev_proc(dev, get_page_device, gx_default_get_page_device);
break;
case 1:
set_dev_proc(dev, get_page_device, gx_page_device_get_page_device);
break;
}
if (target == 0) {
if (dev->color_info.depth == 1) {
dev->cached_colors.black = 1;
dev->cached_colors.white = 0;
} else {
dev->cached_colors.black = 0;
dev->cached_colors.white = (1 << dev->color_info.depth) - 1;
}
} else {
gx_device_set_target((gx_device_forward *)dev, target);
gx_device_forward_color_procs((gx_device_forward *) dev);
gx_device_copy_color_procs((gx_device *)dev, target);
dev->cached_colors = target->cached_colors;
}
if (dev->color_info.depth == 1) {
gdev_mem_mono_set_inverted(dev,
(target == 0 ||
dev->color_info.polarity == GX_CINFO_POLARITY_SUBTRACTIVE));
}
check_device_separable((gx_device *)dev);
gx_device_fill_in_procs((gx_device *)dev);
}
void
gs_make_mem_mono_device(gx_device_memory * dev, gs_memory_t * mem,
gx_device * target)
{
gx_device_init((gx_device *)dev, (const gx_device *)&mem_mono_device,
mem, true);
set_dev_proc(dev, get_page_device, gx_default_get_page_device);
gx_device_set_target((gx_device_forward *)dev, target);
gdev_mem_mono_set_inverted(dev, true);
check_device_separable((gx_device *)dev);
gx_device_fill_in_procs((gx_device *)dev);
}
void
gdev_mem_mono_set_inverted(gx_device_memory * dev, bool black_is_1)
{
if (black_is_1)
dev->palette = mem_mono_b_w_palette;
else
dev->palette = mem_mono_w_b_palette;
}
ulong
gdev_mem_bits_size(const gx_device_memory * dev, int width, int height)
{
int num_planes = dev->num_planes;
gx_render_plane_t plane1;
const gx_render_plane_t *planes;
ulong size;
int pi;
if (num_planes)
planes = dev->planes;
else
planes = &plane1, plane1.depth = dev->color_info.depth, num_planes = 1;
for (size = 0, pi = 0; pi < num_planes; ++pi)
size += bitmap_raster(width * planes[pi].depth);
return ROUND_UP(size * height, ARCH_ALIGN_PTR_MOD);
}
ulong
gdev_mem_line_ptrs_size(const gx_device_memory * dev, int width, int height)
{
return (ulong)height * sizeof(byte *) * max(dev->num_planes, 1);
}
ulong
gdev_mem_data_size(const gx_device_memory * dev, int width, int height)
{
return gdev_mem_bits_size(dev, width, height) +
gdev_mem_line_ptrs_size(dev, width, height);
}
int
gdev_mem_max_height(const gx_device_memory * dev, int width, ulong size,
bool page_uses_transparency)
{
int height;
ulong max_height;
if (page_uses_transparency) {
max_height = size / (bitmap_raster(width
* dev->color_info.depth + ESTIMATED_PDF14_ROW_SPACE(width))
+ sizeof(byte *) * max(dev->num_planes, 1));
height = (int)min(max_height, max_int);
} else {
max_height = size /
(bitmap_raster(width * dev->color_info.depth) +
sizeof(byte *) * max(dev->num_planes, 1));
height = (int)min(max_height, max_int);
while (gdev_mem_data_size(dev, width, height) > size)
--height;
}
return height;
}
int
mem_open(gx_device * dev)
{
gx_device_memory *const mdev = (gx_device_memory *)dev;
if (mdev->num_planes)
return_error(gs_error_rangecheck);
return gdev_mem_open_scan_lines(mdev, dev->height);
}
int
gdev_mem_open_scan_lines(gx_device_memory *mdev, int setup_height)
{
bool line_pointers_adjacent = true;
if (setup_height < 0 || setup_height > mdev->height)
return_error(gs_error_rangecheck);
if (mdev->bitmap_memory != 0) {
ulong size = gdev_mem_bitmap_size(mdev);
if ((uint) size != size)
return_error(gs_error_limitcheck);
mdev->base = gs_alloc_bytes(mdev->bitmap_memory, (uint)size,
"mem_open");
if (mdev->base == 0)
return_error(gs_error_VMerror);
mdev->foreign_bits = false;
} else if (mdev->line_pointer_memory != 0) {
mdev->line_ptrs = (byte **)
gs_alloc_byte_array(mdev->line_pointer_memory, mdev->height,
sizeof(byte *) * max(mdev->num_planes, 1),
"gdev_mem_open_scan_lines");
if (mdev->line_ptrs == 0)
return_error(gs_error_VMerror);
mdev->foreign_line_pointers = false;
line_pointers_adjacent = false;
}
if (line_pointers_adjacent)
mdev->line_ptrs = (byte **)
(mdev->base + gdev_mem_bits_size(mdev, mdev->width, mdev->height));
mdev->raster = gdev_mem_raster(mdev);
return gdev_mem_set_line_ptrs(mdev, NULL, 0, NULL, setup_height);
}
int
gdev_mem_set_line_ptrs(gx_device_memory * mdev, byte * base, int raster,
byte **line_ptrs, int setup_height)
{
int num_planes = mdev->num_planes;
gx_render_plane_t plane1;
const gx_render_plane_t *planes;
byte **pline =
(line_ptrs ? (mdev->line_ptrs = line_ptrs) : mdev->line_ptrs);
byte *data =
(base ? (mdev->raster = raster, mdev->base = base) :
(raster = mdev->raster, mdev->base));
int pi;
if (num_planes) {
if (base && !mdev->plane_depth)
return_error(gs_error_rangecheck);
planes = mdev->planes;
} else {
planes = &plane1;
plane1.depth = mdev->color_info.depth;
num_planes = 1;
}
for (pi = 0; pi < num_planes; ++pi) {
int raster = bitmap_raster(mdev->width * planes[pi].depth);
byte **pptr = pline;
byte **pend = pptr + setup_height;
byte *scan_line = data;
while (pptr < pend) {
*pptr++ = scan_line;
scan_line += raster;
}
data += raster * mdev->height;
pline += setup_height;
}
return 0;
}
void
mem_get_initial_matrix(gx_device * dev, gs_matrix * pmat)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
pmat->xx = mdev->initial_matrix.xx;
pmat->xy = mdev->initial_matrix.xy;
pmat->yx = mdev->initial_matrix.yx;
pmat->yy = mdev->initial_matrix.yy;
pmat->tx = mdev->initial_matrix.tx;
pmat->ty = mdev->initial_matrix.ty;
}
int
mem_close(gx_device * dev)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
if (mdev->bitmap_memory != 0) {
gs_free_object(mdev->bitmap_memory, mdev->base, "mem_close");
mdev->base = 0;
} else if (mdev->line_pointer_memory != 0) {
gs_free_object(mdev->line_pointer_memory, mdev->line_ptrs,
"mem_close");
mdev->line_ptrs = 0;
}
return 0;
}
#undef chunk
#define chunk byte
int
mem_get_bits_rectangle(gx_device * dev, const gs_int_rect * prect,
gs_get_bits_params_t * params, gs_int_rect ** unread)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
gs_get_bits_options_t options = params->options;
int x = prect->p.x, w = prect->q.x - x, y = prect->p.y, h = prect->q.y - y;
if (options == 0) {
params->options =
(GB_ALIGN_STANDARD | GB_ALIGN_ANY) |
(GB_RETURN_COPY | GB_RETURN_POINTER) |
(GB_OFFSET_0 | GB_OFFSET_SPECIFIED | GB_OFFSET_ANY) |
(GB_RASTER_STANDARD | GB_RASTER_SPECIFIED | GB_RASTER_ANY) |
GB_PACKING_CHUNKY | GB_COLORS_NATIVE | GB_ALPHA_NONE;
return_error(gs_error_rangecheck);
}
if ((w <= 0) | (h <= 0)) {
if ((w | h) < 0)
return_error(gs_error_rangecheck);
return 0;
}
if (x < 0 || w > dev->width - x ||
y < 0 || h > dev->height - y
)
return_error(gs_error_rangecheck);
{
gs_get_bits_params_t copy_params;
byte *base = scan_line_base(mdev, y);
int code;
copy_params.options =
GB_COLORS_NATIVE | GB_PACKING_CHUNKY | GB_ALPHA_NONE |
(mdev->raster ==
bitmap_raster(mdev->width * mdev->color_info.depth) ?
GB_RASTER_STANDARD : GB_RASTER_SPECIFIED);
copy_params.raster = mdev->raster;
code = gx_get_bits_return_pointer(dev, x, h, params,
&copy_params, base);
if (code >= 0)
return code;
return gx_get_bits_copy(dev, x, w, h, params, &copy_params, base,
gx_device_raster(dev, true));
}
}
#if !arch_is_big_endian
void
mem_swap_byte_rect(byte * base, uint raster, int x, int w, int h, bool store)
{
int xbit = x & 31;
if (store) {
if (xbit + w > 64) {
if (xbit != 0)
mem_swap_byte_rect(base, raster, x, 1, h, false);
x += w - 1;
xbit = x & 31;
if (xbit == 31)
return;
w = 1;
}
}
{
byte *row = base + ((x >> 5) << 2);
int nw = (xbit + w + 31) >> 5;
int ny;
for (ny = h; ny > 0; row += raster, --ny) {
int nx = nw;
bits32 *pw = (bits32 *) row;
do {
bits32 w = *pw;
*pw++ = (w >> 24) + ((w >> 8) & 0xff00) +
((w & 0xff00) << 8) + (w << 24);
}
while (--nx);
}
}
}
int
mem_word_get_bits_rectangle(gx_device * dev, const gs_int_rect * prect,
gs_get_bits_params_t * params, gs_int_rect ** unread)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
byte *src;
uint dev_raster = gx_device_raster(dev, 1);
int x = prect->p.x;
int w = prect->q.x - x;
int y = prect->p.y;
int h = prect->q.y - y;
int bit_x, bit_w;
int code;
fit_fill_xywh(dev, x, y, w, h);
if (w <= 0 || h <= 0) {
x = y = w = h = 0;
}
bit_x = x * dev->color_info.depth;
bit_w = w * dev->color_info.depth;
src = scan_line_base(mdev, y);
mem_swap_byte_rect(src, dev_raster, bit_x, bit_w, h, false);
code = mem_get_bits_rectangle(dev, prect, params, unread);
mem_swap_byte_rect(src, dev_raster, bit_x, bit_w, h, false);
return code;
}
#endif
gx_color_index
mem_mapped_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
byte br = gx_color_value_to_byte(cv[0]);
register const byte *pptr = mdev->palette.data;
int cnt = mdev->palette.size;
const byte *which = 0;
int best = 256 * 3;
if (mdev->color_info.num_components != 1) {
byte bg = gx_color_value_to_byte(cv[1]);
byte bb = gx_color_value_to_byte(cv[2]);
while ((cnt -= 3) >= 0) {
register int diff = *pptr - br;
if (diff < 0)
diff = -diff;
if (diff < best) {
int dg = pptr[1] - bg;
if (dg < 0)
dg = -dg;
if ((diff += dg) < best) {
int db = pptr[2] - bb;
if (db < 0)
db = -db;
if ((diff += db) < best)
which = pptr, best = diff;
}
}
if (diff == 0)
break;
pptr += 3;
}
} else {
while ((cnt -= 3) >= 0) {
register int diff = *pptr - br;
if (diff < 0)
diff = -diff;
if (diff < best) {
which = pptr, best = diff;
}
if (diff == 0)
break;
pptr += 3;
}
}
return (gx_color_index) ((which - mdev->palette.data) / 3);
}
int
mem_mapped_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
const byte *pptr = mdev->palette.data + (int)color * 3;
prgb[0] = gx_color_value_from_byte(pptr[0]);
prgb[1] = gx_color_value_from_byte(pptr[1]);
prgb[2] = gx_color_value_from_byte(pptr[2]);
return 0;
}
int
mem_draw_thin_line(gx_device *dev, fixed fx0, fixed fy0, fixed fx1, fixed fy1,
const gx_drawing_color *pdcolor,
gs_logical_operation_t lop)
{
return gx_default_draw_thin_line(dev, fx0, fy0, fx1, fy1, pdcolor, lop);
}