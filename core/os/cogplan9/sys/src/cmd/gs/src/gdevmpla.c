#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsbitops.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxgetbit.h"
#include "gdevmem.h"
#include "gdevmpla.h"
private dev_proc_open_device(mem_planar_open);
declare_mem_procs(mem_planar_copy_mono, mem_planar_copy_color, mem_planar_fill_rectangle);
private dev_proc_strip_tile_rectangle(mem_planar_strip_tile_rectangle);
private dev_proc_get_bits_rectangle(mem_planar_get_bits_rectangle);
int
gdev_mem_set_planar(gx_device_memory * mdev, int num_planes,
const gx_render_plane_t *planes )
{
int total_depth;
int same_depth = planes[0].depth;
gx_color_index covered = 0;
int pi;
if (num_planes < 1 || num_planes > GX_DEVICE_COLOR_MAX_COMPONENTS)
return_error(gs_error_rangecheck);
for (pi = 0, total_depth = 0; pi < num_planes; ++pi) {
int shift = planes[pi].shift;
int plane_depth = planes[pi].depth;
gx_color_index mask;
if (shift < 0 || plane_depth > 16 ||
!gdev_mem_device_for_bits(plane_depth))
return_error(gs_error_rangecheck);
mask = (((gx_color_index)1 << plane_depth) - 1) << shift;
if (covered & mask)
return_error(gs_error_rangecheck);
covered |= mask;
if (plane_depth != same_depth)
same_depth = 0;
total_depth += plane_depth;
}
if (total_depth > mdev->color_info.depth)
return_error(gs_error_rangecheck);
mdev->num_planes = num_planes;
memcpy(mdev->planes, planes, num_planes * sizeof(planes[0]));
mdev->plane_depth = same_depth;
set_dev_proc(mdev, open_device, mem_planar_open);
set_dev_proc(mdev, fill_rectangle, mem_planar_fill_rectangle);
set_dev_proc(mdev, copy_mono, mem_planar_copy_mono);
set_dev_proc(mdev, copy_color, mem_planar_copy_color);
set_dev_proc(mdev, copy_alpha, gx_default_copy_alpha);
set_dev_proc(mdev, strip_tile_rectangle, mem_planar_strip_tile_rectangle);
set_dev_proc(mdev, strip_copy_rop, gx_default_strip_copy_rop);
set_dev_proc(mdev, get_bits_rectangle, mem_planar_get_bits_rectangle);
return 0;
}
private int
mem_planar_open(gx_device * dev)
{
gx_device_memory *const mdev = (gx_device_memory *)dev;
if (mdev->num_planes == 0)
return_error(gs_error_rangecheck);
return gdev_mem_open_scan_lines(mdev, dev->height);
}
typedef struct mem_save_params_s {
int depth;
byte *base;
byte **line_ptrs;
} mem_save_params_t;
#define MEM_SAVE_PARAMS(mdev, msp)\
(msp.depth = mdev->color_info.depth,\
msp.base = mdev->base,\
msp.line_ptrs = mdev->line_ptrs)
#define MEM_SET_PARAMS(mdev, plane_depth)\
(mdev->color_info.depth = plane_depth, \
mdev->base = mdev->line_ptrs[0],\
mdev->raster = bitmap_raster(mdev->width * plane_depth))
#define MEM_RESTORE_PARAMS(mdev, msp)\
(mdev->color_info.depth = msp.depth,\
mdev->base = msp.base,\
mdev->line_ptrs = msp.line_ptrs)
private int
mem_planar_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
mem_save_params_t save;
int pi;
MEM_SAVE_PARAMS(mdev, save);
for (pi = 0; pi < mdev->num_planes; ++pi) {
int plane_depth = mdev->planes[pi].depth;
gx_color_index mask = ((gx_color_index)1 << plane_depth) - 1;
const gx_device_memory *mdproto =
gdev_mem_device_for_bits(plane_depth);
MEM_SET_PARAMS(mdev, plane_depth);
dev_proc(mdproto, fill_rectangle)(dev, x, y, w, h,
(color >> mdev->planes[pi].shift) &
mask);
mdev->line_ptrs += mdev->height;
}
MEM_RESTORE_PARAMS(mdev, save);
return 0;
}
private int
mem_planar_copy_mono(gx_device * dev, const byte * base, int sourcex,
int sraster, gx_bitmap_id id, int x, int y, int w, int h,
gx_color_index color0, gx_color_index color1)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
mem_save_params_t save;
int pi;
MEM_SAVE_PARAMS(mdev, save);
for (pi = 0; pi < mdev->num_planes; ++pi) {
int plane_depth = mdev->planes[pi].depth;
int shift = mdev->planes[pi].shift;
gx_color_index mask = ((gx_color_index)1 << plane_depth) - 1;
const gx_device_memory *mdproto =
gdev_mem_device_for_bits(plane_depth);
gx_color_index c0 =
(color0 == gx_no_color_index ? gx_no_color_index :
(color0 >> shift) & mask);
gx_color_index c1 =
(color1 == gx_no_color_index ? gx_no_color_index :
(color1 >> shift) & mask);
MEM_SET_PARAMS(mdev, plane_depth);
if (c0 == c1)
dev_proc(mdproto, fill_rectangle)(dev, x, y, w, h, c0);
else
dev_proc(mdproto, copy_mono)
(dev, base, sourcex, sraster, id, x, y, w, h, c0, c1);
mdev->line_ptrs += mdev->height;
}
MEM_RESTORE_PARAMS(mdev, save);
return 0;
}
private int
mem_planar_copy_color(gx_device * dev, const byte * base, int sourcex,
int sraster, gx_bitmap_id id,
int x, int y, int w, int h)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
#define BUF_LONGS 100
#define BUF_BYTES (BUF_LONGS * ARCH_SIZEOF_LONG)
union b_ {
ulong l[BUF_LONGS];
byte b[BUF_BYTES];
} buf;
int source_depth = dev->color_info.depth;
mem_save_params_t save;
int pi;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
MEM_SAVE_PARAMS(mdev, save);
for (pi = 0; pi < mdev->num_planes; ++pi) {
int plane_depth = mdev->planes[pi].depth;
int shift = mdev->planes[pi].shift;
gx_color_index mask = ((gx_color_index)1 << plane_depth) - 1;
const gx_device_memory *mdproto =
gdev_mem_device_for_bits(plane_depth);
uint plane_raster = bitmap_raster(plane_depth * w);
int br, bw, bh, cx, cy, cw, ch, ix, iy;
MEM_SET_PARAMS(mdev, plane_depth);
if (plane_raster > BUF_BYTES) {
br = BUF_BYTES;
bw = BUF_BYTES * 8 / plane_depth;
bh = 1;
} else {
br = plane_raster;
bw = w;
bh = BUF_BYTES / plane_raster;
}
for (cy = y; cy < y + h; cy += ch) {
ch = min(bh, y + h - cy);
for (cx = x; cx < x + w; cx += cw) {
int sx = sourcex + cx - x;
const byte *source_base = base + sraster * (cy - y);
int source_bit = 0;
cw = min(bw, x + w - cx);
if (sx) {
int xbit = sx * source_depth;
source_base += xbit >> 3;
source_bit = xbit & 7;
}
for (iy = 0; iy < ch; ++iy) {
sample_load_declare_setup(sptr, sbit, source_base,
source_bit, source_depth);
sample_store_declare_setup(dptr, dbit, dbbyte,
buf.b + br * iy,
0, plane_depth);
for (ix = 0; ix < cw; ++ix) {
gx_color_index value;
sample_load_next_any(value, sptr, sbit, source_depth);
value = (value >> shift) & mask;
sample_store_next16(value, dptr, dbit, plane_depth,
dbbyte);
}
sample_store_flush(dptr, dbit, plane_depth, dbbyte);
source_base += sraster;
}
if (plane_depth == 1)
dev_proc(mdproto, copy_mono)
(dev, buf.b, 0, br, gx_no_bitmap_id, cx, cy, cw, ch,
(gx_color_index)0, (gx_color_index)1);
else
dev_proc(mdproto, copy_color)
(dev, buf.b, 0, br, gx_no_bitmap_id, cx, cy, cw, ch);
}
}
mdev->line_ptrs += mdev->height;
}
MEM_RESTORE_PARAMS(mdev, save);
return 0;
#undef BUF_BYTES
#undef BUF_LONGS
}
private int
mem_planar_strip_tile_rectangle(gx_device * dev, const gx_strip_bitmap * tiles,
int x, int y, int w, int h,
gx_color_index color0, gx_color_index color1,
int px, int py)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
mem_save_params_t save;
int pi;
if (color0 == gx_no_color_index && color1 == gx_no_color_index)
return gx_default_strip_tile_rectangle
(dev, tiles, x, y, w, h, color0, color1, px, py);
MEM_SAVE_PARAMS(mdev, save);
for (pi = 0; pi < mdev->num_planes; ++pi) {
int plane_depth = mdev->planes[pi].depth;
int shift = mdev->planes[pi].shift;
gx_color_index mask = ((gx_color_index)1 << plane_depth) - 1;
const gx_device_memory *mdproto =
gdev_mem_device_for_bits(plane_depth);
gx_color_index c0 =
(color0 == gx_no_color_index ? gx_no_color_index :
(color0 >> shift) & mask);
gx_color_index c1 =
(color1 == gx_no_color_index ? gx_no_color_index :
(color1 >> shift) & mask);
MEM_SET_PARAMS(mdev, plane_depth);
if (c0 == c1)
dev_proc(mdproto, fill_rectangle)(dev, x, y, w, h, c0);
else {
set_dev_proc(dev, copy_mono, dev_proc(mdproto, copy_mono));
dev_proc(mdproto, strip_tile_rectangle)
(dev, tiles, x, y, w, h, c0, c1, px, py);
}
mdev->line_ptrs += mdev->height;
}
MEM_RESTORE_PARAMS(mdev, save);
set_dev_proc(dev, copy_mono, mem_planar_copy_mono);
return 0;
}
private int
planar_to_chunky(gx_device_memory *mdev, int x, int y, int w, int h,
int offset, uint draster, byte *dest)
{
int num_planes = mdev->num_planes;
sample_load_declare(sptr[GX_DEVICE_COLOR_MAX_COMPONENTS],
sbit[GX_DEVICE_COLOR_MAX_COMPONENTS]);
sample_store_declare(dptr, dbit, dbbyte);
int ddepth = mdev->color_info.depth;
int direct =
(mdev->color_info.depth != num_planes * mdev->plane_depth ? 0 :
mdev->planes[0].shift == 0 ? -mdev->plane_depth : mdev->plane_depth);
int pi, ix, iy;
if (direct < 0) {
for (pi = 0; pi < num_planes; ++pi)
if (mdev->planes[pi].shift != pi * -direct) {
direct = 0; break;
}
} else if (direct > 0) {
for (pi = 0; pi < num_planes; ++pi)
if (mdev->planes[num_planes - 1 - pi].shift != pi * direct) {
direct = 0; break;
}
}
for (iy = y; iy < y + h; ++iy) {
byte **line_ptr = mdev->line_ptrs + iy;
for (pi = 0; pi < num_planes; ++pi, line_ptr += mdev->height) {
int plane_depth = mdev->planes[pi].depth;
int xbit = x * plane_depth;
sptr[pi] = *line_ptr + (xbit >> 3);
sample_load_setup(sbit[pi], xbit & 7, plane_depth);
}
{
int xbit = offset * ddepth;
dptr = dest + (iy - y) * draster + (xbit >> 3);
sample_store_setup(dbit, xbit & 7, ddepth);
}
if (direct == -8) {
switch (num_planes) {
case 3: {
const byte *p0 = sptr[2];
const byte *p1 = sptr[1];
const byte *p2 = sptr[0];
for (ix = w; ix > 0; --ix, dptr += 3) {
dptr[0] = *p0++;
dptr[1] = *p1++;
dptr[2] = *p2++;
}
}
continue;
case 4:
for (ix = w; ix > 0; --ix, dptr += 4) {
dptr[0] = *sptr[3]++;
dptr[1] = *sptr[2]++;
dptr[2] = *sptr[1]++;
dptr[3] = *sptr[0]++;
}
continue;
default:
break;
}
}
sample_store_preload(dbbyte, dptr, dbit, ddepth);
for (ix = w; ix > 0; --ix) {
gx_color_index color = 0;
for (pi = 0; pi < num_planes; ++pi) {
int plane_depth = mdev->planes[pi].depth;
uint value;
sample_load_next16(value, sptr[pi], sbit[pi], plane_depth);
color |= (gx_color_index)value << mdev->planes[pi].shift;
}
sample_store_next_any(color, dptr, dbit, ddepth, dbbyte);
}
sample_store_flush(dptr, dbit, ddepth, dbbyte);
}
return 0;
}
private int
mem_planar_get_bits_rectangle(gx_device * dev, const gs_int_rect * prect,
gs_get_bits_params_t * params,
gs_int_rect ** unread)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
gs_get_bits_options_t options = params->options;
int x = prect->p.x, w = prect->q.x - x, y = prect->p.y, h = prect->q.y - y;
int num_planes = mdev->num_planes;
gs_get_bits_params_t copy_params;
int code;
if (options == 0) {
params->options =
(GB_ALIGN_STANDARD | GB_ALIGN_ANY) |
(GB_RETURN_COPY | GB_RETURN_POINTER) |
(GB_OFFSET_0 | GB_OFFSET_SPECIFIED | GB_OFFSET_ANY) |
(GB_RASTER_STANDARD | GB_RASTER_SPECIFIED | GB_RASTER_ANY) |
GB_PACKING_CHUNKY |
GB_COLORS_NATIVE | GB_ALPHA_NONE;
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
if (!(~options & (GB_PACKING_PLANAR | GB_SELECT_PLANES))) {
int pi;
for (pi = 0; pi < num_planes; ++pi)
if (params->data[pi] != 0)
break;
if (pi < num_planes) {
int plane = pi++;
for (; pi < num_planes; ++pi)
if (params->data[pi] != 0)
break;
if (pi == num_planes) {
mem_save_params_t save;
copy_params = *params;
copy_params.options =
(options & ~(GB_PACKING_ALL | GB_SELECT_PLANES)) |
GB_PACKING_CHUNKY;
copy_params.data[0] = copy_params.data[plane];
MEM_SAVE_PARAMS(mdev, save);
mdev->line_ptrs += mdev->height * plane;
MEM_SET_PARAMS(mdev, mdev->planes[plane].depth);
code = mem_get_bits_rectangle(dev, prect, &copy_params,
unread);
MEM_RESTORE_PARAMS(mdev, save);
if (code >= 0) {
params->data[plane] = copy_params.data[0];
return code;
}
}
}
}
if (!(~options & (GB_COLORS_NATIVE | GB_ALPHA_NONE |
GB_PACKING_CHUNKY | GB_RETURN_COPY))) {
int offset = (options & GB_OFFSET_SPECIFIED ? params->x_offset : 0);
uint draster =
(options & GB_RASTER_SPECIFIED ? params->raster :
bitmap_raster((offset + w) * mdev->color_info.depth));
planar_to_chunky(mdev, x, y, w, h, offset, draster, params->data[0]);
} else {
#define BUF_LONGS\
max(100, (GX_DEVICE_COLOR_MAX_COMPONENTS * 2 + sizeof(long) - 1) /\
sizeof(long))
#define BUF_BYTES (BUF_LONGS * ARCH_SIZEOF_LONG)
union b_ {
ulong l[BUF_LONGS];
byte b[BUF_BYTES];
} buf;
int br, bw, bh, cx, cy, cw, ch;
int ddepth = mdev->color_info.depth;
uint raster = bitmap_raster(ddepth * mdev->width);
gs_get_bits_params_t dest_params;
if (raster > BUF_BYTES) {
br = BUF_BYTES;
bw = BUF_BYTES * 8 / ddepth;
bh = 1;
} else {
br = raster;
bw = w;
bh = BUF_BYTES / raster;
}
copy_params.options =
GB_COLORS_NATIVE | GB_PACKING_CHUNKY | GB_ALPHA_NONE |
GB_RASTER_STANDARD;
copy_params.raster = raster;
dest_params = *params;
for (cy = y; cy < y + h; cy += ch) {
ch = min(bh, y + h - cy);
for (cx = x; cx < x + w; cx += cw) {
cw = min(bw, x + w - cx);
planar_to_chunky(mdev, cx, cy, cw, ch, 0, br, buf.b);
dest_params.x_offset = params->x_offset + cx - x;
code = gx_get_bits_copy(dev, 0, cw, ch, &dest_params,
&copy_params, buf.b, br);
if (code < 0)
return code;
}
dest_params.data[0] += ch * raster;
}
#undef BUF_BYTES
#undef BUF_LONGS
}
return 0;
}