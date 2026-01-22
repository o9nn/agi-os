#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gdevmem.h"
private dev_proc_map_rgb_color(mem_alpha_map_rgb_color);
private dev_proc_map_color_rgb(mem_alpha_map_color_rgb);
private dev_proc_map_rgb_alpha_color(mem_alpha_map_rgb_alpha_color);
private dev_proc_copy_alpha(mem_alpha_copy_alpha);
void
gs_make_mem_alpha_device(gx_device_memory * adev, gs_memory_t * mem,
gx_device * target, int alpha_bits)
{
gs_make_mem_device(adev, gdev_mem_device_for_bits(alpha_bits),
mem, 0, target);
adev->color_info = gdev_mem_device_for_bits(1)->color_info;
adev->color_info.depth = alpha_bits;
set_dev_proc(adev, map_rgb_color, mem_alpha_map_rgb_color);
set_dev_proc(adev, map_color_rgb, mem_alpha_map_color_rgb);
set_dev_proc(adev, map_rgb_alpha_color, mem_alpha_map_rgb_alpha_color);
set_dev_proc(adev, copy_alpha, mem_alpha_copy_alpha);
}
private gx_color_index
mem_alpha_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
gx_color_index color = gx_forward_map_rgb_color(dev, cv);
return (color == 0 || color == gx_no_color_index ? color :
(gx_color_index) ((1 << mdev->log2_alpha_bits) - 1));
}
private int
mem_alpha_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
return
gx_forward_map_color_rgb(dev,
(color == 0 ? color : (gx_color_index) 1),
prgb);
}
private gx_color_index
mem_alpha_map_rgb_alpha_color(gx_device * dev, gx_color_value r,
gx_color_value g, gx_color_value b, gx_color_value alpha)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
gx_color_index color;
gx_color_value cv[3];
cv[0] = r; cv[1] = g; cv[2] = b;
color = gx_forward_map_rgb_color(dev, cv);
return (color == 0 || color == gx_no_color_index ? color :
(gx_color_index) (alpha >> (gx_color_value_bits -
mdev->log2_alpha_bits)));
}
private int
mem_alpha_copy_alpha(gx_device * dev, const byte * data, int data_x,
int raster, gx_bitmap_id id, int x, int y, int width, int height,
gx_color_index color, int depth)
{
return (color == 0 ?
(*dev_proc(dev, fill_rectangle)) (dev, x, y, width, height,
color) :
(*dev_proc(dev, copy_color)) (dev, data, data_x, raster, id,
x, y, width, height));
}
private dev_proc_close_device(mem_abuf_close);
private dev_proc_copy_mono(mem_abuf_copy_mono);
private dev_proc_fill_rectangle(mem_abuf_fill_rectangle);
private dev_proc_get_clipping_box(mem_abuf_get_clipping_box);
private const gx_device_memory mem_alpha_buffer_device =
mem_device("image(alpha buffer)", 0, 1,
gx_forward_map_rgb_color, gx_forward_map_color_rgb,
mem_abuf_copy_mono, gx_default_copy_color, mem_abuf_fill_rectangle,
gx_no_strip_copy_rop);
void
gs_make_mem_abuf_device(gx_device_memory * adev, gs_memory_t * mem,
gx_device * target, const gs_log2_scale_point * pscale,
int alpha_bits, int mapped_x)
{
gs_make_mem_device(adev, &mem_alpha_buffer_device, mem, 0, target);
adev->max_fill_band = 1 << pscale->y;
adev->log2_scale = *pscale;
adev->log2_alpha_bits = alpha_bits >> 1;
adev->mapped_x = mapped_x;
set_dev_proc(adev, close_device, mem_abuf_close);
set_dev_proc(adev, get_clipping_box, mem_abuf_get_clipping_box);
adev->color_info.anti_alias.text_bits =
adev->color_info.anti_alias.graphics_bits =
alpha_bits;
}
bool
gs_device_is_abuf(const gx_device * dev)
{
return dev->dname == mem_alpha_buffer_device.dname;
}
private int
abuf_flush_block(gx_device_memory * adev, int y)
{
gx_device *target = adev->target;
int block_height = 1 << adev->log2_scale.y;
int alpha_bits = 1 << adev->log2_alpha_bits;
int ddepth =
(adev->width >> adev->log2_scale.x) << adev->log2_alpha_bits;
uint draster = bitmap_raster(ddepth);
int buffer_y = y - adev->mapped_y + adev->mapped_start;
byte *bits;
if (buffer_y >= adev->height)
buffer_y -= adev->height;
bits = scan_line_base(adev, buffer_y);
{
int alpha_mask = ~7;
gs_int_rect bbox;
int width;
bits_bounding_box(bits, block_height, adev->raster, &bbox);
bbox.p.x &= alpha_mask;
bbox.q.x = (bbox.q.x + ~alpha_mask) & alpha_mask;
width = bbox.q.x - bbox.p.x;
bits_compress_scaled(bits, bbox.p.x, width, block_height,
adev->raster, bits, draster, &adev->log2_scale,
adev->log2_alpha_bits);
return (*dev_proc(target, copy_alpha)) (target,
bits, 0, draster, gx_no_bitmap_id,
(adev->mapped_x + bbox.p.x) >>
adev->log2_scale.x,
y >> adev->log2_scale.y,
width >> adev->log2_scale.x, 1,
adev->save_color, alpha_bits);
}
}
private int
abuf_flush(gx_device_memory * adev)
{
int y, code = 0;
int block_height = 1 << adev->log2_scale.y;
for (y = 0; y < adev->mapped_height; y += block_height)
if ((code = abuf_flush_block(adev, adev->mapped_y + y)) < 0)
return code;
adev->mapped_height = adev->mapped_start = 0;
return 0;
}
private int
mem_abuf_close(gx_device * dev)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
int code = abuf_flush(mdev);
if (code < 0)
return code;
return mem_close(dev);
}
typedef struct y_transfer_s {
int y_next;
int height_left;
int transfer_y;
int transfer_height;
} y_transfer;
private void
y_transfer_init(y_transfer * pyt, gx_device * dev, int ty, int th)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
int bh = 1 << mdev->log2_scale.y;
if (ty < mdev->mapped_y || ty > mdev->mapped_y + mdev->mapped_height) {
abuf_flush(mdev);
mdev->mapped_y = ty & -bh;
mdev->mapped_height = bh;
memset(scan_line_base(mdev, 0), 0, bh * mdev->raster);
}
pyt->y_next = ty;
pyt->height_left = th;
pyt->transfer_height = 0;
}
private void
y_transfer_next(y_transfer * pyt, gx_device * dev)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
int my = mdev->mapped_y, mh = mdev->mapped_height;
int ms = mdev->mapped_start;
int ty = pyt->y_next += pyt->transfer_height;
int th = pyt->height_left;
int bh = 1 << mdev->log2_scale.y;
int tby, tbh;
if (ty == my + mh) {
if (mh == mdev->height) {
abuf_flush_block(mdev, my);
mdev->mapped_y = my += bh;
if ((mdev->mapped_start = ms += bh) == mh)
mdev->mapped_start = ms = 0;
} else {
mdev->mapped_height = mh += bh;
}
memset(scan_line_base(mdev, (ms == 0 ? mh : ms) - bh),
0, bh * mdev->raster);
}
tby = ty - my + ms;
if (tby < mdev->height) {
tbh = mdev->height - ms;
if (tbh > mh)
tbh = mh;
tbh -= tby - ms;
} else {
tby -= mdev->height;
tbh = ms + mh - dev->height - tby;
}
if_debug7('V',
"[V]abuf: my=%d, mh=%d, ms=%d, ty=%d, th=%d, tby=%d, tbh=%d\n",
my, mh, ms, ty, th, tby, tbh);
if (tbh > th)
tbh = th;
pyt->height_left = th - tbh;
pyt->transfer_y = tby;
pyt->transfer_height = tbh;
}
private int
mem_abuf_copy_mono(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
y_transfer yt;
if (zero != gx_no_color_index || one == gx_no_color_index)
return_error(gs_error_undefinedresult);
x -= mdev->mapped_x;
fit_copy_xyw(dev, base, sourcex, sraster, id, x, y, w, h);
if (w <= 0 || h <= 0)
return 0;
mdev->save_color = one;
y_transfer_init(&yt, dev, y, h);
while (yt.height_left > 0) {
y_transfer_next(&yt, dev);
(*dev_proc(&mem_mono_device, copy_mono)) (dev,
base + (yt.y_next - y) * sraster,
sourcex, sraster, gx_no_bitmap_id,
x, yt.transfer_y, w, yt.transfer_height,
gx_no_color_index, (gx_color_index) 1);
}
return 0;
}
private int
mem_abuf_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
y_transfer yt;
x -= mdev->mapped_x;
fit_fill_xy(dev, x, y, w, h);
fit_fill_w(dev, x, w);
mdev->save_color = color;
y_transfer_init(&yt, dev, y, h);
while (yt.height_left > 0) {
y_transfer_next(&yt, dev);
(*dev_proc(&mem_mono_device, fill_rectangle)) (dev,
x, yt.transfer_y, w, yt.transfer_height,
(gx_color_index) 1);
}
return 0;
}
private void
mem_abuf_get_clipping_box(gx_device * dev, gs_fixed_rect * pbox)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
gx_device *tdev = mdev->target;
(*dev_proc(tdev, get_clipping_box)) (tdev, pbox);
pbox->p.x <<= mdev->log2_scale.x;
pbox->p.y <<= mdev->log2_scale.y;
pbox->q.x <<= mdev->log2_scale.x;
pbox->q.y <<= mdev->log2_scale.y;
}