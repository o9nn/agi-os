#include "gx.h"
#include "gserrors.h"
#include "gsparam.h"
#include "gxdevice.h"
#include "gdevpccm.h"
#include <errno.h>
#include <vga.h>
typedef struct gx_device_vgalib {
gx_device_common;
int display_mode;
} gx_device_vgalib;
#define vga_dev ((gx_device_vgalib *)dev)
#define XDPI 60
#define YDPI 60
#ifndef A4
#define YSIZE (20.0 * YDPI / 2.5)
#define XSIZE (8.5 / 11)*YSIZE
#else
#define XSIZE 8.27
#define YSIZE 11.69
#endif
private dev_proc_open_device(vgalib_open);
private dev_proc_close_device(vgalib_close);
private dev_proc_map_rgb_color(vgalib_map_rgb_color);
private dev_proc_map_color_rgb(vgalib_map_color_rgb);
private dev_proc_fill_rectangle(vgalib_fill_rectangle);
private dev_proc_tile_rectangle(vgalib_tile_rectangle);
private dev_proc_copy_mono(vgalib_copy_mono);
private dev_proc_copy_color(vgalib_copy_color);
private dev_proc_get_bits(vgalib_get_bits);
private dev_proc_get_params(vgalib_get_params);
private dev_proc_put_params(vgalib_put_params);
const gx_device_vgalib gs_vgalib_device =
{
std_device_std_body(gx_device_vgalib, 0, "vgalib",
0, 0, 1, 1),
{vgalib_open,
NULL,
NULL,
NULL,
vgalib_close,
vgalib_map_rgb_color,
vgalib_map_color_rgb,
vgalib_fill_rectangle,
vgalib_tile_rectangle,
vgalib_copy_mono,
vgalib_copy_color,
NULL,
vgalib_get_bits,
vgalib_get_params,
vgalib_put_params,
NULL,
NULL,
NULL,
NULL,
gx_page_device_get_page_device
},
-1
};
private int
vgalib_open(gx_device * dev)
{
int VGAMODE = vga_dev->display_mode;
int width = dev->width, height = dev->height;
if (VGAMODE == -1)
VGAMODE = vga_getdefaultmode();
if (VGAMODE == -1)
vga_setmode(G640x480x16);
else
vga_setmode(VGAMODE);
vga_clear();
if (width == 0)
width = vga_getxdim() + 1;
if (height == 0)
height = vga_getydim() + 1;
if (dev->y_pixels_per_inch == 1) {
dev->y_pixels_per_inch = height / 11.0;
dev->x_pixels_per_inch = dev->y_pixels_per_inch;
}
gx_device_set_width_height(dev, width, height);
if (vga_getcolors() > 1) {
int index;
static const gx_device_color_info vgalib_16color = dci_pc_4bit;
dev->color_info = vgalib_16color;
for (index = 0; index < 16; ++index) {
gx_color_value rgb[3];
(*dev_proc(dev, map_color_rgb)) (dev, (gx_color_index) index, rgb);
#define cv2pv(cv) ((cv) >> (gx_color_value_bits - 8))
vga_setpalette(index, cv2pv(rgb[0]), cv2pv(rgb[1]), cv2pv(rgb[2]));
#undef cv2pv
}
}
return 0;
}
private int
vgalib_close(gx_device * dev)
{
vga_setmode(TEXT);
return 0;
}
private gx_color_index
vgalib_map_rgb_color(gx_device * dev, gx_color_value red,
gx_color_value green, gx_color_value blue)
{
return pc_4bit_map_rgb_color(dev, red, green, blue);
}
private int
vgalib_map_color_rgb(gx_device * dev, gx_color_index index,
unsigned short rgb[3])
{
return pc_4bit_map_color_rgb(dev, index, rgb);
}
private int
vgalib_tile_rectangle(gx_device * dev, const gx_tile_bitmap * tile,
int x, int y, int w, int h, gx_color_index czero,
gx_color_index cone, int px, int py)
{
if (czero != gx_no_color_index && cone != gx_no_color_index) {
vgalib_fill_rectangle(dev, x, y, w, h, czero);
czero = gx_no_color_index;
}
return gx_default_tile_rectangle(dev, tile, x, y, w, h, czero, cone, px,
py);
}
private int
vgalib_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
int i, j;
fit_fill(dev, x, y, w, h);
vga_setcolor((int)color);
if ((w | h) > 3) {
if (w > h)
for (i = y; i < y + h; ++i)
vga_drawline(x, i, x + w - 1, i);
else
for (j = x; j < x + w; ++j)
vga_drawline(j, y, j, y + h - 1);
} else {
for (i = y; i < y + h; i++)
for (j = x; j < x + w; j++)
vga_drawpixel(j, i);
}
return 0;
}
private int
vgalib_copy_mono(gx_device * dev, const byte * base, int sourcex,
int raster, gx_bitmap_id id, int x, int y, int width,
int height, gx_color_index zero, gx_color_index one)
{
const byte *ptr_line;
int left_bit, dest_y, end_x;
int invert = 0;
int color;
fit_copy(dev, base, sourcex, raster, id, x, y, width, height);
ptr_line = base + (sourcex >> 3);
left_bit = 0x80 >> (sourcex & 7);
dest_y = y, end_x = x + width;
if (zero == gx_no_color_index) {
if (one == gx_no_color_index)
return 0;
color = (int)one;
} else {
if (one == gx_no_color_index) {
color = (int)zero;
invert = -1;
} else {
vgalib_fill_rectangle(dev, x, y, width, height, zero);
color = (int)one;
}
}
vga_setcolor(color);
while (height--) {
const byte *ptr_source = ptr_line;
register int dest_x = x;
register int bit = left_bit;
while (dest_x < end_x) {
if ((*ptr_source ^ invert) & bit)
vga_drawpixel(dest_x, dest_y);
dest_x++;
if ((bit >>= 1) == 0)
bit = 0x80, ptr_source++;
}
dest_y++;
ptr_line += raster;
}
return 0;
}
private int
vgalib_copy_color(gx_device * dev, const byte * base, int sourcex,
int raster, gx_bitmap_id id, int x, int y,
int width, int height)
{
fit_copy(dev, base, sourcex, raster, id, x, y, width, height);
if (gx_device_has_color(dev)) {
const byte *line = base + (sourcex >> 1);
int dest_y = y, end_x = x + width;
if (width <= 0)
return 0;
while (height--) {
const byte *source = line;
register int dest_x = x;
if (sourcex & 1) {
int color = *source++ & 0xf;
vga_setcolor(color);
vga_drawpixel(dest_x, dest_y);
dest_x++;
}
while (dest_x < end_x) {
int color = *source >> 4;
vga_setcolor(color);
vga_drawpixel(dest_x, dest_y);
dest_x++;
if (dest_x < end_x) {
color = *source++ & 0xf;
vga_setcolor(color);
vga_drawpixel(dest_x, dest_y);
dest_x++;
}
}
dest_y++;
line += raster;
}
} else {
vgalib_copy_mono(dev, base, sourcex, raster, id, x, y, width, height,
(gx_color_index) 0, (gx_color_index) 7);
}
return 0;
}
private int
vgalib_get_bits(gx_device * dev, int y, byte * data, byte ** actual_data)
{
int x;
byte *dest = data;
int b = 0;
int depth = dev->color_info.depth;
int mask = (1 << depth) - 1;
int left = 8;
if (actual_data)
*actual_data = data;
for (x = 0; x < dev->width; ++x) {
int color = vga_getpixel(x, y);
if ((left -= depth) < 0)
*dest++ = b, b = 0, left += 8;
b += (color & mask) << left;
}
if (left < 8)
*dest = b;
return 0;
}
private int
vgalib_get_params(gx_device * dev, gs_param_list * plist)
{
int code = gx_default_get_params(dev, plist);
if (code < 0)
return code;
return param_write_int(plist, "DisplayMode", &vga_dev->display_mode);
}
private int
vgalib_put_params(gx_device * dev, gs_param_list * plist)
{
int ecode = 0;
int code;
int imode = vga_dev->display_mode;
const char *param_name;
switch (code = param_read_int(plist, (param_name = "DisplayMode"), &imode)) {
default:
ecode = code;
param_signal_error(plist, param_name, ecode);
case 0:
case 1:
break;
}
if (ecode < 0)
return ecode;
code = gx_default_put_params(dev, plist);
if (code < 0)
return code;
if (imode != vga_dev->display_mode) {
if (dev->is_open)
gs_closedevice(dev);
vga_dev->display_mode = imode;
}
return 0;
}