#include "memory_.h"
#include "gx.h"
#include "gxdevice.h"
#include "gserrors.h"
#include <errno.h>
#include <vga.h>
#include <vgagl.h>
#define first_dc_index 64
private int next_dc_index;
#define dc_hash_size 293
typedef struct {
ushort rgb, index;
} dc_entry;
private dc_entry dynamic_colors[dc_hash_size + 1];
#define XDPI   60
#define YDPI   60
#ifndef A4
#define YSIZE (20.0 * YDPI / 2.5)
#define XSIZE (8.5 / 11)*YSIZE
#else
#define XSIZE 8.3
#define YSIZE 11.7
#endif
typedef struct gx_device_lvga256 {
gx_device_common;
} gx_device_lvga256;
#define lvga256dev ((gx_device_lvga256 *)dev)
private dev_proc_open_device(lvga256_open);
private dev_proc_close_device(lvga256_close);
private dev_proc_map_rgb_color(lvga256_map_rgb_color);
private dev_proc_map_color_rgb(lvga256_map_color_rgb);
private dev_proc_fill_rectangle(lvga256_fill_rectangle);
private dev_proc_tile_rectangle(lvga256_tile_rectangle);
private dev_proc_copy_mono(lvga256_copy_mono);
private dev_proc_copy_color(lvga256_copy_color);
private dev_proc_draw_line(lvga256_draw_line);
private gx_device_procs lvga256_procs =
{
lvga256_open,
NULL,
NULL,
NULL,
lvga256_close,
lvga256_map_rgb_color,
lvga256_map_color_rgb,
lvga256_fill_rectangle,
lvga256_tile_rectangle,
lvga256_copy_mono,
lvga256_copy_color,
lvga256_draw_line
};
gx_device_lvga256 far_data gs_lvga256_device =
{std_device_color_body(gx_device_lvga256, &lvga256_procs, "lvga256",
0, 0,
1, 1,
8, 31, 4  )
};
int
lvga256_open(gx_device * dev)
{
int vgamode;
int width, height;
vga_init();
vgamode = vga_getdefaultmode();
if (vgamode == -1)
vgamode = G320x200x256;
vga_setmode(vgamode);
gl_setcontextvga(vgamode);
width = vga_getxdim();
height = vga_getydim();
dev->y_pixels_per_inch = height / 12.0;
dev->x_pixels_per_inch = dev->y_pixels_per_inch;
gx_device_set_width_height(dev, width, height);
{
int c;
for (c = 0; c < 64; c++) {
static const byte c2[10] =
{0, 42, 0, 0, 0, 0, 0, 0, 21, 63};
gl_setpalettecolor(c, c2[(c >> 2) & 9], c2[(c >> 1) & 9], c2[c & 9]);
}
}
memset(dynamic_colors, 0, (dc_hash_size + 1) * sizeof(dc_entry));
next_dc_index = first_dc_index;
return 0;
}
int
lvga256_close(gx_device * dev)
{
vga_setmode(TEXT);
return 0;
}
gx_color_index
lvga256_map_rgb_color(gx_device * dev, gx_color_value r, gx_color_value g,
gx_color_value b)
{
#define cv_bits(v,n) (v >> (gx_color_value_bits - n))
ushort r5 = cv_bits(r, 5), g5 = cv_bits(g, 5), b5 = cv_bits(b, 5);
static const byte cube_bits[32] =
{0, 128, 128, 128, 128, 128, 128, 128, 128, 128,
8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
1, 128, 128, 128, 128, 128, 128, 128, 128, 128,
9
};
uint cx = ((uint) cube_bits[r5] << 2) + ((uint) cube_bits[g5] << 1) +
(uint) cube_bits[b5];
ushort rgb;
register dc_entry *pdc;
if (cx < 64)
return (gx_color_index) cx;
rgb = (r5 << 10) + (g5 << 5) + b5;
for (pdc = &dynamic_colors[rgb % dc_hash_size]; pdc->rgb != 0; pdc++) {
if (pdc->rgb == rgb)
return (gx_color_index) (pdc->index);
}
if (pdc == &dynamic_colors[dc_hash_size]) {
for (pdc = &dynamic_colors[0]; pdc->rgb != 0; pdc++) {
if (pdc->rgb == rgb)
return (gx_color_index) (pdc->index);
}
}
if (next_dc_index == 256) {
return gx_no_color_index;
}
{
int i = next_dc_index++;
pdc->rgb = rgb;
pdc->index = i;
gl_setpalettecolor(i, cv_bits(r, 6), cv_bits(g, 6), cv_bits(b, 6));
return (gx_color_index) i;
}
}
int
lvga256_map_color_rgb(gx_device * dev, gx_color_index color,
unsigned short prgb[3])
{
prgb[0] = gx_max_color_value;
prgb[1] = gx_max_color_value;
prgb[2] = gx_max_color_value;
return 0;
}
int
lvga256_copy_mono(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one)
{
const byte *ptr_line = base + (sourcex >> 3);
int left_bit = 0x80 >> (sourcex & 7);
int dest_y = y, end_x = x + w;
int invert = 0;
int color;
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
if (zero == gx_no_color_index) {
if (one == gx_no_color_index)
return 0;
color = (int)one;
} else {
if (one == gx_no_color_index) {
color = (int)zero;
invert = -1;
} else {
gl_fillbox(x, y, w, h, 0);
color = (int)one;
}
}
while (h--) {
const byte *ptr_source = ptr_line;
register int dest_x = x;
register int bit = left_bit;
while (dest_x < end_x) {
if ((*ptr_source ^ invert) & bit) {
gl_setpixel(dest_x, dest_y, color);
}
dest_x++;
if ((bit >>= 1) == 0)
bit = 0x80, ptr_source++;
}
dest_y++;
ptr_line += raster;
}
return 0;
}
int
lvga256_copy_color(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
fit_copy(dev, base, sourcex, raster, id, x, y, w, h);
if (gx_device_has_color(dev)) {
const byte *line = base + sourcex;
gl_putbox(x, y, w, h, line);
} else {
lvga256_copy_mono(dev, base, sourcex, raster, id, x, y, w, h,
(gx_color_index) 0, (gx_color_index) 255);
}
return 0;
}
int
lvga256_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
fit_fill(dev, x, y, w, h);
gl_fillbox(x, y, w, h, color);
return 0;
}
int
lvga256_tile_rectangle(gx_device * dev, const gx_tile_bitmap * tile,
int x, int y, int w, int h, gx_color_index czero, gx_color_index cone,
int px, int py)
{
if (czero != gx_no_color_index && cone != gx_no_color_index) {
lvga256_fill_rectangle(dev, x, y, w, h, czero);
czero = gx_no_color_index;
}
return gx_default_tile_rectangle(dev, tile, x, y, w, h, czero, cone, px, py);
}
int
lvga256_draw_line(gx_device * dev, int x0, int y0, int x1, int y1,
gx_color_index color)
{
gl_line(x0, y0, x1, y1, color);
return 0;
}