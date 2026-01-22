#include "gdevprn.h"
#include "gdevmem.h"
#include "gdevpccm.h"
#include "gscdefs.h"
#define PNG_INTERNAL
#define PNG_NO_CONSOLE_IO
#include "png_.h"
#define X_DPI 72
#define Y_DPI 72
private dev_proc_print_page(png_print_page);
private dev_proc_open_device(pngalpha_open);
private dev_proc_encode_color(pngalpha_encode_color);
private dev_proc_decode_color(pngalpha_decode_color);
private dev_proc_copy_alpha(pngalpha_copy_alpha);
private dev_proc_fill_rectangle(pngalpha_fill_rectangle);
private dev_proc_get_params(pngalpha_get_params);
private dev_proc_put_params(pngalpha_put_params);
private dev_proc_create_buf_device(pngalpha_create_buf_device);
const gx_device_printer gs_pngmono_device =
prn_device(prn_std_procs, "pngmono",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, png_print_page);
private const gx_device_procs png16_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
pc_4bit_map_rgb_color, pc_4bit_map_color_rgb);
const gx_device_printer gs_png16_device = {
prn_device_body(gx_device_printer, png16_procs, "png16",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
3, 4, 1, 1, 2, 2, png_print_page)
};
private const gx_device_procs png256_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
pc_8bit_map_rgb_color, pc_8bit_map_color_rgb);
const gx_device_printer gs_png256_device = {
prn_device_body(gx_device_printer, png256_procs, "png256",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
3, 8, 5, 5, 6, 6, png_print_page)
};
private const gx_device_procs pnggray_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_gray_map_rgb_color, gx_default_gray_map_color_rgb);
const gx_device_printer gs_pnggray_device =
{prn_device_body(gx_device_printer, pnggray_procs, "pnggray",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, 8, 255, 0, 256, 0, png_print_page)
};
private const gx_device_procs png16m_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb);
const gx_device_printer gs_png16m_device =
prn_device(png16m_procs, "png16m",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
24, png_print_page);
private const gx_device_procs png48_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb);
const gx_device_printer gs_png48_device =
prn_device(png48_procs, "png48",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
48, png_print_page);
typedef struct gx_device_pngalpha_s gx_device_pngalpha;
struct gx_device_pngalpha_s {
gx_device_common;
gx_prn_device_common;
dev_t_proc_fill_rectangle((*orig_fill_rectangle), gx_device);
int background;
};
private const gx_device_procs pngalpha_procs =
{
pngalpha_open,
NULL,
NULL,
gdev_prn_output_page,
gdev_prn_close,
pngalpha_encode_color,
pngalpha_decode_color,
pngalpha_fill_rectangle,
NULL,
NULL,
NULL,
NULL,
NULL,
pngalpha_get_params,
pngalpha_put_params,
NULL,
NULL,
NULL,
NULL,
gx_page_device_get_page_device,
NULL,
pngalpha_copy_alpha,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
gx_default_DevRGB_get_color_mapping_procs,
gx_default_DevRGB_get_color_comp_index,
pngalpha_encode_color,
pngalpha_decode_color
};
const gx_device_pngalpha gs_pngalpha_device = {
std_device_part1_(gx_device_pngalpha, &pngalpha_procs, "pngalpha",
&st_device_printer, open_init_closed),
{3 ,
3 ,
GX_CINFO_POLARITY_ADDITIVE ,
32 ,
-1 ,
255 ,
255 ,
256 ,
256 ,
{ 4, 4 } ,
GX_CINFO_SEP_LIN_NONE ,
{ 0 } ,
{ 0 } ,
{ 0 } ,
"DeviceRGB" ,
GX_CINFO_OPMODE_UNKNOWN ,
0
},
std_device_part2_(
(int)((float)(DEFAULT_WIDTH_10THS) * (X_DPI) / 10 + 0.5),
(int)((float)(DEFAULT_HEIGHT_10THS) * (Y_DPI) / 10 + 0.5),
X_DPI, Y_DPI),
offset_margin_values(0, 0, 0, 0, 0, 0),
std_device_part3_(),
prn_device_body_rest_(png_print_page),
NULL,
0xffffff
};
private int
png_print_page(gx_device_printer * pdev, FILE * file)
{
gs_memory_t *mem = pdev->memory;
int raster = gdev_prn_raster(pdev);
byte *row = gs_alloc_bytes(mem, raster, "png raster buffer");
png_struct *png_ptr =
png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
png_info *info_ptr =
png_create_info_struct(png_ptr);
int height = pdev->height;
int depth = pdev->color_info.depth;
int y;
int code;
char software_key[80];
char software_text[256];
png_text text_png;
if (row == 0 || png_ptr == 0 || info_ptr == 0) {
code = gs_note_error(gs_error_VMerror);
goto done;
}
if (setjmp(png_ptr->jmpbuf)) {
code = gs_note_error(gs_error_VMerror);
goto done;
}
code = 0;
png_init_io(png_ptr, file);
info_ptr->width = pdev->width;
info_ptr->height = pdev->height;
info_ptr->x_pixels_per_unit =
(png_uint_32) (pdev->HWResolution[0] * (100.0 / 2.54));
info_ptr->y_pixels_per_unit =
(png_uint_32) (pdev->HWResolution[1] * (100.0 / 2.54));
info_ptr->phys_unit_type = PNG_RESOLUTION_METER;
info_ptr->valid |= PNG_INFO_pHYs;
switch (depth) {
case 32:
info_ptr->bit_depth = 8;
info_ptr->color_type = PNG_COLOR_TYPE_RGB_ALPHA;
png_set_invert_alpha(png_ptr);
{ gx_device_pngalpha *ppdev = (gx_device_pngalpha *)pdev;
png_color_16 background;
background.index = 0;
background.red = (ppdev->background >> 16) & 0xff;
background.green = (ppdev->background >> 8) & 0xff;
background.blue = (ppdev->background) & 0xff;
background.gray = 0;
png_set_bKGD(png_ptr, info_ptr, &background);
}
break;
case 48:
info_ptr->bit_depth = 16;
info_ptr->color_type = PNG_COLOR_TYPE_RGB;
#if defined(ARCH_IS_BIG_ENDIAN) && (!ARCH_IS_BIG_ENDIAN)
png_set_swap(png_ptr);
#endif
break;
case 24:
info_ptr->bit_depth = 8;
info_ptr->color_type = PNG_COLOR_TYPE_RGB;
break;
case 8:
info_ptr->bit_depth = 8;
if (gx_device_has_color(pdev))
info_ptr->color_type = PNG_COLOR_TYPE_PALETTE;
else
info_ptr->color_type = PNG_COLOR_TYPE_GRAY;
break;
case 4:
info_ptr->bit_depth = 4;
info_ptr->color_type = PNG_COLOR_TYPE_PALETTE;
break;
case 1:
info_ptr->bit_depth = 1;
info_ptr->color_type = PNG_COLOR_TYPE_GRAY;
png_set_invert_mono(png_ptr);
break;
}
if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
int i;
int num_colors = 1 << depth;
gx_color_value rgb[3];
info_ptr->palette =
(void *)gs_alloc_bytes(mem, 256 * sizeof(png_color),
"png palette");
if (info_ptr->palette == 0) {
code = gs_note_error(gs_error_VMerror);
goto done;
}
info_ptr->num_palette = num_colors;
info_ptr->valid |= PNG_INFO_PLTE;
for (i = 0; i < num_colors; i++) {
(*dev_proc(pdev, map_color_rgb)) ((gx_device *) pdev,
(gx_color_index) i, rgb);
info_ptr->palette[i].red = gx_color_value_to_byte(rgb[0]);
info_ptr->palette[i].green = gx_color_value_to_byte(rgb[1]);
info_ptr->palette[i].blue = gx_color_value_to_byte(rgb[2]);
}
}
strncpy(software_key, "Software", sizeof(software_key));
sprintf(software_text, "%s %d.%02d", gs_product,
(int)(gs_revision / 100), (int)(gs_revision % 100));
text_png.compression = -1;
text_png.key = software_key;
text_png.text = software_text;
text_png.text_length = strlen(software_text);
info_ptr->text = &text_png;
info_ptr->num_text = 1;
png_write_info(png_ptr, info_ptr);
info_ptr->num_text = 0;
info_ptr->text = NULL;
for (y = 0; y < height; y++) {
gdev_prn_copy_scan_lines(pdev, y, row, raster);
png_write_rows(png_ptr, &row, 1);
}
png_write_end(png_ptr, info_ptr);
gs_free_object(mem, info_ptr->palette, "png palette");
done:
png_destroy_write_struct(&png_ptr, &info_ptr);
gs_free_object(mem, row, "png raster buffer");
return code;
}
#ifdef PNG_PROGRESSIVE_READ_SUPPORTED
# if PNG_LIBPNG_VER >= 95
# define PPFB_LENGTH_T png_size_t
# else
# define PPFB_LENGTH_T png_uint_32
# endif
void
png_push_fill_buffer(png_structp png_ptr, png_bytep buffer,
PPFB_LENGTH_T length)
{
}
#endif
private int
pngalpha_open(gx_device * pdev)
{
gx_device_pngalpha *ppdev = (gx_device_pngalpha *)pdev;
int code;
ppdev->printer_procs.buf_procs.create_buf_device =
pngalpha_create_buf_device;
code = gdev_prn_open(pdev);
if ((ppdev->procs.fill_rectangle != pngalpha_fill_rectangle) &&
(ppdev->procs.fill_rectangle != NULL)) {
ppdev->orig_fill_rectangle = ppdev->procs.fill_rectangle;
ppdev->procs.fill_rectangle = pngalpha_fill_rectangle;
}
return code;
}
private int
pngalpha_create_buf_device(gx_device **pbdev, gx_device *target,
const gx_render_plane_t *render_plane, gs_memory_t *mem,
bool for_band)
{
gx_device_printer *ptarget = (gx_device_printer *)target;
int code = gx_default_create_buf_device(pbdev, target,
render_plane, mem, for_band);
set_dev_proc(*pbdev, copy_alpha, ptarget->orig_procs.copy_alpha);
return code;
}
private int
pngalpha_put_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_pngalpha *ppdev = (gx_device_pngalpha *)pdev;
int background;
int code;
switch(code = param_read_int(plist, "BackgroundColor", &background)) {
case 0:
ppdev->background = background & 0xffffff;
break;
case 1:
code = 0;
break;
default:
param_signal_error(plist, "BackgroundColor", code);
break;
}
if (code == 0) {
code = gdev_prn_put_params(pdev, plist);
if ((ppdev->procs.fill_rectangle != pngalpha_fill_rectangle) &&
(ppdev->procs.fill_rectangle != NULL)) {
ppdev->orig_fill_rectangle = ppdev->procs.fill_rectangle;
ppdev->procs.fill_rectangle = pngalpha_fill_rectangle;
}
}
return code;
}
private int
pngalpha_get_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_pngalpha *ppdev = (gx_device_pngalpha *)pdev;
int code = gdev_prn_get_params(pdev, plist);
if (code >= 0)
code = param_write_int(plist, "BackgroundColor",
&(ppdev->background));
return code;
}
private gx_color_index
pngalpha_encode_color(gx_device * dev, const gx_color_value cv[])
{
return
((uint) gx_color_value_to_byte(cv[2]) << 8) +
((ulong) gx_color_value_to_byte(cv[1]) << 16) +
((ulong) gx_color_value_to_byte(cv[0]) << 24);
}
private int
pngalpha_decode_color(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
prgb[0] = gx_color_value_from_byte((color >> 24) & 0xff);
prgb[1] = gx_color_value_from_byte((color >> 16) & 0xff);
prgb[2] = gx_color_value_from_byte((color >> 8) & 0xff);
return 0;
}
private int
pngalpha_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
gx_device_pngalpha *pdev = (gx_device_pngalpha *)dev;
if ((color == 0xffffff00) && (x==0) && (y==0)
&& (w==dev->width) && (h==dev->height)) {
return pdev->orig_fill_rectangle(dev, x, y, w, h, 0xfefefeff);
}
return pdev->orig_fill_rectangle(dev, x, y, w, h, color);
}
private int
pngalpha_copy_alpha(gx_device * dev, const byte * data, int data_x,
int raster, gx_bitmap_id id, int x, int y, int width, int height,
gx_color_index color, int depth)
{
if (depth == 1)
return (*dev_proc(dev, copy_mono)) (dev, data, data_x, raster, id,
x, y, width, height,
gx_no_color_index, color);
{
const byte *row;
gs_memory_t *mem = dev->memory;
int bpp = dev->color_info.depth;
int ncomps = dev->color_info.num_components;
uint in_size = gx_device_raster(dev, false);
byte *lin;
uint out_size;
byte *lout;
int code = 0;
gx_color_value color_cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
int ry;
fit_copy(dev, data, data_x, raster, id, x, y, width, height);
row = data;
out_size = bitmap_raster(width * bpp);
lin = gs_alloc_bytes(mem, in_size, "copy_alpha(lin)");
lout = gs_alloc_bytes(mem, out_size, "copy_alpha(lout)");
if (lin == 0 || lout == 0) {
code = gs_note_error(gs_error_VMerror);
goto out;
}
(*dev_proc(dev, decode_color)) (dev, color, color_cv);
for (ry = y; ry < y + height; row += raster, ++ry) {
byte *line;
int sx, rx;
DECLARE_LINE_ACCUM_COPY(lout, bpp, x);
code = (*dev_proc(dev, get_bits)) (dev, ry, lin, &line);
if (code < 0)
break;
for (sx = data_x, rx = x; sx < data_x + width; ++sx, ++rx) {
gx_color_index previous = gx_no_color_index;
gx_color_index composite;
int alpha2, alpha;
if (depth == 2)
alpha = ((row[sx >> 2] >> ((3 - (sx & 3)) << 1)) & 3) * 5;
else
alpha2 = row[sx >> 1],
alpha = (sx & 1 ? alpha2 & 0xf : alpha2 >> 4);
if (alpha == 15) {
composite = color;
} else {
if (previous == gx_no_color_index) {
const byte *src = line + (rx * (bpp >> 3));
previous = 0;
previous += (gx_color_index) * src++ << 24;
previous += (gx_color_index) * src++ << 16;
previous += (gx_color_index) * src++ << 8;
previous += *src++;
}
if (alpha == 0) {
composite = previous;
} else {
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
int i;
int old_coverage;
int new_coverage;
(*dev_proc(dev, decode_color)) (dev, previous, cv);
cv[3] = previous & 0xff;
old_coverage = 255 - cv[3];
new_coverage =
(255 * alpha + old_coverage * (15 - alpha)) / 15;
for (i=0; i<ncomps; i++)
cv[i] = min(((255 * alpha * color_cv[i]) +
(old_coverage * (15 - alpha ) * cv[i]))
/ (new_coverage * 15), gx_max_color_value);
composite =
(*dev_proc(dev, encode_color)) (dev, cv);
composite |= (255 - new_coverage) & 0xff;
}
}
LINE_ACCUM(composite, bpp);
}
LINE_ACCUM_COPY(dev, lout, bpp, x, rx, raster, ry);
}
out:gs_free_object(mem, lout, "copy_alpha(lout)");
gs_free_object(mem, lin, "copy_alpha(lin)");
return code;
}
}