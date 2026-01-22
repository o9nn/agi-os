#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsdevice.h"
#include "gxdevice.h"
#include "gp.h"
#include "gpcheck.h"
#include "gsparam.h"
#include "gdevpccm.h"
#include "gxdevmem.h"
#include "gdevdevn.h"
#include "gsequivc.h"
#include "gdevdsp.h"
#include "gdevdsp2.h"
#define INITIAL_RESOLUTION 96
#define INITIAL_WIDTH ((INITIAL_RESOLUTION * 85 + 5) / 10)
#define INITIAL_HEIGHT ((INITIAL_RESOLUTION * 110 + 5) / 10)
private dev_proc_open_device(display_open);
private dev_proc_get_initial_matrix(display_get_initial_matrix);
private dev_proc_sync_output(display_sync_output);
private dev_proc_output_page(display_output_page);
private dev_proc_close_device(display_close);
private dev_proc_map_rgb_color(display_map_rgb_color_device4);
private dev_proc_map_color_rgb(display_map_color_rgb_device4);
private dev_proc_encode_color(display_encode_color_device8);
private dev_proc_decode_color(display_decode_color_device8);
private dev_proc_map_rgb_color(display_map_rgb_color_device16);
private dev_proc_map_color_rgb(display_map_color_rgb_device16);
private dev_proc_map_rgb_color(display_map_rgb_color_rgb);
private dev_proc_map_color_rgb(display_map_color_rgb_rgb);
private dev_proc_map_rgb_color(display_map_rgb_color_bgr24);
private dev_proc_map_color_rgb(display_map_color_rgb_bgr24);
private dev_proc_fill_rectangle(display_fill_rectangle);
private dev_proc_copy_mono(display_copy_mono);
private dev_proc_copy_color(display_copy_color);
private dev_proc_get_bits(display_get_bits);
private dev_proc_get_params(display_get_params);
private dev_proc_put_params(display_put_params);
private dev_proc_finish_copydevice(display_finish_copydevice);
private dev_proc_get_color_mapping_procs(display_separation_get_color_mapping_procs);
private dev_proc_get_color_comp_index(display_separation_get_color_comp_index);
private dev_proc_encode_color(display_separation_encode_color);
private dev_proc_decode_color(display_separation_decode_color);
private dev_proc_update_spot_equivalent_colors(display_update_spot_equivalent_colors);
private const gx_device_procs display_procs =
{
display_open,
display_get_initial_matrix,
display_sync_output,
display_output_page,
display_close,
gx_default_w_b_map_rgb_color,
gx_default_w_b_map_color_rgb,
display_fill_rectangle,
NULL,
display_copy_mono,
display_copy_color,
NULL,
display_get_bits,
display_get_params,
display_put_params,
gx_default_cmyk_map_cmyk_color,
gx_default_get_xfont_procs,
NULL,
NULL,
gx_page_device_get_page_device,
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
display_finish_copydevice,
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
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
display_update_spot_equivalent_colors
};
public_st_device_display();
private
ENUM_PTRS_WITH(display_enum_ptrs, gx_device_display *ddev)
if (index == 0) {
if (ddev->mdev) {
return ENUM_OBJ(gx_device_enum_ptr((gx_device *)ddev->mdev));
}
return 0;
}
else if (index-1 < ddev->devn_params.separations.num_separations)
ENUM_RETURN(ddev->devn_params.separations.names[index-1].data);
else
return 0;
ENUM_PTRS_END
private
RELOC_PTRS_WITH(display_reloc_ptrs, gx_device_display *ddev)
if (ddev->mdev) {
ddev->mdev = (gx_device_memory *)
gx_device_reloc_ptr((gx_device *)ddev->mdev, gcst);
}
{   int i;
for (i = 0; i < ddev->devn_params.separations.num_separations; ++i) {
RELOC_PTR(gx_device_display, devn_params.separations.names[i].data);
}
}
RELOC_PTRS_END
const gx_device_display gs_display_device =
{
std_device_std_body_type(gx_device_display, &display_procs, "display",
&st_device_display,
INITIAL_WIDTH, INITIAL_HEIGHT,
INITIAL_RESOLUTION, INITIAL_RESOLUTION),
{0},
NULL,
NULL,
NULL,
0,
NULL,
0,
0,
{
8,
DeviceCMYKComponents,
4,
0,
{0},
{0},
{0, 1, 2, 3, 4, 5, 6, 7 }
},
{ true }
};
private int display_check_structure(gx_device_display *dev);
private void display_free_bitmap(gx_device_display * dev);
private int display_alloc_bitmap(gx_device_display *, gx_device *);
private int display_set_color_format(gx_device_display *dev, int nFormat);
private int display_set_separations(gx_device_display *dev);
private int display_raster(gx_device_display *dev);
private int
display_open(gx_device * dev)
{
gx_device_display *ddev = (gx_device_display *) dev;
int ccode;
ddev->mdev = NULL;
ddev->pBitmap = NULL;
ddev->ulBitmapSize = 0;
if (ddev->callback == NULL)
return 0;
if ((ccode = display_check_structure(ddev)) < 0)
return_error(ccode);
if ((ccode = display_set_color_format(ddev, ddev->nFormat)) < 0)
return_error(ccode);
ccode = (*(ddev->callback->display_open))(ddev->pHandle, dev);
if (ccode < 0)
return_error(ccode);
ccode = (*(ddev->callback->display_presize)) (ddev->pHandle, dev,
dev->width, dev->height, display_raster(ddev), ddev->nFormat);
if (ccode < 0) {
(*(ddev->callback->display_close))(ddev->pHandle, dev);
return_error(ccode);
}
ccode = display_alloc_bitmap(ddev, dev);
if (ccode < 0) {
(*(ddev->callback->display_close))(ddev->pHandle, dev);
return_error(ccode);
}
ccode = (*(ddev->callback->display_size)) (ddev->pHandle, dev,
dev->width, dev->height, display_raster(ddev), ddev->nFormat,
ddev->mdev->base);
if (ccode < 0) {
display_free_bitmap(ddev);
(*(ddev->callback->display_close))(ddev->pHandle, dev);
return_error(ccode);
}
return 0;
}
private void
display_get_initial_matrix(gx_device * dev, gs_matrix * pmat)
{
gx_device_display *ddev = (gx_device_display *) dev;
if ((ddev->nFormat & DISPLAY_FIRSTROW_MASK) == DISPLAY_TOPFIRST)
gx_default_get_initial_matrix(dev, pmat);
else
gx_upright_get_initial_matrix(dev, pmat);
}
int
display_sync_output(gx_device * dev)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
display_set_separations(ddev);
(*(ddev->callback->display_sync))(ddev->pHandle, dev);
return (0);
}
int
display_output_page(gx_device * dev, int copies, int flush)
{
gx_device_display *ddev = (gx_device_display *) dev;
int code;
if (ddev->callback == NULL)
return 0;
display_set_separations(ddev);
code = (*(ddev->callback->display_page))
(ddev->pHandle, dev, copies, flush);
if (code >= 0)
code = gx_finish_output_page(dev, copies, flush);
return code;
}
private int
display_close(gx_device * dev)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
(*(ddev->callback->display_preclose))(ddev->pHandle, dev);
display_free_bitmap(ddev);
(*(ddev->callback->display_close))(ddev->pHandle, dev);
return 0;
}
private gx_color_index
gx_b_w_gray_encode(gx_device * dev, const gx_color_value cv[])
{
return 1 - (cv[0] >> (gx_color_value_bits - 1));
}
private gx_color_index
display_map_rgb_color_device4(gx_device * dev, const gx_color_value cv[])
{
return pc_4bit_map_rgb_color(dev, cv);
}
private int
display_map_color_rgb_device4(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
pc_4bit_map_color_rgb(dev, color, prgb);
return 0;
}
private gx_color_index
display_encode_color_device8(gx_device * dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
gx_color_value k = cv[3];
if ((r == 0) && (g == 0) && (b == 0)) {
k = ((k >> (gx_color_value_bits - 6)) + 1) >> 1;
if (k > 0x1f)
k = 0x1f;
return (k + 0x40);
}
if (k > 0) {
r = ((r+k) > gx_max_color_value) ? gx_max_color_value :
(gx_color_value)(r+k);
g = ((g+k) > gx_max_color_value) ? gx_max_color_value :
(gx_color_value)(g+k);
b = ((b+k) > gx_max_color_value) ? gx_max_color_value :
(gx_color_value)(b+k);
}
r = ((r >> (gx_color_value_bits - 3)) + 1) >> 1;
if (r > 0x3)
r = 0x3;
g = ((g >> (gx_color_value_bits - 3)) + 1) >> 1;
if (g > 0x3)
g = 0x3;
b = ((b >> (gx_color_value_bits - 3)) + 1) >> 1;
if (b > 0x3)
b = 0x3;
return (r << 4) + (g << 2) + b;
}
private int
display_decode_color_device8(gx_device * dev, gx_color_index color,
gx_color_value prgb[4])
{
gx_color_value one;
if (color < 64) {
one = (gx_color_value) (gx_max_color_value / 3);
prgb[0] = (gx_color_value) (((color >> 4) & 3) * one);
prgb[1] = (gx_color_value) (((color >> 2) & 3) * one);
prgb[2] = (gx_color_value) (((color) & 3) * one);
prgb[3] = 0;
}
else if (color < 96) {
one = (gx_color_value) (gx_max_color_value / 31);
prgb[0] = prgb[1] = prgb[2] = 0;
prgb[3] = (gx_color_value) ((color & 0x1f) * one);
}
else {
prgb[0] = prgb[1] = prgb[2] = prgb[3] = 0;
}
return 0;
}
private gx_color_index
display_map_rgb_color_device16(gx_device * dev, const gx_color_value cv[])
{
gx_device_display *ddev = (gx_device_display *) dev;
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN) {
if ((ddev->nFormat & DISPLAY_555_MASK) == DISPLAY_NATIVE_555)
return ((r >> (gx_color_value_bits - 5)) << 10) +
((g >> (gx_color_value_bits - 5)) << 5) +
(b >> (gx_color_value_bits - 5));
else
return ((r >> (gx_color_value_bits - 5)) << 11) +
((g >> (gx_color_value_bits - 6)) << 5) +
(b >> (gx_color_value_bits - 5));
}
if ((ddev->nFormat & DISPLAY_555_MASK) == DISPLAY_NATIVE_555)
return ((r >> (gx_color_value_bits - 5)) << 2) +
(((g >> (gx_color_value_bits - 5)) & 0x7) << 13) +
(((g >> (gx_color_value_bits - 5)) & 0x18) >> 3) +
((b >> (gx_color_value_bits - 5)) << 8);
return ((r >> (gx_color_value_bits - 5)) << 3) +
(((g >> (gx_color_value_bits - 6)) & 0x7) << 13) +
(((g >> (gx_color_value_bits - 6)) & 0x38) >> 3) +
((b >> (gx_color_value_bits - 5)) << 8);
}
private int
display_map_color_rgb_device16(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
gx_device_display *ddev = (gx_device_display *) dev;
ushort value;
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN) {
if ((ddev->nFormat & DISPLAY_555_MASK) == DISPLAY_NATIVE_555) {
value = (ushort) (color >> 10);
prgb[0] = (gx_color_value)
(((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits));
value = (ushort) ((color >> 5) & 0x1f);
prgb[1] = (gx_color_value)
(((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits));
value = (ushort) (color & 0x1f);
prgb[2] = (gx_color_value)
(((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits));
}
else {
value = (ushort) (color >> 11);
prgb[0] = ((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
value = (ushort) ((color >> 5) & 0x3f);
prgb[1] = (gx_color_value)
((value << 10) + (value << 4) + (value >> 2))
>> (16 - gx_color_value_bits);
value = (ushort) (color & 0x1f);
prgb[2] = (gx_color_value)
((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
}
}
else {
if ((ddev->nFormat & DISPLAY_555_MASK) == DISPLAY_NATIVE_555) {
value = (ushort) ((color >> 2) & 0x1f);
prgb[0] = (gx_color_value)
((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
value = (ushort)
(((color << 3) & 0x18) +  ((color >> 13) & 0x7));
prgb[1] = (gx_color_value)
((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
value = (ushort) ((color >> 8) & 0x1f);
prgb[2] = (gx_color_value)
((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits);
}
else {
value = (ushort) ((color >> 3) & 0x1f);
prgb[0] = (gx_color_value)
(((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits));
value = (ushort)
(((color << 3) & 0x38) +  ((color >> 13) & 0x7));
prgb[1] = (gx_color_value)
(((value << 10) + (value << 4) + (value >> 2))
>> (16 - gx_color_value_bits));
value = (ushort) ((color >> 8) & 0x1f);
prgb[2] = (gx_color_value)
(((value << 11) + (value << 6) + (value << 1) +
(value >> 4)) >> (16 - gx_color_value_bits));
}
}
return 0;
}
private gx_color_index
display_map_rgb_color_rgb(gx_device * dev, const gx_color_value cv[])
{
gx_device_display *ddev = (gx_device_display *) dev;
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
int drop = gx_color_value_bits - 8;
gx_color_value red, green, blue;
red  = r >> drop;
green = g >> drop;
blue = b >> drop;
switch (ddev->nFormat & DISPLAY_ALPHA_MASK) {
case DISPLAY_ALPHA_NONE:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN) {
gx_color_value rgb[3];
rgb[0] = r; rgb[1] = g; rgb[2] = b;
return gx_default_rgb_map_rgb_color(dev, rgb);
}
else
return (blue<<16) + (green<<8) + red;
case DISPLAY_ALPHA_FIRST:
case DISPLAY_UNUSED_FIRST:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN)
return ((gx_color_index)red<<16) + (green<<8) + blue;
else
return ((gx_color_index)blue<<16) + (green<<8) + red;
case DISPLAY_ALPHA_LAST:
case DISPLAY_UNUSED_LAST:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN)
return ((gx_color_index)red<<24) + (green<<16) + (blue<<8);
else
return ((gx_color_index)blue<<24) + (green<<16) + (red<<8);
}
return 0;
}
private int
display_map_color_rgb_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
gx_device_display *ddev = (gx_device_display *) dev;
uint bits_per_color = 8;
uint color_mask;
color_mask = (1 << bits_per_color) - 1;
switch (ddev->nFormat & DISPLAY_ALPHA_MASK) {
case DISPLAY_ALPHA_NONE:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN)
return gx_default_rgb_map_color_rgb(dev, color, prgb);
else {
prgb[0] = (gx_color_value) (((color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[1] = (gx_color_value)
(((color >> bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[2] = (gx_color_value)
(((color >> 2*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
}
break;
case DISPLAY_ALPHA_FIRST:
case DISPLAY_UNUSED_FIRST:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN) {
prgb[0] = (gx_color_value)
(((color >> 2*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[1] = (gx_color_value)
(((color >> bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[2] = (gx_color_value) (((color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
}
else {
prgb[0] = (gx_color_value)
(((color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[1] = (gx_color_value)
(((color >> bits_per_color)   & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[2] = (gx_color_value)
(((color >> 2*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
}
break;
case DISPLAY_ALPHA_LAST:
case DISPLAY_UNUSED_LAST:
if ((ddev->nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN) {
prgb[0] = (gx_color_value)
(((color >> 3*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[1] = (gx_color_value)
(((color >> 2*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[2] = (gx_color_value)
(((color >> bits_per_color)   & color_mask) *
(ulong) gx_max_color_value / color_mask);
}
else {
prgb[0] = (gx_color_value)
(((color >> bits_per_color)   & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[1] = (gx_color_value)
(((color >> 2*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
prgb[2] = (gx_color_value)
(((color >> 3*bits_per_color) & color_mask) *
(ulong) gx_max_color_value / color_mask);
}
}
return 0;
}
private gx_color_index
display_map_rgb_color_bgr24(gx_device * dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
return (gx_color_value_to_byte(b)<<16) +
(gx_color_value_to_byte(g)<<8) +
gx_color_value_to_byte(r);
}
private int
display_map_color_rgb_bgr24(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
prgb[0] = gx_color_value_from_byte(color & 0xff);
prgb[1] = gx_color_value_from_byte((color >> 8) & 0xff);
prgb[2] = gx_color_value_from_byte((color >> 16) & 0xff);
return 0;
}
private int
display_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
dev_proc(ddev->mdev, fill_rectangle)((gx_device *)ddev->mdev,
x, y, w, h, color);
if (ddev->callback->display_update)
(*(ddev->callback->display_update))(ddev->pHandle, dev, x, y, w, h);
return 0;
}
private int
display_copy_mono(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
dev_proc(ddev->mdev, copy_mono)((gx_device *)ddev->mdev,
base, sourcex, raster, id, x, y, w, h, zero, one);
if (ddev->callback->display_update)
(*(ddev->callback->display_update))(ddev->pHandle, dev, x, y, w, h);
return 0;
}
private int
display_copy_color(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
dev_proc(ddev->mdev, copy_color)((gx_device *)ddev->mdev,
base, sourcex, raster, id, x, y, w, h);
if (ddev->callback->display_update)
(*(ddev->callback->display_update))(ddev->pHandle, dev, x, y, w, h);
return 0;
}
private int
display_get_bits(gx_device * dev, int y, byte * str, byte ** actual_data)
{
gx_device_display *ddev = (gx_device_display *) dev;
if (ddev->callback == NULL)
return 0;
return dev_proc(ddev->mdev, get_bits)((gx_device *)ddev->mdev,
y, str, actual_data);
}
private int
display_get_params(gx_device * dev, gs_param_list * plist)
{
gx_device_display *ddev = (gx_device_display *) dev;
int code;
gs_param_string dhandle;
int idx;
int val;
int i = 0;
size_t dptr;
char buf[64];
idx = ((int)sizeof(size_t)) * 8 - 4;
buf[i++] = '1';
buf[i++] = '6';
buf[i++] = '#';
dptr = (size_t)(ddev->pHandle);
while (idx >= 0) {
val = (int)(dptr >> idx) & 0xf;
if (val <= 9)
buf[i++] = '0' + val;
else
buf[i++] = 'a' - 10 + val;
idx -= 4;
}
buf[i] = '\0';
param_string_from_transient_string(dhandle, buf);
code = gx_default_get_params(dev, plist);
(void)(code < 0 ||
(code = param_write_string(plist,
"DisplayHandle", &dhandle)) < 0 ||
(code = param_write_int(plist,
"DisplayFormat", &ddev->nFormat)) < 0 ||
(code = param_write_float(plist,
"DisplayResolution", &ddev->HWResolution[1])) < 0);
if (code >= 0 &&
(ddev->nFormat & DISPLAY_COLORS_MASK) == DISPLAY_COLORS_SEPARATION)
code = devn_get_params(dev, plist, &ddev->devn_params,
&ddev->equiv_cmyk_colors);
return code;
}
private int
display_put_params(gx_device * dev, gs_param_list * plist)
{
gx_device_display *ddev = (gx_device_display *) dev;
int ecode = 0, code;
bool is_open = dev->is_open;
gs_param_float_array hwra;
float dispres = 0.0;
int old_width = dev->width;
int old_height = dev->height;
int old_format = ddev->nFormat;
void *old_handle = ddev->pHandle;
gs_devn_params *pdevn_params = &ddev->devn_params;
equivalent_cmyk_color_params *pequiv_colors = &ddev->equiv_cmyk_colors;
gs_devn_params saved_devn_params = *pdevn_params;
equivalent_cmyk_color_params saved_equiv_colors = *pequiv_colors;
int format;
void *handle;
int found_string_handle = 0;
gs_param_string dh = { 0 };
switch (code = param_read_int(plist, "DisplayFormat", &format)) {
case 0:
if (dev->is_open) {
if (ddev->nFormat != format)
ecode = gs_error_rangecheck;
else
break;
}
else {
code = display_set_color_format(ddev, format);
if (code < 0)
ecode = code;
else
break;
}
goto cfe;
default:
ecode = code;
cfe:param_signal_error(plist, "DisplayFormat", ecode);
case 1:
break;
}
switch (code = param_read_string(plist, "DisplayHandle", &dh)) {
case 0:
found_string_handle = 1;
break;
default:
if ((code == gs_error_typecheck) && (sizeof(size_t) <= 4)) {
switch (code = param_read_long(plist, "DisplayHandle",
(long *)(&handle))) {
case 0:
if (dev->is_open) {
if (ddev->pHandle != handle)
ecode = gs_error_rangecheck;
else
break;
}
else {
ddev->pHandle = handle;
break;
}
goto hdle;
default:
ecode = code;
hdle:param_signal_error(plist, "DisplayHandle", ecode);
case 1:
break;
}
break;
}
ecode = code;
param_signal_error(plist, "DisplayHandle", ecode);
case 1:
dh.data = 0;
break;
}
if (found_string_handle) {
size_t ptr = 0;
int i;
int base = 10;
int val;
code = 0;
for (i=0; i<dh.size; i++) {
val = dh.data[i];
if ((val >= '0') && (val <= '9'))
val = val - '0';
else if ((val >= 'A') && (val <= 'F'))
val = val - 'A' + 10;
else if ((val >= 'a') && (val <= 'f'))
val = val - 'a' + 10;
else if (val == '#') {
base = (int)ptr;
ptr = 0;
if ((base != 10) && (base != 16)) {
code = gs_error_rangecheck;
break;
}
continue;
}
else {
code = gs_error_rangecheck;
break;
}
if (base == 10)
ptr = ptr * 10 + val;
else if (base == 16)
ptr = ptr * 16 + val;
else {
code = gs_error_rangecheck;
break;
}
}
if (code == 0) {
if (dev->is_open) {
if (ddev->pHandle != (void *)ptr)
code = gs_error_rangecheck;
}
else
ddev->pHandle = (void *)ptr;
}
if (code < 0) {
ecode = code;
param_signal_error(plist, "DisplayHandle", ecode);
}
}
if (param_read_float_array(plist, "HWResolution", &hwra) == 0)
ddev->HWResolution_set = 1;
switch (code = param_read_float(plist, "DisplayResolution", &dispres)) {
case 0:
if (!ddev->HWResolution_set) {
gx_device_set_resolution(dev, dispres, dispres);
ddev->HWResolution_set = 1;
}
break;
default:
ecode = code;
param_signal_error(plist, "DisplayResolution", ecode);
case 1:
break;
}
if (ecode >= 0 &&
(ddev->nFormat & DISPLAY_COLORS_MASK) == DISPLAY_COLORS_SEPARATION) {
ecode = devn_put_params(dev, plist, pdevn_params, pequiv_colors);
dev->color_info.depth = arch_sizeof_color_index * 8;
}
if (ecode >= 0) {
dev->is_open = false;
ecode = gx_default_put_params(dev, plist);
dev->is_open = is_open;
}
if (ecode < 0) {
*pdevn_params = saved_devn_params;
*pequiv_colors = saved_equiv_colors;
if (format != old_format)
display_set_color_format(ddev, old_format);
ddev->pHandle = old_handle;
dev->width = old_width;
dev->height = old_height;
return ecode;
}
if ( is_open && ddev->callback &&
((old_width != dev->width) || (old_height != dev->height)) ) {
if ((*ddev->callback->display_presize)(ddev->pHandle, dev,
dev->width, dev->height, display_raster(ddev),
ddev->nFormat) < 0) {
*pdevn_params = saved_devn_params;
*pequiv_colors = saved_equiv_colors;
display_set_color_format(ddev, old_format);
ddev->nFormat = old_format;
ddev->pHandle = old_handle;
dev->width = old_width;
dev->height = old_height;
return_error(gs_error_rangecheck);
}
display_free_bitmap(ddev);
code = display_alloc_bitmap(ddev, dev);
if (code < 0) {
(*ddev->callback->display_size)(ddev->pHandle, dev,
0, 0, 0, ddev->nFormat, NULL);
return_error(code);
}
if ((*ddev->callback->display_size)(ddev->pHandle, dev,
dev->width, dev->height, display_raster(ddev),
ddev->nFormat, ddev->mdev->base) < 0)
return_error(gs_error_rangecheck);
}
return 0;
}
int
display_finish_copydevice(gx_device *dev, const gx_device *from_dev)
{
gx_device_display *ddev = (gx_device_display *) dev;
ddev->is_open = false;
ddev->mdev = NULL;
ddev->pBitmap = NULL;
ddev->ulBitmapSize = 0;
return 0;
}
private void
display_separation_gray_cs_to_cmyk_cm(gx_device * dev, frac gray, frac out[])
{
int * map =
(int *)(&((gx_device_display *) dev)->devn_params.separation_order_map);
gray_cs_to_devn_cm(dev, map, gray, out);
}
private void
display_separation_rgb_cs_to_cmyk_cm(gx_device * dev,
const gs_imager_state *pis, frac r, frac g, frac b, frac out[])
{
int * map =
(int *)(&((gx_device_display *) dev)->devn_params.separation_order_map);
rgb_cs_to_devn_cm(dev, map, pis, r, g, b, out);
}
private void
display_separation_cmyk_cs_to_cmyk_cm(gx_device * dev,
frac c, frac m, frac y, frac k, frac out[])
{
int * map =
(int *)(&((gx_device_display *) dev)->devn_params.separation_order_map);
cmyk_cs_to_devn_cm(dev, map, c, m, y, k, out);
}
private const gx_cm_color_map_procs display_separation_cm_procs = {
display_separation_gray_cs_to_cmyk_cm,
display_separation_rgb_cs_to_cmyk_cm,
display_separation_cmyk_cs_to_cmyk_cm
};
private const gx_cm_color_map_procs *
display_separation_get_color_mapping_procs(const gx_device * dev)
{
return &display_separation_cm_procs;
}
private gx_color_index
display_separation_encode_color(gx_device *dev, const gx_color_value colors[])
{
int bpc = ((gx_device_display *)dev)->devn_params.bitspercomponent;
int drop = sizeof(gx_color_value) * 8 - bpc;
gx_color_index color = 0;
int i = 0;
int ncomp = dev->color_info.num_components;
for (; i<ncomp; i++) {
color <<= bpc;
color |= (colors[i] >> drop);
}
if (bpc*ncomp < arch_sizeof_color_index * 8)
color <<= (arch_sizeof_color_index * 8 - ncomp * bpc);
return (color == gx_no_color_index ? color ^ 1 : color);
}
private int
display_separation_decode_color(gx_device * dev, gx_color_index color,
gx_color_value * out)
{
int bpc = ((gx_device_display *)dev)->devn_params.bitspercomponent;
int drop = sizeof(gx_color_value) * 8 - bpc;
int mask = (1 << bpc) - 1;
int i = 0;
int ncomp = dev->color_info.num_components;
if (bpc*ncomp < arch_sizeof_color_index * 8)
color >>= (arch_sizeof_color_index * 8 - ncomp * bpc);
for (; i<ncomp; i++) {
out[ncomp - i - 1] = (gx_color_value) ((color & mask) << drop);
color >>= bpc;
}
return 0;
}
private int
display_update_spot_equivalent_colors(gx_device * dev, const gs_state * pgs)
{
gx_device_display * ddev = (gx_device_display *)dev;
if ((ddev->nFormat & DISPLAY_COLORS_MASK) == DISPLAY_COLORS_SEPARATION)
update_spot_equivalent_cmyk_colors(dev, pgs,
&ddev->devn_params, &ddev->equiv_cmyk_colors);
return 0;
}
private int
display_separation_get_color_comp_index(gx_device * dev,
const char * pname, int name_size, int component_type)
{
return devn_get_color_comp_index(dev,
&(((gx_device_display *)dev)->devn_params),
&(((gx_device_display *)dev)->equiv_cmyk_colors),
pname, name_size, component_type, ENABLE_AUTO_SPOT_COLORS);
}
private int display_check_structure(gx_device_display *ddev)
{
if (ddev->callback == 0)
return_error(gs_error_rangecheck);
if (ddev->callback->size == sizeof(struct display_callback_v1_s)) {
if (ddev->callback->version_major != DISPLAY_VERSION_MAJOR_V1)
return_error(gs_error_rangecheck);
if (ddev->callback->version_minor > DISPLAY_VERSION_MINOR_V1)
return_error(gs_error_rangecheck);
}
else {
if (ddev->callback->size != sizeof(display_callback))
return_error(gs_error_rangecheck);
if (ddev->callback->version_major != DISPLAY_VERSION_MAJOR)
return_error(gs_error_rangecheck);
if (ddev->callback->version_minor > DISPLAY_VERSION_MINOR)
return_error(gs_error_rangecheck);
}
if ((ddev->callback->display_open == NULL) ||
(ddev->callback->display_close == NULL) ||
(ddev->callback->display_presize == NULL) ||
(ddev->callback->display_size == NULL) ||
(ddev->callback->display_sync == NULL) ||
(ddev->callback->display_page == NULL))
return_error(gs_error_rangecheck);
return 0;
}
private void
display_free_bitmap(gx_device_display * ddev)
{
if (ddev->callback == NULL)
return;
if (ddev->pBitmap) {
if (ddev->callback->display_memalloc
&& ddev->callback->display_memfree
&& ddev->pBitmap) {
(*ddev->callback->display_memfree)(ddev->pHandle, ddev,
ddev->pBitmap);
}
else {
gs_free_object(ddev->memory->non_gc_memory,
ddev->pBitmap, "display_free_bitmap");
}
ddev->pBitmap = NULL;
if (ddev->mdev)
ddev->mdev->base = NULL;
}
if (ddev->mdev) {
dev_proc(ddev->mdev, close_device)((gx_device *)ddev->mdev);
gx_device_retain((gx_device *)(ddev->mdev), false);
ddev->mdev = NULL;
}
}
private int
display_raster(gx_device_display *dev)
{
int align = 0;
int bytewidth = dev->width * dev->color_info.depth/8;
switch (dev->nFormat & DISPLAY_ROW_ALIGN_MASK) {
case DISPLAY_ROW_ALIGN_4:
align = 4;
break;
case DISPLAY_ROW_ALIGN_8:
align = 8;
break;
case DISPLAY_ROW_ALIGN_16:
align = 16;
break;
case DISPLAY_ROW_ALIGN_32:
align = 32;
break;
case DISPLAY_ROW_ALIGN_64:
align = 64;
break;
}
if (align < ARCH_ALIGN_PTR_MOD)
align = ARCH_ALIGN_PTR_MOD;
align -= 1;
bytewidth = (bytewidth + align) & (~align);
return bytewidth;
}
private int
display_alloc_bitmap(gx_device_display * ddev, gx_device * param_dev)
{
int ccode;
const gx_device_memory *mdproto;
if (ddev->callback == NULL)
return 0;
display_free_bitmap(ddev);
mdproto = gdev_mem_device_for_bits(ddev->color_info.depth);
if (mdproto == 0)
return_error(gs_error_rangecheck);
ddev->mdev = gs_alloc_struct(gs_memory_stable(ddev->memory),
gx_device_memory, &st_device_memory, "display_memory_device");
if (ddev->mdev == 0)
return_error(gs_error_VMerror);
gs_make_mem_device(ddev->mdev, mdproto, gs_memory_stable(ddev->memory),
0, (gx_device *) NULL);
check_device_separable((gx_device *)(ddev->mdev));
gx_device_fill_in_procs((gx_device *)(ddev->mdev));
gx_device_retain((gx_device *)(ddev->mdev), true);
ddev->mdev->width = param_dev->width;
ddev->mdev->width = display_raster(ddev) * 8 / ddev->color_info.depth;
ddev->mdev->height = param_dev->height;
ddev->mdev->line_pointer_memory = ddev->mdev->memory;
ddev->ulBitmapSize = gdev_mem_bits_size(ddev->mdev,
ddev->mdev->width, ddev->mdev->height);
if (ddev->callback->display_memalloc
&& ddev->callback->display_memfree) {
ddev->pBitmap = (*ddev->callback->display_memalloc)(ddev->pHandle,
ddev, ddev->ulBitmapSize);
}
else {
ddev->pBitmap = gs_alloc_byte_array_immovable(ddev->memory->non_gc_memory,
(uint)ddev->ulBitmapSize, 1, "display_alloc_bitmap");
}
if (ddev->pBitmap == NULL) {
ddev->mdev->width = 0;
ddev->mdev->height = 0;
return_error(gs_error_VMerror);
}
ddev->mdev->base = (byte *) ddev->pBitmap;
ddev->mdev->foreign_bits = true;
ccode = dev_proc(ddev->mdev, open_device)((gx_device *)ddev->mdev);
if (ccode < 0)
display_free_bitmap(ddev);
if (ccode == 0) {
int i;
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
for (i=0; i<GX_DEVICE_COLOR_MAX_COMPONENTS; i++)
cv[i] = (ddev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
? gx_max_color_value : 0;
dev_proc(ddev, fill_rectangle)((gx_device *)ddev,
0, 0, ddev->width, ddev->height,
ddev->procs.encode_color((gx_device *)ddev, cv));
}
return ccode;
}
private int
display_set_separations(gx_device_display *dev)
{
if (((dev->nFormat & DISPLAY_COLORS_MASK) == DISPLAY_COLORS_SEPARATION) &&
(dev->callback->version_major > DISPLAY_VERSION_MAJOR_V1) &&
(dev->callback->display_separation != NULL)) {
char name[64];
int num_spot = dev->devn_params.separations.num_separations;
int num_std_colorants = dev->devn_params.num_std_colorant_names;
int num_comp = num_std_colorants + num_spot;
int comp_map[GX_DEVICE_COLOR_MAX_COMPONENTS];
int comp_num;
int sep_num;
int sep_name_size;
unsigned int c, m, y, k;
memset(comp_map, 0, sizeof(comp_map));
for (sep_num = 0; sep_num < num_comp; sep_num++) {
comp_num = dev->devn_params.separation_order_map[sep_num];
if (comp_num >= 0 && comp_num < GX_DEVICE_COLOR_MAX_COMPONENTS)
comp_map[comp_num] = sep_num;
}
for (comp_num = 0; comp_num < num_comp; comp_num++) {
c = y = m = k = 0;
sep_num = comp_map[comp_num];
if (sep_num < dev->devn_params.num_std_colorant_names) {
sep_name_size =
strlen(dev->devn_params.std_colorant_names[sep_num]);
if (sep_name_size > sizeof(name)-2)
sep_name_size = sizeof(name)-1;
memcpy(name, dev->devn_params.std_colorant_names[sep_num],
sep_name_size);
name[sep_name_size] = '\0';
switch (sep_num) {
case 0: c = 65535; break;
case 1: m = 65535; break;
case 2: y = 65535; break;
case 3: k = 65535; break;
}
}
else {
sep_num -= dev->devn_params.num_std_colorant_names;
sep_name_size =
dev->devn_params.separations.names[sep_num].size;
if (sep_name_size > sizeof(name)-2)
sep_name_size = sizeof(name)-1;
memcpy(name, dev->devn_params.separations.names[sep_num].data,
sep_name_size);
name[sep_name_size] = '\0';
if (dev->equiv_cmyk_colors.color[sep_num].color_info_valid) {
c = dev->equiv_cmyk_colors.color[sep_num].c
* 65535 / frac_1;
m = dev->equiv_cmyk_colors.color[sep_num].m
* 65535 / frac_1;
y = dev->equiv_cmyk_colors.color[sep_num].y
* 65535 / frac_1;
k = dev->equiv_cmyk_colors.color[sep_num].k
* 65535 / frac_1;
}
}
(*dev->callback->display_separation)(dev->pHandle, dev,
comp_num, name,
(unsigned short)c, (unsigned short)m,
(unsigned short)y, (unsigned short)k);
}
}
return 0;
}
typedef enum DISPLAY_MODEL_e {
DISPLAY_MODEL_GRAY=0,
DISPLAY_MODEL_RGB=1,
DISPLAY_MODEL_RGBK=2,
DISPLAY_MODEL_CMYK=3,
DISPLAY_MODEL_SEP=4
} DISPLAY_MODEL;
private void
set_color_info(gx_device_color_info * pdci, DISPLAY_MODEL model,
int nc, int depth, int maxgray, int maxcolor)
{
pdci->num_components = pdci->max_components = nc;
pdci->depth = depth;
pdci->gray_index = 0;
pdci->max_gray = maxgray;
pdci->max_color = maxcolor;
pdci->dither_grays = maxgray + 1;
pdci->dither_colors = maxcolor + 1;
pdci->separable_and_linear = GX_CINFO_UNKNOWN_SEP_LIN;
switch (model) {
case DISPLAY_MODEL_GRAY:
pdci->polarity = GX_CINFO_POLARITY_ADDITIVE;
pdci->cm_name = "DeviceGray";
pdci->gray_index = 0;
break;
case DISPLAY_MODEL_RGB:
pdci->polarity = GX_CINFO_POLARITY_ADDITIVE;
pdci->cm_name = "DeviceRGB";
pdci->gray_index = GX_CINFO_COMP_NO_INDEX;
break;
case DISPLAY_MODEL_RGBK:
pdci->polarity = GX_CINFO_POLARITY_ADDITIVE;
pdci->cm_name = "DeviceRGBK";
pdci->gray_index = 3;
break;
case DISPLAY_MODEL_CMYK:
pdci->polarity = GX_CINFO_POLARITY_SUBTRACTIVE;
pdci->cm_name = "DeviceCMYK";
pdci->gray_index = 3;
break;
default:
case DISPLAY_MODEL_SEP:
pdci->polarity = GX_CINFO_POLARITY_SUBTRACTIVE;
pdci->cm_name = "DeviceCMYK";
pdci->gray_index = GX_CINFO_COMP_NO_INDEX;
break;
}
}
private void
set_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device),
dev_t_proc_get_color_mapping_procs((*get_color_mapping_procs), gx_device),
dev_t_proc_get_color_comp_index((*get_color_comp_index), gx_device))
{
#if 0
pdev->procs.map_rgb_color = encode_color;
pdev->procs.map_color_rgb = decode_color;
#endif
pdev->procs.get_color_mapping_procs = get_color_mapping_procs;
pdev->procs.get_color_comp_index = get_color_comp_index;
pdev->procs.encode_color = encode_color;
pdev->procs.decode_color = decode_color;
}
private void
set_gray_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevGray_get_color_mapping_procs,
gx_default_DevGray_get_color_comp_index);
}
private void
set_rgb_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevRGB_get_color_mapping_procs,
gx_default_DevRGB_get_color_comp_index);
}
private void
set_rgbk_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevRGBK_get_color_mapping_procs,
gx_default_DevRGBK_get_color_comp_index);
}
private void
set_cmyk_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevCMYK_get_color_mapping_procs,
gx_default_DevCMYK_get_color_comp_index);
}
private int
display_set_color_format(gx_device_display *ddev, int nFormat)
{
gx_device * pdev = (gx_device *) ddev;
gx_device_color_info dci = ddev->color_info;
int bpc;
int bpp;
int maxvalue;
int align;
switch (nFormat & DISPLAY_DEPTH_MASK) {
case DISPLAY_DEPTH_1:
bpc = 1;
break;
case DISPLAY_DEPTH_2:
bpc = 2;
break;
case DISPLAY_DEPTH_4:
bpc = 4;
break;
case DISPLAY_DEPTH_8:
bpc = 8;
break;
case DISPLAY_DEPTH_12:
bpc = 12;
break;
case DISPLAY_DEPTH_16:
bpc = 16;
break;
default:
return_error(gs_error_rangecheck);
}
maxvalue = (1 << bpc) - 1;
ddev->devn_params.bitspercomponent = bpc;
switch (ddev->nFormat & DISPLAY_ROW_ALIGN_MASK) {
case DISPLAY_ROW_ALIGN_DEFAULT:
align = ARCH_ALIGN_PTR_MOD;
break;
case DISPLAY_ROW_ALIGN_4:
align = 4;
break;
case DISPLAY_ROW_ALIGN_8:
align = 8;
break;
case DISPLAY_ROW_ALIGN_16:
align = 16;
break;
case DISPLAY_ROW_ALIGN_32:
align = 32;
break;
case DISPLAY_ROW_ALIGN_64:
align = 64;
break;
default:
align = 0;
}
if (align < ARCH_ALIGN_PTR_MOD)
return_error(gs_error_rangecheck);
switch (ddev->nFormat & DISPLAY_ALPHA_MASK) {
case DISPLAY_ALPHA_FIRST:
case DISPLAY_ALPHA_LAST:
return_error(gs_error_rangecheck);
}
switch (nFormat & DISPLAY_COLORS_MASK) {
case DISPLAY_COLORS_NATIVE:
switch (nFormat & DISPLAY_DEPTH_MASK) {
case DISPLAY_DEPTH_1:
set_color_info(&dci, DISPLAY_MODEL_GRAY, 1, 1, 1, 0);
dci.separable_and_linear = GX_CINFO_SEP_LIN_NONE;
set_gray_color_procs(pdev, gx_b_w_gray_encode,
gx_default_b_w_map_color_rgb);
break;
case DISPLAY_DEPTH_4:
set_color_info(&dci, DISPLAY_MODEL_RGB, 3, 4, 3, 2);
dci.separable_and_linear = GX_CINFO_SEP_LIN_NONE;
set_rgb_color_procs(pdev, display_map_rgb_color_device4,
display_map_color_rgb_device4);
break;
case DISPLAY_DEPTH_8:
set_color_info(&dci, DISPLAY_MODEL_RGBK, 4, 8, 31, 3);
dci.separable_and_linear = GX_CINFO_SEP_LIN_NONE;
set_rgbk_color_procs(pdev, display_encode_color_device8,
display_decode_color_device8);
break;
case DISPLAY_DEPTH_16:
if ((ddev->nFormat & DISPLAY_555_MASK)
== DISPLAY_NATIVE_555)
set_color_info(&dci, DISPLAY_MODEL_RGB, 3, 16, 31, 31);
else
set_color_info(&dci, DISPLAY_MODEL_RGB, 3, 16, 63, 63);
set_rgb_color_procs(pdev, display_map_rgb_color_device16,
display_map_color_rgb_device16);
break;
default:
return_error(gs_error_rangecheck);
}
dci.gray_index = GX_CINFO_COMP_NO_INDEX;
break;
case DISPLAY_COLORS_GRAY:
set_color_info(&dci, DISPLAY_MODEL_GRAY, 1, bpc, maxvalue, 0);
if (bpc == 1)
set_gray_color_procs(pdev, gx_default_gray_encode,
gx_default_w_b_map_color_rgb);
else
set_gray_color_procs(pdev, gx_default_gray_encode,
gx_default_gray_map_color_rgb);
break;
case DISPLAY_COLORS_RGB:
if ((nFormat & DISPLAY_ALPHA_MASK) == DISPLAY_ALPHA_NONE)
bpp = bpc * 3;
else
bpp = bpc * 4;
set_color_info(&dci, DISPLAY_MODEL_RGB, 3, bpp, maxvalue, maxvalue);
if (((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_8) &&
((nFormat & DISPLAY_ALPHA_MASK) == DISPLAY_ALPHA_NONE)) {
if ((nFormat & DISPLAY_ENDIAN_MASK) == DISPLAY_BIGENDIAN)
set_rgb_color_procs(pdev, gx_default_rgb_map_rgb_color,
gx_default_rgb_map_color_rgb);
else
set_rgb_color_procs(pdev, display_map_rgb_color_bgr24,
display_map_color_rgb_bgr24);
}
else {
set_rgb_color_procs(pdev, display_map_rgb_color_rgb,
display_map_color_rgb_rgb);
}
break;
case DISPLAY_COLORS_CMYK:
bpp = bpc * 4;
set_color_info(&dci, DISPLAY_MODEL_CMYK, 4, bpp, maxvalue, maxvalue);
if ((nFormat & DISPLAY_ALPHA_MASK) != DISPLAY_ALPHA_NONE)
return_error(gs_error_rangecheck);
if ((nFormat & DISPLAY_ENDIAN_MASK) != DISPLAY_BIGENDIAN)
return_error(gs_error_rangecheck);
if ((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_1)
set_cmyk_color_procs(pdev, cmyk_1bit_map_cmyk_color,
cmyk_1bit_map_color_cmyk);
else if ((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_8)
set_cmyk_color_procs(pdev, cmyk_8bit_map_cmyk_color,
cmyk_8bit_map_color_cmyk);
else
return_error(gs_error_rangecheck);
break;
case DISPLAY_COLORS_SEPARATION:
if ((nFormat & DISPLAY_ENDIAN_MASK) != DISPLAY_BIGENDIAN)
return_error(gs_error_rangecheck);
bpp = arch_sizeof_color_index * 8;
set_color_info(&dci, DISPLAY_MODEL_SEP, bpp/bpc, bpp,
maxvalue, maxvalue);
if ((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_8) {
ddev->devn_params.bitspercomponent = bpc;
set_color_procs(pdev,
display_separation_encode_color,
display_separation_decode_color,
display_separation_get_color_mapping_procs,
display_separation_get_color_comp_index);
}
else
return_error(gs_error_rangecheck);
break;
default:
return_error(gs_error_rangecheck);
}
dci.anti_alias = ddev->color_info.anti_alias;
ddev->color_info = dci;
check_device_separable(pdev);
switch (nFormat & DISPLAY_COLORS_MASK) {
case DISPLAY_COLORS_NATIVE:
ddev->color_info.gray_index = GX_CINFO_COMP_NO_INDEX;
if ((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_1)
ddev->color_info.gray_index = 0;
else if ((nFormat & DISPLAY_DEPTH_MASK) == DISPLAY_DEPTH_8)
ddev->color_info.gray_index = 3;
break;
case DISPLAY_COLORS_RGB:
ddev->color_info.gray_index = GX_CINFO_COMP_NO_INDEX;
break;
case DISPLAY_COLORS_GRAY:
ddev->color_info.gray_index = 0;
break;
case DISPLAY_COLORS_CMYK:
ddev->color_info.gray_index = 3;
break;
case DISPLAY_COLORS_SEPARATION:
ddev->color_info.gray_index = GX_CINFO_COMP_NO_INDEX;
break;
}
ddev->nFormat = nFormat;
return 0;
}