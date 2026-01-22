#include "memory_.h"
#include "gconfigv.h"
#include "gx.h"
#include "gserrors.h"
#include "gxarith.h"
#include "gxdevice.h"
#include "gdevpccm.h"
#include "gdevpcfb.h"
#include "gdevsvga.h"
#include "gsparam.h"
#define first_dc_index 64
private int next_dc_index;
#define dc_hash_size 293
typedef struct {
ushort rgb, index;
} dc_entry;
private dc_entry dynamic_colors[dc_hash_size + 1];
#define num_colors 255
#define fb_dev ((gx_device_svga *)dev)
#define svga_procs(open) {\
open, NULL ,\
NULL , NULL , svga_close,\
svga_map_rgb_color, svga_map_color_rgb,\
svga_fill_rectangle, NULL ,\
svga_copy_mono, svga_copy_color, NULL ,\
svga_get_bits, NULL , svga_put_params,\
NULL , NULL ,\
NULL , NULL ,\
gx_page_device_get_page_device, NULL ,\
svga_copy_alpha\
}
private int svga_save_mode = -1;
#define regen 0xa000
#define set_pixel_ptr(ptr, fbdev, x, y, wnum)\
{	ulong index = (ulong)(y) * fbdev->raster + (uint)(x);\
if ( (uint)(index >> 16) != fbdev->current_page )\
{	(*fbdev->set_page)(fbdev, (fbdev->current_page = index >> 16), wnum);\
}\
ptr = (fb_ptr)MK_PTR(regen, (ushort)index);\
}
#define set_pixel_write_ptr(ptr, fbdev, x, y)\
set_pixel_ptr(ptr, fbdev, x, y, fbdev->wnum_write)
#define set_pixel_read_ptr(ptr, fbdev, x, y)\
set_pixel_ptr(ptr, fbdev, x, y, fbdev->wnum_read)
int
svga_find_mode(gx_device * dev, const mode_info * mip)
{
for (;; mip++) {
if (mip->width >= fb_dev->width &&
mip->height >= fb_dev->height ||
mip[1].mode < 0
) {
fb_dev->mode = mip;
gx_device_adjust_resolution(dev, mip->width, mip->height, 1);
fb_dev->raster = fb_dev->width;
return 0;
}
}
return_error(gs_error_rangecheck);
}
#define svga_dac_set_write_index(i) outportb(0x3c8, i)
#define svga_dac_write(r, g, b)\
(outportb(0x3c9, r), outportb(0x3c9, g), outportb(0x3c9, b))
#define cv_bits(v,n) (v >> (gx_color_value_bits - n))
void
svga_init_colors(gx_device * dev)
{
if (fb_dev->fixed_colors)
next_dc_index = num_colors;
else {
memset(dynamic_colors, 0,
(dc_hash_size + 1) * sizeof(dc_entry));
next_dc_index = first_dc_index;
}
}
private void
svga_load_colors(gx_device * dev)
{
int ci;
svga_dac_set_write_index(0);
if (fb_dev->fixed_colors)
for (ci = 0; ci < num_colors; ci++) {
gx_color_value rgb[3];
pc_8bit_map_color_rgb(dev, (gx_color_index) ci, rgb);
svga_dac_write(cv_bits(rgb[0], 6), cv_bits(rgb[1], 6),
cv_bits(rgb[2], 6));
} else
for (ci = 0; ci < 64; ci++) {
static const byte c2[10] =
{0, 42, 0, 0, 0, 0, 0, 0, 21, 63};
svga_dac_write(c2[(ci >> 2) & 9], c2[(ci >> 1) & 9],
c2[ci & 9]);
}
}
int
svga_open(gx_device * dev)
{
fb_dev->x_pixels_per_inch =
fb_dev->y_pixels_per_inch =
fb_dev->height / PAGE_HEIGHT_INCHES;
if (svga_save_mode < 0)
svga_save_mode = (*fb_dev->get_mode) ();
(*fb_dev->set_mode) (fb_dev->mode->mode);
svga_init_colors(dev);
svga_load_colors(dev);
fb_dev->current_page = -1;
return 0;
}
int
svga_close(gx_device * dev)
{
if (svga_save_mode >= 0)
(*fb_dev->set_mode) (svga_save_mode);
svga_save_mode = -1;
return 0;
}
gx_color_index
svga_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
ushort rgb;
gx_color_value r = cv[0], g = cv[1], b = cv[2];
if (fb_dev->fixed_colors) {
gx_color_index ci = pc_8bit_map_rgb_color(dev, cv);
return ci;
} {
ushort r5 = cv_bits(r, 5), g5 = cv_bits(g, 5), b5 = cv_bits(b, 5);
static const byte cube_bits[32] =
{0, 128, 128, 128, 128, 128, 128, 128, 128, 128,
8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
1, 128, 128, 128, 128, 128, 128, 128, 128, 128,
9
};
uint cx = ((uint) cube_bits[r5] << 2) +
((uint) cube_bits[g5] << 1) +
(uint) cube_bits[b5];
if (cx < 64)
return (gx_color_index) cx;
rgb = (r5 << 10) + (g5 << 5) + b5;
}
{
register dc_entry *pdc;
for (pdc = &dynamic_colors[rgb % dc_hash_size];
pdc->rgb != 0; pdc++
)
if (pdc->rgb == rgb)
return (gx_color_index) (pdc->index);
if (pdc == &dynamic_colors[dc_hash_size]) {
for (pdc = &dynamic_colors[0]; pdc->rgb != 0; pdc++)
if (pdc->rgb == rgb)
return (gx_color_index) (pdc->index);
}
if (next_dc_index == num_colors) {
return gx_no_color_index;
}
{
int i = next_dc_index++;
pdc->rgb = rgb;
pdc->index = i;
svga_dac_set_write_index(i);
svga_dac_write(cv_bits(r, 6), cv_bits(g, 6),
cv_bits(b, 6));
return (gx_color_index) i;
}
}
}
int
svga_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
uint cval;
outportb(0x3c7, (byte) color);
#define dacin() (cval = inportb(0x3c9) >> 1,\
((cval << 11) + (cval << 6) + (cval << 1) + (cval >> 4)) >>\
(16 - gx_color_value_bits))
prgb[0] = dacin();
prgb[1] = dacin();
prgb[2] = dacin();
#undef dacin
return 0;
}
int
svga_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
uint raster = fb_dev->raster;
ushort limit = (ushort) - raster;
int yi;
fb_ptr ptr;
fit_fill(dev, x, y, w, h);
set_pixel_write_ptr(ptr, fb_dev, x, y);
yi = h;
switch (w) {
case 0:
return 0;
case 1:
while (--yi >= 0 && PTR_OFF(ptr) < limit)
ptr[0] = (byte) color,
ptr += raster;
if (!++yi)
return 0;
break;
case 2:
while (--yi >= 0 && PTR_OFF(ptr) < limit)
ptr[0] = ptr[1] = (byte) color,
ptr += raster;
if (!++yi)
return 0;
break;
case 3:
while (--yi >= 0 && PTR_OFF(ptr) < limit)
ptr[0] = ptr[1] = ptr[2] = (byte) color,
ptr += raster;
if (!++yi)
return 0;
break;
case 4:
while (--yi >= 0 && PTR_OFF(ptr) < limit)
ptr[0] = ptr[1] = ptr[2] = ptr[3] = (byte) color,
ptr += raster;
if (!++yi)
return 0;
break;
default:
if (w < 0)
return 0;
if (w == dev->width && h == dev->height &&
color < first_dc_index
)
svga_init_colors(dev);
}
while (--yi >= 0) {
if (PTR_OFF(ptr) < limit) {
memset(ptr, (byte) color, w);
ptr += raster;
} else if (PTR_OFF(ptr) <= (ushort) (-w)) {
memset(ptr, (byte) color, w);
if (yi > 0)
set_pixel_write_ptr(ptr, fb_dev, x, y + h - yi);
} else {
uint left = (uint) 0x10000 - PTR_OFF(ptr);
memset(ptr, (byte) color, left);
set_pixel_write_ptr(ptr, fb_dev, x + left, y + h - 1 - yi);
memset(ptr, (byte) color, w - left);
ptr += raster - left;
}
}
return 0;
}
int
svga_copy_mono(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h, gx_color_index czero, gx_color_index cone)
{
uint raster = fb_dev->raster;
ushort limit;
register int wi;
uint skip;
int yi;
register fb_ptr ptr = (fb_ptr) 0;
const byte *srow;
uint invert;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
limit = (ushort) - w;
skip = raster - w + 1;
srow = base + (sourcex >> 3);
#define izero (int)czero
#define ione (int)cone
if (ione == no_color) {
gx_color_index temp;
if (izero == no_color)
return 0;
temp = czero;
czero = cone;
cone = temp;
invert = ~0;
} else
invert = 0;
if (izero != no_color)
svga_fill_rectangle(dev, x, y, w, h, czero);
for (yi = 0; yi < h; yi++) {
const byte *sptr = srow;
uint bits;
int bitno = sourcex & 7;
wi = w;
if (PTR_OFF(ptr) <= skip) {
set_pixel_write_ptr(ptr, fb_dev, x, y + yi);
} else if (PTR_OFF(ptr) > limit) {
int xi = (ushort) - PTR_OFF(ptr);
svga_copy_mono(dev, srow, sourcex & 7, sraster,
gx_no_bitmap_id, x, y + yi, xi, 1,
gx_no_color_index, cone);
set_pixel_write_ptr(ptr, fb_dev, x + xi, y + yi);
sptr = srow - (sourcex >> 3) + ((sourcex + xi) >> 3);
bitno = (sourcex + xi) & 7;
wi -= xi;
}
bits = *sptr ^ invert;
switch (bitno) {
#define ifbit(msk)\
if ( bits & msk ) *ptr = (byte)ione;\
if ( !--wi ) break; ptr++
case 0:
bit0:ifbit(0x80);
case 1:
ifbit(0x40);
case 2:
ifbit(0x20);
case 3:
ifbit(0x10);
case 4:
ifbit(0x08);
case 5:
ifbit(0x04);
case 6:
ifbit(0x02);
case 7:
ifbit(0x01);
#undef ifbit
bits = *++sptr ^ invert;
goto bit0;
}
ptr += skip;
srow += sraster;
}
#undef izero
#undef ione
return 0;
}
int
svga_copy_color(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h)
{
int xi, yi;
int skip;
const byte *sptr;
fb_ptr ptr;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
skip = sraster - w;
sptr = base + sourcex;
for (yi = y; yi - y < h; yi++) {
ptr = 0;
for (xi = x; xi - x < w; xi++) {
if (PTR_OFF(ptr) == 0)
set_pixel_write_ptr(ptr, fb_dev, xi, yi);
*ptr++ = *sptr++;
}
sptr += skip;
}
return 0;
}
int
svga_put_params(gx_device * dev, gs_param_list * plist)
{
int ecode = 0;
int code;
const char *param_name;
if ((code = ecode) < 0 ||
(code = gx_default_put_params(dev, plist)) < 0
) {
}
return code;
}
int
svga_get_bits(gx_device * dev, int y, byte * data, byte ** actual_data)
{
uint bytes_per_row = dev->width;
ushort limit = (ushort) - bytes_per_row;
fb_ptr src;
if (y < 0 || y >= dev->height)
return gs_error_rangecheck;
set_pixel_read_ptr(src, fb_dev, 0, y);
if (PTR_OFF(src) <= limit)
memcpy(data, src, bytes_per_row);
else {
uint left = (uint) 0x10000 - PTR_OFF(src);
memcpy(data, src, left);
set_pixel_read_ptr(src, fb_dev, left, y);
memcpy(data + left, src, bytes_per_row - left);
}
if (actual_data != 0)
*actual_data = data;
return 0;
}
private int
svga_copy_alpha(gx_device * dev, const byte * base, int sourcex,
int sraster, gx_bitmap_id id, int x, int y, int w, int h,
gx_color_index color, int depth)
{
int xi, yi;
int skip;
const byte *sptr;
byte mask;
int ishift;
byte shades[16];
gx_color_value rgb[3];
int log2_depth = depth >> 1;
int n1 = (1 << depth) - 1;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
shades[0] = (byte) svga_map_rgb_color(dev, gx_max_color_value,
gx_max_color_value,
gx_max_color_value);
shades[n1] = (byte) color;
if (n1 > 1) {
memset(shades + 1, 255, n1 - 1);
svga_map_color_rgb(dev, color, rgb);
}
skip = sraster - ((w * depth) >> 3);
sptr = base + (sourcex >> (3 - log2_depth));
mask = n1;
ishift = (~sourcex & (7 >> log2_depth)) << log2_depth;
for (yi = y; yi - y < h; yi++) {
fb_ptr ptr = 0;
int shift = ishift;
for (xi = x; xi - x < w; xi++, ptr++) {
uint a = (*sptr >> shift) & mask;
if (PTR_OFF(ptr) == 0)
set_pixel_write_ptr(ptr, fb_dev, xi, yi);
map:if (a != 0) {
byte ci = shades[a];
if (ci == 255) {
#define make_shade(v, alpha, n1)\
(gx_max_color_value -\
((ulong)(gx_max_color_value - (v)) * (alpha) / (n1)))
gx_color_value r =
make_shade(rgb[0], a, n1);
gx_color_value g =
make_shade(rgb[1], a, n1);
gx_color_value b =
make_shade(rgb[2], a, n1);
gx_color_index sci =
svga_map_rgb_color(dev, r, g, b);
if (sci == gx_no_color_index) {
a += (n1 + 1 - a) >> 1;
goto map;
}
shades[a] = ci = (byte) sci;
}
*ptr = ci;
}
if (shift == 0)
shift = 8 - depth, sptr++;
else
shift -= depth;
}
sptr += skip;
}
return 0;
}
private dev_proc_open_device(vesa_open);
private const gx_device_procs vesa_procs = svga_procs(vesa_open);
int vesa_get_mode(void);
void vesa_set_mode(int);
private void vesa_set_page(gx_device_svga *, int, int);
gx_device_svga far_data gs_vesa_device =
svga_device(vesa_procs, "vesa", vesa_get_mode, vesa_set_mode, vesa_set_page);
#define bits_include(a, m) !(~(a) & (m))
typedef struct {
byte vesa_signature[4];
ushort vesa_version;
char *product_info;
byte capabilities[4];
ushort *mode_list;
} vga_bios_info;
typedef enum {
m_supported = 1,
m_graphics = 0x10
} mode_attribute;
typedef enum {
w_supported = 1,
w_readable = 2,
w_writable = 4
} win_attribute;
typedef struct {
ushort mode_attributes;
byte win_a_attributes;
byte win_b_attributes;
ushort win_granularity;
ushort win_size;
ushort win_a_segment;
ushort win_b_segment;
void (*win_func_ptr) (int, int);
ushort bytes_per_line;
ushort x_resolution;
ushort y_resolution;
byte x_char_size;
byte y_char_size;
byte number_of_planes;
byte bits_per_pixel;
byte number_of_banks;
byte memory_model;
byte bank_size;
byte _padding[256 - 29];
} vesa_info;
int
vesa_get_mode(void)
{
registers regs;
regs.h.ah = 0x4f;
regs.h.al = 0x03;
int86(0x10, &regs, &regs);
return regs.rshort.bx;
}
void
vesa_set_mode(int mode)
{
registers regs;
regs.h.ah = 0x4f;
regs.h.al = 0x02;
regs.rshort.bx = mode;
int86(0x10, &regs, &regs);
}
private int
vesa_get_info(int mode, vesa_info _ss * info)
{
registers regs;
struct SREGS sregs;
regs.h.ah = 0x4f;
regs.h.al = 0x01;
regs.rshort.cx = mode;
segread(&sregs);
sregs.es = sregs.ss;
regs.rshort.di = PTR_OFF(info);
int86x(0x10, &regs, &regs, &sregs);
#ifdef DEBUG
if (regs.h.ah == 0 && regs.h.al == 0x4f)
dlprintf8("vesa_get_info(%x): ma=%x wa=%x/%x wg=%x ws=%x wseg=%x/%x\n",
mode, info->mode_attributes,
info->win_a_attributes, info->win_b_attributes,
info->win_granularity, info->win_size,
info->win_a_segment, info->win_b_segment);
else
dlprintf3("vesa_get_info(%x) failed: ah=%x al=%x\n",
mode, regs.h.ah, regs.h.al);
#endif
return (regs.h.ah == 0 && regs.h.al == 0x4f ? 0 : -1);
}
private int
vesa_find_mode(gx_device * dev, const mode_info * mode_table)
{
vesa_info info;
const mode_info *mip;
for (mip = mode_table; mip->mode >= 0; mip++) {
if (mip->width >= fb_dev->width &&
mip->height >= fb_dev->height &&
vesa_get_info(mip->mode, &info) >= 0 &&
bits_include(info.mode_attributes,
m_supported | m_graphics) &&
info.win_granularity <= 64 &&
(info.win_granularity & (info.win_granularity - 1)) == 0 &&
info.win_size == 64 &&
bits_include(info.win_a_attributes,
w_supported) &&
info.win_a_segment == regen
) {
fb_dev->wnum_read = 0;
fb_dev->wnum_write = 0;
if (bits_include(info.win_a_attributes,
w_readable | w_writable)
)
break;
else if (info.win_b_segment == regen &&
bits_include(info.win_b_attributes,
w_supported) &&
bits_include(info.win_a_attributes |
info.win_b_attributes,
w_readable | w_writable)
) {
if (!bits_include(info.win_a_attributes,
w_writable)
)
fb_dev->wnum_write = 1;
else
fb_dev->wnum_read = 1;
}
break;
}
}
if (mip->mode < 0)
return_error(gs_error_rangecheck);
fb_dev->mode = mip;
gx_device_adjust_resolution(dev, mip->width, mip->height, 1);
fb_dev->info.vesa.bios_set_page = info.win_func_ptr;
fb_dev->info.vesa.pn_shift = ilog2(64 / info.win_granularity);
fb_dev->raster = info.bytes_per_line;
return 0;
}
private int
vesa_open(gx_device * dev)
{
static const mode_info mode_table[] =
{
{640, 400, 0x100},
{640, 480, 0x101},
{800, 600, 0x103},
{1024, 768, 0x105},
{1280, 1024, 0x107},
{-1, -1, -1}
};
int code = vesa_find_mode(dev, mode_table);
if (code < 0)
return code;
return svga_open(dev);
}
private void
vesa_set_page(gx_device_svga * dev, int pn, int wnum)
{
#if USE_ASM
extern void vesa_call_set_page(void (*)(int, int), int, int);
if (dev->info.vesa.bios_set_page != NULL)
vesa_call_set_page(dev->info.vesa.bios_set_page, pn << dev->info.vesa.pn_shift, wnum);
else
#endif
{
registers regs;
regs.rshort.dx = pn << dev->info.vesa.pn_shift;
regs.h.ah = 0x4f;
regs.h.al = 5;
regs.rshort.bx = wnum;
int86(0x10, &regs, &regs);
}
}
private dev_proc_open_device(atiw_open);
private const gx_device_procs atiw_procs = svga_procs(atiw_open);
private int atiw_get_mode(void);
private void atiw_set_mode(int);
private void atiw_set_page(gx_device_svga *, int, int);
gx_device_svga far_data gs_atiw_device =
svga_device(atiw_procs, "atiw", atiw_get_mode, atiw_set_mode, atiw_set_page);
private int
atiw_get_mode(void)
{
registers regs;
regs.h.ah = 0xf;
int86(0x10, &regs, &regs);
return regs.h.al;
}
private void
atiw_set_mode(int mode)
{
registers regs;
regs.h.ah = 0;
regs.h.al = mode;
int86(0x10, &regs, &regs);
}
private int
atiw_open(gx_device * dev)
{
{
static const mode_info mode_table[] =
{
{640, 400, 0x61},
{640, 480, 0x62},
{800, 600, 0x63},
{1024, 768, 0x64},
{-1, -1, -1}
};
int code = svga_find_mode(dev, mode_table);
if (code < 0)
return code;
fb_dev->info.atiw.select_reg = *(int *)MK_PTR(0xc000, 0x10);
return svga_open(dev);
}
}
private void
atiw_set_page(gx_device_svga * dev, int pn, int wnum)
{
int select_reg = dev->info.atiw.select_reg;
byte reg;
disable();
outportb(select_reg, 0xb2);
reg = inportb(select_reg + 1);
outportb(select_reg, 0xb2);
outportb(select_reg + 1, (reg & 0xe1) + (pn << 1));
enable();
}
private dev_proc_open_device(tvga_open);
private const gx_device_procs tvga_procs = svga_procs(tvga_open);
private void tvga_set_page(gx_device_svga *, int, int);
gx_device_svga far_data gs_tvga_device =
svga_device(tvga_procs, "tvga", atiw_get_mode, atiw_set_mode, tvga_set_page);
private int
tvga_open(gx_device * dev)
{
fb_dev->wnum_read = 1;
fb_dev->wnum_write = 0;
{
static const mode_info mode_table[] =
{
{640, 400, 0x5c},
{640, 480, 0x5d},
{800, 600, 0x5e},
{1024, 768, 0x62},
{-1, -1, -1}
};
int code = svga_find_mode(dev, mode_table);
if (code < 0)
return code;
return svga_open(dev);
}
}
private void
tvga_set_page(gx_device_svga * dev, int pn, int wnum)
{
outportb(0x3c4, 0x0b);
inportb(0x3c4);
outportb(0x3c4, 0x0e);
outportb(0x3c5, pn ^ 2);
}
private dev_proc_open_device(tseng_open);
private const gx_device_procs tseng_procs =
svga_procs(tseng_open);
private void tseng_set_page(gx_device_svga *, int, int);
gx_device_svga far_data gs_tseng_device =
svga_device(tseng_procs, "tseng", atiw_get_mode, atiw_set_mode, tseng_set_page);
private int
tseng_open(gx_device * dev)
{
fb_dev->wnum_read = 1;
fb_dev->wnum_write = 0;
{
static const mode_info mode_table[] =
{
{640, 350, 0x2d},
{640, 480, 0x2e},
{800, 600, 0x30},
{1024, 768, 0x38},
{-1, -1, -1}
};
int code = svga_find_mode(dev, mode_table);
volatile_fb_ptr p0 = (volatile_fb_ptr) MK_PTR(regen, 0);
if (code < 0)
return code;
code = svga_open(dev);
if (code < 0)
return 0;
outportb(0x3cd, 0x44);
*p0 = 4;
outportb(0x3cd, 0x40);
*p0 = 3;
fb_dev->info.tseng.et_model = *p0;
return 0;
}
}
private void
tseng_set_page(gx_device_svga * dev, int pn, int wnum)
{
int shift = dev->info.tseng.et_model;
int mask = (1 << shift) - 1;
if (wnum)
pn <<= shift, mask <<= shift;
outportb(0x3cd, (inportb(0x3cd) & ~mask) + pn);
}
private dev_proc_open_device(cirr_open);
private gx_device_procs cirr_procs = svga_procs(cirr_open);
private void cirr_set_page(gx_device_svga *, int, int);
gx_device_svga gs_cirr_device =
svga_device(cirr_procs, "cirr", atiw_get_mode, atiw_set_mode, cirr_set_page);
private int
cirr_open(gx_device * dev)
{
fb_dev->wnum_read = 1;
fb_dev->wnum_write = 0;
{
static const mode_info mode_table[] =
{
{640, 400, 0x5e},
{640, 480, 0x5f},
{800, 600, 0x5c},
{1024, 768, 0x60},
{-1, -1, -1}
};
int code = svga_find_mode(dev, mode_table);
if (code < 0)
return code;
outportb(0x3c4, 0x06);
outportb(0x3c5, 0x12);
outportb(0x3ce, 0x0b);
outportb(0x3cf, (inportb(0x3cf) & 0xde));
return svga_open(dev);
}
}
private void
cirr_set_page(gx_device_svga * dev, int pn, int wnum)
{
outportb(0x3ce, 0x09);
outportb(0x3cf, pn << 4);
}
private dev_proc_open_device(ali_open);
private const gx_device_procs ali_procs = svga_procs(ali_open);
private void ali_set_page(gx_device_svga *, int, int);
gx_device_svga gs_ali_device =
svga_device(ali_procs, "ali", atiw_get_mode, atiw_set_mode,
ali_set_page);
private int
ali_open(gx_device * dev)
{
fb_dev->wnum_read = 1;
fb_dev->wnum_write = 0;
{
static const mode_info mode_table[] =
{
{640, 400, 0x29},
{640, 480, 0x2a},
{800, 600, 0x2c},
{1024, 768, 0x31},
{-1, -1, -1}
};
int code = svga_find_mode(dev, mode_table);
if (code < 0)
return code;
return svga_open(dev);
}
}
private void
ali_set_page(gx_device_svga * dev, int pn, int wnum)
{
outportb(0x3d6, pn);
outportb(0x3d7, pn);
}