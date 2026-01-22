#include "memory_.h"
#include "gx.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gdevmem.h"
#define mem_true24_strip_copy_rop mem_gray8_rgb24_strip_copy_rop
#ifdef DEBUG
struct stats_mem24_s {
long
fill, fwide, fgray[101], fsetc, fcolor[101], fnarrow[5],
fprevc[257];
double ftotal;
} stats_mem24;
static int prev_count;
static uint prev_colors[256];
# define INCR(v) (++(stats_mem24.v))
#else
# define INCR(v) DO_NOTHING
#endif
#undef chunk
#define chunk byte
declare_mem_procs(mem_true24_copy_mono, mem_true24_copy_color, mem_true24_fill_rectangle);
private dev_proc_copy_alpha(mem_true24_copy_alpha);
const gx_device_memory mem_true24_device =
mem_full_alpha_device("image24", 24, 0, mem_open,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb,
mem_true24_copy_mono, mem_true24_copy_color, mem_true24_fill_rectangle,
gx_default_map_cmyk_color, mem_true24_copy_alpha,
gx_default_strip_tile_rectangle, mem_true24_strip_copy_rop,
mem_get_bits_rectangle);
#undef x_to_byte
#define x_to_byte(x) ((x) * 3)
#define declare_unpack_color(r, g, b, color)\
byte r = (byte)(color >> 16);\
byte g = (byte)((uint)color >> 8);\
byte b = (byte)color
#define put3(ptr, r, g, b)\
(ptr)[0] = r, (ptr)[1] = g, (ptr)[2] = b
#define putw(ptr, wxyz)\
*(bits32 *)(ptr) = (wxyz)
#if arch_is_big_endian
# define set_color24_cache(crgb, r, g, b)\
mdev->color24.rgbr = rgbr = ((bits32)(crgb) << 8) | (r),\
mdev->color24.gbrg = gbrg = (rgbr << 8) | (g),\
mdev->color24.brgb = brgb = (gbrg << 8) | (b),\
mdev->color24.rgb = (crgb)
#else
# define set_color24_cache(crgb, r, g, b)\
mdev->color24.rgbr = rgbr =\
((bits32)(r) << 24) | ((bits32)(b) << 16) |\
((bits16)(g) << 8) | (r),\
mdev->color24.brgb = brgb = (rgbr << 8) | (b),\
mdev->color24.gbrg = gbrg = (brgb << 8) | (g),\
mdev->color24.rgb = (crgb)
#endif
private int
mem_true24_fill_rectangle(gx_device * dev,
int x, int y, int w, int h, gx_color_index color)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
declare_unpack_color(r, g, b, color);
declare_scan_ptr(dest);
fit_fill_xywh(dev, x, y, w, h);
INCR(fill);
#ifdef DEBUG
stats_mem24.ftotal += w;
#endif
if (w >= 5) {
if (h <= 0)
return 0;
INCR(fwide);
setup_rect(dest);
if (r == g && r == b) {
#ifndef USE_MEMSET
int bcntm7 = w * 3 - 7;
register bits32 cword = color | (color << 24);
INCR(fgray[min(w, 100)]);
while (h-- > 0) {
register byte *pptr = dest;
byte *limit = pptr + bcntm7;
switch (x & 3) {
case 3:
*pptr++ = (byte) cword;
case 2:
*pptr++ = (byte) cword;
case 1:
*pptr++ = (byte) cword;
case 0:;
}
*(bits32 *) pptr =
((bits32 *) pptr)[1] =
((bits32 *) pptr)[2] = cword;
pptr += 12;
while (pptr < limit) {
*(bits32 *) pptr =
((bits32 *) pptr)[1] = cword;
pptr += 8;
}
switch ((int)(pptr - limit)) {
case 0:
pptr[6] = (byte) cword;
case 1:
pptr[5] = (byte) cword;
case 2:
pptr[4] = (byte) cword;
case 3:
*(bits32 *) pptr = cword;
break;
case 4:
pptr[2] = (byte) cword;
case 5:
pptr[1] = (byte) cword;
case 6:
pptr[0] = (byte) cword;
case 7:;
}
inc_ptr(dest, draster);
}
#else
int bcnt = w * 3;
INCR(fgray[min(w, 100)]);
while (h-- > 0) {
memset(dest, r, bcnt);
inc_ptr(dest, draster);
}
#endif
} else {
int x3 = -x & 3, ww = w - x3;
bits32 rgbr, gbrg, brgb;
if (mdev->color24.rgb == color) {
rgbr = mdev->color24.rgbr;
gbrg = mdev->color24.gbrg;
brgb = mdev->color24.brgb;
} else {
INCR(fsetc);
set_color24_cache(color, r, g, b);
}
#ifdef DEBUG
{
int ci;
for (ci = 0; ci < prev_count; ++ci)
if (prev_colors[ci] == color)
break;
INCR(fprevc[ci]);
if (ci == prev_count) {
if (ci < countof(prev_colors))
++prev_count;
else
--ci;
}
if (ci) {
memmove(&prev_colors[1], &prev_colors[0],
ci * sizeof(prev_colors[0]));
prev_colors[0] = color;
}
}
#endif
INCR(fcolor[min(w, 100)]);
while (h-- > 0) {
register byte *pptr = dest;
int w1 = ww;
switch (x3) {
case 1:
put3(pptr, r, g, b);
pptr += 3;
break;
case 2:
pptr[0] = r;
pptr[1] = g;
putw(pptr + 2, brgb);
pptr += 6;
break;
case 3:
pptr[0] = r;
putw(pptr + 1, gbrg);
putw(pptr + 5, brgb);
pptr += 9;
break;
case 0:
;
}
#ifdef USE_MEMCPY
if (w1 > 16) {
#define PUTW4(ptr, w)\
BEGIN\
putw(ptr, w); putw((ptr)+12, w); putw((ptr)+24, w); putw((ptr)+36, w);\
END
PUTW4(pptr, rgbr);
PUTW4(pptr + 4, gbrg);
PUTW4(pptr + 8, brgb);
#undef PUTW4
if (w1 > 64) {
memcpy(pptr + 48, pptr, 48);
memcpy(pptr + 96, pptr, 96);
for (pptr += 192; (w1 -= 64) >= 64; pptr += 192)
memcpy(pptr, pptr - 192, 192);
} else
pptr += 48;
for (; (w1 -= 16) >= 16; pptr += 48)
memcpy(pptr, pptr - 48, 48);
}
#endif
while (w1 >= 4) {
putw(pptr, rgbr);
putw(pptr + 4, gbrg);
putw(pptr + 8, brgb);
pptr += 12;
w1 -= 4;
}
switch (w1) {
case 1:
put3(pptr, r, g, b);
break;
case 2:
putw(pptr, rgbr);
pptr[4] = g;
pptr[5] = b;
break;
case 3:
putw(pptr, rgbr);
putw(pptr + 4, gbrg);
pptr[8] = b;
break;
case 0:
;
}
inc_ptr(dest, draster);
}
}
} else if (h > 0) {
INCR(fnarrow[max(w, 0)]);
setup_rect(dest);
switch (w) {
case 4:
do {
dest[9] = dest[6] = dest[3] = dest[0] = r;
dest[10] = dest[7] = dest[4] = dest[1] = g;
dest[11] = dest[8] = dest[5] = dest[2] = b;
inc_ptr(dest, draster);
}
while (--h);
break;
case 3:
do {
dest[6] = dest[3] = dest[0] = r;
dest[7] = dest[4] = dest[1] = g;
dest[8] = dest[5] = dest[2] = b;
inc_ptr(dest, draster);
}
while (--h);
break;
case 2:
do {
dest[3] = dest[0] = r;
dest[4] = dest[1] = g;
dest[5] = dest[2] = b;
inc_ptr(dest, draster);
}
while (--h);
break;
case 1:
do {
dest[0] = r, dest[1] = g, dest[2] = b;
inc_ptr(dest, draster);
}
while (--h);
break;
case 0:
default:
;
}
}
return 0;
}
private int
mem_true24_copy_mono(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
const byte *line;
int sbit;
int first_bit;
declare_scan_ptr(dest);
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
setup_rect(dest);
line = base + (sourcex >> 3);
sbit = sourcex & 7;
first_bit = 0x80 >> sbit;
if (zero != gx_no_color_index) {
declare_unpack_color(r0, g0, b0, zero);
declare_unpack_color(r1, g1, b1, one);
while (h-- > 0) {
register byte *pptr = dest;
const byte *sptr = line;
register int sbyte = *sptr++;
register int bit = first_bit;
int count = w;
do {
if (sbyte & bit) {
if (one != gx_no_color_index)
put3(pptr, r1, g1, b1);
} else
put3(pptr, r0, g0, b0);
pptr += 3;
if ((bit >>= 1) == 0)
bit = 0x80, sbyte = *sptr++;
}
while (--count > 0);
line += sraster;
inc_ptr(dest, draster);
}
} else if (one != gx_no_color_index) {
declare_unpack_color(r1, g1, b1, one);
int first_mask = first_bit << 1;
int first_count, first_skip;
if (sbit + w > 8)
first_mask -= 1,
first_count = 8 - sbit;
else
first_mask -= first_mask >> w,
first_count = w;
first_skip = first_count * 3;
while (h-- > 0) {
register byte *pptr = dest;
const byte *sptr = line;
register int sbyte = *sptr++ & first_mask;
int count = w - first_count;
if (sbyte) {
register int bit = first_bit;
do {
if (sbyte & bit)
put3(pptr, r1, g1, b1);
pptr += 3;
}
while ((bit >>= 1) & first_mask);
} else
pptr += first_skip;
while (count >= 8) {
sbyte = *sptr++;
if (sbyte & 0xf0) {
if (sbyte & 0x80)
put3(pptr, r1, g1, b1);
if (sbyte & 0x40)
put3(pptr + 3, r1, g1, b1);
if (sbyte & 0x20)
put3(pptr + 6, r1, g1, b1);
if (sbyte & 0x10)
put3(pptr + 9, r1, g1, b1);
}
if (sbyte & 0xf) {
if (sbyte & 8)
put3(pptr + 12, r1, g1, b1);
if (sbyte & 4)
put3(pptr + 15, r1, g1, b1);
if (sbyte & 2)
put3(pptr + 18, r1, g1, b1);
if (sbyte & 1)
put3(pptr + 21, r1, g1, b1);
}
pptr += 24;
count -= 8;
}
if (count > 0) {
register int bit = 0x80;
sbyte = *sptr++;
do {
if (sbyte & bit)
put3(pptr, r1, g1, b1);
pptr += 3;
bit >>= 1;
}
while (--count > 0);
}
line += sraster;
inc_ptr(dest, draster);
}
}
return 0;
}
private int
mem_true24_copy_color(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
mem_copy_byte_rect(mdev, base, sourcex, sraster, x, y, w, h);
return 0;
}
private int
mem_true24_copy_alpha(gx_device * dev, const byte * base, int sourcex,
int sraster, gx_bitmap_id id, int x, int y, int w, int h,
gx_color_index color, int depth)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
const byte *line;
declare_scan_ptr(dest);
declare_unpack_color(r, g, b, color);
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
setup_rect(dest);
line = base;
while (h-- > 0) {
register byte *pptr = dest;
int sx;
for (sx = sourcex; sx < sourcex + w; ++sx, pptr += 3) {
int alpha2, alpha;
if (depth == 2)
alpha =
((line[sx >> 2] >> ((3 - (sx & 3)) << 1)) & 3) * 5;
else
alpha2 = line[sx >> 1],
alpha = (sx & 1 ? alpha2 & 0xf : alpha2 >> 4);
if (alpha == 15) {
put3(pptr, r, g, b);
} else if (alpha != 0) {
#define make_shade(old, clr, alpha, amax) \
(old) + (((int)(clr) - (int)(old)) * (alpha) / (amax))
pptr[0] = make_shade(pptr[0], r, alpha, 15);
pptr[1] = make_shade(pptr[1], g, alpha, 15);
pptr[2] = make_shade(pptr[2], b, alpha, 15);
#undef make_shade
}
}
line += sraster;
inc_ptr(dest, draster);
}
return 0;
}
#if !arch_is_big_endian
declare_mem_procs(mem24_word_copy_mono, mem24_word_copy_color, mem24_word_fill_rectangle);
const gx_device_memory mem_true24_word_device =
mem_full_device("image24w", 24, 0, mem_open,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb,
mem24_word_copy_mono, mem24_word_copy_color, mem24_word_fill_rectangle,
gx_default_map_cmyk_color, gx_default_strip_tile_rectangle,
gx_no_strip_copy_rop, mem_word_get_bits_rectangle);
private int
mem24_word_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
byte *base;
uint raster;
fit_fill(dev, x, y, w, h);
base = scan_line_base(mdev, y);
raster = mdev->raster;
mem_swap_byte_rect(base, raster, x * 24, w * 24, h, true);
mem_true24_fill_rectangle(dev, x, y, w, h, color);
mem_swap_byte_rect(base, raster, x * 24, w * 24, h, false);
return 0;
}
private int
mem24_word_copy_mono(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h, gx_color_index zero, gx_color_index one)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
byte *row;
uint raster;
bool store;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
row = scan_line_base(mdev, y);
raster = mdev->raster;
store = (zero != gx_no_color_index && one != gx_no_color_index);
mem_swap_byte_rect(row, raster, x * 24, w * 24, h, store);
mem_true24_copy_mono(dev, base, sourcex, sraster, id,
x, y, w, h, zero, one);
mem_swap_byte_rect(row, raster, x * 24, w * 24, h, false);
return 0;
}
private int
mem24_word_copy_color(gx_device * dev,
const byte * base, int sourcex, int sraster, gx_bitmap_id id,
int x, int y, int w, int h)
{
gx_device_memory * const mdev = (gx_device_memory *)dev;
byte *row;
uint raster;
fit_copy(dev, base, sourcex, sraster, id, x, y, w, h);
row = scan_line_base(mdev, y);
raster = mdev->raster;
mem_swap_byte_rect(row, raster, x * 24, w * 24, h, true);
bytes_copy_rectangle(row + x * 3, raster, base + sourcex * 3, sraster,
w * 3, h);
mem_swap_byte_rect(row, raster, x * 24, w * 24, h, false);
return 0;
}
#endif