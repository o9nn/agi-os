#include "memory_.h"
#include "gx.h"
#include "gsbittab.h"
#include "gserrors.h"
#include "gsropt.h"
#include "gxcindex.h"
#include "gxdcolor.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxdevrop.h"
#include "gdevmem.h"
#include "gdevmrop.h"
#define chunk byte
#define x_offset(px, ty, textures)\
((textures)->shift == 0 ? (px) :\
(px) + (ty) / (textures)->rep_height * (textures)->rep_shift)
int
mem_gray8_rgb24_strip_copy_rop(gx_device * dev,
const byte * sdata, int sourcex, uint sraster, gx_bitmap_id id,
const gx_color_index * scolors,
const gx_strip_bitmap * textures, const gx_color_index * tcolors,
int x, int y, int width, int height,
int phase_x, int phase_y, gs_logical_operation_t lop)
{
gx_device_memory *mdev = (gx_device_memory *) dev;
gs_rop3_t rop = lop_rop(lop);
gx_color_index const_source = gx_no_color_index;
gx_color_index const_texture = gx_no_color_index;
uint draster = mdev->raster;
int line_count;
byte *drow;
int depth = dev->color_info.depth;
int bpp = depth >> 3;
gx_color_index all_ones = ((gx_color_index) 1 << depth) - 1;
gx_color_index strans =
(lop & lop_S_transparent ? all_ones : gx_no_color_index);
gx_color_index ttrans =
(lop & lop_T_transparent ? all_ones : gx_no_color_index);
if (!rop3_uses_S(rop))
const_source = 0;
else if (scolors != 0 && scolors[0] == scolors[1]) {
const_source = scolors[0];
if (const_source == gx_device_black(dev))
rop = rop3_know_S_0(rop);
else if (const_source == gx_device_white(dev))
rop = rop3_know_S_1(rop);
}
if (!rop3_uses_T(rop))
const_texture = 0;
else if (tcolors != 0 && tcolors[0] == tcolors[1]) {
const_texture = tcolors[0];
if (const_texture == gx_device_black(dev))
rop = rop3_know_T_0(rop);
else if (const_texture == gx_device_white(dev))
rop = rop3_know_T_1(rop);
}
if (bpp == 1 &&
(gx_device_has_color(dev) ||
(gx_device_black(dev) != 0 || gx_device_white(dev) != all_ones))
) {
gx_color_index bw_pixel;
switch (rop) {
case rop3_0:
bw_pixel = gx_device_black(dev);
goto bw;
case rop3_1:
bw_pixel = gx_device_white(dev);
bw: if (bw_pixel == 0x00)
rop = rop3_0;
else if (bw_pixel == 0xff)
rop = rop3_1;
else
goto df;
break;
case rop3_D:
break;
case rop3_S:
if (lop & lop_S_transparent)
goto df;
break;
case rop3_T:
if (lop & lop_T_transparent)
goto df;
break;
default:
df: return mem_default_strip_copy_rop(dev,
sdata, sourcex, sraster, id,
scolors, textures, tcolors,
x, y, width, height,
phase_x, phase_y, lop);
}
}
if (const_source == gx_no_color_index) {
fit_copy(dev, sdata, sourcex, sraster, id,
x, y, width, height);
} else {
fit_fill(dev, x, y, width, height);
}
line_count = height;
drow = scan_line_base(mdev, y) + x * bpp;
#define dbit(base, i) ((base)[(i) >> 3] & (0x80 >> ((i) & 7)))
#define cbit8(base, i, colors)\
(dbit(base, i) ? (byte)colors[1] : (byte)colors[0])
#define rop_body_8(s_pixel, t_pixel)\
if ( (s_pixel) == strans || \
(t_pixel) == ttrans \
)\
continue;\
*dptr = (*rop_proc_table[rop])(*dptr, s_pixel, t_pixel)
#define get24(ptr)\
(((gx_color_index)(ptr)[0] << 16) | ((gx_color_index)(ptr)[1] << 8) | (ptr)[2])
#define put24(ptr, pixel)\
(ptr)[0] = (byte)((pixel) >> 16),\
(ptr)[1] = (byte)((uint)(pixel) >> 8),\
(ptr)[2] = (byte)(pixel)
#define cbit24(base, i, colors)\
(dbit(base, i) ? colors[1] : colors[0])
#define rop_body_24(s_pixel, t_pixel)\
if ( (s_pixel) == strans || \
(t_pixel) == ttrans \
)\
continue;\
{ gx_color_index d_pixel = get24(dptr);\
d_pixel = (*rop_proc_table[rop])(d_pixel, s_pixel, t_pixel);\
put24(dptr, d_pixel);\
}
if (const_texture != gx_no_color_index) {
if (const_source != gx_no_color_index) {
for (; line_count-- > 0; drow += draster) {
byte *dptr = drow;
int left = width;
if (bpp == 1)
for (; left > 0; ++dptr, --left) {
rop_body_8((byte)const_source, (byte)const_texture);
}
else
for (; left > 0; dptr += 3, --left) {
rop_body_24(const_source, const_texture);
}
}
} else {
const byte *srow = sdata;
for (; line_count-- > 0; drow += draster, srow += sraster) {
byte *dptr = drow;
int left = width;
if (scolors) {
int sx = sourcex;
if (bpp == 1)
for (; left > 0; ++dptr, ++sx, --left) {
byte s_pixel = cbit8(srow, sx, scolors);
rop_body_8(s_pixel, (byte)const_texture);
}
else
for (; left > 0; dptr += 3, ++sx, --left) {
bits32 s_pixel = cbit24(srow, sx, scolors);
rop_body_24(s_pixel, const_texture);
}
} else if (bpp == 1) {
const byte *sptr = srow + sourcex;
for (; left > 0; ++dptr, ++sptr, --left) {
byte s_pixel = *sptr;
rop_body_8(s_pixel, (byte)const_texture);
}
} else {
const byte *sptr = srow + sourcex * 3;
for (; left > 0; dptr += 3, sptr += 3, --left) {
bits32 s_pixel = get24(sptr);
rop_body_24(s_pixel, const_texture);
}
}
}
}
} else if (const_source != gx_no_color_index) {
uint traster = textures->raster;
int ty = y + phase_y;
for (; line_count-- > 0; drow += draster, ++ty) {
int dx = x, w = width, nw;
byte *dptr = drow;
const byte *trow =
textures->data + (ty % textures->size.y) * traster;
int xoff = x_offset(phase_x, ty, textures);
for (; w > 0; dx += nw, w -= nw) {
int tx = (dx + xoff) % textures->rep_width;
int left = nw = min(w, textures->size.x - tx);
const byte *tptr = trow;
if (tcolors) {
if (bpp == 1)
for (; left > 0; ++dptr, ++tx, --left) {
byte t_pixel = cbit8(tptr, tx, tcolors);
rop_body_8((byte)const_source, t_pixel);
}
else
for (; left > 0; dptr += 3, ++tx, --left) {
bits32 t_pixel = cbit24(tptr, tx, tcolors);
rop_body_24(const_source, t_pixel);
}
} else if (bpp == 1) {
tptr += tx;
for (; left > 0; ++dptr, ++tptr, --left) {
byte t_pixel = *tptr;
rop_body_8((byte)const_source, t_pixel);
}
} else {
tptr += tx * 3;
for (; left > 0; dptr += 3, tptr += 3, --left) {
bits32 t_pixel = get24(tptr);
rop_body_24(const_source, t_pixel);
}
}
}
}
} else {
uint traster = textures->raster;
int ty = y + phase_y;
const byte *srow = sdata;
for (; line_count-- > 0; drow += draster, srow += sraster, ++ty) {
int sx = sourcex;
int dx = x;
int w = width;
int nw;
byte *dptr = drow;
const byte *trow =
textures->data + (ty % textures->size.y) * traster;
int xoff = x_offset(phase_x, ty, textures);
for (; w > 0; dx += nw, w -= nw) {
int tx = (dx + xoff) % textures->rep_width;
int left = nw = min(w, textures->size.x - tx);
const byte *tptr = trow;
if (bpp == 1) {
const byte *sptr = srow + sx;
tptr += tx;
for (; left > 0; ++dptr, ++sptr, ++tptr, ++sx, ++tx, --left) {
byte s_pixel =
(scolors ? cbit8(srow, sx, scolors) : *sptr);
byte t_pixel =
(tcolors ? cbit8(tptr, tx, tcolors) : *tptr);
rop_body_8(s_pixel, t_pixel);
}
} else {
const byte *sptr = srow + sx * 3;
tptr += tx * 3;
for (; left > 0; dptr += 3, sptr += 3, tptr += 3, ++sx, ++tx, --left) {
bits32 s_pixel =
(scolors ? cbit24(srow, sx, scolors) :
get24(sptr));
bits32 t_pixel =
(tcolors ? cbit24(tptr, tx, tcolors) :
get24(tptr));
rop_body_24(s_pixel, t_pixel);
}
}
}
}
}
#undef rop_body_8
#undef rop_body_24
#undef dbit
#undef cbit8
#undef cbit24
return 0;
}