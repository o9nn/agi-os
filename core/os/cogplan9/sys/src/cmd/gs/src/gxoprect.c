#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gxdevice.h"
#include "gsdevice.h"
#include "gxgetbit.h"
#include "gxoprect.h"
#include "gsbitops.h"
private void
unpack_scanline_lt8(
gx_color_index *    destp,
const byte *        srcp,
int                 src_offset,
int                 width,
int                 depth )
{
byte                buff = 0;
int                 i = 0, shift = 8 - depth, p_per_byte = 8 / depth;
if (width == 0)
return;
if (src_offset >= p_per_byte) {
srcp += src_offset / p_per_byte;
src_offset &= (p_per_byte - 1);
}
if (src_offset > 0) {
buff = *srcp++ << (src_offset * depth);
i = src_offset;
width += src_offset;
}
for (; i < width; i++, buff <<= depth) {
if ((i & (p_per_byte - 1)) == 0)
buff = *srcp++;
*destp++ = buff >> shift;
}
}
private void
pack_scanline_lt8(
const gx_color_index *  srcp,
byte *                  destp,
int                     dest_offset,
int                     width,
int                     depth )
{
byte                    buff = 0;
int                     i = 0, p_per_byte = 8 / depth;
if (width == 0)
return;
if (dest_offset >= p_per_byte) {
destp += dest_offset / p_per_byte;
dest_offset &= (p_per_byte - 1);
}
if (dest_offset > 0) {
buff = *destp++ >> (8 - dest_offset * depth);
i = dest_offset;
width += dest_offset;
}
for (; i < width; i++) {
buff = (buff << depth) | *srcp++;
if ((i & (p_per_byte - 1)) == p_per_byte - 1)
*destp++ = buff;
}
if ((i &= (p_per_byte - 1)) != 0) {
int     shift = depth * (p_per_byte - i);
int     mask = (1 << shift) - 1;
*destp = (*destp & mask) | (buff << shift);
}
}
private void
unpack_scanline_ge8(
gx_color_index *    destp,
const byte *        srcp,
int                 src_offset,
int                 width,
int                 depth )
{
gx_color_index      buff = 0;
int                 i, j, bytes_per_p = depth >> 3;
srcp += src_offset * bytes_per_p;
width *= bytes_per_p;
for (i = 0, j = 0; i < width; i++) {
buff = (buff << 8) | *srcp++;
if (++j == bytes_per_p) {
*destp++ = buff;
buff = 0;
j = 0;
}
}
}
private void
pack_scanline_ge8(
const gx_color_index *  srcp,
byte *                  destp,
int                     dest_offset,
int                     width,
int                     depth )
{
gx_color_index          buff = 0;
int                     i, j, bytes_per_p = depth >> 3;
int                     shift = depth - 8;
destp += dest_offset;
width *= bytes_per_p;
for (i = 0, j = bytes_per_p - 1; i < width; i++, buff <<= 8) {
if (++j == bytes_per_p) {
buff = *srcp++;
j = 0;
}
*destp++ = buff >> shift;
}
}
int
gx_overprint_generic_fill_rectangle(
gx_device *             tdev,
gx_color_index          drawn_comps,
int                     x,
int                     y,
int                     w,
int                     h,
gx_color_index          color,
gs_memory_t *           mem )
{
gx_color_value          src_cvals[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index *        pcolor_buff = 0;
byte *                  gb_buff = 0;
gs_get_bits_params_t    gb_params;
gs_int_rect             gb_rect;
int                     depth = tdev->color_info.depth;
int                     bit_x, start_x, end_x, raster, code;
void                    (*unpack_proc)( gx_color_index *,
const byte *,
int, int, int );
void                    (*pack_proc)( const gx_color_index *,
byte *,
int, int, int );
fit_fill(tdev, x, y, w, h);
bit_x = x * depth;
start_x = bit_x & ~(8 * align_bitmap_mod - 1);
end_x = bit_x + w * depth;
if (depth >= 8) {
unpack_proc = unpack_scanline_ge8;
pack_proc = pack_scanline_ge8;
} else {
unpack_proc = unpack_scanline_lt8;
pack_proc = pack_scanline_lt8;
}
if ((code = dev_proc(tdev, decode_color)(tdev, color, src_cvals)) < 0)
return code;
pcolor_buff = (gx_color_index *)
gs_alloc_bytes( mem,
w *  arch_sizeof_color_index,
"overprint generic fill rectangle" );
if (pcolor_buff == 0)
return gs_note_error(gs_error_VMerror);
raster = bitmap_raster(end_x - start_x);
gb_buff = gs_alloc_bytes(mem, raster, "overprint generic fill rectangle");
if (gb_buff == 0) {
gs_free_object( mem,
pcolor_buff,
"overprint generic fill rectangle" );
return gs_note_error(gs_error_VMerror);
}
gb_params.options =  GB_COLORS_NATIVE
| GB_ALPHA_NONE
| GB_DEPTH_ALL
| GB_PACKING_CHUNKY
| GB_RETURN_COPY
| GB_ALIGN_STANDARD
| GB_OFFSET_0
| GB_RASTER_STANDARD;
gb_params.x_offset = 0;
gb_params.data[0] = gb_buff;
gb_params.raster = raster;
gb_rect.p.x = x;
gb_rect.q.x = x + w;
while (h-- > 0 && code >= 0) {
gx_color_index *    cp = pcolor_buff;
int                 i;
gb_rect.p.y = y++;
gb_rect.q.y = y;
code = dev_proc(tdev, get_bits_rectangle)( tdev,
&gb_rect,
&gb_params,
0 );
if (code < 0)
break;
unpack_proc(pcolor_buff, gb_buff, 0, w, depth);
for (i = 0; i < w; i++, cp++) {
gx_color_index  comps;
int             j;
gx_color_value  dest_cvals[GX_DEVICE_COLOR_MAX_COMPONENTS];
if ((code = dev_proc(tdev, decode_color)(tdev, *cp, dest_cvals)) < 0)
break;
for (j = 0, comps = drawn_comps; comps != 0; ++j, comps >>= 1) {
if ((comps & 0x1) != 0)
dest_cvals[j] = src_cvals[j];
}
*cp = dev_proc(tdev, encode_color)(tdev, dest_cvals);
}
pack_proc(pcolor_buff, gb_buff, 0, w, depth);
code = dev_proc(tdev, copy_color)( tdev,
gb_buff,
0,
raster,
gs_no_bitmap_id,
x, y - 1, w, 1 );
}
gs_free_object( mem,
gb_buff,
"overprint generic fill rectangle" );
gs_free_object( mem,
pcolor_buff,
"overprint generic fill rectangle" );
return code;
}
private mono_fill_chunk fill_pat_2[4] = {
mono_fill_make_pattern(0x00), mono_fill_make_pattern(0x55),
mono_fill_make_pattern(0xaa), mono_fill_make_pattern(0xff)
};
private mono_fill_chunk fill_pat_4[16] = {
mono_fill_make_pattern(0x00), mono_fill_make_pattern(0x11),
mono_fill_make_pattern(0x22), mono_fill_make_pattern(0x33),
mono_fill_make_pattern(0x44), mono_fill_make_pattern(0x55),
mono_fill_make_pattern(0x66), mono_fill_make_pattern(0x77),
mono_fill_make_pattern(0x88), mono_fill_make_pattern(0x99),
mono_fill_make_pattern(0xaa), mono_fill_make_pattern(0xbb),
mono_fill_make_pattern(0xcc), mono_fill_make_pattern(0xdd),
mono_fill_make_pattern(0xee), mono_fill_make_pattern(0xff)
};
private mono_fill_chunk
replicate_color(int depth, mono_fill_chunk color)
{
switch (depth) {
case 1:
color = (mono_fill_chunk)(-(int)color); break;
case 2:
color = fill_pat_2[color]; break;
case 4:
color = fill_pat_4[color]; break;
case 8:
color= mono_fill_make_pattern(color); break;
#if mono_fill_chunk_bytes > 2
case 16:
color = (color << 16) | color;
#endif
#if mono_fill_chunk_bytes > 4
case 32:
color = (color << 32) | color;
break;
#endif
}
return color;
}
int
gx_overprint_sep_fill_rectangle_1(
gx_device *             tdev,
gx_color_index          retain_mask,
int                     x,
int                     y,
int                     w,
int                     h,
gx_color_index          color,
gs_memory_t *           mem )
{
byte *                  gb_buff = 0;
gs_get_bits_params_t    gb_params;
gs_int_rect             gb_rect;
int                     code = 0, bit_w, depth = tdev->color_info.depth;
int                     raster;
mono_fill_chunk         rep_color, rep_mask;
fit_fill(tdev, x, y, w, h);
bit_w = w * depth;
if (depth < 8 * sizeof(mono_fill_chunk)) {
rep_color = replicate_color(depth, (mono_fill_chunk)color);
rep_mask = replicate_color(depth, (mono_fill_chunk)retain_mask);
} else {
rep_color = (mono_fill_chunk)color;
rep_mask = (mono_fill_chunk)retain_mask;
}
raster = bitmap_raster(w * depth);
gb_buff = gs_alloc_bytes(mem, raster, "overprint sep fill rectangle 1");
if (gb_buff == 0)
return gs_note_error(gs_error_VMerror);
gb_params.options =  GB_COLORS_NATIVE
| GB_ALPHA_NONE
| GB_DEPTH_ALL
| GB_PACKING_CHUNKY
| GB_RETURN_COPY
| GB_ALIGN_STANDARD
| GB_OFFSET_0
| GB_RASTER_STANDARD;
gb_params.x_offset = 0;
gb_params.data[0] = gb_buff;
gb_params.raster = raster;
gb_rect.p.x = x;
gb_rect.q.x = x + w;
while (h-- > 0 && code >= 0) {
gb_rect.p.y = y++;
gb_rect.q.y = y;
code = dev_proc(tdev, get_bits_rectangle)( tdev,
&gb_rect,
&gb_params,
0 );
if (code < 0)
break;
bits_fill_rectangle_masked( gb_buff,
0,
raster,
rep_color,
rep_mask,
bit_w,
1 );
code = dev_proc(tdev, copy_color)( tdev,
gb_buff,
0,
raster,
gs_no_bitmap_id,
x, y - 1, w, 1 );
}
gs_free_object( mem,
gb_buff,
"overprint generic fill rectangle" );
return code;
}
int
gx_overprint_sep_fill_rectangle_2(
gx_device *             tdev,
gx_color_index          retain_mask,
int                     x,
int                     y,
int                     w,
int                     h,
gx_color_index          color,
gs_memory_t *           mem )
{
byte *                  gb_buff = 0;
gs_get_bits_params_t    gb_params;
gs_int_rect             gb_rect;
int                     code = 0, byte_w, raster;
int                     byte_depth = tdev->color_info.depth >> 3;
byte *                  pcolor;
byte *                  pmask;
fit_fill(tdev, x, y, w, h);
byte_w = w * byte_depth;
pcolor = (byte *)&color;
pmask = (byte *)&retain_mask;
#if arch_is_big_endian
pcolor += arch_sizeof_color_index - byte_depth;
pmask += arch_sizeof_color_index - byte_depth;
#endif
raster = bitmap_raster(w * (byte_depth << 3));
gb_buff = gs_alloc_bytes(mem, raster, "overprint sep fill rectangle 2");
if (gb_buff == 0)
return gs_note_error(gs_error_VMerror);
gb_params.options =  GB_COLORS_NATIVE
| GB_ALPHA_NONE
| GB_DEPTH_ALL
| GB_PACKING_CHUNKY
| GB_RETURN_COPY
| GB_ALIGN_STANDARD
| GB_OFFSET_0
| GB_RASTER_STANDARD;
gb_params.x_offset = 0;
gb_params.data[0] = gb_buff;
gb_params.raster = raster;
gb_rect.p.x = x;
gb_rect.q.x = x + w;
while (h-- > 0 && code >= 0) {
int     i, j;
byte *  cp = gb_buff;
gb_rect.p.y = y++;
gb_rect.q.y = y;
code = dev_proc(tdev, get_bits_rectangle)( tdev,
&gb_rect,
&gb_params,
0 );
if (code < 0)
break;
for (i = 0, j = 0; i < byte_w; i++, cp++) {
*cp = (*cp & pmask[j]) | pcolor[j];
if (++j == byte_depth)
j = 0;
}
code = dev_proc(tdev, copy_color)( tdev,
gb_buff,
0,
raster,
gs_no_bitmap_id,
x, y - 1, w, 1 );
}
gs_free_object( mem,
gb_buff,
"overprint generic fill rectangle" );
return code;
}