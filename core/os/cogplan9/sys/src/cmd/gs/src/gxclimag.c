#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gscspace.h"
#include "gscdefs.h"
#include "gxarith.h"
#include "gxcspace.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxcldev.h"
#include "gxclpath.h"
#include "gxfmap.h"
#include "gxiparam.h"
#include "gxpath.h"
#include "stream.h"
#include "strimpl.h"
#include "sisparam.h"
#include "gxcomp.h"
#include "gsserial.h"
#include "gxdhtserial.h"
extern_gx_image_type_table();
static const bool USE_HL_IMAGES = true;
private int cmd_put_set_data_x(gx_device_clist_writer * cldev,
gx_clist_state * pcls, int data_x);
private bool check_rect_for_trivial_clip(
const gx_clip_path *pcpath,
int px, int py, int qx, int qy
);
int
clist_fill_mask(gx_device * dev,
const byte * data, int data_x, int raster, gx_bitmap_id id,
int x, int y, int width, int height,
const gx_drawing_color * pdcolor, int depth,
gs_logical_operation_t lop, const gx_clip_path * pcpath)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
const byte *orig_data = data;
int orig_data_x = data_x;
int orig_x = x;
int orig_width = width;
int orig_height = height;
int log2_depth = ilog2(depth);
int y0;
int data_x_bit;
byte copy_op =
(depth > 1 ? cmd_op_copy_color_alpha :
cmd_op_copy_mono + cmd_copy_ht_color);
bool slow_rop =
cmd_slow_rop(dev, lop_know_S_0(lop), pdcolor) ||
cmd_slow_rop(dev, lop_know_S_1(lop), pdcolor);
if (depth > 1 && (cdev->disable_mask & clist_disable_copy_alpha) != 0)
return_error(gs_error_unknownerror);
fit_copy(dev, data, data_x, raster, id, x, y, width, height);
y0 = y;
if (((cdev->disable_mask & clist_disable_complex_clip) &&
!check_rect_for_trivial_clip(pcpath, x, y, x + width, y + height)) ||
gs_debug_c('`') || id == gx_no_bitmap_id || lop != lop_default ||
(depth > 1 && !color_writes_pure(pdcolor, lop))
)
copy:
return gx_default_fill_mask(dev, data, data_x, raster, id,
x, y, width, height, pdcolor, depth,
lop, pcpath);
if (cmd_check_clip_path(cdev, pcpath))
cmd_clear_known(cdev, clip_path_known);
data_x_bit = data_x << log2_depth;
FOR_RECTS {
int code;
ulong offset_temp;
TRY_RECT {
code = cmd_update_lop(cdev, pcls, lop);
} HANDLE_RECT(code);
if (depth > 1 && !pcls->color_is_alpha) {
byte *dp;
TRY_RECT {
code =
set_cmd_put_op(dp, cdev, pcls, cmd_opv_set_copy_alpha, 1);
} HANDLE_RECT(code);
pcls->color_is_alpha = 1;
}
TRY_RECT {
code = cmd_do_write_unknown(cdev, pcls, clip_path_known);
if (code >= 0)
code = cmd_do_enable_clip(cdev, pcls, pcpath != NULL);
} HANDLE_RECT(code);
TRY_RECT {
code = cmd_put_drawing_color(cdev, pcls, pdcolor);
if (depth > 1 && code >= 0)
code = cmd_set_color1(cdev, pcls, pdcolor->colors.pure);
} HANDLE_RECT(code);
pcls->colors_used.slow_rop |= slow_rop;
if (!cls_has_tile_id(cdev, pcls, id, offset_temp)) {
gx_strip_bitmap tile;
tile.data = (byte *) orig_data;
tile.raster = raster;
tile.size.x = tile.rep_width = orig_width;
tile.size.y = tile.rep_height = orig_height;
tile.rep_shift = tile.shift = 0;
tile.id = id;
TRY_RECT {
code = clist_change_bits(cdev, pcls, &tile, depth);
} HANDLE_RECT_UNLESS(code,
(code != gs_error_VMerror || !cdev->error_is_retryable) );
if (code < 0) {
goto copy;
}
}
{
gx_cmd_rect rect;
int rsize;
byte op = copy_op + cmd_copy_use_tile;
rect.x = orig_x, rect.y = y0;
rect.width = orig_width, rect.height = yend - y0;
rsize = 1 + cmd_sizexy(rect);
TRY_RECT {
code = (orig_data_x ?
cmd_put_set_data_x(cdev, pcls, orig_data_x) : 0);
if (code >= 0) {
byte *dp;
code = set_cmd_put_op(dp, cdev, pcls, op, rsize);
if (code >= 0) {
dp++;
cmd_putxy(rect, dp);
}
}
} HANDLE_RECT(code);
pcls->rect = rect;
goto end;
}
end:
;
} END_RECTS;
return 0;
}
typedef struct clist_image_enum_s {
gx_image_enum_common;
gs_memory_t *memory;
gs_pixel_image_t image;
gx_drawing_color dcolor;
gs_int_rect rect;
const gs_imager_state *pis;
const gx_clip_path *pcpath;
gs_image_format_t format;
gs_int_point support;
int bits_per_plane;
gs_matrix matrix;
bool uses_color;
clist_color_space_t color_space;
int ymin, ymax;
gx_colors_used_t colors_used;
byte begin_image_command[3 +
2 * cmd_sizew_max +
1 + 6 * sizeof(float) +
(GS_IMAGE_MAX_COMPONENTS + 3) / 4 +
GS_IMAGE_MAX_COMPONENTS * 2 * sizeof(float) +
GS_IMAGE_MAX_COMPONENTS * cmd_sizew_max +
4 * cmd_sizew_max];
int begin_image_command_length;
int y;
bool color_map_is_known;
} clist_image_enum;
gs_private_st_suffix_add3(st_clist_image_enum, clist_image_enum,
"clist_image_enum", clist_image_enum_enum_ptrs,
clist_image_enum_reloc_ptrs, st_gx_image_enum_common,
pis, pcpath, color_space.space);
private image_enum_proc_plane_data(clist_image_plane_data);
private image_enum_proc_end_image(clist_image_end_image);
private const gx_image_enum_procs_t clist_image_enum_procs =
{
clist_image_plane_data, clist_image_end_image
};
private bool image_band_box(gx_device * dev, const clist_image_enum * pie,
int y, int h, gs_int_rect * pbox);
private int begin_image_command(byte *buf, uint buf_size,
const gs_image_common_t *pic);
private int cmd_image_plane_data(gx_device_clist_writer * cldev,
gx_clist_state * pcls,
const gx_image_plane_t * planes,
const gx_image_enum_common_t * pie,
uint bytes_per_plane,
const uint * offsets, int dx, int h);
private uint clist_image_unknowns(gx_device *dev,
const clist_image_enum *pie);
private int write_image_end_all(gx_device *dev,
const clist_image_enum *pie);
private bool
image_matrix_ok_to_band(const gs_matrix * pmat)
{
double t;
if (fabs(pmat->xx * pmat->yy - pmat->xy * pmat->yx) < 0.001)
return false;
if (is_xxyy(pmat) || is_xyyx(pmat))
return true;
t = (fabs(pmat->xx) + fabs(pmat->yy)) /
(fabs(pmat->xy) + fabs(pmat->yx));
return (t < 0.2 || t > 5);
}
int
clist_begin_typed_image(gx_device * dev,
const gs_imager_state * pis, const gs_matrix * pmat,
const gs_image_common_t * pic, const gs_int_rect * prect,
const gx_drawing_color * pdcolor, const gx_clip_path * pcpath,
gs_memory_t * mem, gx_image_enum_common_t ** pinfo)
{
const gs_pixel_image_t * const pim = (const gs_pixel_image_t *)pic;
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
clist_image_enum *pie = 0;
int base_index;
bool indexed;
bool masked = false;
bool has_alpha = false;
int num_components;
int bits_per_pixel;
bool uses_color;
bool varying_depths = false;
gs_matrix mat;
gs_rect sbox, dbox;
gs_image_format_t format;
gx_color_index colors_used = 0;
int code;
switch ((gs_debug_c('`') ? -1 : pic->type->index)) {
case 1:
masked = ((const gs_image1_t *)pim)->ImageMask;
has_alpha = ((const gs_image1_t *)pim)->Alpha != 0;
case 4:
if (pmat == 0)
break;
default:
goto use_default;
}
format = pim->format;
pie = gs_alloc_struct_immovable(mem, clist_image_enum,
&st_clist_image_enum,
"clist_begin_typed_image");
if (pie == 0)
return_error(gs_error_VMerror);
pie->memory = mem;
*pinfo = (gx_image_enum_common_t *) pie;
if (masked) {
base_index = gs_color_space_index_DeviceGray;
indexed = false;
num_components = 1;
uses_color = true;
} else {
const gs_color_space *pcs = pim->ColorSpace;
base_index = gs_color_space_get_index(pcs);
if (base_index == gs_color_space_index_Indexed) {
const gs_color_space *pbcs =
gs_color_space_indexed_base_space(pcs);
indexed = true;
base_index = gs_color_space_get_index(pbcs);
num_components = 1;
} else {
indexed = false;
num_components = gs_color_space_num_components(pcs);
}
uses_color = pim->CombineWithColor && rop3_uses_T(pis->log_op);
}
code = gx_image_enum_common_init((gx_image_enum_common_t *) pie,
(const gs_data_image_t *) pim,
&clist_image_enum_procs, dev,
num_components, format);
{
int i;
for (i = 1; i < pie->num_planes; ++i)
varying_depths |= pie->plane_depths[i] != pie->plane_depths[0];
}
if (code < 0 ||
!USE_HL_IMAGES ||
(cdev->disable_mask & clist_disable_hl_image) ||
cdev->image_enum_id != gs_no_id ||
base_index > gs_color_space_index_DeviceCMYK ||
(uses_color && !gx_dc_is_pure(pdcolor)) ||
has_alpha ||
varying_depths ||
(code = gs_matrix_invert(&pim->ImageMatrix, &mat)) < 0 ||
(code = gs_matrix_multiply(&mat, &ctm_only(pis), &mat)) < 0 ||
!(cdev->disable_mask & clist_disable_nonrect_hl_image ?
(is_xxyy(&mat) || is_xyyx(&mat)) :
image_matrix_ok_to_band(&mat))
)
goto use_default;
{
int bytes_per_plane, bytes_per_row;
bits_per_pixel = pim->BitsPerComponent * num_components;
pie->image = *pim;
pie->dcolor = *pdcolor;
if (prect)
pie->rect = *prect;
else {
pie->rect.p.x = 0, pie->rect.p.y = 0;
pie->rect.q.x = pim->Width, pie->rect.q.y = pim->Height;
}
pie->pis = pis;
pie->pcpath = pcpath;
pie->format = format;
pie->bits_per_plane = bits_per_pixel / pie->num_planes;
pie->matrix = mat;
pie->uses_color = uses_color;
if (masked) {
pie->color_space.byte1 = 0;
pie->color_space.space = 0;
pie->color_space.id = gs_no_id;
} else {
pie->color_space.byte1 = (base_index << 4) |
(indexed ? (pim->ColorSpace->params.indexed.use_proc ? 12 : 8) : 0);
pie->color_space.id =
(pie->color_space.space = pim->ColorSpace)->id;
}
pie->y = pie->rect.p.y;
bytes_per_plane =
(pim->Width * pie->bits_per_plane + 7) >> 3;
bytes_per_row = bytes_per_plane * pie->num_planes;
bytes_per_row = max(bytes_per_row, 1);
if (cmd_largest_size + bytes_per_row > cdev->cend - cdev->cbuf)
goto use_default;
}
if (pim->Interpolate)
pie->support.x = pie->support.y = MAX_ISCALE_SUPPORT + 1;
else
pie->support.x = pie->support.y = 0;
sbox.p.x = pie->rect.p.x - pie->support.x;
sbox.p.y = pie->rect.p.y - pie->support.y;
sbox.q.x = pie->rect.q.x + pie->support.x;
sbox.q.y = pie->rect.q.y + pie->support.y;
gs_bbox_transform(&sbox, &mat, &dbox);
if (cdev->disable_mask & clist_disable_complex_clip)
if (!check_rect_for_trivial_clip(pcpath,
(int)floor(dbox.p.x), (int)floor(dbox.p.y),
(int)ceil(dbox.q.x), (int)ceil(dbox.q.y)))
goto use_default;
if ((pie->begin_image_command_length =
begin_image_command(pie->begin_image_command,
sizeof(pie->begin_image_command), pic)) < 0)
goto use_default;
if (!masked) {
gx_color_index all =
((gx_color_index)1 << dev->color_info.depth) - 1;
if (bits_per_pixel > 4 || pim->Interpolate || num_components > 1)
colors_used = all;
else {
int max_value = (1 << bits_per_pixel) - 1;
float dmin = pim->Decode[0], dmax = pim->Decode[1];
float dtemp;
if (dmax < dmin)
dtemp = dmax, dmax = dmin, dmin = dtemp;
if (dmin != 0 ||
dmax != (indexed ? max_value : 1)
) {
colors_used = all;
} else {
const gs_color_space *pcs = pim->ColorSpace;
cs_proc_remap_color((*remap_color)) = pcs->type->remap_color;
gs_client_color cc;
gx_drawing_color dcolor;
int i;
double denom = (indexed ? 1 : max_value);
for (i = 0; i <= max_value; ++i) {
cc.paint.values[0] = (double)i / denom;
remap_color(&cc, pcs, &dcolor, pis, dev,
gs_color_select_source);
colors_used |= cmd_drawing_colors_used(cdev, &dcolor);
}
}
}
}
pie->colors_used.or = colors_used;
pie->colors_used.slow_rop =
cmd_slow_rop(dev, pis->log_op, (uses_color ? pdcolor : NULL));
pie->color_map_is_known = false;
{
int y0 = (int)floor(dbox.p.y - 0.51);
int y1 = (int)ceil(dbox.q.y + 0.51);
pie->ymin = max(y0, 0);
pie->ymax = min(y1, dev->height);
}
cmd_clear_known(cdev, clist_image_unknowns(dev, pie) | begin_image_known);
cdev->image_enum_id = pie->id;
return 0;
use_default:
gs_free_object(mem, pie, "clist_begin_typed_image");
return gx_default_begin_typed_image(dev, pis, pmat, pic, prect,
pdcolor, pcpath, mem, pinfo);
}
private int
clist_image_plane_data(gx_image_enum_common_t * info,
const gx_image_plane_t * planes, int yh,
int *rows_used)
{
gx_device *dev = info->dev;
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
clist_image_enum *pie = (clist_image_enum *) info;
gs_rect sbox, dbox;
int y_orig = pie->y;
int yh_used = min(yh, pie->rect.q.y - y_orig);
int y0, y1;
int y, height;
int code;
#ifdef DEBUG
if (pie->id != cdev->image_enum_id) {
lprintf2("end_image id = %lu != clist image id = %lu!\n",
(ulong) pie->id, (ulong) cdev->image_enum_id);
*rows_used = 0;
return_error(gs_error_Fatal);
}
#endif
{
int i;
for (i = 1; i < info->num_planes; ++i)
if (planes[i].data_x != planes[0].data_x) {
*rows_used = 0;
return_error(gs_error_rangecheck);
}
}
sbox.p.x = pie->rect.p.x - pie->support.x;
sbox.p.y = (y0 = y_orig) - pie->support.y;
sbox.q.x = pie->rect.q.x + pie->support.x;
sbox.q.y = (y1 = pie->y += yh_used) + pie->support.y;
gs_bbox_transform(&sbox, &pie->matrix, &dbox);
{
int ry0 = (int)floor(dbox.p.y) - 2;
int ry1 = (int)ceil(dbox.q.y) + 2;
int band_height = cdev->page_band_height;
if (ry0 < pie->ymin)
ry0 = pie->ymin;
if (ry1 > pie->ymax)
ry1 = pie->ymax;
if (ry0 >= ry1)
goto done;
y = ry0 / band_height * band_height;
height = min(ROUND_UP(ry1, band_height), dev->height) - y;
}
FOR_RECTS {
gs_int_rect ibox;
gs_int_rect entire_box;
if (!image_band_box(dev, pie, y, height, &ibox))
continue;
{
int band_ymax = min(band_end, pie->ymax);
int band_ymin = max(band_end - band_height, pie->ymin);
if (!image_band_box(dev, pie, band_ymin,
band_ymax - band_ymin, &entire_box))
continue;
}
pcls->colors_used.or |= pie->colors_used.or;
pcls->colors_used.slow_rop |= pie->colors_used.slow_rop;
if (!(pcls->known & begin_image_known)) {
gs_logical_operation_t lop = pie->pis->log_op;
byte *dp;
byte *bp = pie->begin_image_command +
pie->begin_image_command_length;
uint len;
byte image_op = cmd_opv_begin_image;
TRY_RECT {
code = (pie->color_map_is_known ? 0 :
cmd_put_color_mapping(cdev, pie->pis));
pie->color_map_is_known = true;
if (code >= 0) {
uint want_known = ctm_known | clip_path_known |
op_bm_tk_known | opacity_alpha_known |
shape_alpha_known | alpha_known |
(pie->color_space.id == gs_no_id ? 0 :
color_space_known);
code = cmd_do_write_unknown(cdev, pcls, want_known);
}
if (code >= 0)
code = cmd_do_enable_clip(cdev, pcls, pie->pcpath != NULL);
if (code >= 0)
code = cmd_update_lop(cdev, pcls, lop);
} HANDLE_RECT(code);
if (pie->uses_color) {
TRY_RECT {
code = cmd_put_drawing_color(cdev, pcls, &pie->dcolor);
} HANDLE_RECT(code);
}
if (entire_box.p.x != 0 || entire_box.p.y != 0 ||
entire_box.q.x != pie->image.Width ||
entire_box.q.y != pie->image.Height
) {
image_op = cmd_opv_begin_image_rect;
cmd_put2w(entire_box.p.x, entire_box.p.y, bp);
cmd_put2w(pie->image.Width - entire_box.q.x,
pie->image.Height - entire_box.q.y, bp);
}
len = bp - pie->begin_image_command;
TRY_RECT {
code =
set_cmd_put_op(dp, cdev, pcls, image_op, 1 + len);
} HANDLE_RECT(code);
memcpy(dp + 1, pie->begin_image_command, len);
pcls->known |= begin_image_known;
}
{
int bx0 = entire_box.p.x, bx1 = entire_box.q.x;
int by0 = ibox.p.y, by1 = ibox.q.y;
int bpp = pie->bits_per_plane;
int num_planes = pie->num_planes;
uint offsets[gs_image_max_planes];
int i, iy, ih, xskip, xoff, nrows;
uint bytes_per_plane, bytes_per_row, rows_per_cmd;
if (by0 < y0)
by0 = y0;
if (by1 > y1)
by1 = y1;
xoff = bx0 - pie->rect.p.x;
xskip = xoff & -(int)"\001\010\004\010\002\010\004\010"[bpp & 7];
for (i = 0; i < num_planes; ++i)
offsets[i] =
(by0 - y0) * planes[i].raster + ((xskip * bpp) >> 3);
bytes_per_plane = ((bx1 - (pie->rect.p.x + xskip)) * bpp + 7) >> 3;
bytes_per_row = bytes_per_plane * pie->num_planes;
rows_per_cmd =
(cbuf_size - cmd_largest_size) / max(bytes_per_row, 1);
if (rows_per_cmd == 0) {
rows_per_cmd = 1;
}
for (iy = by0, ih = by1 - by0; ih > 0; iy += nrows, ih -= nrows) {
nrows = min(ih, rows_per_cmd);
TRY_RECT {
code = cmd_image_plane_data(cdev, pcls, planes, info,
bytes_per_plane, offsets,
xoff - xskip, nrows);
} HANDLE_RECT(code);
for (i = 0; i < num_planes; ++i)
offsets[i] += planes[i].raster * nrows;
}
}
} END_RECTS_ON_ERROR(
BEGIN
++cdev->ignore_lo_mem_warnings;
NEST_RECT {
code = write_image_end_all(dev, pie);
} UNNEST_RECT;
--cdev->ignore_lo_mem_warnings;
if (!pie->image.Interpolate)
pie->rect.p.y += yh_used;
END,
(code < 0 ? (band_code = code) : code) >= 0,
(cmd_clear_known(cdev,
clist_image_unknowns(dev, pie) | begin_image_known),
pie->color_map_is_known = false,
cdev->image_enum_id = pie->id, true)
);
done:
*rows_used = pie->y - y_orig;
return pie->y >= pie->rect.q.y;
}
private int
clist_image_end_image(gx_image_enum_common_t * info, bool draw_last)
{
gx_device *dev = info->dev;
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
clist_image_enum *pie = (clist_image_enum *) info;
int code;
#ifdef DEBUG
if (pie->id != cdev->image_enum_id) {
lprintf2("end_image id = %lu != clist image id = %lu!\n",
(ulong) pie->id, (ulong) cdev->image_enum_id);
return_error(gs_error_Fatal);
}
#endif
NEST_RECT {
do {
code = write_image_end_all(dev, pie);
} while (code < 0 && cdev->error_is_retryable &&
(code = clist_VMerror_recover(cdev, code)) >= 0
);
if (code < 0 && cdev->error_is_retryable) {
int retry_code;
++cdev->ignore_lo_mem_warnings;
retry_code = write_image_end_all(dev, pie);
--cdev->ignore_lo_mem_warnings;
if (retry_code >= 0 && cdev->driver_call_nesting == 0)
code = clist_VMerror_recover_flush(cdev, code);
}
} UNNEST_RECT;
cdev->image_enum_id = gs_no_id;
gs_free_object(pie->memory, pie, "clist_image_end_image");
return code;
}
int
clist_create_compositor(gx_device * dev,
gx_device ** pcdev, const gs_composite_t * pcte,
gs_imager_state * pis, gs_memory_t * mem)
{
byte * dp;
uint size = 0;
int code = pcte->type->procs.write(pcte, 0, &size);
if (code < 0 && code != gs_error_rangecheck)
return code;
size += 2 + 1;
code = pcte->type->procs.clist_compositor_write_update(pcte, dev,
pcdev, pis, mem);
if (code < 0)
return code;
if (pcte->type->comp_id == GX_COMPOSITOR_PDF14_TRANS) {
gx_device_clist_writer * const cldev =
&((gx_device_clist *)dev)->writer;
int len = cmd_write_ctm_return_length(cldev, &ctm_only(pis));
code = set_cmd_put_all_op(dp, (gx_device_clist_writer *)dev,
cmd_opv_set_ctm, len + 1);
if (code < 0)
return code;
code = cmd_write_ctm(&ctm_only(pis), dp, len);
if (code < 0)
return code;
}
code = set_cmd_put_all_op( dp,
(gx_device_clist_writer *)dev,
cmd_opv_extend,
size );
if (code < 0)
return code;
dp[1] = cmd_opv_ext_create_compositor;
dp[2] = pcte->type->comp_id;
if ((code = pcte->type->procs.write(pcte, dp + 3, &size)) < 0)
((gx_device_clist_writer *)dev)->cnext = dp;
return code;
}
private int
cmd_put_set_data_x(gx_device_clist_writer * cldev, gx_clist_state * pcls,
int data_x)
{
byte *dp;
int code;
if (data_x > 0x1f) {
int dx_msb = data_x >> 5;
code = set_cmd_put_op(dp, cldev, pcls, cmd_opv_set_misc,
2 + cmd_size_w(dx_msb));
if (code >= 0) {
dp[1] = cmd_set_misc_data_x + 0x20 + (data_x & 0x1f);
cmd_put_w(dx_msb, dp + 2);
}
} else {
code = set_cmd_put_op(dp, cldev, pcls, cmd_opv_set_misc, 2);
if (code >= 0)
dp[1] = cmd_set_misc_data_x + data_x;
}
return code;
}
int
cmd_put_halftone(gx_device_clist_writer * cldev, const gx_device_halftone * pdht)
{
uint    ht_size = 0, req_size;
byte *  dp;
byte *  dp0 = 0;
byte *  pht_buff = 0;
int     code = gx_ht_write(pdht, (gx_device *)cldev, 0, &ht_size);
if (code < 0 && code != gs_error_rangecheck)
return code;
req_size = 2 + enc_u_sizew(ht_size);
if ((code = set_cmd_put_all_op(dp, cldev, cmd_opv_extend, req_size)) < 0)
return code;
dp[1] = cmd_opv_ext_put_halftone;
dp += 2;
enc_u_putw(ht_size, dp);
if (ht_size > cbuf_ht_seg_max_size) {
pht_buff = gs_alloc_bytes( cldev->bandlist_memory,
ht_size,
"cmd_put_halftone" );
if (pht_buff == 0)
return_error(gs_error_VMerror);
} else {
req_size += ht_size;
code = set_cmd_put_all_op(dp, cldev, cmd_opv_extend, req_size);
if (code < 0)
return code;
dp0 = dp;
dp[1] = cmd_opv_ext_put_ht_seg;
dp += 2;
enc_u_putw(ht_size, dp);
pht_buff = dp;
}
code = gx_ht_write(pdht, (gx_device *)cldev, pht_buff, &ht_size);
if (code < 0) {
if (ht_size > cbuf_ht_seg_max_size)
gs_free_object( cldev->bandlist_memory,
pht_buff,
"cmd_put_halftone" );
else
cldev->cnext = dp0;
return code;
}
if (ht_size > cbuf_ht_seg_max_size) {
byte *  pbuff = pht_buff;
while (ht_size > 0 && code >= 0) {
int     seg_size, tmp_size;
seg_size = ( ht_size > cbuf_ht_seg_max_size ? cbuf_ht_seg_max_size
: ht_size );
tmp_size = 2 + enc_u_sizew(seg_size) + seg_size;
code = set_cmd_put_all_op(dp, cldev, cmd_opv_extend, tmp_size);
if (code >= 0) {
dp[1] = cmd_opv_ext_put_ht_seg;
dp += 2;
enc_u_putw(seg_size, dp);
memcpy(dp, pbuff, seg_size);
ht_size -= seg_size;
pbuff += seg_size;
}
}
gs_free_object( cldev->bandlist_memory, pht_buff, "cmd_put_halftone");
pht_buff = 0;
}
if (code >= 0)
cldev->device_halftone_id = pdht->id;
return code;
}
int
cmd_put_color_mapping(gx_device_clist_writer * cldev,
const gs_imager_state * pis)
{
int code;
const gx_device_halftone *pdht = pis->dev_ht;
if (pdht->id != cldev->device_halftone_id) {
code = cmd_put_halftone(cldev, pdht);
if (code < 0)
return code;
cldev->device_halftone_id = pdht->id;
}
code = cmd_put_color_map(cldev, cmd_map_black_generation,
0, pis->black_generation,
&cldev->black_generation_id);
if (code < 0)
return code;
code = cmd_put_color_map(cldev, cmd_map_undercolor_removal,
0, pis->undercolor_removal,
&cldev->undercolor_removal_id);
if (code < 0)
return code;
{
uint which = 0;
bool all_same = true;
bool send_default_comp = false;
int i;
gs_id default_comp_id, xfer_ids[4];
#define get_id(pis, color, color_num) \
((pis->set_transfer.color != NULL && pis->set_transfer.color_num >= 0) \
? pis->set_transfer.color->id\
: pis->set_transfer.gray->id)
xfer_ids[0] = get_id(pis, red, red_component_num);
xfer_ids[1] = get_id(pis, green, green_component_num);
xfer_ids[2] = get_id(pis, blue, blue_component_num);
xfer_ids[3] = default_comp_id = pis->set_transfer.gray->id;
#undef get_id
for (i = 0; i < countof(cldev->transfer_ids); ++i) {
if (xfer_ids[i] != cldev->transfer_ids[i])
which |= 1 << i;
if (xfer_ids[i] != default_comp_id)
all_same = false;
if (xfer_ids[i] == default_comp_id &&
cldev->transfer_ids[i] != default_comp_id)
send_default_comp = true;
}
if (which == 0)
return 0;
if (send_default_comp || cldev->transfer_ids[0] != default_comp_id) {
gs_id dummy = gs_no_id;
code = cmd_put_color_map(cldev, cmd_map_transfer, 0,
pis->set_transfer.gray, &dummy);
if (code < 0)
return code;
for (i = 0; i < countof(cldev->transfer_ids); ++i)
cldev->transfer_ids[i] = default_comp_id;
}
if (cldev->transfer_ids[0] != xfer_ids[0]) {
code = cmd_put_color_map(cldev, cmd_map_transfer_0,
pis->set_transfer.red_component_num,
pis->set_transfer.red, &cldev->transfer_ids[0]);
if (code < 0)
return code;
}
if (cldev->transfer_ids[1] != xfer_ids[1]) {
code = cmd_put_color_map(cldev, cmd_map_transfer_1,
pis->set_transfer.green_component_num,
pis->set_transfer.green, &cldev->transfer_ids[1]);
if (code < 0)
return code;
}
if (cldev->transfer_ids[2] != xfer_ids[2]) {
code = cmd_put_color_map(cldev, cmd_map_transfer_2,
pis->set_transfer.blue_component_num,
pis->set_transfer.blue, &cldev->transfer_ids[2]);
if (code < 0)
return code;
}
}
return 0;
}
#define I_FLOOR(x) ((int)floor(x))
#define I_CEIL(x) ((int)ceil(x))
private void
box_merge_point(gs_int_rect * pbox, floatp x, floatp y)
{
int t;
if ((t = I_FLOOR(x)) < pbox->p.x)
pbox->p.x = t;
if ((t = I_CEIL(x)) > pbox->q.x)
pbox->q.x = t;
if ((t = I_FLOOR(y)) < pbox->p.y)
pbox->p.y = t;
if ((t = I_CEIL(y)) > pbox->q.y)
pbox->q.y = t;
}
private bool
image_band_box(gx_device * dev, const clist_image_enum * pie, int y, int h,
gs_int_rect * pbox)
{
fixed by0 = int2fixed(y);
fixed by1 = int2fixed(y + h);
int
px = pie->rect.p.x, py = pie->rect.p.y,
qx = pie->rect.q.x, qy = pie->rect.q.y;
gs_fixed_rect cbox;
gs_rect bbox;
(*dev_proc(dev, get_clipping_box)) (dev, &cbox);
bbox.p.x = fixed2float(cbox.p.x - fixed_half);
bbox.q.x = fixed2float(cbox.q.x + fixed_half);
bbox.p.y = fixed2float(max(cbox.p.y, by0) - fixed_half);
bbox.q.y = fixed2float(min(cbox.q.y, by1) + fixed_half);
#ifdef DEBUG
if (gs_debug_c('b')) {
dlprintf6("[b]band box for (%d,%d),(%d,%d), band (%d,%d) =>\n",
px, py, qx, qy, y, y + h);
dlprintf10("      (%g,%g),(%g,%g), matrix=[%g %g %g %g %g %g]\n",
bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y,
pie->matrix.xx, pie->matrix.xy, pie->matrix.yx,
pie->matrix.yy, pie->matrix.tx, pie->matrix.ty);
}
#endif
if (is_xxyy(&pie->matrix) || is_xyyx(&pie->matrix)) {
gs_rect ibox;
if (gs_bbox_transform_inverse(&bbox, &pie->matrix, &ibox) < 0)
return false;
pbox->p.x = max(px, I_FLOOR(ibox.p.x));
pbox->q.x = min(qx, I_CEIL(ibox.q.x));
pbox->p.y = max(py, I_FLOOR(ibox.p.y));
pbox->q.y = min(qy, I_CEIL(ibox.q.y));
} else {
gs_point rect[4];
gs_point corners[5];
int i;
rect[0].x = rect[3].x = px;
rect[1].x = rect[2].x = qx;
rect[0].y = rect[1].y = py;
rect[2].y = rect[3].y = qy;
if (gs_point_transform_inverse(bbox.p.x, bbox.p.y, &pie->matrix,
&corners[0]) < 0 ||
gs_point_transform_inverse(bbox.q.x, bbox.p.y, &pie->matrix,
&corners[1]) < 0 ||
gs_point_transform_inverse(bbox.q.x, bbox.q.y, &pie->matrix,
&corners[2]) < 0 ||
gs_point_transform_inverse(bbox.p.x, bbox.q.y, &pie->matrix,
&corners[3]) < 0
) {
if_debug0('b', "[b]can't inverse-transform a band corner!\n");
return false;
}
corners[4] = corners[0];
pbox->p.x = qx, pbox->p.y = qy;
pbox->q.x = px, pbox->q.y = py;
for (i = 0; i < 4; ++i) {
gs_point pa, pt;
double dx, dy;
pa = rect[i];
gs_point_transform(pa.x, pa.y, &pie->matrix, &pt);
if (pt.x >= bbox.p.x && pt.x <= bbox.q.x &&
pt.y >= bbox.p.y && pt.y <= bbox.q.y
)
box_merge_point(pbox, pa.x, pa.y);
pa = corners[i];
if (pa.x >= px && pa.x <= qx && pa.y >= py && pa.y <= qy)
box_merge_point(pbox, pa.x, pa.y);
dx = corners[i + 1].x - pa.x;
dy = corners[i + 1].y - pa.y;
#define in_range(t, tc, p, q)\
(0 <= t && t <= 1 && (t = tc) >= p && t <= q)
if (dx != 0) {
double t = (px - pa.x) / dx;
if_debug3('b', "   (px) t=%g => (%d,%g)\n",
t, px, pa.y + t * dy);
if (in_range(t, pa.y + t * dy, py, qy))
box_merge_point(pbox, (floatp) px, t);
t = (qx - pa.x) / dx;
if_debug3('b', "   (qx) t=%g => (%d,%g)\n",
t, qx, pa.y + t * dy);
if (in_range(t, pa.y + t * dy, py, qy))
box_merge_point(pbox, (floatp) qx, t);
}
if (dy != 0) {
double t = (py - pa.y) / dy;
if_debug3('b', "   (py) t=%g => (%g,%d)\n",
t, pa.x + t * dx, py);
if (in_range(t, pa.x + t * dx, px, qx))
box_merge_point(pbox, t, (floatp) py);
t = (qy - pa.y) / dy;
if_debug3('b', "   (qy) t=%g => (%g,%d)\n",
t, pa.x + t * dx, qy);
if (in_range(t, pa.x + t * dx, px, qx))
box_merge_point(pbox, t, (floatp) qy);
}
#undef in_range
}
}
if_debug4('b', "    => (%d,%d),(%d,%d)\n", pbox->p.x, pbox->p.y,
pbox->q.x, pbox->q.y);
if ((pbox->p.x -= pie->support.x) < pie->rect.p.x)
pbox->p.x = pie->rect.p.x;
if ((pbox->p.y -= pie->support.y) < pie->rect.p.y)
pbox->p.y = pie->rect.p.y;
if ((pbox->q.x += pie->support.x) > pie->rect.q.x)
pbox->q.x = pie->rect.q.x;
if ((pbox->q.y += pie->support.y) > pie->rect.q.y)
pbox->q.y = pie->rect.q.y;
return (pbox->p.x < pbox->q.x && pbox->p.y < pbox->q.y);
}
private uint
clist_image_unknowns(gx_device *dev, const clist_image_enum *pie)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
const gs_imager_state *const pis = pie->pis;
uint unknown = 0;
if (cdev->imager_state.ctm.xx != pis->ctm.xx ||
cdev->imager_state.ctm.xy != pis->ctm.xy ||
cdev->imager_state.ctm.yx != pis->ctm.yx ||
cdev->imager_state.ctm.yy != pis->ctm.yy ||
cdev->imager_state.ctm.tx != pis->ctm.tx ||
cdev->imager_state.ctm.ty != pis->ctm.ty
) {
unknown |= ctm_known;
cdev->imager_state.ctm = pis->ctm;
}
if (pie->color_space.id == gs_no_id) {
cdev->color_space.space = 0;
} else {
if (cdev->color_space.id == pie->color_space.id) {
cdev->color_space.space = pie->color_space.space;
} else {
unknown |= color_space_known;
cdev->color_space = pie->color_space;
}
}
if (cmd_check_clip_path(cdev, pie->pcpath))
unknown |= clip_path_known;
if (cdev->imager_state.overprint != pis->overprint ||
cdev->imager_state.overprint_mode != pis->overprint_mode ||
cdev->imager_state.blend_mode != pis->blend_mode ||
cdev->imager_state.text_knockout != pis->text_knockout) {
unknown |= op_bm_tk_known;
cdev->imager_state.overprint = pis->overprint;
cdev->imager_state.overprint_mode = pis->overprint_mode;
cdev->imager_state.blend_mode = pis->blend_mode;
cdev->imager_state.text_knockout = pis->text_knockout;
}
if (cdev->imager_state.opacity.alpha != pis->opacity.alpha) {
unknown |= opacity_alpha_known;
cdev->imager_state.opacity.alpha = pis->opacity.alpha;
}
if (cdev->imager_state.shape.alpha != pis->shape.alpha) {
unknown |= shape_alpha_known;
cdev->imager_state.shape.alpha = pis->shape.alpha;
}
if (cdev->imager_state.alpha != pis->alpha) {
unknown |= alpha_known;
cdev->imager_state.alpha = pis->alpha;
}
return unknown;
}
private int
begin_image_command(byte *buf, uint buf_size, const gs_image_common_t *pic)
{
int i;
stream s;
const gs_color_space *ignore_pcs;
int code;
for (i = 0; i < gx_image_type_table_count; ++i)
if (gx_image_type_table[i] == pic->type)
break;
if (i >= gx_image_type_table_count)
return_error(gs_error_rangecheck);
s_init(&s, NULL);
swrite_string(&s, buf, buf_size);
sputc(&s, (byte)i);
code = pic->type->sput(pic, &s, &ignore_pcs);
return (code < 0 ? code : stell(&s));
}
private int
cmd_image_plane_data(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const gx_image_plane_t * planes,
const gx_image_enum_common_t * pie,
uint bytes_per_plane, const uint * offsets,
int dx, int h)
{
int data_x = planes[0].data_x + dx;
uint nbytes = bytes_per_plane * pie->num_planes * h;
uint len = 1 + cmd_size2w(h, bytes_per_plane) + nbytes;
byte *dp;
uint offset = 0;
int plane, i;
int code;
if (data_x) {
code = cmd_put_set_data_x(cldev, pcls, data_x);
if (code < 0)
return code;
offset = ((data_x & ~7) * cldev->color_info.depth) >> 3;
}
code = set_cmd_put_op(dp, cldev, pcls, cmd_opv_image_data, len);
if (code < 0)
return code;
dp++;
cmd_put2w(h, bytes_per_plane, dp);
for (plane = 0; plane < pie->num_planes; ++plane)
for (i = 0; i < h; ++i) {
memcpy(dp,
planes[plane].data + i * planes[plane].raster +
offsets[plane] + offset,
bytes_per_plane);
dp += bytes_per_plane;
}
return 0;
}
private int
write_image_end_all(gx_device *dev, const clist_image_enum *pie)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code;
int y = pie->ymin;
int height = pie->ymax - y;
if (height <= 0)
return 0;
FOR_RECTS {
byte *dp;
if (!(pcls->known & begin_image_known))
continue;
TRY_RECT {
if_debug1('L', "[L]image_end for band %d\n", band);
code = set_cmd_put_op(dp, cdev, pcls, cmd_opv_image_data, 2);
} HANDLE_RECT(code);
dp[1] = 0;
pcls->known ^= begin_image_known;
} END_RECTS;
return 0;
}
private bool
check_rect_for_trivial_clip(
const gx_clip_path *pcpath,
int px, int py, int qx, int qy
)
{
gs_fixed_rect obox;
gs_fixed_rect imgbox;
if (!pcpath)
return true;
imgbox.p.x = int2fixed(px);
imgbox.p.y = int2fixed(py);
imgbox.q.x = int2fixed(qx);
imgbox.q.y = int2fixed(qy);
if (gx_cpath_includes_rectangle(pcpath,
imgbox.p.x, imgbox.p.y,
imgbox.q.x, imgbox.q.y))
return true;
return (gx_cpath_outer_box(pcpath, &obox)  &&
obox.p.x <= imgbox.q.x && obox.q.x >= imgbox.p.x &&
obox.p.y <= imgbox.q.y && obox.q.y >= imgbox.p.y );
}