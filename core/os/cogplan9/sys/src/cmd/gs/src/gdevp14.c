#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gscdefs.h"
#include "gxdevice.h"
#include "gsdevice.h"
#include "gsstruct.h"
#include "gxistate.h"
#include "gxdcolor.h"
#include "gxiparam.h"
#include "gstparam.h"
#include "gxblend.h"
#include "gxtext.h"
#include "gsdfilt.h"
#include "gsimage.h"
#include "gsrect.h"
#include "gzstate.h"
#include "gdevp14.h"
#include "gsovrc.h"
#include "gxcmap.h"
#include "gscolor1.h"
#include "gstrans.h"
#include "gsutil.h"
#include "gxcldev.h"
#ifdef DUMP_TO_PNG
#include "png_.h"
#endif
# define INCR(v) DO_NOTHING
#define	PDF14_MAX_PLANES 16
private
ENUM_PTRS_WITH(pdf14_buf_enum_ptrs, pdf14_buf *buf)
return 0;
case 0: return ENUM_OBJ(buf->saved);
case 1: return ENUM_OBJ(buf->data);
case 2: return ENUM_OBJ(buf->transfer_fn);
ENUM_PTRS_END
private
RELOC_PTRS_WITH(pdf14_buf_reloc_ptrs, pdf14_buf	*buf)
{
RELOC_VAR(buf->saved);
RELOC_VAR(buf->data);
RELOC_VAR(buf->transfer_fn);
}
RELOC_PTRS_END
gs_private_st_composite(st_pdf14_buf, pdf14_buf, "pdf14_buf",
pdf14_buf_enum_ptrs, pdf14_buf_reloc_ptrs);
gs_private_st_ptrs2(st_pdf14_ctx, pdf14_ctx, "pdf14_ctx",
pdf14_ctx_enum_ptrs, pdf14_ctx_reloc_ptrs,
stack, maskbuf);
#define	X_DPI 72
#define	Y_DPI 72
private	int pdf14_open(gx_device * pdev);
private	dev_proc_close_device(pdf14_close);
private	int pdf14_output_page(gx_device	* pdev,	int num_copies,	int flush);
private	dev_proc_put_params(pdf14_put_params);
private	dev_proc_encode_color(pdf14_encode_color);
private	dev_proc_decode_color(pdf14_decode_color);
private	dev_proc_fill_rectangle(pdf14_fill_rectangle);
private	dev_proc_fill_rectangle(pdf14_mark_fill_rectangle);
private	dev_proc_fill_rectangle(pdf14_mark_fill_rectangle_ko_simple);
private	dev_proc_fill_path(pdf14_fill_path);
private	dev_proc_stroke_path(pdf14_stroke_path);
private	dev_proc_begin_typed_image(pdf14_begin_typed_image);
private	dev_proc_text_begin(pdf14_text_begin);
private	dev_proc_create_compositor(pdf14_create_compositor);
private	dev_proc_create_compositor(pdf14_forward_create_compositor);
private	dev_proc_begin_transparency_group(pdf14_begin_transparency_group);
private	dev_proc_end_transparency_group(pdf14_end_transparency_group);
private	dev_proc_begin_transparency_mask(pdf14_begin_transparency_mask);
private	dev_proc_end_transparency_mask(pdf14_end_transparency_mask);
private	const gx_color_map_procs *
pdf14_get_cmap_procs(const gs_imager_state *, const gx_device *);
#define	XSIZE (int)(8.5	* X_DPI)
#define	YSIZE (int)(11 * Y_DPI)
#define	pdf14_procs(get_color_mapping_procs, get_color_comp_index) \
{\
pdf14_open,			\
NULL,				\
NULL,				\
pdf14_output_page,		\
pdf14_close,			\
pdf14_encode_color,		\
pdf14_decode_color,		\
pdf14_fill_rectangle,		\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
pdf14_put_params,		\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
pdf14_fill_path,		\
pdf14_stroke_path,		\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
NULL,				\
pdf14_begin_typed_image,	\
NULL,				\
NULL,				\
pdf14_create_compositor,	\
NULL,				\
pdf14_text_begin,		\
NULL,				\
pdf14_begin_transparency_group,\
pdf14_end_transparency_group,\
pdf14_begin_transparency_mask,\
pdf14_end_transparency_mask,\
NULL,				\
get_color_mapping_procs,	\
get_color_comp_index,		\
pdf14_encode_color,		\
pdf14_decode_color		\
}
private	const gx_device_procs pdf14_Gray_procs =
pdf14_procs(gx_default_DevGray_get_color_mapping_procs,
gx_default_DevGray_get_color_comp_index);
private	const gx_device_procs pdf14_RGB_procs =
pdf14_procs(gx_default_DevRGB_get_color_mapping_procs,
gx_default_DevRGB_get_color_comp_index);
private	const gx_device_procs pdf14_CMYK_procs =
pdf14_procs(gx_default_DevCMYK_get_color_mapping_procs,
gx_default_DevCMYK_get_color_comp_index);
gs_private_st_composite_use_final(st_pdf14_device, pdf14_device, "pdf14_device",
pdf14_device_enum_ptrs, pdf14_device_reloc_ptrs,
gx_device_finalize);
const pdf14_device gs_pdf14_Gray_device	= {
std_device_color_stype_body(pdf14_device, &pdf14_Gray_procs, "pdf14gray",
&st_pdf14_device,
XSIZE, YSIZE, X_DPI, Y_DPI, 8, 255, 256),
{ 0 }
};
const pdf14_device gs_pdf14_RGB_device = {
std_device_color_stype_body(pdf14_device, &pdf14_RGB_procs, "pdf14RGB",
&st_pdf14_device,
XSIZE, YSIZE, X_DPI, Y_DPI, 24, 255, 256),
{ 0 }
};
const pdf14_device gs_pdf14_CMYK_device	= {
std_device_std_color_full_body_type(pdf14_device, &pdf14_CMYK_procs,
"PDF14cmyk", &st_pdf14_device, XSIZE, YSIZE, X_DPI, Y_DPI, 32,
0, 0, 0, 0, 0, 0),
{ 0 }
};
private
ENUM_PTRS_WITH(pdf14_device_enum_ptrs, pdf14_device *pdev) return 0;
case 0:	return ENUM_OBJ(pdev->ctx);
case 1:	ENUM_RETURN(gx_device_enum_ptr(pdev->target));
ENUM_PTRS_END
private	RELOC_PTRS_WITH(pdf14_device_reloc_ptrs, pdf14_device *pdev)
{
RELOC_VAR(pdev->ctx);
pdev->target = gx_device_reloc_ptr(pdev->target, gcst);
}
RELOC_PTRS_END
private	pdf14_buf *
pdf14_buf_new(gs_int_rect *rect, bool has_alpha_g, bool	has_shape,
int n_chan,
gs_memory_t *memory)
{
pdf14_buf *result;
int rowstride = (rect->q.x - rect->p.x + 3) & -4;
int height = (rect->q.y - rect->p.y);
int n_planes = n_chan + (has_shape ? 1 : 0) + (has_alpha_g ? 1 : 0);
int planestride;
double dsize = (((double) rowstride) * height) * n_planes;
if (dsize > (double)max_uint)
return NULL;
result = gs_alloc_struct(memory, pdf14_buf, &st_pdf14_buf,
"pdf14_buf_new");
if (result == NULL)
return result;
result->isolated = false;
result->knockout = false;
result->has_alpha_g = has_alpha_g;
result->has_shape = has_shape;
result->rect = *rect;
result->n_chan = n_chan;
result->n_planes = n_planes;
result->rowstride = rowstride;
result->transfer_fn = NULL;
if (height < 0) {
result->planestride = 0;
result->data = 0;
} else {
planestride = rowstride * height;
result->planestride = planestride;
result->data = gs_alloc_bytes(memory, planestride * n_planes,
"pdf14_buf_new");
if (result->data == NULL) {
gs_free_object(memory, result, "pdf_buf_new");
return NULL;
}
if (has_alpha_g) {
int alpha_g_plane = n_chan + (has_shape ? 1 : 0);
memset (result->data + alpha_g_plane * planestride, 0, planestride);
}
}
result->bbox.p.x = max_int;
result->bbox.p.y = max_int;
result->bbox.q.x = min_int;
result->bbox.q.y = min_int;
return result;
}
private	void
pdf14_buf_free(pdf14_buf *buf, gs_memory_t *memory)
{
gs_free_object(memory, buf->transfer_fn, "pdf14_buf_free");
gs_free_object(memory, buf->data, "pdf14_buf_free");
gs_free_object(memory, buf, "pdf14_buf_free");
}
private	pdf14_ctx *
pdf14_ctx_new(gs_int_rect *rect, int n_chan, bool additive, gs_memory_t	*memory)
{
pdf14_ctx *result;
pdf14_buf *buf;
result = gs_alloc_struct(memory, pdf14_ctx, &st_pdf14_ctx,
"pdf14_ctx_new");
if (result == NULL)
return result;
buf = pdf14_buf_new(rect, false, false, n_chan, memory);
if (buf == NULL) {
gs_free_object(memory, result, "pdf14_ctx_new");
return NULL;
}
if_debug3('v', "[v]base buf: %d x %d, %d channels\n",
buf->rect.q.x, buf->rect.q.y, buf->n_chan);
memset(buf->data, 0, buf->planestride * buf->n_planes);
buf->saved = NULL;
result->stack = buf;
result->maskbuf = NULL;
result->n_chan = n_chan;
result->memory = memory;
result->rect = *rect;
result->additive = additive;
return result;
}
private	void
pdf14_ctx_free(pdf14_ctx *ctx)
{
pdf14_buf *buf, *next;
for (buf = ctx->stack; buf != NULL; buf = next) {
next = buf->saved;
pdf14_buf_free(buf, ctx->memory);
}
gs_free_object (ctx->memory, ctx, "pdf14_ctx_free");
}
private	pdf14_buf *
pdf14_find_backdrop_buf(pdf14_ctx *ctx)
{
pdf14_buf *buf = ctx->stack;
while (buf != NULL) {
if (buf->isolated) return NULL;
if (!buf->knockout) return buf->saved;
buf = buf->saved;
}
return NULL;
}
private	int
pdf14_push_transparency_group(pdf14_ctx	*ctx, gs_int_rect *rect,
bool isolated, bool knockout,
byte alpha, byte shape,
gs_blend_mode_t blend_mode)
{
pdf14_buf *tos = ctx->stack;
pdf14_buf *buf, *backdrop;
bool has_shape;
if (knockout)
isolated = true;
has_shape = tos->has_shape || tos->knockout;
buf = pdf14_buf_new(rect, !isolated, has_shape, ctx->n_chan, ctx->memory);
if_debug3('v', "[v]push buf: %d x %d, %d channels\n", buf->rect.p.x, buf->rect.p.y, buf->n_chan);
if (buf == NULL)
return_error(gs_error_VMerror);
buf->isolated = isolated;
buf->knockout = knockout;
buf->alpha = alpha;
buf->shape = shape;
buf->blend_mode = blend_mode;
buf->saved = tos;
ctx->stack = buf;
backdrop = pdf14_find_backdrop_buf(ctx);
if (backdrop == NULL) {
memset(buf->data, 0, buf->planestride * (buf->n_chan +
(buf->has_shape ? 1 : 0)));
} else {
byte *buf_plane = buf->data;
byte *tos_plane = tos->data + buf->rect.p.x - tos->rect.p.x +
(buf->rect.p.y - tos->rect.p.y) * tos->rowstride;
int width = buf->rect.q.x - buf->rect.p.x;
int y0 = buf->rect.p.y;
int y1 = buf->rect.q.y;
int i;
int n_chan_copy = buf->n_chan + (tos->has_shape ? 1 : 0);
for (i = 0; i < n_chan_copy; i++) {
byte *buf_ptr = buf_plane;
byte *tos_ptr = tos_plane;
int y;
for (y = y0; y < y1; ++y) {
memcpy (buf_ptr, tos_ptr, width);
buf_ptr += buf->rowstride;
tos_ptr += tos->rowstride;
}
buf_plane += buf->planestride;
tos_plane += tos->planestride;
}
if (has_shape && !tos->has_shape)
memset (buf_plane, 0, buf->planestride);
}
return 0;
}
private	int
pdf14_pop_transparency_group(pdf14_ctx *ctx)
{
pdf14_buf *tos = ctx->stack;
pdf14_buf *nos = tos->saved;
pdf14_buf *maskbuf = ctx->maskbuf;
int y0 = tos->rect.p.y;
int y1 = tos->rect.q.y;
if (y0 < y1) {
int x0 = tos->rect.p.x;
int x1 = tos->rect.q.x;
int n_chan = ctx->n_chan;
int num_comp = n_chan - 1;
byte alpha = tos->alpha;
byte shape = tos->shape;
byte blend_mode = tos->blend_mode;
byte *tos_ptr = tos->data;
byte *nos_ptr = nos->data + x0 - nos->rect.p.x +
(y0 - nos->rect.p.y) * nos->rowstride;
byte *mask_ptr = NULL;
int tos_planestride = tos->planestride;
int nos_planestride = nos->planestride;
int mask_planestride = 0x0badf00d;
byte mask_bg_alpha = 0;
int width = x1 - x0;
int x, y;
int i;
byte tos_pixel[PDF14_MAX_PLANES];
byte nos_pixel[PDF14_MAX_PLANES];
bool tos_isolated = tos->isolated;
bool nos_knockout = nos->knockout;
byte *nos_alpha_g_ptr;
int tos_shape_offset = n_chan * tos_planestride;
int tos_alpha_g_offset = tos_shape_offset +
(tos->has_shape ? tos_planestride : 0);
int nos_shape_offset = n_chan * nos_planestride;
bool nos_has_shape = nos->has_shape;
byte *mask_tr_fn = NULL;
bool additive = ctx->additive;
if (nos == NULL)
return_error(gs_error_rangecheck);
rect_merge(nos->bbox, tos->bbox);
if_debug6('v', "pdf14_pop_transparency_group y0 = %d, y1 = %d, w = %d, alpha = %d, shape = %d, bm = %d\n",
y0, y1, width, alpha, shape, blend_mode);
if (nos->has_alpha_g)
nos_alpha_g_ptr = nos_ptr + n_chan * nos_planestride;
else
nos_alpha_g_ptr = NULL;
if (maskbuf != NULL) {
mask_ptr = maskbuf->data + x0 - maskbuf->rect.p.x +
(y0 - maskbuf->rect.p.y) * maskbuf->rowstride;
mask_planestride = maskbuf->planestride;
mask_bg_alpha = maskbuf->alpha;
mask_tr_fn = maskbuf->transfer_fn;
}
for (y = y0; y < y1; ++y) {
for (x = 0; x < width; ++x) {
byte pix_alpha = alpha;
if (additive) {
for (i = 0; i < n_chan; ++i) {
tos_pixel[i] = tos_ptr[x + i * tos_planestride];
nos_pixel[i] = nos_ptr[x + i * nos_planestride];
}
} else {
for (i = 0; i < num_comp; ++i) {
tos_pixel[i] = 255 - tos_ptr[x + i * tos_planestride];
nos_pixel[i] = 255 - nos_ptr[x + i * nos_planestride];
}
tos_pixel[num_comp] = tos_ptr[x + num_comp * tos_planestride];
nos_pixel[num_comp] = nos_ptr[x + num_comp * nos_planestride];
}
if (mask_ptr != NULL) {
int mask_alpha = mask_ptr[x + num_comp * mask_planestride];
int tmp;
byte mask;
if (mask_alpha == 255) {
mask = additive ? mask_ptr[x]
: 255 - mask_ptr[x + 3 * mask_planestride];
} else if (mask_alpha == 0)
mask = mask_bg_alpha;
else {
int t2 = additive ? mask_ptr[x]
: 255 - mask_ptr[x + 3 * mask_planestride];
t2 = (t2 - mask_bg_alpha) * mask_alpha + 0x80;
mask = mask_bg_alpha + ((t2 + (t2 >> 8)) >> 8);
}
mask = mask_tr_fn[mask];
tmp = pix_alpha * mask + 0x80;
pix_alpha = (tmp + (tmp >> 8)) >> 8;
}
if (nos_knockout) {
byte *nos_shape_ptr = nos_has_shape ?
&nos_ptr[x + nos_shape_offset] : NULL;
byte tos_shape = tos_ptr[x + tos_shape_offset];
art_pdf_composite_knockout_isolated_8(nos_pixel,
nos_shape_ptr,
tos_pixel,
n_chan - 1,
tos_shape,
pix_alpha, shape);
} else if (tos_isolated) {
art_pdf_composite_group_8(nos_pixel, nos_alpha_g_ptr,
tos_pixel,
n_chan - 1,
pix_alpha, blend_mode);
} else {
byte tos_alpha_g = tos_ptr[x + tos_alpha_g_offset];
art_pdf_recomposite_group_8(nos_pixel, nos_alpha_g_ptr,
tos_pixel, tos_alpha_g,
n_chan - 1,
pix_alpha, blend_mode);
}
if (nos_has_shape) {
nos_ptr[x + nos_shape_offset] =
art_pdf_union_mul_8 (nos_ptr[x + nos_shape_offset],
tos_ptr[x + tos_shape_offset],
shape);
}
if (additive) {
for (i = 0; i < n_chan; ++i) {
nos_ptr[x + i * nos_planestride] = nos_pixel[i];
}
} else {
for (i = 0; i < num_comp; ++i)
nos_ptr[x + i * nos_planestride] = 255 - nos_pixel[i];
nos_ptr[x + num_comp * nos_planestride] = nos_pixel[num_comp];
}
if (nos_alpha_g_ptr != NULL)
++nos_alpha_g_ptr;
}
tos_ptr += tos->rowstride;
nos_ptr += nos->rowstride;
if (nos_alpha_g_ptr != NULL)
nos_alpha_g_ptr += nos->rowstride - width;
if (mask_ptr != NULL)
mask_ptr += maskbuf->rowstride;
}
}
ctx->stack = nos;
if_debug0('v', "[v]pop buf\n");
pdf14_buf_free(tos, ctx->memory);
if (maskbuf != NULL) {
pdf14_buf_free(maskbuf, ctx->memory);
ctx->maskbuf = NULL;
}
return 0;
}
private	int
pdf14_push_transparency_mask(pdf14_ctx *ctx, gs_int_rect *rect,	byte bg_alpha,
byte *transfer_fn)
{
pdf14_buf *buf;
if_debug0('v', "[v]pdf_push_transparency_mask\n");
buf = pdf14_buf_new(rect, false, false, ctx->n_chan, ctx->memory);
if (buf == NULL)
return_error(gs_error_VMerror);
buf->isolated = true;
buf->knockout = false;
buf->alpha = bg_alpha;
buf->shape = 0xff;
buf->blend_mode = BLEND_MODE_Normal;
buf->transfer_fn = transfer_fn;
buf->saved = ctx->stack;
ctx->stack = buf;
memset(buf->data, 0, buf->planestride * buf->n_chan);
return 0;
}
private	int
pdf14_pop_transparency_mask(pdf14_ctx *ctx)
{
pdf14_buf *tos = ctx->stack;
ctx->stack = tos->saved;
ctx->maskbuf = tos;
return 0;
}
private	int
pdf14_open(gx_device *dev)
{
pdf14_device *pdev = (pdf14_device *)dev;
gs_int_rect rect;
if_debug2('v', "[v]pdf14_open: width = %d, height = %d\n",
dev->width, dev->height);
rect.p.x = 0;
rect.p.y = 0;
rect.q.x = dev->width;
rect.q.y = dev->height;
pdev->ctx = pdf14_ctx_new(&rect, dev->color_info.num_components + 1,
pdev->color_info.polarity != GX_CINFO_POLARITY_SUBTRACTIVE, dev->memory);
if (pdev->ctx == NULL)
return_error(gs_error_VMerror);
return 0;
}
private	gx_color_index
pdf14_encode_color(gx_device *dev, const gx_color_value	colors[])
{
int drop = sizeof(gx_color_value) * 8 - 8;
gx_color_index color = 0;
int i;
int ncomp = dev->color_info.num_components;
for (i = 0; i < ncomp; i++) {
color <<= 8;
color |= (colors[i] >> drop);
}
return (color == gx_no_color_index ? color ^ 1 : color);
}
private	int
pdf14_decode_color(gx_device * dev, gx_color_index color, gx_color_value * out)
{
int i;
int ncomp = dev->color_info.num_components;
for (i = 0; i < ncomp; i++) {
out[ncomp - i - 1] = (gx_color_value) ((color & 0xff) * 0x101);
color >>= 8;
}
return 0;
}
#ifdef DUMP_TO_PNG
private	int
dump_planar_rgba(gs_memory_t *mem,
const byte *buf, int width, int height, int rowstride, int planestride)
{
int rowbytes = width << 2;
byte *row = gs_malloc(mem, rowbytes, 1, "png raster buffer");
png_struct *png_ptr =
png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
png_info *info_ptr =
png_create_info_struct(png_ptr);
const char *software_key = "Software";
char software_text[256];
png_text text_png;
const byte *buf_ptr = buf;
FILE *file;
int code;
int y;
file = fopen ("c:\\temp\\tmp.png", "wb");
if_debug0('v', "[v]pnga_output_page\n");
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
info_ptr->width = width;
info_ptr->height = height;
info_ptr->x_pixels_per_unit =
(png_uint_32) (96.0 * (100.0 / 2.54));
info_ptr->y_pixels_per_unit =
(png_uint_32) (96.0 * (100.0 / 2.54));
info_ptr->phys_unit_type = PNG_RESOLUTION_METER;
info_ptr->valid |= PNG_INFO_pHYs;
info_ptr->bit_depth = 8;
info_ptr->color_type = PNG_COLOR_TYPE_RGB_ALPHA;
sprintf(software_text, "%s %d.%02d", gs_product,
(int)(gs_revision / 100), (int)(gs_revision % 100));
text_png.compression = -1;
text_png.key = (char *)software_key;
text_png.text = software_text;
text_png.text_length = strlen(software_text);
info_ptr->text = &text_png;
info_ptr->num_text = 1;
png_write_info(png_ptr, info_ptr);
info_ptr->num_text = 0;
info_ptr->text = NULL;
for (y = 0; y < height; ++y) {
int x;
for (x = 0; x < width; ++x) {
row[(x << 2)] = buf_ptr[x];
row[(x << 2) + 1] = buf_ptr[x + planestride];
row[(x << 2) + 2] = buf_ptr[x + planestride * 2];
row[(x << 2) + 3] = buf_ptr[x + planestride * 3];
}
png_write_row(png_ptr, row);
buf_ptr += rowstride;
}
png_write_end(png_ptr, info_ptr);
done:
png_destroy_write_struct(&png_ptr, &info_ptr);
gs_free(mem, row, rowbytes, 1, "png raster buffer");
fclose (file);
return code;
}
#endif
private	int
pdf14_put_image(pdf14_device *pdev, gs_imager_state *pis, gx_device *target)
{
int code;
gs_image1_t image;
gs_matrix pmat;
gx_image_enum_common_t *info;
int width = pdev->width;
int height = pdev->height;
int y;
pdf14_buf *buf = pdev->ctx->stack;
int planestride = buf->planestride;
int num_comp = buf->n_chan - 1;
byte *buf_ptr = buf->data;
byte *linebuf;
gs_color_space cs;
const byte bg = pdev->ctx->additive ? 255 : 0;
#ifdef DUMP_TO_PNG
dump_planar_rgba(pdev->memory, buf_ptr, width, height,
buf->rowstride, buf->planestride);
#endif
#if 0
rc_increment(pdev);
gs_setdevice_no_init(pgs, target);
#endif
if_debug0('v', "[v]pdf14_put_image\n");
switch (num_comp) {
case 1:
gs_cspace_init_DeviceGray(pis->memory, &cs);
break;
case 3:
gs_cspace_init_DeviceRGB(pis->memory, &cs);
break;
case 4:
gs_cspace_init_DeviceCMYK(pis->memory, &cs);
break;
default:
return_error(gs_error_rangecheck);
break;
}
gs_image_t_init_adjust(&image, &cs, false);
image.ImageMatrix.xx = (float)width;
image.ImageMatrix.yy = (float)height;
image.Width = width;
image.Height = height;
image.BitsPerComponent = 8;
pmat.xx = (float)width;
pmat.xy = 0;
pmat.yx = 0;
pmat.yy = (float)height;
pmat.tx = 0;
pmat.ty = 0;
code = dev_proc(target, begin_typed_image) (target,
pis, &pmat,
(gs_image_common_t *)&image,
NULL, NULL, NULL,
pis->memory, &info);
if (code < 0)
return code;
linebuf = gs_alloc_bytes(pdev->memory, width * num_comp, "pdf14_put_image");
for (y = 0; y < height; y++) {
gx_image_plane_t planes;
int x;
int rows_used;
for (x = 0; x < width; x++) {
byte comp, a;
int tmp, comp_num;
a = buf_ptr[x + planestride * num_comp];
if ((a + 1) & 0xfe) {
a ^= 0xff;
for (comp_num = 0; comp_num < num_comp; comp_num++) {
comp  = buf_ptr[x + planestride * comp_num];
tmp = ((bg - comp) * a) + 0x80;
comp += (tmp + (tmp >> 8)) >> 8;
linebuf[x * num_comp + comp_num] = comp;
}
} else if (a == 0) {
for (comp_num = 0; comp_num < num_comp; comp_num++) {
linebuf[x * num_comp + comp_num] = bg;
}
} else {
for (comp_num = 0; comp_num < num_comp; comp_num++) {
comp = buf_ptr[x + planestride * comp_num];
linebuf[x * num_comp + comp_num] = comp;
}
}
}
planes.data = linebuf;
planes.data_x = 0;
planes.raster = width * num_comp;
info->procs->plane_data(info, &planes, 1, &rows_used);
buf_ptr += buf->rowstride;
}
gs_free_object(pdev->memory, linebuf, "pdf14_put_image");
info->procs->end_image(info, true);
#if 0
gs_setdevice_no_init(pgs, (gx_device*) pdev);
rc_decrement_only(pdev, "pdf_14_put_image");
#endif
return code;
}
private	int
pdf14_close(gx_device *dev)
{
pdf14_device *pdev = (pdf14_device *)dev;
if (pdev->ctx) {
pdf14_ctx_free(pdev->ctx);
pdev->ctx = NULL;
}
return 0;
}
private	int
pdf14_output_page(gx_device * dev, int num_copies, int flush)
{
pdf14_device * pdev = (pdf14_device *)dev;
if (pdev->target != NULL)
return (*dev_proc(pdev->target, output_page)) (pdev->target, num_copies, flush);
return 0;
}
#define	COPY_PARAM(p) dev->p = target->p
#define	COPY_ARRAY_PARAM(p) memcpy(dev->p, target->p, sizeof(dev->p))
private	void
gs_pdf14_device_copy_params(gx_device *dev, const gx_device *target)
{
COPY_PARAM(width);
COPY_PARAM(height);
COPY_ARRAY_PARAM(MediaSize);
COPY_ARRAY_PARAM(ImagingBBox);
COPY_PARAM(ImagingBBox_set);
COPY_ARRAY_PARAM(HWResolution);
COPY_ARRAY_PARAM(MarginsHWResolution);
COPY_ARRAY_PARAM(Margins);
COPY_ARRAY_PARAM(HWMargins);
COPY_PARAM(PageCount);
#undef COPY_ARRAY_PARAM
#undef COPY_PARAM
}
private	int
pdf14_forward_put_params(gx_device * dev, gs_param_list	* plist)
{
pdf14_device * pdev = (pdf14_device *)dev;
gx_device * tdev = pdev->target;
int code = 0;
if (tdev != 0 && (code = dev_proc(tdev, put_params)(tdev, plist)) >= 0) {
gx_device_decache_colors(dev);
if (!tdev->is_open)
code = gs_closedevice(dev);
gx_device_copy_params(dev, tdev);
}
return code;
}
private	int
pdf14_put_params(gx_device * dev, gs_param_list	* plist)
{
pdf14_device * pdev = (pdf14_device *)dev;
gx_device * tdev = pdev->target;
int code = 0;
if (tdev != 0 && (code = dev_proc(tdev, put_params)(tdev, plist)) >= 0) {
gx_device_decache_colors(dev);
if (!tdev->is_open)
code = gs_closedevice(dev);
gs_pdf14_device_copy_params(dev, tdev);
}
return code;
}
private	void
pdf14_set_marking_params(gx_device *dev, const gs_imager_state *pis)
{
pdf14_device * pdev = (pdf14_device *)dev;
pdev->opacity = pis->opacity.alpha;
pdev->shape = pis->shape.alpha;
pdev->alpha = pis->opacity.alpha * pis->shape.alpha;
pdev->blend_mode = pis->blend_mode;
if_debug3('v', "[v]set_marking_params, opacity = %g, shape = %g, bm = %d\n",
pdev->opacity, pdev->shape, pis->blend_mode);
}
private	int
pdf14_fill_path(gx_device *dev,	const gs_imager_state *pis,
gx_path *ppath, const gx_fill_params *params,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath)
{
gs_imager_state new_is = *pis;
new_is.log_op |= lop_pdf14;
pdf14_set_marking_params(dev, pis);
return gx_default_fill_path(dev, &new_is, ppath, params, pdcolor, pcpath);
}
private	int
pdf14_stroke_path(gx_device *dev, const	gs_imager_state	*pis,
gx_path *ppath, const gx_stroke_params *params,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath)
{
gs_imager_state new_is = *pis;
new_is.log_op |= lop_pdf14;
pdf14_set_marking_params(dev, pis);
return gx_default_stroke_path(dev, &new_is, ppath, params, pdcolor,
pcpath);
}
private	int
pdf14_begin_typed_image(gx_device * dev, const gs_imager_state * pis,
const gs_matrix *pmat, const gs_image_common_t *pic,
const gs_int_rect * prect,
const gx_drawing_color * pdcolor,
const gx_clip_path * pcpath, gs_memory_t * mem,
gx_image_enum_common_t ** pinfo)
{
pdf14_set_marking_params(dev, pis);
return gx_default_begin_typed_image(dev, pis, pmat, pic, prect, pdcolor,
pcpath, mem, pinfo);
}
private	void
pdf14_set_params(gs_imager_state * pis,	gx_device * dev,
const gs_pdf14trans_params_t * pparams)
{
if (pparams->changed & PDF14_SET_BLEND_MODE)
pis->blend_mode = pparams->blend_mode;
if (pparams->changed & PDF14_SET_TEXT_KNOCKOUT)
pis->text_knockout = pparams->text_knockout;
if (pparams->changed & PDF14_SET_SHAPE_ALPHA)
pis->shape.alpha = pparams->shape.alpha;
if (pparams->changed & PDF14_SET_OPACITY_ALPHA)
pis->opacity.alpha = pparams->opacity.alpha;
pdf14_set_marking_params(dev, pis);
}
private	int
pdf14_forward_open_device(gx_device * dev)
{
gx_device_forward * pdev = (gx_device_forward *)dev;
gx_device * tdev = pdev->target;
int code = 0;
if (tdev == 0)
return_error(gs_error_unknownerror);
if ((code = gs_opendevice(tdev)) >= 0)
gx_device_copy_params(dev, tdev);
return code;
}
private	void
pdf14_forward_device_procs(gx_device * dev)
{
gx_device_forward * pdev = (gx_device_forward *)dev;
memset(&(pdev->procs), 0, size_of(pdev->procs));
gx_device_forward_fill_in_procs(pdev);
set_dev_proc(dev, close_device, gx_forward_close_device);
set_dev_proc(dev, fill_rectangle, gx_forward_fill_rectangle);
set_dev_proc(dev, tile_rectangle, gx_forward_tile_rectangle);
set_dev_proc(dev, copy_mono, gx_forward_copy_mono);
set_dev_proc(dev, copy_color, gx_forward_copy_color);
set_dev_proc(dev, get_page_device, gx_forward_get_page_device);
set_dev_proc(dev, strip_tile_rectangle, gx_forward_strip_tile_rectangle);
set_dev_proc(dev, copy_alpha, gx_forward_copy_alpha);
set_dev_proc(dev, open_device, pdf14_forward_open_device);
set_dev_proc(dev, put_params, pdf14_forward_put_params);
}
private	int
pdf14_disable_device(gx_device * dev)
{
gx_device_forward * pdev = (gx_device_forward *)dev;
if_debug0('v', "[v]pdf14_disable_device\n");
dev->color_info = pdev->target->color_info;
pdf14_forward_device_procs(dev);
set_dev_proc(dev, create_compositor, pdf14_forward_create_compositor);
return 0;
}
private	pdf14_default_colorspace_t
pdf14_determine_default_blend_cs(gx_device * pdev)
{
if (pdev->color_info.polarity == GX_CINFO_POLARITY_SUBTRACTIVE)
return DeviceCMYK;
else {
return DeviceRGB;
}
}
private	int
get_pdf14_device_proto(gx_device * dev,
const pdf14_device ** pdevproto)
{
pdf14_default_colorspace_t dev_cs =
pdf14_determine_default_blend_cs(dev);
switch (dev_cs) {
case DeviceGray:
*pdevproto = &gs_pdf14_Gray_device;
break;
case DeviceRGB:
*pdevproto = &gs_pdf14_RGB_device;
break;
case DeviceCMYK:
*pdevproto = &gs_pdf14_CMYK_device;
break;
default:
return_error(gs_error_rangecheck);
}
return 0;
}
private	int
pdf14_recreate_device(gs_memory_t *mem,	gs_imager_state	* pis,
gx_device * dev)
{
pdf14_device * pdev = (pdf14_device *)dev;
gx_device * target = pdev->target;
const pdf14_device * dev_proto;
int code;
if_debug0('v', "[v]pdf14_recreate_device\n");
code = get_pdf14_device_proto(target, &dev_proto);
if (code < 0)
return code;
pdev->color_info = dev_proto->color_info;
pdev->procs = dev_proto->procs;
gx_device_fill_in_procs(dev);
check_device_separable((gx_device *)pdev);
return code;
}
private	int
gx_update_pdf14_compositor(gx_device * pdev, gs_imager_state * pis,
const gs_pdf14trans_t * pdf14pct, gs_memory_t * mem )
{
pdf14_device *p14dev = (pdf14_device *)pdev;
int code = 0;
switch (pdf14pct->params.pdf14_op) {
default:
break;
case PDF14_PUSH_DEVICE:
p14dev->blend_mode = 0;
p14dev->opacity = p14dev->shape = 0.0;
pdf14_recreate_device(mem, pis, pdev);
break;
case PDF14_POP_DEVICE:
pis->get_cmap_procs = p14dev->save_get_cmap_procs;
gx_set_cmap_procs(pis, p14dev->target);
code = pdf14_put_image(p14dev, pis, p14dev->target);
pdf14_disable_device(pdev);
pdf14_close(pdev);
break;
case PDF14_BEGIN_TRANS_GROUP:
code = gx_begin_transparency_group(pis, pdev, &pdf14pct->params);
break;
case PDF14_END_TRANS_GROUP:
code = gx_end_transparency_group(pis, pdev);
break;
case PDF14_INIT_TRANS_MASK:
code = gx_init_transparency_mask(pis, &pdf14pct->params);
break;
case PDF14_BEGIN_TRANS_MASK:
code = gx_begin_transparency_mask(pis, pdev, &pdf14pct->params);
break;
case PDF14_END_TRANS_MASK:
code = gx_end_transparency_mask(pis, pdev, &pdf14pct->params);
break;
case PDF14_SET_BLEND_PARAMS:
pdf14_set_params(pis, pdev, &pdf14pct->params);
break;
}
return code;
}
private	int
pdf14_forward_create_compositor(gx_device * dev, gx_device * * pcdev,
const gs_composite_t * pct, gs_imager_state * pis,
gs_memory_t * mem)
{
pdf14_device *pdev = (pdf14_device *)dev;
gx_device * tdev = pdev->target;
gx_device * ndev;
int code = 0;
*pcdev = dev;
if (gs_is_pdf14trans_compositor(pct)) {
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pct;
if (pdf14pct->params.pdf14_op == PDF14_PUSH_DEVICE)
return gx_update_pdf14_compositor(dev, pis, pdf14pct, mem);
return 0;
}
code = dev_proc(tdev, create_compositor)(tdev, &ndev, pct, pis, mem);
if (code < 0)
return code;
pdev->target = ndev;
return 0;
}
private	int
pdf14_create_compositor(gx_device * dev, gx_device * * pcdev,
const gs_composite_t * pct, gs_imager_state * pis,
gs_memory_t * mem)
{
if (gs_is_pdf14trans_compositor(pct)) {
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pct;
*pcdev = dev;
return gx_update_pdf14_compositor(dev, pis, pdf14pct, mem);
} else if (gs_is_overprint_compositor(pct)) {
*pcdev = dev;
return 0;
} else
return gx_no_create_compositor(dev, pcdev, pct, pis, mem);
}
private	int
pdf14_text_begin(gx_device * dev, gs_imager_state * pis,
const gs_text_params_t * text, gs_font * font,
gx_path * path, const gx_device_color * pdcolor,
const gx_clip_path * pcpath, gs_memory_t * memory,
gs_text_enum_t ** ppenum)
{
int code;
gs_text_enum_t *penum;
if_debug0('v', "[v]pdf14_text_begin\n");
pdf14_set_marking_params(dev, pis);
code = gx_default_text_begin(dev, pis, text, font, path, pdcolor, pcpath,
memory, &penum);
if (code < 0)
return code;
*ppenum = (gs_text_enum_t *)penum;
return code;
}
private	int
pdf14_fill_rectangle(gx_device * dev,
int x, int y, int w, int h, gx_color_index color)
{
pdf14_device *pdev = (pdf14_device *)dev;
pdf14_buf *buf = pdev->ctx->stack;
fit_fill_xywh(dev, x, y, w, h);
if (w <= 0 || h <= 0)
return 0;
if (buf->knockout)
return pdf14_mark_fill_rectangle_ko_simple(dev, x, y, w, h, color);
else
return pdf14_mark_fill_rectangle(dev, x, y, w, h, color);
}
private	int
pdf14_begin_transparency_group(gx_device *dev,
const gs_transparency_group_params_t *ptgp,
const gs_rect *pbbox,
gs_imager_state *pis,
gs_transparency_state_t **ppts,
gs_memory_t *mem)
{
pdf14_device *pdev = (pdf14_device *)dev;
double alpha = pis->opacity.alpha * pis->shape.alpha;
gs_rect dev_bbox;
gs_int_rect rect;
int code;
code = gs_bbox_transform(pbbox, &ctm_only(pis), &dev_bbox);
if (code < 0)
return code;
rect.p.x = (int)floor(dev_bbox.p.x);
rect.p.y = (int)floor(dev_bbox.p.y);
rect.q.x = (int)ceil(dev_bbox.q.x);
rect.q.y = (int)ceil(dev_bbox.q.y);
rect_intersect(rect, pdev->ctx->rect);
if_debug4('v', "[v]begin_transparency_group, I = %d, K = %d, alpha = %g, bm = %d\n",
ptgp->Isolated, ptgp->Knockout, alpha, pis->blend_mode);
code = pdf14_push_transparency_group(pdev->ctx, &rect,
ptgp->Isolated, ptgp->Knockout,
(byte)floor (255 * alpha + 0.5),
(byte)floor (255 * pis->shape.alpha + 0.5),
pis->blend_mode);
return code;
}
private	int
pdf14_end_transparency_group(gx_device *dev,
gs_imager_state *pis,
gs_transparency_state_t **ppts)
{
pdf14_device *pdev = (pdf14_device *)dev;
int code;
if_debug0('v', "[v]end_transparency_group\n");
code = pdf14_pop_transparency_group(pdev->ctx);
return code;
}
private	int
pdf14_begin_transparency_mask(gx_device	*dev,
const gx_transparency_mask_params_t *ptmp,
const gs_rect *pbbox,
gs_imager_state *pis,
gs_transparency_state_t **ppts,
gs_memory_t *mem)
{
pdf14_device *pdev = (pdf14_device *)dev;
byte bg_alpha = 0;
byte *transfer_fn = (byte *)gs_alloc_bytes(pdev->ctx->memory, 256,
"pdf14_push_transparency_mask");
if (ptmp->Background_components)
bg_alpha = (int)(255 * ptmp->Background[0] + 0.5);
if_debug1('v', "begin transparency mask, bg_alpha = %d\n", bg_alpha);
memcpy(transfer_fn, ptmp->transfer_fn, size_of(ptmp->transfer_fn));
return pdf14_push_transparency_mask(pdev->ctx, &pdev->ctx->rect, bg_alpha,
transfer_fn);
}
private	int
pdf14_end_transparency_mask(gx_device *dev,
gs_transparency_mask_t **pptm)
{
pdf14_device *pdev = (pdf14_device *)dev;
if_debug0('v', "end transparency mask!\n");
return pdf14_pop_transparency_mask(pdev->ctx);
}
private	int
pdf14_mark_fill_rectangle(gx_device * dev,
int x, int y, int w, int h, gx_color_index color)
{
pdf14_device *pdev = (pdf14_device *)dev;
pdf14_buf *buf = pdev->ctx->stack;
int i, j, k;
byte *line, *dst_ptr;
byte src[PDF14_MAX_PLANES];
byte dst[PDF14_MAX_PLANES];
gs_blend_mode_t blend_mode = pdev->blend_mode;
bool additive = pdev->ctx->additive;
int rowstride = buf->rowstride;
int planestride = buf->planestride;
bool has_alpha_g = buf->has_alpha_g;
bool has_shape = buf->has_shape;
int num_chan = buf->n_chan;
int num_comp = num_chan - 1;
int shape_off = num_chan * planestride;
int alpha_g_off = shape_off + (has_shape ? planestride : 0);
byte shape = 0;
byte src_alpha;
if_debug7('v', "[v]pdf14_mark_fill_rectangle, (%d, %d), %d x %d color = %lx  bm %d, nc %d,\n", x, y, w, h, color, blend_mode, num_chan);
if (additive) {
for (i = num_comp - 1; i >= 0; i--) {
src[i] = (byte)(color & 0xff);
color >>= 8;
}
}
else {
for (i = num_comp - 1; i >= 0; i--) {
src[i] = (byte)(0xff - (color & 0xff));
color >>= 8;
}
}
src_alpha = src[num_comp] = (byte)floor (255 * pdev->alpha + 0.5);
if (has_shape)
shape = (byte)floor (255 * pdev->shape + 0.5);
if (x < buf->rect.p.x) x = buf->rect.p.x;
if (y < buf->rect.p.y) y = buf->rect.p.y;
if (x + w > buf->rect.q.x) w = buf->rect.q.x - x;
if (y + h > buf->rect.q.y) h = buf->rect.q.y - y;
if (x < buf->bbox.p.x) buf->bbox.p.x = x;
if (y < buf->bbox.p.y) buf->bbox.p.y = y;
if (x + w > buf->bbox.q.x) buf->bbox.q.x = x + w;
if (y + h > buf->bbox.q.y) buf->bbox.q.y = y + h;
line = buf->data + (x - buf->rect.p.x) + (y - buf->rect.p.y) * rowstride;
for (j = 0; j < h; ++j) {
dst_ptr = line;
for (i = 0; i < w; ++i) {
if (additive) {
for (k = 0; k < num_chan; ++k)
dst[k] = dst_ptr[k * planestride];
}
else {
for (k = 0; k < num_comp; ++k)
dst[k] = 255 - dst_ptr[k * planestride];
dst[num_comp] = dst_ptr[num_comp * planestride];
}
art_pdf_composite_pixel_alpha_8(dst, src, num_comp, blend_mode);
if (additive) {
for (k = 0; k < num_chan; ++k)
dst_ptr[k * planestride] = dst[k];
}
else {
for (k = 0; k < num_comp; ++k)
dst_ptr[k * planestride] = 255 - dst[k];
dst_ptr[num_comp * planestride] = dst[num_comp];
}
if (has_alpha_g) {
int tmp = (255 - dst_ptr[alpha_g_off]) * (255 - src_alpha) + 0x80;
dst_ptr[alpha_g_off] = 255 - ((tmp + (tmp >> 8)) >> 8);
}
if (has_shape) {
int tmp = (255 - dst_ptr[shape_off]) * (255 - shape) + 0x80;
dst_ptr[shape_off] = 255 - ((tmp + (tmp >> 8)) >> 8);
}
++dst_ptr;
}
line += rowstride;
}
return 0;
}
private	int
pdf14_mark_fill_rectangle_ko_simple(gx_device *	dev,
int x, int y, int w, int h, gx_color_index color)
{
pdf14_device *pdev = (pdf14_device *)dev;
pdf14_buf *buf = pdev->ctx->stack;
int i, j, k;
byte *line, *dst_ptr;
byte src[PDF14_MAX_PLANES];
byte dst[PDF14_MAX_PLANES];
int rowstride = buf->rowstride;
int planestride = buf->planestride;
int num_chan = buf->n_chan;
int num_comp = num_chan - 1;
int shape_off = num_chan * planestride;
bool has_shape = buf->has_shape;
byte opacity;
bool additive = pdev->ctx->additive;
if_debug6('v', "[v]pdf14_mark_fill_rectangle_ko_simple, (%d, %d), %d x %d color = %lx  bm %d, nc %d,\n", x, y, w, h, color, num_chan);
if (additive) {
for (i = num_comp - 1; i >= 0; i--) {
src[i] = (byte)(color & 0xff);
color >>= 8;
}
}
else {
for (i = num_comp - 1; i >= 0; i--) {
src[i] = (byte)(0xff - (color & 0xff));
color >>= 8;
}
}
src[num_comp] = (byte)floor (255 * pdev->alpha + 0.5);
opacity = (byte)floor (255 * pdev->opacity + 0.5);
if (x < buf->rect.p.x) x = buf->rect.p.x;
if (y < buf->rect.p.y) y = buf->rect.p.y;
if (x + w > buf->rect.q.x) w = buf->rect.q.x - x;
if (y + h > buf->rect.q.y) h = buf->rect.q.y - y;
if (x < buf->bbox.p.x) buf->bbox.p.x = x;
if (y < buf->bbox.p.y) buf->bbox.p.y = y;
if (x + w > buf->bbox.q.x) buf->bbox.q.x = x + w;
if (y + h > buf->bbox.q.y) buf->bbox.q.y = y + h;
line = buf->data + (x - buf->rect.p.x) + (y - buf->rect.p.y) * rowstride;
for (j = 0; j < h; ++j) {
dst_ptr = line;
for (i = 0; i < w; ++i) {
if (additive) {
for (k = 0; k < num_chan; ++k)
dst[k] = dst_ptr[k * planestride];
}
else {
for (k = 0; k < num_comp; ++k)
dst[k] = 255 - dst_ptr[k * planestride];
dst[num_comp] = dst_ptr[num_comp * planestride];
}
art_pdf_composite_knockout_simple_8(dst,
has_shape ? dst_ptr + shape_off : NULL, src, num_comp, opacity);
if (additive) {
for (k = 0; k < num_chan; ++k)
dst_ptr[k * planestride] = dst[k];
}
else {
for (k = 0; k < num_comp; ++k)
dst_ptr[k * planestride] = 255 - dst[k];
dst_ptr[num_comp * planestride] = dst[num_comp];
}
++dst_ptr;
}
line += rowstride;
}
return 0;
}
private	cmap_proc_gray(pdf14_cmap_gray_direct);
private	cmap_proc_rgb(pdf14_cmap_rgb_direct);
private	cmap_proc_cmyk(pdf14_cmap_cmyk_direct);
private	cmap_proc_rgb_alpha(pdf14_cmap_rgb_alpha_direct);
private	cmap_proc_separation(pdf14_cmap_separation_direct);
private	cmap_proc_devicen(pdf14_cmap_devicen_direct);
private	cmap_proc_is_halftoned(pdf14_cmap_is_halftoned);
private	const gx_color_map_procs pdf14_cmap_many = {
pdf14_cmap_gray_direct,
pdf14_cmap_rgb_direct,
pdf14_cmap_cmyk_direct,
pdf14_cmap_rgb_alpha_direct,
pdf14_cmap_separation_direct,
pdf14_cmap_devicen_direct,
pdf14_cmap_is_halftoned
};
private	inline void
map_components_to_colorants(const frac * pcc,
const gs_devicen_color_map * pcolor_component_map, frac * plist)
{
int i = pcolor_component_map->num_colorants - 1;
int pos;
for (; i >= 0; i--) {
plist[i] = frac_0;
}
for (i = pcolor_component_map->num_components - 1; i >= 0; i--) {
pos = pcolor_component_map->color_map[i];
if (pos >= 0)
plist[pos] = pcc[i];
}
}
private	void
pdf14_cmap_gray_direct(frac gray, gx_device_color * pdc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_gray(dev, gray, cm_comps);
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(cm_comps[i]);
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	void
pdf14_cmap_rgb_direct(frac r, frac g, frac b, gx_device_color *	pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_rgb(dev, pis, r, g, b, cm_comps);
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(cm_comps[i]);
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	void
pdf14_cmap_cmyk_direct(frac c, frac m, frac y, frac k, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_cmyk(dev, c, m, y, k, cm_comps);
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(cm_comps[i]);
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	void
pdf14_cmap_rgb_alpha_direct(frac r, frac g, frac b, frac alpha,	gx_device_color	* pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv_alpha, cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_rgb(dev, pis, r, g, b, cm_comps);
if (alpha != frac_1) {
#ifdef PREMULTIPLY_TOWARDS_WHITE
frac alpha_bias = frac_1 - alpha;
#else
frac alpha_bias = 0;
#endif
for (i = 0; i < ncomps; i++)
cm_comps[i] = (frac)((long)cm_comps[i] * alpha) / frac_1 + alpha_bias;
}
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(cm_comps[i]);
if (dev_proc(dev, map_rgb_alpha_color) != gx_default_map_rgb_alpha_color &&
(cv_alpha = frac2cv(alpha)) != gx_max_color_value)
color = dev_proc(dev, map_rgb_alpha_color)(dev, cv[0], cv[1], cv[2], cv_alpha);
else
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	void
pdf14_cmap_separation_direct(frac all, gx_device_color * pdc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
bool additive = dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE;
frac comp_value = all;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
if (pis->color_component_map.sep_type == SEP_ALL) {
if (additive)
comp_value = frac_1 - comp_value;
i = pis->color_component_map.num_colorants - 1;
for (; i >= 0; i--)
cm_comps[i] = comp_value;
}
else {
map_components_to_colorants(&comp_value, &(pis->color_component_map), cm_comps);
}
if (additive)
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]));
else
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]));
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	void
pdf14_cmap_devicen_direct(const	frac * pcc,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
map_components_to_colorants(pcc, &(pis->color_component_map), cm_comps);;
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]));
else
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]));
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
}
private	bool
pdf14_cmap_is_halftoned(const gs_imager_state *	pis, gx_device * dev)
{
return false;
}
private	const gx_color_map_procs *
pdf14_get_cmap_procs(const gs_imager_state *pis, const gx_device * dev)
{
return &pdf14_cmap_many;
}
int
gs_pdf14_device_push(gs_memory_t *mem, gs_imager_state * pis,
gx_device ** pdev, gx_device * target)
{
const pdf14_device * dev_proto;
pdf14_device *p14dev;
int code;
if_debug0('v', "[v]gs_pdf14_device_push\n");
code = get_pdf14_device_proto(target, &dev_proto);
if (code < 0)
return code;
code = gs_copydevice((gx_device **) &p14dev,
(const gx_device *) dev_proto, mem);
if (code < 0)
return code;
check_device_separable((gx_device *)p14dev);
gx_device_fill_in_procs((gx_device *)p14dev);
gs_pdf14_device_copy_params((gx_device *)p14dev, target);
rc_assign(p14dev->target, target, "gs_pdf14_device_push");
p14dev->save_get_cmap_procs = pis->get_cmap_procs;
pis->get_cmap_procs = pdf14_get_cmap_procs;
gx_set_cmap_procs(pis, (gx_device *)p14dev);
code = dev_proc((gx_device *) p14dev, open_device) ((gx_device *) p14dev);
*pdev = (gx_device *) p14dev;
pdf14_set_marking_params((gx_device *)p14dev, pis);
return code;
}
#define	private_st_gs_pdf14trans_t()\
gs_private_st_ptrs1(st_pdf14trans, gs_pdf14trans_t, "gs_pdf14trans_t",\
st_pdf14trans_enum_ptrs, st_pdf14trans_reloc_ptrs, params.transfer_function)
private_st_gs_pdf14trans_t();
private	bool
c_pdf14trans_equal(const gs_composite_t	* pct0,	const gs_composite_t * pct1)
{
return false;
}
#ifdef DEBUG
static char * pdf14_opcode_names[] = PDF14_OPCODE_NAMES;
#endif
#define	put_value(dp, value)\
memcpy(dp, &value, sizeof(value));\
dp += sizeof(value)
private	int
c_pdf14trans_write(const gs_composite_t	* pct, byte * data, uint * psize)
{
const gs_pdf14trans_params_t * pparams = &((const gs_pdf14trans_t *)pct)->params;
int need, avail = *psize;
byte buf[21 + sizeof(pparams->Background)
+ sizeof(pparams->GrayBackground) + sizeof(pparams->bbox)];
byte * pbuf = buf;
int opcode = pparams->pdf14_op;
int mask_size = 0;
*pbuf++ = opcode;
switch (opcode) {
default:
break;
case PDF14_PUSH_DEVICE:
case PDF14_POP_DEVICE:
case PDF14_END_TRANS_GROUP:
case PDF14_END_TRANS_MASK:
break;
case PDF14_BEGIN_TRANS_GROUP:
*pbuf++ = (pparams->Isolated & 1) + ((pparams->Knockout & 1) << 1);
*pbuf++ = pparams->blend_mode;
put_value(pbuf, pparams->opacity.alpha);
put_value(pbuf, pparams->shape.alpha);
put_value(pbuf, pparams->bbox);
break;
case PDF14_INIT_TRANS_MASK:
*pbuf++ = pparams->csel;
break;
case PDF14_BEGIN_TRANS_MASK:
put_value(pbuf, pparams->subtype);
*pbuf++ = pparams->function_is_identity;
*pbuf++ = pparams->Background_components;
if (pparams->Background_components) {
const int l = sizeof(pparams->Background[0]) * pparams->Background_components;
memcpy(pbuf, pparams->Background, l);
pbuf += l;
memcpy(pbuf, &pparams->GrayBackground, sizeof(pparams->GrayBackground));
pbuf += sizeof(pparams->GrayBackground);
}
if (!pparams->function_is_identity)
mask_size = sizeof(pparams->transfer_fn);
break;
case PDF14_SET_BLEND_PARAMS:
*pbuf++ = pparams->changed;
if (pparams->changed & PDF14_SET_BLEND_MODE)
*pbuf++ = pparams->blend_mode;
if (pparams->changed & PDF14_SET_TEXT_KNOCKOUT)
*pbuf++ = pparams->text_knockout;
if (pparams->changed & PDF14_SET_OPACITY_ALPHA)
put_value(pbuf, pparams->opacity.alpha);
if (pparams->changed & PDF14_SET_SHAPE_ALPHA)
put_value(pbuf, pparams->shape.alpha);
break;
}
#undef put_value
need = (pbuf - buf) + mask_size;
*psize = need;
if (need > avail)
return_error(gs_error_rangecheck);
memcpy(data, buf, need - mask_size);
if (mask_size)
memcpy(data + need - mask_size, pparams->transfer_fn, mask_size);
if_debug2('v', "[v] c_pdf14trans_write: opcode = %s need = %d\n",
pdf14_opcode_names[opcode], need);
return 0;
}
int gs_create_pdf14trans( gs_composite_t ** ppct,
const gs_pdf14trans_params_t * pparams,
gs_memory_t * mem );
#define	read_value(dp, value)\
memcpy(&value, dp, sizeof(value));\
dp += sizeof(value)
private	int
c_pdf14trans_read(gs_composite_t * * ppct, const byte *	data,
uint size, gs_memory_t * mem )
{
gs_pdf14trans_params_t params = {0};
const byte * start = data;
int used, code = 0;
if (size < 1)
return_error(gs_error_rangecheck);
params.pdf14_op = *data++;
if_debug2('v', "[v] c_pdf14trans_read: opcode = %s  avail = %d",
pdf14_opcode_names[params.pdf14_op], size);
switch (params.pdf14_op) {
default:
break;
case PDF14_PUSH_DEVICE:
case PDF14_POP_DEVICE:
case PDF14_END_TRANS_GROUP:
break;
case PDF14_BEGIN_TRANS_GROUP:
params.Isolated = (*data) & 1;
params.Knockout = (*data++ >> 1) & 1;
params.blend_mode = *data++;
read_value(data, params.opacity.alpha);
read_value(data, params.shape.alpha);
read_value(data, params.bbox);
break;
case PDF14_INIT_TRANS_MASK:
params.csel = *data++;
break;
case PDF14_BEGIN_TRANS_MASK:
read_value(data, params.subtype);
params.function_is_identity = *data++;
params.Background_components = *data++;
if (params.Background_components) {
const int l = sizeof(params.Background[0]) * params.Background_components;
memcpy(params.Background, data, l);
data += l;
memcpy(&params.GrayBackground, data, sizeof(params.GrayBackground));
data += sizeof(params.GrayBackground);
}
if (params.function_is_identity) {
int i;
for (i = 0; i < MASK_TRANSFER_FUNCTION_SIZE; i++) {
params.transfer_fn[i] = (byte)floor(i *
(255.0 / (MASK_TRANSFER_FUNCTION_SIZE - 1)) + 0.5);
}
} else {
read_value(data, params.transfer_fn);
}
break;
case PDF14_END_TRANS_MASK:
break;
case PDF14_SET_BLEND_PARAMS:
params.changed = *data++;
if (params.changed & PDF14_SET_BLEND_MODE)
params.blend_mode = *data++;
if (params.changed & PDF14_SET_TEXT_KNOCKOUT)
params.text_knockout = *data++;
if (params.changed & PDF14_SET_OPACITY_ALPHA)
read_value(data, params.opacity.alpha);
if (params.changed & PDF14_SET_SHAPE_ALPHA)
read_value(data, params.shape.alpha);
break;
}
code = gs_create_pdf14trans(ppct, &params, mem);
if (code < 0)
return code;
used = data - start;
if_debug1('v', "  used = %d\n", used);
return used;
}
private	int
c_pdf14trans_create_default_compositor(const gs_composite_t * pct,
gx_device ** pp14dev, gx_device * tdev, gs_imager_state * pis,
gs_memory_t * mem)
{
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pct;
gx_device * p14dev = NULL;
int code = 0;
switch (pdf14pct->params.pdf14_op) {
case PDF14_PUSH_DEVICE:
code = gs_pdf14_device_push(mem, pis, &p14dev, tdev);
*pp14dev = p14dev;
break;
default:
*pp14dev = tdev;
break;
}
return code;
}
private	composite_clist_write_update(c_pdf14trans_clist_write_update);
private	composite_clist_read_update(c_pdf14trans_clist_read_update);
const gs_composite_type_t   gs_composite_pdf14trans_type = {
GX_COMPOSITOR_PDF14_TRANS,
{
c_pdf14trans_create_default_compositor,
c_pdf14trans_equal,
c_pdf14trans_write,
c_pdf14trans_read,
c_pdf14trans_clist_write_update,
c_pdf14trans_clist_read_update
}
};
const gs_composite_type_t   gs_composite_pdf14trans_no_clist_writer_type = {
GX_COMPOSITOR_PDF14_TRANS,
{
c_pdf14trans_create_default_compositor,
c_pdf14trans_equal,
c_pdf14trans_write,
c_pdf14trans_read,
gx_default_composite_clist_write_update,
c_pdf14trans_clist_read_update
}
};
int
gs_is_pdf14trans_compositor(const gs_composite_t * pct)
{
return (pct->type == &gs_composite_pdf14trans_type
|| pct->type == &gs_composite_pdf14trans_no_clist_writer_type);
}
int
gs_create_pdf14trans(
gs_composite_t **               ppct,
const gs_pdf14trans_params_t *  pparams,
gs_memory_t *                   mem )
{
gs_pdf14trans_t *                pct;
rc_alloc_struct_0( pct,
gs_pdf14trans_t,
&st_pdf14trans,
mem,
return_error(gs_error_VMerror),
"gs_create_pdf14trans" );
pct->type = &gs_composite_pdf14trans_type;
pct->id = gs_next_ids(mem, 1);
pct->params = *pparams;
*ppct = (gs_composite_t *)pct;
return 0;
}
int
send_pdf14trans(gs_imager_state	* pis, gx_device * dev,
gx_device * * pcdev, gs_pdf14trans_params_t * pparams, gs_memory_t * mem)
{
gs_composite_t * pct = NULL;
int code;
code = gs_create_pdf14trans(&pct, pparams, mem);
if (code < 0)
return code;
code = dev_proc(dev, create_compositor) (dev, pcdev, pct, pis, mem);
gs_free_object(pis->memory, pct, "send_pdf14trans");
return code;
}
typedef	struct pdf14_clist_device_s {
gx_device_forward_common;
const gx_color_map_procs *(*save_get_cmap_procs)(const gs_imager_state *,
const gx_device *);
gx_device_color_info saved_target_color_info;
float opacity;
float shape;
gs_blend_mode_t blend_mode;
bool text_knockout;
} pdf14_clist_device;
gs_private_st_suffix_add0_final(st_pdf14_clist_device,
pdf14_clist_device, "pdf14_clist_device",
device_c_pdf14_clist_enum_ptrs, device_c_pdf14_clist_reloc_ptrs,
gx_device_finalize, st_device_forward);
#define	pdf14_clist_procs(get_color_mapping_procs, get_color_comp_index,\
encode_color, decode_color) \
{\
NULL,				\
gx_forward_get_initial_matrix,	\
gx_forward_sync_output,		\
gx_forward_output_page,		\
gx_forward_close_device,	\
encode_color,			\
decode_color,			\
gx_forward_fill_rectangle,	\
gx_forward_tile_rectangle,	\
gx_forward_copy_mono,		\
gx_forward_copy_color,		\
NULL		,		\
gx_forward_get_bits,		\
gx_forward_get_params,		\
pdf14_put_params,		\
encode_color,			\
gx_forward_get_xfont_procs,	\
gx_forward_get_xfont_device,	\
NULL,				\
gx_forward_get_page_device,	\
gx_forward_get_alpha_bits,	\
NULL,				\
gx_forward_get_band,		\
gx_forward_copy_rop,		\
pdf14_clist_fill_path,		\
pdf14_clist_stroke_path,		\
gx_forward_fill_mask,		\
gx_forward_fill_trapezoid,	\
gx_forward_fill_parallelogram,	\
gx_forward_fill_triangle,	\
gx_forward_draw_thin_line,	\
pdf14_clist_begin_image,	\
gx_forward_image_data,		\
gx_forward_end_image,		\
gx_forward_strip_tile_rectangle, \
gx_forward_strip_copy_rop,	\
gx_forward_get_clipping_box,	\
pdf14_clist_begin_typed_image,	\
gx_forward_get_bits_rectangle,	\
NULL,				\
pdf14_clist_create_compositor,	\
gx_forward_get_hardware_params,	\
pdf14_clist_text_begin,		\
NULL,				\
pdf14_begin_transparency_group,\
pdf14_end_transparency_group,\
pdf14_begin_transparency_mask,\
pdf14_end_transparency_mask,\
NULL,				\
get_color_mapping_procs,	\
get_color_comp_index,		\
encode_color,			\
decode_color			\
}
private	dev_proc_create_compositor(pdf14_clist_create_compositor);
private	dev_proc_create_compositor(pdf14_clist_forward_create_compositor);
private	dev_proc_fill_path(pdf14_clist_fill_path);
private	dev_proc_stroke_path(pdf14_clist_stroke_path);
private	dev_proc_text_begin(pdf14_clist_text_begin);
private	dev_proc_begin_image(pdf14_clist_begin_image);
private	dev_proc_begin_typed_image(pdf14_clist_begin_typed_image);
private	const gx_device_procs pdf14_clist_Gray_procs =
pdf14_clist_procs(gx_default_DevGray_get_color_mapping_procs,
gx_default_DevGray_get_color_comp_index,
gx_default_8bit_map_gray_color,
gx_default_8bit_map_color_gray);
private	const gx_device_procs pdf14_clist_RGB_procs =
pdf14_clist_procs(gx_default_DevRGB_get_color_mapping_procs,
gx_default_DevRGB_get_color_comp_index,
gx_default_rgb_map_rgb_color,
gx_default_rgb_map_color_rgb);
private	const gx_device_procs pdf14_clist_CMYK_procs =
pdf14_clist_procs(gx_default_DevCMYK_get_color_mapping_procs,
gx_default_DevCMYK_get_color_comp_index,
cmyk_8bit_map_cmyk_color, cmyk_8bit_map_color_cmyk);
const pdf14_clist_device pdf14_clist_Gray_device = {
std_device_color_stype_body(pdf14_clist_device, &pdf14_clist_Gray_procs,
"pdf14clistgray", &st_pdf14_clist_device,
XSIZE, YSIZE, X_DPI, Y_DPI, 8, 255, 256),
{ 0 }
};
const pdf14_clist_device pdf14_clist_RGB_device	= {
std_device_color_stype_body(pdf14_clist_device, &pdf14_clist_RGB_procs,
"pdf14clistRGB", &st_pdf14_clist_device,
XSIZE, YSIZE, X_DPI, Y_DPI, 24, 255, 256),
{ 0 }
};
const pdf14_clist_device pdf14_clist_CMYK_device = {
std_device_std_color_full_body_type(pdf14_clist_device,
&pdf14_clist_CMYK_procs, "PDF14clistcmyk",
&st_pdf14_clist_device, XSIZE, YSIZE, X_DPI, Y_DPI, 32,
0, 0, 0, 0, 0, 0),
{ 0 }
};
private	int
get_pdf14_clist_device_proto(gx_device * dev,
const pdf14_clist_device ** pdevproto)
{
pdf14_default_colorspace_t dev_cs =
pdf14_determine_default_blend_cs(dev);
switch (dev_cs) {
case DeviceGray:
*pdevproto = &pdf14_clist_Gray_device;
break;
case DeviceRGB:
*pdevproto = &pdf14_clist_RGB_device;
break;
case DeviceCMYK:
*pdevproto = &pdf14_clist_CMYK_device;
break;
default:
return_error(gs_error_rangecheck);
}
return 0;
}
private	int
pdf14_create_clist_device(gs_memory_t *mem, gs_imager_state * pis,
gx_device ** ppdev, gx_device * target)
{
const pdf14_clist_device * dev_proto;
pdf14_clist_device *pdev;
int code;
if_debug0('v', "[v]pdf14_create_clist_device\n");
code = get_pdf14_clist_device_proto(target, &dev_proto);
if (code < 0)
return code;
code = gs_copydevice((gx_device **) &pdev,
(const gx_device *) dev_proto, mem);
if (code < 0)
return code;
check_device_separable((gx_device *)pdev);
gx_device_fill_in_procs((gx_device *)pdev);
gs_pdf14_device_copy_params((gx_device *)pdev, target);
rc_assign(pdev->target, target, "pdf14_create_clist_device");
code = dev_proc((gx_device *) pdev, open_device) ((gx_device *) pdev);
*ppdev = (gx_device *) pdev;
return code;
}
private	int
pdf14_disable_clist_device(gs_memory_t *mem, gs_imager_state * pis,
gx_device * dev)
{
gx_device_forward * pdev = (gx_device_forward *)dev;
gx_device * target = pdev->target;
if_debug0('v', "[v]pdf14_disable_clist_device\n");
dev->color_info = target->color_info;
pdf14_forward_device_procs(dev);
set_dev_proc(dev, create_compositor, pdf14_clist_forward_create_compositor);
return 0;
}
private	int
pdf14_recreate_clist_device(gs_memory_t	*mem, gs_imager_state *	pis,
gx_device * dev)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
gx_device * target = pdev->target;
const pdf14_clist_device * dev_proto;
int code;
if_debug0('v', "[v]pdf14_recreate_clist_device\n");
code = get_pdf14_clist_device_proto(target, &dev_proto);
if (code < 0)
return code;
pdev->color_info = dev_proto->color_info;
pdev->procs = dev_proto->procs;
gx_device_fill_in_procs(dev);
check_device_separable((gx_device *)pdev);
return code;
}
private	int
pdf14_clist_create_compositor(gx_device	* dev, gx_device ** pcdev,
const gs_composite_t * pct, gs_imager_state * pis, gs_memory_t * mem)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
int code;
if (gs_is_pdf14trans_compositor(pct)) {
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pct;
switch (pdf14pct->params.pdf14_op) {
case PDF14_PUSH_DEVICE:
pdev->saved_target_color_info = pdev->target->color_info;
pdev->target->color_info = pdev->color_info;
pdev->save_get_cmap_procs = pis->get_cmap_procs;
pis->get_cmap_procs = pdf14_get_cmap_procs;
gx_set_cmap_procs(pis, dev);
code = pdf14_recreate_clist_device(mem, pis, dev);
pdev->blend_mode = pdev->text_knockout = 0;
pdev->opacity = pdev->shape = 0.0;
if (code < 0)
return code;
{
gs_composite_t pctemp = *pct;
pctemp.type = &gs_composite_pdf14trans_no_clist_writer_type;
code = dev_proc(pdev->target, create_compositor)
(pdev->target, pcdev, &pctemp, pis, mem);
*pcdev = dev;
return code;
}
case PDF14_POP_DEVICE:
pdev->target->color_info = pdev->saved_target_color_info;
pis->get_cmap_procs = pdev->save_get_cmap_procs;
gx_set_cmap_procs(pis, pdev->target);
pdf14_disable_clist_device(mem, pis, dev);
code = cmd_put_color_mapping(
(gx_device_clist_writer *)(pdev->target), pis);
if (code < 0)
return code;
break;
case PDF14_BEGIN_TRANS_GROUP:
pdev->text_knockout = pdf14pct->params.Knockout;
pdev->blend_mode = pdf14pct->params.blend_mode;
pdev->opacity = pdf14pct->params.opacity.alpha;
pdev->shape = pdf14pct->params.shape.alpha;
{
const gs_pdf14trans_params_t * pparams = &((const gs_pdf14trans_t *)pct)->params;
if (pparams->Background_components != 0 &&
pparams->Background_components != pdev->color_info.num_components)
return_error(gs_error_rangecheck);
}
break;
default:
break;
}
}
code = dev_proc(pdev->target, create_compositor)
(pdev->target, pcdev, pct, pis, mem);
if (*pcdev != pdev->target)
rc_assign(pdev->target, *pcdev, "pdf14_clist_create_compositor");
*pcdev = dev;
return code;
}
private	int
pdf14_clist_forward_create_compositor(gx_device	* dev, gx_device * * pcdev,
const gs_composite_t * pct, gs_imager_state * pis,
gs_memory_t * mem)
{
pdf14_device *pdev = (pdf14_device *)dev;
gx_device * tdev = pdev->target;
gx_device * ndev;
int code = 0;
*pcdev = dev;
if (gs_is_pdf14trans_compositor(pct)) {
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pct;
if (pdf14pct->params.pdf14_op == PDF14_PUSH_DEVICE)
return pdf14_clist_create_compositor(dev, &ndev, pct, pis, mem);
return 0;
}
code = dev_proc(tdev, create_compositor)(tdev, &ndev, pct, pis, mem);
if (code < 0)
return code;
pdev->target = ndev;
return 0;
}
private	int
pdf14_clist_update_params(pdf14_clist_device * pdev, const gs_imager_state * pis)
{
gs_pdf14trans_params_t params = { 0 };
gx_device * pcdev;
int changed = 0;
int code = 0;
params.pdf14_op = PDF14_SET_BLEND_PARAMS;
if (pis->blend_mode != pdev->blend_mode) {
changed |= PDF14_SET_BLEND_MODE;
params.blend_mode = pdev->blend_mode = pis->blend_mode;
}
if (pis->text_knockout != pdev->text_knockout) {
changed |= PDF14_SET_TEXT_KNOCKOUT;
params.text_knockout = pdev->text_knockout = pis->text_knockout;
}
if (pis->shape.alpha != pdev->shape) {
changed |= PDF14_SET_SHAPE_ALPHA;
params.shape.alpha = pdev->shape = pis->shape.alpha;
}
if (pis->opacity.alpha != pdev->opacity) {
changed |= PDF14_SET_OPACITY_ALPHA;
params.opacity.alpha = pdev->opacity = pis->opacity.alpha;
}
if (changed != 0) {
params.changed = changed;
code = send_pdf14trans((gs_imager_state *)pis, (gx_device *)pdev,
&pcdev, &params, pis->memory);
}
return code;
}
private	int
pdf14_clist_fill_path(gx_device	*dev, const gs_imager_state *pis,
gx_path *ppath, const gx_fill_params *params,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
gs_imager_state new_is = *pis;
int code;
code = pdf14_clist_update_params(pdev, pis);
if (code < 0)
return code;
new_is.log_op |= lop_pdf14;
return gx_default_fill_path(dev, &new_is, ppath, params, pdcolor, pcpath);
}
private	int
pdf14_clist_stroke_path(gx_device *dev,	const gs_imager_state *pis,
gx_path *ppath, const gx_stroke_params *params,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
gs_imager_state new_is = *pis;
int code;
code = pdf14_clist_update_params(pdev, pis);
if (code < 0)
return code;
new_is.log_op |= lop_pdf14;
return gx_default_stroke_path(dev, &new_is, ppath, params, pdcolor, pcpath);
}
private	int
pdf14_clist_text_begin(gx_device * dev,	gs_imager_state	* pis,
const gs_text_params_t * text, gs_font * font,
gx_path * path, const gx_device_color * pdcolor,
const gx_clip_path * pcpath, gs_memory_t * memory,
gs_text_enum_t ** ppenum)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
gs_text_enum_t *penum;
int code;
code = pdf14_clist_update_params(pdev, pis);
if (code < 0)
return code;
code = gx_default_text_begin(dev, pis, text, font, path,
pdcolor, pcpath, memory, &penum);
if (code < 0)
return code;
*ppenum = (gs_text_enum_t *)penum;
return code;
}
private	int
pdf14_clist_begin_image(gx_device * dev,
const gs_imager_state * pis, const gs_image_t * pim,
gs_image_format_t format, const gs_int_rect * prect,
const gx_drawing_color * pdcolor,
const gx_clip_path * pcpath,
gs_memory_t * memory, gx_image_enum_common_t ** pinfo)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
int code;
code = pdf14_clist_update_params(pdev, pis);
if (code < 0)
return code;
return gx_default_begin_image(dev, pis, pim, format, prect,
pdcolor, pcpath, memory, pinfo);
}
private	int
pdf14_clist_begin_typed_image(gx_device	* dev, const gs_imager_state * pis,
const gs_matrix *pmat, const gs_image_common_t *pic,
const gs_int_rect * prect,
const gx_drawing_color * pdcolor,
const gx_clip_path * pcpath, gs_memory_t * mem,
gx_image_enum_common_t ** pinfo)
{
pdf14_clist_device * pdev = (pdf14_clist_device *)dev;
int code;
code = pdf14_clist_update_params(pdev, pis);
if (code < 0)
return code;
return gx_default_begin_typed_image(dev, pis, pmat,
pic, prect, pdcolor, pcpath, mem, pinfo);
}
private	int
c_pdf14trans_clist_write_update(const gs_composite_t * pcte, gx_device * dev,
gx_device ** pcdev, gs_imager_state * pis, gs_memory_t * mem)
{
const gs_pdf14trans_t * pdf14pct = (const gs_pdf14trans_t *) pcte;
pdf14_clist_device * p14dev;
int code = 0;
switch (pdf14pct->params.pdf14_op) {
case PDF14_PUSH_DEVICE:
code = pdf14_create_clist_device(mem, pis, pcdev, dev);
p14dev = (pdf14_clist_device *)(*pcdev);
p14dev->saved_target_color_info = dev->color_info;
dev->color_info = (*pcdev)->color_info;
p14dev->save_get_cmap_procs = pis->get_cmap_procs;
pis->get_cmap_procs = pdf14_get_cmap_procs;
gx_set_cmap_procs(pis, dev);
return code;
case PDF14_POP_DEVICE:
code = cmd_put_halftone((gx_device_clist_writer *)
(((pdf14_clist_device *)dev)->target), pis->dev_ht);
break;
default:
break;
}
*pcdev = dev;
return code;
}
private	int
c_pdf14trans_clist_read_update(gs_composite_t *	pcte, gx_device	* cdev,
gx_device * tdev, gs_imager_state * pis, gs_memory_t * mem)
{
pdf14_device * p14dev = (pdf14_device *)tdev;
gs_pdf14trans_t * pdf14pct = (gs_pdf14trans_t *) pcte;
switch (pdf14pct->params.pdf14_op) {
case PDF14_PUSH_DEVICE:
p14dev->saved_clist_color_info = cdev->color_info;
cdev->color_info = p14dev->color_info;
break;
case PDF14_POP_DEVICE:
cdev->color_info = p14dev->saved_clist_color_info;
break;
default:
break;
}
return 0;
}