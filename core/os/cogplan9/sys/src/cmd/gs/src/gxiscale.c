#include "gx.h"
#include "math_.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxfrac.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gsccolor.h"
#include "gspaint.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gxdcolor.h"
#include "gxistate.h"
#include "gxdevmem.h"
#include "gxcpath.h"
#include "gximage.h"
#include "stream.h"
#include "siinterp.h"
#include "siscale.h"
#define USE_MITCHELL_FILTER
iclass_proc(gs_image_class_0_interpolate);
private irender_proc(image_render_interpolate);
irender_proc_t
gs_image_class_0_interpolate(gx_image_enum * penum)
{
const gs_imager_state *pis = penum->pis;
gs_memory_t *mem = penum->memory;
stream_image_scale_params_t iss;
stream_image_scale_state *pss;
const stream_template *template;
byte *line;
const gs_color_space *pcs = penum->pcs;
const gs_color_space *pccs;
gs_point dst_xy;
uint in_size;
if (!penum->interpolate)
return 0;
if (penum->use_mask_color || penum->posture != image_portrait ||
penum->masked || penum->alpha ||
(penum->dev->color_info.num_components == 1 &&
penum->dev->color_info.max_gray < 15) ||
(penum->dev->color_info.num_components > 1 &&
penum->dev->color_info.max_color < 15)
) {
penum->interpolate = false;
return 0;
}
#if defined(USE_MITCHELL_FILTER) && defined(USE_CONSERVATIVE_INTERPOLATION_RULES)
if (penum->bps < 4 || penum->bps * penum->spp < 8 ||
(fabs(penum->matrix.xx) <= 5 && fabs(penum->matrix.yy <= 5))
) {
penum->interpolate = false;
return 0;
}
#endif
gs_distance_transform((float)penum->rect.w, (float)penum->rect.h,
&penum->matrix, &dst_xy);
iss.BitsPerComponentOut = sizeof(frac) * 8;
iss.MaxValueOut = frac_1;
iss.WidthOut = (int)ceil(fabs(dst_xy.x));
iss.HeightOut = (int)ceil(fabs(dst_xy.y));
iss.WidthIn = penum->rect.w;
iss.HeightIn = penum->rect.h;
pccs = cs_concrete_space(pcs, pis);
iss.Colors = cs_num_components(pccs);
if (penum->bps <= 8 && penum->device_color) {
iss.BitsPerComponentIn = 8;
iss.MaxValueIn = 0xff;
in_size =
(penum->matrix.xx < 0 ?
iss.WidthIn * iss.Colors : 0);
} else {
iss.BitsPerComponentIn = sizeof(frac) * 8;
iss.MaxValueIn = frac_1;
in_size = round_up(iss.WidthIn * iss.Colors * sizeof(frac),
align_bitmap_mod);
}
{
uint out_size =
iss.WidthOut * max(iss.Colors * (iss.BitsPerComponentOut / 8),
arch_sizeof_color_index);
line = gs_alloc_bytes(mem, in_size + out_size,
"image scale src+dst line");
}
#ifdef USE_MITCHELL_FILTER
template = &s_IScale_template;
#else
template = &s_IIEncode_template;
#endif
pss = (stream_image_scale_state *)
s_alloc_state(mem, template->stype, "image scale state");
if (line == 0 || pss == 0 ||
(pss->params = iss, pss->template = template,
(*pss->template->init) ((stream_state *) pss) < 0)
) {
gs_free_object(mem, pss, "image scale state");
gs_free_object(mem, line, "image scale src+dst line");
penum->interpolate = false;
return 0;
}
penum->line = line;
penum->scaler = pss;
penum->line_xy = 0;
{
gx_dda_fixed x0;
x0 = penum->dda.pixel0.x;
if (penum->matrix.xx < 0)
dda_advance(x0, penum->rect.w);
penum->xyi.x = fixed2int_pixround(dda_current(x0));
}
penum->xyi.y = fixed2int_pixround(dda_current(penum->dda.pixel0.y));
if_debug0('b', "[b]render=interpolate\n");
return &image_render_interpolate;
}
private int
image_render_interpolate(gx_image_enum * penum, const byte * buffer,
int data_x, uint iw, int h, gx_device * dev)
{
stream_image_scale_state *pss = penum->scaler;
const gs_imager_state *pis = penum->pis;
const gs_color_space *pcs = penum->pcs;
gs_logical_operation_t lop = penum->log_op;
int c = pss->params.Colors;
stream_cursor_read r;
stream_cursor_write w;
byte *out = penum->line;
if (h != 0) {
int sizeofPixelIn = pss->params.BitsPerComponentIn / 8;
uint row_size = pss->params.WidthIn * c * sizeofPixelIn;
const byte *bdata = buffer + data_x * c * sizeofPixelIn;
if (sizeofPixelIn == 1) {
if (penum->matrix.xx >= 0) {
r.ptr = bdata - 1;
} else {
const byte *p = bdata + row_size - c;
byte *q = out;
int i;
for (i = 0; i < pss->params.WidthIn; p -= c, q += c, ++i)
memcpy(q, p, c);
r.ptr = out - 1;
out = q;
}
} else {
int bps = penum->bps;
int dc = penum->spp;
const byte *pdata = bdata;
frac *psrc = (frac *) penum->line;
gs_client_color cc;
int i;
r.ptr = (byte *) psrc - 1;
if_debug0('B', "[B]Concrete row:\n[B]");
for (i = 0; i < pss->params.WidthIn; i++, psrc += c) {
int j;
if (bps <= 8)
for (j = 0; j < dc; ++pdata, ++j) {
decode_sample(*pdata, cc, j);
} else
for (j = 0; j < dc; pdata += sizeof(frac), ++j) {
decode_frac(*(const frac *)pdata, cc, j);
}
(*pcs->type->concretize_color) (&cc, pcs, psrc, pis);
#ifdef DEBUG
if (gs_debug_c('B')) {
int ci;
for (ci = 0; ci < c; ++ci)
dprintf2("%c%04x", (ci == 0 ? ' ' : ','), psrc[ci]);
}
#endif
}
out += round_up(pss->params.WidthIn * c * sizeof(frac),
align_bitmap_mod);
if_debug0('B', "\n");
}
r.limit = r.ptr + row_size;
} else
r.ptr = 0, r.limit = 0;
{
int xo = penum->xyi.x;
int yo = penum->xyi.y;
int width = pss->params.WidthOut;
int sizeofPixelOut = pss->params.BitsPerComponentOut / 8;
int dy;
const gs_color_space *pconcs = cs_concrete_space(pcs, pis);
int bpp = dev->color_info.depth;
uint raster = bitmap_raster(width * bpp);
if (penum->matrix.yy > 0)
dy = 1;
else
dy = -1, yo--;
for (;;) {
int ry = yo + penum->line_xy * dy;
int x;
const frac *psrc;
gx_device_color devc;
int status, code;
DECLARE_LINE_ACCUM_COPY(out, bpp, xo);
w.limit = out + width *
max(c * sizeofPixelOut, arch_sizeof_color_index) - 1;
w.ptr = w.limit - width * c * sizeofPixelOut;
psrc = (const frac *)(w.ptr + 1);
status = (*pss->template->process)
((stream_state *) pss, &r, &w, h == 0);
if (status < 0 && status != EOFC)
return_error(gs_error_ioerror);
if (w.ptr == w.limit) {
int xe = xo + width;
if_debug1('B', "[B]Interpolated row %d:\n[B]",
penum->line_xy);
for (x = xo; x < xe;) {
#ifdef DEBUG
if (gs_debug_c('B')) {
int ci;
for (ci = 0; ci < c; ++ci)
dprintf2("%c%04x", (ci == 0 ? ' ' : ','),
psrc[ci]);
}
#endif
code = (*pconcs->type->remap_concrete_color)
(psrc, pcs, &devc, pis, dev, gs_color_select_source);
if (code < 0)
return code;
if (color_is_pure(&devc)) {
gx_color_index color = devc.colors.pure;
if (c == 3) {
do {
LINE_ACCUM(color, bpp);
x++, psrc += 3;
} while (x < xe && psrc[-3] == psrc[0] &&
psrc[-2] == psrc[1] &&
psrc[-1] == psrc[2]);
} else {
LINE_ACCUM(color, bpp);
x++, psrc += c;
}
} else {
int rcode;
LINE_ACCUM_COPY(dev, out, bpp, xo, x, raster, ry);
rcode = gx_fill_rectangle_device_rop(x, ry,
1, 1, &devc, dev, lop);
if (rcode < 0)
return rcode;
LINE_ACCUM_SKIP(bpp);
l_xprev = x + 1;
x++, psrc += c;
}
}
LINE_ACCUM_COPY(dev, out, bpp, xo, x, raster, ry);
penum->line_xy++;
if_debug0('B', "\n");
}
if ((status == 0 && r.ptr == r.limit) || status == EOFC)
break;
}
}
return (h == 0 ? 0 : 1);
}