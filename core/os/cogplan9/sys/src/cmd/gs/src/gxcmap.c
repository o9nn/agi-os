#include "gx.h"
#include "gserrors.h"
#include "gsccolor.h"
#include "gxalpha.h"
#include "gxcspace.h"
#include "gxfarith.h"
#include "gxfrac.h"
#include "gxdcconv.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gxlum.h"
#include "gzstate.h"
#include "gxdither.h"
#include "gxcdevn.h"
#include "string_.h"
public_st_device_color();
private
ENUM_PTRS_WITH(device_color_enum_ptrs, gx_device_color *cptr)
{
return ENUM_USING(*cptr->type->stype, vptr, size, index);
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(device_color_reloc_ptrs, gx_device_color *cptr)
{
RELOC_USING(*cptr->type->stype, vptr, size);
}
RELOC_PTRS_END
gx_color_index
gx_default_encode_color(gx_device * dev, const gx_color_value cv[])
{
int             ncomps = dev->color_info.num_components;
int             i;
const byte *    comp_shift = dev->color_info.comp_shift;
const byte *    comp_bits = dev->color_info.comp_bits;
gx_color_index  color = 0;
#ifdef DEBUG
if ( dev->color_info.separable_and_linear != GX_CINFO_SEP_LIN ) {
dprintf( "gx_default_encode_color() requires separable and linear\n" );
return gx_no_color_index;
}
#endif
for (i = 0; i < ncomps; i++) {
color |= (gx_color_index)(cv[i] >> (gx_color_value_bits - comp_bits[i]))
<< comp_shift[i];
}
return color;
}
int
gx_default_decode_color(gx_device * dev, gx_color_index color, gx_color_value cv[])
{
int                     ncomps = dev->color_info.num_components;
int                     i;
const byte *            comp_shift = dev->color_info.comp_shift;
const byte *            comp_bits = dev->color_info.comp_bits;
const gx_color_index *  comp_mask = dev->color_info.comp_mask;
uint shift, ivalue, nbits, scale;
#ifdef DEBUG
if ( dev->color_info.separable_and_linear != GX_CINFO_SEP_LIN ) {
dprintf( "gx_default_decode_color() requires separable and linear\n" );
return gs_error_rangecheck;
}
#endif
for (i = 0; i < ncomps; i++) {
nbits = comp_bits[i];
scale = gx_max_color_value / ((1 << nbits) - 1);
ivalue = (color & comp_mask[i]) >> comp_shift[i];
cv[i] = ivalue * scale;
shift = nbits - (gx_color_value_bits % nbits);
cv[i] += ivalue >> shift;
}
return 0;
}
gx_color_index
gx_error_encode_color(gx_device * dev, const gx_color_value colors[])
{
#ifdef DEBUG
if (strcmp(dev->dname, "null") != 0)
dprintf("No encode_color proc defined for device.\n");
#endif
return gx_no_color_index;
}
int
gx_error_decode_color(gx_device * dev, gx_color_index cindex, gx_color_value colors[])
{
int i=dev->color_info.num_components;
#ifdef DEBUG
dprintf("No decode_color proc defined for device.\n");
#endif
for(; i>=0; i--)
colors[i] = 0;
return gs_error_rangecheck;
}
gx_color_index
gx_default_gray_fast_encode(gx_device * dev, const gx_color_value cv[])
{
return cv[0] >> (gx_color_value_bits - dev->color_info.depth);
}
gx_color_index
gx_default_gray_encode(gx_device * dev, const gx_color_value cv[])
{
return cv[0] * (dev->color_info.max_gray + 1) / (gx_max_color_value + 1);
}
gx_color_index
gx_backwards_compatible_gray_encode(gx_device *dev,
const gx_color_value cv[])
{
gx_color_value gray_val = cv[0];
gx_color_value rgb_cv[3];
rgb_cv[0] = gray_val;
rgb_cv[1] = gray_val;
rgb_cv[2] = gray_val;
return (*dev_proc(dev, map_rgb_color))(dev, rgb_cv);
}
void
gray_cs_to_gray_cm(gx_device * dev, frac gray, frac out[])
{
out[0] = gray;
}
static void
rgb_cs_to_gray_cm(gx_device * dev, const gs_imager_state *pis,
frac r, frac g, frac b, frac out[])
{
out[0] = color_rgb_to_gray(r, g, b, NULL);
}
static void
cmyk_cs_to_gray_cm(gx_device * dev, frac c, frac m, frac y, frac k, frac out[])
{
out[0] = color_cmyk_to_gray(c, m, y, k, NULL);
}
static void
gray_cs_to_rgb_cm(gx_device * dev, frac gray, frac out[])
{
out[0] = out[1] = out[2] = gray;
}
void
rgb_cs_to_rgb_cm(gx_device * dev, const gs_imager_state *pis,
frac r, frac g, frac b, frac out[])
{
out[0] = r;
out[1] = g;
out[2] = b;
}
static void
cmyk_cs_to_rgb_cm(gx_device * dev, frac c, frac m, frac y, frac k, frac out[])
{
color_cmyk_to_rgb(c, m, y, k, NULL, out);
}
static void
gray_cs_to_rgbk_cm(gx_device * dev, frac gray, frac out[])
{
out[0] = out[1] = out[2] = frac_0;
out[3] = gray;
}
static void
rgb_cs_to_rgbk_cm(gx_device * dev, const gs_imager_state *pis,
frac r, frac g, frac b, frac out[])
{
if ((r == g) && (g == b)) {
out[0] = out[1] = out[2] = frac_0;
out[3] = r;
}
else {
out[0] = r;
out[1] = g;
out[2] = b;
out[3] = frac_0;
}
}
static void
cmyk_cs_to_rgbk_cm(gx_device * dev, frac c, frac m, frac y, frac k, frac out[])
{
frac rgb[3];
if ((c == frac_0) && (m == frac_0) && (y == frac_0)) {
out[0] = out[1] = out[2] = frac_0;
out[3] = frac_1 - k;
}
else {
color_cmyk_to_rgb(c, m, y, k, NULL, rgb);
rgb_cs_to_rgbk_cm(dev, NULL, rgb[0], rgb[1], rgb[2], out);
}
}
static void
gray_cs_to_cmyk_cm(gx_device * dev, frac gray, frac out[])
{
out[0] = out[1] = out[2] = frac_0;
out[3] = frac_1 - gray;
}
static void
rgb_cs_to_cmyk_cm(gx_device * dev, const gs_imager_state *pis,
frac r, frac g, frac b, frac out[])
{
if (pis != 0)
color_rgb_to_cmyk(r, g, b, pis, out);
else {
frac    c = frac_1 - r, m = frac_1 - g, y = frac_1 - b;
frac    k = min(c, min(m, g));
out[0] = c - k;
out[1] = m - k;
out[2] = y - k;
out[3] = k;
}
}
void
cmyk_cs_to_cmyk_cm(gx_device * dev, frac c, frac m, frac y, frac k, frac out[])
{
out[0] = c;
out[1] = m;
out[2] = y;
out[3] = k;
}
static const gx_cm_color_map_procs DeviceGray_procs = {
gray_cs_to_gray_cm, rgb_cs_to_gray_cm, cmyk_cs_to_gray_cm
};
static const gx_cm_color_map_procs DeviceRGB_procs = {
gray_cs_to_rgb_cm, rgb_cs_to_rgb_cm, cmyk_cs_to_rgb_cm
};
static const gx_cm_color_map_procs DeviceCMYK_procs = {
gray_cs_to_cmyk_cm, rgb_cs_to_cmyk_cm, cmyk_cs_to_cmyk_cm
};
static const gx_cm_color_map_procs DeviceRGBK_procs = {
gray_cs_to_rgbk_cm, rgb_cs_to_rgbk_cm, cmyk_cs_to_rgbk_cm
};
const gx_cm_color_map_procs *
gx_default_DevGray_get_color_mapping_procs(const gx_device * dev)
{
return &DeviceGray_procs;
}
const gx_cm_color_map_procs *
gx_default_DevRGB_get_color_mapping_procs(const gx_device * dev)
{
return &DeviceRGB_procs;
}
const gx_cm_color_map_procs *
gx_default_DevCMYK_get_color_mapping_procs(const gx_device * dev)
{
return &DeviceCMYK_procs;
}
const gx_cm_color_map_procs *
gx_default_DevRGBK_get_color_mapping_procs(const gx_device * dev)
{
return &DeviceRGBK_procs;
}
const gx_cm_color_map_procs *
gx_error_get_color_mapping_procs(const gx_device * dev)
{
#ifdef DEBUG
dprintf("No get_color_mapping_procs proc defined for device.\n");
#endif
return NULL;
}
#define compare_color_names(pname, name_size, name_str) \
(name_size == (int)strlen(name_str) && strncmp(pname, name_str, name_size) == 0)
int
gx_default_DevGray_get_color_comp_index(gx_device * dev, const char * pname,
int name_size, int component_type)
{
if (compare_color_names(pname, name_size, "Gray") ||
compare_color_names(pname, name_size, "Grey"))
return 0;
else
return -1;
}
int
gx_default_DevRGB_get_color_comp_index(gx_device * dev, const char * pname,
int name_size, int component_type)
{
if (compare_color_names(pname, name_size, "Red"))
return 0;
if (compare_color_names(pname, name_size, "Green"))
return 1;
if (compare_color_names(pname, name_size, "Blue"))
return 2;
else
return -1;
}
int
gx_default_DevCMYK_get_color_comp_index(gx_device * dev, const char * pname,
int name_size, int component_type)
{
if (compare_color_names(pname, name_size, "Cyan"))
return 0;
if (compare_color_names(pname, name_size, "Magenta"))
return 1;
if (compare_color_names(pname, name_size, "Yellow"))
return 2;
if (compare_color_names(pname, name_size, "Black"))
return 3;
else
return -1;
}
int
gx_default_DevRGBK_get_color_comp_index(gx_device * dev, const char * pname,
int name_size, int component_type)
{
if (compare_color_names(pname, name_size, "Red"))
return 0;
if (compare_color_names(pname, name_size, "Green"))
return 1;
if (compare_color_names(pname, name_size, "Blue"))
return 2;
if (compare_color_names(pname, name_size, "Black"))
return 3;
else
return -1;
}
int
gx_error_get_color_comp_index(gx_device * dev, const char * pname,
int name_size, int component_type)
{
#ifdef DEBUG
dprintf("No get_color_comp_index proc defined for device.\n");
#endif
return -1;
}
#undef compare_color_names
private cmap_proc_gray(cmap_gray_halftoned);
private cmap_proc_gray(cmap_gray_direct);
private cmap_proc_rgb(cmap_rgb_halftoned);
private cmap_proc_rgb(cmap_rgb_direct);
#define cmap_cmyk_halftoned cmap_cmyk_direct
private cmap_proc_cmyk(cmap_cmyk_direct);
private cmap_proc_rgb_alpha(cmap_rgb_alpha_halftoned);
private cmap_proc_rgb_alpha(cmap_rgb_alpha_direct);
private cmap_proc_rgb_alpha(cmap_rgb_alpha_halftoned);
private cmap_proc_rgb_alpha(cmap_rgb_alpha_direct);
private cmap_proc_separation(cmap_separation_halftoned);
private cmap_proc_separation(cmap_separation_direct);
private cmap_proc_devicen(cmap_devicen_halftoned);
private cmap_proc_devicen(cmap_devicen_direct);
private cmap_proc_is_halftoned(cmap_halftoned_is_halftoned);
private cmap_proc_is_halftoned(cmap_direct_is_halftoned);
private const gx_color_map_procs cmap_few = {
cmap_gray_halftoned,
cmap_rgb_halftoned,
cmap_cmyk_halftoned,
cmap_rgb_alpha_halftoned,
cmap_separation_halftoned,
cmap_devicen_halftoned,
cmap_halftoned_is_halftoned
};
private const gx_color_map_procs cmap_many = {
cmap_gray_direct,
cmap_rgb_direct,
cmap_cmyk_direct,
cmap_rgb_alpha_direct,
cmap_separation_direct,
cmap_devicen_direct,
cmap_direct_is_halftoned
};
const gx_color_map_procs *const cmap_procs_default = &cmap_many;
const gx_color_map_procs *
gx_get_cmap_procs(const gs_imager_state *pis, const gx_device * dev)
{
return (pis->get_cmap_procs)(pis, dev);
}
const gx_color_map_procs *
gx_default_get_cmap_procs(const gs_imager_state *pis, const gx_device * dev)
{
return (gx_device_must_halftone(dev) ? &cmap_few : &cmap_many);
}
void
gx_set_cmap_procs(gs_imager_state * pis, const gx_device * dev)
{
pis->cmap_procs = gx_get_cmap_procs(pis, dev);
}
int
gx_remap_color(gs_state * pgs)
{
const gs_color_space *pcs = pgs->color_space;
int                   code;
code = (*pcs->type->remap_color) (pgs->ccolor, pcs, pgs->dev_color,
(gs_imager_state *) pgs, pgs->device,
gs_color_select_texture);
if (code >= 0 && pgs->effective_overprint_mode == 1)
code = gs_do_set_overprint(pgs);
return code;
}
const gs_color_space *
gx_no_concrete_space(const gs_color_space * pcs, const gs_imager_state * pis)
{
return NULL;
}
const gs_color_space *
gx_same_concrete_space(const gs_color_space * pcs, const gs_imager_state * pis)
{
return pcs;
}
int
gx_no_concretize_color(const gs_client_color * pcc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
return_error(gs_error_rangecheck);
}
int
gx_default_remap_color(const gs_client_color * pcc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
frac conc[GS_CLIENT_COLOR_MAX_COMPONENTS];
const gs_color_space *pconcs;
int i = pcs->type->num_components(pcs);
int code = (*pcs->type->concretize_color)(pcc, pcs, conc, pis);
if (code < 0)
return code;
pconcs = cs_concrete_space(pcs, pis);
code = (*pconcs->type->remap_concrete_color)(conc, pconcs, pdc, pis, dev, select);
i = any_abs(i);
for (i--; i >= 0; i--)
pdc->ccolor.paint.values[i] = pcc->paint.values[i];
pdc->ccolor_valid = true;
return code;
}
int
gx_concretize_DeviceGray(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
float ftemp;
pconc[0] = unit_frac(pc->paint.values[0], ftemp);
return 0;
}
int
gx_remap_concrete_DGray(const frac * pconc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
if (pis->alpha == gx_max_color_value)
(*pis->cmap_procs->map_gray)
(pconc[0], pdc, pis, dev, select);
else
(*pis->cmap_procs->map_rgb_alpha)
(pconc[0], pconc[0], pconc[0], cv2frac(pis->alpha),
pdc, pis, dev, select);
return 0;
}
int
gx_remap_DeviceGray(const gs_client_color * pc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
float ftemp;
frac fgray = unit_frac(pc->paint.values[0], ftemp);
pdc->ccolor.paint.values[0] = pc->paint.values[0];
pdc->ccolor_valid = true;
if (pis->alpha == gx_max_color_value)
(*pis->cmap_procs->map_gray)
(fgray, pdc, pis, dev, select);
else
(*pis->cmap_procs->map_rgb_alpha)
(fgray, fgray, fgray, cv2frac(pis->alpha), pdc, pis, dev, select);
return 0;
}
int
gx_concretize_DeviceRGB(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
float ftemp;
pconc[0] = unit_frac(pc->paint.values[0], ftemp);
pconc[1] = unit_frac(pc->paint.values[1], ftemp);
pconc[2] = unit_frac(pc->paint.values[2], ftemp);
return 0;
}
int
gx_remap_concrete_DRGB(const frac * pconc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
if (pis->alpha == gx_max_color_value)
gx_remap_concrete_rgb(pconc[0], pconc[1], pconc[2],
pdc, pis, dev, select);
else
gx_remap_concrete_rgb_alpha(pconc[0], pconc[1], pconc[2],
cv2frac(pis->alpha),
pdc, pis, dev, select);
return 0;
}
int
gx_remap_DeviceRGB(const gs_client_color * pc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
float ftemp;
frac fred = unit_frac(pc->paint.values[0], ftemp), fgreen = unit_frac(pc->paint.values[1], ftemp),
fblue = unit_frac(pc->paint.values[2], ftemp);
pdc->ccolor.paint.values[0] = pc->paint.values[0];
pdc->ccolor.paint.values[1] = pc->paint.values[1];
pdc->ccolor.paint.values[2] = pc->paint.values[2];
pdc->ccolor_valid = true;
if (pis->alpha == gx_max_color_value)
gx_remap_concrete_rgb(fred, fgreen, fblue,
pdc, pis, dev, select);
else
gx_remap_concrete_rgb_alpha(fred, fgreen, fblue, cv2frac(pis->alpha),
pdc, pis, dev, select);
return 0;
}
int
gx_concretize_DeviceCMYK(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
float ftemp;
pconc[0] = unit_frac(pc->paint.values[0], ftemp);
pconc[1] = unit_frac(pc->paint.values[1], ftemp);
pconc[2] = unit_frac(pc->paint.values[2], ftemp);
pconc[3] = unit_frac(pc->paint.values[3], ftemp);
return 0;
}
int
gx_remap_concrete_DCMYK(const frac * pconc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
gx_remap_concrete_cmyk(pconc[0], pconc[1], pconc[2], pconc[3], pdc,
pis, dev, select);
return 0;
}
int
gx_remap_DeviceCMYK(const gs_client_color * pc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
float ft0, ft1, ft2, ft3;
pdc->ccolor.paint.values[0] = pc->paint.values[0];
pdc->ccolor.paint.values[1] = pc->paint.values[1];
pdc->ccolor.paint.values[2] = pc->paint.values[2];
pdc->ccolor.paint.values[3] = pc->paint.values[3];
pdc->ccolor_valid = true;
gx_remap_concrete_cmyk((frac)unit_frac(pc->paint.values[0], ft0),
(frac)unit_frac(pc->paint.values[1], ft1),
(frac)unit_frac(pc->paint.values[2], ft2),
(frac)unit_frac(pc->paint.values[3], ft3),
pdc, pis, dev, select);
return 0;
}
private void
cmap_gray_halftoned(frac gray, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
dev_proc(dev, get_color_mapping_procs)(dev)->map_gray(dev, gray, cm_comps);
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_render_device_DeviceN(cm_comps, pdc, dev, pis->dev_ht,
&pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
}
private void
cmap_gray_direct(frac gray, gx_device_color * pdc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_gray(dev, gray, cm_comps);
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
else
cmap_gray_halftoned(gray, pdc, pis, dev, select);
}
private void
cmap_rgb_halftoned(frac r, frac g, frac b, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
dev_proc(dev, get_color_mapping_procs)(dev)->map_rgb(dev, pis, r, g, b, cm_comps);
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_render_device_DeviceN(cm_comps, pdc, dev, pis->dev_ht,
&pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
}
private void
cmap_rgb_direct(frac r, frac g, frac b, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_rgb(dev, pis, r, g, b, cm_comps);
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
else
cmap_rgb_halftoned(r, g, b, pdc, pis, dev, select);
}
private void
cmap_cmyk_direct(frac c, frac m, frac y, frac k, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index color;
dev_proc(dev, get_color_mapping_procs)(dev)->map_cmyk(dev, c, m, y, k, cm_comps);
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_device_must_halftone(dev)) {
if (gx_render_device_DeviceN(cm_comps, pdc, dev,
pis->dev_ht, &pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
return;
}
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(cm_comps[i]);
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
else {
if (gx_render_device_DeviceN(cm_comps, pdc, dev,
pis->dev_ht, &pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
return;
}
}
private void
cmap_rgb_alpha_halftoned(frac r, frac g, frac b, frac alpha,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
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
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_render_device_DeviceN(cm_comps, pdc, dev, pis->dev_ht,
&pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
}
private void
cmap_rgb_alpha_direct(frac r, frac g, frac b, frac alpha, gx_device_color * pdc,
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
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]));
else
for (i = 0; i < ncomps; i++)
cv[i] = frac2cv(frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]));
if (dev_proc(dev, map_rgb_alpha_color) != gx_default_map_rgb_alpha_color &&
(cv_alpha = frac2cv(alpha)) != gx_max_color_value)
color = dev_proc(dev, map_rgb_alpha_color)(dev, cv[0], cv[1], cv[2], cv_alpha);
else
color = dev_proc(dev, encode_color)(dev, cv);
if (color != gx_no_color_index)
color_set_pure(pdc, color);
else
cmap_rgb_alpha_halftoned(r, g, b, alpha, pdc, pis, dev, select);
}
private inline void
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
private void
cmap_separation_halftoned(frac all, gx_device_color * pdc,
const gs_imager_state * pis, gx_device * dev, gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
bool additive = dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE;
frac comp_value = all;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
if (pis->color_component_map.sep_type == SEP_ALL) {
if (additive)
comp_value = frac_1 - comp_value;
i = pis->color_component_map.num_colorants - 1;
for (; i >= 0; i--)
cm_comps[i] = comp_value;
}
else {
map_components_to_colorants(&all, &(pis->color_component_map), cm_comps);
}
if (additive)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_render_device_DeviceN(cm_comps, pdc, dev, pis->dev_ht,
&pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
}
private void
cmap_separation_direct(frac all, gx_device_color * pdc, const gs_imager_state * pis,
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
else
cmap_separation_halftoned(all, pdc, pis, dev, select);
}
private void
cmap_devicen_halftoned(const frac * pcc,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
int i, ncomps = dev->color_info.num_components;
frac cm_comps[GX_DEVICE_COLOR_MAX_COMPONENTS];
map_components_to_colorants(pcc, &(pis->color_component_map), cm_comps);
if (dev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
for (i = 0; i < ncomps; i++)
cm_comps[i] = gx_map_color_frac(pis,
cm_comps[i], effective_transfer[i]);
else
for (i = 0; i < ncomps; i++)
cm_comps[i] = frac_1 - gx_map_color_frac(pis,
(frac)(frac_1 - cm_comps[i]), effective_transfer[i]);
if (gx_render_device_DeviceN(cm_comps, pdc, dev, pis->dev_ht,
&pis->screen_phase[select]) == 1)
gx_color_load_select(pdc, pis, dev, select);
}
private void
cmap_devicen_direct(const frac * pcc,
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
else
cmap_devicen_halftoned(pcc, pdc, pis, dev, select);
}
private bool
cmap_halftoned_is_halftoned(const gs_imager_state * pis, gx_device * dev)
{
return true;
}
private bool
cmap_direct_is_halftoned(const gs_imager_state * pis, gx_device * dev)
{
return false;
}
float
gs_identity_transfer(floatp value, const gx_transfer_map * pmap)
{
return (float) value;
}
float
gs_mapped_transfer(floatp value, const gx_transfer_map * pmap)
{
return gx_map_color_float(pmap, value);
}
void
gx_set_identity_transfer(gx_transfer_map *pmap)
{
int i;
pmap->proc = gs_identity_transfer;
for (i = 0; i < transfer_map_size; ++i)
pmap->values[i] = bits2frac(i, log2_transfer_map_size);
}
#if FRAC_MAP_INTERPOLATE
frac
gx_color_frac_map(frac cv, const frac * values)
{
#define cp_frac_bits (frac_bits - log2_transfer_map_size)
int cmi = frac2bits_floor(cv, log2_transfer_map_size);
frac mv = values[cmi];
int rem, mdv;
rem = cv - bits2frac(cmi, log2_transfer_map_size);
if (rem == 0)
return mv;
mdv = values[cmi + 1] - mv;
#if arch_ints_are_short
if (mdv < -1 << (16 - cp_frac_bits) ||
mdv > 1 << (16 - cp_frac_bits)
)
return mv + (uint) (((ulong) rem * mdv) >> cp_frac_bits);
#endif
return mv + ((rem * mdv) >> cp_frac_bits);
#undef cp_frac_bits
}
#endif
gx_color_index
gx_default_w_b_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
int             i, ncomps = dev->color_info.num_components;
gx_color_value  cv_all = 0;
for (i = 0; i < ncomps; i++)
cv_all |= cv[i];
return cv_all > gx_max_color_value / 2 ? (gx_color_index)1
: (gx_color_index)0;
}
int
gx_default_w_b_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
prgb[0] = prgb[1] = prgb[2] = -(gx_color_value) color;
return 0;
}
gx_color_index
gx_default_b_w_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
int             i, ncomps = dev->color_info.num_components;
gx_color_value  cv_all = 0;
for (i = 0; i < ncomps; i++)
cv_all |= cv[i];
return cv_all > gx_max_color_value / 2 ? (gx_color_index)0
: (gx_color_index)1;
}
int
gx_default_b_w_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
prgb[0] = prgb[1] = prgb[2] = -((gx_color_value) color ^ 1);
return 0;
}
gx_color_index
gx_default_gray_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_value gray =
(((cv[0] * (ulong) lum_red_weight) +
(cv[1] * (ulong) lum_green_weight) +
(cv[2] * (ulong) lum_blue_weight) +
(lum_all_weights / 2)) / lum_all_weights
* dev->color_info.max_gray +
(gx_max_color_value / 2)) / gx_max_color_value;
return gray;
}
int
gx_default_gray_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
gx_color_value gray = (gx_color_value)
(color * gx_max_color_value / dev->color_info.max_gray);
prgb[0] = gray;
prgb[1] = gray;
prgb[2] = gray;
return 0;
}
gx_color_index
gx_default_8bit_map_gray_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_index color = gx_color_value_to_byte(cv[0]);
return (color == gx_no_color_index ? color ^ 1 : color);
}
int
gx_default_8bit_map_color_gray(gx_device * dev, gx_color_index color,
gx_color_value pgray[1])
{
pgray[0] = (gx_color_value)(color * gx_max_color_value / 255);
return 0;
}
gx_color_index
gx_default_rgb_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
if (dev->color_info.depth == 24)
return gx_color_value_to_byte(cv[2]) +
((uint) gx_color_value_to_byte(cv[1]) << 8) +
((ulong) gx_color_value_to_byte(cv[0]) << 16);
else {
int bpc = dev->color_info.depth / 3;
int drop = sizeof(gx_color_value) * 8 - bpc;
return ( ( (((gx_color_index)cv[0] >> drop) << bpc) +
((gx_color_index)cv[1] >> drop)         ) << bpc) +
((gx_color_index)cv[2] >> drop);
}
}
int
gx_default_rgb_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
if (dev->color_info.depth == 24) {
prgb[0] = gx_color_value_from_byte(color >> 16);
prgb[1] = gx_color_value_from_byte((color >> 8) & 0xff);
prgb[2] = gx_color_value_from_byte(color & 0xff);
} else {
uint bits_per_color = dev->color_info.depth / 3;
uint color_mask = (1 << bits_per_color) - 1;
prgb[0] = ((color >> (bits_per_color * 2)) & color_mask) *
(ulong) gx_max_color_value / color_mask;
prgb[1] = ((color >> (bits_per_color)) & color_mask) *
(ulong) gx_max_color_value / color_mask;
prgb[2] = (color & color_mask) *
(ulong) gx_max_color_value / color_mask;
}
return 0;
}
gx_color_index
gx_default_map_cmyk_color(gx_device * dev, const gx_color_value cv[])
{
frac rgb[3];
gx_color_value rgb_cv[3];
color_cmyk_to_rgb(cv2frac(cv[0]), cv2frac(cv[1]), cv2frac(cv[2]), cv2frac(cv[3]),
NULL, rgb);
rgb_cv[0] = frac2cv(rgb[0]);
rgb_cv[1] = frac2cv(rgb[1]);
rgb_cv[2] = frac2cv(rgb[2]);
return (*dev_proc(dev, map_rgb_color)) (dev, rgb_cv);
}
gx_color_index
cmyk_1bit_map_cmyk_color(gx_device * dev, const gx_color_value cv[])
{
#define CV_BIT(v) ((v) >> (gx_color_value_bits - 1))
return (gx_color_index)
(CV_BIT(cv[3]) + (CV_BIT(cv[2]) << 1) + (CV_BIT(cv[1]) << 2) + (CV_BIT(cv[0]) << 3));
#undef CV_BIT
}
int
cmyk_1bit_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
if (color & 1)
prgb[0] = prgb[1] = prgb[2] = 0;
else {
prgb[0] = (color & 8 ? 0 : gx_max_color_value);
prgb[1] = (color & 4 ? 0 : gx_max_color_value);
prgb[2] = (color & 2 ? 0 : gx_max_color_value);
}
return 0;
}
int
cmyk_1bit_map_color_cmyk(gx_device * dev, gx_color_index color,
gx_color_value pcv[4])
{
pcv[0] = (color & 8 ? 0 : gx_max_color_value);
pcv[1] = (color & 4 ? 0 : gx_max_color_value);
pcv[2] = (color & 2 ? 0 : gx_max_color_value);
pcv[3] = (color & 1 ? 0 : gx_max_color_value);
return 0;
}
gx_color_index
cmyk_8bit_map_cmyk_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_index color =
gx_color_value_to_byte(cv[3]) +
((uint)gx_color_value_to_byte(cv[2]) << 8) +
((uint)gx_color_value_to_byte(cv[1]) << 16) +
((uint)gx_color_value_to_byte(cv[0]) << 24);
return (color == gx_no_color_index ? color ^ 1 : color);
}
int
cmyk_8bit_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
int
not_k = (int) (~color & 0xff),
r = not_k - (int) (color >> 24),
g = not_k - (int) ((color >> 16) & 0xff),
b = not_k - (int) ((color >> 8) & 0xff);
prgb[0] = (r < 0 ? 0 : gx_color_value_from_byte(r));
prgb[1] = (g < 0 ? 0 : gx_color_value_from_byte(g));
prgb[2] = (b < 0 ? 0 : gx_color_value_from_byte(b));
return 0;
}
int
cmyk_8bit_map_color_cmyk(gx_device * dev, gx_color_index color,
gx_color_value pcv[4])
{
pcv[0] = gx_color_value_from_byte((color >> 24) & 0xff);
pcv[1] = gx_color_value_from_byte((color >> 16) & 0xff);
pcv[2] = gx_color_value_from_byte((color >> 8) & 0xff);
pcv[3] = gx_color_value_from_byte(color & 0xff);
return 0;
}
gx_color_index
gx_default_map_rgb_alpha_color(gx_device * dev,
gx_color_value r, gx_color_value g, gx_color_value b, gx_color_value alpha)
{
gx_color_value cv[3];
cv[0] = r; cv[1] = g; cv[2] = b;
return (*dev_proc(dev, map_rgb_color))(dev, cv);
}
int
gx_default_map_color_rgb_alpha(gx_device * dev, gx_color_index color,
gx_color_value prgba[4])
{
prgba[3] = gx_max_color_value;
return (*dev_proc(dev, map_color_rgb)) (dev, color, prgba);
}