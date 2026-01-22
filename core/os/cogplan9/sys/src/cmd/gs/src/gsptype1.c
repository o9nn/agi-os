#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsrop.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gxarith.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxcoord.h"
#include "gxcspace.h"
#include "gxcolor2.h"
#include "gxdcolor.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxclip2.h"
#include "gspath.h"
#include "gxpath.h"
#include "gxpcolor.h"
#include "gxp1impl.h"
#include "gzstate.h"
#include "gsimage.h"
#include "gsiparm4.h"
#include "gsovrc.h"
#define ADJUST_SCALE_FOR_THIN_LINES 0
#define ADJUST_SCALE_BY_GS_TRADITION 0
#define ADJUST_AS_ADOBE 1
private_st_pattern1_template();
private_st_pattern1_instance();
private ENUM_PTRS_BEGIN(pattern1_instance_enum_ptrs) {
if (index < st_pattern1_template_max_ptrs) {
gs_ptr_type_t ptype =
ENUM_SUPER_ELT(gs_pattern1_instance_t, st_pattern1_template,
template, 0);
if (ptype)
return ptype;
return ENUM_OBJ(NULL);
}
ENUM_PREFIX(st_pattern_instance, st_pattern1_template_max_ptrs);
} ENUM_PTRS_END
private RELOC_PTRS_BEGIN(pattern1_instance_reloc_ptrs) {
RELOC_PREFIX(st_pattern_instance);
RELOC_SUPER(gs_pattern1_instance_t, st_pattern1_template, template);
} RELOC_PTRS_END
private pattern_proc_uses_base_space(gs_pattern1_uses_base_space);
private pattern_proc_make_pattern(gs_pattern1_make_pattern);
private pattern_proc_get_pattern(gs_pattern1_get_pattern);
private pattern_proc_set_color(gs_pattern1_set_color);
private const gs_pattern_type_t gs_pattern1_type = {
1, {
gs_pattern1_uses_base_space, gs_pattern1_make_pattern,
gs_pattern1_get_pattern, gs_pattern1_remap_color,
gs_pattern1_set_color
}
};
int
gs_cspace_build_Pattern1(gs_color_space ** ppcspace,
const gs_color_space * pbase_cspace, gs_memory_t * pmem)
{
gs_color_space *pcspace = 0;
int code;
if (pbase_cspace != 0) {
if (gs_color_space_num_components(pcspace) < 0)
return_error(gs_error_rangecheck);
}
code = gs_cspace_alloc(&pcspace, &gs_color_space_type_Pattern, pmem);
if (code < 0)
return code;
if (pbase_cspace != 0) {
pcspace->params.pattern.has_base_space = true;
gs_cspace_init_from((gs_color_space *) & (pcspace->params.pattern.base_space),
pbase_cspace
);
} else
pcspace->params.pattern.has_base_space = false;
*ppcspace = pcspace;
return 0;
}
void
gs_pattern1_init(gs_pattern1_template_t * ppat)
{
gs_pattern_common_init((gs_pattern_template_t *)ppat, &gs_pattern1_type);
}
private int compute_inst_matrix(gs_pattern1_instance_t * pinst,
const gs_state * saved, gs_rect * pbbox, int width, int height);
int
gs_makepattern(gs_client_color * pcc, const gs_pattern1_template_t * pcp,
const gs_matrix * pmat, gs_state * pgs, gs_memory_t * mem)
{
return gs_pattern1_make_pattern(pcc, (const gs_pattern_template_t *)pcp,
pmat, pgs, mem);
}
private int
gs_pattern1_make_pattern(gs_client_color * pcc,
const gs_pattern_template_t * ptemp,
const gs_matrix * pmat, gs_state * pgs,
gs_memory_t * mem)
{
const gs_pattern1_template_t *pcp = (const gs_pattern1_template_t *)ptemp;
gs_pattern1_instance_t inst;
gs_pattern1_instance_t *pinst;
gs_state *saved;
gs_rect bbox;
gs_fixed_rect cbox;
gx_device * pdev = pgs->device;
int dev_width = pdev->width;
int dev_height = pdev->height;
int code = gs_make_pattern_common(pcc, (const gs_pattern_template_t *)pcp,
pmat, pgs, mem,
&st_pattern1_instance);
if (code < 0)
return code;
if (mem == 0)
mem = gs_state_memory(pgs);
pinst = (gs_pattern1_instance_t *)pcc->pattern;
*(gs_pattern_instance_t *)&inst = *(gs_pattern_instance_t *)pinst;
saved = inst.saved;
switch (pcp->PaintType) {
case 1:
gs_set_logical_op(saved, lop_default);
break;
case 2:
gx_set_device_color_1(saved);
break;
default:
code = gs_note_error(gs_error_rangecheck);
goto fsaved;
}
inst.template = *pcp;
code = compute_inst_matrix(&inst, saved, &bbox, dev_width, dev_height);
if (code < 0)
goto fsaved;
#define mat inst.step_matrix
if_debug6('t', "[t]step_matrix=[%g %g %g %g %g %g]\n",
mat.xx, mat.xy, mat.yx, mat.yy, mat.tx, mat.ty);
if_debug4('t', "[t]bbox=(%g,%g),(%g,%g)\n",
bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y);
{
float bbw = bbox.q.x - bbox.p.x;
float bbh = bbox.q.y - bbox.p.y;
if (ADJUST_SCALE_BY_GS_TRADITION) {
inst.size.x = (int)(bbw + 0.8);
inst.size.y = (int)(bbh + 0.8);
} else {
inst.size.x = (int)ceil(bbw);
inst.size.y = (int)ceil(bbh);
}
if (inst.size.x == 0 || inst.size.y == 0) {
gs_make_identity(&mat);
bbox.p.x = bbox.p.y = bbox.q.x = bbox.q.y = 0;
} else {
if (fabs(mat.xx * mat.yy - mat.xy * mat.yx) < 1.0e-6) {
code = gs_note_error(gs_error_rangecheck);
goto fsaved;
}
if (ADJUST_SCALE_BY_GS_TRADITION &&
mat.xy == 0 && mat.yx == 0 &&
fabs(fabs(mat.xx) - bbw) < 0.5 &&
fabs(fabs(mat.yy) - bbh) < 0.5
) {
gs_scale(saved, fabs(inst.size.x / mat.xx),
fabs(inst.size.y / mat.yy));
code = compute_inst_matrix(&inst, saved, &bbox,
dev_width, dev_height);
if (code < 0)
goto fsaved;
if (ADJUST_SCALE_FOR_THIN_LINES) {
gs_scale(saved, (fabs(inst.size.x) - 1.0 / fixed_scale) / fabs(inst.size.x),
(fabs(inst.size.y) - 1.0 / fixed_scale) / fabs(inst.size.y));
}
if_debug2('t',
"[t]adjusted XStep & YStep to size=(%d,%d)\n",
inst.size.x, inst.size.y);
if_debug4('t', "[t]bbox=(%g,%g),(%g,%g)\n",
bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y);
} else if (ADJUST_AS_ADOBE) {
if (mat.xy == 0 && mat.yx == 0 &&
fabs(fabs(mat.xx) - bbw) < 0.5 &&
fabs(fabs(mat.yy) - bbh) < 0.5
) {
if (inst.step_matrix.xx <= 2) {
gs_scale(saved, fabs(inst.size.x / mat.xx), 1);
inst.step_matrix.xx = (float)inst.size.x;
} else {
inst.step_matrix.xx = (float)floor(inst.step_matrix.xx + 0.5);
if (bbw >= inst.size.x - 1.0 / fixed_scale)
gs_scale(saved, (fabs(inst.size.x) - 1.0 / fixed_scale) / fabs(inst.size.x), 1);
}
if (inst.step_matrix.yy <= 2) {
gs_scale(saved, 1, fabs(inst.size.y / mat.yy));
inst.step_matrix.yy = (float)inst.size.y;
} else {
inst.step_matrix.yy = (float)floor(inst.step_matrix.yy + 0.5);
if (bbh >= inst.size.y - 1.0 / fixed_scale)
gs_scale(saved, 1, (fabs(inst.size.y) - 1.0 / fixed_scale) / fabs(inst.size.y));
}
code = gs_bbox_transform(&inst.template.BBox, &ctm_only(saved), &bbox);
if (code < 0)
goto fsaved;
}
}
}
}
if ((code = gs_bbox_transform_inverse(&bbox, &mat, &inst.bbox)) < 0)
goto fsaved;
if_debug4('t', "[t]ibbox=(%g,%g),(%g,%g)\n",
inst.bbox.p.x, inst.bbox.p.y, inst.bbox.q.x, inst.bbox.q.y);
inst.is_simple = (fabs(mat.xx) == inst.size.x && mat.xy == 0 &&
mat.yx == 0 && fabs(mat.yy) == inst.size.y);
if_debug6('t',
"[t]is_simple? xstep=(%g,%g) ystep=(%g,%g) size=(%d,%d)\n",
inst.step_matrix.xx, inst.step_matrix.xy,
inst.step_matrix.yx, inst.step_matrix.yy,
inst.size.x, inst.size.y);
inst.uses_mask = true;
gx_translate_to_fixed(saved, float2fixed_rounded(mat.tx - bbox.p.x),
float2fixed_rounded(mat.ty - bbox.p.y));
mat.tx = bbox.p.x;
mat.ty = bbox.p.y;
#undef mat
cbox.p.x = fixed_0;
cbox.p.y = fixed_0;
cbox.q.x = int2fixed(inst.size.x);
cbox.q.y = int2fixed(inst.size.y);
code = gx_clip_to_rectangle(saved, &cbox);
if (code < 0)
goto fsaved;
inst.id = gs_next_ids(mem, 1);
*pinst = inst;
return 0;
#undef mat
fsaved:gs_state_free(saved);
gs_free_object(mem, pinst, "gs_makepattern");
return code;
}
private int
clamp_pattern_bbox(gs_pattern1_instance_t * pinst, gs_rect * pbbox,
int width, int height, const gs_matrix * pmat)
{
double xstep = pinst->template.XStep;
double ystep = pinst->template.YStep;
double xmin = pbbox->q.x;
double xmax = pbbox->p.x;
double ymin = pbbox->q.y;
double ymax = pbbox->p.y;
int ixpat, iypat, iystart;
double xpat, ypat;
double xlower, xupper, ylower, yupper;
double xdev, ydev;
gs_rect dev_page, pat_page;
gs_point dev_pat_origin, dev_step;
int code;
dev_page.p.x = dev_page.p.y = 0;
dev_page.q.x = width;
dev_page.q.y = height;
code = gs_bbox_transform_inverse(&dev_page, pmat, &pat_page);
if (code < 0)
return code;
gs_point_transform(0.0, 0.0, pmat, &dev_pat_origin);
ixpat = (int) floor((pat_page.p.x - pinst->template.BBox.q.x) / xstep);
iystart = (int) floor((pat_page.p.y - pinst->template.BBox.q.y) / ystep);
for (; ; ixpat++) {
xpat = ixpat * xstep;
for (iypat = iystart; ; iypat++) {
ypat = iypat * ystep;
gs_point_transform(xpat, ypat, pmat, &dev_step);
xdev = dev_step.x - dev_pat_origin.x;
ydev = dev_step.y - dev_pat_origin.y;
xlower = (xdev + pbbox->p.x > 0) ? pbbox->p.x : -xdev;
xupper = (xdev + pbbox->q.x < width) ? pbbox->q.x : -xdev + width;
ylower = (ydev + pbbox->p.y > 0) ? pbbox->p.y : -ydev;
yupper = (ydev + pbbox->q.y < height) ? pbbox->q.y : -ydev + height;
if (xlower < xupper && ylower < yupper) {
if (xlower < xmin)
xmin = xlower;
if (xupper > xmax)
xmax = xupper;
if (ylower < ymin)
ymin = ylower;
if (yupper > ymax)
ymax = yupper;
}
if (ypat > pat_page.q.y - pinst->template.BBox.p.y)
break;
}
if (xpat > pat_page.q.x - pinst->template.BBox.p.x)
break;
}
if (xmin < xmax && ymin < ymax) {
pbbox->p.x = xmin;
pbbox->q.x = xmax;
pbbox->p.y = ymin;
pbbox->q.y = ymax;
} else {
pbbox->p.x = pbbox->p.y = 0;
pbbox->q.x = pbbox->q.y = 1;
}
return 0;
}
private int
compute_inst_matrix(gs_pattern1_instance_t * pinst, const gs_state * saved,
gs_rect * pbbox, int width, int height)
{
double xx = pinst->template.XStep * saved->ctm.xx;
double xy = pinst->template.XStep * saved->ctm.xy;
double yx = pinst->template.YStep * saved->ctm.yx;
double yy = pinst->template.YStep * saved->ctm.yy;
int code;
if (xx == 0 || yy == 0) {
double temp;
temp = xx, xx = yx, yx = temp;
temp = xy, xy = yy, yy = temp;
}
if (xx < 0)
xx = -xx, xy = -xy;
if (yy < 0)
yx = -yx, yy = -yy;
pinst->step_matrix.xx = xx;
pinst->step_matrix.xy = xy;
pinst->step_matrix.yx = yx;
pinst->step_matrix.yy = yy;
pinst->step_matrix.tx = saved->ctm.tx;
pinst->step_matrix.ty = saved->ctm.ty;
code = gs_bbox_transform(&pinst->template.BBox, &ctm_only(saved), pbbox);
if (code >= 0 &&
(pbbox->q.x - pbbox->p.x > width || pbbox->q.y - pbbox->p.y > height))
code = clamp_pattern_bbox(pinst, pbbox, width,
height, &ctm_only(saved));
return code;
}
private bool
gs_pattern1_uses_base_space(const gs_pattern_template_t *ptemp)
{
return ((const gs_pattern1_template_t *)ptemp)->PaintType == 2;
}
private const gs_pattern_template_t *
gs_pattern1_get_pattern(const gs_pattern_instance_t *pinst)
{
return (const gs_pattern_template_t *)
&((const gs_pattern1_instance_t *)pinst)->template;
}
private int
gs_pattern1_set_color(const gs_client_color * pcc, gs_state * pgs)
{
gs_pattern1_instance_t * pinst = (gs_pattern1_instance_t *)pcc->pattern;
gs_pattern1_template_t * ptmplt = &pinst->template;
if (ptmplt->PaintType == 2) {
const gs_color_space *  pcs = pgs->color_space;
pcs = (const gs_color_space *)&(pcs->params.pattern.base_space);
return pcs->type->set_overprint(pcs, pgs);
} else {
gs_overprint_params_t   params;
params.retain_any_comps = false;
pgs->effective_overprint_mode = 0;
return gs_state_update_overprint(pgs, &params);
}
}
const gs_pattern1_template_t *
gs_getpattern(const gs_client_color * pcc)
{
const gs_pattern_instance_t *pinst = pcc->pattern;
return (pinst == 0 || pinst->type != &gs_pattern1_type ? 0 :
&((const gs_pattern1_instance_t *)pinst)->template);
}
public_st_gs_bitmap();
public_st_gs_tile_bitmap();
public_st_gs_depth_bitmap();
public_st_gs_tile_depth_bitmap();
public_st_gx_strip_bitmap();
typedef struct pixmap_info_s {
gs_depth_bitmap bitmap;
const gs_color_space *pcspace;
uint white_index;
void (*free_proc)(gs_memory_t *, void *, client_name_t);
} pixmap_info;
gs_private_st_suffix_add1(st_pixmap_info,
pixmap_info,
"pixmap info. struct",
pixmap_enum_ptr,
pixmap_reloc_ptr,
st_gs_depth_bitmap,
pcspace
);
#define st_pixmap_info_max_ptrs (1 + st_tile_bitmap_max_ptrs)
private void
free_pixmap_pattern(
gs_memory_t *           pmem,
void *                  pvpinst,
client_name_t           cname
)
{
gs_pattern1_instance_t *pinst = (gs_pattern1_instance_t *)pvpinst;
pixmap_info *ppmap = pinst->template.client_data;
ppmap->free_proc(pmem, pvpinst, cname);
gs_free_object(pmem, ppmap, cname);
}
private int bitmap_paint(gs_image_enum * pen, gs_data_image_t * pim,
const gs_depth_bitmap * pbitmap, gs_state * pgs);
private int
mask_PaintProc(const gs_client_color * pcolor, gs_state * pgs)
{
const pixmap_info *ppmap = gs_getpattern(pcolor)->client_data;
const gs_depth_bitmap *pbitmap = &(ppmap->bitmap);
gs_image_enum *pen =
gs_image_enum_alloc(gs_state_memory(pgs), "mask_PaintProc");
gs_image1_t mask;
if (pen == 0)
return_error(gs_error_VMerror);
gs_image_t_init_mask(&mask, true);
mask.Width = pbitmap->size.x;
mask.Height = pbitmap->size.y;
gs_image_init(pen, &mask, false, pgs);
return bitmap_paint(pen, (gs_data_image_t *) & mask, pbitmap, pgs);
}
private int
image_PaintProc(const gs_client_color * pcolor, gs_state * pgs)
{
const pixmap_info *ppmap = gs_getpattern(pcolor)->client_data;
const gs_depth_bitmap *pbitmap = &(ppmap->bitmap);
gs_image_enum *pen =
gs_image_enum_alloc(gs_state_memory(pgs), "image_PaintProc");
gs_color_space cs;
const gs_color_space *pcspace;
gx_image_enum_common_t *pie;
int transparent = ppmap->white_index < (1 << (pbitmap->num_comps * pbitmap->pix_depth));
union {
gs_image1_t i1;
gs_image4_t i4;
} image;
int code;
if (pen == 0)
return_error(gs_error_VMerror);
if (ppmap->pcspace == 0) {
gs_cspace_init_DeviceGray(pgs->memory, &cs);
pcspace = &cs;
} else
pcspace = ppmap->pcspace;
gs_gsave(pgs);
gs_setcolorspace(pgs, pcspace);
if (transparent)
gs_image4_t_init( (gs_image4_t *) &image, pcspace);
else
gs_image_t_init_adjust( (gs_image_t *) &image, pcspace, 0);
image.i1.Width = pbitmap->size.x;
image.i1.Height = pbitmap->size.y;
if (transparent) {
image.i4.MaskColor_is_range = false;
image.i4.MaskColor[0] = ppmap->white_index;
}
image.i1.Decode[0] = 0.0;
image.i1.Decode[1] = (float)((1 << pbitmap->pix_depth) - 1);
image.i1.BitsPerComponent = pbitmap->pix_depth;
if (ppmap->pcspace == 0) {
image.i1.Decode[0] = 1.0;
image.i1.Decode[1] = 0.0;
}
if ( (code = gs_image_begin_typed( (const gs_image_common_t *)&image,
pgs,
false,
&pie )) >= 0 &&
(code = gs_image_enum_init( pen,
pie,
(gs_data_image_t *)&image,
pgs )) >= 0      )
code = bitmap_paint(pen, (gs_data_image_t *) & image, pbitmap, pgs);
gs_grestore(pgs);
return code;
}
private int
bitmap_paint(gs_image_enum * pen, gs_data_image_t * pim,
const gs_depth_bitmap * pbitmap, gs_state * pgs)
{
uint raster = pbitmap->raster;
uint nbytes = (pim->Width * pbitmap->pix_depth + 7) >> 3;
uint used;
const byte *dp = pbitmap->data;
int n;
int code = 0, code1;
if (nbytes == raster)
code = gs_image_next(pen, dp, nbytes * pim->Height, &used);
else
for (n = pim->Height; n > 0 && code >= 0; dp += raster, --n)
code = gs_image_next(pen, dp, nbytes, &used);
code1 = gs_image_cleanup_and_free_enum(pen);
if (code >= 0 && code1 < 0)
code = code1;
return code;
}
int
gs_makepixmappattern(
gs_client_color * pcc,
const gs_depth_bitmap * pbitmap,
bool mask,
const gs_matrix * pmat,
long id,
const gs_color_space * pcspace,
uint white_index,
gs_state * pgs,
gs_memory_t * mem
)
{
gs_pattern1_template_t pat;
pixmap_info *ppmap;
gs_matrix mat, smat;
int code;
if ((mask) || (pcspace == 0)) {
if (pbitmap->pix_depth != 1)
return_error(gs_error_rangecheck);
pcspace = 0;
} else if (gs_color_space_get_index(pcspace) != gs_color_space_index_Indexed)
return_error(gs_error_rangecheck);
if (pbitmap->num_comps != 1)
return_error(gs_error_rangecheck);
if (mem == 0)
mem = gs_state_memory(pgs);
ppmap = gs_alloc_struct(mem,
pixmap_info,
&st_pixmap_info,
"makepximappattern"
);
if (ppmap == 0)
return_error(gs_error_VMerror);
ppmap->bitmap = *pbitmap;
ppmap->pcspace = pcspace;
ppmap->white_index = white_index;
gs_pattern1_init(&pat);
uid_set_UniqueID(&pat.uid, (id == no_UniqueID) ? gs_next_ids(mem, 1) : id);
pat.PaintType = (mask ? 2 : 1);
pat.TilingType = 1;
pat.BBox.p.x = 0;
pat.BBox.p.y = 0;
pat.BBox.q.x = pbitmap->size.x;
pat.BBox.q.y = pbitmap->size.y;
pat.XStep = (float)pbitmap->size.x;
pat.YStep = (float)pbitmap->size.y;
pat.PaintProc = (mask ? mask_PaintProc : image_PaintProc);
pat.client_data = ppmap;
gs_currentmatrix(pgs, &smat);
gs_make_identity(&mat);
gs_setmatrix(pgs, &mat);
if (pmat == NULL)
pmat = &mat;
if ((code = gs_makepattern(pcc, &pat, pmat, pgs, mem)) != 0)
gs_free_object(mem, ppmap, "makebitmappattern_xform");
else {
gs_pattern1_instance_t *pinst =
(gs_pattern1_instance_t *)pcc->pattern;
if (!mask && (white_index >= (1 << pbitmap->pix_depth)))
pinst->uses_mask = false;
ppmap->free_proc = pinst->rc.free;
pinst->rc.free = free_pixmap_pattern;
gs_setgray(pinst->saved, 0.0);
}
gs_setmatrix(pgs, &smat);
return code;
}
int
gs_makebitmappattern_xform(
gs_client_color * pcc,
const gx_tile_bitmap * ptile,
bool mask,
const gs_matrix * pmat,
long id,
gs_state * pgs,
gs_memory_t * mem
)
{
gs_depth_bitmap bitmap;
bitmap.data = ptile->data;
bitmap.raster = ptile->raster;
bitmap.size.x = ptile->rep_width;
bitmap.size.y = ptile->rep_height;
bitmap.id = ptile->id;
bitmap.pix_depth = 1;
bitmap.num_comps = 1;
return gs_makepixmappattern(pcc, &bitmap, mask, pmat, id, 0, 0, pgs, mem);
}
private dev_color_proc_get_dev_halftone(gx_dc_pattern_get_dev_halftone);
private dev_color_proc_load(gx_dc_pattern_load);
private dev_color_proc_equal(gx_dc_pattern_equal);
private dev_color_proc_load(gx_dc_pure_masked_load);
private dev_color_proc_get_dev_halftone(gx_dc_pure_masked_get_dev_halftone);
private dev_color_proc_equal(gx_dc_pure_masked_equal);
private dev_color_proc_load(gx_dc_binary_masked_load);
private dev_color_proc_get_dev_halftone(gx_dc_binary_masked_get_dev_halftone);
private dev_color_proc_equal(gx_dc_binary_masked_equal);
private dev_color_proc_load(gx_dc_colored_masked_load);
private dev_color_proc_get_dev_halftone(gx_dc_colored_masked_get_dev_halftone);
private dev_color_proc_equal(gx_dc_colored_masked_equal);
gs_private_st_composite(st_dc_pattern, gx_device_color, "dc_pattern",
dc_pattern_enum_ptrs, dc_pattern_reloc_ptrs);
const gx_device_color_type_t gx_dc_pattern = {
&st_dc_pattern,
gx_dc_pattern_save_dc, gx_dc_pattern_get_dev_halftone,
gx_dc_ht_get_phase,
gx_dc_pattern_load, gx_dc_pattern_fill_rectangle,
gx_dc_default_fill_masked, gx_dc_pattern_equal,
gx_dc_pattern_write, gx_dc_pattern_read,
gx_dc_pattern_get_nonzero_comps
};
extern_st(st_dc_ht_binary);
gs_private_st_composite(st_dc_pure_masked, gx_device_color, "dc_pure_masked",
dc_masked_enum_ptrs, dc_masked_reloc_ptrs);
const gx_device_color_type_t gx_dc_pure_masked = {
&st_dc_pure_masked,
gx_dc_pattern_save_dc, gx_dc_pure_masked_get_dev_halftone,
gx_dc_no_get_phase,
gx_dc_pure_masked_load, gx_dc_pure_masked_fill_rect,
gx_dc_default_fill_masked, gx_dc_pure_masked_equal,
gx_dc_pattern_write, gx_dc_pattern_read,
gx_dc_pure_get_nonzero_comps
};
gs_private_st_composite(st_dc_binary_masked, gx_device_color,
"dc_binary_masked", dc_binary_masked_enum_ptrs,
dc_binary_masked_reloc_ptrs);
const gx_device_color_type_t gx_dc_binary_masked = {
&st_dc_binary_masked,
gx_dc_pattern_save_dc, gx_dc_binary_masked_get_dev_halftone,
gx_dc_ht_get_phase,
gx_dc_binary_masked_load, gx_dc_binary_masked_fill_rect,
gx_dc_default_fill_masked, gx_dc_binary_masked_equal,
gx_dc_pattern_write, gx_dc_pattern_read,
gx_dc_ht_binary_get_nonzero_comps
};
gs_private_st_composite_only(st_dc_colored_masked, gx_device_color,
"dc_colored_masked",
dc_masked_enum_ptrs, dc_masked_reloc_ptrs);
const gx_device_color_type_t gx_dc_colored_masked = {
&st_dc_colored_masked,
gx_dc_pattern_save_dc, gx_dc_colored_masked_get_dev_halftone,
gx_dc_ht_get_phase,
gx_dc_colored_masked_load, gx_dc_colored_masked_fill_rect,
gx_dc_default_fill_masked, gx_dc_colored_masked_equal,
gx_dc_pattern_write, gx_dc_pattern_read,
gx_dc_ht_colored_get_nonzero_comps
};
#undef gx_dc_type_pattern
const gx_device_color_type_t *const gx_dc_type_pattern = &gx_dc_pattern;
#define gx_dc_type_pattern (&gx_dc_pattern)
private
ENUM_PTRS_WITH(dc_pattern_enum_ptrs, gx_device_color *cptr)
{
return ENUM_USING(st_dc_pure_masked, vptr, size, index - 1);
}
case 0:
{
gx_color_tile *tile = cptr->colors.pattern.p_tile;
ENUM_RETURN((tile == 0 ? tile : tile - tile->index));
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(dc_pattern_reloc_ptrs, gx_device_color *cptr)
{
gx_color_tile *tile = cptr->colors.pattern.p_tile;
if (tile != 0) {
uint index = tile->index;
RELOC_TYPED_OFFSET_PTR(gx_device_color, colors.pattern.p_tile, index);
}
RELOC_USING(st_dc_pure_masked, vptr, size);
}
RELOC_PTRS_END
private ENUM_PTRS_WITH(dc_masked_enum_ptrs, gx_device_color *cptr)
ENUM_SUPER(gx_device_color, st_client_color, ccolor, 1);
case 0:
{
gx_color_tile *mask = cptr->mask.m_tile;
ENUM_RETURN((mask == 0 ? mask : mask - mask->index));
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(dc_masked_reloc_ptrs, gx_device_color *cptr)
{
gx_color_tile *mask = cptr->mask.m_tile;
RELOC_SUPER(gx_device_color, st_client_color, ccolor);
if (mask != 0) {
uint index = mask->index;
RELOC_TYPED_OFFSET_PTR(gx_device_color, mask.m_tile, index);
}
}
RELOC_PTRS_END
private ENUM_PTRS_BEGIN(dc_binary_masked_enum_ptrs)
{
return ENUM_USING(st_dc_ht_binary, vptr, size, index - 2);
}
case 0:
case 1:
return ENUM_USING(st_dc_pure_masked, vptr, size, index);
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(dc_binary_masked_reloc_ptrs)
{
RELOC_USING(st_dc_pure_masked, vptr, size);
RELOC_USING(st_dc_ht_binary, vptr, size);
}
RELOC_PTRS_END
void
gx_dc_pattern_save_dc(
const gx_device_color * pdevc,
gx_device_color_saved * psdc )
{
psdc->type = pdevc->type;
if (pdevc->ccolor_valid) {
psdc->colors.pattern.id = pdevc->ccolor.pattern->pattern_id;
psdc->colors.pattern.phase = pdevc->phase;
} {
psdc->colors.pattern.id = gs_no_id;
psdc->colors.pattern.phase.x = psdc->colors.pattern.phase.y = 0;
}
}
private const gx_device_halftone *
gx_dc_pattern_get_dev_halftone(const gx_device_color * pdevc)
{
return 0;
}
private const gx_device_halftone *
gx_dc_pure_masked_get_dev_halftone(const gx_device_color * pdevc)
{
return 0;
}
private const gx_device_halftone *
gx_dc_binary_masked_get_dev_halftone(const gx_device_color * pdevc)
{
return pdevc->colors.binary.b_ht;
}
private const gx_device_halftone *
gx_dc_colored_masked_get_dev_halftone(const gx_device_color * pdevc)
{
return pdevc->colors.colored.c_ht;
}
#define FINISH_PATTERN_LOAD\
while ( !gx_pattern_cache_lookup(pdevc, pis, dev, select) )\
{ code = gx_pattern_load(pdevc, pis, dev, select);\
if ( code < 0 ) break;\
}\
return code;
private int
gx_dc_pattern_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int code = 0;
FINISH_PATTERN_LOAD
}
private int
gx_dc_pure_masked_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int code = (*gx_dc_type_data_pure.load) (pdevc, pis, dev, select);
if (code < 0)
return code;
FINISH_PATTERN_LOAD
}
private int
gx_dc_binary_masked_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int code = (*gx_dc_type_data_ht_binary.load) (pdevc, pis, dev, select);
if (code < 0)
return code;
FINISH_PATTERN_LOAD
}
private int
gx_dc_colored_masked_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int code = (*gx_dc_type_data_ht_colored.load) (pdevc, pis, dev, select);
if (code < 0)
return code;
FINISH_PATTERN_LOAD
}
bool
gx_pattern_cache_lookup(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
gx_pattern_cache *pcache = pis->pattern_cache;
gx_bitmap_id id = pdevc->mask.id;
if (id == gx_no_bitmap_id) {
color_set_null_pattern(pdevc);
return true;
}
if (pcache != 0) {
gx_color_tile *ctile = &pcache->tiles[id % pcache->num_tiles];
bool internal_accum = true;
if (pis->have_pattern_streams) {
int code = dev_proc(dev, pattern_manage)(dev, id, NULL, pattern_manage__load);
internal_accum = (code == 0);
if (code < 0)
return false;
}
if (ctile->id == id &&
ctile->is_dummy == !internal_accum &&
(pdevc->type != &gx_dc_pattern ||
ctile->depth == dev->color_info.depth)
) {
int px = pis->screen_phase[select].x;
int py = pis->screen_phase[select].y;
if (pdevc->type == &gx_dc_pattern) {
pdevc->colors.pattern.p_tile = ctile;
color_set_phase_mod(pdevc, px, py,
ctile->tbits.rep_width,
ctile->tbits.rep_height);
}
pdevc->mask.m_tile =
(ctile->tmask.data == 0 ? (gx_color_tile *) 0 :
ctile);
pdevc->mask.m_phase.x = -px;
pdevc->mask.m_phase.y = -py;
return true;
}
}
return false;
}
#undef FINISH_PATTERN_LOAD
private bool
gx_dc_pattern_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
return pdevc2->type == pdevc1->type &&
pdevc1->phase.x == pdevc2->phase.x &&
pdevc1->phase.y == pdevc2->phase.y &&
pdevc1->mask.id == pdevc2->mask.id;
}
int
gx_dc_pattern_get_nonzero_comps(
const gx_device_color * pdevc_ignored,
const gx_device *       dev_ignored,
gx_color_index *        pcomp_bits_ignored )
{
return 1;
}
private bool
gx_dc_pure_masked_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
return (*gx_dc_type_pure->equal) (pdevc1, pdevc2) &&
pdevc1->mask.id == pdevc2->mask.id;
}
private bool
gx_dc_binary_masked_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
return (*gx_dc_type_ht_binary->equal) (pdevc1, pdevc2) &&
pdevc1->mask.id == pdevc2->mask.id;
}
private bool
gx_dc_colored_masked_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
return (*gx_dc_type_ht_colored->equal) (pdevc1, pdevc2) &&
pdevc1->mask.id == pdevc2->mask.id;
}
int
gx_dc_pattern_write(
const gx_device_color *         pdevc,
const gx_device_color_saved *   psdc,
const gx_device *               dev,
byte *                          data,
uint *                          psize )
{
return_error(gs_error_unknownerror);
}
int
gx_dc_pattern_read(
gx_device_color *       pdevc,
const gs_imager_state * pis,
const gx_device_color * prior_devc,
const gx_device *       dev,
const byte *            data,
uint                    size,
gs_memory_t *           mem )
{
return_error(gs_error_unknownerror);
}