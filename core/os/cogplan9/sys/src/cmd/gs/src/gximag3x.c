#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsbitops.h"
#include "gscspace.h"
#include "gscpixel.h"
#include "gsstruct.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gximag3x.h"
#include "gxistate.h"
#include "gdevbbox.h"
extern_st(st_color_space);
private dev_proc_begin_typed_image(gx_begin_image3x);
private image_enum_proc_plane_data(gx_image3x_plane_data);
private image_enum_proc_end_image(gx_image3x_end_image);
private image_enum_proc_flush(gx_image3x_flush);
private image_enum_proc_planes_wanted(gx_image3x_planes_wanted);
private_st_gs_image3x();
const gx_image_type_t gs_image_type_3x = {
&st_gs_image3x, gx_begin_image3x, gx_data_image_source_size,
gx_image_no_sput, gx_image_no_sget, gx_image_default_release,
IMAGE3X_IMAGETYPE
};
private const gx_image_enum_procs_t image3x_enum_procs = {
gx_image3x_plane_data, gx_image3x_end_image,
gx_image3x_flush, gx_image3x_planes_wanted
};
private void
gs_image3x_mask_init(gs_image3x_mask_t *pimm)
{
pimm->InterleaveType = 0;
pimm->has_Matte = false;
gs_data_image_t_init(&pimm->MaskDict, 1);
pimm->MaskDict.BitsPerComponent = 0;
}
void
gs_image3x_t_init(gs_image3x_t * pim, const gs_color_space * color_space)
{
gs_pixel_image_t_init((gs_pixel_image_t *) pim, color_space);
pim->type = &gs_image_type_3x;
gs_image3x_mask_init(&pim->Opacity);
gs_image3x_mask_init(&pim->Shape);
}
typedef struct image3x_channel_state_s {
gx_image_enum_common_t *info;
gx_device *mdev;
gs_image3_interleave_type_t InterleaveType;
int width, height, full_height, depth;
byte *data;
int y;
int skip;
} image3x_channel_state_t;
typedef struct gx_image3x_enum_s {
gx_image_enum_common;
gx_device *pcdev;
int num_components;
int bpc;
gs_memory_t *memory;
#define NUM_MASKS 2
image3x_channel_state_t mask[NUM_MASKS], pixel;
} gx_image3x_enum_t;
extern_st(st_gx_image_enum_common);
gs_private_st_suffix_add9(st_image3x_enum, gx_image3x_enum_t,
"gx_image3x_enum_t", image3x_enum_enum_ptrs, image3x_enum_reloc_ptrs,
st_gx_image_enum_common, pcdev, mask[0].info, mask[0].mdev, mask[0].data,
mask[1].info, mask[1].mdev, mask[1].data, pixel.info, pixel.data);
typedef struct image3x_channel_values_s {
gs_matrix matrix;
gs_point corner;
gs_int_rect rect;
gs_image_t image;
} image3x_channel_values_t;
private int check_image3x_mask(const gs_image3x_t *pim,
const gs_image3x_mask_t *pimm,
const image3x_channel_values_t *ppcv,
image3x_channel_values_t *pmcv,
image3x_channel_state_t *pmcs,
gs_memory_t *mem);
int
gx_begin_image3x_generic(gx_device * dev,
const gs_imager_state *pis, const gs_matrix *pmat,
const gs_image_common_t *pic, const gs_int_rect *prect,
const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath, gs_memory_t *mem,
image3x_make_mid_proc_t make_mid,
image3x_make_mcde_proc_t make_mcde,
gx_image_enum_common_t **pinfo)
{
const gs_image3x_t *pim = (const gs_image3x_t *)pic;
gx_image3x_enum_t *penum;
gx_device *pcdev = 0;
image3x_channel_values_t mask[2], pixel;
gs_matrix mat;
gx_device *midev[2];
gx_image_enum_common_t *minfo[2];
gs_int_point origin[2];
int code;
int i;
if (pim->Height <= 0)
return_error(gs_error_rangecheck);
penum = gs_alloc_struct(mem, gx_image3x_enum_t, &st_image3x_enum,
"gx_begin_image3x");
if (penum == 0)
return_error(gs_error_VMerror);
penum->mask[0].info = 0, penum->mask[0].mdev = 0, penum->mask[0].data = 0;
penum->mask[1].info = 0, penum->mask[1].mdev = 0, penum->mask[1].data = 0;
penum->pixel.info = 0, penum->pixel.data = 0;
if (prect)
pixel.rect = *prect;
else {
pixel.rect.p.x = pixel.rect.p.y = 0;
pixel.rect.q.x = pim->Width;
pixel.rect.q.y = pim->Height;
}
if ((code = gs_matrix_invert(&pim->ImageMatrix, &pixel.matrix)) < 0 ||
(code = gs_point_transform(pim->Width, pim->Height, &pixel.matrix,
&pixel.corner)) < 0 ||
(code = check_image3x_mask(pim, &pim->Opacity, &pixel, &mask[0],
&penum->mask[0], mem)) < 0 ||
(code = check_image3x_mask(pim, &pim->Shape, &pixel, &mask[1],
&penum->mask[1], mem)) < 0
) {
goto out0;
}
penum->num_components =
gs_color_space_num_components(pim->ColorSpace);
gx_image_enum_common_init((gx_image_enum_common_t *) penum,
(const gs_data_image_t *)pim,
&image3x_enum_procs, dev,
1 + penum->num_components,
pim->format);
penum->pixel.width = pixel.rect.q.x - pixel.rect.p.x;
penum->pixel.height = pixel.rect.q.y - pixel.rect.p.y;
penum->pixel.full_height = pim->Height;
penum->pixel.y = 0;
if (penum->mask[0].data || penum->mask[1].data) {
penum->pixel.data =
gs_alloc_bytes(mem,
(penum->pixel.width * pim->BitsPerComponent *
penum->num_components + 7) >> 3,
"gx_begin_image3x(pixel.data)");
if (penum->pixel.data == 0) {
code = gs_note_error(gs_error_VMerror);
goto out1;
}
}
penum->bpc = pim->BitsPerComponent;
penum->memory = mem;
if (pmat == 0)
pmat = &ctm_only(pis);
for (i = 0; i < NUM_MASKS; ++i) {
gs_rect mrect;
gx_device *mdev;
gs_color_space *pmcs;
if (penum->mask[i].depth == 0) {
midev[i] = 0;
minfo[i] = 0;
continue;
}
pmcs =  gs_alloc_struct(mem, gs_color_space, &st_color_space,
"gx_begin_image3x_generic");
if (pmcs == 0)
return_error(gs_error_VMerror);
gs_cspace_init_DevicePixel(mem, pmcs, penum->mask[i].depth);
mrect.p.x = mrect.p.y = 0;
mrect.q.x = penum->mask[i].width;
mrect.q.y = penum->mask[i].height;
if ((code = gs_matrix_multiply(&mask[i].matrix, pmat, &mat)) < 0 ||
(code = gs_bbox_transform(&mrect, &mat, &mrect)) < 0
)
return code;
origin[i].x = (int)floor(mrect.p.x);
origin[i].y = (int)floor(mrect.p.y);
code = make_mid(&mdev, dev,
(int)ceil(mrect.q.x) - origin[i].x,
(int)ceil(mrect.q.y) - origin[i].y,
penum->mask[i].depth, mem);
if (code < 0)
goto out1;
penum->mask[i].mdev = mdev;
gs_image_t_init(&mask[i].image, pmcs);
mask[i].image.ColorSpace = pmcs;
mask[i].image.adjust = false;
{
const gx_image_type_t *type1 = mask[i].image.type;
const gs_image3x_mask_t *pixm =
(i == 0 ? &pim->Opacity : &pim->Shape);
*(gs_data_image_t *)&mask[i].image = pixm->MaskDict;
mask[i].image.type = type1;
mask[i].image.BitsPerComponent = pixm->MaskDict.BitsPerComponent;
}
{
gs_matrix m_mat;
m_mat = *pmat;
m_mat.tx -= origin[i].x;
m_mat.ty -= origin[i].y;
code = gx_device_begin_typed_image(mdev, NULL, &m_mat,
(const gs_image_common_t *)&mask[i].image,
&mask[i].rect, NULL, NULL,
mem, &penum->mask[i].info);
if (code < 0)
goto out2;
}
midev[i] = mdev;
minfo[i] = penum->mask[i].info;
}
gs_image_t_init(&pixel.image, pim->ColorSpace);
{
const gx_image_type_t *type1 = pixel.image.type;
*(gs_pixel_image_t *)&pixel.image = *(const gs_pixel_image_t *)pim;
pixel.image.type = type1;
}
code = make_mcde(dev, pis, pmat, (const gs_image_common_t *)&pixel.image,
prect, pdcolor, pcpath, mem, &penum->pixel.info,
&pcdev, midev, minfo, origin, pim);
if (code < 0)
goto out3;
penum->pcdev = pcdev;
{
int added_depth = 0;
int pi = 0;
for (i = 0; i < NUM_MASKS; ++i) {
if (penum->mask[i].depth == 0)
continue;
switch (penum->mask[i].InterleaveType) {
case interleave_chunky:
added_depth += pim->BitsPerComponent;
break;
case interleave_separate_source:
penum->plane_widths[pi] = penum->mask[i].width;
penum->plane_depths[pi] = penum->mask[i].depth;
++pi;
break;
default:
code = gs_note_error(gs_error_Fatal);
goto out3;
}
}
memcpy(&penum->plane_widths[pi], &penum->pixel.info->plane_widths[0],
penum->pixel.info->num_planes * sizeof(penum->plane_widths[0]));
memcpy(&penum->plane_depths[pi], &penum->pixel.info->plane_depths[0],
penum->pixel.info->num_planes * sizeof(penum->plane_depths[0]));
penum->plane_depths[pi] += added_depth;
penum->num_planes = pi + penum->pixel.info->num_planes;
}
if (midev[0])
gx_device_retain(midev[0], true);
if (midev[1])
gx_device_retain(midev[1], true);
gx_device_retain(pcdev, true);
*pinfo = (gx_image_enum_common_t *) penum;
return 0;
out3:
if (penum->mask[1].info)
gx_image_end(penum->mask[1].info, false);
if (penum->mask[0].info)
gx_image_end(penum->mask[0].info, false);
out2:
if (penum->mask[1].mdev) {
gs_closedevice(penum->mask[1].mdev);
gs_free_object(mem, penum->mask[1].mdev,
"gx_begin_image3x(mask[1].mdev)");
}
if (penum->mask[0].mdev) {
gs_closedevice(penum->mask[0].mdev);
gs_free_object(mem, penum->mask[0].mdev,
"gx_begin_image3x(mask[0].mdev)");
}
out1:
gs_free_object(mem, penum->mask[0].data, "gx_begin_image3x(mask[0].data)");
gs_free_object(mem, penum->mask[1].data, "gx_begin_image3x(mask[1].data)");
gs_free_object(mem, penum->pixel.data, "gx_begin_image3x(pixel.data)");
out0:
gs_free_object(mem, penum, "gx_begin_image3x");
return code;
}
private bool
check_image3x_extent(floatp mask_coeff, floatp data_coeff)
{
if (mask_coeff == 0)
return data_coeff == 0;
if (data_coeff == 0 || (mask_coeff > 0) != (data_coeff > 0))
return false;
return true;
}
private bool
check_image3x_mask(const gs_image3x_t *pim, const gs_image3x_mask_t *pimm,
const image3x_channel_values_t *ppcv,
image3x_channel_values_t *pmcv,
image3x_channel_state_t *pmcs, gs_memory_t *mem)
{
int mask_width = pimm->MaskDict.Width, mask_height = pimm->MaskDict.Height;
int code;
if (pimm->MaskDict.BitsPerComponent == 0) {
pmcs->depth = 0;
pmcs->InterleaveType = 0;
return 0;
}
if (mask_height <= 0)
return_error(gs_error_rangecheck);
switch (pimm->InterleaveType) {
default:
return_error(gs_error_rangecheck);
case interleave_chunky:
if (mask_width != pim->Width ||
mask_height != pim->Height ||
pimm->MaskDict.BitsPerComponent != pim->BitsPerComponent ||
pim->format != gs_image_format_chunky
)
return_error(gs_error_rangecheck);
break;
case interleave_separate_source:
switch (pimm->MaskDict.BitsPerComponent) {
case 1: case 2: case 4: case 8:
break;
default:
return_error(gs_error_rangecheck);
}
}
if (!check_image3x_extent(pim->ImageMatrix.xx,
pimm->MaskDict.ImageMatrix.xx) ||
!check_image3x_extent(pim->ImageMatrix.xy,
pimm->MaskDict.ImageMatrix.xy) ||
!check_image3x_extent(pim->ImageMatrix.yx,
pimm->MaskDict.ImageMatrix.yx) ||
!check_image3x_extent(pim->ImageMatrix.yy,
pimm->MaskDict.ImageMatrix.yy)
)
return_error(gs_error_rangecheck);
if ((code = gs_matrix_invert(&pimm->MaskDict.ImageMatrix, &pmcv->matrix)) < 0 ||
(code = gs_point_transform(mask_width, mask_height,
&pmcv->matrix, &pmcv->corner)) < 0
)
return code;
if (fabs(ppcv->matrix.tx - pmcv->matrix.tx) >= 0.5 ||
fabs(ppcv->matrix.ty - pmcv->matrix.ty) >= 0.5 ||
fabs(ppcv->corner.x - pmcv->corner.x) >= 0.5 ||
fabs(ppcv->corner.y - pmcv->corner.y) >= 0.5
)
return_error(gs_error_rangecheck);
pmcv->rect.p.x = ppcv->rect.p.x * mask_width / pim->Width;
pmcv->rect.p.y = ppcv->rect.p.y * mask_height / pim->Height;
pmcv->rect.q.x = (ppcv->rect.q.x * mask_width + pim->Width - 1) /
pim->Width;
pmcv->rect.q.y = (ppcv->rect.q.y * mask_height + pim->Height - 1) /
pim->Height;
pmcs->InterleaveType = pimm->InterleaveType;
pmcs->width = pmcv->rect.q.x - pmcv->rect.p.x;
pmcs->height = pmcv->rect.q.y - pmcv->rect.p.y;
pmcs->full_height = pimm->MaskDict.Height;
pmcs->depth = pimm->MaskDict.BitsPerComponent;
if (pmcs->InterleaveType == interleave_chunky) {
pmcs->data =
gs_alloc_bytes(mem,
(pmcs->width * pimm->MaskDict.BitsPerComponent + 7) >> 3,
"gx_begin_image3x(mask data)");
if (pmcs->data == 0)
return_error(gs_error_VMerror);
}
pmcs->y = pmcs->skip = 0;
return 0;
}
private int
channel_next(const image3x_channel_state_t *pics1,
const image3x_channel_state_t *pics2)
{
int h1 = pics1->full_height;
int h2 = pics2->full_height;
long current = pics1->y * (long)h2 - pics2->y * (long)h1;
#ifdef DEBUG
if (current < 0)
lprintf4("channel_next invariant fails: %d/%d < %d/%d\n",
pics1->y, pics1->full_height,
pics2->y, pics2->full_height);
#endif
return ((current -= h1) >= 0 ? -1 :
current + h2 >= 0 ? 0 : 1);
}
private IMAGE3X_MAKE_MID_PROC(make_midx_default);
private int
make_midx_default(gx_device **pmidev, gx_device *dev, int width, int height,
int depth, gs_memory_t *mem)
{
const gx_device_memory *mdproto = gdev_mem_device_for_bits(depth);
gx_device_memory *midev;
int code;
if (mdproto == 0)
return_error(gs_error_rangecheck);
midev = gs_alloc_struct(mem, gx_device_memory, &st_device_memory,
"make_mid_default");
if (midev == 0)
return_error(gs_error_VMerror);
gs_make_mem_device(midev, mdproto, mem, 0, NULL);
midev->bitmap_memory = mem;
midev->width = width;
midev->height = height;
check_device_separable((gx_device *)midev);
gx_device_fill_in_procs((gx_device *)midev);
code = dev_proc(midev, open_device)((gx_device *)midev);
if (code < 0) {
gs_free_object(mem, midev, "make_midx_default");
return code;
}
midev->is_open = true;
dev_proc(midev, fill_rectangle)
((gx_device *)midev, 0, 0, width, height, (gx_color_index)0);
*pmidev = (gx_device *)midev;
return 0;
}
private IMAGE3X_MAKE_MCDE_PROC(make_mcdex_default);
private int
make_mcdex_default(gx_device *dev, const gs_imager_state *pis,
const gs_matrix *pmat, const gs_image_common_t *pic,
const gs_int_rect *prect, const gx_drawing_color *pdcolor,
const gx_clip_path *pcpath, gs_memory_t *mem,
gx_image_enum_common_t **pinfo,
gx_device **pmcdev, gx_device *midev[2],
gx_image_enum_common_t *pminfo[2],
const gs_int_point origin[2],
const gs_image3x_t *pim)
{
gx_device_bbox *bbdev =
gs_alloc_struct_immovable(mem, gx_device_bbox, &st_device_bbox,
"make_mcdex_default");
int code;
if (bbdev == 0)
return_error(gs_error_VMerror);
gx_device_bbox_init(bbdev, dev, mem);
gx_device_bbox_fwd_open_close(bbdev, false);
code = dev_proc(bbdev, begin_typed_image)
((gx_device *)bbdev, pis, pmat, pic, prect, pdcolor, pcpath, mem,
pinfo);
if (code < 0) {
gs_free_object(mem, bbdev, "make_mcdex_default");
return code;
}
*pmcdev = (gx_device *)bbdev;
return 0;
}
private int
gx_begin_image3x(gx_device * dev,
const gs_imager_state * pis, const gs_matrix * pmat,
const gs_image_common_t * pic, const gs_int_rect * prect,
const gx_drawing_color * pdcolor, const gx_clip_path * pcpath,
gs_memory_t * mem, gx_image_enum_common_t ** pinfo)
{
return gx_begin_image3x_generic(dev, pis, pmat, pic, prect, pdcolor,
pcpath, mem, make_midx_default,
make_mcdex_default, pinfo);
}
private int
gx_image3x_plane_data(gx_image_enum_common_t * info,
const gx_image_plane_t * planes, int height,
int *rows_used)
{
gx_image3x_enum_t *penum = (gx_image3x_enum_t *) info;
int pixel_height = penum->pixel.height;
int pixel_used = 0;
int mask_height[2];
int mask_used[2];
int h1 = pixel_height - penum->pixel.y;
int h;
const gx_image_plane_t *pixel_planes;
gx_image_plane_t pixel_plane, mask_plane[2];
int code = 0;
int i, pi = 0;
int num_chunky = 0;
for (i = 0; i < NUM_MASKS; ++i) {
int mh = mask_height[i] = penum->mask[i].height;
mask_plane[i].data = 0;
mask_used[i] = 0;
if (!penum->mask[i].depth)
continue;
h1 = min(h1, mh - penum->mask[i].y);
if (penum->mask[i].InterleaveType == interleave_chunky)
++num_chunky;
}
h = min(height, h1);
*rows_used = 0;
if (h <= 0)
return 0;
for (i = 0; i < NUM_MASKS; ++i)
if (penum->mask[i].InterleaveType == interleave_separate_source) {
if (h > 1)
h = 1;
mask_plane[i] = planes[pi++];
}
pixel_planes = &planes[pi];
if (num_chunky) {
int bpc = penum->bpc;
int num_components = penum->num_components;
int width = penum->pixel.width;
uint bit_x = bpc * (num_components + num_chunky) * planes[pi].data_x;
sample_load_declare_setup(sptr, sbit, planes[0].data + (bit_x >> 3),
bit_x & 7, bpc);
sample_store_declare_setup(pptr, pbit, pbbyte,
penum->pixel.data, 0, bpc);
sample_store_declare(dptr[NUM_MASKS], dbit[NUM_MASKS],
dbbyte[NUM_MASKS]);
int depth[NUM_MASKS];
int x;
if (h > 1) {
h = 1;
}
for (i = 0; i < NUM_MASKS; ++i)
if (penum->mask[i].data) {
depth[i] = penum->mask[i].depth;
mask_plane[i].data = dptr[i] = penum->mask[i].data;
mask_plane[i].data_x = 0;
sample_store_setup(dbit[i], 0, depth[i]);
sample_store_preload(dbbyte[i], dptr[i], 0, depth[i]);
} else
depth[i] = 0;
pixel_plane.data = pptr;
pixel_plane.data_x = 0;
pixel_planes = &pixel_plane;
for (x = 0; x < width; ++x) {
uint value;
for (i = 0; i < NUM_MASKS; ++i)
if (depth[i]) {
sample_load_next12(value, sptr, sbit, bpc);
sample_store_next12(value, dptr[i], dbit[i], depth[i],
dbbyte[i]);
}
for (i = 0; i < num_components; ++i) {
sample_load_next12(value, sptr, sbit, bpc);
sample_store_next12(value, pptr, pbit, bpc, pbbyte);
}
}
for (i = 0; i < NUM_MASKS; ++i)
if (penum->mask[i].data)
sample_store_flush(dptr[i], dbit[i], depth[i], dbbyte[i]);
sample_store_flush(pptr, pbit, bpc, pbbyte);
}
for (i = 0; i < NUM_MASKS; ++i)
if (mask_plane[i].data) {
int skip = penum->mask[i].skip;
if (skip >= h) {
penum->mask[i].skip = skip - (mask_used[i] = h);
} else {
int mask_h = h - skip;
mask_plane[i].data += skip * mask_plane[i].raster;
penum->mask[i].skip = 0;
code = gx_image_plane_data_rows(penum->mask[i].info,
&mask_plane[i],
mask_h, &mask_used[i]);
mask_used[i] += skip;
}
*rows_used = mask_used[i];
penum->mask[i].y += mask_used[i];
if (code < 0)
return code;
}
if (pixel_planes[0].data) {
for (i = 0; i < NUM_MASKS; ++i)
if (penum->mask[i].info)
gx_image_flush(penum->mask[i].info);
code = gx_image_plane_data_rows(penum->pixel.info, pixel_planes, h,
&pixel_used);
*rows_used = pixel_used;
penum->pixel.y += pixel_used;
if (code < 0) {
for (i = 0; i < NUM_MASKS; ++i)
if (mask_used[i] > pixel_used) {
int skip = mask_used[i] - pixel_used;
penum->mask[i].skip = skip;
penum->mask[i].y -= skip;
mask_used[i] = pixel_used;
}
}
}
if_debug7('b', "[b]image3x h=%d %sopacity.y=%d %sopacity.y=%d %spixel.y=%d\n",
h, (mask_plane[0].data ? "+" : ""), penum->mask[0].y,
(mask_plane[1].data ? "+" : ""), penum->mask[1].y,
(pixel_planes[0].data ? "+" : ""), penum->pixel.y);
if (penum->mask[0].y >= penum->mask[0].height &&
penum->mask[1].y >= penum->mask[1].height &&
penum->pixel.y >= penum->pixel.height)
return 1;
return (code < 0 ? code : 0);
}
private int
gx_image3x_flush(gx_image_enum_common_t * info)
{
gx_image3x_enum_t * const penum = (gx_image3x_enum_t *) info;
int code = gx_image_flush(penum->mask[0].info);
if (code >= 0)
code = gx_image_flush(penum->mask[1].info);
if (code >= 0)
code = gx_image_flush(penum->pixel.info);
return code;
}
private bool
gx_image3x_planes_wanted(const gx_image_enum_common_t * info, byte *wanted)
{
const gx_image3x_enum_t * const penum = (const gx_image3x_enum_t *) info;
bool
sso = penum->mask[0].InterleaveType == interleave_separate_source,
sss = penum->mask[1].InterleaveType == interleave_separate_source;
if (sso & sss) {
int mask_next = channel_next(&penum->mask[1], &penum->pixel);
memset(wanted + 2, (mask_next <= 0 ? 0xff : 0), info->num_planes - 2);
wanted[1] = (mask_next >= 0 ? 0xff : 0);
if (wanted[1]) {
mask_next = channel_next(&penum->mask[0], &penum->mask[1]);
wanted[0] = mask_next >= 0;
} else
wanted[0] = 0;
return false;
} else if (sso | sss) {
const image3x_channel_state_t *pics =
(sso ? &penum->mask[0] : &penum->mask[1]);
int mask_next = channel_next(pics, &penum->pixel);
wanted[0] = (mask_next >= 0 ? 0xff : 0);
memset(wanted + 1, (mask_next <= 0 ? 0xff : 0), info->num_planes - 1);
return false
;
} else {
wanted[0] = 0xff;
return true;
}
}
private int
gx_image3x_end_image(gx_image_enum_common_t * info, bool draw_last)
{
gx_image3x_enum_t *penum = (gx_image3x_enum_t *) info;
gs_memory_t *mem = penum->memory;
gx_device *mdev0 = penum->mask[0].mdev;
int ocode =
(penum->mask[0].info ? gx_image_end(penum->mask[0].info, draw_last) :
0);
gx_device *mdev1 = penum->mask[1].mdev;
int scode =
(penum->mask[1].info ? gx_image_end(penum->mask[1].info, draw_last) :
0);
gx_device *pcdev = penum->pcdev;
int pcode = gx_image_end(penum->pixel.info, draw_last);
gs_closedevice(pcdev);
if (mdev0)
gs_closedevice(mdev0);
if (mdev1)
gs_closedevice(mdev1);
gs_free_object(mem, penum->mask[0].data,
"gx_image3x_end_image(mask[0].data)");
gs_free_object(mem, penum->mask[1].data,
"gx_image3x_end_image(mask[1].data)");
gs_free_object(mem, penum->pixel.data,
"gx_image3x_end_image(pixel.data)");
gs_free_object(mem, pcdev, "gx_image3x_end_image(pcdev)");
gs_free_object(mem, mdev0, "gx_image3x_end_image(mask[0].mdev)");
gs_free_object(mem, mdev1, "gx_image3x_end_image(mask[1].mdev)");
gs_free_object(mem, penum, "gx_image3x_end_image");
return (pcode < 0 ? pcode : scode < 0 ? scode : ocode);
}