#define JPEG_INTERNALS
#include "jinclude.h"
#include "jpeglib.h"
#include "transupp.h"
#if TRANSFORMS_SUPPORTED
LOCAL(void)
do_flip_h (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays)
{
JDIMENSION MCU_cols, comp_width, blk_x, blk_y;
int ci, k, offset_y;
JBLOCKARRAY buffer;
JCOEFPTR ptr1, ptr2;
JCOEF temp1, temp2;
jpeg_component_info *compptr;
MCU_cols = dstinfo->image_width / (dstinfo->max_h_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_width = MCU_cols * compptr->h_samp_factor;
for (blk_y = 0; blk_y < compptr->height_in_blocks;
blk_y += compptr->v_samp_factor) {
buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
for (blk_x = 0; blk_x * 2 < comp_width; blk_x++) {
ptr1 = buffer[offset_y][blk_x];
ptr2 = buffer[offset_y][comp_width - blk_x - 1];
for (k = 0; k < DCTSIZE2; k += 2) {
temp1 = *ptr1;
temp2 = *ptr2;
*ptr1++ = temp2;
*ptr2++ = temp1;
temp1 = *ptr1;
temp2 = *ptr2;
*ptr1++ = -temp2;
*ptr2++ = -temp1;
}
}
}
}
}
}
LOCAL(void)
do_flip_v (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION MCU_rows, comp_height, dst_blk_x, dst_blk_y;
int ci, i, j, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JBLOCKROW src_row_ptr, dst_row_ptr;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
MCU_rows = dstinfo->image_height / (dstinfo->max_v_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_height = MCU_rows * compptr->v_samp_factor;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
if (dst_blk_y < comp_height) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci],
comp_height - dst_blk_y - (JDIMENSION) compptr->v_samp_factor,
(JDIMENSION) compptr->v_samp_factor, FALSE);
} else {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, FALSE);
}
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
if (dst_blk_y < comp_height) {
dst_row_ptr = dst_buffer[offset_y];
src_row_ptr = src_buffer[compptr->v_samp_factor - offset_y - 1];
for (dst_blk_x = 0; dst_blk_x < compptr->width_in_blocks;
dst_blk_x++) {
dst_ptr = dst_row_ptr[dst_blk_x];
src_ptr = src_row_ptr[dst_blk_x];
for (i = 0; i < DCTSIZE; i += 2) {
for (j = 0; j < DCTSIZE; j++)
*dst_ptr++ = *src_ptr++;
for (j = 0; j < DCTSIZE; j++)
*dst_ptr++ = - *src_ptr++;
}
}
} else {
jcopy_block_row(src_buffer[offset_y], dst_buffer[offset_y],
compptr->width_in_blocks);
}
}
}
}
}
LOCAL(void)
do_transpose (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION dst_blk_x, dst_blk_y;
int ci, i, j, offset_x, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
for (dst_blk_x = 0; dst_blk_x < compptr->width_in_blocks;
dst_blk_x += compptr->h_samp_factor) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_x,
(JDIMENSION) compptr->h_samp_factor, FALSE);
for (offset_x = 0; offset_x < compptr->h_samp_factor; offset_x++) {
src_ptr = src_buffer[offset_x][dst_blk_y + offset_y];
dst_ptr = dst_buffer[offset_y][dst_blk_x + offset_x];
for (i = 0; i < DCTSIZE; i++)
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
}
}
}
}
}
}
LOCAL(void)
do_rot_90 (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION MCU_cols, comp_width, dst_blk_x, dst_blk_y;
int ci, i, j, offset_x, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
MCU_cols = dstinfo->image_width / (dstinfo->max_h_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_width = MCU_cols * compptr->h_samp_factor;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
for (dst_blk_x = 0; dst_blk_x < compptr->width_in_blocks;
dst_blk_x += compptr->h_samp_factor) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_x,
(JDIMENSION) compptr->h_samp_factor, FALSE);
for (offset_x = 0; offset_x < compptr->h_samp_factor; offset_x++) {
src_ptr = src_buffer[offset_x][dst_blk_y + offset_y];
if (dst_blk_x < comp_width) {
dst_ptr = dst_buffer[offset_y]
[comp_width - dst_blk_x - offset_x - 1];
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
i++;
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
}
} else {
dst_ptr = dst_buffer[offset_y][dst_blk_x + offset_x];
for (i = 0; i < DCTSIZE; i++)
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
}
}
}
}
}
}
}
LOCAL(void)
do_rot_270 (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION MCU_rows, comp_height, dst_blk_x, dst_blk_y;
int ci, i, j, offset_x, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
MCU_rows = dstinfo->image_height / (dstinfo->max_v_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_height = MCU_rows * compptr->v_samp_factor;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
for (dst_blk_x = 0; dst_blk_x < compptr->width_in_blocks;
dst_blk_x += compptr->h_samp_factor) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_x,
(JDIMENSION) compptr->h_samp_factor, FALSE);
for (offset_x = 0; offset_x < compptr->h_samp_factor; offset_x++) {
dst_ptr = dst_buffer[offset_y][dst_blk_x + offset_x];
if (dst_blk_y < comp_height) {
src_ptr = src_buffer[offset_x]
[comp_height - dst_blk_y - offset_y - 1];
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < DCTSIZE; j++) {
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
j++;
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
}
}
} else {
src_ptr = src_buffer[offset_x][dst_blk_y + offset_y];
for (i = 0; i < DCTSIZE; i++)
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
}
}
}
}
}
}
}
LOCAL(void)
do_rot_180 (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION MCU_cols, MCU_rows, comp_width, comp_height, dst_blk_x, dst_blk_y;
int ci, i, j, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JBLOCKROW src_row_ptr, dst_row_ptr;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
MCU_cols = dstinfo->image_width / (dstinfo->max_h_samp_factor * DCTSIZE);
MCU_rows = dstinfo->image_height / (dstinfo->max_v_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_width = MCU_cols * compptr->h_samp_factor;
comp_height = MCU_rows * compptr->v_samp_factor;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
if (dst_blk_y < comp_height) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci],
comp_height - dst_blk_y - (JDIMENSION) compptr->v_samp_factor,
(JDIMENSION) compptr->v_samp_factor, FALSE);
} else {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, FALSE);
}
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
if (dst_blk_y < comp_height) {
dst_row_ptr = dst_buffer[offset_y];
src_row_ptr = src_buffer[compptr->v_samp_factor - offset_y - 1];
for (dst_blk_x = 0; dst_blk_x < comp_width; dst_blk_x++) {
dst_ptr = dst_row_ptr[dst_blk_x];
src_ptr = src_row_ptr[comp_width - dst_blk_x - 1];
for (i = 0; i < DCTSIZE; i += 2) {
for (j = 0; j < DCTSIZE; j += 2) {
*dst_ptr++ = *src_ptr++;
*dst_ptr++ = - *src_ptr++;
}
for (j = 0; j < DCTSIZE; j += 2) {
*dst_ptr++ = - *src_ptr++;
*dst_ptr++ = *src_ptr++;
}
}
}
for (; dst_blk_x < compptr->width_in_blocks; dst_blk_x++) {
dst_ptr = dst_row_ptr[dst_blk_x];
src_ptr = src_row_ptr[dst_blk_x];
for (i = 0; i < DCTSIZE; i += 2) {
for (j = 0; j < DCTSIZE; j++)
*dst_ptr++ = *src_ptr++;
for (j = 0; j < DCTSIZE; j++)
*dst_ptr++ = - *src_ptr++;
}
}
} else {
dst_row_ptr = dst_buffer[offset_y];
src_row_ptr = src_buffer[offset_y];
for (dst_blk_x = 0; dst_blk_x < comp_width; dst_blk_x++) {
dst_ptr = dst_row_ptr[dst_blk_x];
src_ptr = src_row_ptr[comp_width - dst_blk_x - 1];
for (i = 0; i < DCTSIZE2; i += 2) {
*dst_ptr++ = *src_ptr++;
*dst_ptr++ = - *src_ptr++;
}
}
for (; dst_blk_x < compptr->width_in_blocks; dst_blk_x++) {
dst_ptr = dst_row_ptr[dst_blk_x];
src_ptr = src_row_ptr[dst_blk_x];
for (i = 0; i < DCTSIZE2; i++)
*dst_ptr++ = *src_ptr++;
}
}
}
}
}
}
LOCAL(void)
do_transverse (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jvirt_barray_ptr *dst_coef_arrays)
{
JDIMENSION MCU_cols, MCU_rows, comp_width, comp_height, dst_blk_x, dst_blk_y;
int ci, i, j, offset_x, offset_y;
JBLOCKARRAY src_buffer, dst_buffer;
JCOEFPTR src_ptr, dst_ptr;
jpeg_component_info *compptr;
MCU_cols = dstinfo->image_width / (dstinfo->max_h_samp_factor * DCTSIZE);
MCU_rows = dstinfo->image_height / (dstinfo->max_v_samp_factor * DCTSIZE);
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
comp_width = MCU_cols * compptr->h_samp_factor;
comp_height = MCU_rows * compptr->v_samp_factor;
for (dst_blk_y = 0; dst_blk_y < compptr->height_in_blocks;
dst_blk_y += compptr->v_samp_factor) {
dst_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, dst_coef_arrays[ci], dst_blk_y,
(JDIMENSION) compptr->v_samp_factor, TRUE);
for (offset_y = 0; offset_y < compptr->v_samp_factor; offset_y++) {
for (dst_blk_x = 0; dst_blk_x < compptr->width_in_blocks;
dst_blk_x += compptr->h_samp_factor) {
src_buffer = (*srcinfo->mem->access_virt_barray)
((j_common_ptr) srcinfo, src_coef_arrays[ci], dst_blk_x,
(JDIMENSION) compptr->h_samp_factor, FALSE);
for (offset_x = 0; offset_x < compptr->h_samp_factor; offset_x++) {
if (dst_blk_y < comp_height) {
src_ptr = src_buffer[offset_x]
[comp_height - dst_blk_y - offset_y - 1];
if (dst_blk_x < comp_width) {
dst_ptr = dst_buffer[offset_y]
[comp_width - dst_blk_x - offset_x - 1];
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < DCTSIZE; j++) {
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
j++;
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
}
i++;
for (j = 0; j < DCTSIZE; j++) {
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
j++;
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
}
}
} else {
dst_ptr = dst_buffer[offset_y][dst_blk_x + offset_x];
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < DCTSIZE; j++) {
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
j++;
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
}
}
}
} else {
src_ptr = src_buffer[offset_x][dst_blk_y + offset_y];
if (dst_blk_x < comp_width) {
dst_ptr = dst_buffer[offset_y]
[comp_width - dst_blk_x - offset_x - 1];
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
i++;
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = -src_ptr[i*DCTSIZE+j];
}
} else {
dst_ptr = dst_buffer[offset_y][dst_blk_x + offset_x];
for (i = 0; i < DCTSIZE; i++)
for (j = 0; j < DCTSIZE; j++)
dst_ptr[j*DCTSIZE+i] = src_ptr[i*DCTSIZE+j];
}
}
}
}
}
}
}
}
GLOBAL(void)
jtransform_request_workspace (j_decompress_ptr srcinfo,
jpeg_transform_info *info)
{
jvirt_barray_ptr *coef_arrays = NULL;
jpeg_component_info *compptr;
int ci;
if (info->force_grayscale &&
srcinfo->jpeg_color_space == JCS_YCbCr &&
srcinfo->num_components == 3) {
info->num_components = 1;
} else {
info->num_components = srcinfo->num_components;
}
switch (info->transform) {
case JXFORM_NONE:
case JXFORM_FLIP_H:
break;
case JXFORM_FLIP_V:
case JXFORM_ROT_180:
coef_arrays = (jvirt_barray_ptr *)
(*srcinfo->mem->alloc_small) ((j_common_ptr) srcinfo, JPOOL_IMAGE,
SIZEOF(jvirt_barray_ptr) * info->num_components);
for (ci = 0; ci < info->num_components; ci++) {
compptr = srcinfo->comp_info + ci;
coef_arrays[ci] = (*srcinfo->mem->request_virt_barray)
((j_common_ptr) srcinfo, JPOOL_IMAGE, FALSE,
(JDIMENSION) jround_up((long) compptr->width_in_blocks,
(long) compptr->h_samp_factor),
(JDIMENSION) jround_up((long) compptr->height_in_blocks,
(long) compptr->v_samp_factor),
(JDIMENSION) compptr->v_samp_factor);
}
break;
case JXFORM_TRANSPOSE:
case JXFORM_TRANSVERSE:
case JXFORM_ROT_90:
case JXFORM_ROT_270:
coef_arrays = (jvirt_barray_ptr *)
(*srcinfo->mem->alloc_small) ((j_common_ptr) srcinfo, JPOOL_IMAGE,
SIZEOF(jvirt_barray_ptr) * info->num_components);
for (ci = 0; ci < info->num_components; ci++) {
compptr = srcinfo->comp_info + ci;
coef_arrays[ci] = (*srcinfo->mem->request_virt_barray)
((j_common_ptr) srcinfo, JPOOL_IMAGE, FALSE,
(JDIMENSION) jround_up((long) compptr->height_in_blocks,
(long) compptr->v_samp_factor),
(JDIMENSION) jround_up((long) compptr->width_in_blocks,
(long) compptr->h_samp_factor),
(JDIMENSION) compptr->h_samp_factor);
}
break;
}
info->workspace_coef_arrays = coef_arrays;
}
LOCAL(void)
transpose_critical_parameters (j_compress_ptr dstinfo)
{
int tblno, i, j, ci, itemp;
jpeg_component_info *compptr;
JQUANT_TBL *qtblptr;
JDIMENSION dtemp;
UINT16 qtemp;
dtemp = dstinfo->image_width;
dstinfo->image_width = dstinfo->image_height;
dstinfo->image_height = dtemp;
for (ci = 0; ci < dstinfo->num_components; ci++) {
compptr = dstinfo->comp_info + ci;
itemp = compptr->h_samp_factor;
compptr->h_samp_factor = compptr->v_samp_factor;
compptr->v_samp_factor = itemp;
}
for (tblno = 0; tblno < NUM_QUANT_TBLS; tblno++) {
qtblptr = dstinfo->quant_tbl_ptrs[tblno];
if (qtblptr != NULL) {
for (i = 0; i < DCTSIZE; i++) {
for (j = 0; j < i; j++) {
qtemp = qtblptr->quantval[i*DCTSIZE+j];
qtblptr->quantval[i*DCTSIZE+j] = qtblptr->quantval[j*DCTSIZE+i];
qtblptr->quantval[j*DCTSIZE+i] = qtemp;
}
}
}
}
}
LOCAL(void)
trim_right_edge (j_compress_ptr dstinfo)
{
int ci, max_h_samp_factor;
JDIMENSION MCU_cols;
max_h_samp_factor = 1;
for (ci = 0; ci < dstinfo->num_components; ci++) {
int h_samp_factor = dstinfo->comp_info[ci].h_samp_factor;
max_h_samp_factor = MAX(max_h_samp_factor, h_samp_factor);
}
MCU_cols = dstinfo->image_width / (max_h_samp_factor * DCTSIZE);
if (MCU_cols > 0)
dstinfo->image_width = MCU_cols * (max_h_samp_factor * DCTSIZE);
}
LOCAL(void)
trim_bottom_edge (j_compress_ptr dstinfo)
{
int ci, max_v_samp_factor;
JDIMENSION MCU_rows;
max_v_samp_factor = 1;
for (ci = 0; ci < dstinfo->num_components; ci++) {
int v_samp_factor = dstinfo->comp_info[ci].v_samp_factor;
max_v_samp_factor = MAX(max_v_samp_factor, v_samp_factor);
}
MCU_rows = dstinfo->image_height / (max_v_samp_factor * DCTSIZE);
if (MCU_rows > 0)
dstinfo->image_height = MCU_rows * (max_v_samp_factor * DCTSIZE);
}
GLOBAL(jvirt_barray_ptr *)
jtransform_adjust_parameters (j_decompress_ptr srcinfo,
j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jpeg_transform_info *info)
{
if (info->force_grayscale) {
if ((dstinfo->jpeg_color_space == JCS_YCbCr &&
dstinfo->num_components == 3) ||
(dstinfo->jpeg_color_space == JCS_GRAYSCALE &&
dstinfo->num_components == 1)) {
int sv_quant_tbl_no = dstinfo->comp_info[0].quant_tbl_no;
jpeg_set_colorspace(dstinfo, JCS_GRAYSCALE);
dstinfo->comp_info[0].quant_tbl_no = sv_quant_tbl_no;
} else {
ERREXIT(dstinfo, JERR_CONVERSION_NOTIMPL);
}
}
switch (info->transform) {
case JXFORM_NONE:
break;
case JXFORM_FLIP_H:
if (info->trim)
trim_right_edge(dstinfo);
break;
case JXFORM_FLIP_V:
if (info->trim)
trim_bottom_edge(dstinfo);
break;
case JXFORM_TRANSPOSE:
transpose_critical_parameters(dstinfo);
break;
case JXFORM_TRANSVERSE:
transpose_critical_parameters(dstinfo);
if (info->trim) {
trim_right_edge(dstinfo);
trim_bottom_edge(dstinfo);
}
break;
case JXFORM_ROT_90:
transpose_critical_parameters(dstinfo);
if (info->trim)
trim_right_edge(dstinfo);
break;
case JXFORM_ROT_180:
if (info->trim) {
trim_right_edge(dstinfo);
trim_bottom_edge(dstinfo);
}
break;
case JXFORM_ROT_270:
transpose_critical_parameters(dstinfo);
if (info->trim)
trim_bottom_edge(dstinfo);
break;
}
if (info->workspace_coef_arrays != NULL)
return info->workspace_coef_arrays;
return src_coef_arrays;
}
GLOBAL(void)
jtransform_execute_transformation (j_decompress_ptr srcinfo,
j_compress_ptr dstinfo,
jvirt_barray_ptr *src_coef_arrays,
jpeg_transform_info *info)
{
jvirt_barray_ptr *dst_coef_arrays = info->workspace_coef_arrays;
switch (info->transform) {
case JXFORM_NONE:
break;
case JXFORM_FLIP_H:
do_flip_h(srcinfo, dstinfo, src_coef_arrays);
break;
case JXFORM_FLIP_V:
do_flip_v(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
case JXFORM_TRANSPOSE:
do_transpose(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
case JXFORM_TRANSVERSE:
do_transverse(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
case JXFORM_ROT_90:
do_rot_90(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
case JXFORM_ROT_180:
do_rot_180(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
case JXFORM_ROT_270:
do_rot_270(srcinfo, dstinfo, src_coef_arrays, dst_coef_arrays);
break;
}
}
#endif
GLOBAL(void)
jcopy_markers_setup (j_decompress_ptr srcinfo, JCOPY_OPTION option)
{
#ifdef SAVE_MARKERS_SUPPORTED
int m;
if (option != JCOPYOPT_NONE) {
jpeg_save_markers(srcinfo, JPEG_COM, 0xFFFF);
}
if (option == JCOPYOPT_ALL) {
for (m = 0; m < 16; m++)
jpeg_save_markers(srcinfo, JPEG_APP0 + m, 0xFFFF);
}
#endif
}
GLOBAL(void)
jcopy_markers_execute (j_decompress_ptr srcinfo, j_compress_ptr dstinfo,
JCOPY_OPTION option)
{
jpeg_saved_marker_ptr marker;
for (marker = srcinfo->marker_list; marker != NULL; marker = marker->next) {
if (dstinfo->write_JFIF_header &&
marker->marker == JPEG_APP0 &&
marker->data_length >= 5 &&
GETJOCTET(marker->data[0]) == 0x4A &&
GETJOCTET(marker->data[1]) == 0x46 &&
GETJOCTET(marker->data[2]) == 0x49 &&
GETJOCTET(marker->data[3]) == 0x46 &&
GETJOCTET(marker->data[4]) == 0)
continue;
if (dstinfo->write_Adobe_marker &&
marker->marker == JPEG_APP0+14 &&
marker->data_length >= 5 &&
GETJOCTET(marker->data[0]) == 0x41 &&
GETJOCTET(marker->data[1]) == 0x64 &&
GETJOCTET(marker->data[2]) == 0x6F &&
GETJOCTET(marker->data[3]) == 0x62 &&
GETJOCTET(marker->data[4]) == 0x65)
continue;
#ifdef NEED_FAR_POINTERS
{
unsigned int i;
jpeg_write_m_header(dstinfo, marker->marker, marker->data_length);
for (i = 0; i < marker->data_length; i++)
jpeg_write_m_byte(dstinfo, marker->data[i]);
}
#else
jpeg_write_marker(dstinfo, marker->marker,
marker->data, marker->data_length);
#endif
}
}