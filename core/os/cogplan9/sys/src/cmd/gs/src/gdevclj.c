#include "math_.h"
#include "gx.h"
#include "gsparam.h"
#include "gdevprn.h"
#include "gdevpcl.h"
typedef struct gx_device_clj_s gx_device_clj;
struct gx_device_clj_s {
gx_device_common;
gx_prn_device_common;
bool rotated;
};
#define pclj ((gx_device_clj *)pdev)
#define USE_FAST_MODE
#define X_DPI 300
#define Y_DPI 300
typedef struct clj_paper_size_s {
uint        tag;
int         orient;
float       width, height;
gs_point    offsets;
} clj_paper_size;
private const clj_paper_size    clj_paper_sizes[] = {
{   2,  1, 11.00 * 72.0, 8.50 * 72.0, { .200 * 72.0, 0.0 } },
{   1,  1, 10.50 * 72.0, 7.25 * 72.0, { .200 * 72.0, 0.0 } },
{  26,  1, 11.69 * 72.0, 8.27 * 72.0, { .197 * 72.0, 0.0 } }
};
private const float supported_resolutions[] = { 75.0, 100.0, 150.0, 300.0 };
#define CLJ_MAX_RES        300.0
#define CLJ_MAX_SCANLINE   (12.0 * 72.0)
private bool
is_supported_resolution(
const float HWResolution[2]
)
{
int     i;
for (i = 0; i < countof(supported_resolutions); i++) {
if (HWResolution[0] == supported_resolutions[i])
return HWResolution[0] == HWResolution[1];
}
return false;
}
private const clj_paper_size *
get_paper_size(
const float             MediaSize[2],
bool *                  rotatep
)
{
static const float      tolerance = 5.0;
float                   width = MediaSize[0];
float                   height = MediaSize[1];
const clj_paper_size *  psize = 0;
int                     i;
for (i = 0, psize = clj_paper_sizes; i < countof(clj_paper_sizes); i++, psize++) {
if ( (fabs(width - psize->width) <= tolerance)  &&
(fabs(height - psize->height) <= tolerance)  ) {
if (rotatep != 0)
*rotatep = false;
return psize;
} else if ( (fabs(width - psize->height) <= tolerance) &&
(fabs(height - psize->width) <= tolerance)   ) {
if (rotatep != 0)
*rotatep = true;
return psize;
}
}
return 0;
}
private void
clj_get_initial_matrix( gx_device *pdev, gs_matrix *pmat)
{
floatp      	fs_res = pdev->HWResolution[0] / 72.0;
floatp      	ss_res = pdev->HWResolution[1] / 72.0;
const clj_paper_size *psize;
psize = get_paper_size(pdev->MediaSize, NULL);
if (psize == 0) {
pmat->xx = fs_res;
pmat->xy = 0.0;
pmat->yx = 0.0;
pmat->yy = -ss_res;
pmat->tx = 0.0;
pmat->ty = pdev->MediaSize[1] * ss_res;
return;
}
if (pclj->rotated) {
pmat->xx = 0.0;
pmat->xy = ss_res;
pmat->yx = fs_res;
pmat->yy = 0.0;
pmat->tx = -psize->offsets.x * fs_res;
pmat->ty = -psize->offsets.y * ss_res;
} else {
pmat->xx = fs_res;
pmat->xy = 0.0;
pmat->yx = 0.0;
pmat->yy = -ss_res;
pmat->tx = -psize->offsets.x * fs_res;
pmat->ty = pdev->height + psize->offsets.y * ss_res;
}
}
private int
clj_get_params(gx_device *pdev, gs_param_list *plist)
{
gs_param_dict mdict;
int code = gdev_prn_get_params(pdev, plist);
int ecode = code;
int i;
code = gdev_begin_input_media(plist, &mdict, countof(clj_paper_sizes));
if (code < 0)
ecode = code;
else {
for (i = 0; i < countof(clj_paper_sizes); ++i) {
code = gdev_write_input_page_size(i, &mdict,
clj_paper_sizes[i].width,
clj_paper_sizes[i].height);
if (code < 0)
ecode = code;
}
code = gdev_end_input_media(plist, &mdict);
if (code < 0)
ecode = code;
}
return ecode;
}
private int
clj_media_size(float mediasize[2], gs_param_list *plist)
{
gs_param_float_array fres;
gs_param_float_array fsize;
gs_param_int_array hwsize;
int have_pagesize = 0;
if ( (param_read_float_array(plist, "HWResolution", &fres) == 0) &&
!is_supported_resolution(fres.data) )
return_error(gs_error_rangecheck);
if ( (param_read_float_array(plist, "PageSize", &fsize) == 0) ||
(param_read_float_array(plist, ".MediaSize", &fsize) == 0) ) {
mediasize[0] = fsize.data[0];
mediasize[1] = fsize.data[1];
have_pagesize = 1;
}
if (param_read_int_array(plist, "HWSize", &hwsize) == 0) {
mediasize[0] = ((float)hwsize.data[0]) / fres.data[0];
mediasize[1] = ((float)hwsize.data[1]) / fres.data[1];
have_pagesize = 1;
}
return have_pagesize;
}
private int
clj_put_params(
gx_device *             pdev,
gs_param_list *         plist
)
{
float		    mediasize[2];
bool                    rotate = false;
int                     have_pagesize = clj_media_size(mediasize, plist);
if (have_pagesize < 0)
return have_pagesize;
if (have_pagesize) {
if (get_paper_size(mediasize, &rotate) == 0 || rotate)
return_error(gs_error_rangecheck);
}
return gdev_prn_put_params(pdev, plist);
}
private void
pack_and_compress_scanline(
const byte *        pin,
int                 in_size,
byte  *             pout[3],
int                 out_size[3]
)
{
#define BUFF_SIZE                                                           \
( ((int)(CLJ_MAX_RES * CLJ_MAX_SCANLINE / 72.0) + sizeof(ulong) - 1)    \
/ sizeof(ulong) )
ulong               buff[3 * BUFF_SIZE];
byte *              p_c = (byte *)buff;
byte *              p_m = (byte *)(buff + BUFF_SIZE);
byte *              p_y = (byte *)(buff + 2 * BUFF_SIZE);
ulong *             ptrs[3];
byte                c_val = 0, m_val = 0, y_val = 0;
ulong               mask = 0x80;
int                 i;
for (i = 0; i < in_size; i++) {
uint    ival = *pin++;
if (ival != 0) {
if ((ival & 0x4) != 0)
y_val |= mask;
if ((ival & 0x2) != 0)
m_val |= mask;
if ((ival & 0x1) != 0)
c_val |= mask;
}
if ((mask >>= 1) == 0) {
*p_c++ = c_val;
c_val = 0L;
*p_m++ = m_val;
m_val = 0L;
*p_y++ = y_val;
y_val = 0L;
mask = 0x80;
}
}
if (mask != 0x80) {
*p_c++ = c_val;
*p_m++ = m_val;
*p_y++ = y_val;
}
while ((((ulong)p_c) & (sizeof(ulong) - 1)) != 0) {
*p_c++ = 0;
*p_m++ = 0;
*p_y++ = 0;
}
ptrs[0] = (ulong *)p_c;
ptrs[1] = (ulong *)p_m;
ptrs[2] = (ulong *)p_y;
for (i = 0; i < 3; i++) {
ulong * p_start = buff + i * BUFF_SIZE;
ulong * p_end = ptrs[i];
while ((p_end > p_start) && (p_end[-1] == 0))
p_end--;
if (p_start == p_end)
out_size[i] = 0;
else
out_size[i] = gdev_pcl_mode2compress(p_start, p_end, pout[i]);
}
#undef BUFF_SIZE
}
private int
clj_print_page(
gx_device_printer *     pdev,
FILE *                  prn_stream
)
{
gs_memory_t *mem = pdev->memory;
bool                    rotate;
const clj_paper_size *  psize = get_paper_size(pdev->MediaSize, &rotate);
int                     lsize = pdev->width;
int                     clsize = (lsize + (lsize + 255) / 128) / 8;
byte *                  data = 0;
byte *                  cdata[3];
int                     blank_lines = 0;
int                     i;
floatp                  fs_res = pdev->HWResolution[0] / 72.0;
floatp                  ss_res = pdev->HWResolution[1] / 72.0;
int			    imageable_width, imageable_height;
if (psize == 0)
return_error(gs_error_unregistered);
if ((data = gs_alloc_bytes(mem, lsize, "clj_print_page(data)")) == 0)
return_error(gs_error_VMerror);
if ((cdata[0] = gs_alloc_bytes(mem, 3 * clsize, "clj_print_page(cdata)")) == 0) {
gs_free_object(mem, data, "clj_print_page(data)");
return_error(gs_error_VMerror);
}
cdata[1] = cdata[0] + clsize;
cdata[2] = cdata[1] + clsize;
if (pclj->rotated) {
imageable_width = pdev->width - (2 * psize->offsets.x) * fs_res;
imageable_height = pdev->height - (2 * psize->offsets.y) * ss_res;
}
else {
imageable_width = pdev->width - (2 * psize->offsets.y) * ss_res;
imageable_height = pdev->height - (2 * psize->offsets.x) * fs_res;
}
fprintf( prn_stream,
"\033E\033&u300D\033&l%da1x%dO\033*p0x0y+50x-100Y\033*t%dR"
#ifdef USE_FAST_MODE
"\033*r-3U"
#else
"\033*v6W\001\002\003\001\001\001"
#endif
"\033*r0f%ds%dt1A\033*b2M",
psize->tag,
pclj->rotated,
(int)(pdev->HWResolution[0]),
imageable_width,
imageable_height
);
for (i = 0; i < imageable_height; i++) {
int     clen[3];
gdev_prn_copy_scan_lines(pdev, i, data, lsize);
pack_and_compress_scanline(data, imageable_width, cdata, clen);
if ((clen[0] == 0) && (clen[1] == 0) && (clen[2] == 0))
++blank_lines;
else {
if (blank_lines != 0) {
fprintf(prn_stream, "\033*b%dY", blank_lines);
blank_lines = 0;
}
fprintf(prn_stream, "\033*b%dV", clen[0]);
fwrite(cdata[0], sizeof(byte), clen[0], prn_stream);
fprintf(prn_stream, "\033*b%dV", clen[1]);
fwrite(cdata[1], sizeof(byte), clen[1], prn_stream);
fprintf(prn_stream, "\033*b%dW", clen[2]);
fwrite(cdata[2], sizeof(byte), clen[2], prn_stream);
}
}
fputs("\033*rC\f", prn_stream);
gs_free_object(mem, cdata[0], "clj_print_page(cdata)");
gs_free_object(mem, data, "clj_print_page(data)");
return 0;
}
#define CLJ_PROCS(get_params, put_params)\
gdev_prn_open,                  \
clj_get_initial_matrix,         \
NULL,	                    \
gdev_prn_output_page,           \
gdev_prn_close,                 \
gdev_pcl_3bit_map_rgb_color,    \
gdev_pcl_3bit_map_color_rgb,    \
NULL,	                    \
NULL,	                    \
NULL,	                    \
NULL,	                    \
NULL,	                    \
NULL,	                    \
get_params, 	            \
put_params,                     \
NULL,	                    \
NULL,	                    \
NULL,	                    \
NULL,	                    \
gx_page_device_get_page_device
private gx_device_procs cljet5_procs = {
CLJ_PROCS(clj_get_params, clj_put_params)
};
#define CLJ_DEVICE_BODY(procs, dname, rotated)\
prn_device_body(\
gx_device_clj,\
procs,                  \
dname,                  \
110,                    \
85,                     \
X_DPI, Y_DPI,           \
0.167, 0.167,           \
0.167, 0.167,\
3,                      \
8,			    \
1, 1, 		    \
2, 2,		     \
clj_print_page          \
),\
rotated
gx_device_clj gs_cljet5_device = {
CLJ_DEVICE_BODY(cljet5_procs, "cljet5", 0 )
};
private int
clj_pr_get_params( gx_device *pdev, gs_param_list *plist )
{
int code;
if (pclj->rotated) {
float ftmp;
int   itmp;
ftmp = pdev->MediaSize[0];
pdev->MediaSize[0] = pdev->MediaSize[1];
pdev->MediaSize[1] = ftmp;
itmp = pdev->width;
pdev->width = pdev->height;
pdev->height = itmp;
}
code = gdev_prn_get_params(pdev, plist);
if (pclj->rotated) {
float ftmp;
int   itmp;
ftmp = pdev->MediaSize[0];
pdev->MediaSize[0] = pdev->MediaSize[1];
pdev->MediaSize[1] = ftmp;
itmp = pdev->width;
pdev->width = pdev->height;
pdev->height = itmp;
}
return code;
}
private int
clj_pr_put_params(
gx_device *             pdev,
gs_param_list *         plist
)
{
float		    mediasize[2];
int                     code = 0;
bool                    rotate = false;
int                     have_pagesize = clj_media_size(mediasize, plist);
if (have_pagesize < 0)
return have_pagesize;
if (have_pagesize) {
if (get_paper_size(mediasize, &rotate) == 0)
return_error(gs_error_rangecheck);
if (rotate) {
gs_param_float_array	pf_array;
gs_c_param_list		alist;
float			ftmp = mediasize[0];
mediasize[0] = mediasize[1];
mediasize[1] = ftmp;
pf_array.data = mediasize;
pf_array.size = 2;
pf_array.persistent = false;
gs_c_param_list_write(&alist, pdev->memory);
code = param_write_float_array((gs_param_list *)&alist, ".MediaSize", &pf_array);
gs_c_param_list_read(&alist);
gs_c_param_list_set_target(&alist, plist);
if ((code = gdev_prn_put_params(pdev, (gs_param_list *)&alist)) >= 0)
pclj->rotated = true;
gs_c_param_list_release(&alist);
} else {
if ((code = gdev_prn_put_params(pdev, plist)) >= 0)
pclj->rotated = false;
}
} else
code = gdev_prn_put_params(pdev, plist);
return code;
}
private gx_device_procs cljet5pr_procs = {
CLJ_PROCS(clj_pr_get_params, clj_pr_put_params)
};
gx_device_clj gs_cljet5pr_device = {
CLJ_DEVICE_BODY(cljet5pr_procs, "cljet5pr", 1 )
};