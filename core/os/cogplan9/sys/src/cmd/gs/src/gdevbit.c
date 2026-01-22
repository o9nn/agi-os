#include "math_.h"
#include "gdevprn.h"
#include "gsparam.h"
#include "gscrd.h"
#include "gscrdp.h"
#include "gxlum.h"
#include "gdevdcrd.h"
#ifndef X_DPI
# define X_DPI 72
#endif
#ifndef Y_DPI
# define Y_DPI 72
#endif
private dev_proc_map_rgb_color(bit_mono_map_color);
private dev_proc_map_color_rgb(bit_map_color_rgb);
private dev_proc_map_cmyk_color(bit_map_cmyk_color);
private dev_proc_get_params(bit_get_params);
private dev_proc_put_params(bit_put_params);
private dev_proc_print_page(bit_print_page);
#define bit_procs(encode_color)\
{ gdev_prn_open,\
gx_default_get_initial_matrix,\
NULL, \
gdev_prn_output_page,\
gdev_prn_close,\
encode_color, \
bit_map_color_rgb, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
bit_get_params,\
bit_put_params,\
encode_color, \
NULL, \
NULL, \
NULL, \
gx_page_device_get_page_device, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
NULL, \
encode_color, \
bit_map_color_rgb \
}
#define REAL_NUM_COMPONENTS(dev) (dev->dname[3] == 'c' ? 4 : \
dev->dname[3] == 'r' ? 3 : 1)
private const gx_device_procs bitmono_procs =
bit_procs(bit_mono_map_color);
const gx_device_printer gs_bit_device =
{prn_device_body(gx_device_printer, bitmono_procs, "bit",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, 1, 1, 0, 2, 1, bit_print_page)
};
private const gx_device_procs bitrgb_procs =
bit_procs(gx_default_rgb_map_rgb_color);
const gx_device_printer gs_bitrgb_device =
{prn_device_body(gx_device_printer, bitrgb_procs, "bitrgb",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
3, 4, 1, 1, 2, 2, bit_print_page)
};
private const gx_device_procs bitcmyk_procs =
bit_procs(bit_map_cmyk_color);
const gx_device_printer gs_bitcmyk_device =
{prn_device_body(gx_device_printer, bitcmyk_procs, "bitcmyk",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
4, 4, 1, 1, 2, 2, bit_print_page)
};
private gx_color_index
bit_mono_map_color(gx_device * dev, const gx_color_value cv[])
{
int bpc = dev->color_info.depth;
int drop = sizeof(gx_color_value) * 8 - bpc;
gx_color_value gray = cv[0];
return (bpc == 1 ? gx_max_color_value - gray : gray) >> drop;
}
private int
bit_map_color_rgb(gx_device * dev, gx_color_index color, gx_color_value cv[4])
{
int depth = dev->color_info.depth;
int ncomp = REAL_NUM_COMPONENTS(dev);
int bpc = depth / ncomp;
uint mask = (1 << bpc) - 1;
#define cvalue(c) ((gx_color_value)((ulong)(c) * gx_max_color_value / mask))
switch (ncomp) {
case 1:
cv[0] =
(depth == 1 ? (color ? 0 : gx_max_color_value) :
cvalue(color));
break;
case 3:
{
gx_color_index cshift = color;
cv[2] = cvalue(cshift & mask);
cshift >>= bpc;
cv[1] = cvalue(cshift & mask);
cv[0] = cvalue(cshift >> bpc);
}
break;
case 4:
{
gx_color_index cshift = color;
uint c, m, y, k;
k = cshift & mask;
cshift >>= bpc;
y = cshift & mask;
cshift >>= bpc;
m = cshift & mask;
c = cshift >> bpc;
cv[0] = cvalue((mask - c) * (mask - k) / mask);
cv[1] = cvalue((mask - m) * (mask - k) / mask);
cv[2] = cvalue((mask - y) * (mask - k) / mask);
}
break;
}
return 0;
#undef cvalue
}
private gx_color_index
bit_map_cmyk_color(gx_device * dev, const gx_color_value cv[])
{
int bpc = dev->color_info.depth / 4;
int drop = sizeof(gx_color_value) * 8 - bpc;
gx_color_index color =
(((((((gx_color_index) cv[0] >> drop) << bpc) +
(cv[1] >> drop)) << bpc) +
(cv[2] >> drop)) << bpc) +
(cv[3] >> drop);
return (color == gx_no_color_index ? color ^ 1 : color);
}
private int
bit_get_params(gx_device * pdev, gs_param_list * plist)
{
int code, ecode;
int real_ncomps = REAL_NUM_COMPONENTS(pdev);
int ncomps = pdev->color_info.num_components;
int forcemono = (ncomps == real_ncomps ? 0 : 1);
pdev->color_info.num_components = real_ncomps;
ecode = gdev_prn_get_params(pdev, plist);
code = sample_device_crd_get_params(pdev, plist, "CRDDefault");
if (code < 0)
ecode = code;
if ((code = param_write_int(plist, "ForceMono", &forcemono)) < 0) {
ecode = code;
}
pdev->color_info.num_components = ncomps;
return ecode;
}
private int
bit_put_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_color_info save_info;
int ncomps = pdev->color_info.num_components;
int real_ncomps = REAL_NUM_COMPONENTS(pdev);
int bpc = pdev->color_info.depth / real_ncomps;
int v;
int ecode = 0;
int code;
static const byte depths[4][16] = {
{1, 2, 0, 4, 8, 0, 0, 8, 0, 0, 0, 16, 0, 0, 0, 16},
{0},
{4, 8, 0, 16, 16, 0, 0, 24, 0, 0, 0, 40, 0, 0, 0, 48},
{4, 8, 0, 16, 32, 0, 0, 32, 0, 0, 0, 48, 0, 0, 0, 64}
};
const char *vname;
pdev->color_info.num_components = real_ncomps;
if ((code = param_read_int(plist, (vname = "GrayValues"), &v)) != 1 ||
(code = param_read_int(plist, (vname = "RedValues"), &v)) != 1 ||
(code = param_read_int(plist, (vname = "GreenValues"), &v)) != 1 ||
(code = param_read_int(plist, (vname = "BlueValues"), &v)) != 1
) {
if (code < 0)
ecode = code;
else
switch (v) {
case 2: bpc = 1; break;
case 4: bpc = 2; break;
case 16: bpc = 4; break;
case 32: bpc = 5; break;
case 256: bpc = 8; break;
case 4096: bpc = 12; break;
case 65536: bpc = 16; break;
default:
param_signal_error(plist, vname,
ecode = gs_error_rangecheck);
}
}
switch (code = param_read_int(plist, (vname = "ForceMono"), &v)) {
case 0:
if (v == 1) {
ncomps = 1;
break;
}
else if (v == 0) {
ncomps = real_ncomps;
break;
}
code = gs_error_rangecheck;
default:
ecode = code;
param_signal_error(plist, vname, ecode);
case 1:
break;
}
if (ecode < 0)
return ecode;
save_info = pdev->color_info;
pdev->color_info.depth = depths[real_ncomps - 1][bpc - 1];
pdev->color_info.max_gray = pdev->color_info.max_color =
(pdev->color_info.dither_grays =
pdev->color_info.dither_colors =
(1 << bpc)) - 1;
ecode = gdev_prn_put_params(pdev, plist);
if (ecode < 0) {
pdev->color_info = save_info;
return ecode;
}
pdev->color_info.num_components = ncomps;
if (pdev->color_info.depth != save_info.depth ||
pdev->color_info.num_components != save_info.num_components
) {
gs_closedevice(pdev);
}
if (dev_proc(pdev, map_cmyk_color) == cmyk_1bit_map_cmyk_color ||
dev_proc(pdev, map_cmyk_color) == cmyk_8bit_map_cmyk_color ||
dev_proc(pdev, map_cmyk_color) == bit_map_cmyk_color) {
set_dev_proc(pdev, map_cmyk_color,
pdev->color_info.depth == 4 ? cmyk_1bit_map_cmyk_color :
pdev->color_info.depth == 32 ? cmyk_8bit_map_cmyk_color :
bit_map_cmyk_color);
}
set_linear_color_bits_mask_shift(pdev);
pdev->color_info.separable_and_linear = GX_CINFO_SEP_LIN;
return 0;
}
private int
bit_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *) pdev);
byte *in = gs_alloc_bytes(pdev->memory, line_size, "bit_print_page(in)");
byte *data;
int nul = !strcmp(pdev->fname, "nul");
int lnum = 0, bottom = pdev->height;
if (in == 0)
return_error(gs_error_VMerror);
for (; lnum < bottom; ++lnum) {
gdev_prn_get_bits(pdev, lnum, in, &data);
if (!nul)
fwrite(data, 1, line_size, prn_stream);
}
gs_free_object(pdev->memory, in, "bit_print_page(in)");
return 0;
}