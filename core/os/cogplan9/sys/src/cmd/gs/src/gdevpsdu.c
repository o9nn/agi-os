#include "stdio_.h"
#include "jpeglib_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gdevpsdf.h"
#include "strimpl.h"
#include "sa85x.h"
#include "scfx.h"
#include "sdct.h"
#include "sjpeg.h"
#include "spprint.h"
#include "sstring.h"
#include "gsovrc.h"
public_st_device_psdf();
public_st_psdf_binary_writer();
const psdf_set_color_commands_t psdf_set_fill_color_commands = {
"g", "rg", "k", "cs", "sc", "scn"
};
const psdf_set_color_commands_t psdf_set_stroke_color_commands = {
"G", "RG", "K", "CS", "SC", "SCN"
};
extern stream_state_proc_put_params(s_DCTE_put_params, stream_DCT_state);
int
psdf_setlinewidth(gx_device_vector * vdev, floatp width)
{
pprintg1(gdev_vector_stream(vdev), "%g w\n", width);
return 0;
}
int
psdf_setlinecap(gx_device_vector * vdev, gs_line_cap cap)
{
pprintd1(gdev_vector_stream(vdev), "%d J\n", cap);
return 0;
}
int
psdf_setlinejoin(gx_device_vector * vdev, gs_line_join join)
{
pprintd1(gdev_vector_stream(vdev), "%d j\n", join);
return 0;
}
int
psdf_setmiterlimit(gx_device_vector * vdev, floatp limit)
{
pprintg1(gdev_vector_stream(vdev), "%g M\n", limit);
return 0;
}
int
psdf_setdash(gx_device_vector * vdev, const float *pattern, uint count,
floatp offset)
{
stream *s = gdev_vector_stream(vdev);
int i;
stream_puts(s, "[ ");
for (i = 0; i < count; ++i)
pprintg1(s, "%g ", pattern[i]);
pprintg1(s, "] %g d\n", offset);
return 0;
}
int
psdf_setflat(gx_device_vector * vdev, floatp flatness)
{
pprintg1(gdev_vector_stream(vdev), "%g i\n", flatness);
return 0;
}
int
psdf_setlogop(gx_device_vector * vdev, gs_logical_operation_t lop,
gs_logical_operation_t diff)
{
return 0;
}
int
psdf_dorect(gx_device_vector * vdev, fixed x0, fixed y0, fixed x1, fixed y1,
gx_path_type_t type)
{
int code = (*vdev_proc(vdev, beginpath)) (vdev, type);
if (code < 0)
return code;
pprintg4(gdev_vector_stream(vdev), "%g %g %g %g re\n",
fixed2float(x0), fixed2float(y0),
fixed2float(x1 - x0), fixed2float(y1 - y0));
return (*vdev_proc(vdev, endpath)) (vdev, type);
}
int
psdf_beginpath(gx_device_vector * vdev, gx_path_type_t type)
{
return 0;
}
int
psdf_moveto(gx_device_vector * vdev, floatp x0, floatp y0, floatp x, floatp y,
gx_path_type_t type)
{
pprintg2(gdev_vector_stream(vdev), "%g %g m\n", x, y);
return 0;
}
int
psdf_lineto(gx_device_vector * vdev, floatp x0, floatp y0, floatp x, floatp y,
gx_path_type_t type)
{
pprintg2(gdev_vector_stream(vdev), "%g %g l\n", x, y);
return 0;
}
int
psdf_curveto(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x1, floatp y1, floatp x2, floatp y2, floatp x3, floatp y3,
gx_path_type_t type)
{
if (x1 == x0 && y1 == y0 && x2 == x3 && y2 == y3)
pprintg2(gdev_vector_stream(vdev), "%g %g l\n", x3, y3);
else if (x1 == x0 && y1 == y0)
pprintg4(gdev_vector_stream(vdev), "%g %g %g %g v\n",
x2, y2, x3, y3);
else if (x3 == x2 && y3 == y2)
pprintg4(gdev_vector_stream(vdev), "%g %g %g %g y\n",
x1, y1, x2, y2);
else
pprintg6(gdev_vector_stream(vdev), "%g %g %g %g %g %g c\n",
x1, y1, x2, y2, x3, y3);
return 0;
}
int
psdf_closepath(gx_device_vector * vdev, floatp x0, floatp y0,
floatp x_start, floatp y_start, gx_path_type_t type)
{
stream_puts(gdev_vector_stream(vdev), "h\n");
return 0;
}
gx_color_index
psdf_adjust_color_index(gx_device_vector *vdev, gx_color_index color)
{
return (color == (gx_no_color_index ^ 1) ? gx_no_color_index : color);
}
double
psdf_round(double v, int precision, int radix)
{
double mul = 1;
double w = v;
if (w <= 0)
return w;
while (w < precision) {
w *= radix;
mul *= radix;
}
return (int)(w + 0.5) / mul;
}
private inline double
round_byte_color(gx_color_index cv)
{
return (int)((uint)cv * (1000.0 / 255.0) + 0.5) / 1000.0;
}
int
psdf_set_color(gx_device_vector * vdev, const gx_drawing_color * pdc,
const psdf_set_color_commands_t *ppscc)
{
const char *setcolor;
if (!gx_dc_is_pure(pdc))
return_error(gs_error_rangecheck);
{
stream *s = gdev_vector_stream(vdev);
gx_color_index color =
psdf_adjust_color_index(vdev, gx_dc_pure_color(pdc));
double v3 = round_byte_color(color & 0xff);
switch (vdev->color_info.num_components) {
case 4:
if ((color & 0xffffff00) == 0 && ppscc->setgray != 0) {
v3 = 1.0 - v3;
goto g;
}
pprintg4(s, "%g %g %g %g", round_byte_color(color >> 24),
round_byte_color((color >> 16) & 0xff),
round_byte_color((color >> 8) & 0xff), v3);
setcolor = ppscc->setcmykcolor;
break;
case 3:
if (!((color ^ (color >> 8)) & 0xffff) && ppscc->setgray != 0)
goto g;
pprintg3(s, "%g %g %g", round_byte_color((color >> 16) & 0xff),
round_byte_color((color >> 8) & 0xff), v3);
setcolor = ppscc->setrgbcolor;
break;
case 1:
g:
pprintg1(s, "%g", v3);
setcolor = ppscc->setgray;
break;
default:
return_error(gs_error_rangecheck);
}
if (setcolor)
pprints1(s, " %s\n", setcolor);
}
return 0;
}
int
psdf_begin_binary(gx_device_psdf * pdev, psdf_binary_writer * pbw)
{
gs_memory_t *mem = pbw->memory = pdev->v_memory;
pbw->target = pdev->strm;
pbw->dev = pdev;
pbw->strm = 0;
if (!pdev->binary_ok) {
#define BUF_SIZE 100
byte *buf = gs_alloc_bytes(mem, BUF_SIZE, "psdf_begin_binary(buf)");
stream_A85E_state *ss = (stream_A85E_state *)
s_alloc_state(mem, s_A85E_template.stype,
"psdf_begin_binary(stream_state)");
stream *s = s_alloc(mem, "psdf_begin_binary(stream)");
if (buf == 0 || ss == 0 || s == 0) {
gs_free_object(mem, s, "psdf_begin_binary(stream)");
gs_free_object(mem, ss, "psdf_begin_binary(stream_state)");
gs_free_object(mem, buf, "psdf_begin_binary(buf)");
return_error(gs_error_VMerror);
}
ss->template = &s_A85E_template;
s_init_filter(s, (stream_state *)ss, buf, BUF_SIZE, pdev->strm);
#undef BUF_SIZE
pbw->strm = s;
} else {
pbw->strm = pdev->strm;
}
return 0;
}
int
psdf_encode_binary(psdf_binary_writer * pbw, const stream_template * template,
stream_state * ss)
{
return (s_add_filter(&pbw->strm, template, ss, pbw->memory) == 0 ?
gs_note_error(gs_error_VMerror) : 0);
}
int
psdf_DCT_filter(gs_param_list *plist ,
stream_state *st,
int Columns, int Rows, int Colors,
psdf_binary_writer *pbw )
{
stream_DCT_state *const ss = (stream_DCT_state *) st;
gs_memory_t *mem = st->memory;
jpeg_compress_data *jcdp;
gs_c_param_list rcc_list;
int code;
gs_c_param_list_write(&rcc_list, mem);
if ((code = param_write_int((gs_param_list *)&rcc_list, "Rows",
&Rows)) < 0 ||
(code = param_write_int((gs_param_list *)&rcc_list, "Columns",
&Columns)) < 0 ||
(code = param_write_int((gs_param_list *)&rcc_list, "Colors",
&Colors)) < 0
) {
goto rcc_fail;
}
gs_c_param_list_read(&rcc_list);
if (plist)
gs_c_param_list_set_target(&rcc_list, plist);
jcdp = gs_alloc_struct_immovable(mem, jpeg_compress_data,
&st_jpeg_compress_data, "zDCTE");
if (jcdp == 0)
return_error(gs_error_VMerror);
ss->data.compress = jcdp;
jcdp->memory = ss->jpeg_memory = mem;
if ((code = gs_jpeg_create_compress(ss)) < 0)
goto dcte_fail;
s_DCTE_put_params((gs_param_list *)&rcc_list, ss);
jcdp->template = s_DCTE_template;
ss->scan_line_size = jcdp->cinfo.input_components *
jcdp->cinfo.image_width;
jcdp->template.min_in_size =
max(s_DCTE_template.min_in_size, ss->scan_line_size);
jcdp->template.min_out_size =
max(s_DCTE_template.min_out_size, ss->Markers.size);
if (pbw)
code = psdf_encode_binary(pbw, &jcdp->template, st);
if (code >= 0) {
gs_c_param_list_release(&rcc_list);
return 0;
}
dcte_fail:
gs_jpeg_destroy(ss);
gs_free_object(mem, jcdp, "setup_image_compression");
rcc_fail:
gs_c_param_list_release(&rcc_list);
return code;
}
int
psdf_CFE_binary(psdf_binary_writer * pbw, int w, int h, bool invert)
{
gs_memory_t *mem = pbw->memory;
const stream_template *template = &s_CFE_template;
stream_CFE_state *st =
gs_alloc_struct(mem, stream_CFE_state, template->stype,
"psdf_CFE_binary");
int code;
if (st == 0)
return_error(gs_error_VMerror);
(*template->set_defaults) ((stream_state *) st);
st->K = -1;
st->Columns = w;
st->Rows = 0;
st->BlackIs1 = !invert;
st->EndOfBlock = pbw->strm->state->template != &s_A85E_template;
code = psdf_encode_binary(pbw, template, (stream_state *) st);
if (code < 0)
gs_free_object(mem, st, "psdf_CFE_binary");
return code;
}
int
psdf_end_binary(psdf_binary_writer * pbw)
{
int status = s_close_filters(&pbw->strm, pbw->target);
return (status >= 0 ? 0 : gs_note_error(gs_error_ioerror));
}
int
psdf_get_bits(gx_device * dev, int y, byte * data, byte ** actual_data)
{
return_error(gs_error_unregistered);
}
int
psdf_get_bits_rectangle(
gx_device * dev,
const gs_int_rect * prect,
gs_get_bits_params_t * params,
gs_int_rect ** unread )
{
return_error(gs_error_unregistered);
}
int
psdf_create_compositor(
gx_device * dev,
gx_device ** pcdev,
const gs_composite_t * pct,
gs_imager_state * pis,
gs_memory_t * mem )
{
if (gs_is_overprint_compositor(pct)) {
*pcdev = dev;
return 0;
} else
return gx_default_create_compositor(dev, pcdev, pct, pis, mem);
}