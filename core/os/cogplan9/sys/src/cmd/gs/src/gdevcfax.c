#include "gdevprn.h"
#include "strimpl.h"
#include "scfx.h"
#include "gdevfax.h"
private dev_proc_print_page(cfax_print_page);
private dev_proc_close_device(cfax_prn_close);
private const gx_device_procs gdev_cfax_std_procs =
prn_params_procs(gdev_prn_open, gdev_prn_output_page, cfax_prn_close,
gdev_fax_get_params, gdev_fax_put_params);
const gx_device_fax gs_cfax_device = {
FAX_DEVICE_BODY(gx_device_fax, gdev_cfax_std_procs, "cfax", cfax_print_page)
};
private void
cfax_byte(uint c, FILE * file)
{
fputc(c & 0xff, file);
}
private void
cfax_word(ushort c, FILE * file)
{
cfax_byte(c & 0xff, file);
cfax_byte(c >> 8, file);
}
private void
cfax_dword(ulong c, FILE * file)
{
cfax_byte(c & 0xff, file);
cfax_byte(c >> 8, file);
cfax_byte(c >> 16, file);
cfax_byte(c >> 24, file);
}
private void
cfax_doc_hdr(FILE * file)
{
cfax_byte('S', file);
cfax_byte('f', file);
cfax_byte('f', file);
cfax_byte('f', file);
cfax_byte(1, file);
cfax_byte(0, file);
cfax_word(0, file);
cfax_word(0, file);
cfax_word(20, file);
cfax_dword(0, file);
cfax_dword(0, file);
}
private void
cfax_page_hdr(gx_device_printer * pdev, FILE * file)
{
cfax_byte(254, file);
cfax_byte(16, file);
cfax_byte((pdev->y_pixels_per_inch < 100 ? 0 : 1), file);
cfax_byte(0, file);
cfax_byte(0, file);
cfax_byte(0, file);
cfax_word(pdev->width, file);
cfax_word(pdev->height, file);
cfax_dword(0, file);
cfax_dword(0, file);
}
private void
cfax_doc_end(FILE * file)
{
cfax_byte(254, file);
cfax_byte(0, file);
}
private int
cfax_stream_print_page_width(gx_device_printer * pdev, FILE * prn_stream,
const stream_template * temp, stream_state * ss,
int width)
{
gs_memory_t *mem = pdev->memory;
int code = 0;
stream_cursor_read r;
stream_cursor_write w;
int in_size = gdev_prn_raster((gx_device *) pdev);
int col_size = (width * pdev->color_info.depth + 7) >> 3;
int max_size = max(in_size, col_size);
int lnum, nbytes, i;
byte *in;
byte *out;
bool nul = !strcmp(pdev->fname, "nul");
ss->template = temp;
ss->memory = mem;
in = gs_alloc_bytes(mem, temp->min_in_size + max_size + 1,
"cfax_stream_print_page(in)");
#define OUT_SIZE 1000
out = gs_alloc_bytes(mem, OUT_SIZE, "cfax_stream_print_page(out)");
if (in == 0 || out == 0) {
code = gs_note_error(gs_error_VMerror);
goto done;
}
for (lnum = 0; lnum < pdev->height; lnum++) {
r.ptr = in - 1;
r.limit = in + col_size;
w.ptr = out - 1;
w.limit = w.ptr + OUT_SIZE;
code = (*temp->init) (ss);
if (code < 0)
return_error(gs_error_limitcheck);
gdev_prn_copy_scan_lines(pdev, lnum, in, in_size);
if (col_size > in_size) {
memset(in + in_size , 0, col_size - in_size);
}
code = (*temp->process) (ss, &r, &w, 1 );
nbytes = w.ptr - out + 1;
if (!nul) {
if (nbytes > 0) {
if (nbytes < 217) {
cfax_byte(nbytes, prn_stream);
for (i = 0; i < nbytes; i++)
cfax_byte(out[i], prn_stream);
} else {
cfax_byte(0, prn_stream);
cfax_word(nbytes, prn_stream);
for (i = 0; i < nbytes; i++)
cfax_byte(out[i], prn_stream);
}
} else {
cfax_byte(218, prn_stream);
}
}
if (temp->release != 0)
(*temp->release) (ss);
}
#undef OUT_SIZE
done:
gs_free_object(mem, out, "cfax_stream_print_page(out)");
gs_free_object(mem, in, "cfax_stream_print_page(in)");
return code;
}
private int
cfax_begin_page(gx_device_printer * pdev, FILE * fp, int width)
{
int save_width = pdev->width;
pdev->width = width;
if (gdev_prn_file_is_new(pdev)) {
cfax_doc_hdr(fp);
}
cfax_page_hdr(pdev, fp);
pdev->width = save_width;
return 0;
}
private int
cfax_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
stream_CFE_state state;
int code;
gdev_fax_init_fax_state(&state, (gx_device_fax *)pdev);
state.EndOfLine = false;
state.EndOfBlock = false;
state.EncodedByteAlign = true;
state.FirstBitLowOrder = true;
state.K = 0;
cfax_begin_page(pdev, prn_stream, state.Columns);
code = cfax_stream_print_page_width(pdev, prn_stream,
&s_CFE_template, (stream_state *) &state, state.Columns);
return code;
}
private int
cfax_prn_close(gx_device * pdev)
{
gx_device_printer * const ppdev = (gx_device_printer *)pdev;
if (ppdev->file != NULL) {
cfax_doc_end(ppdev->file);
}
return gdev_prn_close(pdev);
}