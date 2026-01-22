#include "memory_.h"
#include "stdio_.h"
#include "jpeglib_.h"
#include "jerror_.h"
#include "gdebug.h"
#include "gsmemory.h"
#include "gsmalloc.h"
#include "strimpl.h"
#include "sdct.h"
#include "sjpeg.h"
private void
dctd_init_source(j_decompress_ptr dinfo)
{
}
static const JOCTET fake_eoi[2] =
{0xFF, JPEG_EOI};
private boolean
dctd_fill_input_buffer(j_decompress_ptr dinfo)
{
jpeg_decompress_data *jddp =
(jpeg_decompress_data *) ((char *)dinfo -
offset_of(jpeg_decompress_data, dinfo));
if (!jddp->input_eod)
return FALSE;
WARNMS(dinfo, JWRN_JPEG_EOF);
dinfo->src->next_input_byte = fake_eoi;
dinfo->src->bytes_in_buffer = 2;
jddp->faked_eoi = true;
return TRUE;
}
private void
dctd_skip_input_data(j_decompress_ptr dinfo, long num_bytes)
{
struct jpeg_source_mgr *src = dinfo->src;
jpeg_decompress_data *jddp =
(jpeg_decompress_data *) ((char *)dinfo -
offset_of(jpeg_decompress_data, dinfo));
if (num_bytes > 0) {
if (num_bytes > src->bytes_in_buffer) {
jddp->skip += num_bytes - src->bytes_in_buffer;
src->next_input_byte += src->bytes_in_buffer;
src->bytes_in_buffer = 0;
return;
}
src->next_input_byte += num_bytes;
src->bytes_in_buffer -= num_bytes;
}
}
private void
dctd_term_source(j_decompress_ptr dinfo)
{
}
private void
s_DCTD_set_defaults(stream_state * st)
{
s_DCT_set_defaults(st);
}
private int
s_DCTD_init(stream_state * st)
{
stream_DCT_state *const ss = (stream_DCT_state *) st;
struct jpeg_source_mgr *src = &ss->data.decompress->source;
src->init_source = dctd_init_source;
src->fill_input_buffer = dctd_fill_input_buffer;
src->skip_input_data = dctd_skip_input_data;
src->term_source = dctd_term_source;
src->resync_to_restart = jpeg_resync_to_restart;
ss->data.common->memory = ss->jpeg_memory;
ss->data.decompress->dinfo.src = src;
ss->data.decompress->skip = 0;
ss->data.decompress->input_eod = false;
ss->data.decompress->faked_eoi = false;
ss->phase = 0;
return 0;
}
private int
s_DCTD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_DCT_state *const ss = (stream_DCT_state *) st;
jpeg_decompress_data *jddp = ss->data.decompress;
struct jpeg_source_mgr *src = jddp->dinfo.src;
int code;
if_debug3('w', "[wdd]process avail=%u, skip=%u, last=%d\n",
(uint) (pr->limit - pr->ptr), (uint) jddp->skip, last);
if (jddp->skip != 0) {
long avail = pr->limit - pr->ptr;
if (avail < jddp->skip) {
jddp->skip -= avail;
pr->ptr = pr->limit;
if (!last)
return 0;
jddp->skip = 0;
}
pr->ptr += jddp->skip;
jddp->skip = 0;
}
src->next_input_byte = pr->ptr + 1;
src->bytes_in_buffer = pr->limit - pr->ptr;
jddp->input_eod = last;
switch (ss->phase) {
case 0:
while (pr->ptr < pr->limit && pr->ptr[1] != 0xff)
pr->ptr++;
if (pr->ptr == pr->limit)
return 0;
src->next_input_byte = pr->ptr + 1;
src->bytes_in_buffer = pr->limit - pr->ptr;
ss->phase = 1;
case 1:
if ((code = gs_jpeg_read_header(ss, TRUE)) < 0)
return ERRC;
pr->ptr =
(jddp->faked_eoi ? pr->limit : src->next_input_byte - 1);
switch (code) {
case JPEG_SUSPENDED:
return 0;
}
if (ss->ColorTransform >= 0 &&
!jddp->dinfo.saw_Adobe_marker) {
switch (jddp->dinfo.num_components) {
case 3:
jddp->dinfo.jpeg_color_space =
(ss->ColorTransform ? JCS_YCbCr : JCS_RGB);
break;
case 4:
jddp->dinfo.jpeg_color_space =
(ss->ColorTransform ? JCS_YCCK : JCS_CMYK);
break;
}
}
ss->phase = 2;
case 2:
if ((code = gs_jpeg_start_decompress(ss)) < 0)
return ERRC;
pr->ptr =
(jddp->faked_eoi ? pr->limit : src->next_input_byte - 1);
if (code == 0)
return 0;
ss->scan_line_size =
jddp->dinfo.output_width * jddp->dinfo.output_components;
if_debug4('w', "[wdd]width=%u, components=%d, scan_line_size=%u, min_out_size=%u\n",
jddp->dinfo.output_width,
jddp->dinfo.output_components,
ss->scan_line_size, jddp->template.min_out_size);
if (ss->scan_line_size > (uint) jddp->template.min_out_size) {
jddp->scanline_buffer =
gs_alloc_bytes_immovable(jddp->memory,
ss->scan_line_size,
"s_DCTD_process(scanline_buffer)");
if (jddp->scanline_buffer == NULL)
return ERRC;
}
jddp->bytes_in_scanline = 0;
ss->phase = 3;
case 3:
dumpbuffer:
if (jddp->bytes_in_scanline != 0) {
uint avail = pw->limit - pw->ptr;
uint tomove = min(jddp->bytes_in_scanline,
avail);
if_debug2('w', "[wdd]moving %u/%u\n",
tomove, avail);
memcpy(pw->ptr + 1, jddp->scanline_buffer +
(ss->scan_line_size - jddp->bytes_in_scanline),
tomove);
pw->ptr += tomove;
jddp->bytes_in_scanline -= tomove;
if (jddp->bytes_in_scanline != 0)
return 1;
}
while (jddp->dinfo.output_height > jddp->dinfo.output_scanline) {
int read;
byte *samples;
if (jddp->scanline_buffer != NULL)
samples = jddp->scanline_buffer;
else {
if ((uint) (pw->limit - pw->ptr) < ss->scan_line_size)
return 1;
samples = pw->ptr + 1;
}
read = gs_jpeg_read_scanlines(ss, &samples, 1);
if (read < 0)
return ERRC;
if_debug3('w', "[wdd]read returns %d, used=%u, faked_eoi=%d\n",
read,
(uint) (src->next_input_byte - 1 - pr->ptr),
(int)jddp->faked_eoi);
pr->ptr =
(jddp->faked_eoi ? pr->limit : src->next_input_byte - 1);
if (!read)
return 0;
if (jddp->scanline_buffer != NULL) {
jddp->bytes_in_scanline = ss->scan_line_size;
goto dumpbuffer;
}
pw->ptr += ss->scan_line_size;
}
ss->phase = 4;
case 4:
if ((code = gs_jpeg_finish_decompress(ss)) < 0)
return ERRC;
pr->ptr =
(jddp->faked_eoi ? pr->limit : src->next_input_byte - 1);
if (code == 0)
return 0;
ss->phase = 5;
case 5:
return EOFC;
}
return ERRC;
}
private void
s_DCTD_release(stream_state * st)
{
stream_DCT_state *const ss = (stream_DCT_state *) st;
gs_jpeg_destroy(ss);
if (ss->data.decompress->scanline_buffer != NULL)
gs_free_object(ss->data.common->memory,
ss->data.decompress->scanline_buffer,
"s_DCTD_release(scanline_buffer)");
gs_free_object(ss->data.common->memory, ss->data.decompress,
"s_DCTD_release");
st->template = &s_DCTD_template;
}
const stream_template s_DCTD_template =
{&st_DCT_state, s_DCTD_init, s_DCTD_process, 2000, 4000, s_DCTD_release,
s_DCTD_set_defaults
};