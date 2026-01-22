#include "stdio_.h"
#include "string_.h"
#include "jpeglib_.h"
#include "jerror_.h"
#include "gx.h"
#include "gserrors.h"
#include "strimpl.h"
#include "sdct.h"
#include "sjpeg.h"
int
gs_jpeg_create_compress(stream_DCT_state * st)
{
gs_jpeg_error_setup(st);
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_stream_data_common_init(st->data.compress);
jpeg_create_compress(&st->data.compress->cinfo);
return 0;
}
int
gs_jpeg_set_defaults(stream_DCT_state * st)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_set_defaults(&st->data.compress->cinfo);
return 0;
}
int
gs_jpeg_set_colorspace(stream_DCT_state * st,
J_COLOR_SPACE colorspace)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_set_colorspace(&st->data.compress->cinfo, colorspace);
return 0;
}
int
gs_jpeg_set_linear_quality(stream_DCT_state * st,
int scale_factor, boolean force_baseline)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_set_linear_quality(&st->data.compress->cinfo,
scale_factor, force_baseline);
return 0;
}
int
gs_jpeg_set_quality(stream_DCT_state * st,
int quality, boolean force_baseline)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_set_quality(&st->data.compress->cinfo,
quality, force_baseline);
return 0;
}
int
gs_jpeg_start_compress(stream_DCT_state * st,
boolean write_all_tables)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_start_compress(&st->data.compress->cinfo, write_all_tables);
return 0;
}
int
gs_jpeg_write_scanlines(stream_DCT_state * st,
JSAMPARRAY scanlines,
int num_lines)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
return (int)jpeg_write_scanlines(&st->data.compress->cinfo,
scanlines, (JDIMENSION) num_lines);
}
int
gs_jpeg_finish_compress(stream_DCT_state * st)
{
if (setjmp(st->data.common->exit_jmpbuf))
return_error(gs_jpeg_log_error(st));
jpeg_finish_compress(&st->data.compress->cinfo);
return 0;
}