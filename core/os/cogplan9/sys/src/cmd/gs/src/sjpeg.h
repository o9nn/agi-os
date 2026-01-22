#ifndef sjpeg_INCLUDED
# define sjpeg_INCLUDED
void gs_jpeg_error_setup(stream_DCT_state * st);
int gs_jpeg_log_error(stream_DCT_state * st);
JQUANT_TBL *gs_jpeg_alloc_quant_table(stream_DCT_state * st);
JHUFF_TBL *gs_jpeg_alloc_huff_table(stream_DCT_state * st);
int gs_jpeg_destroy(stream_DCT_state * st);
int gs_jpeg_create_compress(stream_DCT_state * st);
int gs_jpeg_set_defaults(stream_DCT_state * st);
int gs_jpeg_set_colorspace(stream_DCT_state * st,
J_COLOR_SPACE colorspace);
int gs_jpeg_set_linear_quality(stream_DCT_state * st,
int scale_factor, boolean force_baseline);
int gs_jpeg_set_quality(stream_DCT_state * st,
int quality, boolean force_baseline);
int gs_jpeg_start_compress(stream_DCT_state * st,
boolean write_all_tables);
int gs_jpeg_write_scanlines(stream_DCT_state * st,
JSAMPARRAY scanlines, int num_lines);
int gs_jpeg_finish_compress(stream_DCT_state * st);
int gs_jpeg_create_decompress(stream_DCT_state * st);
int gs_jpeg_read_header(stream_DCT_state * st,
boolean require_image);
int gs_jpeg_start_decompress(stream_DCT_state * st);
int gs_jpeg_read_scanlines(stream_DCT_state * st,
JSAMPARRAY scanlines, int max_lines);
int gs_jpeg_finish_decompress(stream_DCT_state * st);
#endif