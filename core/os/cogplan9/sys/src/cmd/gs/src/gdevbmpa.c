#include "stdio_.h"
#include "gserrors.h"
#include "gdevprna.h"
#include "gdevpccm.h"
#include "gdevbmp.h"
#include "gdevppla.h"
#include "gpsync.h"
typedef struct gx_device_async_s {
gx_device_common;
gx_prn_device_common;
bool UsePlanarBuffer;
int buffered_page_exists;
long file_offset_to_data[4];
} gx_device_async;
#define async_device(procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page)\
{ prn_device_std_margins_body(gx_device_async, procs, dname,\
w10, h10, xdpi, ydpi, lm, tm, lm, bm, rm, tm, color_bits, print_page),\
0, 0, { 0, 0, 0, 0 }\
}
private dev_proc_open_device(bmpa_writer_open);
private dev_proc_open_device(bmpa_cmyk_writer_open);
private prn_dev_proc_open_render_device(bmpa_reader_open_render_device);
private dev_proc_print_page_copies(bmpa_reader_print_page_copies);
private dev_proc_print_page_copies(bmpa_cmyk_reader_print_copies);
private prn_dev_proc_buffer_page(bmpa_reader_buffer_page);
private prn_dev_proc_buffer_page(bmpa_cmyk_reader_buffer_page);
private dev_proc_output_page(bmpa_reader_output_page);
private dev_proc_get_params(bmpa_get_params);
private dev_proc_put_params(bmpa_put_params);
private dev_proc_get_hardware_params(bmpa_get_hardware_params);
private prn_dev_proc_start_render_thread(bmpa_reader_start_render_thread);
private prn_dev_proc_get_space_params(bmpa_get_space_params);
#define default_print_page 0
private const gx_device_procs bmpamono_procs =
prn_procs(bmpa_writer_open, gdev_prn_output_page, gdev_prn_close);
const gx_device_async gs_bmpamono_device =
async_device(bmpamono_procs, "bmpamono",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
1, default_print_page);
#define bmpa_cmyk_procs(p_open, p_map_color_rgb, p_map_cmyk_color)\
p_open, NULL, NULL, gdev_prn_output_page, gdev_prn_close,\
NULL, p_map_color_rgb, NULL, NULL, NULL, NULL, NULL, NULL,\
bmpa_get_params, bmpa_put_params,\
p_map_cmyk_color, NULL, NULL, NULL, gx_page_device_get_page_device
private const gx_device_procs bmpasep1_procs = {
bmpa_cmyk_procs(bmpa_cmyk_writer_open, cmyk_1bit_map_color_rgb,
cmyk_1bit_map_cmyk_color)
};
const gx_device_async gs_bmpasep1_device = {
prn_device_body(gx_device_async, bmpasep1_procs, "bmpasep1",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
4, 4, 1, 1, 2, 2, default_print_page)
};
private const gx_device_procs bmpasep8_procs = {
bmpa_cmyk_procs(bmpa_cmyk_writer_open, cmyk_8bit_map_color_rgb,
cmyk_8bit_map_cmyk_color)
};
const gx_device_async gs_bmpasep8_device = {
prn_device_body(gx_device_async, bmpasep8_procs, "bmpasep8",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
4, 32, 255, 255, 256, 256, default_print_page)
};
private const gx_device_procs bmpa16_procs =
prn_color_procs(bmpa_writer_open, gdev_prn_output_page, gdev_prn_close,
pc_4bit_map_rgb_color, pc_4bit_map_color_rgb);
const gx_device_async gs_bmpa16_device =
async_device(bmpa16_procs, "bmpa16",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
4, default_print_page);
private const gx_device_procs bmpa256_procs =
prn_color_procs(bmpa_writer_open, gdev_prn_output_page, gdev_prn_close,
pc_8bit_map_rgb_color, pc_8bit_map_color_rgb);
const gx_device_async gs_bmpa256_device =
async_device(bmpa256_procs, "bmpa256",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
8, default_print_page);
private const gx_device_procs bmpa16m_procs =
prn_color_procs(bmpa_writer_open, gdev_prn_output_page, gdev_prn_close,
bmp_map_16m_rgb_color, bmp_map_16m_color_rgb);
const gx_device_async gs_bmpa16m_device =
async_device(bmpa16m_procs, "bmpa16m",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
24, default_print_page);
private const gx_device_procs bmpa32b_procs = {
bmpa_cmyk_procs(bmpa_writer_open, gx_default_map_color_rgb,
gx_default_cmyk_map_cmyk_color)
};
const gx_device_async gs_bmpa32b_device =
async_device(bmpa32b_procs, "bmpa32b",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
32, default_print_page);
private void bmpa_reader_thread(void *);
private int
bmpa_open_writer(gx_device *pdev ,
dev_proc_print_page_copies((*reader_print_page_copies)),
prn_dev_proc_buffer_page((*reader_buffer_page)))
{
gx_device_async * const pwdev = (gx_device_async *)pdev;
int max_width;
int max_raster;
int min_band_height;
int max_src_image_row;
init_async_render_procs(pwdev, bmpa_reader_start_render_thread,
reader_buffer_page,
reader_print_page_copies);
set_dev_proc(pdev, get_params, bmpa_get_params);
set_dev_proc(pdev, put_params, bmpa_put_params);
set_dev_proc(pdev, get_hardware_params, bmpa_get_hardware_params);
set_dev_proc(pdev, output_page, bmpa_reader_output_page);
pwdev->printer_procs.get_space_params = bmpa_get_space_params;
pwdev->printer_procs.open_render_device =
bmpa_reader_open_render_device;
max_width = DEFAULT_WIDTH_10THS * 60;
min_band_height = max(1, (DEFAULT_HEIGHT_10THS * 60) / 100);
max_raster = bitmap_raster(max_width * pwdev->color_info.depth);
max_src_image_row = max_width * 4 * 2;
if (pwdev->UsePlanarBuffer)
gdev_prn_set_procs_planar(pdev);
return gdev_prn_async_write_open((gx_device_printer *)pdev,
max_raster, min_band_height,
max_src_image_row);
}
private int
bmpa_writer_open(gx_device *pdev )
{
return bmpa_open_writer(pdev, bmpa_reader_print_page_copies,
bmpa_reader_buffer_page);
}
private int
bmpa_cmyk_writer_open(gx_device *pdev )
{
return bmpa_open_writer(pdev, bmpa_cmyk_reader_print_copies,
bmpa_cmyk_reader_buffer_page);
}
private int
bmpa_reader_buffer_planes(gx_device_printer *pdev, FILE *prn_stream,
int num_copies, int first_plane,
int last_plane, int raster);
private void
bmpa_reader_thread(void *params)
{
gdev_prn_async_render_thread((gdev_prn_start_render_params *)params);
}
private int
bmpa_reader_start_render_thread(gdev_prn_start_render_params *params)
{
return gp_create_thread(bmpa_reader_thread, params);
}
private int
bmpa_reader_open_render_device(gx_device_printer *ppdev)
{
return gdev_prn_async_render_open(ppdev);
}
private int
bmpa_reader_output_page(gx_device *pdev, int num_copies, int flush)
{
if ( num_copies > 0 || !flush ) {
int code = gdev_prn_open_printer_positionable(pdev, 1, 1);
if ( code < 0 )
return code;
}
return gdev_prn_output_page(pdev, num_copies, flush);
}
private int
bmpa_reader_print_planes(gx_device_printer *pdev, FILE *prn_stream,
int num_copies, int first_plane, int last_plane,
int raster)
{
gx_device_async * const prdev = (gx_device_async *)pdev;
uint bmp_raster = raster + (-raster & 3);
int code = 0;
int y;
byte *row = 0;
byte *raster_data;
int plane;
if (prdev->buffered_page_exists) {
code = bmpa_reader_buffer_planes(pdev, prn_stream, num_copies,
first_plane, last_plane, raster);
goto done;
}
#ifdef SINGLE_PAGE
if (ftell(prn_stream) != 0)
return 0;
#endif
row = gs_alloc_bytes(pdev->memory, bmp_raster, "bmp file buffer");
if (row == 0)
return_error(gs_error_VMerror);
for (plane = first_plane; plane <= last_plane; ++plane) {
gx_render_plane_t render_plane;
code =
(first_plane < 0 ? write_bmp_header(pdev, prn_stream) :
write_bmp_separated_header(pdev, prn_stream));
if (code < 0)
goto done;
if ((prdev->file_offset_to_data[plane - first_plane] =
ftell(prn_stream)) == -1L) {
code = gs_note_error(gs_error_ioerror);
goto done;
}
if (plane >= 0)
gx_render_plane_init(&render_plane, (gx_device *)pdev, plane);
for (y = prdev->height - 1; y >= 0; y--) {
uint actual_raster;
code = gdev_prn_get_lines(pdev, y, 1, row, bmp_raster,
&raster_data, &actual_raster,
(plane < 0 ? NULL : &render_plane));
if (code < 0)
goto done;
if (fwrite((const char *)raster_data, actual_raster, 1, prn_stream) < 1) {
code = gs_error_ioerror;
goto done;
}
}
}
done:
gs_free_object(pdev->memory, row, "bmp file buffer");
prdev->buffered_page_exists = 0;
return code;
}
private int
bmpa_reader_print_page_copies(gx_device_printer *pdev, FILE *prn_stream,
int num_copies)
{
return bmpa_reader_print_planes(pdev, prn_stream, num_copies, -1, -1,
gdev_prn_raster(pdev));
}
private int
bmpa_cmyk_plane_raster(gx_device_printer *pdev)
{
return bitmap_raster(pdev->width * (pdev->color_info.depth / 4));
}
private int
bmpa_cmyk_reader_print_copies(gx_device_printer *pdev, FILE *prn_stream,
int num_copies)
{
return bmpa_reader_print_planes(pdev, prn_stream, num_copies, 0, 3,
bmpa_cmyk_plane_raster(pdev));
}
private int
bmpa_reader_buffer_planes(gx_device_printer *pdev, FILE *file, int num_copies,
int first_plane, int last_plane, int raster)
{
gx_device_async * const prdev = (gx_device_async *)pdev;
gx_device * const dev = (gx_device *)pdev;
int code = 0;
if (!prdev->buffered_page_exists) {
code = bmpa_reader_print_planes(pdev, file, num_copies,
first_plane, last_plane, raster);
goto done;
}
{
byte *raster_data;
gx_device_clist_reader *const crdev =
(gx_device_clist_reader *)pdev;
int raster = gx_device_raster(dev, 1);
int padding = -raster & 3;
int bmp_raster = raster + padding;
int plane;
if (!pdev->buffer_space) {
code = gs_note_error(gs_error_Fatal);
goto done;
}
raster_data = crdev->data;
for (plane = first_plane; plane <= last_plane; ++plane) {
gx_render_plane_t render_plane;
gx_device *bdev;
int y, band_base_line;
if (fseek(file, prdev->file_offset_to_data[plane - first_plane],
SEEK_SET)) {
code = gs_note_error(gs_error_ioerror);
goto done;
}
if (plane >= 0)
gx_render_plane_init(&render_plane, (gx_device *)pdev, plane);
else
render_plane.index = -1;
code = gdev_create_buf_device(crdev->buf_procs.create_buf_device,
&bdev, crdev->target, &render_plane,
dev->memory, true);
if (code < 0)
goto done;
for (y = dev->height - 1; y >= 0; y = band_base_line - 1) {
int band_height =
dev_proc(dev, get_band)(dev, y, &band_base_line);
int line;
gs_int_rect band_rect;
code = crdev->buf_procs.setup_buf_device
(bdev, raster_data, bmp_raster, NULL, 0, band_height,
band_height);
if (code < 0)
goto done;
for (line = band_height - 1; line >= 0; --line)
if (fread(raster_data + line * bmp_raster,
raster, 1, file) < 1 ||
fseek(file, padding, SEEK_CUR)
) {
code = gs_note_error(gs_error_ioerror);
goto done;
}
band_rect.p.x = 0;
band_rect.p.y = band_base_line;
band_rect.q.x = pdev->width;
band_rect.q.y = band_base_line + band_height;
if ((code = clist_render_rectangle((gx_device_clist *)pdev,
&band_rect, bdev,
&render_plane, false)) < 0)
goto done;
if (fseek(file, -bmp_raster * band_height, SEEK_CUR)) {
code = gs_note_error(gs_error_ioerror);
goto done;
}
for (line = band_height - 1; line >= 0; --line) {
if (fwrite(raster_data + line * bmp_raster,
bmp_raster, 1, file) < 1 ||
fseek(file, padding, SEEK_CUR)
) {
code = gs_note_error(gs_error_ioerror);
goto done;
}
}
}
crdev->buf_procs.destroy_buf_device(bdev);
}
}
done:
prdev->buffered_page_exists = (code >= 0);
return code;
}
private int
bmpa_reader_buffer_page(gx_device_printer *pdev, FILE *prn_stream,
int num_copies)
{
return bmpa_reader_buffer_planes(pdev, prn_stream, num_copies, -1, -1,
gdev_prn_raster(pdev));
}
private int
bmpa_cmyk_reader_buffer_page(gx_device_printer *pdev, FILE *prn_stream,
int num_copies)
{
return bmpa_reader_buffer_planes(pdev, prn_stream, num_copies, 0, 3,
bmpa_cmyk_plane_raster(pdev));
}
private void
bmpa_get_space_params(const gx_device_printer *pdev,
gdev_prn_space_params *space_params)
{
int render_space;
int writer_space;
const int tile_cache_space = 50 * 1024;
const int min_image_rows = 2;
int min_row_space =
min_image_rows * ( 4 * ( pdev->width + sizeof(int) - 1 ) );
int min_band_height = max(1, pdev->height / 100);
space_params->band.BandWidth = pdev->width;
space_params->band.BandHeight = min_band_height;
render_space = gdev_mem_data_size( (const gx_device_memory *)pdev,
space_params->band.BandWidth,
space_params->band.BandHeight );
writer_space =
5000 + (72 + 8) * ( (pdev->height / space_params->band.BandHeight) + 1 );
space_params->band.BandBufferSpace =
max(render_space, writer_space) + tile_cache_space;
space_params->BufferSpace =
max(render_space, writer_space + min_row_space) + tile_cache_space;
space_params->BufferSpace = space_params->band.BandBufferSpace;
}
private int
bmpa_get_params(gx_device * pdev, gs_param_list * plist)
{
gx_device_async * const bdev = (gx_device_async *)pdev;
return gdev_prn_get_params_planar(pdev, plist, &bdev->UsePlanarBuffer);
}
private int
bmpa_put_params(gx_device *pdev, gs_param_list *plist)
{
gx_device_async * const bdev = (gx_device_async *)pdev;
return gdev_prn_put_params_planar(pdev, plist, &bdev->UsePlanarBuffer);
}
private int
bmpa_get_hardware_params(gx_device *dev, gs_param_list *plist)
{
static const char *const test_value = "Test value";
static const char *const test_name = "TestValue";
int code = 0;
if ( param_requested(plist, test_name) ) {
gs_param_string param_str;
param_string_from_string(param_str, test_value);
code = param_write_string(plist, test_name, &param_str);
}
return code;
}