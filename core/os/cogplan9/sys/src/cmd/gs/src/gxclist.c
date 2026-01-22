#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxcldev.h"
#include "gxclpath.h"
#include "gsparams.h"
#include "gxdcolor.h"
#define CLIST_IS_WRITER(cdev) ((cdev)->common.ymin < 0)
extern_st(st_imager_state);
private
ENUM_PTRS_WITH(device_clist_enum_ptrs, gx_device_clist *cdev)
if (index < st_device_forward_max_ptrs) {
gs_ptr_type_t ret = ENUM_USING_PREFIX(st_device_forward, 0);
return (ret ? ret : ENUM_OBJ(0));
}
if (!CLIST_IS_WRITER(cdev))
return 0;
index -= st_device_forward_max_ptrs;
switch (index) {
case 0: return ENUM_OBJ((cdev->writer.image_enum_id != gs_no_id ?
cdev->writer.clip_path : 0));
case 1: return ENUM_OBJ((cdev->writer.image_enum_id != gs_no_id ?
cdev->writer.color_space.space : 0));
default:
return ENUM_USING(st_imager_state, &cdev->writer.imager_state,
sizeof(gs_imager_state), index - 2);
}
ENUM_PTRS_END
private
RELOC_PTRS_WITH(device_clist_reloc_ptrs, gx_device_clist *cdev)
{
RELOC_PREFIX(st_device_forward);
if (!CLIST_IS_WRITER(cdev))
return;
if (cdev->writer.image_enum_id != gs_no_id) {
RELOC_VAR(cdev->writer.clip_path);
RELOC_VAR(cdev->writer.color_space.space);
}
RELOC_USING(st_imager_state, &cdev->writer.imager_state,
sizeof(gs_imager_state));
} RELOC_PTRS_END
public_st_device_clist();
private dev_proc_open_device(clist_open);
private dev_proc_output_page(clist_output_page);
private dev_proc_close_device(clist_close);
private dev_proc_get_band(clist_get_band);
private int clist_put_current_params(gx_device_clist_writer *cldev);
const gx_device_procs gs_clist_device_procs = {
clist_open,
gx_forward_get_initial_matrix,
gx_default_sync_output,
clist_output_page,
clist_close,
gx_forward_map_rgb_color,
gx_forward_map_color_rgb,
clist_fill_rectangle,
gx_default_tile_rectangle,
clist_copy_mono,
clist_copy_color,
gx_default_draw_line,
gx_default_get_bits,
gx_forward_get_params,
gx_forward_put_params,
gx_forward_map_cmyk_color,
gx_forward_get_xfont_procs,
gx_forward_get_xfont_device,
gx_forward_map_rgb_alpha_color,
gx_forward_get_page_device,
gx_forward_get_alpha_bits,
clist_copy_alpha,
clist_get_band,
gx_default_copy_rop,
clist_fill_path,
clist_stroke_path,
clist_fill_mask,
gx_default_fill_trapezoid,
clist_fill_parallelogram,
clist_fill_triangle,
gx_default_draw_thin_line,
gx_default_begin_image,
gx_default_image_data,
gx_default_end_image,
clist_strip_tile_rectangle,
clist_strip_copy_rop,
gx_forward_get_clipping_box,
clist_begin_typed_image,
clist_get_bits_rectangle,
gx_forward_map_color_rgb_alpha,
clist_create_compositor,
gx_forward_get_hardware_params,
gx_default_text_begin,
gx_default_finish_copydevice,
NULL,
NULL,
NULL,
NULL,
NULL,
gx_forward_get_color_mapping_procs,
gx_forward_get_color_comp_index,
gx_forward_encode_color,
gx_forward_decode_color,
gx_default_pattern_manage,
gx_default_fill_rectangle_hl_color,
gx_default_include_color_space,
gx_default_fill_linear_color_scanline,
gx_default_fill_linear_color_trapezoid,
gx_default_fill_linear_color_triangle,
gx_forward_update_spot_equivalent_colors
};
const gs_imager_state clist_imager_state_initial =
{gs_imager_state_initial(300.0 / 72.0)};
private uint
clist_tile_cache_size(const gx_device * target, uint data_size)
{
uint bits_size =
(data_size / 5) & -align_cached_bits_mod;
if (!gx_device_must_halftone(target)) {
bits_size -= bits_size >> 2;
}
#define min_bits_size 1024
if (bits_size < min_bits_size)
bits_size = min_bits_size;
#undef min_bits_size
return bits_size;
}
private int
clist_init_tile_cache(gx_device * dev, byte * init_data, ulong data_size)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
byte *data = init_data;
uint bits_size = data_size;
uint avg_char_size =
(uint)(dev->HWResolution[0] * dev->HWResolution[1] *
(0.5 * 10 / 72 * 10 / 72 / 8)) + 24;
uint hc = bits_size / avg_char_size;
uint hsize;
while ((hc + 1) & hc)
hc |= hc >> 1;
if (hc < 0xff)
hc = 0xff;
else if (hc > 0xfff)
hc = 0xfff;
while (hc >= 3 && (hsize = (hc + 1) * sizeof(tile_hash)) >= bits_size)
hc >>= 1;
if (hc < 3)
return_error(gs_error_rangecheck);
cdev->tile_hash_mask = hc;
cdev->tile_max_count = hc - (hc >> 2);
cdev->tile_table = (tile_hash *) data;
data += hsize;
bits_size -= hsize;
gx_bits_cache_chunk_init(&cdev->chunk, data, bits_size);
gx_bits_cache_init(&cdev->bits, &cdev->chunk);
return 0;
}
private int
clist_init_bands(gx_device * dev, gx_device_memory *bdev, uint data_size,
int band_width, int band_height)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int nbands;
if (gdev_mem_data_size(bdev, band_width, band_height) > data_size)
return_error(gs_error_rangecheck);
cdev->page_band_height = band_height;
nbands = (cdev->target->height + band_height - 1) / band_height;
cdev->nbands = nbands;
#ifdef DEBUG
if (gs_debug_c('l') | gs_debug_c(':'))
dlprintf4("[:]width=%d, band_width=%d, band_height=%d, nbands=%d\n",
bdev->width, band_width, band_height, nbands);
#endif
return 0;
}
private int
clist_init_states(gx_device * dev, byte * init_data, uint data_size)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
ulong state_size = cdev->nbands * (ulong) sizeof(gx_clist_state);
if (state_size + sizeof(cmd_prefix) + cmd_largest_size + 100 > data_size)
return_error(gs_error_rangecheck);
cdev->states = (gx_clist_state *) init_data;
cdev->cbuf = init_data + state_size;
cdev->cend = init_data + data_size;
return 0;
}
private int
clist_init_data(gx_device * dev, byte * init_data, uint data_size)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
gx_device *target = cdev->target;
const int band_width =
cdev->page_info.band_params.BandWidth =
(cdev->band_params.BandWidth ? cdev->band_params.BandWidth :
target->width);
int band_height = cdev->band_params.BandHeight;
bool page_uses_transparency = cdev->page_uses_transparency;
const uint band_space =
cdev->page_info.band_params.BandBufferSpace =
(cdev->band_params.BandBufferSpace ?
cdev->band_params.BandBufferSpace : data_size);
byte *data = init_data;
uint size = band_space;
uint bits_size;
gx_device_memory bdev;
gx_device *pbdev = (gx_device *)&bdev;
int code;
cdev->buf_procs.create_buf_device(&pbdev, target, NULL, NULL, true);
if (dev_proc(pbdev, copy_alpha) == gx_no_copy_alpha)
cdev->disable_mask |= clist_disable_copy_alpha;
if (band_height) {
uint band_data_size =
gdev_mem_data_size(&bdev, band_width, band_height);
if (band_data_size >= band_space)
return_error(gs_error_rangecheck);
bits_size = min(band_space - band_data_size, data_size >> 1);
} else {
bits_size = clist_tile_cache_size(target, band_space);
bits_size = min(bits_size, data_size >> 1);
band_height = gdev_mem_max_height(&bdev, band_width,
band_space - bits_size, page_uses_transparency);
if (band_height == 0)
return_error(gs_error_rangecheck);
}
code = clist_init_tile_cache(dev, data, bits_size);
if (code < 0)
return code;
cdev->page_tile_cache_size = bits_size;
data += bits_size;
size -= bits_size;
code = clist_init_bands(dev, &bdev, size, band_width, band_height);
if (code < 0)
return code;
return clist_init_states(dev, data, data_size - bits_size);
}
private int
clist_reset(gx_device * dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code = clist_init_data(dev, cdev->data, cdev->data_size);
int nbands;
if (code < 0)
return (cdev->permanent_error = code);
cdev->permanent_error = 0;
nbands = cdev->nbands;
cdev->ymin = cdev->ymax = -1;
memset(cdev->tile_table, 0, (cdev->tile_hash_mask + 1) *
sizeof(*cdev->tile_table));
cdev->cnext = cdev->cbuf;
cdev->ccl = 0;
cdev->band_range_list.head = cdev->band_range_list.tail = 0;
cdev->band_range_min = 0;
cdev->band_range_max = nbands - 1;
{
int band;
gx_clist_state *states = cdev->states;
for (band = 0; band < nbands; band++, states++) {
static const gx_clist_state cls_initial =
{cls_initial_values};
*states = cls_initial;
}
}
cdev->tile_band_mask_size =
((nbands + (align_bitmap_mod * 8 - 1)) >> 3) &
~(align_bitmap_mod - 1);
memset(&cdev->tile_params, 0, sizeof(cdev->tile_params));
cdev->tile_depth = 0;
cdev->tile_known_min = nbands;
cdev->tile_known_max = -1;
cdev->imager_state = clist_imager_state_initial;
cdev->clip_path = NULL;
cdev->clip_path_id = gs_no_id;
cdev->color_space.byte1 = 0;
cdev->color_space.id = gs_no_id;
cdev->color_space.space = 0;
{
int i;
for (i = 0; i < countof(cdev->transfer_ids); ++i)
cdev->transfer_ids[i] = gs_no_id;
}
cdev->black_generation_id = gs_no_id;
cdev->undercolor_removal_id = gs_no_id;
cdev->device_halftone_id = gs_no_id;
cdev->image_enum_id = gs_no_id;
return 0;
}
private int
clist_init(gx_device * dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code = clist_reset(dev);
if (code >= 0) {
cdev->image_enum_id = gs_no_id;
cdev->error_is_retryable = 0;
cdev->driver_call_nesting = 0;
cdev->ignore_lo_mem_warnings = 0;
}
return code;
}
private int
clist_reinit_output_file(gx_device *dev)
{ gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code = 0;
int b_block = sizeof(cmd_block) * (cdev->nbands + 2);
int c_block =
cdev->cend - cdev->cbuf + 2 + cdev->nbands * 2 + (cdev->nbands + 1);
if ( clist_test_VMerror_recoverable(cdev) )
{ if (cdev->page_bfile != 0)
code = clist_set_memory_warning(cdev->page_bfile, b_block);
if (code >= 0 && cdev->page_cfile != 0)
code = clist_set_memory_warning(cdev->page_cfile, c_block);
}
return code;
}
private int
clist_emit_page_header(gx_device *dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code = 0;
if ((cdev->disable_mask & clist_disable_pass_thru_params)) {
do
if ((code = clist_put_current_params(cdev)) >= 0)
break;
while ((code = clist_VMerror_recover(cdev, code)) >= 0);
cdev->permanent_error = (code < 0 ? code : 0);
if (cdev->permanent_error < 0)
cdev->error_is_retryable = 0;
}
return code;
}
private void
clist_reset_page(gx_device_clist_writer *cwdev)
{
cwdev->page_bfile_end_pos = 0;
cwdev->page_info.scan_lines_per_colors_used = 0;
memset(cwdev->page_info.band_colors_used, 0,
sizeof(cwdev->page_info.band_colors_used));
}
private int
clist_open_output_file(gx_device *dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
char fmode[4];
int code;
if (cdev->do_not_open_or_close_bandfiles)
return 0;
cdev->page_cfile = 0;
cdev->page_bfile = 0;
code = clist_init(dev);
if (code < 0)
return code;
strcpy(fmode, "w+");
strcat(fmode, gp_fmode_binary_suffix);
cdev->page_cfname[0] = 0;
cdev->page_bfname[0] = 0;
clist_reset_page(cdev);
if ((code = clist_fopen(cdev->page_cfname, fmode, &cdev->page_cfile,
cdev->bandlist_memory, cdev->bandlist_memory,
true)) < 0 ||
(code = clist_fopen(cdev->page_bfname, fmode, &cdev->page_bfile,
cdev->bandlist_memory, cdev->bandlist_memory,
true)) < 0 ||
(code = clist_reinit_output_file(dev)) < 0
) {
clist_close_output_file(dev);
cdev->permanent_error = code;
cdev->error_is_retryable = 0;
}
return code;
}
int
clist_close_page_info(gx_band_page_info_t *ppi)
{
if (ppi->cfile != NULL) {
clist_fclose(ppi->cfile, ppi->cfname, true);
ppi->cfile = NULL;
}
if (ppi->bfile != NULL) {
clist_fclose(ppi->bfile, ppi->bfname, true);
ppi->bfile = NULL;
}
return 0;
}
int
clist_close_output_file(gx_device *dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
return clist_close_page_info(&cdev->page_info);
}
private int
clist_open(gx_device *dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code;
cdev->permanent_error = 0;
code = clist_init(dev);
if (code < 0)
return code;
code = clist_open_output_file(dev);
if ( code >= 0)
code = clist_emit_page_header(dev);
return code;
}
private int
clist_close(gx_device *dev)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
if (cdev->do_not_open_or_close_bandfiles)
return 0;
return clist_close_output_file(dev);
}
private int
clist_output_page(gx_device * dev, int num_copies, int flush)
{
return_error(gs_error_Fatal);
}
int
clist_finish_page(gx_device *dev, bool flush)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int code;
if (flush) {
if (cdev->page_cfile != 0)
clist_rewind(cdev->page_cfile, true, cdev->page_cfname);
if (cdev->page_bfile != 0)
clist_rewind(cdev->page_bfile, true, cdev->page_bfname);
clist_reset_page(cdev);
} else {
if (cdev->page_cfile != 0)
clist_fseek(cdev->page_cfile, 0L, SEEK_END, cdev->page_cfname);
if (cdev->page_bfile != 0)
clist_fseek(cdev->page_bfile, 0L, SEEK_END, cdev->page_bfname);
}
code = clist_init(dev);
if (code >= 0)
code = clist_reinit_output_file(dev);
if (code >= 0)
code = clist_emit_page_header(dev);
return code;
}
int
clist_end_page(gx_device_clist_writer * cldev)
{
int code = cmd_write_buffer(cldev, cmd_opv_end_page);
cmd_block cb;
int ecode = 0;
if (code >= 0) {
cb.band_min = cb.band_max = cmd_band_end;
cb.pos = (cldev->page_cfile == 0 ? 0 : clist_ftell(cldev->page_cfile));
code = clist_fwrite_chars(&cb, sizeof(cb), cldev->page_bfile);
if (code > 0)
code = 0;
}
if (code >= 0) {
clist_compute_colors_used(cldev);
ecode |= code;
cldev->page_bfile_end_pos = clist_ftell(cldev->page_bfile);
}
if (code < 0)
ecode = code;
if (cldev->page_bfile != 0)
clist_set_memory_warning(cldev->page_bfile, 0);
if (cldev->page_cfile != 0)
clist_set_memory_warning(cldev->page_cfile, 0);
#ifdef DEBUG
if (gs_debug_c('l') | gs_debug_c(':'))
dlprintf2("[:]clist_end_page at cfile=%ld, bfile=%ld\n",
cb.pos, cldev->page_bfile_end_pos);
#endif
return 0;
}
void
clist_compute_colors_used(gx_device_clist_writer *cldev)
{
int nbands = cldev->nbands;
int bands_per_colors_used =
(nbands + PAGE_INFO_NUM_COLORS_USED - 1) /
PAGE_INFO_NUM_COLORS_USED;
int band;
cldev->page_info.scan_lines_per_colors_used =
cldev->page_band_height * bands_per_colors_used;
memset(cldev->page_info.band_colors_used, 0,
sizeof(cldev->page_info.band_colors_used));
for (band = 0; band < nbands; ++band) {
int entry = band / bands_per_colors_used;
cldev->page_info.band_colors_used[entry].or |=
cldev->states[band].colors_used.or;
cldev->page_info.band_colors_used[entry].slow_rop |=
cldev->states[band].colors_used.slow_rop;
}
}
int
clist_VMerror_recover(gx_device_clist_writer *cldev,
int old_error_code)
{
int code = old_error_code;
int pages_remain;
if (!clist_test_VMerror_recoverable(cldev) ||
!cldev->error_is_retryable ||
old_error_code != gs_error_VMerror
)
return old_error_code;
do {
pages_remain =
(*cldev->free_up_bandlist_memory)( (gx_device *)cldev, false );
if (pages_remain < 0) {
code = pages_remain;
break;
}
if (clist_reinit_output_file( (gx_device *)cldev ) == 0) {
code = pages_remain;
break;
}
} while (pages_remain);
if_debug1('L', "[L]soft flush of command list, status: %d\n", code);
return code;
}
int
clist_VMerror_recover_flush(gx_device_clist_writer *cldev,
int old_error_code)
{
int free_code = 0;
int reset_code = 0;
int code;
if (!clist_test_VMerror_recoverable(cldev) ||
old_error_code != gs_error_VMerror
)
return old_error_code;
free_code = (*cldev->free_up_bandlist_memory)( (gx_device *)cldev, true );
reset_code = clist_reset( (gx_device *)cldev );
if (reset_code >= 0)
reset_code = clist_open_output_file( (gx_device *)cldev );
if ( reset_code >= 0 &&
(cldev->disable_mask & clist_disable_pass_thru_params)
)
reset_code = clist_put_current_params(cldev);
if (reset_code < 0) {
cldev->permanent_error = reset_code;
cldev->error_is_retryable = 0;
}
code = (reset_code < 0 ? reset_code : free_code < 0 ? old_error_code : 0);
if_debug1('L', "[L]hard flush of command list, status: %d\n", code);
return code;
}
private int
clist_put_current_params(gx_device_clist_writer *cldev)
{
gx_device *target = cldev->target;
gs_c_param_list param_list;
int code;
if (cldev->permanent_error)
return cldev->permanent_error;
gs_c_param_list_write(&param_list, cldev->memory);
code = (*dev_proc(target, get_params))
(target, (gs_param_list *)&param_list);
if (code >= 0) {
gs_c_param_list_read(&param_list);
code = cmd_put_params( cldev, (gs_param_list *)&param_list );
}
gs_c_param_list_release(&param_list);
return code;
}
private int
clist_get_band(gx_device * dev, int y, int *band_start)
{
gx_device_clist_writer * const cdev =
&((gx_device_clist *)dev)->writer;
int band_height = cdev->page_band_height;
int start;
if (y < 0)
y = 0;
else if (y >= dev->height)
y = dev->height;
*band_start = start = y - y % band_height;
return min(dev->height - start, band_height);
}