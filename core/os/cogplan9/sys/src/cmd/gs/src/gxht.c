#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsbitops.h"
#include "gsutil.h"
#include "gxdcolor.h"
#include "gxfixed.h"
#include "gxdevice.h"
#include "gxistate.h"
#include "gzht.h"
#include "gsserial.h"
#define max_cached_tiles_HUGE 5000
#define max_ht_bits_HUGE 1000000
#define max_cached_tiles_LARGE 577
#define max_ht_bits_LARGE 100000
#define max_cached_tiles_SMALL 25
#define max_ht_bits_SMALL 1000
gs_public_st_composite(st_dc_ht_binary, gx_device_color, "dc_ht_binary",
dc_ht_binary_enum_ptrs, dc_ht_binary_reloc_ptrs);
private dev_color_proc_save_dc(gx_dc_ht_binary_save_dc);
private dev_color_proc_get_dev_halftone(gx_dc_ht_binary_get_dev_halftone);
private dev_color_proc_load(gx_dc_ht_binary_load);
private dev_color_proc_fill_rectangle(gx_dc_ht_binary_fill_rectangle);
private dev_color_proc_fill_masked(gx_dc_ht_binary_fill_masked);
private dev_color_proc_equal(gx_dc_ht_binary_equal);
private dev_color_proc_write(gx_dc_ht_binary_write);
private dev_color_proc_read(gx_dc_ht_binary_read);
const gx_device_color_type_t
gx_dc_type_data_ht_binary =
{&st_dc_ht_binary,
gx_dc_ht_binary_save_dc, gx_dc_ht_binary_get_dev_halftone,
gx_dc_ht_get_phase,
gx_dc_ht_binary_load, gx_dc_ht_binary_fill_rectangle,
gx_dc_ht_binary_fill_masked, gx_dc_ht_binary_equal,
gx_dc_ht_binary_write, gx_dc_ht_binary_read,
gx_dc_ht_binary_get_nonzero_comps
};
#undef gx_dc_type_ht_binary
const gx_device_color_type_t *const gx_dc_type_ht_binary =
&gx_dc_type_data_ht_binary;
#define gx_dc_type_ht_binary (&gx_dc_type_data_ht_binary)
private
ENUM_PTRS_WITH(dc_ht_binary_enum_ptrs, gx_device_color *cptr) return 0;
ENUM_PTR(0, gx_device_color, colors.binary.b_ht);
case 1:
{
gx_ht_tile *tile = cptr->colors.binary.b_tile;
ENUM_RETURN(tile ? tile - tile->index : 0);
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(dc_ht_binary_reloc_ptrs, gx_device_color *cptr)
{
gx_ht_tile *tile = cptr->colors.binary.b_tile;
uint index = tile ? tile->index : 0;
RELOC_PTR(gx_device_color, colors.binary.b_ht);
RELOC_TYPED_OFFSET_PTR(gx_device_color, colors.binary.b_tile, index);
}
RELOC_PTRS_END
#undef cptr
private_st_ht_tiles();
private
ENUM_PTRS_BEGIN_PROC(ht_tiles_enum_ptrs)
{
return 0;
}
ENUM_PTRS_END_PROC
private RELOC_PTRS_BEGIN(ht_tiles_reloc_ptrs)
{
gx_ht_tile *ht_tiles = vptr;
byte *bits = ht_tiles->tiles.data;
uint diff;
if (bits == 0)
return;
RELOC_VAR(bits);
if (size == size_of(gx_ht_tile)) {
ht_tiles->tiles.data = bits;
return;
}
diff = ht_tiles[1].tiles.data - ht_tiles[0].tiles.data;
for (; size; ht_tiles++, size -= size_of(gx_ht_tile), bits += diff) {
ht_tiles->tiles.data = bits;
}
}
RELOC_PTRS_END
private_st_ht_cache();
uint
gx_ht_cache_default_tiles(void)
{
#if arch_small_memory
return max_cached_tiles_SMALL;
#else
return (gs_debug_c('.') ? max_cached_tiles_SMALL :
max_cached_tiles_LARGE);
#endif
}
uint
gx_ht_cache_default_bits(void)
{
#if arch_small_memory
return max_ht_bits_SMALL;
#else
return (gs_debug_c('.') ? max_ht_bits_SMALL :
max_ht_bits_LARGE);
#endif
}
gx_ht_cache *
gx_ht_alloc_cache(gs_memory_t * mem, uint max_tiles, uint max_bits)
{
gx_ht_cache *pcache =
gs_alloc_struct(mem, gx_ht_cache, &st_ht_cache,
"alloc_ht_cache(struct)");
byte *tbits =
gs_alloc_bytes(mem, max_bits, "alloc_ht_cache(bits)");
gx_ht_tile *ht_tiles =
gs_alloc_struct_array(mem, max_tiles, gx_ht_tile, &st_ht_tiles,
"alloc_ht_cache(ht_tiles)");
if (pcache == 0 || tbits == 0 || ht_tiles == 0) {
gs_free_object(mem, ht_tiles, "alloc_ht_cache(ht_tiles)");
gs_free_object(mem, tbits, "alloc_ht_cache(bits)");
gs_free_object(mem, pcache, "alloc_ht_cache(struct)");
return 0;
}
pcache->bits = tbits;
pcache->bits_size = max_bits;
pcache->ht_tiles = ht_tiles;
pcache->num_tiles = max_tiles;
pcache->order.cache = pcache;
pcache->order.transfer = 0;
gx_ht_clear_cache(pcache);
return pcache;
}
void
gx_ht_free_cache(gs_memory_t * mem, gx_ht_cache * pcache)
{
gs_free_object(mem, pcache->ht_tiles, "free_ht_cache(ht_tiles)");
gs_free_object(mem, pcache->bits, "free_ht_cache(bits)");
gs_free_object(mem, pcache, "free_ht_cache(struct)");
}
bool
gx_check_tile_cache_current(const gs_imager_state * pis)
{
return false;
}
bool
gx_check_tile_cache(const gs_imager_state * pis)
{
return false;
}
int
gx_check_tile_size(const gs_imager_state * pis, int w, int y, int h,
gs_color_select_t select, int *ppx)
{
return -1;
}
private int render_ht(gx_ht_tile *, int, const gx_ht_order *,
gx_bitmap_id);
private gx_ht_tile *
gx_render_ht_default(gx_ht_cache * pcache, int b_level)
{
const gx_ht_order *porder = &pcache->order;
int level = porder->levels[b_level];
gx_ht_tile *bt = &pcache->ht_tiles[level / pcache->levels_per_tile];
if (bt->level != level) {
int code = render_ht(bt, level, porder, pcache->base_id + b_level);
if (code < 0)
return 0;
}
return bt;
}
private gx_ht_tile *
gx_render_ht_1_tile(gx_ht_cache * pcache, int b_level)
{
const gx_ht_order *porder = &pcache->order;
int level = porder->levels[b_level];
gx_ht_tile *bt = &pcache->ht_tiles[0];
if (bt->level != level) {
int code = render_ht(bt, level, porder, pcache->base_id + b_level);
if (code < 0)
return 0;
}
return bt;
}
private gx_ht_tile *
gx_render_ht_1_level(gx_ht_cache * pcache, int b_level)
{
const gx_ht_order *porder = &pcache->order;
int level = porder->levels[b_level];
gx_ht_tile *bt = &pcache->ht_tiles[level];
if (bt->level != level) {
int code = render_ht(bt, level, porder, pcache->base_id + b_level);
if (code < 0)
return 0;
}
return bt;
}
private void
gx_dc_ht_binary_save_dc(const gx_device_color * pdevc,
gx_device_color_saved * psdc)
{
psdc->type = pdevc->type;
psdc->colors.binary.b_color[0] = pdevc->colors.binary.color[0];
psdc->colors.binary.b_color[1] = pdevc->colors.binary.color[1];
psdc->colors.binary.b_level = pdevc->colors.binary.b_level;
psdc->colors.binary.b_index = pdevc->colors.binary.b_index;
psdc->phase = pdevc->phase;
}
private const gx_device_halftone *
gx_dc_ht_binary_get_dev_halftone(const gx_device_color * pdevc)
{
return pdevc->colors.binary.b_ht;
}
private int
gx_dc_ht_binary_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * dev, gs_color_select_t select)
{
int component_index = pdevc->colors.binary.b_index;
const gx_ht_order *porder =
(component_index < 0 ?
&pdevc->colors.binary.b_ht->order :
&pdevc->colors.binary.b_ht->components[component_index].corder);
gx_ht_cache *pcache = porder->cache;
if (pcache->order.bit_data != porder->bit_data)
gx_ht_init_cache(pis->memory, pcache, porder);
pdevc->colors.binary.b_tile = NULL;
return 0;
}
private int
gx_dc_ht_binary_load_cache(const gx_device_color * pdevc)
{
int component_index = pdevc->colors.binary.b_index;
const gx_ht_order *porder =
&pdevc->colors.binary.b_ht->components[component_index].corder;
gx_ht_cache *pcache = porder->cache;
int b_level = pdevc->colors.binary.b_level;
int level = porder->levels[b_level];
gx_ht_tile *bt = &pcache->ht_tiles[level / pcache->levels_per_tile];
if (bt->level != level) {
int code = render_ht(bt, level, porder, pcache->base_id + b_level);
if (code < 0)
return_error(gs_error_Fatal);
}
((gx_device_color *)pdevc)->colors.binary.b_tile = bt;
return 0;
}
private int
gx_dc_ht_binary_fill_rectangle(const gx_device_color * pdevc, int x, int y,
int w, int h, gx_device * dev, gs_logical_operation_t lop,
const gx_rop_source_t * source)
{
gx_rop_source_t no_source;
gx_dc_ht_binary_load_cache(pdevc);
if (dev->color_info.depth > 1)
lop &= ~lop_T_transparent;
if (source == NULL && lop_no_S_is_T(lop))
return (*dev_proc(dev, strip_tile_rectangle)) (dev,
&pdevc->colors.binary.b_tile->tiles,
x, y, w, h, pdevc->colors.binary.color[0],
pdevc->colors.binary.color[1],
pdevc->phase.x, pdevc->phase.y);
if (pdevc->colors.binary.color[0] == gx_no_color_index)
lop = rop3_use_D_when_T_0(lop);
if (pdevc->colors.binary.color[1] == gx_no_color_index)
lop = rop3_use_D_when_T_1(lop);
if (source == NULL)
set_rop_no_source(source, no_source, dev);
return (*dev_proc(dev, strip_copy_rop)) (dev, source->sdata,
source->sourcex, source->sraster, source->id,
(source->use_scolors ? source->scolors : NULL),
&pdevc->colors.binary.b_tile->tiles,
pdevc->colors.binary.color,
x, y, w, h, pdevc->phase.x, pdevc->phase.y,
lop);
}
private int
gx_dc_ht_binary_fill_masked(const gx_device_color * pdevc, const byte * data,
int data_x, int raster, gx_bitmap_id id, int x, int y, int w, int h,
gx_device * dev, gs_logical_operation_t lop, bool invert)
{
int code = gx_dc_ht_binary_load_cache(pdevc);
if (code < 0)
return code;
return gx_dc_default_fill_masked(pdevc, data, data_x, raster, id,
x, y, w, h, dev, lop, invert);
}
private bool
gx_dc_ht_binary_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
return pdevc2->type == pdevc1->type &&
pdevc1->phase.x == pdevc2->phase.x &&
pdevc1->phase.y == pdevc2->phase.y &&
gx_dc_binary_color0(pdevc1) == gx_dc_binary_color0(pdevc2) &&
gx_dc_binary_color1(pdevc1) == gx_dc_binary_color1(pdevc2) &&
pdevc1->colors.binary.b_level == pdevc2->colors.binary.b_level;
}
private const int   dc_ht_binary_has_color0 = 0x01;
private const int   dc_ht_binary_has_color1 = 0x02;
private const int   dc_ht_binary_has_level = 0x04;
private const int   dc_ht_binary_has_index = 0x08;
private int
gx_dc_ht_binary_write(
const gx_device_color *         pdevc,
const gx_device_color_saved *   psdc0,
const gx_device *               dev,
byte *                          pdata,
uint *                          psize )
{
int                             req_size = 1;
int                             flag_bits = 0;
uint                            tmp_size;
byte *                          pdata0 = pdata;
const gx_device_color_saved *   psdc = psdc0;
int                             code;
if (psdc != 0 && psdc->type != pdevc->type)
psdc = 0;
if ( psdc == 0                                                      ||
pdevc->colors.binary.color[0] != psdc->colors.binary.b_color[0]  ) {
flag_bits |= dc_ht_binary_has_color0;
tmp_size = 0;
(void)gx_dc_write_color( pdevc->colors.binary.color[0],
dev,
pdata,
&tmp_size );
req_size += tmp_size;
}
if ( psdc == 0                                                      ||
pdevc->colors.binary.color[1] != psdc->colors.binary.b_color[1]  ) {
flag_bits |= dc_ht_binary_has_color1;
tmp_size = 0;
(void)gx_dc_write_color( pdevc->colors.binary.color[1],
dev,
pdata,
&tmp_size );
req_size += tmp_size;
}
if ( psdc == 0                                                  ||
pdevc->colors.binary.b_level != psdc->colors.binary.b_level  ) {
flag_bits |= dc_ht_binary_has_level;
req_size += enc_u_sizew(pdevc->colors.binary.b_level);
}
if ( psdc == 0                                                  ||
pdevc->colors.binary.b_index != psdc->colors.binary.b_index  ) {
flag_bits |= dc_ht_binary_has_index;
req_size += 1;
}
if (flag_bits == 0) {
*psize = 0;
return 1;
}
if (req_size > *psize) {
*psize = req_size;
return gs_error_rangecheck;
}
*pdata++ = (byte)flag_bits;
if ((flag_bits & dc_ht_binary_has_color0) != 0) {
tmp_size = req_size - (pdata - pdata0);
code = gx_dc_write_color( pdevc->colors.binary.color[0],
dev,
pdata,
&tmp_size );
if (code < 0)
return code;
pdata += tmp_size;
}
if ((flag_bits & dc_ht_binary_has_color1) != 0) {
tmp_size = req_size - (pdata - pdata0);
code = gx_dc_write_color( pdevc->colors.binary.color[1],
dev,
pdata,
&tmp_size );
if (code < 0)
return code;
pdata += tmp_size;
}
if ((flag_bits & dc_ht_binary_has_level) != 0)
enc_u_putw(pdevc->colors.binary.b_level, pdata);
if ((flag_bits & dc_ht_binary_has_index) != 0)
*pdata++ = pdevc->colors.binary.b_index;
*psize = pdata - pdata0;
return 0;
}
private int
gx_dc_ht_binary_read(
gx_device_color *       pdevc,
const gs_imager_state * pis,
const gx_device_color * prior_devc,
const gx_device *       dev,
const byte *            pdata,
uint                    size,
gs_memory_t *           mem )
{
gx_device_color         devc;
const byte *            pdata0 = pdata;
int                     code, flag_bits;
if (prior_devc != 0 && prior_devc->type == gx_dc_type_ht_binary)
devc = *prior_devc;
else
memset(&devc, 0, sizeof(devc));
devc.type = gx_dc_type_ht_binary;
devc.colors.binary.b_ht = pis->dev_ht;
devc.colors.binary.b_tile = 0;
if (size == 0)
return_error(gs_error_rangecheck);
size --;
flag_bits = *pdata++;
if ((flag_bits & dc_ht_binary_has_color0) != 0) {
code = gx_dc_read_color( &devc.colors.binary.color[0],
dev,
pdata,
size );
if (code < 0)
return code;
size -= code;
pdata += code;
}
if ((flag_bits & dc_ht_binary_has_color1) != 0) {
code = gx_dc_read_color( &devc.colors.binary.color[1],
dev,
pdata,
size );
if (code < 0)
return code;
size -= code;
pdata += code;
}
if ((flag_bits & dc_ht_binary_has_level) != 0) {
const byte *    pdata_start = pdata;
if (size < 1)
return_error(gs_error_rangecheck);
enc_u_getw(devc.colors.binary.b_level, pdata);
size -= pdata - pdata_start;
}
if ((flag_bits & dc_ht_binary_has_index) != 0) {
if (size == 0)
return_error(gs_error_rangecheck);
--size;
devc.colors.binary.b_index = *pdata++;
}
color_set_phase_mod( &devc,
pis->screen_phase[0].x,
pis->screen_phase[0].y,
pis->dev_ht->lcm_width,
pis->dev_ht->lcm_height );
*pdevc = devc;
return pdata - pdata0;
}
int
gx_dc_ht_binary_get_nonzero_comps(
const gx_device_color * pdevc,
const gx_device *       dev,
gx_color_index *        pcomp_bits )
{
int                     code;
gx_color_value          cvals_0[GX_DEVICE_COLOR_MAX_COMPONENTS],
cvals_1[GX_DEVICE_COLOR_MAX_COMPONENTS];
if ( (code = dev_proc(dev, decode_color)( (gx_device *)dev,
pdevc->colors.binary.color[0],
cvals_0 )) >= 0 &&
(code = dev_proc(dev, decode_color)( (gx_device *)dev,
pdevc->colors.binary.color[1],
cvals_1 )) >= 0   ) {
int     i, ncomps = dev->color_info.num_components;
int     mask = 0x1, comp_bits = 0;
for (i = 0; i < ncomps; i++, mask <<= 1) {
if (cvals_0[i] != 0 || cvals_1[i] != 0)
comp_bits |= mask;
}
*pcomp_bits = comp_bits;
code = 0;
}
return code;
}
void
gx_ht_init_cache(const gs_memory_t *mem, gx_ht_cache * pcache, const gx_ht_order * porder)
{
uint width = porder->width;
uint height = porder->height;
uint size = width * height + 1;
int width_unit =
(width <= ht_mask_bits / 2 ? ht_mask_bits / width * width :
width);
int height_unit = height;
uint raster = porder->raster;
uint tile_bytes = raster * height;
uint shift = porder->shift;
int num_cached;
int i;
byte *tbits = pcache->bits;
if (porder->num_bits >= size)
size = porder->num_bits + 1;
num_cached = pcache->bits_size / tile_bytes;
if (num_cached > size)
num_cached = size;
if (num_cached > pcache->num_tiles)
num_cached = pcache->num_tiles;
if (num_cached == size &&
tile_bytes * num_cached <= pcache->bits_size / 2
) {
uint rep_raster =
((pcache->bits_size / num_cached) / height) &
~(align_bitmap_mod - 1);
uint rep_count = rep_raster * 8 / width;
if (rep_count > sizeof(ulong) * 8)
rep_count = sizeof(ulong) * 8;
width_unit = width * rep_count;
raster = bitmap_raster(width_unit);
tile_bytes = raster * height;
}
pcache->base_id = gs_next_ids(mem, porder->num_levels + 1);
pcache->order = *porder;
pcache->order.transfer = 0;
pcache->num_cached = num_cached;
pcache->levels_per_tile = (size + num_cached - 1) / num_cached;
pcache->tiles_fit = -1;
memset(tbits, 0, pcache->bits_size);
for (i = 0; i < num_cached; i++, tbits += tile_bytes) {
register gx_ht_tile *bt = &pcache->ht_tiles[i];
bt->level = 0;
bt->index = i;
bt->tiles.data = tbits;
bt->tiles.raster = raster;
bt->tiles.size.x = width_unit;
bt->tiles.size.y = height_unit;
bt->tiles.rep_width = width;
bt->tiles.rep_height = height;
bt->tiles.shift = bt->tiles.rep_shift = shift;
}
pcache->render_ht =
(pcache->num_tiles == 1 ? gx_render_ht_1_tile :
pcache->levels_per_tile == 1 ? gx_render_ht_1_level :
gx_render_ht_default);
}
private int
render_ht(gx_ht_tile * pbt, int level  ,
const gx_ht_order * porder, gx_bitmap_id new_id)
{
byte *data = pbt->tiles.data;
int code;
if_debug7('H', "[H]Halftone cache slot 0x%lx: old=%d, new=%d, w=%d(%d), h=%d(%d):\n",
(ulong) data, pbt->level, level,
pbt->tiles.size.x, porder->width,
pbt->tiles.size.y, porder->num_bits / porder->width);
#ifdef DEBUG
if (level < 0 || level > porder->num_bits) {
lprintf3("Error in render_ht: level=%d, old level=%d, num_bits=%d\n",
level, pbt->level, porder->num_bits);
return_error(gs_error_Fatal);
}
#endif
code = porder->procs->render(pbt, level, porder);
if (code < 0)
return code;
pbt->level = level;
pbt->tiles.id = new_id;
if (pbt->tiles.raster > porder->raster)
bits_replicate_horizontally(data, pbt->tiles.rep_width,
pbt->tiles.rep_height, porder->raster,
pbt->tiles.size.x, pbt->tiles.raster);
if (pbt->tiles.size.y > pbt->tiles.rep_height &&
pbt->tiles.shift == 0
)
bits_replicate_vertically(data, pbt->tiles.rep_height,
pbt->tiles.raster, pbt->tiles.size.y);
#ifdef DEBUG
if (gs_debug_c('H')) {
const byte *p = pbt->tiles.data;
int wb = pbt->tiles.raster;
const byte *ptr = p + wb * pbt->tiles.size.y;
while (p < ptr) {
dprintf8(" %d%d%d%d%d%d%d%d",
*p >> 7, (*p >> 6) & 1, (*p >> 5) & 1,
(*p >> 4) & 1, (*p >> 3) & 1, (*p >> 2) & 1,
(*p >> 1) & 1, *p & 1);
if ((++p - data) % wb == 0)
dputc('\n');
}
}
#endif
return 0;
}