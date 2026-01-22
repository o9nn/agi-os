#include "memory_.h"
#include "gx.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gsbitops.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxcldev.h"
#include "gxfmap.h"
#define CHAR_ALL_BANDS_COUNT max_ushort
uint
clist_bitmap_bytes(uint width_bits, uint height, int compression_mask,
uint * width_bytes, uint * raster)
{
uint full_raster = *raster = bitmap_raster(width_bits);
uint short_raster = (width_bits + 7) >> 3;
uint width_bytes_last;
if (compression_mask & cmd_mask_compress_any)
*width_bytes = width_bytes_last = full_raster;
else if (short_raster <= cmd_max_short_width_bytes ||
height <= 1 ||
(compression_mask & decompress_spread) != 0
)
*width_bytes = width_bytes_last = short_raster;
else
*width_bytes = full_raster, width_bytes_last = short_raster;
return
(height == 0 ? 0 : *width_bytes * (height - 1) + width_bytes_last);
}
private int
cmd_compress_bitmap(stream_state * st, const byte * data, uint width_bits,
uint raster, uint height, stream_cursor_write * pw)
{
uint width_bytes = bitmap_raster(width_bits);
int status = 0;
stream_cursor_read r;
r.ptr = data - 1;
if (raster == width_bytes) {
r.limit = r.ptr + raster * height;
status = (*st->template->process) (st, &r, pw, true);
} else {
uint y;
for (y = 1; (r.limit = r.ptr + width_bytes), y < height; ++y) {
status = (*st->template->process) (st, &r, pw, false);
if (status)
break;
if (r.ptr != r.limit) {
status = -1;
break;
}
r.ptr += raster - width_bytes;
}
if (status == 0)
status = (*st->template->process) (st, &r, pw, true);
}
if (st->template->release)
(*st->template->release) (st);
return status;
}
int
cmd_put_bits(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const byte * data, uint width_bits, uint height, uint raster, int op_size,
int compression_mask, byte ** pdp, uint * psize)
{
uint short_raster, full_raster;
uint short_size =
clist_bitmap_bytes(width_bits, height,
compression_mask & ~cmd_mask_compress_any,
&short_raster, &full_raster);
uint uncompressed_raster;
uint uncompressed_size =
clist_bitmap_bytes(width_bits, height, compression_mask,
&uncompressed_raster, &full_raster);
uint max_size = cbuf_size - op_size;
gs_memory_t *mem = cldev->memory;
byte *dp;
int compress = 0;
if (short_size >= 50 &&
(compression_mask & cmd_mask_compress_any) != 0 &&
(uncompressed_size <= max_size ||
(compression_mask & decompress_elsewhere) != 0)
) {
union ss_ {
stream_state ss;
stream_CFE_state cf;
stream_RLE_state rl;
} sstate;
int code;
int try_size = op_size + min(uncompressed_size, max_size);
*psize = try_size;
code = (pcls != 0 ?
set_cmd_put_op(dp, cldev, pcls, 0, try_size) :
set_cmd_put_all_op(dp, cldev, 0, try_size));
if (code < 0)
return code;
cmd_uncount_op(0, try_size);
if (compression_mask & (1 << cmd_compress_cfe)) {
clist_cfe_init(&sstate.cf,
uncompressed_raster << 3 ,
mem);
compress = cmd_compress_cfe;
} else if (compression_mask & (1 << cmd_compress_rle)) {
clist_rle_init(&sstate.rl);
compress = cmd_compress_rle;
}
if (compress) {
byte *wbase = dp + (op_size - 1);
stream_cursor_write w;
uint wmax = min(uncompressed_size, max_size);
int status;
w.ptr = wbase;
w.limit = w.ptr + min(wmax, short_size >> 1);
status = cmd_compress_bitmap((stream_state *) & sstate, data,
uncompressed_raster << 3 ,
raster, height, &w);
if (status == 0) {
uint wcount = w.ptr - wbase;
cmd_shorten_list_op(cldev,
(pcls ? &pcls->list : &cldev->band_range_list),
try_size - (op_size + wcount));
*psize = op_size + wcount;
goto out;
}
}
if (uncompressed_size > max_size) {
if_debug1 ('L', "[L]Uncompressed bits %u too large for buffer\n",
uncompressed_size);
cmd_shorten_list_op(cldev,
(pcls ? &pcls->list : &cldev->band_range_list),
try_size);
return_error(gs_error_limitcheck);
}
if (uncompressed_size != short_size) {
if_debug2 ('L', "[L]Shortening bits from %u to %u\n",
try_size, op_size + short_size);
cmd_shorten_list_op(cldev,
(pcls ? &pcls->list : &cldev->band_range_list),
try_size - (op_size + short_size));
*psize = op_size + short_size;
}
compress = 0;
} else if (uncompressed_size > max_size)
return_error(gs_error_limitcheck);
else {
int code;
*psize = op_size + short_size;
code = (pcls != 0 ?
set_cmd_put_op(dp, cldev, pcls, 0, *psize) :
set_cmd_put_all_op(dp, cldev, 0, *psize));
if (code < 0)
return code;
cmd_uncount_op(0, *psize);
}
bytes_copy_rectangle(dp + op_size, short_raster, data, raster,
short_raster, height);
out:
*pdp = dp;
return compress;
}
private uint
cmd_size_tile_params(const gx_strip_bitmap * tile)
{
return 2 + cmd_size_w(tile->rep_width) + cmd_size_w(tile->rep_height) +
(tile->rep_width == tile->size.x ? 0 :
cmd_size_w(tile->size.x / tile->rep_width)) +
(tile->rep_height == tile->size.y ? 0 :
cmd_size_w(tile->size.y / tile->rep_height)) +
(tile->rep_shift == 0 ? 0 : cmd_size_w(tile->rep_shift));
}
private void
cmd_store_tile_params(byte * dp, const gx_strip_bitmap * tile, int depth,
uint csize)
{
byte *p = dp + 2;
byte bd = cmd_depth_to_code(depth);
*dp = cmd_count_op(cmd_opv_set_tile_size, csize);
p = cmd_put_w(tile->rep_width, p);
p = cmd_put_w(tile->rep_height, p);
if (tile->rep_width != tile->size.x) {
p = cmd_put_w(tile->size.x / tile->rep_width, p);
bd |= 0x20;
}
if (tile->rep_height != tile->size.y) {
p = cmd_put_w(tile->size.y / tile->rep_height, p);
bd |= 0x40;
}
if (tile->rep_shift != 0) {
cmd_put_w(tile->rep_shift, p);
bd |= 0x80;
}
dp[1] = bd;
}
inline private int
cmd_put_tile_index(gx_device_clist_writer *cldev, gx_clist_state *pcls,
uint indx)
{
int idelta = indx - pcls->tile_index + 8;
byte *dp;
int code;
if (!(idelta & ~15)) {
code = set_cmd_put_op(dp, cldev, pcls,
cmd_op_delta_tile_index + idelta, 1);
if (code < 0)
return code;
} else {
code = set_cmd_put_op(dp, cldev, pcls,
cmd_op_set_tile_index + (indx >> 8), 2);
if (code < 0)
return code;
dp[1] = indx & 0xff;
}
if_debug2('L', "[L]writing index=%u, offset=%lu\n",
indx, cldev->tile_table[indx].offset);
return 0;
}
int
cmd_put_color_map(gx_device_clist_writer * cldev, cmd_map_index map_index,
int comp_num, const gx_transfer_map * map, gs_id * pid)
{
byte *dp;
int code;
if (map == 0) {
if (pid && *pid == gs_no_id)
return 0;
code = set_cmd_put_all_op(dp, cldev, cmd_opv_set_misc, 3);
if (code < 0)
return code;
dp[1] = cmd_set_misc_map + (cmd_map_none << 4) + map_index;
dp[2] = comp_num;
if (pid)
*pid = gs_no_id;
} else {
if (pid && map->id == *pid)
return 0;
if (map->proc == gs_identity_transfer) {
code = set_cmd_put_all_op(dp, cldev, cmd_opv_set_misc, 3);
if (code < 0)
return code;
dp[1] = cmd_set_misc_map + (cmd_map_identity << 4) + map_index;
dp[2] = comp_num;
} else {
code = set_cmd_put_all_op(dp, cldev, cmd_opv_set_misc,
3 + sizeof(map->values));
if (code < 0)
return code;
dp[1] = cmd_set_misc_map + (cmd_map_other << 4) + map_index;
dp[2] = comp_num;
memcpy(dp + 3, map->values, sizeof(map->values));
}
if (pid)
*pid = map->id;
}
return 0;
}
#define tile_id_hash(id) (id)
#define tile_hash_next(index) ((index) + 413)
typedef struct tile_loc_s {
uint index;
tile_slot *tile;
} tile_loc;
private bool
clist_find_bits(gx_device_clist_writer * cldev, gx_bitmap_id id, tile_loc * ploc)
{
uint index = tile_id_hash(id);
const tile_hash *table = cldev->tile_table;
uint mask = cldev->tile_hash_mask;
ulong offset;
for (; (offset = table[index &= mask].offset) != 0;
index = tile_hash_next(index)
) {
tile_slot *tile = (tile_slot *) (cldev->data + offset);
if (tile->id == id) {
ploc->index = index;
ploc->tile = tile;
return true;
}
}
ploc->index = index;
return false;
}
private void
clist_delete_tile(gx_device_clist_writer * cldev, tile_slot * slot)
{
tile_hash *table = cldev->tile_table;
uint mask = cldev->tile_hash_mask;
uint index = slot->index;
ulong offset;
if_debug2('L', "[L]deleting index=%u, offset=%lu\n",
index, (ulong) ((byte *) slot - cldev->data));
gx_bits_cache_free(&cldev->bits, (gx_cached_bits_head *) slot,
&cldev->chunk);
table[index].offset = 0;
while ((offset = table[index = tile_hash_next(index) & mask].offset) != 0) {
tile_slot *tile = (tile_slot *) (cldev->data + offset);
tile_loc loc;
if (!clist_find_bits(cldev, tile->id, &loc)) {
if_debug2('L', "[L]move-deleting index=%u, offset=%lu\n",
index, offset);
gx_bits_cache_free(&cldev->bits,
(gx_cached_bits_head *) (cldev->data + offset),
&cldev->chunk);
table[index].offset = 0;
}
}
}
private int
clist_add_tile(gx_device_clist_writer * cldev, const gx_strip_bitmap * tiles,
uint sraster, int depth)
{
uint raster = tiles->raster;
uint size_bytes = raster * tiles->size.y;
uint tsize =
sizeof(tile_slot) + cldev->tile_band_mask_size + size_bytes;
gx_cached_bits_head *slot_head;
#define slot ((tile_slot *)slot_head)
if (cldev->bits.csize == cldev->tile_max_count) {
gx_bits_cache_alloc(&cldev->bits, (ulong) cldev->chunk.size,
&slot_head);
if (slot_head == 0) {
cldev->bits.cnext = 0;
gx_bits_cache_alloc(&cldev->bits, (ulong) cldev->chunk.size,
&slot_head);
#ifdef DEBUG
if (slot_head == 0) {
lprintf("No entry to delete!\n");
return_error(gs_error_Fatal);
}
#endif
}
clist_delete_tile(cldev, slot);
}
while (gx_bits_cache_alloc(&cldev->bits, (ulong) tsize, &slot_head) < 0) {
if (slot_head == 0) {
if (cldev->bits.cnext == 0) {
return_error(gs_error_limitcheck);
}
cldev->bits.cnext = 0;
} else
clist_delete_tile(cldev, slot);
}
slot->cb_depth = depth;
slot->cb_raster = raster;
slot->width = tiles->rep_width;
slot->height = tiles->rep_height;
slot->shift = slot->rep_shift = tiles->rep_shift;
slot->x_reps = slot->y_reps = 1;
slot->id = tiles->id;
memset(ts_mask(slot), 0, cldev->tile_band_mask_size);
bytes_copy_rectangle(ts_bits(cldev, slot), raster,
tiles->data, sraster,
(tiles->rep_width * depth + 7) >> 3,
tiles->rep_height);
{
tile_loc loc;
#ifdef DEBUG
if (clist_find_bits(cldev, tiles->id, &loc))
lprintf1("clist_find_bits(0x%lx) should have failed!\n",
(ulong) tiles->id);
#else
clist_find_bits(cldev, tiles->id, &loc);
#endif
slot->index = loc.index;
cldev->tile_table[loc.index].offset =
(byte *) slot_head - cldev->data;
if_debug2('L', "[L]adding index=%u, offset=%lu\n",
loc.index, cldev->tile_table[loc.index].offset);
}
slot->num_bands = 0;
return 0;
}
private void
clist_new_tile_params(gx_strip_bitmap * new_tile, const gx_strip_bitmap * tiles,
int depth, const gx_device_clist_writer * cldev)
{
#define max_tile_reps_x 255
#define max_tile_bytes_x 32
#define max_tile_reps_y 4
#define max_tile_bytes 256
uint rep_width = tiles->rep_width;
uint rep_height = tiles->rep_height;
uint rep_width_bits = rep_width * depth;
uint tile_overhead =
sizeof(tile_slot) + cldev->tile_band_mask_size;
uint max_bytes = cldev->chunk.size / (rep_width_bits * rep_height);
max_bytes -= min(max_bytes, tile_overhead);
if (max_bytes > max_tile_bytes)
max_bytes = max_tile_bytes;
*new_tile = *tiles;
{
uint max_bits_x = max_bytes * 8 / rep_height;
uint reps_x =
min(max_bits_x, max_tile_bytes_x * 8) / rep_width_bits;
uint reps_y;
while (reps_x > max_tile_reps_x)
reps_x >>= 1;
new_tile->size.x = max(reps_x, 1) * rep_width;
new_tile->raster = bitmap_raster(new_tile->size.x * depth);
if (tiles->shift != 0)
reps_y = 1;
else {
reps_y = max_bytes / (new_tile->raster * rep_height);
if (reps_y > max_tile_reps_y)
reps_y = max_tile_reps_y;
else if (reps_y < 1)
reps_y = 1;
}
new_tile->size.y = reps_y * rep_height;
}
#undef max_tile_reps_x
#undef max_tile_bytes_x
#undef max_tile_reps_y
#undef max_tile_bytes
}
int
clist_change_tile(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const gx_strip_bitmap * tiles, int depth)
{
tile_loc loc;
int code;
#define tile_params_differ(cldev, tiles, depth)\
((tiles)->rep_width != (cldev)->tile_params.rep_width ||\
(tiles)->rep_height != (cldev)->tile_params.rep_height ||\
(tiles)->rep_shift != (cldev)->tile_params.rep_shift ||\
(depth) != (cldev)->tile_depth)
top:if (clist_find_bits(cldev, tiles->id, &loc)) {
int band_index = pcls - cldev->states;
byte *bptr = ts_mask(loc.tile) + (band_index >> 3);
byte bmask = 1 << (band_index & 7);
if (*bptr & bmask) {
if (pcls->tile_index == loc.index)
return 0;
if ((code = cmd_put_tile_index(cldev, pcls, loc.index)) < 0)
return code;
} else {
uint extra = 0;
if tile_params_differ
(cldev, tiles, depth) {
int band;
clist_new_tile_params(&cldev->tile_params, tiles, depth,
cldev);
cldev->tile_depth = depth;
for (band = cldev->tile_known_min;
band <= cldev->tile_known_max;
++band
)
cldev->states[band].known &= ~tile_params_known;
cldev->tile_known_min = cldev->nbands;
cldev->tile_known_max = -1;
}
if (!(pcls->known & tile_params_known)) {
extra = cmd_size_tile_params(&cldev->tile_params);
} {
ulong offset = (byte *) loc.tile - cldev->chunk.data;
uint rsize =
extra + 1 + cmd_size_w(loc.index) + cmd_size_w(offset);
byte *dp;
uint csize;
int code =
cmd_put_bits(cldev, pcls, ts_bits(cldev, loc.tile),
tiles->rep_width * depth, tiles->rep_height,
loc.tile->cb_raster, rsize,
(cldev->tile_params.size.x > tiles->rep_width ?
decompress_elsewhere | decompress_spread :
decompress_elsewhere),
&dp, &csize);
if (code < 0)
return code;
if (extra) {
cmd_store_tile_params(dp, &cldev->tile_params, depth,
extra);
dp += extra;
pcls->known |= tile_params_known;
if (band_index < cldev->tile_known_min)
cldev->tile_known_min = band_index;
if (band_index > cldev->tile_known_max)
cldev->tile_known_max = band_index;
}
*dp = cmd_count_op(cmd_opv_set_tile_bits, csize - extra);
dp++;
dp = cmd_put_w(loc.index, dp);
cmd_put_w(offset, dp);
*bptr |= bmask;
loc.tile->num_bands++;
}
}
pcls->tile_index = loc.index;
pcls->tile_id = loc.tile->id;
return 0;
}
{
gx_strip_bitmap new_tile;
gx_strip_bitmap *ptile;
if (tile_params_differ(cldev, tiles, depth)) {
clist_new_tile_params(&new_tile, tiles, depth, cldev);
ptile = &new_tile;
} else {
cldev->tile_params.id = tiles->id;
cldev->tile_params.data = tiles->data;
ptile = &cldev->tile_params;
}
code = clist_add_tile(cldev, ptile, tiles->raster, depth);
if (code < 0)
return code;
}
goto top;
#undef tile_params_differ
}
int
clist_change_bits(gx_device_clist_writer * cldev, gx_clist_state * pcls,
const gx_strip_bitmap * tiles, int depth)
{
tile_loc loc;
int code;
top:if (clist_find_bits(cldev, tiles->id, &loc)) {
uint band_index = pcls - cldev->states;
byte *bptr = ts_mask(loc.tile) + (band_index >> 3);
byte bmask = 1 << (band_index & 7);
if (*bptr & bmask) {
if (pcls->tile_index == loc.index)
return 0;
cmd_put_tile_index(cldev, pcls, loc.index);
} else {
ulong offset = (byte *) loc.tile - cldev->chunk.data;
uint rsize = 2 + cmd_size_w(loc.tile->width) +
cmd_size_w(loc.tile->height) + cmd_size_w(loc.index) +
cmd_size_w(offset);
byte *dp;
uint csize;
gx_clist_state *bit_pcls = pcls;
int code;
if (loc.tile->num_bands == CHAR_ALL_BANDS_COUNT)
bit_pcls = NULL;
code = cmd_put_bits(cldev, bit_pcls, ts_bits(cldev, loc.tile),
loc.tile->width * depth,
loc.tile->height, loc.tile->cb_raster,
rsize,
(1 << cmd_compress_cfe) | decompress_elsewhere,
&dp, &csize);
if (code < 0)
return code;
*dp = cmd_count_op(cmd_opv_set_bits, csize);
dp[1] = (depth << 2) + code;
dp += 2;
dp = cmd_put_w(loc.tile->width, dp);
dp = cmd_put_w(loc.tile->height, dp);
dp = cmd_put_w(loc.index, dp);
cmd_put_w(offset, dp);
if (bit_pcls == NULL) {
memset(ts_mask(loc.tile), 0xff,
cldev->tile_band_mask_size);
loc.tile->num_bands = cldev->nbands;
} else {
*bptr |= bmask;
loc.tile->num_bands++;
}
}
pcls->tile_index = loc.index;
pcls->tile_id = loc.tile->id;
return 0;
}
code = clist_add_tile(cldev, tiles, tiles->raster, depth);
if (code < 0)
return code;
goto top;
}