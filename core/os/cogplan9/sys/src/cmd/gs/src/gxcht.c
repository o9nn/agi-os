#include <assert.h>
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gxarith.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gxdcolor.h"
#include "gxistate.h"
#include "gzht.h"
#include "gsserial.h"
#define USE_SLOW_CODE 0
#define tile_longs_LARGE 256
#define tile_longs_SMALL 64
#if arch_small_memory
# define tile_longs_allocated tile_longs_SMALL
# define tile_longs tile_longs_SMALL
#else
# define tile_longs_allocated tile_longs_LARGE
# ifdef DEBUG
# define tile_longs\
(gs_debug_c('.') ? tile_longs_SMALL : tile_longs_LARGE)
# else
# define tile_longs tile_longs_LARGE
# endif
#endif
gs_private_st_ptrs1(st_dc_ht_colored, gx_device_color, "dc_ht_colored",
dc_ht_colored_enum_ptrs, dc_ht_colored_reloc_ptrs, colors.colored.c_ht);
private dev_color_proc_save_dc(gx_dc_ht_colored_save_dc);
private dev_color_proc_get_dev_halftone(gx_dc_ht_colored_get_dev_halftone);
private dev_color_proc_load(gx_dc_ht_colored_load);
private dev_color_proc_fill_rectangle(gx_dc_ht_colored_fill_rectangle);
private dev_color_proc_equal(gx_dc_ht_colored_equal);
private dev_color_proc_write(gx_dc_ht_colored_write);
private dev_color_proc_read(gx_dc_ht_colored_read);
const gx_device_color_type_t gx_dc_type_data_ht_colored = {
&st_dc_ht_colored,
gx_dc_ht_colored_save_dc, gx_dc_ht_colored_get_dev_halftone,
gx_dc_ht_get_phase,
gx_dc_ht_colored_load, gx_dc_ht_colored_fill_rectangle,
gx_dc_default_fill_masked, gx_dc_ht_colored_equal,
gx_dc_ht_colored_write, gx_dc_ht_colored_read,
gx_dc_ht_colored_get_nonzero_comps
};
#undef gx_dc_type_ht_colored
const gx_device_color_type_t *const gx_dc_type_ht_colored =
&gx_dc_type_data_ht_colored;
#define gx_dc_type_ht_colored (&gx_dc_type_data_ht_colored)
private void
gx_dc_ht_colored_save_dc(const gx_device_color * pdevc,
gx_device_color_saved * psdc)
{
psdc->type = pdevc->type;
memcpy( psdc->colors.colored.c_base,
pdevc->colors.colored.c_base,
sizeof(psdc->colors.colored.c_base) );
memcpy( psdc->colors.colored.c_level,
pdevc->colors.colored.c_level,
sizeof(psdc->colors.colored.c_base) );
psdc->colors.colored.alpha = pdevc->colors.colored.alpha;
psdc->phase = pdevc->phase;
}
private const gx_device_halftone *
gx_dc_ht_colored_get_dev_halftone(const gx_device_color * pdevc)
{
return pdevc->colors.colored.c_ht;
}
private bool
gx_dc_ht_colored_equal(const gx_device_color * pdevc1,
const gx_device_color * pdevc2)
{
uint num_comp = pdevc1->colors.colored.num_components;
if (pdevc2->type != pdevc1->type ||
pdevc1->colors.colored.c_ht != pdevc2->colors.colored.c_ht ||
pdevc1->colors.colored.alpha != pdevc2->colors.colored.alpha ||
pdevc1->phase.x != pdevc2->phase.x ||
pdevc1->phase.y != pdevc2->phase.y ||
num_comp != pdevc2->colors.colored.num_components
)
return false;
return
!memcmp(pdevc1->colors.colored.c_base,
pdevc2->colors.colored.c_base,
num_comp * sizeof(pdevc1->colors.colored.c_base[0])) &&
!memcmp(pdevc1->colors.colored.c_level,
pdevc2->colors.colored.c_level,
num_comp * sizeof(pdevc1->colors.colored.c_level[0]));
}
private const int dc_ht_colored_has_base = 0x01;
private const int dc_ht_colored_has_level = 0x02;
private const int dc_ht_colored_has_alpha = 0x04;
private const int dc_ht_colored_alpha_is_max = 0x08;
private int
gx_dc_ht_colored_write(
const gx_device_color * pdevc,
const gx_device_color_saved * psdc0,
const gx_device * dev,
byte * pdata,
uint * psize )
{
int req_size = 1;
int flag_bits = 0;
int num_comps = dev->color_info.num_components;
int depth = dev->color_info.depth;
gx_color_index plane_mask = pdevc->colors.colored.plane_mask;
gx_color_value alpha = pdevc->colors.colored.alpha;
const gx_device_color_saved * psdc = psdc0;
byte * pdata0 = pdata;
assert(pdevc->colors.colored.num_components == num_comps);
if (psdc != 0 && psdc->type != pdevc->type)
psdc = 0;
if ( psdc == 0 ||
memcmp( pdevc->colors.colored.c_base,
psdc->colors.colored.c_base,
num_comps * sizeof(pdevc->colors.colored.c_base[0]) ) != 0 ) {
flag_bits |= dc_ht_colored_has_base;
if (num_comps == depth)
req_size += (num_comps + 7) >> 3;
else
req_size += num_comps * sizeof(pdevc->colors.colored.c_base[0]);
}
plane_mask = pdevc->colors.colored.plane_mask;
if ( psdc == 0 ||
memcmp( pdevc->colors.colored.c_level,
psdc->colors.colored.c_level,
num_comps * sizeof(pdevc->colors.colored.c_level[0]) ) != 0 ) {
gx_color_index comp_bit;
int i;
uint tmp_mask;
flag_bits |= dc_ht_colored_has_level;
if (num_comps > 8 * sizeof(uint)) {
tmp_mask = (uint)plane_mask;
req_size += enc_u_sizew(tmp_mask);
tmp_mask = (uint)(plane_mask >> (8 * sizeof(uint)));
req_size += enc_u_sizew(tmp_mask);
} else {
tmp_mask = (uint)plane_mask;
req_size += enc_u_sizew(tmp_mask);
}
for (i = 0, comp_bit = 0x1; i < num_comps; i++, comp_bit <<= 1) {
if ((plane_mask & comp_bit) != 0)
req_size += enc_u_sizew(pdevc->colors.colored.c_level[i]);
}
}
if (psdc == 0 || alpha != psdc->colors.colored.alpha) {
if (alpha == gx_max_color_value)
flag_bits |= dc_ht_colored_alpha_is_max;
else {
flag_bits |= dc_ht_colored_has_alpha;
req_size += enc_u_sizew(alpha);
}
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
if ((flag_bits & dc_ht_colored_has_base) != 0) {
if (num_comps == depth) {
gx_color_index base_mask = 0;
int num_bytes = (num_comps + 7) >> 3;
int i;
for (i = 0; i < num_comps; i++) {
if (pdevc->colors.colored.c_base[i] != 0)
base_mask |= (gx_color_index)1 << i;
}
for (i = 0; i < num_bytes; i++, base_mask >>= 8)
*pdata++ = (byte)base_mask;
} else {
memcpy( pdata,
pdevc->colors.colored.c_base,
num_comps * sizeof(pdevc->colors.colored.c_base[0]) );
pdata += num_comps * sizeof(pdevc->colors.colored.c_base[0]);
}
}
if ((flag_bits & dc_ht_colored_has_level) != 0) {
gx_color_index code_bit;
int i;
uint tmp_mask;
if (num_comps > 8 * sizeof(uint)) {
tmp_mask = (uint)plane_mask;
enc_u_putw(tmp_mask, pdata);
tmp_mask = (uint)(plane_mask >> (8 * sizeof(uint)));
enc_u_putw(tmp_mask, pdata);
} else {
tmp_mask = (uint)plane_mask;
enc_u_putw(tmp_mask, pdata);
}
for (i = 0, code_bit = 0x1; i < num_comps; i++, code_bit <<= 1) {
if ((plane_mask & code_bit) != 0)
enc_u_putw(pdevc->colors.colored.c_level[i], pdata);
}
}
if ((flag_bits & dc_ht_colored_has_alpha) != 0)
enc_u_putw(alpha, pdata);
*psize = pdata - pdata0;
return 0;
}
private int
gx_dc_ht_colored_read(
gx_device_color * pdevc,
const gs_imager_state * pis,
const gx_device_color * prior_devc,
const gx_device * dev,
const byte * pdata,
uint size,
gs_memory_t * mem )
{
gx_device_color devc;
int num_comps = dev->color_info.num_components;
int depth = dev->color_info.depth;
const byte * pdata0 = pdata;
int flag_bits;
if (prior_devc != 0 && prior_devc->type == gx_dc_type_ht_colored)
devc = *prior_devc;
else
memset(&devc, 0, sizeof(devc));
devc.type = gx_dc_type_ht_colored;
devc.colors.colored.num_components = num_comps;
devc.colors.colored.c_ht = pis->dev_ht;
if (size == 0)
return_error(gs_error_rangecheck);
size--;
flag_bits = *pdata++;
if ((flag_bits & dc_ht_colored_has_base) != 0) {
if (depth == num_comps) {
gx_color_index base_mask = 0;
int num_bytes = (num_comps + 7) >> 3;
int i, shift = 0;
if (size < num_bytes)
return_error(gs_error_rangecheck);
size -= num_bytes;
for (i = 0; i < num_bytes; i++, shift += 8)
base_mask |= (gx_color_index)(*pdata++) << shift;
for (i = 0; i < num_comps; i++, base_mask >>= 1)
devc.colors.colored.c_base[i] = base_mask & 0x1;
} else {
if (size < num_comps)
return_error(gs_error_rangecheck);
size -= num_comps;
memcpy(devc.colors.colored.c_base, pdata, num_comps);
pdata += num_comps;
}
}
if ((flag_bits & dc_ht_colored_has_level) != 0) {
const byte * pdata_start = pdata;
gx_color_index plane_mask;
uint tmp_mask;
int i;
if (size < 1)
return_error(gs_error_rangecheck);
if (num_comps > 8 * sizeof(uint)) {
enc_u_getw(tmp_mask, pdata);
plane_mask = (gx_color_index)tmp_mask;
enc_u_getw(tmp_mask, pdata);
plane_mask = (gx_color_index)tmp_mask << (8 * sizeof(uint));
} else {
enc_u_getw(tmp_mask, pdata);
plane_mask = (gx_color_index)tmp_mask;
}
devc.colors.colored.plane_mask = plane_mask;
for (i = 0; i < num_comps; i++, plane_mask >>= 1) {
if ((plane_mask & 0x1) != 0) {
if (size - (pdata - pdata_start) < 1)
return_error(gs_error_rangecheck);
enc_u_getw(devc.colors.colored.c_level[i], pdata);
} else
devc.colors.colored.c_level[i] = 0;
}
size -= pdata - pdata_start;
}
if ((flag_bits & dc_ht_colored_alpha_is_max) != 0)
devc.colors.colored.alpha = gx_max_color_value;
else if ((flag_bits & dc_ht_colored_has_alpha) != 0) {
const byte * pdata_start = pdata;
if (size < 1)
return_error(gs_error_rangecheck);
enc_u_getw(devc.colors.colored.alpha, pdata);
size -= pdata - pdata_start;
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
gx_dc_ht_colored_get_nonzero_comps(
const gx_device_color * pdevc,
const gx_device * dev_ignored,
gx_color_index * pcomp_bits )
{
int i, ncomps = pdevc->colors.colored.num_components;
gx_color_index comp_bits = pdevc->colors.colored.plane_mask;
for (i = 0; i < ncomps; i++) {
if (pdevc->colors.colored.c_base[i] != 0)
comp_bits |= ((gx_color_index)1) << i;
}
*pcomp_bits = comp_bits;
return 0;
}
#define MAX_DCC GX_DEVICE_COLOR_MAX_COMPONENTS
#define MAX_DCC_16 (2 * MAX_DCC < 16 ? 16 : 2 * MAX_DCC)
typedef gx_color_value gx_color_value_array[MAX_DCC];
typedef struct color_values_pair_s {
gx_color_value_array values[2];
} color_values_pair_t;
#define SET_HT_COLORS_PROC(proc)\
int proc(\
color_values_pair_t *pvp,\
gx_color_index colors[MAX_DCC_16],\
const gx_const_strip_bitmap *sbits[MAX_DCC],\
const gx_device_color *pdevc,\
gx_device *dev,\
gx_ht_cache *caches[MAX_DCC],\
int nplanes\
)
private SET_HT_COLORS_PROC(set_ht_colors_le_4);
private SET_HT_COLORS_PROC(set_cmyk_1bit_colors);
private SET_HT_COLORS_PROC(set_ht_colors_gt_4);
#define SET_COLOR_HT_PROC(proc)\
void proc(\
byte *dest_data, \
uint dest_raster, \
int px, \
int py,\
int w, \
int h,\
int depth, \
int special, \
int nplanes,\
gx_color_index plane_mask, \
gx_device *dev, \
const color_values_pair_t *pvp, \
gx_color_index colors[MAX_DCC], \
\
const gx_const_strip_bitmap * sbits[MAX_DCC] \
\
)
private SET_COLOR_HT_PROC(set_color_ht_le_4);
private SET_COLOR_HT_PROC(set_color_ht_gt_4);
private int
gx_dc_ht_colored_load(gx_device_color * pdevc, const gs_imager_state * pis,
gx_device * ignore_dev, gs_color_select_t select)
{
return 0;
}
private int
gx_dc_ht_colored_fill_rectangle(const gx_device_color * pdevc,
int x, int y, int w, int h,
gx_device * dev, gs_logical_operation_t lop,
const gx_rop_source_t * source)
{
ulong tbits[tile_longs_allocated];
const uint tile_bytes = tile_longs * size_of(long);
gx_strip_bitmap tiles;
gx_rop_source_t no_source;
const gx_device_halftone *pdht = pdevc->colors.colored.c_ht;
int depth = dev->color_info.depth;
int nplanes = dev->color_info.num_components;
SET_HT_COLORS_PROC((*set_ht_colors)) =
(
#if USE_SLOW_CODE
set_ht_colors_gt_4
#else
(dev_proc(dev, map_cmyk_color) == gx_default_encode_color &&
dev->color_info.depth == 4) ?
set_cmyk_1bit_colors :
nplanes <= 4 ? set_ht_colors_le_4 :
set_ht_colors_gt_4
#endif
);
SET_COLOR_HT_PROC((*set_color_ht)) =
(
#if !USE_SLOW_CODE
!(pdevc->colors.colored.plane_mask & ~(gx_color_index)15) &&
set_ht_colors != set_ht_colors_gt_4 ?
set_color_ht_le_4 :
#endif
set_color_ht_gt_4);
color_values_pair_t vp;
gx_color_index colors[MAX_DCC_16];
const gx_const_strip_bitmap *sbits[MAX_DCC];
gx_ht_cache *caches[MAX_DCC];
int special;
int code = 0;
int raster;
uint size_x;
int dw, dh;
int lw = pdht->lcm_width, lh = pdht->lcm_height;
bool no_rop;
int i;
if (w <= 0 || h <= 0)
return 0;
if ((w | h) >= 16) {
gs_fixed_rect cbox;
int t;
dev_proc(dev, get_clipping_box)(dev, &cbox);
if ((t = fixed2int(cbox.p.x)) > x) {
if ((w += x - t) <= 0)
return 0;
x = t;
}
if ((t = fixed2int(cbox.p.y)) > y) {
if ((h += y - t) <= 0)
return 0;
y = t;
}
if ((t = fixed2int(cbox.q.x)) < x + w)
if ((w = t - x) <= 0)
return 0;
if ((t = fixed2int(cbox.q.y)) < y + h)
if ((h = t - y) <= 0)
return 0;
}
lop &= ~lop_T_transparent;
if (pdht->components == 0) {
caches[0] = caches[1] = caches[2] = caches[3] = pdht->order.cache;
for (i = 4; i < nplanes; ++i)
caches[i] = pdht->order.cache;
} else {
gx_ht_order_component *pocs = pdht->components;
for (i = 0; i < nplanes; ++i)
caches[i] = pocs[i].corder.cache;
}
special = set_ht_colors(&vp, colors, sbits, pdevc, dev, caches, nplanes);
no_rop = source == NULL && lop_no_S_is_T(lop);
if ((w > lw || h > lh) &&
(raster = bitmap_raster(lw * depth)) <= tile_bytes / lh
) {
fit_fill(dev, x, y, w, h);
if (w > lw || h > lh) {
tiles.data = (byte *)tbits;
tiles.raster = raster;
tiles.rep_width = tiles.size.x = lw;
tiles.rep_height = tiles.size.y = lh;
tiles.id = gs_next_ids(dev->memory, 1);
tiles.rep_shift = tiles.shift = 0;
set_color_ht((byte *)tbits, raster, 0, 0, lw, lh, depth,
special, nplanes, pdevc->colors.colored.plane_mask,
dev, &vp, colors, sbits);
if (no_rop)
return (*dev_proc(dev, strip_tile_rectangle)) (dev, &tiles,
x, y, w, h,
gx_no_color_index, gx_no_color_index,
pdevc->phase.x, pdevc->phase.y);
if (source == NULL)
set_rop_no_source(source, no_source, dev);
return (*dev_proc(dev, strip_copy_rop)) (dev, source->sdata,
source->sourcex, source->sraster, source->id,
(source->use_scolors ? source->scolors : NULL),
&tiles, NULL,
x, y, w, h,
pdevc->phase.x, pdevc->phase.y,
lop);
}
}
size_x = w * depth;
raster = bitmap_raster(size_x);
if (raster > tile_bytes) {
if (x < 0)
w += x, x = 0;
if (x > dev->width - w)
w = dev->width - x;
if (w <= 0)
return 0;
size_x = w * depth;
raster = bitmap_raster(size_x);
if (raster > tile_bytes) {
dw = tile_bytes * 8 / depth;
size_x = dw * depth;
raster = bitmap_raster(size_x);
dh = 1;
goto fit;
}
}
dw = w;
dh = tile_bytes / raster;
if (dh > h)
dh = h;
fit:
if (!no_rop) {
tiles.data = (byte *)tbits;
tiles.id = gx_no_bitmap_id;
tiles.raster = raster;
tiles.rep_width = tiles.size.x = size_x / depth;
tiles.rep_shift = tiles.shift = 0;
}
while (w) {
int cy = y, ch = dh, left = h;
for (;;) {
set_color_ht((byte *)tbits, raster,
x + pdevc->phase.x, cy + pdevc->phase.y,
dw, ch, depth, special, nplanes,
pdevc->colors.colored.plane_mask,
dev, &vp, colors, sbits);
if (no_rop) {
code = (*dev_proc(dev, copy_color))
(dev, (byte *)tbits, 0, raster, gx_no_bitmap_id,
x, cy, dw, ch);
} else {
tiles.rep_height = tiles.size.y = ch;
if (source == NULL)
set_rop_no_source(source, no_source, dev);
code = (*dev_proc(dev, strip_copy_rop))
(dev, source->sdata, source->sourcex, source->sraster,
source->id,
(source->use_scolors ? source->scolors : NULL),
&tiles, NULL, x, cy, dw, ch, 0, 0, lop);
}
if (code < 0)
return code;
if (!(left -= ch))
break;
cy += ch;
if (ch > left)
ch = left;
}
if (!(w -= dw))
break;
x += dw;
if (dw > w)
dw = w;
}
return code;
}
private const struct {
ulong pad;
byte bytes[sizeof(ulong) * 8];
} ht_no_bitmap_data = { 0 };
private const gx_const_strip_bitmap ht_no_bitmap = {
&ht_no_bitmap_data.bytes[0], sizeof(ulong),
{sizeof(ulong) * 8, sizeof(ht_no_bitmap_data.bytes) / sizeof(ulong)},
gx_no_bitmap_id, 1, 1, 0, 0
};
#define SET_PLANE_COLOR_CONSTANT(i)\
BEGIN\
pvp->values[1][i] = pvp->values[0][i] = \
fractional_color(pdc->colors.colored.c_base[i], max_color);\
sbits[i] = &ht_no_bitmap;\
END
#define SET_PLANE_COLOR(i)\
BEGIN\
uint q = pdc->colors.colored.c_base[i];\
uint r = pdc->colors.colored.c_level[i];\
\
pvp->values[0][i] = fractional_color(q, max_color);\
if (r == 0)\
pvp->values[1][i] = pvp->values[0][i], sbits[i] = &ht_no_bitmap;\
else if (!invert) {\
pvp->values[1][i] = fractional_color(q + 1, max_color);\
sbits[i] = (const gx_const_strip_bitmap *)\
&gx_render_ht(caches[i], r)->tiles;\
} else { \
const gx_device_halftone *pdht = pdc->colors.colored.c_ht; \
int nlevels =\
(pdht->components ?\
pdht->components[i].corder.num_levels :\
pdht->order.num_levels);\
\
pvp->values[1][i] = pvp->values[0][i]; \
pvp->values[0][i] = fractional_color(q + 1, max_color); \
sbits[i] = (const gx_const_strip_bitmap *)\
&gx_render_ht(caches[i], nlevels - r)->tiles; \
}\
END
private int
set_ht_colors_le_4(color_values_pair_t *pvp ,
gx_color_index colors[MAX_DCC_16] ,
const gx_const_strip_bitmap * sbits[MAX_DCC],
const gx_device_color * pdc, gx_device * dev,
gx_ht_cache * caches[MAX_DCC], int nplanes)
{
gx_color_value max_color = dev->color_info.dither_colors - 1;
gx_color_value cvalues[4];
bool invert = dev->color_info.polarity == GX_CINFO_POLARITY_SUBTRACTIVE;
SET_PLANE_COLOR(0);
if (nplanes >= 2) {
SET_PLANE_COLOR(1);
}
if (nplanes >= 3) {
SET_PLANE_COLOR(2);
}
if (nplanes == 3) {
gx_color_value alpha = pdc->colors.colored.alpha;
if (alpha == gx_max_color_value) {
#define M(i)\
cvalues[0] = pvp->values[(i) & 1][0];\
cvalues[1] = pvp->values[((i) & 2) >> 1][1];\
cvalues[2] = pvp->values[(i) >> 2][2];\
colors[i] = dev_proc(dev, encode_color)(dev, cvalues);
M(0); M(1); M(2); M(3); M(4); M(5); M(6); M(7);
#undef M
} else {
#define M(i)\
colors[i] = dev_proc(dev, map_rgb_alpha_color)(dev, pvp->values[(i) & 1][0],\
pvp->values[((i) & 2) >> 1][1],\
pvp->values[(i) >> 2][2], alpha)
M(0); M(1); M(2); M(3); M(4); M(5); M(6); M(7);
#undef M
}
} else if (nplanes > 3){
SET_PLANE_COLOR(3);
if (nplanes > 4) {
int pi;
for (pi = 4; pi < nplanes; ++pi)
SET_PLANE_COLOR_CONSTANT(pi);
}
#define M(i)\
cvalues[0] = pvp->values[(i) & 1][0];\
cvalues[1] = pvp->values[((i) & 2) >> 1][1];\
cvalues[2] = pvp->values[((i) & 4) >> 2][2];\
cvalues[3] = pvp->values[(i) >> 3][3];\
colors[i] = dev_proc(dev, encode_color)(dev, cvalues)
switch ((int)pdc->colors.colored.plane_mask) {
case 15:
M(15); M(14); M(13); M(12);
M(11); M(10); M(9); M(8);
case 7:
M(7); M(6); M(5); M(4);
c3: case 3:
M(3); M(2);
c1: case 1:
M(1);
break;
case 14:
M(14); M(12); M(10); M(8);
case 6:
M(6); M(4);
c2: case 2:
M(2);
break;
case 13:
M(13); M(12); M(9); M(8);
case 5:
M(5); M(4);
goto c1;
case 12:
M(12); M(8);
case 4:
M(4);
break;
case 11:
M(11); M(10); M(9); M(8);
goto c3;
case 10:
M(10); M(8);
goto c2;
case 9:
M(9); M(8);
goto c1;
case 8:
M(8);
break;
case 0:;
}
M(0);
#undef M
}
return 0;
}
private int
set_cmyk_1bit_colors(color_values_pair_t *ignore_pvp,
gx_color_index colors[MAX_DCC_16] ,
const gx_const_strip_bitmap * sbits[MAX_DCC ],
const gx_device_color * pdc, gx_device * dev,
gx_ht_cache * caches[MAX_DCC ],
int nplanes )
{
const gx_device_halftone *pdht = pdc->colors.colored.c_ht;
bits32 mask0 = 0, mask1 = 0;
#define SET_PLANE_COLOR_CMYK(i, mask)\
BEGIN\
uint r = pdc->colors.colored.c_level[i];\
\
if (r == 0) {\
if (pdc->colors.colored.c_base[i])\
mask0 |= mask, mask1 |= mask;\
sbits[3 - i] = &ht_no_bitmap;\
} else {\
int nlevels =\
(pdht->components ?\
pdht->components[i].corder.num_levels :\
pdht->order.num_levels);\
\
mask0 |= mask;\
sbits[3 - i] = (const gx_const_strip_bitmap *)\
&gx_render_ht(caches[i], nlevels - r)->tiles;\
}\
END
SET_PLANE_COLOR_CMYK(0, (bits32)~0x77777777);
SET_PLANE_COLOR_CMYK(1, 0x44444444);
SET_PLANE_COLOR_CMYK(2, 0x22222222);
SET_PLANE_COLOR_CMYK(3, 0x11111111);
#undef SET_PLANE_COLOR_CMYK
{
gx_ht_cache *ctemp;
ctemp = caches[0], caches[0] = caches[3], caches[3] = ctemp;
ctemp = caches[1], caches[1] = caches[2], caches[2] = ctemp;
}
colors[0] = mask0;
colors[1] = mask1;
return 1;
}
private int
set_ht_colors_gt_4(color_values_pair_t *pvp,
gx_color_index colors[MAX_DCC_16 ],
const gx_const_strip_bitmap * sbits[MAX_DCC],
const gx_device_color * pdc, gx_device * dev,
gx_ht_cache * caches[MAX_DCC], int nplanes)
{
gx_color_value max_color = dev->color_info.dither_colors - 1;
bool invert = dev->color_info.polarity == GX_CINFO_POLARITY_SUBTRACTIVE;
gx_color_index plane_mask = pdc->colors.colored.plane_mask;
int i;
gx_color_value cv[MAX_DCC] = {0};
for (i = 0; i < nplanes; ++i)
if ((plane_mask >> i) & 1)
SET_PLANE_COLOR(i);
else
SET_PLANE_COLOR_CONSTANT(i);
for (i = 0; i < nplanes; i++ ) {
cv[i] = pvp->values[0][i];
colors[2 * i] = dev_proc(dev, encode_color)(dev, cv);
if ((plane_mask >> i) & 1) {
cv[i] = pvp->values[1][i];
colors[2 * i + 1] = dev_proc(dev, encode_color)(dev, cv);
}
cv[i] = 0;
}
return 0;
}
typedef struct tile_cursor_s {
int tile_shift;
int xoffset;
int xshift;
uint xbytes;
int xbits;
const byte *row;
const byte *tdata;
uint raster;
const byte *data;
int bit_shift;
} tile_cursor_t;
private void
init_tile_cursor(int i, tile_cursor_t *ptc, const gx_const_strip_bitmap *btile,
int endx, int lasty)
{
int tw = btile->size.x;
int bx = ((ptc->tile_shift = btile->shift) == 0 ? endx :
endx + lasty / btile->size.y * ptc->tile_shift) % tw;
int by = lasty % btile->size.y;
ptc->xoffset = bx >> 3;
ptc->xshift = 8 - (bx & 7);
ptc->xbytes = (tw - 1) >> 3;
ptc->xbits = ((tw - 1) & 7) + 1;
ptc->tdata = btile->data;
ptc->raster = btile->raster;
ptc->row = ptc->tdata + by * (int)ptc->raster;
ptc->data = ptc->row + ptc->xoffset;
ptc->bit_shift = ptc->xshift;
if_debug6('h', "[h]plane %d: size=%d,%d shift=%d bx=%d by=%d\n",
i, tw, btile->size.y, btile->shift, bx, by);
}
private void
wrap_shifted_cursor(tile_cursor_t *ptc, const gx_const_strip_bitmap *psbit)
{
ptc->row += ptc->raster * (psbit->size.y - 1);
if (ptc->tile_shift) {
if ((ptc->xshift += ptc->tile_shift) >= 8) {
if ((ptc->xoffset -= ptc->xshift >> 3) < 0) {
int bx = (ptc->xoffset << 3) + 8 - (ptc->xshift & 7) +
psbit->size.x;
ptc->xoffset = bx >> 3;
ptc->xshift = 8 - (bx & 7);
} else
ptc->xshift &= 7;
}
}
}
#define STEP_ROW(c, i)\
BEGIN\
if (c.row > c.tdata)\
c.row -= c.raster;\
else { \
wrap_shifted_cursor(&c, sbits[i]);\
}\
c.data = c.row + c.xoffset;\
c.bit_shift = c.xshift;\
END
private const bits32 expand_8x1_to_8x4[256] = {
#define X16(c)\
c+0, c+1, c+0x10, c+0x11, c+0x100, c+0x101, c+0x110, c+0x111,\
c+0x1000, c+0x1001, c+0x1010, c+0x1011, c+0x1100, c+0x1101, c+0x1110, c+0x1111
X16(0x00000000), X16(0x00010000), X16(0x00100000), X16(0x00110000),
X16(0x01000000), X16(0x01010000), X16(0x01100000), X16(0x01110000),
X16(0x10000000), X16(0x10010000), X16(0x10100000), X16(0x10110000),
X16(0x11000000), X16(0x11010000), X16(0x11100000), X16(0x11110000)
#undef X16
};
private void
set_color_ht_le_4(byte *dest_data, uint dest_raster, int px, int py,
int w, int h, int depth, int special, int nplanes,
gx_color_index plane_mask, gx_device *ignore_dev,
const color_values_pair_t *ignore_pvp,
gx_color_index colors[MAX_DCC_16],
const gx_const_strip_bitmap * sbits[MAX_DCC])
{
int x, y;
tile_cursor_t cursor[MAX_DCC];
int dbytes = depth >> 3;
byte *dest_row =
dest_data + dest_raster * (h - 1) + (w * depth) / 8;
if (special > 0) {
plane_mask =
"\000\010\004\014\002\012\006\016\001\011\005\015\003\013\007\017"[plane_mask];
}
if_debug6('h',
"[h]color_ht_le_4: x=%d y=%d w=%d h=%d plane_mask=0x%lu depth=%d\n",
px, py, w, h, (ulong)plane_mask, depth);
{
int endx = w + px;
int lasty = h - 1 + py;
if (plane_mask & 1)
init_tile_cursor(0, &cursor[0], sbits[0], endx, lasty);
if (plane_mask & 2)
init_tile_cursor(1, &cursor[1], sbits[1], endx, lasty);
if (plane_mask & 4)
init_tile_cursor(2, &cursor[2], sbits[2], endx, lasty);
if (plane_mask & 8)
init_tile_cursor(3, &cursor[3], sbits[3], endx, lasty);
}
for (y = h; ; dest_row -= dest_raster) {
byte *dest = dest_row;
--y;
for (x = w; x > 0;) {
bits32 indices;
int nx, i;
register uint bits;
#define NEXT_BITS(c)\
BEGIN\
if (c.data > c.row) {\
bits = ((c.data[-1] << 8) | *c.data) >> c.bit_shift;\
c.data--;\
} else {\
bits = *c.data >> c.bit_shift;\
c.data += c.xbytes;\
if ((c.bit_shift -= c.xbits) < 0) {\
bits |= *c.data << -c.bit_shift;\
c.bit_shift += 8;\
} else {\
bits |= ((c.data[-1] << 8) | *c.data) >> c.bit_shift;\
c.data--;\
}\
}\
END
if (plane_mask & 1) {
NEXT_BITS(cursor[0]);
indices = expand_8x1_to_8x4[bits & 0xff];
} else
indices = 0;
if (plane_mask & 2) {
NEXT_BITS(cursor[1]);
indices |= expand_8x1_to_8x4[bits & 0xff] << 1;
}
if (plane_mask & 4) {
NEXT_BITS(cursor[2]);
indices |= expand_8x1_to_8x4[bits & 0xff] << 2;
}
if (plane_mask & 8) {
NEXT_BITS(cursor[3]);
indices |= expand_8x1_to_8x4[bits & 0xff] << 3;
}
#undef NEXT_BITS
nx = min(x, 8);
x -= nx;
switch (dbytes) {
case 0:
if (special > 0) {
indices =
(indices & colors[1]) | (~indices & colors[0]);
i = nx;
if ((x + nx) & 1) {
*dest = (*dest & 0xf) +
((indices & 0xf) << 4);
indices >>= 4;
--i;
}
for (; (i -= 2) >= 0; indices >>= 8)
*--dest = (byte)indices;
if (i & 1)
*--dest = indices & 0xf;
} else {
i = nx;
if ((x + nx) & 1) {
*dest = (*dest & 0xf) +
((byte)colors[indices & 0xf] << 4);
indices >>= 4;
--i;
}
for (; (i -= 2) >= 0; indices >>= 8)
*--dest =
(byte)colors[indices & 0xf] +
((byte)colors[(indices >> 4) & 0xf]
<< 4);
if (i & 1)
*--dest = (byte)colors[indices & 0xf];
}
break;
case 4:
for (i = nx; --i >= 0; indices >>= 4) {
bits32 tcolor = (bits32)colors[indices & 0xf];
dest -= 4;
dest[3] = (byte)tcolor;
dest[2] = (byte)(tcolor >> 8);
tcolor >>= 16;
dest[1] = (byte)tcolor;
dest[0] = (byte)(tcolor >> 8);
}
break;
case 3:
for (i = nx; --i >= 0; indices >>= 4) {
bits32 tcolor = (bits32)colors[indices & 0xf];
dest -= 3;
dest[2] = (byte) tcolor;
dest[1] = (byte)(tcolor >> 8);
dest[0] = (byte)(tcolor >> 16);
}
break;
case 2:
for (i = nx; --i >= 0; indices >>= 4) {
uint tcolor =
(uint)colors[indices & 0xf];
dest -= 2;
dest[1] = (byte)tcolor;
dest[0] = (byte)(tcolor >> 8);
}
break;
case 1:
for (i = nx; --i >= 0; indices >>= 4)
*--dest = (byte)colors[indices & 0xf];
break;
}
}
if (y == 0)
break;
if (plane_mask & 1)
STEP_ROW(cursor[0], 0);
if (plane_mask & 2)
STEP_ROW(cursor[1], 1);
if (plane_mask & 4)
STEP_ROW(cursor[2], 2);
if (plane_mask & 8)
STEP_ROW(cursor[3], 3);
}
}
private void
set_color_ht_gt_4(byte *dest_data, uint dest_raster, int px, int py,
int w, int h, int depth, int special, int num_planes,
gx_color_index plane_mask, gx_device *dev,
const color_values_pair_t *pvp,
gx_color_index colors[MAX_DCC_16],
const gx_const_strip_bitmap * sbits[MAX_DCC])
{
int x, y;
tile_cursor_t cursor[MAX_DCC];
int dbytes = depth >> 3;
byte *dest_row =
dest_data + dest_raster * (h - 1) + (w * depth) / 8;
int pmin, pmax;
gx_color_index base_color = 0;
if (plane_mask == 0)
pmin = 0, pmax = -1;
else {
for (pmin = 0; !((plane_mask >> pmin) & 1); )
++pmin;
for (pmax = 0; (plane_mask >> pmax) > 1; )
++pmax;
}
if_debug6('h',
"[h]color_ht_gt_4: x=%d y=%d w=%d h=%d plane_mask=0x%lu depth=%d\n",
px, py, w, h, (ulong)plane_mask, depth);
{
int endx = w + px;
int lasty = h - 1 + py;
int i;
for (i = pmin; i <= pmax; ++i)
if ((plane_mask >> i) & 1)
init_tile_cursor(i, &cursor[i], sbits[i], endx, lasty);
}
{
int i;
for (i = 0; i < num_planes; ++i)
if ((~plane_mask >> i) & 1)
base_color |= colors[2 * i];
}
for (y = h; ; dest_row -= dest_raster) {
byte *dest = dest_row;
int i;
--y;
for (x = w; x > 0;) {
gx_color_index tcolor = base_color;
for (i = pmin; i <= pmax; ++i)
if ((plane_mask >> i) & 1) {
tile_cursor_t *ptc = &cursor[i];
byte tile_bit;
b: if (ptc->bit_shift < 8)
tile_bit = *ptc->data >> ptc->bit_shift++;
else if (ptc->data > ptc->row) {
tile_bit = *--(ptc->data);
ptc->bit_shift = 1;
} else {
ptc->data += ptc->xbytes;
ptc->bit_shift = 8 - ptc->xbits;
goto b;
}
tcolor |= colors[2 * i + (tile_bit & 1)];
}
--x;
switch (dbytes) {
case 0:
if (x & 1) {
*--dest = (byte)tcolor;
} else {
*dest = (*dest & 0xf) + ((byte)tcolor << 4);
}
break;
case 4:
dest[-4] = (byte)(tcolor >> 24);
case 3:
dest[-3] = (byte)(tcolor >> 16);
case 2:
dest[-2] = (byte)(tcolor >> 8);
case 1:
dest[-1] = (byte)tcolor;
dest -= dbytes;
break;
}
}
if (y == 0)
break;
for (i = pmin; i <= pmax; ++i)
if ((plane_mask >> i) & 1)
STEP_ROW(cursor[i], i);
}
}