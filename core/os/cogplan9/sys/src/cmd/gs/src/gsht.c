#include "memory_.h"
#include "string_.h"
#include <stdlib.h>
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gxarith.h"
#include "gzstate.h"
#include "gxdevice.h"
#include "gzht.h"
#include "gswts.h"
void gx_set_effective_transfer(gs_state *);
private const ushort ht_wts_suppress_release = (ushort)(-1);
public_st_ht_order();
private_st_ht_order_component();
public_st_ht_order_comp_element();
public_st_halftone();
public_st_device_halftone();
private
ENUM_PTRS_WITH(ht_order_enum_ptrs, gx_ht_order *porder) return 0;
case 0: ENUM_RETURN((porder->data_memory ? porder->levels : 0));
case 1: ENUM_RETURN((porder->data_memory ? porder->bit_data : 0));
case 2: ENUM_RETURN(porder->cache);
case 3: ENUM_RETURN(porder->transfer);
ENUM_PTRS_END
private
RELOC_PTRS_WITH(ht_order_reloc_ptrs, gx_ht_order *porder)
{
if (porder->data_memory) {
RELOC_VAR(porder->levels);
RELOC_VAR(porder->bit_data);
}
RELOC_VAR(porder->cache);
RELOC_VAR(porder->transfer);
}
RELOC_PTRS_END
private
ENUM_PTRS_WITH(halftone_enum_ptrs, gs_halftone *hptr) return 0;
case 0:
switch (hptr->type)
{
case ht_type_spot:
ENUM_RETURN((hptr->params.spot.transfer == 0 ?
hptr->params.spot.transfer_closure.data :
0));
case ht_type_threshold:
ENUM_RETURN_CONST_STRING_PTR(gs_halftone, params.threshold.thresholds);
case ht_type_threshold2:
return ENUM_CONST_BYTESTRING(&hptr->params.threshold2.thresholds);
case ht_type_client_order:
ENUM_RETURN(hptr->params.client_order.client_data);
case ht_type_multiple:
case ht_type_multiple_colorscreen:
ENUM_RETURN(hptr->params.multiple.components);
case ht_type_none:
case ht_type_screen:
case ht_type_colorscreen:
return 0;
}
case 1:
switch (hptr->type) {
case ht_type_threshold:
ENUM_RETURN((hptr->params.threshold.transfer == 0 ?
hptr->params.threshold.transfer_closure.data :
0));
case ht_type_threshold2:
ENUM_RETURN(hptr->params.threshold2.transfer_closure.data);
case ht_type_client_order:
ENUM_RETURN(hptr->params.client_order.transfer_closure.data);
default:
return 0;
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(halftone_reloc_ptrs, gs_halftone *hptr)
{
switch (hptr->type) {
case ht_type_spot:
if (hptr->params.spot.transfer == 0)
RELOC_PTR(gs_halftone, params.spot.transfer_closure.data);
break;
case ht_type_threshold:
RELOC_CONST_STRING_PTR(gs_halftone, params.threshold.thresholds);
if (hptr->params.threshold.transfer == 0)
RELOC_PTR(gs_halftone, params.threshold.transfer_closure.data);
break;
case ht_type_threshold2:
RELOC_CONST_BYTESTRING_VAR(hptr->params.threshold2.thresholds);
RELOC_OBJ_VAR(hptr->params.threshold2.transfer_closure.data);
break;
case ht_type_client_order:
RELOC_PTR(gs_halftone, params.client_order.client_data);
RELOC_PTR(gs_halftone, params.client_order.transfer_closure.data);
break;
case ht_type_multiple:
case ht_type_multiple_colorscreen:
RELOC_PTR(gs_halftone, params.multiple.components);
break;
case ht_type_none:
case ht_type_screen:
case ht_type_colorscreen:
break;
}
}
RELOC_PTRS_END
int
gs_setscreen(gs_state * pgs, gs_screen_halftone * phsp)
{
gs_screen_enum senum;
int code = gx_ht_process_screen(&senum, pgs, phsp,
gs_currentaccuratescreens());
if (code < 0)
return code;
return gs_screen_install(&senum);
}
int
gs_currentscreen(const gs_state * pgs, gs_screen_halftone * phsp)
{
switch (pgs->halftone->type) {
case ht_type_screen:
*phsp = pgs->halftone->params.screen;
return 0;
case ht_type_colorscreen:
*phsp = pgs->halftone->params.colorscreen.screens.colored.gray;
return 0;
default:
return_error(gs_error_undefined);
}
}
int
gs_currentscreenlevels(const gs_state * pgs)
{
int gi = 0;
if (pgs->device != 0)
gi = pgs->device->color_info.gray_index;
if (gi != GX_CINFO_COMP_NO_INDEX)
return pgs->dev_ht->components[gi].corder.num_levels;
else
return pgs->dev_ht->components[0].corder.num_levels;
}
int
gx_imager_setscreenphase(gs_imager_state * pis, int x, int y,
gs_color_select_t select)
{
if (select == gs_color_select_all) {
int i;
for (i = 0; i < gs_color_select_count; ++i)
gx_imager_setscreenphase(pis, x, y, (gs_color_select_t) i);
return 0;
} else if (select < 0 || select >= gs_color_select_count)
return_error(gs_error_rangecheck);
pis->screen_phase[select].x = x;
pis->screen_phase[select].y = y;
return 0;
}
int
gs_setscreenphase(gs_state * pgs, int x, int y, gs_color_select_t select)
{
int code = gx_imager_setscreenphase((gs_imager_state *) pgs, x, y,
select);
if (code >= 0 && (select == gs_color_select_texture ||
select == gs_color_select_all)
)
gx_unset_dev_color(pgs);
return code;
}
int
gs_currentscreenphase_pis(const gs_imager_state * pis, gs_int_point * pphase,
gs_color_select_t select)
{
if (select < 0 || select >= gs_color_select_count)
return_error(gs_error_rangecheck);
*pphase = pis->screen_phase[select];
return 0;
}
int
gs_currentscreenphase(const gs_state * pgs, gs_int_point * pphase,
gs_color_select_t select)
{
return gs_currentscreenphase_pis((const gs_imager_state *)pgs, pphase, select);
}
int
gs_currenthalftone(gs_state * pgs, gs_halftone * pht)
{
*pht = *pgs->halftone;
return 0;
}
int
gx_ht_process_screen_memory(gs_screen_enum * penum, gs_state * pgs,
gs_screen_halftone * phsp, bool accurate, gs_memory_t * mem)
{
gs_point pt;
int code = gs_screen_init_memory(penum, pgs, phsp, accurate, mem);
if (code < 0)
return code;
while ((code = gs_screen_currentpoint(penum, &pt)) == 0)
if ((code = gs_screen_next(penum, (*phsp->spot_function) (pt.x, pt.y))) < 0)
return code;
return 0;
}
int
gx_ht_alloc_ht_order(gx_ht_order * porder, uint width, uint height,
uint num_levels, uint num_bits, uint strip_shift,
const gx_ht_order_procs_t *procs, gs_memory_t * mem)
{
porder->wse = NULL;
porder->wts = NULL;
porder->width = width;
porder->height = height;
porder->raster = bitmap_raster(width);
porder->shift = strip_shift;
porder->orig_height = porder->height;
porder->orig_shift = porder->shift;
porder->full_height = ht_order_full_height(porder);
porder->num_levels = num_levels;
porder->num_bits = num_bits;
porder->procs = procs;
porder->data_memory = mem;
if (num_levels > 0) {
porder->levels =
(uint *)gs_alloc_byte_array(mem, porder->num_levels, sizeof(uint),
"alloc_ht_order_data(levels)");
if (porder->levels == 0)
return_error(gs_error_VMerror);
} else
porder->levels = 0;
if (num_bits > 0) {
porder->bit_data =
gs_alloc_byte_array(mem, porder->num_bits,
porder->procs->bit_data_elt_size,
"alloc_ht_order_data(bit_data)");
if (porder->bit_data == 0) {
gs_free_object(mem, porder->levels, "alloc_ht_order_data(levels)");
porder->levels = 0;
return_error(gs_error_VMerror);
}
} else
porder->bit_data = 0;
porder->cache = 0;
porder->transfer = 0;
return 0;
}
private int
gx_ht_copy_ht_order(gx_ht_order * pdest, gx_ht_order * psrc, gs_memory_t * mem)
{
int code;
*pdest = *psrc;
code = gx_ht_alloc_ht_order(pdest, psrc->width, psrc->height,
psrc->num_levels, psrc->num_bits, psrc->shift,
psrc->procs, mem);
if (code < 0)
return code;
if (pdest->levels != 0)
memcpy(pdest->levels, psrc->levels, psrc->num_levels * sizeof(uint));
if (pdest->bit_data != 0)
memcpy(pdest->bit_data, psrc->bit_data,
psrc->num_bits * psrc->procs->bit_data_elt_size);
pdest->wse = psrc->wse;
pdest->transfer = psrc->transfer;
rc_increment(pdest->transfer);
return 0;
}
private void
gx_ht_move_ht_order(gx_ht_order * pdest, gx_ht_order * psrc)
{
uint width = psrc->width, height = psrc->height, shift = psrc->shift;
pdest->params = psrc->params;
pdest->wse = psrc->wse;
pdest->wts = 0;
pdest->width = width;
pdest->height = height;
pdest->raster = bitmap_raster(width);
pdest->shift = shift;
pdest->orig_height = height;
pdest->orig_shift = shift;
pdest->full_height = ht_order_full_height(pdest);
pdest->num_levels = psrc->num_levels;
pdest->num_bits = psrc->num_bits;
pdest->procs = psrc->procs;
pdest->data_memory = psrc->data_memory;
pdest->levels = psrc->levels;
pdest->bit_data = psrc->bit_data;
pdest->cache = psrc->cache;
pdest->transfer = psrc->transfer;
}
int
gx_ht_alloc_order(gx_ht_order * porder, uint width, uint height,
uint strip_shift, uint num_levels, gs_memory_t * mem)
{
gx_ht_order order;
int code;
order = *porder;
gx_compute_cell_values(&order.params);
code = gx_ht_alloc_ht_order(&order, width, height, num_levels,
width * height, strip_shift,
&ht_order_procs_default, mem);
if (code < 0)
return code;
*porder = order;
return 0;
}
int
gx_ht_alloc_threshold_order(gx_ht_order * porder, uint width, uint height,
uint num_levels, gs_memory_t * mem)
{
gx_ht_order order;
uint num_bits = width * height;
const gx_ht_order_procs_t *procs =
(num_bits > 2000 && num_bits <= max_ushort ?
&ht_order_procs_short : &ht_order_procs_default);
int code;
order = *porder;
gx_compute_cell_values(&order.params);
code = gx_ht_alloc_ht_order(&order, width, height, num_levels,
width * height, 0, procs, mem);
if (code < 0)
return code;
*porder = order;
return 0;
}
int
gx_ht_alloc_client_order(gx_ht_order * porder, uint width, uint height,
uint num_levels, uint num_bits, gs_memory_t * mem)
{
gx_ht_order order;
int code;
order = *porder;
order.params.M = width, order.params.N = 0;
order.params.R = 1;
order.params.M1 = height, order.params.N1 = 0;
order.params.R1 = 1;
gx_compute_cell_values(&order.params);
code = gx_ht_alloc_ht_order(&order, width, height, num_levels,
num_bits, 0, &ht_order_procs_default, mem);
if (code < 0)
return code;
*porder = order;
return 0;
}
private int
compare_samples(const void *p1, const void *p2)
{
ht_sample_t m1 = ((const gx_ht_bit *)p1)->mask;
ht_sample_t m2 = ((const gx_ht_bit *)p2)->mask;
return (m1 < m2 ? -1 : m1 > m2 ? 1 : 0);
}
void
gx_sort_ht_order(gx_ht_bit * recs, uint N)
{
int i;
for (i = 0; i < N; i++)
recs[i].offset = i;
qsort((void *)recs, N, sizeof(*recs), compare_samples);
#ifdef DEBUG
if (gs_debug_c('H')) {
uint i;
dlputs("[H]Sorted samples:\n");
for (i = 0; i < N; i++)
dlprintf3("%5u: %5u: %u\n",
i, recs[i].offset, recs[i].mask);
}
#endif
}
void
gx_ht_construct_spot_order(gx_ht_order * porder)
{
uint width = porder->width;
uint num_levels = porder->num_levels;
uint strip = num_levels / width;
gx_ht_bit *bits = (gx_ht_bit *)porder->bit_data;
uint *levels = porder->levels;
uint shift = porder->orig_shift;
uint full_height = porder->full_height;
uint num_bits = porder->num_bits;
uint copies = num_bits / (width * strip);
gx_ht_bit *bp = bits + num_bits - 1;
uint i;
gx_sort_ht_order(bits, num_levels);
if_debug5('h',
"[h]spot order: num_levels=%u w=%u h=%u strip=%u shift=%u\n",
num_levels, width, porder->orig_height, strip, shift);
for (i = num_levels; i > 0;) {
uint offset = bits[--i].offset;
uint x = offset % width;
uint hy = offset - x;
uint k;
levels[i] = i * copies;
for (k = 0; k < copies;
k++, bp--, hy += num_levels, x = (x + width - shift) % width
)
bp->offset = hy + x;
}
if (num_bits == width * full_height) {
porder->height = full_height;
porder->shift = 0;
}
gx_ht_construct_bits(porder);
}
void
gx_ht_construct_bit(gx_ht_bit * bit, int width, int bit_num)
{
uint padding = bitmap_raster(width) * 8 - width;
int pix = bit_num;
ht_mask_t mask;
byte *pb;
pix += pix / width * padding;
bit->offset = (pix >> 3) & -size_of(mask);
mask = (ht_mask_t) 1 << (~pix & (ht_mask_bits - 1));
pix = ht_mask_bits - width;
while ((pix -= width) >= 0)
mask |= mask >> width;
bit->mask = 0;
for (pb = (byte *) & bit->mask + (sizeof(mask) - 1);
mask != 0;
mask >>= 8, pb--
)
*pb = (byte) mask;
}
void
gx_ht_construct_bits(gx_ht_order * porder)
{
uint i;
gx_ht_bit *phb;
for (i = 0, phb = (gx_ht_bit *)porder->bit_data;
i < porder->num_bits;
i++, phb++)
gx_ht_construct_bit(phb, porder->width, phb->offset);
#ifdef DEBUG
if (gs_debug_c('H')) {
dlprintf1("[H]Halftone order bits 0x%lx:\n", (ulong)porder->bit_data);
for (i = 0, phb = (gx_ht_bit *)porder->bit_data;
i < porder->num_bits;
i++, phb++)
dlprintf3("%4d: %u:0x%lx\n", i, phb->offset,
(ulong) phb->mask);
}
#endif
}
void
gx_ht_order_release(gx_ht_order * porder, gs_memory_t * mem, bool free_cache)
{
if (free_cache) {
if (porder->cache != 0)
gx_ht_free_cache(mem, porder->cache);
else if (porder->wse != 0)
gs_wts_free_enum(porder->wse);
}
porder->cache = 0;
if (porder->wts != 0 && porder->width != ht_wts_suppress_release)
gs_wts_free_screen(porder->wts);
porder->wts = 0;
rc_decrement(porder->transfer, "gx_ht_order_release(transfer)");
porder->transfer = 0;
if (porder->data_memory != 0) {
gs_free_object(porder->data_memory, porder->bit_data,
"gx_ht_order_release(bit_data)");
gs_free_object(porder->data_memory, porder->levels,
"gx_ht_order_release(levels)");
}
porder->levels = 0;
porder->bit_data = 0;
}
void
gx_device_halftone_release(gx_device_halftone * pdht, gs_memory_t * mem)
{
if (pdht->components) {
int i;
for (i = 0; i < pdht->num_comp; ++i)
if (pdht->components[i].corder.bit_data !=
pdht->order.bit_data
) {
gx_ht_order_release(&pdht->components[i].corder, mem, true);
}
gs_free_object(mem, pdht->components,
"gx_dev_ht_release(components)");
pdht->components = 0;
pdht->num_comp = 0;
}
gx_ht_order_release(&pdht->order, mem, false);
}
int
gs_color_name_component_number(gx_device * dev, const char * pname,
int name_size, int halftonetype)
{
int num_colorant;
#define check_colorant_name(dev, name) \
((*dev_proc(dev, get_color_comp_index)) (dev, name, strlen(name), NO_COMP_NAME_TYPE))
#define check_colorant_name_length(dev, name, length) \
((*dev_proc(dev, get_color_comp_index)) (dev, name, length, NO_COMP_NAME_TYPE))
#define check_name(str, pname, length) \
((strlen(str) == length) && (strncmp(pname, str, length) == 0))
num_colorant = check_colorant_name_length(dev, pname, name_size);
if (num_colorant >= 0) {
if (num_colorant == GX_DEVICE_COLOR_MAX_COMPONENTS)
num_colorant = -1;
return num_colorant;
}
if (check_name("Default", pname, name_size))
return GX_DEVICE_COLOR_MAX_COMPONENTS;
if (halftonetype == ht_type_colorscreen ||
halftonetype == ht_type_multiple_colorscreen) {
if (check_name("Red", pname, name_size))
num_colorant = check_colorant_name(dev, "Cyan");
else if (check_name("Green", pname, name_size))
num_colorant = check_colorant_name(dev, "Magenta");
else if (check_name("Blue", pname, name_size))
num_colorant = check_colorant_name(dev, "Yellow");
else if (check_name("Gray", pname, name_size))
num_colorant = check_colorant_name(dev, "Black");
#undef check_colorant_name
#undef check_colorant_name_length
#undef check_name
}
return num_colorant;
}
int
gs_cname_to_colorant_number(gs_state * pgs, byte * pname, uint name_size,
int halftonetype)
{
gx_device * dev = pgs->device;
return gs_color_name_component_number(dev, (char *)pname, name_size,
halftonetype);
}
int
gx_imager_dev_ht_install(
gs_imager_state * pis,
gx_device_halftone * pdht,
gs_halftone_type type,
const gx_device * dev )
{
gx_device_halftone dht;
int num_comps = pdht->num_dev_comp;
int i, code = 0;
bool used_default = false;
int lcm_width = 1, lcm_height = 1;
gs_wts_screen_enum_t * wse0 = pdht->order.wse;
wts_screen_t * wts0 = 0;
bool mem_diff = pdht->rc.memory != pis->memory;
memset(&dht.order, 0, sizeof(dht.order));
dht.id = gs_next_ids(pis->memory, 1);
dht.type = type;
dht.components = gs_alloc_struct_array(
pis->memory,
num_comps,
gx_ht_order_component,
&st_ht_order_component_element,
"gx_imager_dev_ht_install(components)" );
if (dht.components == NULL)
return_error(gs_error_VMerror);
dht.num_comp = dht.num_dev_comp = num_comps;
memset(dht.components, 0, num_comps * sizeof(dht.components[0]));
for (i = 0; i < num_comps; i++)
dht.components[i].comp_number = -1;
if (pdht->components != 0) {
int input_ncomps = pdht->num_comp;
for (i = 0; i < input_ncomps && code >= 0; i++) {
gx_ht_order_component * p_s_comp = &pdht->components[i];
gx_ht_order * p_s_order = &p_s_comp->corder;
int comp_num = p_s_comp->comp_number;
if (comp_num >= 0 && comp_num < GX_DEVICE_COLOR_MAX_COMPONENTS) {
gx_ht_order * p_d_order = &dht.components[comp_num].corder;
dht.components[comp_num].comp_number = comp_num;
if (mem_diff)
code = gx_ht_copy_ht_order( p_d_order,
p_s_order,
pis->memory );
else {
used_default = used_default ||
p_s_order->bit_data == pdht->order.bit_data;
gx_ht_move_ht_order(p_d_order, p_s_order);
}
}
}
}
for (i = 0; i < num_comps && code >= 0; i++) {
gx_ht_order * porder = &dht.components[i].corder;
gs_wts_screen_enum_t * wse;
if (dht.components[i].comp_number != i) {
if (used_default || mem_diff)
code = gx_ht_copy_ht_order(porder, &pdht->order, pis->memory);
else {
gx_ht_move_ht_order(porder, &pdht->order);
used_default = true;
}
dht.components[i].comp_number = i;
}
if ((wse = porder->wse) != 0) {
wts_screen_t * wts = 0;
porder->width = 0;
porder->wse = 0;
if (wse != wse0)
wts = wts_screen_from_enum(wse);
else {
if (wts0 == 0)
wts0 = wts_screen_from_enum(wse);
else
porder->width = ht_wts_suppress_release;
wts = wts0;
}
if (wts == 0)
code = gs_error_VMerror;
else
porder->wts = wts;
} else {
uint w = porder->width, h = porder->full_height;
int dw = igcd(lcm_width, w), dh = igcd(lcm_height, h);
lcm_width /= dw;
lcm_height /= dh;
lcm_width = (w > max_int / lcm_width ? max_int : lcm_width * w);
lcm_height = (h > max_int / lcm_height ? max_int : lcm_height * h);
if (porder->cache == 0) {
uint tile_bytes, num_tiles;
gx_ht_cache * pcache;
tile_bytes = porder->raster
* (porder->num_bits / porder->width);
num_tiles = 1 + max_tile_cache_bytes / tile_bytes;
pcache = gx_ht_alloc_cache( pis->memory,
num_tiles,
tile_bytes * num_tiles );
if (pcache == NULL)
code = gs_error_VMerror;
else {
porder->cache = pcache;
gx_ht_init_cache(pis->memory, pcache, porder);
}
}
}
}
dht.lcm_width = lcm_width;
dht.lcm_height = lcm_height;
if (code >= 0) {
gx_device_halftone * pisdht = pis->dev_ht;
rc_header tmp_rc;
if (pisdht != 0 && pisdht->rc.ref_count == 1) {
if (pdht != pisdht)
gx_device_halftone_release(pisdht, pisdht->rc.memory);
} else {
rc_unshare_struct( pis->dev_ht,
gx_device_halftone,
&st_device_halftone,
pis->memory,
BEGIN code = gs_error_VMerror; goto err; END,
"gx_imager_dev_ht_install" );
pisdht = pis->dev_ht;
}
if (pdht->components != 0) {
int input_ncomps = pdht->num_comp;
for (i = 0; i < input_ncomps; i++) {
gx_ht_order_component * p_s_comp = &pdht->components[i];
gx_ht_order * p_s_order = &p_s_comp->corder;
int comp_num = p_s_comp->comp_number;
if ( comp_num >= 0 &&
comp_num < GX_DEVICE_COLOR_MAX_COMPONENTS ) {
if (p_s_order->wse != 0)
gs_wts_free_enum(p_s_order->wse);
memset(p_s_order, 0, sizeof(*p_s_order));
} else if ( comp_num == GX_DEVICE_COLOR_MAX_COMPONENTS &&
used_default )
memset(p_s_order, 0, sizeof(*p_s_order));
}
}
if (used_default) {
if (wse0 != 0)
gs_wts_free_enum(wse0);
memset(&pdht->order, 0, sizeof(pdht->order));
}
tmp_rc = pisdht->rc;
*pisdht = dht;
pisdht->rc = tmp_rc;
gx_imager_set_effective_xfer(pis);
return 0;
}
err:
for (i = 0; i < num_comps; i++) {
gx_ht_order_component * pcomp = &dht.components[i];
gx_ht_order * porder = &pcomp->corder;
if (pcomp->comp_number == -1)
gx_ht_order_release(porder, pis->memory, true);
}
gs_free_object(pis->memory, dht.components, "gx_imager_dev_ht_install");
return code;
}
int
gx_ht_install(gs_state * pgs, const gs_halftone * pht,
gx_device_halftone * pdht)
{
gs_memory_t *mem = pht->rc.memory;
gs_halftone *old_ht = pgs->halftone;
gs_halftone *new_ht;
int code;
pdht->num_dev_comp = pgs->device->color_info.num_components;
if (old_ht != 0 && old_ht->rc.memory == mem &&
old_ht->rc.ref_count == 1
)
new_ht = old_ht;
else
rc_alloc_struct_1(new_ht, gs_halftone, &st_halftone,
mem, return_error(gs_error_VMerror),
"gx_ht_install(new halftone)");
code = gx_imager_dev_ht_install((gs_imager_state *) pgs,
pdht, pht->type, gs_currentdevice_inline(pgs));
if (code < 0) {
if (new_ht != old_ht)
gs_free_object(mem, new_ht, "gx_ht_install(new halftone)");
return code;
}
gx_device_halftone_release(pdht, pdht->rc.memory);
if (new_ht != old_ht)
rc_decrement(old_ht, "gx_ht_install(old halftone)");
{
rc_header rc;
rc = new_ht->rc;
*new_ht = *pht;
new_ht->rc = rc;
}
pgs->halftone = new_ht;
gx_unset_dev_color(pgs);
return 0;
}
#define check_colorant_name(name, dev) \
((*dev_proc(dev, get_color_comp_index)) (dev, name, strlen(name), NO_NAME_TYPE))
void
gx_imager_set_effective_xfer(gs_imager_state * pis)
{
const gx_device_halftone *pdht = pis->dev_ht;
gx_transfer_map *pmap;
int i, component_num;
for (i = 0; i < GX_DEVICE_COLOR_MAX_COMPONENTS; i++)
pis->effective_transfer[i] = pis->set_transfer.gray;
if (pis->set_transfer.red) {
component_num = pis->set_transfer.red_component_num;
if (component_num >= 0)
pis->effective_transfer[component_num] = pis->set_transfer.red;;
}
if (pis->set_transfer.green) {
component_num = pis->set_transfer.green_component_num;
if (component_num >= 0)
pis->effective_transfer[component_num] = pis->set_transfer.green;
}
if (pis->set_transfer.blue) {
component_num = pis->set_transfer.blue_component_num;
if (component_num >= 0)
pis->effective_transfer[component_num] = pis->set_transfer.blue;
}
if (pdht == NULL)
return;
for (i = 0; i < pdht->num_comp; i++) {
pmap = pdht->components[i].corder.transfer;
if (pmap != NULL)
pis->effective_transfer[i] = pmap;
}
}
void
gx_set_effective_transfer(gs_state * pgs)
{
gx_imager_set_effective_xfer((gs_imager_state *) pgs);
}