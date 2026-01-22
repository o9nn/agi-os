#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gxcomp.h"
#include "gxdevice.h"
#include "gsdevice.h"
#include "gxgetbit.h"
#include "gsovrc.h"
#include "gxdcolor.h"
#include "gxoprect.h"
#include "gsbitops.h"
#include "gxistate.h"
private_st_gs_overprint_t();
private int
write_color_index(gx_color_index cindex, byte * data, uint * psize)
{
int             num_bytes = 0;
gx_color_index  ctmp = cindex;
for (num_bytes = 1; (ctmp >>= 7) != 0; ++num_bytes)
;
if (num_bytes > *psize) {
*psize = num_bytes;
return gs_error_rangecheck;
}
ctmp = cindex;
*psize = num_bytes;
for (; num_bytes > 1; ctmp >>= 7, --num_bytes)
*data++ = 0x80 | (ctmp & 0x7f);
*data = ctmp & 0x7f;
return 0;
}
private int
read_color_index(gx_color_index * pcindex, const byte * data, uint size)
{
gx_color_index  cindex = 0;
int             nbytes = 0, shift = 0;
for (;; shift += 7, data++) {
if (++nbytes > size)
return_error(gs_error_rangecheck);
else {
int     c = *data;
cindex += (c & 0x7f) << shift;
if ((c & 0x80) == 0)
break;
}
}
*pcindex = cindex;
return nbytes;
}
private bool
c_overprint_equal(const gs_composite_t * pct0, const gs_composite_t * pct1)
{
if (pct0->type == pct1->type) {
const gs_overprint_params_t *    pparams0;
const gs_overprint_params_t *    pparams1;
pparams0 = &((const gs_overprint_t *)(pct0))->params;
pparams1 = &((const gs_overprint_t *)(pct1))->params;
if (!pparams0->retain_any_comps)
return !pparams1->retain_any_comps;
else if (pparams0->retain_spot_comps)
return pparams1->retain_spot_comps;
else
return pparams0->drawn_comps == pparams1->drawn_comps;
} else
return false;
}
#define OVERPRINT_ANY_COMPS     1
#define OVERPRINT_SPOT_COMPS    2
private int
c_overprint_write(const gs_composite_t * pct, byte * data, uint * psize)
{
const gs_overprint_params_t *   pparams = &((const gs_overprint_t *)pct)->params;
byte                            flags = 0;
int                             used = 1, avail = *psize;
if (pparams->retain_any_comps) {
flags |= OVERPRINT_ANY_COMPS;
if (pparams->retain_spot_comps)
flags |= OVERPRINT_SPOT_COMPS;
else {
uint    tmp_size = (avail > 0 ? avail - 1 : 0);
int     code = write_color_index( pparams->drawn_comps,
data + 1,
&tmp_size );
if (code < 0 && code != gs_error_rangecheck)
return code;
used += tmp_size;
}
}
*psize = used;
if (used > avail)
return_error(gs_error_rangecheck);
data[0] = flags;
return 0;
}
private int
c_overprint_read(
gs_composite_t **       ppct,
const byte *            data,
uint                    size,
gs_memory_t *           mem )
{
gs_overprint_params_t   params;
byte                    flags = 0;
int                     code = 0, nbytes = 1;
if (size < 1)
return_error(gs_error_rangecheck);
flags = *data;
params.retain_any_comps = (flags & OVERPRINT_ANY_COMPS) != 0;
params.retain_spot_comps = (flags & OVERPRINT_SPOT_COMPS) != 0;
if (params.retain_any_comps && !params.retain_spot_comps) {
code = read_color_index(&params.drawn_comps, data + 1, size - 1);
if (code < 0)
return code;
nbytes += code;
}
code = gs_create_overprint(ppct, &params, mem);
return code < 0 ? code : nbytes;
}
private composite_create_default_compositor_proc(c_overprint_create_default_compositor);
const gs_composite_type_t   gs_composite_overprint_type = {
GX_COMPOSITOR_OVERPRINT,
{
c_overprint_create_default_compositor,
c_overprint_equal,
c_overprint_write,
c_overprint_read,
gx_default_composite_clist_write_update,
gx_default_composite_clist_read_update
}
};
int
gs_create_overprint(
gs_composite_t **               ppct,
const gs_overprint_params_t *   pparams,
gs_memory_t *                   mem )
{
gs_overprint_t *                pct;
rc_alloc_struct_0( pct,
gs_overprint_t,
&st_overprint,
mem,
return_error(gs_error_VMerror),
"gs_create_overprint" );
pct->type = &gs_composite_overprint_type;
pct->id = gs_next_ids(mem, 1);
pct->params = *pparams;
*ppct = (gs_composite_t *)pct;
return 0;
}
int
gs_is_overprint_compositor(const gs_composite_t * pct)
{
return pct->type == &gs_composite_overprint_type;
}
typedef struct overprint_device_s {
gx_device_forward_common;
gx_color_index  drawn_comps;
gx_color_index  retain_mask;
} overprint_device_t;
gs_private_st_suffix_add0_final( st_overprint_device_t,
overprint_device_t,
"overprint_device_t",
overprint_device_t_enum_ptrs,
overprint_device_t_reloc_ptrs,
gx_device_finalize,
st_device_forward );
private dev_proc_open_device(overprint_open_device);
private dev_proc_put_params(overprint_put_params);
private dev_proc_get_page_device(overprint_get_page_device);
private dev_proc_create_compositor(overprint_create_compositor);
private gx_device_procs no_overprint_procs = {
overprint_open_device,
0,
0,
0,
0,
0,
0,
gx_forward_fill_rectangle,
gx_forward_tile_rectangle,
gx_forward_copy_mono,
gx_forward_copy_color,
0,
0,
0,
overprint_put_params,
0,
0,
0,
0,
overprint_get_page_device,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
gx_forward_strip_tile_rectangle,
0,
0,
0,
0,
0,
overprint_create_compositor,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0
};
private dev_proc_fill_rectangle(overprint_generic_fill_rectangle);
private dev_proc_fill_rectangle(overprint_sep_fill_rectangle);
private gx_device_procs generic_overprint_procs = {
overprint_open_device,
0,
0,
0,
0,
0,
0,
overprint_generic_fill_rectangle,
gx_default_tile_rectangle,
gx_default_copy_mono,
gx_default_copy_color,
gx_default_draw_line,
0,
0,
overprint_put_params,
0,
0,
gx_default_get_xfont_device,
0,
overprint_get_page_device,
0,
gx_default_copy_alpha,
0,
gx_default_copy_rop,
gx_default_fill_path,
gx_default_stroke_path,
gx_default_fill_mask,
gx_default_fill_trapezoid,
gx_default_fill_parallelogram,
gx_default_fill_triangle,
gx_default_draw_thin_line,
gx_default_begin_image,
0,
0,
gx_default_strip_tile_rectangle,
gx_default_strip_copy_rop,
0,
gx_default_begin_typed_image,
0,
0,
overprint_create_compositor,
0,
gx_default_text_begin,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0
};
private gx_device_procs sep_overprint_procs = {
overprint_open_device,
0,
0,
0,
0,
0,
0,
overprint_sep_fill_rectangle,
gx_default_tile_rectangle,
gx_default_copy_mono,
gx_default_copy_color,
gx_default_draw_line,
0,
0,
overprint_put_params,
0,
0,
gx_default_get_xfont_device,
0,
overprint_get_page_device,
0,
gx_default_copy_alpha,
0,
gx_default_copy_rop,
gx_default_fill_path,
gx_default_stroke_path,
gx_default_fill_mask,
gx_default_fill_trapezoid,
gx_default_fill_parallelogram,
gx_default_fill_triangle,
gx_default_draw_thin_line,
gx_default_begin_image,
0,
0,
gx_default_strip_tile_rectangle,
gx_default_strip_copy_rop,
0,
gx_default_begin_typed_image,
0,
0,
overprint_create_compositor,
0,
gx_default_text_begin,
0,
0,
0,
0,
0,
0,
0,
0,
0,
0
};
const overprint_device_t    gs_overprint_device = {
std_device_std_body_open( overprint_device_t,
0,
"overprint_device",
0, 0,
1, 1 ),
{ 0 }
};
#if !arch_is_big_endian
private gx_color_index
swap_color_index(int depth, gx_color_index color)
{
int             shift = depth - 8;
gx_color_index  mask = 0xff;
color =  ((color >> shift) & mask)
| ((color & mask) << shift)
| (color & ~((mask << shift) | mask));
if (depth > 24) {
shift -= 16;
mask <<= 8;
color =  ((color >> shift) & mask)
| ((color & mask) << shift)
| (color & ~((mask << shift) | mask));
if (depth > 40) {
shift -= 16;
mask <<= 8;
color =  ((color >> shift) & mask)
| ((color & mask) << shift)
| (color & ~((mask << shift) | mask));
if (depth > 56) {
shift -= 16;
mask <<= 8;
color =  ((color >> shift) & mask)
| ((color & mask) << shift)
| (color & ~((mask << shift) | mask));
}
}
}
return color;
}
#endif
private void
set_retain_mask(overprint_device_t * opdev)
{
int             i, ncomps = opdev->color_info.num_components;
gx_color_index  drawn_comps = opdev->drawn_comps, retain_mask = 0;
#if !arch_is_big_endian
int             depth = opdev->color_info.depth;
#endif
for (i = 0; i < ncomps; i++, drawn_comps >>= 1) {
if ((drawn_comps & 0x1) == 0)
retain_mask |= opdev->color_info.comp_mask[i];
}
#if !arch_is_big_endian
if (depth > 8)
retain_mask = swap_color_index(depth, retain_mask);
#endif
opdev->retain_mask = retain_mask;
}
private gx_color_index
check_drawn_comps(int ncomps, frac cvals[GX_DEVICE_COLOR_MAX_COMPONENTS])
{
int              i;
gx_color_index   mask = 0x1, drawn_comps = 0;
for (i = 0; i < ncomps; i++, mask <<= 1) {
if (cvals[i] != frac_0)
drawn_comps |= mask;
}
return drawn_comps;
}
private int
update_overprint_params(
overprint_device_t *            opdev,
const gs_overprint_params_t *   pparams )
{
int                             ncomps = opdev->color_info.num_components;
if (!pparams->retain_any_comps) {
if (dev_proc(opdev, fill_rectangle) != gx_forward_fill_rectangle)
memcpy( &opdev->procs,
&no_overprint_procs,
sizeof(no_overprint_procs) );
return 0;
}
if (opdev->color_info.separable_and_linear == GX_CINFO_SEP_LIN)
memcpy( &opdev->procs,
&sep_overprint_procs,
sizeof(sep_overprint_procs) );
else
memcpy( &opdev->procs,
&generic_overprint_procs,
sizeof(generic_overprint_procs) );
if (!pparams->retain_spot_comps)
opdev->drawn_comps = pparams->drawn_comps;
else {
gx_device *                     dev = (gx_device *)opdev;
const gx_cm_color_map_procs *   pprocs;
frac                            cvals[GX_DEVICE_COLOR_MAX_COMPONENTS];
gx_color_index                  drawn_comps = 0;
static const frac               frac_13 = float2frac(1.0 / 3.0);
if ((pprocs = dev_proc(opdev, get_color_mapping_procs)(dev)) == 0 ||
pprocs->map_gray == 0                                         ||
pprocs->map_rgb == 0                                          ||
pprocs->map_cmyk == 0                                           )
return_error(gs_error_unknownerror);
pprocs->map_gray(dev, frac_13, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_rgb(dev, 0, frac_13, frac_0, frac_0, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_rgb(dev, 0, frac_0, frac_13, frac_0, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_rgb(dev, 0, frac_0, frac_0, frac_13, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_cmyk(dev, frac_13, frac_0, frac_0, frac_0, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_cmyk(dev, frac_0, frac_13, frac_0, frac_0, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_cmyk(dev, frac_0, frac_0, frac_13, frac_0, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
pprocs->map_cmyk(dev, frac_0, frac_0, frac_0, frac_13, cvals);
drawn_comps |= check_drawn_comps(ncomps, cvals);
opdev->drawn_comps = drawn_comps;
}
if (opdev->drawn_comps == ((gx_color_index)1 << ncomps) - 1) {
memcpy( &opdev->procs,
&no_overprint_procs,
sizeof(no_overprint_procs) );
return 0;
}
if (opdev->color_info.separable_and_linear == GX_CINFO_SEP_LIN)
set_retain_mask(opdev);
return 0;
}
private int
overprint_open_device(gx_device * dev)
{
overprint_device_t *    opdev = (overprint_device_t *)dev;
gx_device *             tdev = opdev->target;
int                     code = 0;
if (tdev == 0)
return_error(gs_error_unknownerror);
if ((code = gs_opendevice(tdev)) >= 0)
gx_device_copy_params(dev, tdev);
return code;
}
private int
overprint_put_params(gx_device * dev, gs_param_list * plist)
{
overprint_device_t *    opdev = (overprint_device_t *)dev;
gx_device *             tdev = opdev->target;
int                     code = 0;
if (tdev != 0 && (code = dev_proc(tdev, put_params)(tdev, plist)) >= 0) {
gx_device_decache_colors(dev);
if (!tdev->is_open)
code = gs_closedevice(dev);
}
return code;
}
private gx_device *
overprint_get_page_device(gx_device * dev)
{
overprint_device_t *    opdev = (overprint_device_t *)dev;
gx_device *             tdev = opdev->target;
return tdev == 0 ? 0 : dev_proc(tdev, get_page_device)(tdev);
}
private int
overprint_create_compositor(
gx_device *             dev,
gx_device **            pcdev,
const gs_composite_t *  pct,
gs_imager_state *	    pis,
gs_memory_t *           memory )
{
if (pct->type != &gs_composite_overprint_type)
return gx_default_create_compositor(dev, pcdev, pct, pis, memory);
else {
int     code;
code = update_overprint_params(
(overprint_device_t *)dev,
&((const gs_overprint_t *)pct)->params );
if (code >= 0)
*pcdev = dev;
return code;
}
}
private int
overprint_generic_fill_rectangle(
gx_device *     dev,
int             x,
int             y,
int             width,
int             height,
gx_color_index  color )
{
overprint_device_t *    opdev = (overprint_device_t *)dev;
gx_device *             tdev = opdev->target;
if (tdev == 0)
return 0;
else
return gx_overprint_generic_fill_rectangle( tdev,
opdev->drawn_comps,
x, y, width, height,
color,
dev->memory );
}
private int
overprint_sep_fill_rectangle(
gx_device *     dev,
int             x,
int             y,
int             width,
int             height,
gx_color_index  color )
{
overprint_device_t *    opdev = (overprint_device_t *)dev;
gx_device *             tdev = opdev->target;
if (tdev == 0)
return 0;
else {
int     depth = tdev->color_info.depth;
#if !arch_is_big_endian
if (depth > 8)
color = swap_color_index(depth, color);
#endif
if ( depth <= 8 * sizeof(mono_fill_chunk) &&
(depth & (depth - 1)) == 0             )
return gx_overprint_sep_fill_rectangle_1( tdev,
opdev->retain_mask,
x, y, width, height,
color,
dev->memory );
else
return gx_overprint_sep_fill_rectangle_2( tdev,
opdev->retain_mask,
x, y, width, height,
color,
dev->memory );
}
}
private void
fill_in_procs(gx_device_procs * pprocs)
{
gx_device_forward   tmpdev;
memcpy( &tmpdev.color_info,
&gs_overprint_device.color_info,
sizeof(tmpdev.color_info) );
tmpdev.color_info.separable_and_linear = GX_CINFO_SEP_LIN_NONE;
tmpdev.static_procs = 0;
memcpy(&tmpdev.procs, pprocs, sizeof(tmpdev.procs));
gx_device_forward_fill_in_procs(&tmpdev);
memcpy(pprocs, &tmpdev.procs, sizeof(tmpdev.procs));
}
private int
c_overprint_create_default_compositor(
const gs_composite_t *  pct,
gx_device **            popdev,
gx_device *             tdev,
gs_imager_state *	    pis,
gs_memory_t *           mem )
{
const gs_overprint_t *  ovrpct = (const gs_overprint_t *)pct;
overprint_device_t *    opdev = 0;
if ( !ovrpct->params.retain_any_comps) {
*popdev = tdev;
return 0;
}
if (no_overprint_procs.get_xfont_procs == 0) {
fill_in_procs(&no_overprint_procs);
fill_in_procs(&generic_overprint_procs);
fill_in_procs(&sep_overprint_procs);
}
opdev = gs_alloc_struct_immovable( mem,
overprint_device_t,
&st_overprint_device_t,
"create overprint compositor" );
if ((*popdev = (gx_device *)opdev) == 0)
return_error(gs_error_VMerror);
gx_device_init( (gx_device *)opdev,
(const gx_device *)&gs_overprint_device,
mem,
true );
gx_device_copy_params((gx_device *)opdev, tdev);
gx_device_set_target((gx_device_forward *)opdev, tdev);
return update_overprint_params( opdev,
&((const gs_overprint_t *)pct)->params );
}