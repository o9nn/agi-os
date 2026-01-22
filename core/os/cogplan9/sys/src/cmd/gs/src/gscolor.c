#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gsccolor.h"
#include "gxcspace.h"
#include "gxdcconv.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gscolor2.h"
#include "gzstate.h"
void gx_set_effective_transfer(gs_state *);
public_st_client_color();
public_st_transfer_map();
private
ENUM_PTRS_WITH(transfer_map_enum_ptrs, gx_transfer_map *mptr) return 0;
case 0: ENUM_RETURN((mptr->proc == 0 ? mptr->closure.data : 0));
ENUM_PTRS_END
private RELOC_PTRS_WITH(transfer_map_reloc_ptrs, gx_transfer_map *mptr)
{
if (mptr->proc == 0)
RELOC_PTR(gx_transfer_map, closure.data);
}
RELOC_PTRS_END
void
gx_init_paint_1(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[0] = 0.0;
}
void
gx_init_paint_3(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[2] = 0.0;
pcc->paint.values[1] = 0.0;
pcc->paint.values[0] = 0.0;
}
void
gx_init_paint_4(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[3] = 1.0;
gx_init_paint_3(pcc, pcs);
}
#define FORCE_UNIT(p) (p <= 0.0 ? 0.0 : p >= 1.0 ? 1.0 : p)
void
gx_restrict01_paint_1(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[0] = FORCE_UNIT(pcc->paint.values[0]);
}
void
gx_restrict01_paint_3(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[2] = FORCE_UNIT(pcc->paint.values[2]);
pcc->paint.values[1] = FORCE_UNIT(pcc->paint.values[1]);
pcc->paint.values[0] = FORCE_UNIT(pcc->paint.values[0]);
}
void
gx_restrict01_paint_4(gs_client_color * pcc, const gs_color_space * pcs)
{
pcc->paint.values[3] = FORCE_UNIT(pcc->paint.values[3]);
gx_restrict01_paint_3(pcc, pcs);
}
void
gx_no_adjust_color_count(const gs_client_color * pcc,
const gs_color_space * pcs, int delta)
{
}
void load_transfer_map(gs_state *, gx_transfer_map *, floatp);
int
gs_setgray(gs_state * pgs, floatp gray)
{
gs_color_space cs;
int code;
gs_cspace_init_DeviceGray(pgs->memory, &cs);
if ((code = gs_setcolorspace(pgs, &cs)) >= 0) {
gs_client_color * pcc = pgs->ccolor;
cs_adjust_color_count(pgs, -1);
pcc->paint.values[0] = FORCE_UNIT(gray);
pcc->pattern = 0;
gx_unset_dev_color(pgs);
}
return code;
}
int
gs_setrgbcolor(gs_state * pgs, floatp r, floatp g, floatp b)
{
gs_color_space cs;
int code;
gs_cspace_init_DeviceRGB(pgs->memory, &cs);
if ((code = gs_setcolorspace(pgs, &cs)) >= 0) {
gs_client_color * pcc = pgs->ccolor;
cs_adjust_color_count(pgs, -1);
pcc->paint.values[0] = FORCE_UNIT(r);
pcc->paint.values[1] = FORCE_UNIT(g);
pcc->paint.values[2] = FORCE_UNIT(b);
pcc->pattern = 0;
gx_unset_dev_color(pgs);
}
return code;
}
int
gs_setnullcolor(gs_state * pgs)
{
if (pgs->in_cachedevice)
return_error(gs_error_undefined);
gs_setgray(pgs, 0.0);
color_set_null(pgs->dev_color);
return 0;
}
int
gs_settransfer(gs_state * pgs, gs_mapping_proc tproc)
{
return gs_settransfer_remap(pgs, tproc, true);
}
int
gs_settransfer_remap(gs_state * pgs, gs_mapping_proc tproc, bool remap)
{
gx_transfer *ptran = &pgs->set_transfer;
rc_decrement(ptran->red, "gs_settransfer");
rc_decrement(ptran->green, "gs_settransfer");
rc_decrement(ptran->blue, "gs_settransfer");
rc_unshare_struct(ptran->gray, gx_transfer_map, &st_transfer_map,
pgs->memory, goto fail, "gs_settransfer");
ptran->gray->proc = tproc;
ptran->gray->id = gs_next_ids(pgs->memory, 1);
ptran->red = 0;
ptran->green = 0;
ptran->blue = 0;
if (remap) {
load_transfer_map(pgs, ptran->gray, 0.0);
gx_set_effective_transfer(pgs);
gx_unset_dev_color(pgs);
} else
gx_set_effective_transfer(pgs);
return 0;
fail:
rc_increment(ptran->red);
rc_increment(ptran->green);
rc_increment(ptran->blue);
rc_increment(ptran->gray);
return_error(gs_error_VMerror);
}
gs_mapping_proc
gs_currenttransfer(const gs_state * pgs)
{
return pgs->set_transfer.gray->proc;
}
void
gx_set_device_color_1(gs_state * pgs)
{
gs_color_space cs;
gs_setoverprint(pgs, false);
gs_setoverprintmode(pgs, 0);
gs_cspace_init_DeviceGray(pgs->memory, &cs);
gs_setcolorspace(pgs, &cs);
set_nonclient_dev_color(pgs->dev_color, 1);
pgs->log_op = lop_default;
if (pgs->effective_overprint_mode == 1)
(void)gs_do_set_overprint(pgs);
}
private float
transfer_use_proc(floatp value, const gx_transfer_map * pmap,
const void *ignore_proc_data)
{
return (*pmap->proc) (value, pmap);
}
void
load_transfer_map(gs_state * pgs, gx_transfer_map * pmap, floatp min_value)
{
gs_mapping_closure_proc_t proc;
const void *proc_data;
frac *values = pmap->values;
frac fmin = float2frac(min_value);
int i;
if (pmap->proc == 0)
proc = pmap->closure.proc, proc_data = pmap->closure.data;
else
proc = transfer_use_proc, proc_data = 0 ;
for (i = 0; i < transfer_map_size; i++) {
float fval =
(*proc) ((float)i / (transfer_map_size - 1), pmap, proc_data);
values[i] =
(fval < min_value ? fmin :
fval >= 1.0 ? frac_1 :
float2frac(fval));
}
}