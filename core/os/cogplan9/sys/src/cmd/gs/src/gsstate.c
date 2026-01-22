#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gzstate.h"
#include "gxcspace.h"
#include "gsalpha.h"
#include "gscolor2.h"
#include "gscoord.h"
#include "gscie.h"
#include "gxclipsr.h"
#include "gxcmap.h"
#include "gxdevice.h"
#include "gxpcache.h"
#include "gzht.h"
#include "gzline.h"
#include "gspath.h"
#include "gzpath.h"
#include "gzcpath.h"
#include "gsovrc.h"
#include "gxcolor2.h"
#include "gxpcolor.h"
private gs_state *gstate_alloc(gs_memory_t *, client_name_t,
const gs_state *);
private gs_state *gstate_clone(gs_state *, gs_memory_t *, client_name_t,
gs_state_copy_reason_t);
private void gstate_free_contents(gs_state *);
private int gstate_copy(gs_state *, const gs_state *,
gs_state_copy_reason_t, client_name_t);
typedef struct gs_state_parts_s {
gx_path *path;
gx_clip_path *clip_path;
gx_clip_path *effective_clip_path;
gs_color_space *color_space;
gs_client_color *ccolor;
gx_device_color *dev_color;
} gs_state_parts;
#define GSTATE_ASSIGN_PARTS(pto, pfrom)\
((pto)->path = (pfrom)->path, (pto)->clip_path = (pfrom)->clip_path,\
(pto)->effective_clip_path = (pfrom)->effective_clip_path,\
(pto)->color_space = (pfrom)->color_space,\
(pto)->ccolor = (pfrom)->ccolor, (pto)->dev_color = (pfrom)->dev_color)
extern_st(st_imager_state);
public_st_gs_state();
private ENUM_PTRS_WITH(gs_state_enum_ptrs, gs_state *gsvptr)
ENUM_PREFIX(st_imager_state, gs_state_num_ptrs + 2);
#define e1(i,elt) ENUM_PTR(i,gs_state,elt);
gs_state_do_ptrs(e1)
case gs_state_num_ptrs:
ENUM_RETURN(gx_device_enum_ptr(gsvptr->device));
case gs_state_num_ptrs + 1:
ENUM_RETURN(gsvptr->dfilter_stack);
#undef e1
ENUM_PTRS_END
private RELOC_PTRS_WITH(gs_state_reloc_ptrs, gs_state *gsvptr)
{
RELOC_PREFIX(st_imager_state);
{
#define r1(i,elt) RELOC_PTR(gs_state,elt);
gs_state_do_ptrs(r1)
#undef r1
gsvptr->device = gx_device_reloc_ptr(gsvptr->device, gcst);
RELOC_PTR(gs_state, dfilter_stack);
}
}
RELOC_PTRS_END
private int
gstate_copy_client_data(gs_state * pgs, void *dto, void *dfrom,
gs_state_copy_reason_t reason)
{
return (pgs->client_procs.copy_for != 0 ?
(*pgs->client_procs.copy_for) (dto, dfrom, reason) :
(*pgs->client_procs.copy) (dto, dfrom));
}
private const gs_imager_state gstate_initial = {
gs_imager_state_initial(1.0)
};
private gs_memory_t *
gstate_path_memory(gs_memory_t *mem)
{
return gs_memory_stable(mem);
}
gs_state *
gs_state_alloc(gs_memory_t * mem)
{
gs_state *pgs = gstate_alloc(mem, "gs_state_alloc", NULL);
int code;
if (pgs == 0)
return 0;
pgs->saved = 0;
*(gs_imager_state *)pgs = gstate_initial;
code = gs_imager_state_initialize((gs_imager_state *) pgs, mem);
if (code < 0)
goto fail;
rc_alloc_struct_1(pgs->halftone, gs_halftone, &st_halftone, mem,
goto fail, "gs_state_alloc(halftone)");
pgs->halftone->type = ht_type_none;
pgs->path = gx_path_alloc(gstate_path_memory(mem), "gs_state_alloc(path)");
pgs->clip_path = gx_cpath_alloc(mem, "gs_state_alloc(clip_path)");
pgs->clip_stack = 0;
pgs->view_clip = gx_cpath_alloc(mem, "gs_state_alloc(view_clip)");
pgs->view_clip->rule = 0;
pgs->effective_clip_id = pgs->clip_path->id;
pgs->effective_view_clip_id = gs_no_id;
pgs->effective_clip_path = pgs->clip_path;
pgs->effective_clip_shared = true;
gs_cspace_init_DeviceGray(pgs->memory, pgs->color_space);
pgs->in_cachedevice = 0;
gx_set_device_color_1(pgs);
pgs->device = 0;
gs_nulldevice(pgs);
gs_setalpha(pgs, 1.0);
gs_settransfer(pgs, gs_identity_transfer);
gs_setflat(pgs, 1.0);
gs_setfilladjust(pgs, 0.25, 0.25);
gs_setlimitclamp(pgs, false);
gs_setstrokeadjust(pgs, true);
pgs->font = 0;
pgs->root_font = 0;
pgs->in_charpath = (gs_char_path_mode) 0;
pgs->show_gstate = 0;
pgs->level = 0;
pgs->dfilter_stack = 0;
pgs->transparency_group_stack = 0;
if (gs_initgraphics(pgs) >= 0)
return pgs;
fail:
gs_state_free(pgs);
return 0;
}
void
gs_state_set_client(gs_state * pgs, void *pdata,
const gs_state_client_procs * pprocs, bool client_has_pattern_streams)
{
pgs->client_data = pdata;
pgs->client_procs = *pprocs;
pgs->have_pattern_streams = client_has_pattern_streams;
}
#undef gs_state_client_data
void *
gs_state_client_data(const gs_state * pgs)
{
return pgs->client_data;
}
int
gs_state_free(gs_state * pgs)
{
gstate_free_contents(pgs);
gs_free_object(pgs->memory, pgs, "gs_state_free");
return 0;
}
int
gs_gsave(gs_state * pgs)
{
gs_state *pnew = gstate_clone(pgs, pgs->memory, "gs_gsave",
copy_for_gsave);
if (pnew == 0)
return_error(gs_error_VMerror);
pnew->clip_stack = 0;
rc_increment(pnew->dfilter_stack);
pgs->saved = pnew;
if (pgs->show_gstate == pgs)
pgs->show_gstate = pnew->show_gstate = pnew;
pgs->level++;
if_debug2('g', "[g]gsave -> 0x%lx, level = %d\n",
(ulong) pnew, pgs->level);
return 0;
}
int
gs_gsave_for_save(gs_state * pgs, gs_state ** psaved)
{
int code;
gx_clip_path *old_cpath = pgs->view_clip;
gx_clip_path *new_cpath;
if (old_cpath) {
new_cpath =
gx_cpath_alloc_shared(old_cpath, pgs->memory,
"gs_gsave_for_save(view_clip)");
if (new_cpath == 0)
return_error(gs_error_VMerror);
} else {
new_cpath = 0;
}
code = gs_gsave(pgs);
if (code < 0)
goto fail;
if (pgs->effective_clip_path == pgs->view_clip)
pgs->effective_clip_path = new_cpath;
pgs->view_clip = new_cpath;
*psaved = pgs->saved;
pgs->saved = 0;
return code;
fail:
if (new_cpath)
gx_cpath_free(new_cpath, "gs_gsave_for_save(view_clip)");
return code;
}
int
gs_grestore_only(gs_state * pgs)
{
gs_state *saved = pgs->saved;
void *pdata = pgs->client_data;
void *sdata;
gs_transparency_state_t *tstack = pgs->transparency_stack;
bool prior_overprint = pgs->overprint;
if_debug2('g', "[g]grestore 0x%lx, level was %d\n",
(ulong) saved, pgs->level);
if (!saved)
return 1;
sdata = saved->client_data;
if (saved->pattern_cache == 0)
saved->pattern_cache = pgs->pattern_cache;
pgs->client_data = sdata;
saved->client_data = pdata;
if (pdata != 0 && sdata != 0)
gstate_copy_client_data(pgs, pdata, sdata, copy_for_grestore);
gstate_free_contents(pgs);
*pgs = *saved;
pgs->transparency_stack = tstack;
if (pgs->show_gstate == saved)
pgs->show_gstate = pgs;
gs_free_object(pgs->memory, saved, "gs_grestore");
if (prior_overprint || pgs->overprint)
return gs_do_set_overprint(pgs);
else
return 0;
}
int
gs_grestore(gs_state * pgs)
{
int code;
if (!pgs->saved)
return gs_gsave(pgs);
code = gs_grestore_only(pgs);
if (code < 0)
return code;
if (pgs->saved)
return 0;
return gs_gsave(pgs);
}
int
gs_grestoreall_for_restore(gs_state * pgs, gs_state * saved)
{
int code;
while (pgs->saved->saved) {
code = gs_grestore(pgs);
if (code < 0)
return code;
}
if (pgs->pattern_cache)
(*pgs->pattern_cache->free_all) (pgs->pattern_cache);
pgs->saved->saved = saved;
code = gs_grestore(pgs);
if (code < 0)
return code;
if (pgs->view_clip) {
gx_cpath_free(pgs->view_clip, "gs_grestoreall_for_restore");
pgs->view_clip = 0;
}
return gs_grestore(pgs);
}
int
gs_grestoreall(gs_state * pgs)
{
if (!pgs->saved)
return gs_gsave(pgs);
while (pgs->saved->saved) {
int code = gs_grestore(pgs);
if (code < 0)
return code;
}
return gs_grestore(pgs);
}
gs_state *
gs_gstate(gs_state * pgs)
{
return gs_state_copy(pgs, pgs->memory);
}
gs_state *
gs_state_copy(gs_state * pgs, gs_memory_t * mem)
{
gs_state *pnew;
gx_clip_path *view_clip = pgs->view_clip;
pgs->view_clip = 0;
pnew = gstate_clone(pgs, mem, "gs_gstate", copy_for_gstate);
rc_increment(pnew->clip_stack);
rc_increment(pnew->dfilter_stack);
pgs->view_clip = view_clip;
if (pnew == 0)
return 0;
pnew->saved = 0;
pnew->show_gstate =
(pgs->show_gstate == pgs ? pnew : 0);
return pnew;
}
int
gs_copygstate(gs_state * pto, const gs_state * pfrom)
{
return gstate_copy(pto, pfrom, copy_for_copygstate, "gs_copygstate");
}
int
gs_currentgstate(gs_state * pto, const gs_state * pgs)
{
int code =
gstate_copy(pto, pgs, copy_for_currentgstate, "gs_currentgstate");
if (code >= 0)
pto->view_clip = 0;
return code;
}
int
gs_setgstate(gs_state * pgs, const gs_state * pfrom)
{
gs_state *saved_show = pgs->show_gstate;
int level = pgs->level;
gx_clip_path *view_clip = pgs->view_clip;
gs_transparency_state_t *tstack = pgs->transparency_stack;
int code;
pgs->view_clip = 0;
code = gstate_copy(pgs, pfrom, copy_for_setgstate, "gs_setgstate");
if (code < 0)
return code;
pgs->level = level;
pgs->view_clip = view_clip;
pgs->show_gstate =
(pgs->show_gstate == pfrom ? pgs : saved_show);
pgs->transparency_stack = tstack;
return gs_do_set_overprint(pgs);
}
gs_memory_t *
gs_state_memory(const gs_state * pgs)
{
return pgs->memory;
}
gs_state *
gs_state_saved(const gs_state * pgs)
{
return pgs->saved;
}
gs_state *
gs_state_swap_saved(gs_state * pgs, gs_state * new_saved)
{
gs_state *saved = pgs->saved;
pgs->saved = new_saved;
return saved;
}
gs_memory_t *
gs_state_swap_memory(gs_state * pgs, gs_memory_t * mem)
{
gs_memory_t *memory = pgs->memory;
pgs->memory = mem;
return memory;
}
int
gs_state_update_overprint(gs_state * pgs, const gs_overprint_params_t * pparams)
{
gs_composite_t * pct = 0;
gs_imager_state * pis = (gs_imager_state *)pgs;
int code;
gx_device * dev = pgs->device;
gx_device * ovptdev;
if ( (code = gs_create_overprint(&pct, pparams, pgs->memory)) >= 0 &&
(code = dev_proc(dev, create_compositor)( dev,
&ovptdev,
pct,
pis,
pgs->memory )) >= 0 ) {
if (ovptdev != dev)
gx_set_device_only(pgs, ovptdev);
}
if (pct != 0)
gs_free_object(pgs->memory, pct, "gs_state_update_overprint");
if (code == gs_error_unknownerror && !pparams->retain_any_comps)
code = 0;
return code;
}
int
gs_do_set_overprint(gs_state * pgs)
{
const gs_color_space * pcs = pgs->color_space;
const gs_client_color * pcc = pgs->ccolor;
int code = 0;
if (cs_num_components(pcs) < 0 && pcc->pattern != 0)
code = pcc->pattern->type->procs.set_color(pcc, pgs);
else
pcs->type->set_overprint(pcs, pgs);
return code;
}
void
gs_setoverprint(gs_state * pgs, bool ovp)
{
bool prior_ovp = pgs->overprint;
pgs->overprint = ovp;
if (prior_ovp != ovp)
(void)gs_do_set_overprint(pgs);
}
bool
gs_currentoverprint(const gs_state * pgs)
{
return pgs->overprint;
}
int
gs_setoverprintmode(gs_state * pgs, int mode)
{
int prior_mode = pgs->effective_overprint_mode;
int code = 0;
if (mode < 0 || mode > 1)
return_error(gs_error_rangecheck);
pgs->overprint_mode = mode;
if (pgs->overprint && prior_mode != mode)
code = gs_do_set_overprint(pgs);
return code;
}
int
gs_currentoverprintmode(const gs_state * pgs)
{
return pgs->overprint_mode;
}
int
gs_initgraphics(gs_state * pgs)
{
int code;
gs_initmatrix(pgs);
if ((code = gs_newpath(pgs)) < 0 ||
(code = gs_initclip(pgs)) < 0 ||
(code = gs_setlinewidth(pgs, 1.0)) < 0 ||
(code = gs_setlinecap(pgs, gstate_initial.line_params.cap)) < 0 ||
(code = gs_setlinejoin(pgs, gstate_initial.line_params.join)) < 0 ||
(code = gs_setcurvejoin(pgs, gstate_initial.line_params.curve_join)) < 0 ||
(code = gs_setdash(pgs, (float *)0, 0, 0.0)) < 0 ||
(gs_setdashadapt(pgs, false),
(code = gs_setdotlength(pgs, 0.0, false))) < 0 ||
(code = gs_setdotorientation(pgs)) < 0 ||
(code = gs_setmiterlimit(pgs, gstate_initial.line_params.miter_limit)) < 0
)
return code;
gs_init_rop(pgs);
return 0;
}
int
gs_setfilladjust(gs_state * pgs, floatp adjust_x, floatp adjust_y)
{
#define CLAMP_TO_HALF(v)\
((v) <= 0 ? fixed_0 : (v) >= 0.5 ? fixed_half : float2fixed(v));
pgs->fill_adjust.x = CLAMP_TO_HALF(adjust_x);
pgs->fill_adjust.y = CLAMP_TO_HALF(adjust_y);
return 0;
#undef CLAMP_TO_HALF
}
int
gs_currentfilladjust(const gs_state * pgs, gs_point * adjust)
{
adjust->x = fixed2float(pgs->fill_adjust.x);
adjust->y = fixed2float(pgs->fill_adjust.y);
return 0;
}
void
gs_setlimitclamp(gs_state * pgs, bool clamp)
{
pgs->clamp_coordinates = clamp;
}
bool
gs_currentlimitclamp(const gs_state * pgs)
{
return pgs->clamp_coordinates;
}
void
gs_settextrenderingmode(gs_state * pgs, uint trm)
{
pgs->text_rendering_mode = trm;
}
uint
gs_currenttextrenderingmode(const gs_state * pgs)
{
return pgs->text_rendering_mode;
}
private void
gstate_free_parts(const gs_state * parts, gs_memory_t * mem, client_name_t cname)
{
gs_free_object(mem, parts->dev_color, cname);
gs_free_object(mem, parts->ccolor, cname);
gs_free_object(mem, parts->color_space, cname);
if (!parts->effective_clip_shared)
gx_cpath_free(parts->effective_clip_path, cname);
gx_cpath_free(parts->clip_path, cname);
gx_path_free(parts->path, cname);
}
private int
gstate_alloc_parts(gs_state * parts, const gs_state * shared,
gs_memory_t * mem, client_name_t cname)
{
gs_memory_t *path_mem = gstate_path_memory(mem);
parts->path =
(shared ?
gx_path_alloc_shared(shared->path, path_mem,
"gstate_alloc_parts(path)") :
gx_path_alloc(path_mem, "gstate_alloc_parts(path)"));
parts->clip_path =
(shared ?
gx_cpath_alloc_shared(shared->clip_path, mem,
"gstate_alloc_parts(clip_path)") :
gx_cpath_alloc(mem, "gstate_alloc_parts(clip_path)"));
if (!shared || shared->effective_clip_shared) {
parts->effective_clip_path = parts->clip_path;
parts->effective_clip_shared = true;
} else {
parts->effective_clip_path =
gx_cpath_alloc_shared(shared->effective_clip_path, mem,
"gstate_alloc_parts(effective_clip_path)");
parts->effective_clip_shared = false;
}
parts->color_space =
gs_alloc_struct(mem, gs_color_space, &st_color_space, cname);
parts->ccolor =
gs_alloc_struct(mem, gs_client_color, &st_client_color, cname);
parts->dev_color =
gs_alloc_struct(mem, gx_device_color, &st_device_color, cname);
if (parts->path == 0 || parts->clip_path == 0 ||
parts->effective_clip_path == 0 ||
parts->color_space == 0 || parts->ccolor == 0 ||
parts->dev_color == 0
) {
gstate_free_parts(parts, mem, cname);
return_error(gs_error_VMerror);
}
return 0;
}
private gs_state *
gstate_alloc(gs_memory_t * mem, client_name_t cname, const gs_state * pfrom)
{
gs_state *pgs =
gs_alloc_struct(mem, gs_state, &st_gs_state, cname);
if (pgs == 0)
return 0;
if (gstate_alloc_parts(pgs, pfrom, mem, cname) < 0) {
gs_free_object(mem, pgs, cname);
return 0;
}
pgs->memory = mem;
return pgs;
}
private int
gstate_copy_dash(gs_state * pto, const gs_state * pfrom)
{
return gs_setdash(pto, pfrom->line_params.dash.pattern,
pfrom->line_params.dash.pattern_size,
pfrom->line_params.dash.offset);
}
private gs_state *
gstate_clone(gs_state * pfrom, gs_memory_t * mem, client_name_t cname,
gs_state_copy_reason_t reason)
{
gs_state *pgs = gstate_alloc(mem, cname, pfrom);
gs_state_parts parts;
if (pgs == 0)
return 0;
GSTATE_ASSIGN_PARTS(&parts, pgs);
*pgs = *pfrom;
pgs->transparency_stack = 0;
if (pgs->line_params.dash.pattern) {
int code;
pgs->line_params.dash.pattern = 0;
code = gstate_copy_dash(pgs, pfrom);
if (code < 0)
goto fail;
}
if (pgs->client_data != 0) {
void *pdata = pgs->client_data = (*pgs->client_procs.alloc) (mem);
if (pdata == 0 ||
gstate_copy_client_data(pgs, pdata, pfrom->client_data, reason) < 0
)
goto fail;
}
gs_imager_state_copied((gs_imager_state *)pgs);
rc_increment(pgs->device);
*parts.color_space = *pfrom->color_space;
*parts.ccolor = *pfrom->ccolor;
*parts.dev_color = *pfrom->dev_color;
if (reason == copy_for_gsave) {
float *dfrom = pfrom->line_params.dash.pattern;
float *dto = pgs->line_params.dash.pattern;
GSTATE_ASSIGN_PARTS(pfrom, &parts);
pgs->line_params.dash.pattern = dfrom;
pfrom->line_params.dash.pattern = dto;
} else {
GSTATE_ASSIGN_PARTS(pgs, &parts);
}
cs_adjust_counts(pgs, 1);
return pgs;
fail:
gs_free_object(mem, pgs->line_params.dash.pattern, cname);
GSTATE_ASSIGN_PARTS(pgs, &parts);
gstate_free_parts(pgs, mem, cname);
gs_free_object(mem, pgs, cname);
return 0;
}
private void
gstate_free_contents(gs_state * pgs)
{
gs_memory_t *mem = pgs->memory;
const char *const cname = "gstate_free_contents";
rc_decrement(pgs->device, cname);
rc_decrement(pgs->clip_stack, cname);
rc_decrement(pgs->dfilter_stack, cname);
cs_adjust_counts(pgs, -1);
if (pgs->client_data != 0)
(*pgs->client_procs.free) (pgs->client_data, mem);
gs_free_object(mem, pgs->line_params.dash.pattern, cname);
gstate_free_parts(pgs, mem, cname);
gs_imager_state_release((gs_imager_state *)pgs);
}
private int
gstate_copy(gs_state * pto, const gs_state * pfrom,
gs_state_copy_reason_t reason, client_name_t cname)
{
gs_state_parts parts;
GSTATE_ASSIGN_PARTS(&parts, pto);
if (pfrom->line_params.dash.pattern || pto->line_params.dash.pattern) {
int code = gstate_copy_dash(pto, pfrom);
if (code < 0)
return code;
}
cs_adjust_counts(pto, -1);
gx_path_assign_preserve(pto->path, pfrom->path);
gx_cpath_assign_preserve(pto->clip_path, pfrom->clip_path);
if (pfrom->effective_clip_shared) {
parts.effective_clip_path =
(pfrom->effective_clip_path == pfrom->view_clip ?
pto->view_clip : parts.clip_path);
} else
gx_cpath_assign_preserve(pto->effective_clip_path,
pfrom->effective_clip_path);
*parts.color_space = *pfrom->color_space;
*parts.ccolor = *pfrom->ccolor;
*parts.dev_color = *pfrom->dev_color;
cs_adjust_counts(pto, 1);
#define RCCOPY(element)\
rc_pre_assign(pto->element, pfrom->element, cname)
RCCOPY(device);
RCCOPY(clip_stack);
RCCOPY(dfilter_stack);
{
struct gx_pattern_cache_s *pcache = pto->pattern_cache;
void *pdata = pto->client_data;
gs_memory_t *mem = pto->memory;
gs_state *saved = pto->saved;
float *pattern = pto->line_params.dash.pattern;
gs_imager_state_pre_assign((gs_imager_state *)pto,
(const gs_imager_state *)pfrom);
*pto = *pfrom;
pto->client_data = pdata;
pto->memory = mem;
pto->saved = saved;
pto->line_params.dash.pattern = pattern;
if (pto->pattern_cache == 0)
pto->pattern_cache = pcache;
if (pfrom->client_data != 0) {
gstate_copy_client_data((gs_state *) pfrom, pdata,
pfrom->client_data, reason);
}
}
GSTATE_ASSIGN_PARTS(pto, &parts);
#undef RCCOPY
pto->show_gstate =
(pfrom->show_gstate == pfrom ? pto : 0);
return 0;
}
gs_id gx_get_clip_path_id(gs_state *pgs)
{
return pgs->clip_path->id;
}