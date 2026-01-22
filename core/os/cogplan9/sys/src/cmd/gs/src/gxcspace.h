#ifndef gxcspace_INCLUDED
# define gxcspace_INCLUDED
#include "gscspace.h"
#include "gsccolor.h"
#include "gscsel.h"
#include "gxfrac.h"
#ifndef gx_device_color_DEFINED
# define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
struct gs_color_space_type_s {
gs_color_space_index index;
bool can_be_base_space;
bool can_be_alt_space;
gs_memory_type_ptr_t stype;
#define cs_proc_num_components(proc)\
int proc(const gs_color_space *)
#define cs_num_components(pcs)\
(*(pcs)->type->num_components)(pcs)
cs_proc_num_components((*num_components));
#define cs_proc_base_space(proc)\
const gs_color_space *proc(const gs_color_space *)
#define cs_base_space(pcs)\
(*(pcs)->type->base_space)(pcs)
cs_proc_base_space((*base_space));
#define cs_proc_init_color(proc)\
void proc(gs_client_color *, const gs_color_space *)
#define cs_init_color(pcc, pcs)\
(*(pcs)->type->init_color)(pcc, pcs)
#define cs_full_init_color(pcc, pcs)\
((pcc)->pattern = 0, cs_init_color(pcc, pcs))
cs_proc_init_color((*init_color));
#define cs_proc_restrict_color(proc)\
void proc(gs_client_color *, const gs_color_space *)
#define cs_restrict_color(pcc, pcs)\
((pcs)->type->restrict_color(pcc, pcs))
cs_proc_restrict_color((*restrict_color));
#define cs_proc_concrete_space(proc)\
const gs_color_space *proc(const gs_color_space *,\
const gs_imager_state *)
#define cs_concrete_space(pcs, pis)\
(*(pcs)->type->concrete_space)(pcs, pis)
cs_proc_concrete_space((*concrete_space));
#define cs_proc_concretize_color(proc)\
int proc(const gs_client_color *, const gs_color_space *,\
frac *, const gs_imager_state *)
#define cs_concretize_color(pcc, pcs, values, pis)\
(*(pcs)->type->concretize_color)(pcc, pcs, values, pis)
cs_proc_concretize_color((*concretize_color));
#define cs_proc_remap_concrete_color(proc)\
int proc(const frac *, const gs_color_space * pcs, gx_device_color *,\
const gs_imager_state *, gx_device *, gs_color_select_t)
cs_proc_remap_concrete_color((*remap_concrete_color));
#define cs_proc_remap_color(proc)\
int proc(const gs_client_color *, const gs_color_space *,\
gx_device_color *, const gs_imager_state *, gx_device *,\
gs_color_select_t)
cs_proc_remap_color((*remap_color));
#define cs_proc_install_cspace(proc)\
int proc(const gs_color_space *, gs_state *)
cs_proc_install_cspace((*install_cspace));
#define cs_proc_set_overprint(proc)\
int proc(const gs_color_space *, gs_state *)
cs_proc_set_overprint((*set_overprint));
#define cs_proc_adjust_cspace_count(proc)\
void proc(const gs_color_space *, int)
#define cs_adjust_cspace_count(pgs, delta)\
(*(pgs)->color_space->type->adjust_cspace_count)((pgs)->color_space, delta)
cs_proc_adjust_cspace_count((*adjust_cspace_count));
#define cs_proc_adjust_color_count(proc)\
void proc(const gs_client_color *, const gs_color_space *, int)
#define cs_adjust_color_count(pgs, delta)\
(*(pgs)->color_space->type->adjust_color_count)\
((pgs)->ccolor, (pgs)->color_space, delta)
cs_proc_adjust_color_count((*adjust_color_count));
#define cs_adjust_counts(pgs, delta)\
(cs_adjust_color_count(pgs, delta), cs_adjust_cspace_count(pgs, delta))
#define cs_proc_serialize(proc)\
int proc(const gs_color_space *, stream *)
#define cs_serialize(pcs, s)\
(*(pcs)->type->serialize)(pcs, s)
cs_proc_serialize((*serialize));
#define cs_proc_is_linear(proc)\
int proc(gs_direct_color_space *cs, const gs_imager_state * pis,\
gx_device *dev,\
const gs_client_color *c0, const gs_client_color *c1,\
const gs_client_color *c2, const gs_client_color *c3,\
float smoothness)
#define cs_is_linear(pcs, pis, dev, c0, c1, c2, c3, smoothness)\
(*(pcs)->type->is_linear)(pcs, pis, dev, c0, c1, c2, c3, smoothness)
cs_proc_is_linear((*is_linear));
};
extern_st(st_base_color_space);
#define public_st_base_color_space() \
gs_public_st_simple(st_base_color_space, gs_base_color_space,\
"gs_base_color_space")
cs_proc_num_components(gx_num_components_1);
cs_proc_num_components(gx_num_components_3);
cs_proc_num_components(gx_num_components_4);
cs_proc_base_space(gx_no_base_space);
cs_proc_init_color(gx_init_paint_1);
cs_proc_init_color(gx_init_paint_3);
cs_proc_init_color(gx_init_paint_4);
cs_proc_restrict_color(gx_restrict01_paint_1);
cs_proc_restrict_color(gx_restrict01_paint_3);
cs_proc_restrict_color(gx_restrict01_paint_4);
cs_proc_concrete_space(gx_no_concrete_space);
cs_proc_concrete_space(gx_same_concrete_space);
cs_proc_concretize_color(gx_no_concretize_color);
cs_proc_remap_color(gx_default_remap_color);
cs_proc_install_cspace(gx_no_install_cspace);
cs_proc_set_overprint(gx_spot_colors_set_overprint);
cs_proc_adjust_cspace_count(gx_no_adjust_cspace_count);
cs_proc_adjust_color_count(gx_no_adjust_color_count);
cs_proc_serialize(gx_serialize_cspace_type);
cs_proc_is_linear(gx_cspace_no_linear);
cs_proc_is_linear(gx_cspace_is_linear_default);
cs_proc_remap_color(gx_remap_DeviceGray);
cs_proc_concretize_color(gx_concretize_DeviceGray);
cs_proc_remap_concrete_color(gx_remap_concrete_DGray);
cs_proc_remap_color(gx_remap_DeviceRGB);
cs_proc_concretize_color(gx_concretize_DeviceRGB);
cs_proc_remap_concrete_color(gx_remap_concrete_DRGB);
cs_proc_remap_color(gx_remap_DeviceCMYK);
cs_proc_concretize_color(gx_concretize_DeviceCMYK);
cs_proc_remap_concrete_color(gx_remap_concrete_DCMYK);
extern_st(st_color_space);
void gs_cspace_init(gs_color_space *pcs,
const gs_color_space_type *pcstype,
gs_memory_t *mem, bool isheap);
int gs_cspace_alloc(gs_color_space **ppcspace,
const gs_color_space_type *pcstype,
gs_memory_t *mem);
#endif