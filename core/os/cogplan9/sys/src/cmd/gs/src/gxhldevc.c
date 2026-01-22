#include "gx.h"
#include "gzstate.h"
#include "gscspace.h"
#include "gxcspace.h"
#include "gxhldevc.h"
#include "memory_.h"
#include "gxpcolor.h"
#include "gsptype2.h"
void gx_hld_saved_color_init(gx_hl_saved_color * psc)
{
gx_device_color temp_devc;
memset(psc, 0, sizeof(*psc));
psc->color_space_id = psc->pattern_id = gs_no_id;
color_set_null(&temp_devc);
temp_devc.type->save_dc(&temp_devc, &(psc->saved_dev_color));
}
const gs_state * gx_hld_get_gstate_ptr(const gs_imager_state * pis)
{
extern_st(st_gs_state);
if (pis == NULL || gs_object_type(pis->memory, pis) != &st_gs_state)
return NULL;
return (const gs_state *) pis;
}
bool
gx_hld_save_color(const gs_imager_state * pis, const gx_device_color * pdevc,
gx_hl_saved_color * psc)
{
const gs_state * pgs = gx_hld_get_gstate_ptr(pis);
memset(psc, 0, sizeof(*psc));
if (pdevc == NULL) {
gx_hld_saved_color_init(psc);
return false;
} else if (pgs == NULL) {
psc->color_space_id = psc->pattern_id = gs_no_id;
pdevc->type->save_dc(pdevc, &(psc->saved_dev_color));
return false;
} else {
const gs_color_space * pcs = pgs->color_space;
int i = gs_color_space_num_components(pcs);
psc->color_space_id = pcs->id;
pdevc->type->save_dc(pdevc, &(psc->saved_dev_color));
i = any_abs(i);
for (i--; i >= 0; i--)
psc->ccolor.paint.values[i] = pdevc->ccolor.paint.values[i];
if ((pdevc->type == gx_dc_type_pattern
|| pdevc->type == gx_dc_type_pattern2) && pdevc->ccolor_valid)
psc->pattern_id = pdevc->ccolor.pattern->pattern_id;
else
psc->pattern_id = gs_no_id;
return true;
}
}
bool gx_hld_saved_color_equal(const gx_hl_saved_color * psc1,
const gx_hl_saved_color * psc2)
{
return (memcmp(psc1, psc2, sizeof(*psc1)) == 0);
}
bool gx_hld_saved_color_same_cspace(const gx_hl_saved_color * psc1,
const gx_hl_saved_color * psc2)
{
if (psc1->color_space_id != psc2->color_space_id)
return false;
if (psc1->pattern_id != psc2->pattern_id)
return false;
if (psc1->ccolor_valid != psc2->ccolor_valid)
return false;
if (psc1->color_space_id != psc2->color_space_id)
return false;
return true;
}
bool
gx_hld_is_hl_color_available(const gs_imager_state * pis,
const gx_device_color * pdevc)
{
const gs_state * pgs = gx_hld_get_gstate_ptr(pis);
if (pgs != NULL && pdevc != NULL && pdevc->ccolor_valid)
return true;
return false;
}
gx_hld_get_color_space_and_ccolor_status
gx_hld_get_color_space_and_ccolor(const gs_imager_state * pis,
const gx_device_color * pdevc, const gs_color_space ** ppcs,
const gs_client_color ** ppcc)
{
if (gx_hld_is_hl_color_available(pis, pdevc)) {
const gs_state * pgs = gx_hld_get_gstate_ptr(pis);
const gs_color_space * pcs = pgs->color_space;
*ppcs = pcs;
*ppcc = &(pdevc->ccolor);
if (pdevc->type == gx_dc_type_pattern
|| pdevc->type == &gx_dc_pure_masked
|| pdevc->type == gx_dc_type_pattern2)
return pattern_color_sapce;
else {
return non_pattern_color_space;
}
}
*ppcs = NULL;
*ppcc = NULL;
return use_process_color;
}
int
gx_hld_get_number_color_components(const gs_imager_state * pis)
{
const gs_state * pgs = gx_hld_get_gstate_ptr(pis);
if (pgs != NULL) {
const gs_color_space * pcs = pgs->color_space;
int n = gs_color_space_num_components(pcs);
return (n >= 0 ? n : -n - 1);
} else
return -1;
}
gx_hld_get_color_component_status
gx_hld_get_color_component(const gs_imager_state * pis,
const gx_device_color * pdevc,
int comp_num, float * output)
{
if (pdevc != NULL && pdevc->ccolor_valid) {
int ncomp = gx_hld_get_number_color_components(pis);
if (ncomp < 0)
return invalid_color_info;
if (comp_num < 0 || comp_num >= ncomp)
return invalid_component_requested;
*output = pdevc->ccolor.paint.values[comp_num];
return valid_result;
}
return invalid_color_info;
}