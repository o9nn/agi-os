#ifndef gxhldevc_INCLUDED
#  define gxhldevc_INCLUDED
#include "gsdcolor.h"
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
typedef struct gx_hl_saved_color_s {
gs_id color_space_id;
gs_id pattern_id;
bool ccolor_valid;
gs_client_color ccolor;
gx_device_color_saved saved_dev_color;
} gx_hl_saved_color;
void gx_hld_saved_color_init(gx_hl_saved_color * psc);
const gs_state * gx_hld_get_gstate_ptr(const gs_imager_state * pis);
bool gx_hld_save_color(const gs_imager_state * pis,
const gx_device_color * pdevc, gx_hl_saved_color * psc);
bool gx_hld_saved_color_equal(const gx_hl_saved_color * psc1,
const gx_hl_saved_color * psc2);
bool gx_hld_saved_color_same_cspace(const gx_hl_saved_color * psc1,
const gx_hl_saved_color * psc2);
bool
gx_hld_is_hl_color_available(const gs_imager_state * pis,
const gx_device_color * pdevc);
typedef enum {
non_pattern_color_space,
pattern_color_sapce,
use_process_color
} gx_hld_get_color_space_and_ccolor_status;
gx_hld_get_color_space_and_ccolor_status gx_hld_get_color_space_and_ccolor(
const gs_imager_state * pis, const gx_device_color * pdevc,
const gs_color_space ** ppcs, const gs_client_color ** ppcc);
int gx_hld_get_number_color_components(const gs_imager_state * pis);
typedef enum {
valid_result = 1,
invalid_color_info = 2,
invalid_component_requested = 3
} gx_hld_get_color_component_status;
gx_hld_get_color_component_status gx_hld_get_color_component(
const gs_imager_state * pis, const gx_device_color * pdevc,
int comp_numi, float * output);
#endif