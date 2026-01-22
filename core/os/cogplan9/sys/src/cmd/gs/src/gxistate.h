#ifndef gxistate_INCLUDED
#  define gxistate_INCLUDED
#include "gscsel.h"
#include "gsrefct.h"
#include "gsropt.h"
#include "gstparam.h"
#include "gxcvalue.h"
#include "gxcmap.h"
#include "gxfixed.h"
#include "gxline.h"
#include "gxmatrix.h"
#include "gxtmap.h"
#include "gscspace.h"
#include "gstrans.h"
#ifndef gs_halftone_DEFINED
#  define gs_halftone_DEFINED
typedef struct gs_halftone_s gs_halftone;
#endif
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#ifndef gx_device_halftone_DEFINED
#  define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
typedef struct gx_transfer_s {
int red_component_num;
gx_transfer_map *red;
int green_component_num;
gx_transfer_map *green;
int blue_component_num;
gx_transfer_map *blue;
int gray_component_num;
gx_transfer_map *gray;
} gx_transfer;
#define gs_color_rendering_state_common\
\
\
\
gs_halftone *halftone;			\
gs_int_point screen_phase[gs_color_select_count];\
\
gx_device_halftone *dev_ht;		\
\
\
\
struct gs_cie_render_s *cie_render;	\
gx_transfer_map *black_generation;	\
gx_transfer_map *undercolor_removal;	\
\
\
\
\
\
gx_transfer set_transfer;		\
gx_transfer_map *effective_transfer[GX_DEVICE_COLOR_MAX_COMPONENTS]; \
\
\
\
\
\
struct gx_cie_joint_caches_s *cie_joint_caches;		\
\
const struct gx_color_map_procs_s *cmap_procs;		\
\
gs_devicen_color_map color_component_map;\
\
\
\
struct gx_pattern_cache_s *pattern_cache
#define gs_cr_state_do_rc_ptrs(m)\
m(halftone) m(dev_ht) m(cie_render)\
m(black_generation) m(undercolor_removal)\
m(set_transfer.red) m(set_transfer.green)\
m(set_transfer.blue) m(set_transfer.gray)\
m(cie_joint_caches)
#define gs_cr_state_do_ptrs(m)\
m(0,halftone) m(1,dev_ht)\
m(2,cie_render) m(3,black_generation) m(4,undercolor_removal)\
m(5,set_transfer.red) m(6,set_transfer.green)\
m(7,set_transfer.blue) m(8,set_transfer.gray)\
m(9,cie_joint_caches) m(10,pattern_cache)
#define st_cr_state_num_ptrs 11
typedef struct gs_devicen_color_map_s {
bool use_alt_cspace;
separation_type sep_type;
uint num_components;
uint num_colorants;
gs_id cspace_id;
int color_map[GS_CLIENT_COLOR_MAX_COMPONENTS];
} gs_devicen_color_map;
#define gs_imager_state_common\
gs_memory_t *memory;\
void *client_data;\
gx_line_params line_params;\
gs_matrix_fixed ctm;\
bool current_point_valid;\
gs_point current_point;\
gs_point subpath_start;\
bool clamp_coordinates;\
gs_logical_operation_t log_op;\
gx_color_value alpha;\
gs_blend_mode_t blend_mode;\
gs_transparency_source_t opacity, shape;\
gs_id soft_mask_id;\
bool text_knockout;\
uint text_rendering_mode;\
gs_transparency_state_t *transparency_stack;\
bool overprint;\
int overprint_mode;\
int effective_overprint_mode;\
float flatness;\
gs_fixed_point fill_adjust; \
bool stroke_adjust;\
bool accurate_curves;\
bool have_pattern_streams;\
float smoothness;\
const gx_color_map_procs *\
(*get_cmap_procs)(const gs_imager_state *, const gx_device *);\
gs_color_rendering_state_common
#define st_imager_state_num_ptrs\
(st_line_params_num_ptrs + st_cr_state_num_ptrs + 4)
#define ctm_only(pis) (*(const gs_matrix *)&(pis)->ctm)
#define ctm_only_writable(pis) (*(gs_matrix *)&(pis)->ctm)
#define set_ctm_only(pis, mat) (*(gs_matrix *)&(pis)->ctm = (mat))
#define gs_init_rop(pis) ((pis)->log_op = lop_default)
#define gs_currentflat_inline(pis) ((pis)->flatness)
#define gs_currentlineparams_inline(pis) (&(pis)->line_params)
#define gs_current_logical_op_inline(pis) ((pis)->log_op)
#define gs_set_logical_op_inline(pis, lop) ((pis)->log_op = (lop))
#ifndef gs_imager_state_DEFINED
#  define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
struct gs_imager_state_s {
gs_imager_state_common;
};
#define gs_imager_state_initial(scale)\
0, 0, { gx_line_params_initial },\
{ (float)(scale), 0.0, 0.0, (float)(-(scale)), 0.0, 0.0 },\
false, {0, 0}, {0, 0}, false, \
lop_default, gx_max_color_value, BLEND_MODE_Compatible,\
{ 1.0, 0 }, { 1.0, 0 }, 0, 0, 0, 0, 0, 0, 0, 1.0,\
{ fixed_half, fixed_half }, 0, 0, 0, 1.0,\
gx_default_get_cmap_procs
#define public_st_imager_state()	\
gs_public_st_composite(st_imager_state, gs_imager_state, "gs_imager_state",\
imager_state_enum_ptrs, imager_state_reloc_ptrs)
int gs_imager_state_initialize(gs_imager_state * pis, gs_memory_t * mem);
gs_imager_state *
gs_imager_state_copy(const gs_imager_state * pis, gs_memory_t * mem);
void gs_imager_state_copied(gs_imager_state * pis);
void gs_imager_state_pre_assign(gs_imager_state *to,
const gs_imager_state *from);
void gs_imager_state_release(gs_imager_state * pis);
int gs_currentscreenphase_pis(const gs_imager_state *, gs_int_point *, gs_color_select_t);
#endif