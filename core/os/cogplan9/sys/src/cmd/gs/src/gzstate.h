#ifndef gzstate_INCLUDED
# define gzstate_INCLUDED
#include "gscpm.h"
#include "gscspace.h"
#include "gsrefct.h"
#include "gxdcolor.h"
#include "gxistate.h"
#include "gsstate.h"
#include "gxstate.h"
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef gx_clip_path_DEFINED
# define gx_clip_path_DEFINED
typedef struct gx_clip_path_s gx_clip_path;
#endif
#ifndef gx_clip_stack_DEFINED
# define gx_clip_stack_DEFINED
typedef struct gx_clip_stack_s gx_clip_stack_t;
#endif
#ifndef gs_color_space_DEFINED
# define gs_color_space_DEFINED
typedef struct gs_color_space_s gs_color_space;
#endif
#ifndef gs_client_color_DEFINED
# define gs_client_color_DEFINED
typedef struct gs_client_color_s gs_client_color;
#endif
#ifndef gs_font_DEFINED
# define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef gs_transparency_group_DEFINED
# define gs_transparency_group_DEFINED
typedef struct gs_transparency_group_s gs_transparency_group_t;
#endif
#ifndef gs_device_filter_stack_DEFINED
# define gs_device_filter_stack_DEFINED
typedef struct gs_device_filter_stack_s gs_device_filter_stack_t;
#endif
#ifndef gs_device_filter_DEFINED
# define gs_device_filter_DEFINED
typedef struct gs_device_filter_s gs_device_filter_t;
#endif
struct gs_device_filter_stack_s {
gs_device_filter_stack_t *next;
gs_device_filter_t *df;
gx_device *next_device;
rc_header rc;
};
struct gs_state_s {
gs_imager_state_common;
gs_state *saved;
gs_matrix ctm_inverse;
bool ctm_inverse_valid;
gs_matrix ctm_default;
bool ctm_default_set;
gx_path *path;
gx_clip_path *clip_path;
gx_clip_stack_t *clip_stack;
gx_clip_path *view_clip;
gs_id effective_clip_id;
gs_id effective_view_clip_id;
gx_clip_path *effective_clip_path;
bool effective_clip_shared;
gs_color_space *color_space;
gs_client_color *ccolor;
gx_device_color *dev_color;
gs_font *font;
gs_font *root_font;
gs_matrix_fixed char_tm;
#define char_tm_only(pgs) *(gs_matrix *)&(pgs)->char_tm
bool char_tm_valid;
gs_in_cache_device_t in_cachedevice;
gs_char_path_mode in_charpath;
gs_state *show_gstate;
int level;
gx_device *device;
#undef gs_currentdevice_inline
#define gs_currentdevice_inline(pgs) ((pgs)->device)
gs_device_filter_stack_t *dfilter_stack;
gs_transparency_group_t *transparency_group_stack;
#define gs_state_client_data(pgs) ((pgs)->client_data)
gs_state_client_procs client_procs;
};
#define public_st_gs_state() \
gs_public_st_composite(st_gs_state, gs_state, "gs_state",\
gs_state_enum_ptrs, gs_state_reloc_ptrs)
#define gs_state_do_ptrs(m)\
m(0,saved) m(1,path) m(2,clip_path) m(3,clip_stack)\
m(4,view_clip) m(5,effective_clip_path)\
m(6,color_space) m(7,ccolor) m(8,dev_color)\
m(9,font) m(10,root_font) m(11,show_gstate) \
m(12,transparency_group_stack)
#define gs_state_num_ptrs 13
#define gx_setcurrentpoint(pgs, xx, yy)\
(pgs)->current_point.x = xx;\
(pgs)->current_point.y = yy;
#endif