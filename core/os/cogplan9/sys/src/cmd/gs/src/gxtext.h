#ifndef gxtext_INCLUDED
#  define gxtext_INCLUDED
#include "gstext.h"
#include "gsrefct.h"
typedef struct gs_text_enum_procs_s gs_text_enum_procs_t;
#ifndef cached_fm_pair_DEFINED
#  define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
typedef struct gs_text_returned_s {
gs_char current_char;
gs_glyph current_glyph;
gs_point total_width;
} gs_text_returned_t;
#define MAX_FONT_STACK 5
typedef struct gx_font_stack_item_s {
gs_font *font;
uint index;
} gx_font_stack_item_t;
typedef struct gx_font_stack_s {
int depth;
gx_font_stack_item_t items[1 + MAX_FONT_STACK];
} gx_font_stack_t;
rc_free_proc(rc_free_text_enum);
#define gs_text_enum_common\
\
gs_text_params_t text;	\
gx_device *dev;\
gx_device *imaging_dev;	\
gs_imager_state *pis;\
gs_font *orig_font;\
gx_path *path;			\
const gx_device_color *pdcolor;	\
const gx_clip_path *pcpath;		\
gs_memory_t *memory;\
\
const gs_text_enum_procs_t *procs;\
\
\
rc_header rc;\
gs_font *current_font; \
gs_glyph outer_CID; \
bool is_pure_color; \
gs_log2_scale_point log2_scale;	\
cached_fm_pair *pair; \
uint index;			\
uint xy_index;		\
gx_font_stack_t fstack;\
int cmap_code;		\
\
gs_point FontBBox_as_Metrics2;  \
\
bool device_disabled_grid_fitting;\
\
gs_text_returned_t returned
struct gs_text_enum_s {
gs_text_enum_common;
};
#define st_gs_text_enum_max_ptrs (st_gs_text_params_max_ptrs + 8)
#define public_st_gs_text_enum()	\
gs_public_st_composite(st_gs_text_enum, gs_text_enum_t, "gs_text_enum_t",\
text_enum_enum_ptrs, text_enum_reloc_ptrs)
int gs_text_enum_init(gs_text_enum_t *pte,
const gs_text_enum_procs_t *procs,
gx_device *dev, gs_imager_state *pis,
const gs_text_params_t *text,
gs_font *font, gx_path *path,
const gx_device_color *pdcolor,
const gx_clip_path *pcpath,
gs_memory_t *mem);
void gs_text_enum_copy_dynamic(gs_text_enum_t *pto,
const gs_text_enum_t *pfrom,
bool for_return);
#define SHOW_IS(penum, op_mask)\
(((penum)->text.operation & (op_mask)) != 0)
#define SHOW_IS_ALL_OF(penum, op_mask)\
(((penum)->text.operation & (op_mask)) == (op_mask))
#define SHOW_IS_ADD_TO_ALL(penum)	\
SHOW_IS(penum, TEXT_ADD_TO_ALL_WIDTHS)
#define SHOW_IS_ADD_TO_SPACE(penum)	\
SHOW_IS(penum, TEXT_ADD_TO_SPACE_WIDTH)
#define SHOW_IS_DO_KERN(penum)		\
SHOW_IS(penum, TEXT_INTERVENE)
#define SHOW_IS_SLOW(penum)		\
SHOW_IS(penum, TEXT_REPLACE_WIDTHS | TEXT_ADD_TO_ALL_WIDTHS | TEXT_ADD_TO_SPACE_WIDTH | TEXT_INTERVENE)
#define SHOW_IS_DRAWING(penum)		\
!SHOW_IS(penum, TEXT_DO_NONE)
#define SHOW_IS_STRINGWIDTH(penum)	\
SHOW_IS_ALL_OF(penum, TEXT_DO_NONE | TEXT_RETURN_WIDTH)
struct gs_text_enum_procs_s {
#define text_enum_proc_resync(proc)\
int proc(gs_text_enum_t *pte, const gs_text_enum_t *pfrom)
text_enum_proc_resync((*resync));
#define text_enum_proc_process(proc)\
int proc(gs_text_enum_t *pte)
text_enum_proc_process((*process));
#define text_enum_proc_is_width_only(proc)\
bool proc(const gs_text_enum_t *pte)
text_enum_proc_is_width_only((*is_width_only));
#define text_enum_proc_current_width(proc)\
int proc(const gs_text_enum_t *pte, gs_point *pwidth)
text_enum_proc_current_width((*current_width));
#define text_enum_proc_set_cache(proc)\
int proc(gs_text_enum_t *pte, const double *values,\
gs_text_cache_control_t control)
text_enum_proc_set_cache((*set_cache));
#define text_enum_proc_retry(proc)\
int proc(gs_text_enum_t *pte)
text_enum_proc_retry((*retry));
#define text_enum_proc_release(proc)\
void proc(gs_text_enum_t *pte, client_name_t cname)
text_enum_proc_release((*release));
};
text_enum_proc_release(gx_default_text_release);
#endif