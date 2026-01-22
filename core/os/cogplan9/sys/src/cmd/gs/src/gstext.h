#ifndef gstext_INCLUDED
# define gstext_INCLUDED
#include "gsccode.h"
#include "gscpm.h"
#define TEXT_HAS_MORE_THAN_ONE_(op, any)\
( ((op) & any) & (((op) & any) - 1) )
#define TEXT_OPERATION_IS_INVALID(op)\
(!((op) & TEXT_FROM_ANY) ||\
!((op) & TEXT_DO_ANY) ||\
TEXT_HAS_MORE_THAN_ONE_(op, TEXT_FROM_ANY) ||\
TEXT_HAS_MORE_THAN_ONE_(op, TEXT_DO_ANY) ||\
(((op) & TEXT_ADD_ANY) && ((op) & TEXT_REPLACE_WIDTHS))\
)
#define TEXT_PARAMS_ARE_INVALID(params)\
(TEXT_OPERATION_IS_INVALID((params)->operation) ||\
( ((params)->operation & TEXT_FROM_ANY_SINGLE) && ((params)->size != 1) )\
)
#define TEXT_FROM_STRING 0x00001
#define TEXT_FROM_BYTES 0x00002
#define TEXT_FROM_CHARS 0x00004
#define TEXT_FROM_GLYPHS 0x00008
#define TEXT_FROM_SINGLE_CHAR 0x00010
#define TEXT_FROM_SINGLE_GLYPH 0x00020
#define TEXT_FROM_ANY_SINGLE \
(TEXT_FROM_SINGLE_CHAR | TEXT_FROM_SINGLE_GLYPH)
#define TEXT_FROM_ANY \
(TEXT_FROM_STRING | TEXT_FROM_BYTES | TEXT_FROM_CHARS | TEXT_FROM_GLYPHS |\
TEXT_FROM_ANY_SINGLE)
#define TEXT_ADD_TO_ALL_WIDTHS 0x00040
#define TEXT_ADD_TO_SPACE_WIDTH 0x00080
#define TEXT_ADD_ANY \
(TEXT_ADD_TO_ALL_WIDTHS | TEXT_ADD_TO_SPACE_WIDTH)
#define TEXT_REPLACE_WIDTHS 0x00100
#define TEXT_DO_NONE 0x00200
#define TEXT_DO_DRAW 0x00400
#define TEXT_DO_CHARWIDTH 0x00800
#define TEXT_DO_FALSE_CHARPATH 0x01000
#define TEXT_DO_TRUE_CHARPATH 0x02000
#define TEXT_DO_FALSE_CHARBOXPATH 0x04000
#define TEXT_DO_TRUE_CHARBOXPATH 0x08000
#define TEXT_DO_ANY_CHARPATH \
(TEXT_DO_CHARWIDTH | TEXT_DO_FALSE_CHARPATH | TEXT_DO_TRUE_CHARPATH |\
TEXT_DO_FALSE_CHARBOXPATH | TEXT_DO_TRUE_CHARBOXPATH)
#define TEXT_DO_ANY \
(TEXT_DO_NONE | TEXT_DO_DRAW | TEXT_DO_ANY_CHARPATH)
#define TEXT_INTERVENE 0x10000
#define TEXT_RETURN_WIDTH 0x20000
typedef struct gs_text_params_s {
uint operation;
union sd_ {
const byte *bytes;
const gs_char *chars;
const gs_glyph *glyphs;
gs_char d_char;
gs_glyph d_glyph;
} data;
uint size;
gs_point delta_all;
gs_point delta_space;
union s_ {
gs_char s_char;
gs_glyph s_glyph;
} space;
const float *x_widths;
const float *y_widths;
uint widths_size;
} gs_text_params_t;
#define st_gs_text_params_max_ptrs 3
#define public_st_gs_text_params() \
gs_public_st_composite(st_gs_text_params, gs_text_params_t,\
"gs_text_params", text_params_enum_ptrs, text_params_reloc_ptrs)
int gs_text_replaced_width(const gs_text_params_t *text, uint index,
gs_point *pwidth);
#ifndef gs_text_enum_DEFINED
# define gs_text_enum_DEFINED
typedef struct gs_text_enum_s gs_text_enum_t;
#endif
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef gs_imager_state_DEFINED
# define gs_imager_state_DEFINED
typedef struct gs_imager_state_s gs_imager_state;
#endif
#ifndef gx_device_color_DEFINED
# define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#ifndef gs_font_DEFINED
# define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef gx_clip_path_DEFINED
# define gx_clip_path_DEFINED
typedef struct gx_clip_path_s gx_clip_path;
#endif
#define dev_t_proc_text_begin(proc, dev_t)\
int proc(dev_t *dev,\
gs_imager_state *pis,\
const gs_text_params_t *text,\
gs_font *font,\
gx_path *path, \
const gx_device_color *pdcolor, \
const gx_clip_path *pcpath, \
gs_memory_t *memory,\
gs_text_enum_t **ppte)
#define dev_proc_text_begin(proc)\
dev_t_proc_text_begin(proc, gx_device)
dev_proc_text_begin(gx_device_text_begin);
#ifndef gs_state_DEFINED
# define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
int gs_text_begin(gs_state * pgs, const gs_text_params_t * text,
gs_memory_t * mem, gs_text_enum_t ** ppenum);
int gs_text_update_dev_color(gs_state * pgs, gs_text_enum_t * pte);
int
gs_show_begin(gs_state *, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_ashow_begin(gs_state *, floatp, floatp, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_widthshow_begin(gs_state *, floatp, floatp, gs_char,
const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_awidthshow_begin(gs_state *, floatp, floatp, gs_char,
floatp, floatp, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_kshow_begin(gs_state *, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_xyshow_begin(gs_state *, const byte *, uint,
const float *, const float *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_glyphshow_begin(gs_state *, gs_glyph,
gs_memory_t *, gs_text_enum_t **),
gs_cshow_begin(gs_state *, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_stringwidth_begin(gs_state *, const byte *, uint,
gs_memory_t *, gs_text_enum_t **),
gs_charpath_begin(gs_state *, const byte *, uint, bool,
gs_memory_t *, gs_text_enum_t **),
gs_glyphpath_begin(gs_state *, gs_glyph, bool,
gs_memory_t *, gs_text_enum_t **),
gs_glyphwidth_begin(gs_state *, gs_glyph,
gs_memory_t *, gs_text_enum_t **),
gs_charboxpath_begin(gs_state *, const byte *, uint, bool,
gs_memory_t *, gs_text_enum_t **);
int gs_text_restart(gs_text_enum_t *pte, const gs_text_params_t *text);
int gs_text_resync(gs_text_enum_t *pte, const gs_text_enum_t *pfrom);
#define TEXT_PROCESS_RENDER 1
#define TEXT_PROCESS_INTERVENE 2
#define TEXT_PROCESS_CDEVPROC 3
int gs_text_process(gs_text_enum_t *pte);
gs_font *gs_text_current_font(const gs_text_enum_t *pte);
gs_char gs_text_current_char(const gs_text_enum_t *pte);
gs_char gs_text_next_char(const gs_text_enum_t *pte);
gs_glyph gs_text_current_glyph(const gs_text_enum_t *pte);
int gs_text_total_width(const gs_text_enum_t *pte, gs_point *pwidth);
bool gs_text_is_width_only(const gs_text_enum_t *pte);
int gs_text_current_width(const gs_text_enum_t *pte, gs_point *pwidth);
typedef enum {
TEXT_SET_CHAR_WIDTH,
TEXT_SET_CACHE_DEVICE,
TEXT_SET_CACHE_DEVICE2
} gs_text_cache_control_t;
int
gs_text_set_cache(gs_text_enum_t *pte, const double *values,
gs_text_cache_control_t control),
gs_text_setcharwidth(gs_text_enum_t *pte, const double wxy[2]),
gs_text_setcachedevice(gs_text_enum_t *pte, const double wbox[6]),
gs_text_setcachedevice2(gs_text_enum_t *pte, const double wbox2[10]);
int gs_text_retry(gs_text_enum_t *pte);
void gs_text_release(gs_text_enum_t *pte, client_name_t cname);
#endif