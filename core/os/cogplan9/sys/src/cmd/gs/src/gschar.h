#ifndef gschar_INCLUDED
# define gschar_INCLUDED
#include "gsccode.h"
#include "gscpm.h"
#ifndef gs_show_enum_DEFINED
# define gs_show_enum_DEFINED
typedef struct gs_show_enum_s gs_show_enum;
#endif
#ifndef gs_font_DEFINED
# define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
gs_show_enum *gs_show_enum_alloc(gs_memory_t *, gs_state *, client_name_t);
void gs_show_enum_release(gs_show_enum *, gs_memory_t *);
int
gs_show_n_init(gs_show_enum *, gs_state *, const char *, uint),
gs_ashow_n_init(gs_show_enum *, gs_state *, floatp, floatp, const char *, uint),
gs_widthshow_n_init(gs_show_enum *, gs_state *, floatp, floatp, gs_char, const char *, uint),
gs_awidthshow_n_init(gs_show_enum *, gs_state *, floatp, floatp, gs_char, floatp, floatp, const char *, uint),
gs_kshow_n_init(gs_show_enum *, gs_state *, const char *, uint),
gs_xyshow_n_init(gs_show_enum *, gs_state *, const char *, uint),
gs_glyphshow_init(gs_show_enum *, gs_state *, gs_glyph), gs_cshow_n_init(gs_show_enum *, gs_state *, const char *, uint),
gs_stringwidth_n_init(gs_show_enum *, gs_state *, const char *, uint),
gs_charpath_n_init(gs_show_enum *, gs_state *, const char *, uint, bool),
gs_glyphpath_init(gs_show_enum *, gs_state *, gs_glyph, bool),
gs_glyphwidth_init(gs_show_enum *, gs_state *, gs_glyph),
gs_charboxpath_n_init(gs_show_enum *, gs_state *, const char *, uint, bool);
int gs_show_use_glyph(gs_show_enum *, gs_glyph);
#define gs_show_render TEXT_PROCESS_RENDER
#define gs_show_kern TEXT_PROCESS_INTERVENE
#define gs_show_move TEXT_PROCESS_INTERVENE
int gs_show_next(gs_show_enum *);
gs_char
gs_show_current_char(const gs_show_enum *),
gs_kshow_previous_char(const gs_show_enum *),
gs_kshow_next_char(const gs_show_enum *);
gs_font *
gs_show_current_font(const gs_show_enum *);
gs_glyph
gs_show_current_glyph(const gs_show_enum *);
int gs_show_current_width(const gs_show_enum *, gs_point *);
void gs_show_width(const gs_show_enum *, gs_point *);
gs_char_path_mode
gs_show_in_charpath(const gs_show_enum *);
int gs_setcachedevice_float(gs_show_enum *, gs_state *, const float * );
int gs_setcachedevice_double(gs_show_enum *, gs_state *, const double * );
#define gs_setcachedevice(penum, pgs, pw)\
gs_setcachedevice_float(penum, pgs, pw)
int gs_setcachedevice2_float(gs_show_enum *, gs_state *, const float * );
int gs_setcachedevice2_double(gs_show_enum *, gs_state *, const double * );
#define gs_setcachedevice2(penum, pgs, pw2)\
gs_setcachedevice2_float(penum, pgs, pw2)
int gs_setcharwidth(gs_show_enum *, gs_state *, floatp, floatp);
bool gs_show_width_only(const gs_show_enum *);
#endif