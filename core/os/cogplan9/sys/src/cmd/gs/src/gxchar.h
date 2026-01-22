#ifndef gxchar_INCLUDED
#  define gxchar_INCLUDED
#include "gschar.h"
#include "gxtext.h"
#ifndef cached_char_DEFINED
#  define cached_char_DEFINED
typedef struct cached_char_s cached_char;
#endif
#ifndef cached_fm_pair_DEFINED
#  define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef gs_text_enum_DEFINED
#  define gs_text_enum_DEFINED
typedef struct gs_text_enum_s gs_text_enum_t;
#endif
#ifndef gx_device_memory_DEFINED
#  define gx_device_memory_DEFINED
typedef struct gx_device_memory_s gx_device_memory;
#endif
#ifndef gx_device_null_DEFINED
#  define gx_device_null_DEFINED
typedef struct gx_device_null_s gx_device_null;
#endif
typedef enum {
sws_none,
sws_cache,
sws_no_cache,
sws_cache_width_only,
sws_retry
} show_width_status;
struct gs_show_enum_s {
gs_text_enum_common;
bool auto_release;
gs_state *pgs;
int level;
gs_char_path_mode charpath_flag;
gs_state *show_gstate;
int can_cache;
gs_int_rect ibox;
gs_int_rect obox;
int ftx, fty;
gs_glyph (*encode_char)(gs_font *, gs_char, gs_glyph_space_t);
gs_log2_scale_point fapi_log2_scale;
gs_point fapi_glyph_shift;
gx_device_memory *dev_cache;
gx_device_memory *dev_cache2;
gx_device_null *dev_null;
gs_fixed_point wxy;
gs_fixed_point origin;
cached_char *cc;
show_width_status width_status;
int (*continue_proc) (gs_show_enum *);
};
#define gs_show_enum_s_DEFINED
#define public_st_gs_show_enum() \
gs_public_st_composite(st_gs_show_enum, gs_show_enum, "gs_show_enum",\
show_enum_enum_ptrs, show_enum_reloc_ptrs)
int gx_current_char(const gs_text_enum_t * pte);
#ifndef gs_font_dir_DEFINED
#  define gs_font_dir_DEFINED
typedef struct gs_font_dir_s gs_font_dir;
#endif
cached_char *
gx_alloc_char_bits(gs_font_dir *, gx_device_memory *, gx_device_memory *, ushort, ushort, const gs_log2_scale_point *, int);
void gx_open_cache_device(gx_device_memory *, cached_char *);
void gx_free_cached_char(gs_font_dir *, cached_char *);
void gx_add_cached_char(gs_font_dir *, gx_device_memory *, cached_char *, cached_fm_pair *, const gs_log2_scale_point *);
void gx_add_char_bits(gs_font_dir *, cached_char *, const gs_log2_scale_point *);
cached_char *
gx_lookup_cached_char(const gs_font *, const cached_fm_pair *, gs_glyph, int, int, gs_fixed_point *);
cached_char *
gx_lookup_xfont_char(const gs_state *, cached_fm_pair *, gs_char, gs_glyph, int);
int gx_image_cached_char(gs_show_enum *, cached_char *);
void gx_compute_text_oversampling(const gs_show_enum * penum, const gs_font *pfont,
int alpha_bits, gs_log2_scale_point *p_log2_scale);
int set_char_width(gs_show_enum *penum, gs_state *pgs, floatp wx, floatp wy);
int gx_default_text_restore_state(gs_text_enum_t *pte);
int gx_hld_stringwidth_begin(gs_imager_state * pis, gx_path **path);
#endif