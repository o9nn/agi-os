#ifndef gxfcache_INCLUDED
#  define gxfcache_INCLUDED
#include "gsccode.h"
#include "gsuid.h"
#include "gsxfont.h"
#include "gxbcache.h"
#include "gxfixed.h"
#include "gxftype.h"
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef cached_fm_pair_DEFINED
#  define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
#ifndef gs_matrix_DEFINED
#  define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
#ifndef ttfFont_DEFINED
#  define ttfFont_DEFINED
typedef struct ttfFont_s ttfFont;
#endif
#ifndef gx_ttfReader_DEFINED
#  define gx_ttfReader_DEFINED
typedef struct gx_ttfReader_s gx_ttfReader;
#endif
#ifndef ttfInterpreter_DEFINED
#  define ttfInterpreter_DEFINED
typedef struct ttfInterpreter_s ttfInterpreter;
#endif
#ifndef gx_device_spot_analyzer_DEFINED
#   define gx_device_spot_analyzer_DEFINED
typedef struct gx_device_spot_analyzer_s gx_device_spot_analyzer;
#endif
#ifndef gs_state_DEFINED
#  define gs_state_DEFINED
typedef struct gs_state_s gs_state;
#endif
struct cached_fm_pair_s {
gs_font *font;
gs_uid UID;
font_type FontType;
uint hash;
float mxx, mxy, myx, myy;
int num_chars;
bool xfont_tried;
gx_xfont *xfont;
gs_memory_t *memory;
uint index;
ttfFont *ttf;
gx_ttfReader *ttr;
bool design_grid;
};
#define private_st_cached_fm_pair() \
gs_private_st_ptrs5(st_cached_fm_pair, cached_fm_pair,\
"cached_fm_pair", fm_pair_enum_ptrs, fm_pair_reloc_ptrs,\
font, UID.xvalues, xfont, ttf, ttr)
#define private_st_cached_fm_pair_elt()	\
gs_private_st_element(st_cached_fm_pair_element, cached_fm_pair,\
"cached_fm_pair[]", fm_pair_element_enum_ptrs, fm_pair_element_reloc_ptrs,\
st_cached_fm_pair)
#define fm_pair_is_free(pair)\
((pair)->font == 0 && !uid_is_valid(&(pair)->UID))
#define fm_pair_set_free(pair)\
((pair)->font = 0, uid_set_invalid(&(pair)->UID))
#define fm_pair_init(pair)\
(fm_pair_set_free(pair), (pair)->xfont_tried = false, (pair)->xfont = 0)
typedef struct fm_pair_cache_s {
uint msize, mmax;
cached_fm_pair *mdata;
uint mnext;
} fm_pair_cache;
typedef gx_bits_cache_chunk char_cache_chunk;
typedef gx_cached_bits_head cached_char_head;
#define cc_head_is_free(cch) cb_head_is_free(cch)
#define cc_head_set_free(cch) cb_head_set_free(cch)
#ifndef cached_char_DEFINED
#  define cached_char_DEFINED
typedef struct cached_char_s cached_char;
#endif
struct cached_char_s {
gx_cached_bits_common;
#define cc_depth(cc) ((cc)->cb_depth)
#define cc_set_depth(cc, d) ((cc)->cb_depth = (d))
cached_fm_pair *pair;
bool linked;
#define cc_pair(cc) ((cc)->pair)
#define cc_set_pair_only(cc, p) ((cc)->pair = (p))
gs_glyph code;
byte wmode;
char_cache_chunk *chunk;
uint loc;
uint pair_index;
gs_fixed_point subpix_origin;
#define cc_raster(cc) ((cc)->raster)
#define cc_set_raster(cc, r) ((cc)->raster = (r))
gx_xglyph xglyph;
gs_fixed_point wxy;
gs_fixed_point offset;
};
#define cc_is_free(cc) cc_head_is_free(&(cc)->head)
#define cc_set_free(cc) cc_head_set_free(&(cc)->head)
#define cc_set_pair(cc, p)\
((cc)->pair_index = ((cc)->pair = (p))->index)
#define cc_has_bits(cc) ((cc)->id != gx_no_bitmap_id)
#define private_st_cached_char() \
gs_private_st_composite(st_cached_char, cached_char, "cached_char",\
cached_char_enum_ptrs, cached_char_reloc_ptrs)
#define private_st_cached_char_ptr() \
gs_private_st_composite(st_cached_char_ptr, cached_char *,\
"cached_char *", cc_ptr_enum_ptrs, cc_ptr_reloc_ptrs)
#define private_st_cached_char_ptr_elt() \
gs_private_st_element(st_cached_char_ptr_element, cached_char *,\
"cached_char *[]", cc_ptr_element_enum_ptrs, cc_ptr_element_reloc_ptrs,\
st_cached_char_ptr)
#define align_cached_char_mod align_cached_bits_mod
#define sizeof_cached_char\
ROUND_UP(sizeof(cached_char), align_cached_char_mod)
#define cc_bits(cc) ((byte *)(cc) + sizeof_cached_char)
#define cc_const_bits(cc) ((const byte *)(cc) + sizeof_cached_char)
#define chars_head_index(glyph, pair)\
((uint)(glyph) * 59 + (pair)->hash * 73)
typedef struct char_cache_s {
gx_bits_cache_common;
gs_memory_t *struct_memory;
gs_memory_t *bits_memory;
cached_char **table;
uint table_mask;
uint bmax;
uint cmax;
uint bspace;
uint lower;
uint upper;
gs_glyph_mark_proc_t mark_glyph;
void *mark_glyph_data;
} char_cache;
#ifndef gs_font_dir_DEFINED
#  define gs_font_dir_DEFINED
typedef struct gs_font_dir_s gs_font_dir;
#endif
struct gs_font_dir_s {
gs_font *orig_fonts;
gs_font *scaled_fonts;
uint ssize, smax;
fm_pair_cache fmcache;
char_cache ccache;
uint enum_index;
uint enum_offset;
bool align_to_pixels;
void *glyph_to_unicode_table;
gs_memory_t *memory;
ttfInterpreter *tti;
uint grid_fit_tt;
gx_device_spot_analyzer *san;
int (*global_glyph_code)(const gs_memory_t *mem, gs_const_string *gstr, gs_glyph *pglyph);
};
#define private_st_font_dir()	\
gs_private_st_composite(st_font_dir, gs_font_dir, "gs_font_dir",\
font_dir_enum_ptrs, font_dir_reloc_ptrs)
#define font_dir_do_ptrs(m)\
m(0,scaled_fonts) m(1,fmcache.mdata)\
m(2,ccache.table) m(3,ccache.mark_glyph_data)\
m(4,glyph_to_unicode_table) m(5,tti) m(6,san)
#define st_font_dir_max_ptrs 7
int gx_char_cache_alloc(gs_memory_t * struct_mem, gs_memory_t * bits_mem,
gs_font_dir * pdir, uint bmax, uint mmax,
uint cmax, uint upper);
void gx_char_cache_init(gs_font_dir *);
void gx_purge_selected_cached_chars(gs_font_dir *,
bool(*)(const gs_memory_t *, cached_char *, void *), void *);
void gx_compute_char_matrix(const gs_matrix *char_tm, const gs_log2_scale_point *log2_scale,
float *mxx, float *mxy, float *myx, float *myy);
void gx_compute_ccache_key(gs_font * pfont, const gs_matrix *char_tm,
const gs_log2_scale_point *log2_scale, bool design_grid,
float *mxx, float *mxy, float *myx, float *myy);
int gx_lookup_fm_pair(gs_font * pfont, const gs_matrix *char_tm,
const gs_log2_scale_point *log2_scale, bool design_grid, cached_fm_pair **ppair);
int gx_add_fm_pair(register gs_font_dir * dir, gs_font * font, const gs_uid * puid,
const gs_matrix * char_tm, const gs_log2_scale_point *log2_scale,
bool design_grid, cached_fm_pair **ppair);
void gx_lookup_xfont(const gs_state *, cached_fm_pair *, int);
void gs_purge_fm_pair(gs_font_dir *, cached_fm_pair *, int);
void gs_purge_font_from_char_caches(gs_font_dir *, const gs_font *);
#endif