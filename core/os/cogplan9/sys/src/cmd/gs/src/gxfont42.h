#ifndef gxfont42_INCLUDED
# define gxfont42_INCLUDED
#ifndef gs_glyph_cache_DEFINED
# define gs_glyph_cache_DEFINED
typedef struct gs_glyph_cache_s gs_glyph_cache;
#endif
#ifndef cached_fm_pair_DEFINED
# define cached_fm_pair_DEFINED
typedef struct cached_fm_pair_s cached_fm_pair;
#endif
#ifndef gs_type42_data_DEFINED
#define gs_type42_data_DEFINED
typedef struct gs_type42_data_s gs_type42_data;
#endif
#ifndef gs_font_type42_DEFINED
# define gs_font_type42_DEFINED
typedef struct gs_font_type42_s gs_font_type42;
#endif
typedef struct gs_type42_mtx_s {
uint numMetrics;
ulong offset;
uint length;
} gs_type42_mtx_t;
struct gs_type42_data_s {
int (*string_proc) (gs_font_type42 *, ulong, uint, const byte **);
void *proc_data;
uint (*get_glyph_index)(gs_font_type42 *pfont, gs_glyph glyph);
int (*get_outline)(gs_font_type42 *pfont, uint glyph_index,
gs_glyph_data_t *pgd);
int (*get_metrics)(gs_font_type42 *pfont, uint glyph_index, int wmode,
float sbw[4]);
ulong cmap;
ulong glyf;
uint unitsPerEm;
uint indexToLocFormat;
gs_type42_mtx_t metrics[2];
ulong loca;
uint numGlyphs;
uint trueNumGlyphs;
uint *len_glyphs;
gs_glyph_cache *gdcache;
bool warning_patented;
bool warning_bad_instruction;
};
#define gs_font_type42_common\
gs_font_base_common;\
gs_type42_data data
struct gs_font_type42_s {
gs_font_type42_common;
};
extern_st(st_gs_font_type42);
#define public_st_gs_font_type42() \
gs_public_st_suffix_add3_final(st_gs_font_type42, gs_font_type42,\
"gs_font_type42", font_type42_enum_ptrs, font_type42_reloc_ptrs,\
gs_font_finalize, st_gs_font_base, data.proc_data, data.len_glyphs, \
data.gdcache)
int gs_type42_font_init(gs_font_type42 *);
int gs_type42_append(uint glyph_index, gs_imager_state * pis,
gx_path * ppath, const gs_log2_scale_point * pscale,
bool charpath_flag, int paint_type, cached_fm_pair *pair);
int gs_type42_get_metrics(gs_font_type42 * pfont, uint glyph_index,
float psbw[4]);
int gs_type42_wmode_metrics(gs_font_type42 * pfont, uint glyph_index,
int wmode, float psbw[4]);
int gs_type42_default_get_metrics(gs_font_type42 *pfont, uint glyph_index,
int wmode, float sbw[4]);
int gs_type42_get_outline_from_TT_file(gs_font_type42 * pfont, stream *s, uint glyph_index,
gs_glyph_data_t *pgd);
font_proc_enumerate_glyph(gs_type42_enumerate_glyph);
font_proc_glyph_info(gs_type42_glyph_info);
font_proc_glyph_outline(gs_type42_glyph_outline);
int gs_type42_glyph_info_by_gid(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,
int members, gs_glyph_info_t *info, uint glyph_index);
#endif