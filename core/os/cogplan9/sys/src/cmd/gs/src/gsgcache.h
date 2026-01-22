#ifndef gxgcache_INCLUDED
#  define gxgcache_INCLUDED
#ifndef gs_font_type42_DEFINED
#  define gs_font_type42_DEFINED
typedef struct gs_font_type42_s gs_font_type42;
#endif
#ifndef gs_glyph_data_DEFINED
#   define gs_glyph_data_DEFINED
typedef struct gs_glyph_data_s gs_glyph_data_t;
#endif
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif
#ifndef gs_glyph_cache_DEFINED
#  define gs_glyph_cache_DEFINED
typedef struct gs_glyph_cache_s gs_glyph_cache;
#endif
typedef int (*get_glyph_data_from_file)(gs_font_type42 *pfont, stream *s, uint glyph_index,
gs_glyph_data_t *pgd);
gs_glyph_cache *gs_glyph_cache__alloc(gs_font_type42 *pfont, stream *s,
get_glyph_data_from_file read_data);
int gs_glyph_cache__release(void  *data, void*);
int gs_get_glyph_data_cached(gs_font_type42 *pfont, uint glyph_index, gs_glyph_data_t *pgd);
#endif