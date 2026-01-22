#ifndef ifont42_INCLUDED
#  define ifont42_INCLUDED
int build_gs_TrueType_font(i_ctx_t *, os_ptr, gs_font_type42 **, font_type,
gs_memory_type_ptr_t, const char *, const char *,
build_font_options_t);
int font_string_array_param(const gs_memory_t *mem, os_ptr, const char *, ref *);
int font_GlyphDirectory_param(os_ptr, ref *);
int font_gdir_get_outline(const gs_memory_t *mem, const ref *, long, gs_glyph_data_t *);
int string_array_access_proc(const gs_memory_t *mem, const ref *, int, ulong, uint, const byte **);
#endif