#ifndef gxfcopy_INCLUDED
#  define gxfcopy_INCLUDED
#include "gsccode.h"
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef gs_matrix_DEFINED
#  define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
int gs_copy_font(gs_font *font, const gs_matrix *orig_matrix,
gs_memory_t *mem, gs_font **pfont_new);
int gs_copy_glyph(gs_font *font, gs_glyph glyph, gs_font *copied);
#define COPY_GLYPH_NO_OLD 1
#define COPY_GLYPH_NO_NEW 2
#define COPY_GLYPH_BY_INDEX 4
int gs_copy_glyph_options(gs_font *font, gs_glyph glyph, gs_font *copied,
int options);
int gs_copied_font_add_encoding(gs_font *copied, gs_char chr, gs_glyph glyph);
int gs_copy_font_complete(gs_font *font, gs_font *copied);
int gs_copied_can_copy_glyphs(const gs_font *cfont, const gs_font *ofont,
gs_glyph *glyphs, int num_glyphs, int glyphs_step,
bool check_hinting);
int copied_drop_extension_glyphs(gs_font *cfont);
#endif