#ifndef gxttfb_INCLUDED
#  define gxttfb_INCLUDED
#include "ttfoutl.h"
#ifndef gx_ttfReader_DEFINED
#  define gx_ttfReader_DEFINED
typedef struct gx_ttfReader_s gx_ttfReader;
#endif
#ifndef gs_font_type42_DEFINED
#  define gs_font_type42_DEFINED
typedef struct gs_font_type42_s gs_font_type42;
#endif
struct gx_ttfReader_s {
ttfReader super;
int pos;
bool error;
int extra_glyph_index;
gs_font_type42 *pfont;
gs_memory_t *memory;
gs_glyph_data_t glyph_data;
};
gx_ttfReader *gx_ttfReader__create(gs_memory_t *mem);
void gx_ttfReader__destroy(gx_ttfReader *this);
void gx_ttfReader__set_font(gx_ttfReader *this, gs_font_type42 *pfont);
ttfFont *ttfFont__create(gs_font_dir *dir);
void ttfFont__destroy(ttfFont *this, gs_font_dir *dir);
int ttfFont__Open_aux(ttfFont *this, ttfInterpreter *tti, gx_ttfReader *r, gs_font_type42 *pfont,
const gs_matrix * char_tm, const gs_log2_scale_point *log2_scale,
bool design_grid);
int gx_ttf_outline(ttfFont *ttf, gx_ttfReader *r, gs_font_type42 *pfont, int glyph_index,
const gs_matrix *m, const gs_log2_scale_point * pscale,
gx_path *path, bool grid_fit);
#endif