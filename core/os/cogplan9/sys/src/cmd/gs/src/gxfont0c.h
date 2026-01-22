#ifndef gxfont0c_INCLUDED
#  define gxfont0c_INCLUDED
#include "gxfont0.h"
#include "gxfcid.h"
int gs_font_type0_from_cidfont(gs_font_type0 **ppfont0, gs_font *font,
int wmode, const gs_matrix *psmat,
gs_memory_t *mem);
int gs_font_type0_from_type42(gs_font_type0 **ppfont0, gs_font_type42 *pfont42,
int wmode, bool use_cmap, gs_memory_t *mem);
int gs_font_cid2_from_type42(gs_font_cid2 **ppfcid, gs_font_type42 *pfont42,
int wmode, gs_memory_t *mem);
int gs_cmap_from_type42_cmap(gs_cmap_t **ppcmap, gs_font_type42 *pfont42,
int wmode, gs_memory_t *mem);
#endif