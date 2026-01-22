#ifndef gscencs_INCLUDED
#  define gscencs_INCLUDED
#include "stdpre.h"
#include "gstypes.h"
#include "gsccode.h"
extern const gs_glyph gs_c_min_std_encoding_glyph;
gs_glyph gs_c_known_encode(gs_char chr, int encoding_index);
gs_char gs_c_decode(gs_glyph glyph, int ei);
int gs_c_glyph_name(gs_glyph glyph, gs_const_string *pstr);
bool gs_is_c_glyph_name(const byte *str, uint len);
gs_glyph gs_c_name_glyph(const byte *str, uint len);
#endif