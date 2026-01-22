#ifndef ifont_INCLUDED
#  define ifont_INCLUDED
#include "gsccode.h"
#include "gsstype.h"
typedef struct font_data_s {
ref dict;
ref BuildChar;
ref BuildGlyph;
ref Encoding;
ref CharStrings;
ref GlyphNames2Unicode;
union _fs {
struct _f1 {
ref OtherSubrs;
ref Subrs;
ref GlobalSubrs;
} type1;
struct _f42 {
ref sfnts;
ref CIDMap;
ref GlyphDirectory;
} type42;
struct _fc0 {
ref GlyphDirectory;
ref GlyphData;
ref DataSource;
} cid0;
} u;
} font_data;
extern_st(st_font_data);
#define public_st_font_data()	\
gs_public_st_ref_struct(st_font_data, font_data, "font_data")
#define pfont_data(pfont) ((font_data *)((pfont)->client_data))
#define pfont_dict(pfont) (&pfont_data(pfont)->dict)
int font_bbox_param(const gs_memory_t *mem, const ref *pfdict, double bbox[4]);
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
int font_param(const ref * pfdict, gs_font ** ppfont);
bool zfont_mark_glyph_name(const gs_memory_t *mem, gs_glyph glyph, void *ignore_data);
font_proc_font_info(zfont_info);
#endif