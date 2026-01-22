#ifndef gdevpsf_INCLUDED
#  define gdevpsf_INCLUDED
#include "gsccode.h"
#include "gsgdata.h"
#ifndef gs_font_DEFINED
#  define gs_font_DEFINED
typedef struct gs_font_s gs_font;
#endif
#ifndef gs_font_base_DEFINED
#  define gs_font_base_DEFINED
typedef struct gs_font_base_s gs_font_base;
#endif
#ifndef stream_DEFINED
#  define stream_DEFINED
typedef struct stream_s stream;
#endif
typedef struct psf_glyph_enum_s psf_glyph_enum_t;
struct psf_glyph_enum_s {
gs_font *font;
struct su_ {
union sus_ {
const gs_glyph *list;
const byte *bits;
} selected;
uint size;
} subset;
gs_glyph_space_t glyph_space;
ulong index;
int (*enumerate_next)(psf_glyph_enum_t *, gs_glyph *);
};
void psf_enumerate_list_begin(psf_glyph_enum_t *ppge, gs_font *font,
const gs_glyph *subset_list,
uint subset_size,
gs_glyph_space_t glyph_space);
#define psf_enumerate_glyphs_begin psf_enumerate_list_begin
void psf_enumerate_bits_begin(psf_glyph_enum_t *ppge, gs_font *font,
const byte *subset_bits, uint subset_size,
gs_glyph_space_t glyph_space);
#define psf_enumerate_cids_begin(ppge, font, bits, size)\
psf_enumerate_bits_begin(ppge, font, bits, size, GLYPH_SPACE_NAME)
void psf_enumerate_glyphs_reset(psf_glyph_enum_t *ppge);
int psf_enumerate_glyphs_next(psf_glyph_enum_t *ppge, gs_glyph *pglyph);
int psf_add_subset_pieces(gs_glyph *glyphs, uint *pcount, uint max_count,
uint max_pieces, gs_font *font);
int psf_sort_glyphs(gs_glyph *glyphs, int count);
int psf_sorted_glyphs_index_of(const gs_glyph *glyphs, int count,
gs_glyph glyph);
bool psf_sorted_glyphs_include(const gs_glyph *glyphs, int count,
gs_glyph glyph);
#define MAX_CFF_MISC_STRINGS 40
#define MAX_CFF_STD_STRINGS 500
typedef struct psf_outline_glyphs_s {
gs_glyph notdef;
gs_glyph *subset_data;
gs_glyph *subset_glyphs;
uint subset_size;
} psf_outline_glyphs_t;
#ifndef gs_font_type1_DEFINED
#  define gs_font_type1_DEFINED
typedef struct gs_font_type1_s gs_font_type1;
#endif
typedef int (*glyph_data_proc_t)(gs_font_base *, gs_glyph,
gs_glyph_data_t *, gs_font_type1 **);
int psf_check_outline_glyphs(gs_font_base *pfont,
psf_glyph_enum_t *ppge,
glyph_data_proc_t glyph_data);
int psf_get_outline_glyphs(psf_outline_glyphs_t *pglyphs,
gs_font_base *pfont, gs_glyph *subset_glyphs,
uint subset_size, glyph_data_proc_t glyph_data);
int psf_type1_glyph_data(gs_font_base *, gs_glyph, gs_glyph_data_t *,
gs_font_type1 **);
int psf_get_type1_glyphs(psf_outline_glyphs_t *pglyphs,
gs_font_type1 *pfont,
gs_glyph *subset_glyphs, uint subset_size);
#define WRITE_TYPE1_EEXEC 1
#define WRITE_TYPE1_ASCIIHEX 2
#define WRITE_TYPE1_EEXEC_PAD 4
#define WRITE_TYPE1_EEXEC_MARK 8
#define WRITE_TYPE1_POSTSCRIPT 16
#define WRITE_TYPE1_WITH_LENIV 32
int psf_write_type1_font(stream *s, gs_font_type1 *pfont, int options,
gs_glyph *subset_glyphs, uint subset_size,
const gs_const_string *alt_font_name,
int lengths[3]);
#define WRITE_TYPE2_NO_LENIV 1
#define WRITE_TYPE2_CHARSTRINGS 2
#define WRITE_TYPE2_AR3 4
#define WRITE_TYPE2_NO_GSUBRS 8
int psf_write_type2_font(stream *s, gs_font_type1 *pfont, int options,
gs_glyph *subset_glyphs, uint subset_size,
const gs_const_string *alt_font_name,
gs_int_rect *FontBBox);
#ifndef gs_font_cid0_DEFINED
#  define gs_font_cid0_DEFINED
typedef struct gs_font_cid0_s gs_font_cid0;
#endif
int psf_write_cid0_font(stream *s, gs_font_cid0 *pfont, int options,
const byte *subset_cids, uint subset_size,
const gs_const_string *alt_font_name);
#ifndef gs_cmap_DEFINED
#  define gs_cmap_DEFINED
typedef struct gs_cmap_s gs_cmap_t;
#endif
typedef int (*psf_put_name_chars_proc_t)(stream *, const byte *, uint);
int psf_write_cmap(const gs_memory_t *mem, stream *s, const gs_cmap_t *pcmap,
psf_put_name_chars_proc_t put_name_chars,
const gs_const_string *alt_cmap_name, int font_index_only);
#ifndef gs_font_type42_DEFINED
#  define gs_font_type42_DEFINED
typedef struct gs_font_type42_s gs_font_type42;
#endif
#define WRITE_TRUETYPE_CMAP 1
#define WRITE_TRUETYPE_NAME 2
#define WRITE_TRUETYPE_POST 4
#define WRITE_TRUETYPE_NO_TRIMMED_TABLE 8
#define WRITE_TRUETYPE_HVMTX 16
int psf_write_truetype_font(stream *s, gs_font_type42 *pfont, int options,
gs_glyph *subset_glyphs, uint subset_size,
const gs_const_string *alt_font_name);
int psf_write_truetype_stripped(stream *s, gs_font_type42 *pfont);
#ifndef gs_font_cid2_DEFINED
#  define gs_font_cid2_DEFINED
typedef struct gs_font_cid2_s gs_font_cid2;
#endif
int psf_write_cid2_font(stream *s, gs_font_cid2 *pfont, int options,
const byte *subset_glyphs, uint subset_size,
const gs_const_string *alt_font_name);
int psf_write_cid2_stripped(stream *s, gs_font_cid2 *pfont);
int psf_convert_type1_to_type2(stream *s, const gs_glyph_data_t *pgd,
gs_font_type1 *pfont);
#endif