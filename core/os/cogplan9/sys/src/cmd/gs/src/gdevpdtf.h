#ifndef gdevpdtf_INCLUDED
# define gdevpdtf_INCLUDED
#include "gdevpdtx.h"
#ifndef gs_cmap_DEFINED
# define gs_cmap_DEFINED
typedef struct gs_cmap_s gs_cmap_t;
#endif
#ifndef gs_font_type0_DEFINED
# define gs_font_type0_DEFINED
typedef struct gs_font_type0_s gs_font_type0;
#endif
#ifndef pdf_base_font_DEFINED
# define pdf_base_font_DEFINED
typedef struct pdf_base_font_s pdf_base_font_t;
#endif
#ifndef pdf_font_descriptor_DEFINED
# define pdf_font_descriptor_DEFINED
typedef struct pdf_font_descriptor_s pdf_font_descriptor_t;
#endif
#ifndef pdf_char_glyph_pair_DEFINED
# define pdf_char_glyph_pair_DEFINED
typedef struct pdf_char_glyph_pair_s pdf_char_glyph_pair_t;
#endif
struct pdf_char_glyph_pair_s {
gs_char chr;
gs_glyph glyph;
};
typedef int (*pdf_font_write_contents_proc_t)
(gx_device_pdf *, pdf_font_resource_t *);
typedef struct pdf_encoding_element_s {
gs_glyph glyph;
gs_const_string str;
bool is_difference;
} pdf_encoding_element_t;
#define private_st_pdf_encoding1() \
gs_private_st_const_strings1(st_pdf_encoding1,\
pdf_encoding_element_t, "pdf_encoding_element_t",\
pdf_encoding1_enum_ptrs, pdf_encoding1_reloc_ptrs, str)
#define private_st_pdf_encoding_element() \
gs_private_st_element(st_pdf_encoding_element, pdf_encoding_element_t,\
"pdf_encoding_element_t[]", pdf_encoding_elt_enum_ptrs,\
pdf_encoding_elt_reloc_ptrs, st_pdf_encoding1)
typedef struct {
gs_id id;
pdf_resource_type_t type;
} pdf_resource_ref_t;
struct pdf_font_resource_s {
pdf_resource_common(pdf_font_resource_t);
font_type FontType;
pdf_font_write_contents_proc_t write_contents;
gs_string BaseFont;
pdf_font_descriptor_t *FontDescriptor;
pdf_base_font_t *base_font;
uint count;
double *Widths;
byte *used;
pdf_resource_t *res_ToUnicode;
gs_cmap_t *cmap_ToUnicode;
union {
struct {
pdf_font_resource_t *DescendantFont;
char Encoding_name[max(
17,
sizeof(long) * 8 / 3 + 1 + 4
) + 1
];
gs_const_string CMapName;
bool cmap_is_standard;
int WMode;
} type0;
struct {
long CIDSystemInfo_id;
ushort *CIDToGIDMap;
gs_id glyphshow_font_id;
double *Widths2;
double *v;
byte *used2;
pdf_font_resource_t *parent;
} cidfont;
struct {
int FirstChar, LastChar;
gs_encoding_index_t BaseEncoding;
pdf_encoding_element_t *Encoding;
gs_point *v;
union {
struct {
bool is_MM_instance;
} type1;
struct {
int _dummy;
} truetype;
struct {
gs_int_rect FontBBox;
gs_matrix FontMatrix;
pdf_char_proc_t *char_procs;
int max_y_offset;
bool bitmap_font;
pdf_resource_ref_t *used_resources;
int used_resources_count;
int used_resources_max;
byte *cached;
} type3;
} s;
} simple;
} u;
};
#define public_st_pdf_font_resource() \
gs_public_st_composite(st_pdf_font_resource, pdf_font_resource_t,\
"pdf_font_resource_t", pdf_font_resource_enum_ptrs,\
pdf_font_resource_reloc_ptrs)
typedef enum {
FONT_EMBED_STANDARD,
FONT_EMBED_NO,
FONT_EMBED_YES
} pdf_font_embed_t;
typedef struct pdf_standard_font_s {
pdf_font_resource_t *pdfont;
gs_matrix orig_matrix;
} pdf_standard_font_t;
#define private_st_pdf_standard_font() \
gs_private_st_ptrs1(st_pdf_standard_font, pdf_standard_font_t,\
"pdf_standard_font_t", pdf_std_font_enum_ptrs, pdf_std_font_reloc_ptrs,\
pdfont)
#define private_st_pdf_standard_font_element() \
gs_private_st_element(st_pdf_standard_font_element, pdf_standard_font_t,\
"pdf_standard_font_t[]", pdf_std_font_elt_enum_ptrs,\
pdf_std_font_elt_reloc_ptrs, st_pdf_standard_font)
struct pdf_outline_fonts_s {
pdf_standard_font_t *standard_fonts;
};
#define private_st_pdf_outline_fonts() \
gs_private_st_ptrs1(st_pdf_outline_fonts, pdf_outline_fonts_t,\
"pdf_outline_fonts_t", pdf_outline_fonts_enum_ptrs,\
pdf_outline_fonts_reloc_ptrs, standard_fonts)
pdf_outline_fonts_t *pdf_outline_fonts_alloc(gs_memory_t *mem);
pdf_standard_font_t *pdf_standard_fonts(const gx_device_pdf *pdev);
void pdf_clean_standard_fonts(const gx_device_pdf *pdev);
int pdf_free_font_cache(gx_device_pdf *pdev);
int pdf_font_type0_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
gs_id rid, pdf_font_resource_t *DescendantFont,
const gs_const_string *CMapName);
int pdf_font_type3_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
pdf_font_write_contents_proc_t write_contents);
int pdf_font_std_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
bool is_original, gs_id rid, gs_font_base *pfont, int index);
int pdf_font_simple_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
gs_id rid, pdf_font_descriptor_t *pfd);
int pdf_font_cidfont_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
gs_id rid, pdf_font_descriptor_t *pfd);
int pdf_obtain_cidfont_widths_arrays(gx_device_pdf *pdev, pdf_font_resource_t *pdfont,
int wmode, double **w, double **w0, double **v);
int font_resource_encoded_alloc(gx_device_pdf *pdev, pdf_font_resource_t **ppfres,
gs_id rid, font_type ftype,
pdf_font_write_contents_proc_t write_contents);
int pdf_resize_resource_arrays(gx_device_pdf *pdev, pdf_font_resource_t *pfres,
int chars_count);
gs_font_base *pdf_font_resource_font(const pdf_font_resource_t *pdfont, bool complete);
pdf_font_embed_t pdf_font_embed_status(gx_device_pdf *pdev, gs_font *font,
int *pindex,
pdf_char_glyph_pair_t *pairs, int num_glyphs);
int pdf_compute_BaseFont(gx_device_pdf *pdev, pdf_font_resource_t *pdfont, bool finish);
int pdf_close_text_document(gx_device_pdf *pdev);
const gs_font_name *pdf_choose_font_name(gs_font *font, bool key_name);
int pdf_cmap_alloc(gx_device_pdf *pdev, const gs_cmap_t *pcmap,
pdf_resource_t **ppres , int font_index_only);
int pdf_font_add_cid_to_gid(pdf_font_resource_t *pdfont, uint cid, uint gid);
#endif