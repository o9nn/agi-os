#ifndef gdevpdtt_INCLUDED
# define gdevpdtt_INCLUDED
#ifndef pdf_char_glyph_pair_DEFINED
# define pdf_char_glyph_pair_DEFINED
typedef struct pdf_char_glyph_pair_s pdf_char_glyph_pair_t;
#endif
#ifndef pdf_char_glyph_pairs_DEFINED
# define pdf_char_glyph_pairs_DEFINED
typedef struct pdf_char_glyph_pairs_s pdf_char_glyph_pairs_t;
#endif
struct pdf_char_glyph_pairs_s {
int num_all_chars;
int num_unused_chars;
int unused_offset;
pdf_char_glyph_pair_t s[1];
};
typedef struct pdf_text_enum_s {
gs_text_enum_common;
gs_text_enum_t *pte_default;
gs_fixed_point origin;
bool charproc_accum;
bool cdevproc_callout;
double cdevproc_result[10];
pdf_char_glyph_pairs_t *cgp;
} pdf_text_enum_t;
#define private_st_pdf_text_enum()\
extern_st(st_gs_text_enum);\
gs_private_st_suffix_add2(st_pdf_text_enum, pdf_text_enum_t,\
"pdf_text_enum_t", pdf_text_enum_enum_ptrs, pdf_text_enum_reloc_ptrs,\
st_gs_text_enum, pte_default, cgp)
typedef struct pdf_text_process_state_s {
pdf_text_state_values_t values;
gs_font *font;
} pdf_text_process_state_t;
typedef struct pdf_glyph_width_s {
double w;
gs_point xy;
gs_point v;
} pdf_glyph_width_t;
typedef struct pdf_glyph_widths_s {
pdf_glyph_width_t Width;
pdf_glyph_width_t real_width;
bool replaced_v;
} pdf_glyph_widths_t;
#define PROCESS_TEXT_PROC(proc)\
int proc(gs_text_enum_t *pte, void *vbuf, uint bsize)
int pdf_font_orig_matrix(const gs_font *font, gs_matrix *pmat);
int font_orig_scale(const gs_font *font, double *sx);
bool pdf_check_encoding_compatibility(const pdf_font_resource_t *pdfont,
const pdf_char_glyph_pair_t *pairs, int num_chars);
int
pdf_obtain_font_resource(pdf_text_enum_t *penum,
const gs_string *pstr, pdf_font_resource_t **ppdfont);
int pdf_obtain_font_resource_unencoded(pdf_text_enum_t *penum,
const gs_string *pstr, pdf_font_resource_t **ppdfont, const gs_glyph *gdata);
int pdf_obtain_cidfont_resource(gx_device_pdf *pdev, gs_font *subfont,
pdf_font_resource_t **ppdsubf,
pdf_char_glyph_pairs_t *cgp);
int pdf_obtain_parent_type0_font_resource(gx_device_pdf *pdev, pdf_font_resource_t *pdsubf,
const gs_const_string *CMapName, pdf_font_resource_t **pdfont);
int pdf_attached_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **pdfont, byte **glyph_usage,
double **real_widths, int *num_chars, int *num_widths);
int pdf_attach_font_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t *pdfont);
int pdf_make_font3_resource(gx_device_pdf *pdev, gs_font *font,
pdf_font_resource_t **ppdfont);
int pdf_update_text_state(pdf_text_process_state_t *ppts,
const pdf_text_enum_t *penum,
pdf_font_resource_t *pdfont,
const gs_matrix *pfmat);
int pdf_set_text_process_state(gx_device_pdf *pdev,
const gs_text_enum_t *pte,
pdf_text_process_state_t *ppts);
int pdf_glyph_widths(pdf_font_resource_t *pdfont, int wmode, gs_glyph glyph,
gs_font *font, pdf_glyph_widths_t *pwidths,
const double cdevproc_result[10]);
int pdf_default_text_begin(gs_text_enum_t *pte, const gs_text_params_t *text,
gs_text_enum_t **ppte);
bool pdf_is_simple_font(gs_font *font);
bool pdf_is_CID_font(gs_font *font);
void pdf_font3_scale(gx_device_pdf *pdev, gs_font *font, double *scale);
void pdf_text_release_cgp(pdf_text_enum_t *penum);
PROCESS_TEXT_PROC(process_composite_text);
PROCESS_TEXT_PROC(process_cmap_text);
PROCESS_TEXT_PROC(process_cid_text);
PROCESS_TEXT_PROC(process_plain_text);
int pdf_encode_process_string(pdf_text_enum_t *penum, gs_string *pstr,
const gs_glyph *gdata, const gs_matrix *pfmat,
pdf_text_process_state_t *ppts);
int process_text_modify_width(pdf_text_enum_t *pte, gs_font *font,
pdf_text_process_state_t *ppts,
const gs_const_string *pstr,
gs_point *pdpt);
int
pdf_add_ToUnicode(gx_device_pdf *pdev, gs_font *font, pdf_font_resource_t *pdfont,
gs_glyph glyph, gs_char ch);
int pdf_encode_glyph(gs_font_base *bfont, gs_glyph glyph0,
byte *buf, int buf_size, int *char_code_length);
int pdf_shift_text_currentpoint(pdf_text_enum_t *penum, gs_point *wpt);
#endif