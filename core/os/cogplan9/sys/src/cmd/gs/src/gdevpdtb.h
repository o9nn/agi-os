#ifndef gdevpdtb_INCLUDED
# define gdevpdtb_INCLUDED
#include "gdevpdtx.h"
#ifndef pdf_base_font_DEFINED
# define pdf_base_font_DEFINED
typedef struct pdf_base_font_s pdf_base_font_t;
#endif
int pdf_base_font_alloc(gx_device_pdf *pdev, pdf_base_font_t **ppbfont,
gs_font_base *font, const gs_matrix *orig_matrix,
bool is_standard, bool orig_name);
gs_string *pdf_base_font_name(pdf_base_font_t *pbfont);
gs_font_base *pdf_base_font_font(const pdf_base_font_t *pbfont, bool complete);
bool pdf_base_font_is_subset(const pdf_base_font_t *pbfont);
void pdf_base_font_drop_complete(pdf_base_font_t *pbfont);
int pdf_base_font_copy_glyph(pdf_base_font_t *pbfont, gs_glyph glyph,
gs_font_base *font);
bool pdf_has_subset_prefix(const byte *str, uint size);
int pdf_add_subset_prefix(const gx_device_pdf *pdev, gs_string *pstr,
byte *used, int count);
bool pdf_do_subset_font(gx_device_pdf *pdev, pdf_base_font_t *pbfont,
gs_id rid);
int pdf_write_FontFile_entry(gx_device_pdf *pdev, pdf_base_font_t *pbfont);
int pdf_write_embedded_font(gx_device_pdf *pdev, pdf_base_font_t *pbfont,
gs_int_rect *FontBBox, gs_id rid, cos_dict_t **ppcd);
int pdf_write_CharSet(gx_device_pdf *pdev, pdf_base_font_t *pbfont);
int pdf_write_CIDSet(gx_device_pdf *pdev, pdf_base_font_t *pbfont,
long *pcidset_id);
bool pdf_is_standard_font(pdf_base_font_t *bfont);
void pdf_set_FontFile_object(pdf_base_font_t *bfont, cos_dict_t *pcd);
const cos_dict_t * pdf_get_FontFile_object(pdf_base_font_t *bfont);
#endif