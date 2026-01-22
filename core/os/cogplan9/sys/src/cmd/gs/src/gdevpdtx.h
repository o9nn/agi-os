#ifndef gdevpdtx_INCLUDED
# define gdevpdtx_INCLUDED
#include "gdevpdt.h"
#ifndef pdf_bitmap_fonts_DEFINED
# define pdf_bitmap_fonts_DEFINED
typedef struct pdf_bitmap_fonts_s pdf_bitmap_fonts_t;
#endif
#ifndef pdf_outline_fonts_DEFINED
# define pdf_outline_fonts_DEFINED
typedef struct pdf_outline_fonts_s pdf_outline_fonts_t;
#endif
#ifndef pdf_text_state_DEFINED
# define pdf_text_state_DEFINED
typedef struct pdf_text_state_s pdf_text_state_t;
#endif
struct pdf_text_data_s {
pdf_outline_fonts_t *outline_fonts;
pdf_bitmap_fonts_t *bitmap_fonts;
pdf_text_state_t *text_state;
};
#define private_st_pdf_text_data() \
gs_private_st_ptrs3(st_pdf_text_data, pdf_text_data_t, "pdf_text_data_t",\
pdf_text_data_enum_ptrs, pdf_text_data_reloc_ptrs,\
outline_fonts, bitmap_fonts, text_state)
typedef struct pdf_font_resource_s pdf_font_resource_t;
long pdf_font_id(const pdf_font_resource_t *pdfont);
int pdf_used_charproc_resources(gx_device_pdf *pdev, pdf_font_resource_t *pdfont);
#endif