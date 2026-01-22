#ifndef gdevpdtd_INCLUDED
#  define gdevpdtd_INCLUDED
#include "gdevpdtx.h"
#include "gdevpdtb.h"
#ifndef pdf_font_descriptor_DEFINED
#  define pdf_font_descriptor_DEFINED
typedef struct pdf_font_descriptor_s pdf_font_descriptor_t;
#endif
int pdf_font_descriptor_alloc(gx_device_pdf *pdev,
pdf_font_descriptor_t **ppfd,
gs_font_base *font, bool embed);
long pdf_font_descriptor_id(const pdf_font_descriptor_t *pfd);
font_type pdf_font_descriptor_FontType(const pdf_font_descriptor_t *pfd);
bool pdf_font_descriptor_embedding(const pdf_font_descriptor_t *pfd);
bool pdf_font_descriptor_is_subset(const pdf_font_descriptor_t *pfd);
gs_string *pdf_font_descriptor_name(pdf_font_descriptor_t *pfd);
gs_font_base *pdf_font_descriptor_font(const pdf_font_descriptor_t *pfd, bool complete);
void pdf_font_descriptor_drop_complete_font(const pdf_font_descriptor_t *pfd);
gs_string *pdf_font_descriptor_base_name(const pdf_font_descriptor_t *pfd);
int pdf_font_used_glyph(pdf_font_descriptor_t *pfd, gs_glyph glyph,
gs_font_base *font);
int pdf_compute_font_descriptor(pdf_font_descriptor_t *pfd);
int pdf_finish_FontDescriptor(gx_device_pdf *pdev,
pdf_font_descriptor_t *pfd);
int pdf_finish_font_descriptors(gx_device_pdf *pdev,
int (*finish_proc)(gx_device_pdf *,
pdf_font_descriptor_t *));
int pdf_write_FontDescriptor(gx_device_pdf *pdev,
pdf_font_descriptor_t *pfd);
int pdf_release_FontDescriptor_components(gx_device_pdf *pdev, pdf_font_descriptor_t *pfd);
#endif