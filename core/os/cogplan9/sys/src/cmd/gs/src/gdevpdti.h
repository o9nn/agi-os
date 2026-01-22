#ifndef gdevpdti_INCLUDED
# define gdevpdti_INCLUDED
#include "gdevpdt.h"
#ifndef pdf_bitmap_fonts_DEFINED
# define pdf_bitmap_fonts_DEFINED
typedef struct pdf_bitmap_fonts_s pdf_bitmap_fonts_t;
#endif
void pdf_close_text_page(gx_device_pdf *pdev);
int pdf_char_image_y_offset(const gx_device_pdf *pdev, int x, int y, int h);
int pdf_begin_char_proc(gx_device_pdf * pdev, int w, int h, int x_width,
int y_offset, gs_id id, pdf_char_proc_t **ppcp,
pdf_stream_position_t * ppos);
int pdf_end_char_proc(gx_device_pdf * pdev, pdf_stream_position_t * ppos);
int pdf_do_char_image(gx_device_pdf * pdev, const pdf_char_proc_t * pcp,
const gs_matrix * pimat);
pdf_bitmap_fonts_t *pdf_bitmap_fonts_alloc(gs_memory_t *mem);
int pdf_write_bitmap_fonts_Encoding(gx_device_pdf *pdev);
int pdf_write_contents_bitmap(gx_device_pdf *pdev, pdf_font_resource_t *pdfont);
#endif