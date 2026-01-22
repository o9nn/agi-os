#ifndef gdevpdt_INCLUDED
# define gdevpdt_INCLUDED
pdf_text_state_t *pdf_text_state_alloc(gs_memory_t *mem);
pdf_text_data_t *pdf_text_data_alloc(gs_memory_t *mem);
void pdf_reset_text_page(pdf_text_data_t *ptd);
void pdf_reset_text_state(pdf_text_data_t *ptd);
void pdf_close_text_page(gx_device_pdf *pdev);
int pdf_close_text_document(gx_device_pdf *pdev);
int pdf_from_stream_to_text(gx_device_pdf *pdev);
int pdf_from_string_to_text(gx_device_pdf *pdev);
void pdf_close_text_contents(gx_device_pdf *pdev);
int pdf_char_image_y_offset(const gx_device_pdf *pdev, int x, int y, int h);
int pdf_begin_char_proc(gx_device_pdf * pdev, int w, int h, int x_width,
int y_offset, gs_id id, pdf_char_proc_t **ppcp,
pdf_stream_position_t * ppos);
int pdf_end_char_proc(gx_device_pdf * pdev,
pdf_stream_position_t * ppos);
int pdf_do_char_image(gx_device_pdf * pdev, const pdf_char_proc_t * pcp,
const gs_matrix * pimat);
#endif