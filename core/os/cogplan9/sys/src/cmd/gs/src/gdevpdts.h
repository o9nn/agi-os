#ifndef gdevpdts_INCLUDED
#  define gdevpdts_INCLUDED
#include "gsmatrix.h"
#ifndef pdf_text_state_DEFINED
#  define pdf_text_state_DEFINED
typedef struct pdf_text_state_s pdf_text_state_t;
#endif
typedef struct pdf_text_state_values_s {
float character_spacing;
pdf_font_resource_t *pdfont;
double size;
gs_matrix matrix;
int render_mode;
float word_spacing;
} pdf_text_state_values_t;
#define TEXT_STATE_VALUES_DEFAULT\
0,				\
NULL,			\
0,				\
{ identity_matrix_body },	\
0,				\
0
int pdf_from_stream_to_text(gx_device_pdf *pdev);
int pdf_from_string_to_text(gx_device_pdf *pdev);
void pdf_close_text_contents(gx_device_pdf *pdev);
bool pdf_render_mode_uses_stroke(const gx_device_pdf *pdev,
const pdf_text_state_values_t *ptsv);
void pdf_get_text_state_values(gx_device_pdf *pdev,
pdf_text_state_values_t *ptsv);
void pdf_set_text_wmode(gx_device_pdf *pdev, int wmode);
int pdf_set_text_state_values(gx_device_pdf *pdev,
const pdf_text_state_values_t *ptsv);
int pdf_text_distance_transform(floatp wx, floatp wy,
const pdf_text_state_t *pts,
gs_point *ppt);
void pdf_text_position(const gx_device_pdf *pdev, gs_point *ppt);
int pdf_append_chars(gx_device_pdf * pdev, const byte * str, uint size,
floatp wx, floatp wy, bool nobreak);
#endif