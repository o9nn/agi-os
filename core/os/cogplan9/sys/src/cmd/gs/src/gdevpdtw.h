#ifndef gdevpdtw_INCLUDED
# define gdevpdtw_INCLUDED
int
pdf_write_contents_type0(gx_device_pdf *pdev, pdf_font_resource_t *pdfont),
pdf_finish_write_contents_type3(gx_device_pdf *pdev,
pdf_font_resource_t *pdfont),
pdf_write_contents_std(gx_device_pdf *pdev, pdf_font_resource_t *pdfont),
pdf_write_contents_simple(gx_device_pdf *pdev, pdf_font_resource_t *pdfont),
pdf_write_contents_cid0(gx_device_pdf *pdev, pdf_font_resource_t *pdfont),
pdf_write_contents_cid2(gx_device_pdf *pdev, pdf_font_resource_t *pdfont),
pdf_different_encoding_index(const pdf_font_resource_t *pdfont, int ch0),
pdf_write_encoding(gx_device_pdf *pdev, const pdf_font_resource_t *pdfont, long id, int ch),
pdf_write_encoding_ref(gx_device_pdf *pdev, const pdf_font_resource_t *pdfont, long id);
#ifndef gs_cid_system_info_DEFINED
# define gs_cid_system_info_DEFINED
typedef struct gs_cid_system_info_s gs_cid_system_info_t;
#endif
#ifndef gs_cmap_DEFINED
# define gs_cmap_DEFINED
typedef struct gs_cmap_s gs_cmap_t;
#endif
int pdf_write_cid_system_info(gx_device_pdf *pdev,
const gs_cid_system_info_t *pcidsi, gs_id object_id);
int pdf_write_cmap(gx_device_pdf *pdev, const gs_cmap_t *pcmap,
pdf_resource_t **ppres, int font_index_only);
#endif