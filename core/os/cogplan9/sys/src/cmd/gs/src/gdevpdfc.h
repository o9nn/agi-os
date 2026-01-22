#ifndef gdevpdfc_INCLUDED
#  define gdevpdfc_INCLUDED
typedef enum {
ONE_STEP_NOT,
ONE_STEP_LMN,
ONE_STEP_ABC
} cie_cache_one_step_t;
int pdf_finish_cie_space(cos_array_t *pca, cos_dict_t *pcd,
const gs_cie_common *pciec);
int pdf_iccbased_color_space(gx_device_pdf *pdev, cos_value_t *pvalue,
const gs_color_space *pcs, cos_array_t *pca);
int pdf_convert_cie_space(gx_device_pdf *pdev, cos_array_t *pca,
const gs_color_space *pcs, const char *dcsname,
const gs_cie_common *pciec, const gs_range *prange,
cie_cache_one_step_t one_step,
const gs_matrix3 *pmat, const gs_range_t **pprange);
int pdf_put_lab_color_space(cos_array_t *pca, cos_dict_t *pcd,
const gs_range ranges[3] );
#endif