#ifndef iimage_INCLUDED
#  define iimage_INCLUDED
typedef struct image_params_s {
bool MultipleDataSources;
ref DataSource[gs_image_max_components];
const float *pDecode;
} image_params;
int data_image_params(const gs_memory_t *mem,
const ref *op, gs_data_image_t *pim,
image_params *pip, bool require_DataSource,
int num_components, int max_bits_per_component,
bool has_alpha);
int pixel_image_params(i_ctx_t *i_ctx_p, const ref *op,
gs_pixel_image_t *pim, image_params * pip,
int max_bits_per_component, bool has_alpha);
int zimage_setup(i_ctx_t *i_ctx_p, const gs_pixel_image_t * pim,
const ref * sources, bool uses_color, int npop);
int image1_setup(i_ctx_t * i_ctx_p, bool has_alpha);
#endif