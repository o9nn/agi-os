#ifndef gscrd_INCLUDED
#  define gscrd_INCLUDED
#include "gscie.h"
int
gs_cie_render1_build(gs_cie_render ** ppcrd, gs_memory_t * mem,
client_name_t cname);
int
gs_cie_render1_init_from(const gs_memory_t  *mem,
gs_cie_render *pcrd,
void *client_data,
const gs_cie_render * pfrom_crd,
const gs_vector3 * WhitePoint,
const gs_vector3 * BlackPoint,
const gs_matrix3 * MatrixPQR,
const gs_range3 * RangePQR,
const gs_cie_transform_proc3 * TransformPQR,
const gs_matrix3 * MatrixLMN,
const gs_cie_render_proc3 * EncodeLMN,
const gs_range3 * RangeLMN,
const gs_matrix3 * MatrixABC,
const gs_cie_render_proc3 * EncodeABC,
const gs_range3 * RangeABC,
const gs_cie_render_table_t * RenderTable);
int
gs_cie_render1_initialize(const gs_memory_t *mem,
gs_cie_render *pcrd,
void *client_data,
const gs_vector3 * WhitePoint,
const gs_vector3 * BlackPoint,
const gs_matrix3 * MatrixPQR,
const gs_range3 * RangePQR,
const gs_cie_transform_proc3 * TransformPQR,
const gs_matrix3 * MatrixLMN,
const gs_cie_render_proc3 * EncodeLMN,
const gs_range3 * RangeLMN,
const gs_matrix3 * MatrixABC,
const gs_cie_render_proc3 * EncodeABC,
const gs_range3 * RangeABC,
const gs_cie_render_table_t * RenderTable);
#define gs_cie_render_client_data(pcrd) ((pcrd)->client_data)
#endif