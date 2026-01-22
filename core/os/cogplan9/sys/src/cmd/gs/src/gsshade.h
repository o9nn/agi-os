#ifndef gsshade_INCLUDED
# define gsshade_INCLUDED
#include "gsccolor.h"
#include "gscspace.h"
#include "gsdsrc.h"
#include "gsfunc.h"
#include "gsmatrix.h"
#include "gxfixed.h"
typedef enum {
shading_type_Function_based = 1,
shading_type_Axial = 2,
shading_type_Radial = 3,
shading_type_Free_form_Gouraud_triangle = 4,
shading_type_Lattice_form_Gouraud_triangle = 5,
shading_type_Coons_patch = 6,
shading_type_Tensor_product_patch = 7
} gs_shading_type_t;
#define gs_shading_params_common\
gs_color_space *ColorSpace;\
gs_client_color *Background;\
bool have_BBox;\
gs_rect BBox;\
bool AntiAlias
typedef struct gs_shading_params_s {
gs_shading_params_common;
} gs_shading_params_t;
#ifndef gs_shading_t_DEFINED
# define gs_shading_t_DEFINED
typedef struct gs_shading_s gs_shading_t;
#endif
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#define SHADING_FILL_RECTANGLE_PROC(proc)\
int proc(const gs_shading_t *psh, const gs_rect *prect,\
const gs_fixed_rect *prect_clip, gx_device *dev,\
gs_imager_state *pis)
typedef SHADING_FILL_RECTANGLE_PROC((*shading_fill_rectangle_proc_t));
#define gs_shading_fill_rectangle(psh, prect, prect_clip, dev, pis)\
((psh)->head.procs.fill_rectangle(psh, prect, prect_clip, dev, pis))
typedef struct gs_shading_procs_s {
shading_fill_rectangle_proc_t fill_rectangle;
} gs_shading_procs_t;
typedef struct gs_shading_head_s {
gs_shading_type_t type;
gs_shading_procs_t procs;
} gs_shading_head_t;
struct gs_shading_s {
gs_shading_head_t head;
gs_shading_params_t params;
};
#define ShadingType(psh) ((psh)->head.type)
#define private_st_shading() \
gs_private_st_ptrs2(st_shading, gs_shading_t, "gs_shading_t",\
shading_enum_ptrs, shading_reloc_ptrs,\
params.ColorSpace, params.Background)
typedef struct gs_shading_Fb_params_s {
gs_shading_params_common;
float Domain[4];
gs_matrix Matrix;
gs_function_t *Function;
} gs_shading_Fb_params_t;
#define private_st_shading_Fb() \
gs_private_st_suffix_add1(st_shading_Fb, gs_shading_Fb_t,\
"gs_shading_Fb_t", shading_Fb_enum_ptrs, shading_Fb_reloc_ptrs,\
st_shading, params.Function)
typedef struct gs_shading_A_params_s {
gs_shading_params_common;
float Coords[4];
float Domain[2];
gs_function_t *Function;
bool Extend[2];
} gs_shading_A_params_t;
#define private_st_shading_A() \
gs_private_st_suffix_add1(st_shading_A, gs_shading_A_t,\
"gs_shading_A_t", shading_A_enum_ptrs, shading_A_reloc_ptrs,\
st_shading, params.Function)
typedef struct gs_shading_R_params_s {
gs_shading_params_common;
float Coords[6];
float Domain[2];
gs_function_t *Function;
bool Extend[2];
} gs_shading_R_params_t;
#define private_st_shading_R() \
gs_private_st_suffix_add1(st_shading_R, gs_shading_R_t,\
"gs_shading_R_t", shading_R_enum_ptrs, shading_R_reloc_ptrs,\
st_shading, params.Function)
#define gs_shading_mesh_params_common\
gs_shading_params_common;\
gs_data_source_t DataSource;\
int BitsPerCoordinate;\
int BitsPerComponent;\
float *Decode;\
gs_function_t *Function
typedef struct gs_shading_mesh_params_s {
gs_shading_mesh_params_common;
} gs_shading_mesh_params_t;
typedef struct gs_shading_mesh_s {
gs_shading_head_t head;
gs_shading_mesh_params_t params;
} gs_shading_mesh_t;
#define private_st_shading_mesh() \
gs_private_st_composite(st_shading_mesh, gs_shading_mesh_t,\
"gs_shading_mesh_t", shading_mesh_enum_ptrs, shading_mesh_reloc_ptrs)
typedef struct gs_shading_FfGt_params_s {
gs_shading_mesh_params_common;
int BitsPerFlag;
} gs_shading_FfGt_params_t;
#define private_st_shading_FfGt() \
gs_private_st_composite_only(st_shading_FfGt, gs_shading_FfGt_t,\
"gs_shading_FfGt_t", shading_mesh_enum_ptrs, shading_mesh_reloc_ptrs)
typedef struct gs_shading_LfGt_params_s {
gs_shading_mesh_params_common;
int VerticesPerRow;
} gs_shading_LfGt_params_t;
#define private_st_shading_LfGt() \
gs_private_st_composite_only(st_shading_LfGt, gs_shading_LfGt_t,\
"gs_shading_LfGt_t", shading_mesh_enum_ptrs, shading_mesh_reloc_ptrs)
typedef struct gs_shading_Cp_params_s {
gs_shading_mesh_params_common;
int BitsPerFlag;
} gs_shading_Cp_params_t;
#define private_st_shading_Cp() \
gs_private_st_composite_only(st_shading_Cp, gs_shading_Cp_t,\
"gs_shading_Cp_t", shading_mesh_enum_ptrs, shading_mesh_reloc_ptrs)
typedef struct gs_shading_Tpp_params_s {
gs_shading_mesh_params_common;
int BitsPerFlag;
} gs_shading_Tpp_params_t;
#define private_st_shading_Tpp() \
gs_private_st_composite_only(st_shading_Tpp, gs_shading_Tpp_t,\
"gs_shading_Tpp_t", shading_mesh_enum_ptrs, shading_mesh_reloc_ptrs)
void gs_shading_Fb_params_init(gs_shading_Fb_params_t * params);
void gs_shading_A_params_init(gs_shading_A_params_t * params);
void gs_shading_R_params_init(gs_shading_R_params_t * params);
void gs_shading_FfGt_params_init(gs_shading_FfGt_params_t * params);
void gs_shading_LfGt_params_init(gs_shading_LfGt_params_t * params);
void gs_shading_Cp_params_init(gs_shading_Cp_params_t * params);
void gs_shading_Tpp_params_init(gs_shading_Tpp_params_t * params);
int gs_shading_Fb_init(gs_shading_t ** ppsh,
const gs_shading_Fb_params_t * params,
gs_memory_t * mem);
int gs_shading_A_init(gs_shading_t ** ppsh,
const gs_shading_A_params_t * params,
gs_memory_t * mem);
int gs_shading_R_init(gs_shading_t ** ppsh,
const gs_shading_R_params_t * params,
gs_memory_t * mem);
int gs_shading_FfGt_init(gs_shading_t ** ppsh,
const gs_shading_FfGt_params_t * params,
gs_memory_t * mem);
int gs_shading_LfGt_init(gs_shading_t ** ppsh,
const gs_shading_LfGt_params_t * params,
gs_memory_t * mem);
int gs_shading_Cp_init(gs_shading_t ** ppsh,
const gs_shading_Cp_params_t * params,
gs_memory_t * mem);
int gs_shading_Tpp_init(gs_shading_t ** ppsh,
const gs_shading_Tpp_params_t * params,
gs_memory_t * mem);
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
int gs_shading_fill_path_adjusted(const gs_shading_t *psh, gx_path *ppath,
const gs_fixed_rect *prect, gx_device *dev,
gs_imager_state *pis, bool fill_background);
#endif