#ifndef gxshade_INCLUDED
# define gxshade_INCLUDED
#include "gsshade.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "stream.h"
typedef struct gs_shading_Fb_s {
gs_shading_head_t head;
gs_shading_Fb_params_t params;
} gs_shading_Fb_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_Fb_fill_rectangle);
typedef struct gs_shading_A_s {
gs_shading_head_t head;
gs_shading_A_params_t params;
} gs_shading_A_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_A_fill_rectangle);
typedef struct gs_shading_R_s {
gs_shading_head_t head;
gs_shading_R_params_t params;
} gs_shading_R_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_R_fill_rectangle);
typedef struct gs_shading_FfGt_s {
gs_shading_head_t head;
gs_shading_FfGt_params_t params;
} gs_shading_FfGt_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_FfGt_fill_rectangle);
typedef struct gs_shading_LfGt_s {
gs_shading_head_t head;
gs_shading_LfGt_params_t params;
} gs_shading_LfGt_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_LfGt_fill_rectangle);
typedef struct gs_shading_Cp_s {
gs_shading_head_t head;
gs_shading_Cp_params_t params;
} gs_shading_Cp_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_Cp_fill_rectangle);
typedef struct gs_shading_Tpp_s {
gs_shading_head_t head;
gs_shading_Tpp_params_t params;
} gs_shading_Tpp_t;
SHADING_FILL_RECTANGLE_PROC(gs_shading_Tpp_fill_rectangle);
typedef struct shade_coord_stream_s shade_coord_stream_t;
struct shade_coord_stream_s {
stream ds;
stream *s;
uint bits;
int left;
bool ds_EOF;
const gs_shading_mesh_params_t *params;
const gs_matrix_fixed *pctm;
int (*get_value)(shade_coord_stream_t *cs, int num_bits, uint *pvalue);
int (*get_decoded)(shade_coord_stream_t *cs, int num_bits,
const float decode[2], float *pvalue);
bool (*is_eod)(const shade_coord_stream_t *cs);
};
typedef struct mesh_vertex_s {
gs_fixed_point p;
float cc[GS_CLIENT_COLOR_MAX_COMPONENTS];
} mesh_vertex_t;
typedef struct shading_vertex_s shading_vertex_t;
void shade_next_init(shade_coord_stream_t * cs,
const gs_shading_mesh_params_t * params,
const gs_imager_state * pis);
int shade_next_flag(shade_coord_stream_t * cs, int BitsPerFlag);
int shade_next_coords(shade_coord_stream_t * cs, gs_fixed_point * ppt,
int num_points);
int shade_next_color(shade_coord_stream_t * cs, float *pc);
int shade_next_vertex(shade_coord_stream_t * cs, shading_vertex_t * vertex);
#define shading_fill_state_common\
gx_device *dev;\
gs_imager_state *pis;\
const gs_color_space *direct_space;\
int num_components; \
float cc_max_error[GS_CLIENT_COLOR_MAX_COMPONENTS]
typedef struct shading_fill_state_s {
shading_fill_state_common;
} shading_fill_state_t;
void shade_init_fill_state(shading_fill_state_t * pfs,
const gs_shading_t * psh, gx_device * dev,
gs_imager_state * pis);
#ifndef gx_device_color_DEFINED
# define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
int shade_fill_path(const shading_fill_state_t * pfs, gx_path * ppath,
gx_device_color * pdevc, const gs_fixed_point *fill_adjust);
#endif