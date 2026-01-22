#ifndef gxshade4_INCLUDED
# define gxshade4_INCLUDED
#define USE_LINEAR_COLOR_PROCS 1
#define QUADRANGLES 0
#define INTERPATCH_PADDING (fixed_1 / 2)
#define COLOR_CONTIGUITY 1
#define LAZY_WEDGES 1
#define VD_TRACE_DOWN 1
#define NOFILL_TEST 0
#define SKIP_TEST 0
#define mesh_max_depth (16 * 3 + 1)
typedef struct mesh_frame_s {
mesh_vertex_t va, vb, vc;
bool check_clipping;
} mesh_frame_t;
#define mesh_fill_state_common\
shading_fill_state_common;\
const gs_shading_mesh_t *pshm;\
gs_fixed_rect rect;\
int depth;\
mesh_frame_t frames[mesh_max_depth]
typedef struct mesh_fill_state_s {
mesh_fill_state_common;
} mesh_fill_state_t;
typedef struct wedge_vertex_list_elem_s wedge_vertex_list_elem_t;
struct wedge_vertex_list_elem_s {
gs_fixed_point p;
int level;
bool divide_count;
wedge_vertex_list_elem_t *next, *prev;
};
typedef struct {
bool last_side;
wedge_vertex_list_elem_t *beg, *end;
} wedge_vertex_list_t;
#define LAZY_WEDGES_MAX_LEVEL 9
typedef struct patch_fill_state_s {
mesh_fill_state_common;
const gs_function_t *Function;
bool vectorization;
int n_color_args;
fixed max_small_coord;
wedge_vertex_list_elem_t *wedge_vertex_list_elem_buffer;
wedge_vertex_list_elem_t *free_wedge_vertex;
int wedge_vertex_list_elem_count;
int wedge_vertex_list_elem_count_max;
gs_client_color color_domain;
fixed fixed_flat;
double smoothness;
bool maybe_self_intersecting;
bool monotonic_color;
bool linear_color;
bool unlinear;
bool inside;
} patch_fill_state_t;
typedef struct patch_color_s {
float t[2];
gs_client_color cc;
} patch_color_t;
struct shading_vertex_s {
gs_fixed_point p;
patch_color_t c;
};
typedef struct patch_curve_s {
mesh_vertex_t vertex;
gs_fixed_point control[2];
bool straight;
} patch_curve_t;
int mesh_init_fill_state(mesh_fill_state_t * pfs,
const gs_shading_mesh_t * psh,
const gs_fixed_rect * rect_clip,
gx_device * dev, gs_imager_state * pis);
int init_patch_fill_state(patch_fill_state_t *pfs);
void term_patch_fill_state(patch_fill_state_t *pfs);
int mesh_triangle(patch_fill_state_t *pfs,
const shading_vertex_t *p0, const shading_vertex_t *p1, const shading_vertex_t *p2);
int mesh_padding(patch_fill_state_t *pfs, const gs_fixed_point *p0, const gs_fixed_point *p1,
const patch_color_t *c0, const patch_color_t *c1);
int patch_fill(patch_fill_state_t * pfs, const patch_curve_t curve[4],
const gs_fixed_point interior[4],
void (*transform) (gs_fixed_point *, const patch_curve_t[4],
const gs_fixed_point[4], floatp, floatp));
int wedge_vertex_list_elem_buffer_alloc(patch_fill_state_t *pfs);
void wedge_vertex_list_elem_buffer_free(patch_fill_state_t *pfs);
void patch_resolve_color(patch_color_t * ppcr, const patch_fill_state_t *pfs);
int gx_shade_background(gx_device *pdev, const gs_fixed_rect *rect,
const gx_device_color *pdevc, gs_logical_operation_t log_op);
#endif