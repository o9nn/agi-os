#ifndef gdevvec_INCLUDED
# define gdevvec_INCLUDED
#include "gp.h"
#include "gsropt.h"
#include "gxdevice.h"
#include "gdevbbox.h"
#include "gxiparam.h"
#include "gxistate.h"
#include "gxhldevc.h"
#include "stream.h"
typedef struct gx_device_vector_s gx_device_vector;
#define fname_size (gp_file_name_sizeof - 1)
#define max_dash 11
typedef enum {
gx_path_type_none = 0,
gx_path_type_fill = 1,
gx_path_type_stroke = 2,
gx_path_type_clip = 4,
gx_path_type_winding_number = 0,
gx_path_type_even_odd = 8,
gx_path_type_optimize = 16,
gx_path_type_always_close = 32,
gx_path_type_rule = gx_path_type_winding_number | gx_path_type_even_odd
} gx_path_type_t;
typedef enum {
gx_rect_x_first,
gx_rect_y_first
} gx_rect_direction_t;
typedef struct gx_device_vector_procs_s {
int (*beginpage) (gx_device_vector * vdev);
int (*setlinewidth) (gx_device_vector * vdev, floatp width);
int (*setlinecap) (gx_device_vector * vdev, gs_line_cap cap);
int (*setlinejoin) (gx_device_vector * vdev, gs_line_join join);
int (*setmiterlimit) (gx_device_vector * vdev, floatp limit);
int (*setdash) (gx_device_vector * vdev, const float *pattern,
uint count, floatp offset);
int (*setflat) (gx_device_vector * vdev, floatp flatness);
int (*setlogop) (gx_device_vector * vdev, gs_logical_operation_t lop,
gs_logical_operation_t diff);
bool (*can_handle_hl_color) (gx_device_vector * vdev, const gs_imager_state * pis,
const gx_drawing_color * pdc);
int (*setfillcolor) (gx_device_vector * vdev, const gs_imager_state * pis,
const gx_drawing_color * pdc);
int (*setstrokecolor) (gx_device_vector * vdev, const gs_imager_state * pis,
const gx_drawing_color * pdc);
int (*dopath) (gx_device_vector * vdev, const gx_path * ppath,
gx_path_type_t type, const gs_matrix *pmat);
int (*dorect) (gx_device_vector * vdev, fixed x0, fixed y0, fixed x1,
fixed y1, gx_path_type_t type);
int (*beginpath) (gx_device_vector * vdev, gx_path_type_t type);
int (*moveto) (gx_device_vector * vdev, floatp x0, floatp y0,
floatp x, floatp y, gx_path_type_t type);
int (*lineto) (gx_device_vector * vdev, floatp x0, floatp y0,
floatp x, floatp y, gx_path_type_t type);
int (*curveto) (gx_device_vector * vdev, floatp x0, floatp y0,
floatp x1, floatp y1, floatp x2, floatp y2,
floatp x3, floatp y3, gx_path_type_t type);
int (*closepath) (gx_device_vector * vdev, floatp x0, floatp y0,
floatp x_start, floatp y_start, gx_path_type_t type);
int (*endpath) (gx_device_vector * vdev, gx_path_type_t type);
} gx_device_vector_procs;
int gdev_vector_setflat(gx_device_vector * vdev, floatp flatness);
int gdev_vector_dopath(gx_device_vector * vdev, const gx_path * ppath,
gx_path_type_t type, const gs_matrix *pmat);
int gdev_vector_dorect(gx_device_vector * vdev, fixed x0, fixed y0,
fixed x1, fixed y1, gx_path_type_t type);
#define gx_device_vector_common\
gx_device_common;\
gs_memory_t *v_memory;\
\
const gx_device_vector_procs *vec_procs;\
\
char fname[fname_size + 1];\
FILE *file;\
stream *strm;\
byte *strmbuf;\
uint strmbuf_size;\
int open_options; \
\
gs_imager_state state;\
float dash_pattern[max_dash];\
bool fill_used_process_color;\
bool stroke_used_process_color;\
gx_hl_saved_color saved_fill_color;\
gx_hl_saved_color saved_stroke_color;\
gs_id no_clip_path_id; \
gs_id clip_path_id;\
\
gx_path_type_t fill_options, stroke_options; \
gs_point scale; \
bool in_page; \
gx_device_bbox *bbox_device; \
\
gx_color_index black, white
#define vdev_proc(vdev, p) ((vdev)->vec_procs->p)
#define vector_initial_values\
0, \
0, \
{ 0 }, \
0, \
0, \
0, \
0, \
0, \
{ 0 }, \
{ 0 }, \
true, \
true, \
{ 0 }, \
{ 0 }, \
gs_no_id, \
gs_no_id, \
0, 0, \
{ X_DPI/72.0, Y_DPI/72.0 }, \
0, \
0, \
gx_no_color_index, \
gx_no_color_index
struct gx_device_vector_s {
gx_device_vector_common;
};
extern_st(st_device_vector);
#define public_st_device_vector() \
gs_public_st_suffix_add3_final(st_device_vector, gx_device_vector,\
"gx_device_vector", device_vector_enum_ptrs,\
device_vector_reloc_ptrs, gx_device_finalize, st_device, strm, strmbuf,\
bbox_device)
#define st_device_vector_max_ptrs (st_device_max_ptrs + 3)
void gdev_vector_init(gx_device_vector * vdev);
void gdev_vector_reset(gx_device_vector * vdev);
#define VECTOR_OPEN_FILE_ASCII 1
#define VECTOR_OPEN_FILE_SEQUENTIAL 2
#define VECTOR_OPEN_FILE_SEQUENTIAL_OK 4
#define VECTOR_OPEN_FILE_BBOX 8
int gdev_vector_open_file_options(gx_device_vector * vdev,
uint strmbuf_size, int open_options);
#define gdev_vector_open_file_bbox(vdev, bufsize, bbox)\
gdev_vector_open_file_options(vdev, bufsize,\
(bbox ? VECTOR_OPEN_FILE_BBOX : 0))
#define gdev_vector_open_file(vdev, strmbuf_size)\
gdev_vector_open_file_bbox(vdev, strmbuf_size, false)
stream *gdev_vector_stream(gx_device_vector * vdev);
int gdev_vector_update_log_op(gx_device_vector * vdev,
gs_logical_operation_t lop);
int gdev_vector_update_fill_color(gx_device_vector * vdev,
const gs_imager_state * pis,
const gx_drawing_color * pdcolor);
int gdev_vector_prepare_fill(gx_device_vector * vdev,
const gs_imager_state * pis,
const gx_fill_params * params,
const gx_drawing_color * pdcolor);
int gdev_vector_prepare_stroke(gx_device_vector * vdev,
const gs_imager_state * pis,
const gx_stroke_params * params,
const gx_drawing_color * pdcolor,
floatp scale);
int gdev_vector_stroke_scaling(const gx_device_vector *vdev,
const gs_imager_state *pis,
double *pscale, gs_matrix *pmat);
typedef struct gdev_vector_dopath_state_s {
gx_device_vector *vdev;
gx_path_type_t type;
bool first;
gs_matrix scale_mat;
gs_point start;
gs_point prev;
} gdev_vector_dopath_state_t;
void gdev_vector_dopath_init(gdev_vector_dopath_state_t *state,
gx_device_vector *vdev,
gx_path_type_t type, const gs_matrix *pmat);
int gdev_vector_dopath_segment(gdev_vector_dopath_state_t *state, int pe_op,
gs_fixed_point vs[3]);
int gdev_vector_write_polygon(gx_device_vector * vdev,
const gs_fixed_point * points, uint count,
bool close, gx_path_type_t type);
int gdev_vector_write_rectangle(gx_device_vector * vdev,
fixed x0, fixed y0, fixed x1, fixed y1,
bool close, gx_rect_direction_t dir);
int gdev_vector_write_clip_path(gx_device_vector * vdev,
const gx_clip_path * pcpath);
int gdev_vector_update_clip_path(gx_device_vector * vdev,
const gx_clip_path * pcpath);
int gdev_vector_close_file(gx_device_vector * vdev);
#define gdev_vector_image_enum_common\
gx_image_enum_common;\
\
gs_memory_t *memory; \
gx_image_enum_common_t *default_info; \
gx_image_enum_common_t *bbox_info; \
int width, height;\
int bits_per_pixel; \
uint bits_per_row; \
\
int y
typedef struct gdev_vector_image_enum_s {
gdev_vector_image_enum_common;
} gdev_vector_image_enum_t;
extern_st(st_vector_image_enum);
#define public_st_vector_image_enum() \
gs_public_st_ptrs2(st_vector_image_enum, gdev_vector_image_enum_t,\
"gdev_vector_image_enum_t", vector_image_enum_enum_ptrs,\
vector_image_enum_reloc_ptrs, default_info, bbox_info)
int gdev_vector_begin_image(gx_device_vector * vdev,
const gs_imager_state * pis, const gs_image_t * pim,
gs_image_format_t format, const gs_int_rect * prect,
const gx_drawing_color * pdcolor, const gx_clip_path * pcpath,
gs_memory_t * mem, const gx_image_enum_procs_t * pprocs,
gdev_vector_image_enum_t * pie);
int gdev_vector_end_image(gx_device_vector * vdev,
gdev_vector_image_enum_t * pie, bool draw_last, gx_color_index pad);
dev_proc_put_params(gdev_vector_put_params);
dev_proc_get_params(gdev_vector_get_params);
dev_proc_fill_rectangle(gdev_vector_fill_rectangle);
dev_proc_fill_path(gdev_vector_fill_path);
dev_proc_stroke_path(gdev_vector_stroke_path);
dev_proc_fill_trapezoid(gdev_vector_fill_trapezoid);
dev_proc_fill_parallelogram(gdev_vector_fill_parallelogram);
dev_proc_fill_triangle(gdev_vector_fill_triangle);
#endif