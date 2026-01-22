#ifndef gsmatrix_INCLUDED
# define gsmatrix_INCLUDED
#define _matrix_body\
float xx, xy, yx, yy, tx, ty
struct gs_matrix_s {
_matrix_body;
};
#ifndef gs_matrix_DEFINED
# define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
#define constant_matrix_body(xx, xy, yx, yy, tx, ty)\
(float)(xx), (float)(xy), (float)(yx),\
(float)(yy), (float)(tx), (float)(ty)
#define is_xxyy(pmat) is_fzero2((pmat)->xy, (pmat)->yx)
#define is_xyyx(pmat) is_fzero2((pmat)->xx, (pmat)->yy)
#define identity_matrix_body\
constant_matrix_body(1, 0, 0, 1, 0, 0)
void gs_make_identity(gs_matrix *);
int gs_make_translation(floatp, floatp, gs_matrix *),
gs_make_scaling(floatp, floatp, gs_matrix *),
gs_make_rotation(floatp, gs_matrix *);
int gs_matrix_multiply(const gs_matrix *, const gs_matrix *, gs_matrix *),
gs_matrix_invert(const gs_matrix *, gs_matrix *),
gs_matrix_translate(const gs_matrix *, floatp, floatp, gs_matrix *),
gs_matrix_scale(const gs_matrix *, floatp, floatp, gs_matrix *),
gs_matrix_rotate(const gs_matrix *, floatp, gs_matrix *);
int gs_point_transform(floatp, floatp, const gs_matrix *, gs_point *),
gs_point_transform_inverse(floatp, floatp, const gs_matrix *, gs_point *),
gs_distance_transform(floatp, floatp, const gs_matrix *, gs_point *),
gs_distance_transform_inverse(floatp, floatp, const gs_matrix *, gs_point *),
gs_points_bbox(const gs_point[4], gs_rect *),
gs_bbox_transform_only(const gs_rect *, const gs_matrix *, gs_point[4]),
gs_bbox_transform(const gs_rect *, const gs_matrix *, gs_rect *),
gs_bbox_transform_inverse(const gs_rect *, const gs_matrix *, gs_rect *);
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
int sget_matrix(stream *, gs_matrix *);
int sput_matrix(stream *, const gs_matrix *);
#endif