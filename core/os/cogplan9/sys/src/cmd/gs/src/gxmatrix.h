#ifndef gxmatrix_INCLUDED
# define gxmatrix_INCLUDED
#include "gsmatrix.h"
#define PRECISE_CURRENTPOINT 1
struct gs_matrix_fixed_s {
_matrix_body;
fixed tx_fixed, ty_fixed;
bool txy_fixed_valid;
};
#ifndef gs_matrix_fixed_DEFINED
#define gs_matrix_fixed_DEFINED
typedef struct gs_matrix_fixed_s gs_matrix_fixed;
#endif
int gs_matrix_fixed_from_matrix(gs_matrix_fixed *, const gs_matrix *);
int gs_point_transform2fixed(const gs_matrix_fixed *, floatp, floatp,
gs_fixed_point *);
int gs_distance_transform2fixed(const gs_matrix_fixed *, floatp, floatp,
gs_fixed_point *);
#if PRECISE_CURRENTPOINT
int gs_point_transform2fixed_rounding(const gs_matrix_fixed * pmat,
floatp x, floatp y, gs_fixed_point * ppt);
#endif
typedef struct {
long xx, xy, yx, yy;
int skewed;
int shift;
int max_bits;
fixed round;
} fixed_coeff;
fixed fixed_coeff_mult(fixed, long, const fixed_coeff *, int);
#define m_fixed(v, c, fc, maxb)\
(((v) + (fixed_1 << (maxb - 1))) &\
((-fixed_1 << maxb) | _fixed_fraction_v) ? \
fixed_coeff_mult((v), (fc).c, &(fc), maxb) : \
arith_rshift(fixed2int_var(v) * (fc).c + (fc).round, (fc).shift))
#endif