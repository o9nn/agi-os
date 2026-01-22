#ifndef gxcoord_INCLUDED
# define gxcoord_INCLUDED
#include "gscoord.h"
int gx_translate_to_fixed(gs_state *, fixed, fixed);
int gx_scale_char_matrix(gs_state *, int, int);
int gx_matrix_to_fixed_coeff(const gs_matrix *, fixed_coeff *, int);
#endif