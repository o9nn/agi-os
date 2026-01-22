#ifndef gzline_INCLUDED
# define gzline_INCLUDED
#include "gxline.h"
#define private_st_line_params() \
gs_private_st_complex_only(st_line_params, gx_line_params, "line_params",\
0, line_params_enum_ptrs, line_params_reloc_ptrs, 0)
#define st_line_params_num_ptrs 1
const gx_line_params *gs_currentlineparams(const gs_imager_state *);
#endif