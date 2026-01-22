#ifndef gxline_INCLUDED
#  define gxline_INCLUDED
#include "gslparam.h"
#include "gsmatrix.h"
typedef struct gx_dash_params_s {
float *pattern;
uint pattern_size;
float offset;
bool adapt;
float pattern_length;
bool init_ink_on;
int init_index;
float init_dist_left;
} gx_dash_params;
#define gx_dash_params_initial\
NULL, 0, 0.0, 0, 0.0, 1, 0, 0.0
typedef struct gx_line_params_s {
float half_width;
gs_line_cap cap;
gs_line_join join;
int curve_join;
float miter_limit;
float miter_check;
float dot_length;
bool dot_length_absolute;
gs_matrix dot_orientation;
gx_dash_params dash;
} gx_line_params;
#define gx_set_line_width(plp, wid)\
((plp)->half_width = (wid) / 2)
#define gx_current_line_width(plp)\
((plp)->half_width * 2)
int gx_set_miter_limit(gx_line_params *, floatp);
#define gx_current_miter_limit(plp)\
((plp)->miter_limit)
int gx_set_dash(gx_dash_params *, const float *, uint, floatp, gs_memory_t *);
#define gx_set_dash_adapt(pdp, adpt) ((pdp)->adapt = (adpt))
int gx_set_dot_length(gx_line_params *, floatp, bool);
#define gx_line_params_initial\
0.0, gs_cap_butt, gs_join_miter, -1,\
10.0, (float)0.20305866, 0.0, 0,\
{ identity_matrix_body }, { gx_dash_params_initial }
#endif