#ifndef gxctable_INCLUDED
# define gxctable_INCLUDED
#include "gxfixed.h"
#include "gxfrac.h"
typedef struct gx_color_lookup_table_s {
int n;
int dims[4];
int m;
const gs_const_string *table;
} gx_color_lookup_table;
void gx_color_interpolate_nearest(const fixed * pi,
const gx_color_lookup_table * pclt, frac * pv);
void gx_color_interpolate_linear(const fixed * pi,
const gx_color_lookup_table * pclt, frac * pv);
#endif