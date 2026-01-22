#ifndef gxdevndi_INCLUDED
#  define gxdevndi_INCLUDED
#include "gxfrac.h"
#ifndef gx_device_halftone_DEFINED
#  define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
int gx_render_device_color_devn(P10(frac red, frac green, frac blue, frac white,
bool cmyk, gx_color_value alpha,
gx_device_color * pdevc, gx_device * dev,
gx_device_halftone * pdht,
const gs_int_point * ht_phase));
#endif