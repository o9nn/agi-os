#ifndef gxdcconv_INCLUDED
#  define gxdcconv_INCLUDED
#include "gxfrac.h"
frac color_rgb_to_gray(frac r, frac g, frac b,
const gs_imager_state * pis);
void color_rgb_to_cmyk(frac r, frac g, frac b,
const gs_imager_state * pis, frac cmyk[4]);
frac color_cmyk_to_gray(frac c, frac m, frac y, frac k,
const gs_imager_state * pis);
void color_cmyk_to_rgb(frac c, frac m, frac y, frac k,
const gs_imager_state * pis, frac rgb[3]);
#endif