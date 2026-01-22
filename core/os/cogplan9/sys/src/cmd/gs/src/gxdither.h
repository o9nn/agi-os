#ifndef gxdither_INCLUDED
# define gxdither_INCLUDED
#include "gxfrac.h"
#ifndef gx_device_halftone_DEFINED
# define gx_device_halftone_DEFINED
typedef struct gx_device_halftone_s gx_device_halftone;
#endif
int gx_render_device_DeviceN(frac * pcolor, gx_device_color * pdevc,
gx_device * dev, gx_device_halftone * pdht, const gs_int_point * ht_phase);
int gx_devn_reduce_colored_halftone(gx_device_color *pdevc, gx_device *dev);
#endif