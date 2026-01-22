#ifndef gxgetbit_INCLUDED
#  define gxgetbit_INCLUDED
#include "gxbitfmt.h"
#ifndef gs_get_bits_params_DEFINED
#  define gs_get_bits_params_DEFINED
typedef struct gs_get_bits_params_s gs_get_bits_params_t;
#endif
typedef gx_bitmap_format_t gs_get_bits_options_t;
struct gs_get_bits_params_s {
gs_get_bits_options_t options;
byte *data[32];
int x_offset;
uint raster;
};
int gx_get_bits_return_pointer(gx_device * dev, int x, int h,
gs_get_bits_params_t * params,
const gs_get_bits_params_t *stored,
byte * stored_base);
int gx_get_bits_copy(gx_device * dev, int x, int w, int h,
gs_get_bits_params_t * params,
const gs_get_bits_params_t *stored,
const byte * src_base, uint dev_raster);
#endif