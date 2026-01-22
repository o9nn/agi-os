#ifndef gxdhtres_INCLUDED
# define gxdhtres_INCLUDED
#include "stdpre.h"
#ifndef gx_device_halftone_resource_DEFINED
# define gx_device_halftone_resource_DEFINED
typedef struct gx_device_halftone_resource_s gx_device_halftone_resource_t;
#endif
struct gx_device_halftone_resource_s {
const char *rname;
int HalftoneType;
int Width;
int Height;
int num_levels;
const unsigned int *levels;
const void *bit_data;
int elt_size;
};
#define DEVICE_HALFTONE_RESOURCE_PROC(proc)\
const gx_device_halftone_resource_t *const *proc(void)
#endif