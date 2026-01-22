#ifndef gsimage_INCLUDED
# define gsimage_INCLUDED
#include "gsiparam.h"
#ifndef gx_image_enum_common_t_DEFINED
# define gx_image_enum_common_t_DEFINED
typedef struct gx_image_enum_common_s gx_image_enum_common_t;
#endif
int gs_image_begin_typed(const gs_image_common_t * pic, gs_state * pgs,
bool uses_color, gx_image_enum_common_t ** ppie);
typedef struct gs_image_enum_s gs_image_enum;
gs_image_enum *gs_image_enum_alloc(gs_memory_t *, client_name_t);
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
int gs_image_init(gs_image_enum * penum, const gs_image_t * pim,
bool MultipleDataSources, gs_state * pgs);
int gs_image_enum_init(gs_image_enum * penum,
gx_image_enum_common_t * pie,
const gs_data_image_t * pim, gs_state *pgs);
uint gs_image_bytes_per_plane_row(const gs_image_enum * penum, int plane);
#define gs_image_bytes_per_row(penum)\
gs_image_bytes_per_plane_row(penum, 0)
const byte *gs_image_planes_wanted(gs_image_enum *penum);
int gs_image_next_planes(gs_image_enum *penum, gs_const_string *plane_data,
uint *used);
int gs_image_next(gs_image_enum * penum, const byte * dbytes,
uint dsize, uint * pused);
int gs_image_cleanup(gs_image_enum * penum);
int gs_image_cleanup_and_free_enum(gs_image_enum * penum);
#endif