#ifndef gxclip2_INCLUDED
#  define gxclip2_INCLUDED
#include "gxmclip.h"
typedef gx_device_mask_clip gx_device_tile_clip;
#define st_device_tile_clip st_device_mask_clip
#define private_st_device_tile_clip() \
const byte gxclip2_dummy = 0
int tile_clip_initialize(gx_device_tile_clip * cdev,
const gx_strip_bitmap * tiles,
gx_device * tdev, int px, int py,
gs_memory_t *mem);
void tile_clip_set_phase(gx_device_tile_clip * cdev, int px, int py);
#endif