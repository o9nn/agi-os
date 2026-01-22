#ifndef gxmclip_INCLUDED
#  define gxmclip_INCLUDED
#include "gxclip.h"
#define tile_clip_buffer_request 300
#define tile_clip_buffer_size\
((tile_clip_buffer_request / arch_sizeof_long) * arch_sizeof_long)
typedef struct gx_device_mask_clip_s {
gx_device_forward_common;
gx_strip_bitmap tiles;
gx_device_memory mdev;
gs_int_point phase;
union _b {
byte bytes[tile_clip_buffer_size];
ulong longs[tile_clip_buffer_size / arch_sizeof_long];
} buffer;
} gx_device_mask_clip;
extern_st(st_device_mask_clip);
#define public_st_device_mask_clip()	\
gs_public_st_composite_use_final(st_device_mask_clip, gx_device_mask_clip,\
"gx_device_mask_clip", device_mask_clip_enum_ptrs,\
device_mask_clip_reloc_ptrs, gx_device_finalize)
int gx_mask_clip_initialize(gx_device_mask_clip * cdev,
const gx_device_mask_clip * proto,
const gx_bitmap * bits, gx_device * tdev,
int tx, int ty, gs_memory_t *mem);
#define setup_mask_copy_mono(cdev, color, mcolor0, mcolor1)\
BEGIN\
if ( cdev->mdev.base == 0 ) {\
\
return gx_default_copy_mono(dev, data, sourcex, raster, id,\
x, y, w, h, color0, color1);\
}\
if ( color1 != gx_no_color_index ) {\
if ( color0 != gx_no_color_index ) {\
\
code =\
(*dev_proc(dev, fill_rectangle))(dev, x, y, w, h, color0);\
if ( code < 0 )\
return code;\
}\
color = color1;\
mcolor0 = 0, mcolor1 = gx_no_color_index;\
} else if ( color0 != gx_no_color_index ) {\
color = color0;\
mcolor0 = gx_no_color_index, mcolor1 = 0;\
} else\
return 0;\
END
#endif