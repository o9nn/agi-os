#ifndef gdevcmap_INCLUDED
#  define gdevcmap_INCLUDED
typedef enum {
device_cmap_identity = 0,
device_cmap_snap_to_primaries,
device_cmap_color_to_black_over_white,
device_cmap_monochrome
} gx_device_color_mapping_method_t;
#define device_cmap_max_method device_cmap_monochrome
typedef struct gx_device_cmap_s {
gx_device_forward_common;
gx_device_color_mapping_method_t mapping_method;
} gx_device_cmap;
extern_st(st_device_cmap);
#define public_st_device_cmap()	\
gs_public_st_suffix_add0_final(st_device_cmap, gx_device_cmap,\
"gx_device_cmap", device_cmap_enum_ptrs, device_cmap_reloc_ptrs,\
gx_device_finalize, st_device_forward)
int gdev_cmap_init(gx_device_cmap * dev, gx_device * target,
gx_device_color_mapping_method_t mapping_method);
#endif