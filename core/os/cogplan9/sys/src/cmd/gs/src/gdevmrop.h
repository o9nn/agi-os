#ifndef gdevmrop_INCLUDED
#  define gdevmrop_INCLUDED
gs_rop3_t gs_transparent_rop(gs_logical_operation_t lop);
#ifdef DEBUG
void trace_copy_rop(const char *cname, gx_device * dev,
const byte * sdata, int sourcex, uint sraster,
gx_bitmap_id id, const gx_color_index * scolors,
const gx_strip_bitmap * textures,
const gx_color_index * tcolors,
int x, int y, int width, int height,
int phase_x, int phase_y, gs_logical_operation_t lop);
#endif
#ifndef gx_device_color_DEFINED
#  define gx_device_color_DEFINED
typedef struct gx_device_color_s gx_device_color;
#endif
#ifndef gx_device_rop_texture_DEFINED
#  define gx_device_rop_texture_DEFINED
typedef struct gx_device_rop_texture_s gx_device_rop_texture;
#endif
struct gx_device_rop_texture_s {
gx_device_forward_common;
gs_logical_operation_t log_op;
gx_device_color texture;
};
#define private_st_device_rop_texture()	\
gs_private_st_composite_use_final(st_device_rop_texture,\
gx_device_rop_texture, "gx_device_rop_texture",\
device_rop_texture_enum_ptrs, device_rop_texture_reloc_ptrs,\
gx_device_finalize)
int gx_alloc_rop_texture_device(gx_device_rop_texture ** prsdev,
gs_memory_t * mem,
client_name_t cname);
void gx_make_rop_texture_device(gx_device_rop_texture * rsdev,
gx_device * target,
gs_logical_operation_t lop,
const gx_device_color * texture);
#endif