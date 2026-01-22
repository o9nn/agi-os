#ifndef gxdevmem_INCLUDED
#  define gxdevmem_INCLUDED
#include "gxrplane.h"
#ifndef gx_device_memory_DEFINED
#  define gx_device_memory_DEFINED
typedef struct gx_device_memory_s gx_device_memory;
#endif
struct gx_device_memory_s {
gx_device_forward_common;
uint raster;
byte *base;
#define scan_line_base(dev,y) ((dev)->line_ptrs[y])
gs_memory_t *bitmap_memory;
bool foreign_bits;
gs_memory_t *line_pointer_memory;
bool foreign_line_pointers;
int num_planes;
gx_render_plane_t planes[GX_DEVICE_COLOR_MAX_COMPONENTS];
gs_matrix initial_matrix;
byte **line_ptrs;
gs_const_string palette;
struct _c24 {
gx_color_index rgb;
bits32 rgbr, gbrg, brgb;
} color24;
struct _c40 {
gx_color_index abcde;
bits32 abcd, bcde, cdea, deab, eabc;
} color40;
struct _c48 {
gx_color_index abcdef;
bits32 abcd, cdef, efab;
} color48;
struct _c56 {
gx_color_index abcdefg;
bits32 abcd, bcde, cdef, defg, efga, fgab, gabc;
} color56;
struct _c64 {
gx_color_index abcdefgh;
bits32 abcd, efgh;
} color64;
gs_log2_scale_point log2_scale;
int log2_alpha_bits;
int mapped_x;
int mapped_y;
int mapped_height;
int mapped_start;
gx_color_index save_color;
int plane_depth;
};
extern_st(st_device_memory);
#define public_st_device_memory() \
gs_public_st_composite_use_final(st_device_memory, gx_device_memory,\
"gx_device_memory", device_memory_enum_ptrs, device_memory_reloc_ptrs,\
gx_device_finalize)
#define st_device_memory_max_ptrs (st_device_forward_max_ptrs + 2)
#define mem_device_init_private\
0,			\
(byte *)0,		\
0,			\
true,			\
0,			\
true,			\
0,			\
{ { 0 } },		\
{ identity_matrix_body },	\
(byte **)0,		\
{ (byte *)0, 0 },	\
{ gx_no_color_index },	\
{ gx_no_color_index },	\
{ gx_no_color_index },	\
{ gx_no_color_index },	\
{ gx_no_color_index },	\
{ 0, 0 }, 0,		\
0, 0, 0, 0,		\
gx_no_color_index
ulong gdev_mem_bits_size(const gx_device_memory *mdev, int width,
int height);
ulong gdev_mem_line_ptrs_size(const gx_device_memory *mdev, int width,
int height);
ulong gdev_mem_data_size(const gx_device_memory *mdev, int width,
int height);
#define gdev_mem_bitmap_size(mdev)\
gdev_mem_data_size(mdev, (mdev)->width, (mdev)->height)
int gdev_mem_max_height(const gx_device_memory * dev, int width, ulong size,
bool page_uses_transparency);
#define gdev_mem_raster(mdev)\
gx_device_raster((const gx_device *)(mdev), true)
const gx_device_memory *gdev_mem_device_for_bits(int);
const gx_device_memory *gdev_mem_word_device_for_bits(int);
void gs_make_mem_mono_device(gx_device_memory * mdev, gs_memory_t * mem,
gx_device * target);
void gs_make_mem_device(gx_device_memory * mdev,
const gx_device_memory * mdproto,
gs_memory_t * mem, int page_device,
gx_device * target);
void gs_make_mem_abuf_device(gx_device_memory * adev, gs_memory_t * mem,
gx_device * target,
const gs_log2_scale_point * pscale,
int alpha_bits, int mapped_x);
void gs_make_mem_alpha_device(gx_device_memory * adev, gs_memory_t * mem,
gx_device * target, int alpha_bits);
int gdev_mem_open_scan_lines(gx_device_memory *mdev, int setup_height);
int gdev_mem_set_line_ptrs(gx_device_memory *mdev,
byte *base, int raster, byte **line_ptrs,
int setup_height);
void gdev_mem_mono_set_inverted(gx_device_memory * mdev, bool black_is_1);
bool gs_device_is_memory(const gx_device *);
bool gs_device_is_abuf(const gx_device *);
#endif