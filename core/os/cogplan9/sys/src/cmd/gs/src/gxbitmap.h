#ifndef gxbitmap_INCLUDED
# define gxbitmap_INCLUDED
#include "gstypes.h"
#include "gsbitmap.h"
typedef gs_bitmap_id gx_bitmap_id;
#define gx_no_bitmap_id gs_no_bitmap_id
#if arch_align_long_mod <= 4
# define log2_align_bitmap_mod 2
#else
#if arch_align_long_mod == 8
# define log2_align_bitmap_mod 3
#endif
#endif
#define align_bitmap_mod (1 << log2_align_bitmap_mod)
#define bitmap_raster(width_bits)\
((uint)((((width_bits) + (align_bitmap_mod * 8 - 1))\
>> (log2_align_bitmap_mod + 3)) << log2_align_bitmap_mod))
#define gx_bitmap_common(data_type) gs_bitmap_common(data_type)
typedef struct gx_bitmap_s {
gx_bitmap_common(byte);
} gx_bitmap;
typedef struct gx_const_bitmap_s {
gx_bitmap_common(const byte);
} gx_const_bitmap;
#define gx_tile_bitmap_common(data_type) gs_tile_bitmap_common(data_type)
typedef struct gx_tile_bitmap_s {
gx_tile_bitmap_common(byte);
} gx_tile_bitmap;
typedef struct gx_const_tile_bitmap_s {
gx_tile_bitmap_common(const byte);
} gx_const_tile_bitmap;
#define gx_strip_bitmap_common(data_type)\
gx_tile_bitmap_common(data_type);\
ushort rep_shift;\
ushort shift
typedef struct gx_strip_bitmap_s {
gx_strip_bitmap_common(byte);
} gx_strip_bitmap;
typedef struct gx_const_strip_bitmap_s {
gx_strip_bitmap_common(const byte);
} gx_const_strip_bitmap;
extern_st(st_gx_strip_bitmap);
#define public_st_gx_strip_bitmap() \
gs_public_st_suffix_add0_local(st_gx_strip_bitmap, gx_strip_bitmap,\
"gx_strip_bitmap", bitmap_enum_ptrs, bitmap_reloc_ptrs,\
st_gs_tile_bitmap)
#define st_gx_strip_bitmap_max_ptrs 1
#endif