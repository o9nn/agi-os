#ifndef gsbitmap_INCLUDED
#define gsbitmap_INCLUDED
#include "gsstype.h"
typedef gs_id gs_bitmap_id;
#define gs_no_bitmap_id gs_no_id
#define gs_bitmap_common(data_type) \
data_type * data; \
int raster; \
gs_int_point size; \
gs_bitmap_id id
typedef struct gs_bitmap_s {
gs_bitmap_common(byte);
} gs_bitmap;
typedef struct gs_const_bitmap_s {
gs_bitmap_common(const byte);
} gs_const_bitmap;
#define gs_tile_bitmap_common(data_type) \
gs_bitmap_common(data_type); \
ushort rep_width, rep_height
typedef struct gs_tile_bitmap_s {
gs_tile_bitmap_common(byte);
} gs_tile_bitmap;
typedef struct gs_const_tile_bitmap_s {
gs_tile_bitmap_common(const byte);
} gs_const_tile_bitmap;
#define gs_depth_bitmap_common(data_type) \
gs_bitmap_common(data_type); \
byte pix_depth; \
byte num_comps \
typedef struct gs_depth_bitmap_s {
gs_depth_bitmap_common(byte);
} gs_depth_bitmap;
typedef struct gs_const_depth_bitmap_s {
gs_depth_bitmap_common(const byte);
} gs_const_depth_bitmap;
#define gs_tile_depth_bitmap_common(data_type) \
gs_tile_bitmap_common(data_type); \
byte pix_depth; \
byte num_comps \
typedef struct gs_tile_depth_bitmap_s {
gs_tile_depth_bitmap_common(byte);
} gs_tile_depth_bitmap;
typedef struct gs_const_tile_depth_bitmap_s {
gs_tile_depth_bitmap_common(const byte);
} gs_const_tile_depth_bitmap;
extern_st(st_gs_bitmap);
extern_st(st_gs_tile_bitmap);
extern_st(st_gs_depth_bitmap);
extern_st(st_gs_tile_depth_bitmap);
#define public_st_gs_bitmap() \
gs_public_st_ptrs1( st_gs_bitmap, \
gs_bitmap, \
"client bitmap", \
bitmap_enum_ptrs, \
bitmap_reloc_ptrs, \
data \
)
#define public_st_gs_tile_bitmap() \
gs_public_st_suffix_add0_local( st_gs_tile_bitmap, \
gs_tile_bitmap, \
"client tile bitmap", \
bitmap_enum_ptrs, \
bitmap_reloc_ptrs, \
st_gs_bitmap \
)
#define public_st_gs_depth_bitmap() \
gs_public_st_suffix_add0_local( st_gs_depth_bitmap, \
gs_depth_bitmap, \
"client depth bitmap", \
bitmap_enum_ptrs, \
bitmap_reloc_ptrs, \
st_gs_bitmap \
)
#define public_st_gs_tile_depth_bitmap() \
gs_public_st_suffix_add0_local( st_gs_tile_depth_bitmap, \
gs_tile_depth_bitmap, \
"client tile_depth bitmap", \
bitmap_enum_ptrs, \
bitmap_reloc_ptrs, \
st_gs_tile_bitmap \
)
#endif