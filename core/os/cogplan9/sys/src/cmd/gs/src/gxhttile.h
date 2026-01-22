#ifndef gxhttile_INCLUDED
# define gxhttile_INCLUDED
#ifndef gx_ht_tile_DEFINED
# define gx_ht_tile_DEFINED
typedef struct gx_ht_tile_s gx_ht_tile;
#endif
struct gx_ht_tile_s {
gx_strip_bitmap tiles;
int level;
uint index;
};
#endif