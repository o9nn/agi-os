#ifndef gxpcache_INCLUDED
# define gxpcache_INCLUDED
#ifndef gx_pattern_cache_DEFINED
# define gx_pattern_cache_DEFINED
typedef struct gx_pattern_cache_s gx_pattern_cache;
#endif
#ifndef gx_color_tile_DEFINED
# define gx_color_tile_DEFINED
typedef struct gx_color_tile_s gx_color_tile;
#endif
struct gx_pattern_cache_s {
gs_memory_t *memory;
gx_color_tile *tiles;
uint num_tiles;
uint tiles_used;
uint next;
ulong bits_used;
ulong max_bits;
void (*free_all) (gx_pattern_cache *);
};
#define private_st_pattern_cache() \
gs_private_st_ptrs1(st_pattern_cache, gx_pattern_cache,\
"gx_pattern_cache", pattern_cache_enum, pattern_cache_reloc, tiles)
#endif