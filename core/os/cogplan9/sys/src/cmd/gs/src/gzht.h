#ifndef gzht_INCLUDED
#  define gzht_INCLUDED
#include "gscsel.h"
#include "gxht.h"
#include "gxfmap.h"
#include "gxdht.h"
#include "gxhttile.h"
void gx_sort_ht_order(gx_ht_bit *, uint);
int gx_ht_alloc_ht_order(gx_ht_order * porder, uint width, uint height,
uint num_levels, uint num_bits, uint strip_shift,
const gx_ht_order_procs_t *procs,
gs_memory_t * mem);
int gx_ht_alloc_order(gx_ht_order * porder, uint width, uint height,
uint strip_shift, uint num_levels, gs_memory_t *mem);
int gx_ht_alloc_threshold_order(gx_ht_order * porder, uint width,
uint height, uint num_levels,
gs_memory_t * mem);
int gx_ht_alloc_client_order(gx_ht_order * porder, uint width, uint height,
uint num_levels, uint num_bits, gs_memory_t * mem);
void gx_ht_construct_spot_order(gx_ht_order *);
int gx_ht_construct_threshold_order(gx_ht_order *, const byte *);
void gx_ht_construct_bit(gx_ht_bit * bit, int width, int bit_num);
void gx_ht_construct_bits(gx_ht_order *);
struct gs_screen_enum_s {
gs_halftone halftone;
gx_ht_order order;
gs_matrix mat;
gs_matrix mat_inv;
int x, y;
int strip, shift;
gs_state *pgs;
};
#define private_st_gs_screen_enum() \
gs_private_st_composite(st_gs_screen_enum, gs_screen_enum,\
"gs_screen_enum", screen_enum_enum_ptrs, screen_enum_reloc_ptrs)
int gs_sethalftone_prepare(gs_state *, gs_halftone *,
gx_device_halftone *);
int gs_screen_order_alloc(gx_ht_order *, gs_memory_t *);
int gs_screen_order_init_memory(gx_ht_order *, const gs_state *,
gs_screen_halftone *, bool, gs_memory_t *);
#define gs_screen_order_init(porder, pgs, phsp, accurate)\
gs_screen_order_init_memory(porder, pgs, phsp, accurate, pgs->memory)
int gs_screen_enum_init_memory(gs_screen_enum *, const gx_ht_order *,
gs_state *, const gs_screen_halftone *,
gs_memory_t *);
#define gs_screen_enum_init(penum, porder, pgs, phsp)\
gs_screen_enum_init_memory(penum, porder, pgs, phsp, pgs->memory)
int gx_ht_process_screen_memory(gs_screen_enum * penum, gs_state * pgs,
gs_screen_halftone * phsp, bool accurate,
gs_memory_t * mem);
#define gx_ht_process_screen(penum, pgs, phsp, accurate)\
gx_ht_process_screen_memory(penum, pgs, phsp, accurate, pgs->memory)
struct gx_ht_cache_s {
byte *bits;
uint bits_size;
gx_ht_tile *ht_tiles;
uint num_tiles;
gx_ht_order order;
int num_cached;
int levels_per_tile;
int tiles_fit;
gx_bitmap_id base_id;
gx_ht_tile *(*render_ht)(gx_ht_cache *, int);
};
#define max_cached_tiles_HUGE 5000
#define max_ht_bits_HUGE 1000000
#define max_cached_tiles_LARGE 577
#define max_ht_bits_LARGE 100000
#define max_cached_tiles_SMALL 25
#define max_ht_bits_SMALL 1000
#define max_tile_bytes_LARGE 4096
#define max_tile_bytes_SMALL 512
#if arch_small_memory
#  define max_tile_cache_bytes max_tile_bytes_SMALL
#else
#  define max_tile_cache_bytes\
(gs_debug_c('.') ? max_tile_bytes_SMALL : max_tile_bytes_LARGE)
#endif
#define private_st_ht_tiles()	\
gs_private_st_composite(st_ht_tiles, gx_ht_tile, "ht tiles",\
ht_tiles_enum_ptrs, ht_tiles_reloc_ptrs)
#define private_st_ht_cache()	\
gs_private_st_ptrs_add2(st_ht_cache, gx_ht_cache, "ht cache",\
ht_cache_enum_ptrs, ht_cache_reloc_ptrs,\
st_ht_order, order, bits, ht_tiles)
#define frac_color_(f, maxv)\
(gx_color_value)(((f) * (0xffffL * 2) + maxv) / (maxv * 2))
extern const gx_color_value *const fc_color_quo[8];
#define fractional_color(f, maxv)\
((maxv) <= 7 ? fc_color_quo[maxv][f] : frac_color_(f, maxv))
uint gx_ht_cache_default_tiles(void);
uint gx_ht_cache_default_bits(void);
gx_ht_cache *gx_ht_alloc_cache(gs_memory_t *, uint, uint);
void gx_ht_free_cache(gs_memory_t *, gx_ht_cache *);
#define gx_ht_clear_cache(pcache)\
((pcache)->order.levels = 0, (pcache)->order.bit_data = 0,\
(pcache)->ht_tiles[0].tiles.data = 0)
void gx_ht_init_cache(const gs_memory_t *mem, gx_ht_cache *, const gx_ht_order *);
bool gx_check_tile_cache_current(const gs_imager_state * pis);
bool gx_check_tile_cache(const gs_imager_state *);
int gx_check_tile_size(const gs_imager_state * pis, int w, int y, int h,
gs_color_select_t select, int *ppx);
#define gx_render_ht(pcache, b_level)\
((pcache)->render_ht(pcache, b_level))
void gx_ht_order_release(gx_ht_order * porder, gs_memory_t * mem, bool free_cache);
int gx_imager_dev_ht_install(gs_imager_state * pis,
gx_device_halftone * pdht,
gs_halftone_type type,
const gx_device * dev);
int gx_ht_install(gs_state *, const gs_halftone *, gx_device_halftone *);
void gx_imager_set_effective_xfer(gs_imager_state * pis);
void gx_set_effective_transfer(gs_state * pgs);
int gs_color_name_component_number(gx_device * dev, const char * pname,
int name_size, int halftonetype);
int gs_cname_to_colorant_number(gs_state * pgs, byte * pname, uint name_size,
int halftonetype);
#endif