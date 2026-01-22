#ifndef gxbcache_INCLUDED
#  define gxbcache_INCLUDED
#include "gxbitmap.h"
typedef struct gx_cached_bits_head_s {
uint size;
uint depth;
} gx_cached_bits_head;
#define cb_head_is_free(cbh) ((cbh)->depth == 0)
#define cb_head_set_free(cbh) ((cbh)->depth = 0)
#define gx_cached_bits_common\
gx_cached_bits_head head;	\
\
\
ushort width, height, shift;\
ushort raster;\
gx_bitmap_id id
#define cb_depth head.depth
#define cb_raster raster
typedef struct gx_cached_bits_s {
gx_cached_bits_common;
} gx_cached_bits;
#define cb_is_free(cb) cb_head_is_free(&(cb)->head)
#define align_cached_bits_mod\
(max(align_bitmap_mod, max(arch_align_ptr_mod, arch_align_long_mod)))
typedef struct gx_bits_cache_chunk_s gx_bits_cache_chunk;
struct gx_bits_cache_chunk_s {
gx_bits_cache_chunk *next;
byte *data;
uint size;
uint allocated;
};
#define gx_bits_cache_common\
gx_bits_cache_chunk *chunks;	\
uint cnext;			\
\
uint bsize;			\
uint csize
typedef struct gx_bits_cache_s {
gx_bits_cache_common;
} gx_bits_cache;
void gx_bits_cache_init(gx_bits_cache *, gx_bits_cache_chunk *);
void gx_bits_cache_chunk_init(gx_bits_cache_chunk *, byte *, uint);
int gx_bits_cache_alloc(gx_bits_cache *, ulong, gx_cached_bits_head **);
void gx_bits_cache_shorten(gx_bits_cache *, gx_cached_bits_head *,
uint, gx_bits_cache_chunk *);
void gx_bits_cache_free(gx_bits_cache *, gx_cached_bits_head *,
gx_bits_cache_chunk *);
#endif