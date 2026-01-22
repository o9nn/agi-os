#ifndef szlibxx_INCLUDED
# define szlibxx_INCLUDED
#include "szlibx.h"
#include "zlib.h"
typedef struct zlib_block_s zlib_block_t;
struct zlib_block_s {
void *data;
zlib_block_t *next;
zlib_block_t *prev;
};
#define private_st_zlib_block() \
gs_private_st_ptrs3(st_zlib_block, zlib_block_t, "zlib_block_t",\
zlib_block_enum_ptrs, zlib_block_reloc_ptrs, next, prev, data)
struct zlib_dynamic_state_s {
gs_memory_t *memory;
zlib_block_t *blocks;
z_stream zstate;
} ;
#define private_st_zlib_dynamic_state() \
gs_private_st_ptrs1(st_zlib_dynamic_state, zlib_dynamic_state_t,\
"zlib_dynamic_state_t", zlib_dynamic_enum_ptrs, zlib_dynamic_reloc_ptrs,\
blocks)
void *s_zlib_alloc(void *mem, uint items, uint size);
void s_zlib_free(void *mem, void *address);
int s_zlib_alloc_dynamic_state(stream_zlib_state *ss);
void s_zlib_free_dynamic_state(stream_zlib_state *ss);
#endif