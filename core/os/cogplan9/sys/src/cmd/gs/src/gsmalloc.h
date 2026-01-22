#ifndef gsmalloc_INCLUDED
# define gsmalloc_INCLUDED
typedef struct gs_malloc_block_s gs_malloc_block_t;
typedef struct gs_malloc_memory_s {
gs_memory_common;
gs_malloc_block_t *allocated;
long limit;
long used;
long max_used;
} gs_malloc_memory_t;
gs_malloc_memory_t *gs_malloc_memory_init(void);
#define gs_malloc_memory_release(mem)\
gs_memory_free_all((gs_memory_t *)mem, FREE_ALL_EVERYTHING,\
"gs_malloc_memory_release")
gs_memory_t * gs_malloc_init(const gs_memory_t *parent);
void gs_malloc_release(gs_memory_t *mem);
#define gs_malloc(mem, nelts, esize, cname)\
(void *)gs_alloc_byte_array(mem->non_gc_memory, nelts, esize, cname)
#define gs_free(mem, data, nelts, esize, cname)\
gs_free_object(mem->non_gc_memory, data, cname)
int gs_malloc_wrap(gs_memory_t **wrapped, gs_malloc_memory_t *contents);
gs_malloc_memory_t *gs_malloc_wrapped_contents(gs_memory_t *wrapped);
gs_malloc_memory_t *gs_malloc_unwrap(gs_memory_t *wrapped);
#endif