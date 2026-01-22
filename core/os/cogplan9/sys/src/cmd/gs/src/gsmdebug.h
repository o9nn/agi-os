#ifndef gsmdebug_INCLUDED
# define gsmdebug_INCLUDED
extern const byte
gs_alloc_fill_alloc,
gs_alloc_fill_block,
gs_alloc_fill_collected,
gs_alloc_fill_deleted,
gs_alloc_fill_free;
#define gs_alloc_debug gs_debug['@']
extern void gs_alloc_memset(void *, int , ulong);
#ifdef DEBUG
# define gs_alloc_fill(ptr, fill, len)\
BEGIN if ( gs_alloc_debug ) gs_alloc_memset(ptr, fill, (ulong)(len)); END
#else
# define gs_alloc_fill(ptr, fill, len)\
DO_NOTHING
#endif
#endif