#if !defined(gsmemlok_INCLUDED)
#  define gsmemlok_INCLUDED
#include "gsmemory.h"
#include "gxsync.h"
typedef struct gs_memory_locked_s {
gs_memory_common;
gs_memory_t *target;
gx_monitor_t *monitor;
} gs_memory_locked_t;
int gs_memory_locked_init(
gs_memory_locked_t * lmem,
gs_memory_t * target
);
void gs_memory_locked_release(gs_memory_locked_t *lmem);
gs_memory_t * gs_memory_locked_target(const gs_memory_locked_t *lmem);
#endif