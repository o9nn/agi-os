#if !defined(gxsync_INCLUDED)
# define gxsync_INCLUDED
#include "gpsync.h"
#include "gsmemory.h"
typedef struct gx_semaphore_s {
gs_memory_t *memory;
gp_semaphore native;
} gx_semaphore_t;
gx_semaphore_t *
gx_semaphore_alloc(
gs_memory_t * memory
);
void
gx_semaphore_free(
gx_semaphore_t * sema
);
#define gx_semaphore_wait(sema) gp_semaphore_wait(&(sema)->native)
#define gx_semaphore_signal(sema) gp_semaphore_signal(&(sema)->native)
typedef struct gx_monitor_s {
gs_memory_t *memory;
gp_monitor native;
} gx_monitor_t;
gx_monitor_t *
gx_monitor_alloc(
gs_memory_t * memory
);
void
gx_monitor_free(
gx_monitor_t * mon
);
#define gx_monitor_enter(sema) gp_monitor_enter(&(sema)->native)
#define gx_monitor_leave(sema) gp_monitor_leave(&(sema)->native)
#endif