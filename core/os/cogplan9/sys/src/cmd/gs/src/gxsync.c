#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsmemory.h"
#include "gxsync.h"
gx_semaphore_t *
gx_semaphore_alloc(
gs_memory_t * memory
)
{
gx_semaphore_t *sema;
unsigned semaSizeof
= sizeof(*sema) - sizeof(sema->native) + gp_semaphore_sizeof();
if (gp_semaphore_open(0) == 0)
sema = (gx_semaphore_t *) gs_alloc_bytes(memory, semaSizeof,
"gx_semaphore (create)");
else
sema = (gx_semaphore_t *) gs_alloc_bytes_immovable(memory, semaSizeof,
"gx_semaphore (create)");
if (sema == 0)
return 0;
sema->memory = memory;
if (gp_semaphore_open(&sema->native) < 0) {
gs_free_object(memory, sema, "gx_semaphore (alloc)");
return 0;
}
return sema;
}
void
gx_semaphore_free(
gx_semaphore_t * sema
)
{
if (sema) {
gp_semaphore_close(&sema->native);
gs_free_object(sema->memory, sema, "gx_semaphore (free)");
}
}
#define gx_semaphore_wait(sema) gp_semaphore_wait(&(sema)->native)
#define gx_semaphore_signal(sema) gp_semaphore_signal(&(sema)->native)
gx_monitor_t *
gx_monitor_alloc(
gs_memory_t * memory
)
{
gx_monitor_t *mon;
unsigned monSizeof
= sizeof(*mon) - sizeof(mon->native) + gp_monitor_sizeof();
if (gp_monitor_open(0) == 0)
mon = (gx_monitor_t *) gs_alloc_bytes(memory, monSizeof,
"gx_monitor (create)");
else
mon = (gx_monitor_t *) gs_alloc_bytes_immovable(memory, monSizeof,
"gx_monitor (create)");
if (mon == 0)
return 0;
mon->memory = memory;
if (gp_monitor_open(&mon->native) < 0) {
gs_free_object(memory, mon, "gx_monitor (alloc)");
return 0;
}
return mon;
}
void
gx_monitor_free(
gx_monitor_t * mon
)
{
if (mon) {
gp_monitor_close(&mon->native);
gs_free_object(mon->memory, mon, "gx_monitor (free)");
}
}
#define gx_monitor_enter(sema) gp_monitor_enter(&(sema)->native)
#define gx_monitor_leave(sema) gp_monitor_leave(&(sema)->native)