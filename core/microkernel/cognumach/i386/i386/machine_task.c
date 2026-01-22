#include <kern/lock.h>
#include <mach/mach_types.h>
#include <kern/slab.h>
#include <kern/task.h>
#include <machine/task.h>
#include <machine/io_perm.h>
struct kmem_cache machine_task_iopb_cache;
void
machine_task_module_init (void)
{
kmem_cache_init (&machine_task_iopb_cache, "i386_task_iopb", IOPB_BYTES, 0,
NULL, 0);
}
void
machine_task_init (task_t task)
{
task->machine.iopb_size = 0;
task->machine.iopb = 0;
simple_lock_init (&task->machine.iopb_lock);
}
void
machine_task_terminate (const task_t task)
{
if (task->machine.iopb)
kmem_cache_free (&machine_task_iopb_cache,
(vm_offset_t) task->machine.iopb);
}
void
machine_task_collect (task_t task)
{
simple_lock (&task->machine.iopb_lock);
if (task->machine.iopb_size == 0 && task->machine.iopb)
{
kmem_cache_free (&machine_task_iopb_cache,
(vm_offset_t) task->machine.iopb);
task->machine.iopb = 0;
}
simple_unlock (&task->machine.iopb_lock);
}