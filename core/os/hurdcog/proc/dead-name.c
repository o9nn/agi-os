#include <hurd/ports.h>
#include "proc.h"
void
ports_dead_name (void *notify, mach_port_t dead_name)
{
struct proc *p;
check_dead_execdata_notify (dead_name);
p = task_find_nocreate (dead_name);
if (p)
process_has_exited (p);
ports_interrupt_notified_rpcs (notify, dead_name, MACH_NOTIFY_DEAD_NAME);
}