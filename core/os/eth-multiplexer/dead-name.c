#include <hurd/ports.h>
#include "vdev.h"
void
ports_dead_name (void *notify, mach_port_t dead_name)
{
debug ("ports_dead_name is called\n");
remove_dead_port_from_dev (dead_name);
ports_interrupt_notified_rpcs (notify, dead_name, MACH_NOTIFY_DEAD_NAME);
}