#include "ports.h"
#include <mach/notify.h>
void __attribute__ ((weak))
ports_dead_name (void *notify, mach_port_t dead_name)
{
ports_interrupt_notified_rpcs (notify, dead_name, MACH_NOTIFY_DEAD_NAME);
}