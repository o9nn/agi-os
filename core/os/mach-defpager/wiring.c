#include <mach.h>
#include <mach_init.h>
#include <mach/gnumach.h>
#include <mach/machine/vm_param.h>
#include "default_pager.h"
mach_port_t	this_task;
mach_port_t	priv_host_port = MACH_PORT_NULL;
void
wire_setup(mach_port_t	host_priv)
{
priv_host_port = host_priv;
this_task = mach_task_self();
}
void
wire_thread(void)
{
kern_return_t	kr;
if (priv_host_port == MACH_PORT_NULL)
return;
kr = thread_wire(priv_host_port,
mach_thread_self(),
TRUE);
if (kr != KERN_SUCCESS)
panic("wire_thread: %d", kr);
}