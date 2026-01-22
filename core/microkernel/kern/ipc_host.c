#include <mach/message.h>
#include <kern/debug.h>
#include <kern/host.h>
#include <kern/mach_host.server.h>
#include <kern/processor.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/ipc_host.h>
#include <kern/ipc_kobject.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <mach/mach_traps.h>
#include <machine/spl.h>
void ipc_host_init(void)
{
ipc_port_t port;
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("ipc_host_init");
ipc_kobject_set(port, (ipc_kobject_t) &realhost, IKOT_HOST);
realhost.host_self = port;
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("ipc_host_init");
ipc_kobject_set(port, (ipc_kobject_t) &realhost, IKOT_HOST_PRIV);
realhost.host_priv_self = port;
ipc_pset_init(&default_pset);
ipc_pset_enable(&default_pset);
ipc_processor_init(master_processor);
}
mach_port_name_t
mach_host_self(void)
{
ipc_port_t sright;
sright = ipc_port_make_send(realhost.host_self);
return ipc_port_copyout_send(sright, current_space());
}
void
ipc_processor_init(
processor_t processor)
{
ipc_port_t port;
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("ipc_processor_init");
processor->processor_self = port;
ipc_kobject_set(port, (ipc_kobject_t) processor, IKOT_PROCESSOR);
}
void
ipc_pset_init(
processor_set_t pset)
{
ipc_port_t port;
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("ipc_pset_init");
pset->pset_self = port;
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("ipc_pset_init");
pset->pset_name_self = port;
}
void
ipc_pset_enable(
processor_set_t pset)
{
pset_lock(pset);
if (pset->active) {
ipc_kobject_set(pset->pset_self,
(ipc_kobject_t) pset, IKOT_PSET);
ipc_kobject_set(pset->pset_name_self,
(ipc_kobject_t) pset, IKOT_PSET_NAME);
pset_ref_lock(pset);
pset->ref_count += 2;
pset_ref_unlock(pset);
}
pset_unlock(pset);
}
void
ipc_pset_disable(
processor_set_t pset)
{
ipc_kobject_set(pset->pset_self, IKO_NULL, IKOT_NONE);
ipc_kobject_set(pset->pset_name_self, IKO_NULL, IKOT_NONE);
pset->ref_count -= 2;
}
void
ipc_pset_terminate(
processor_set_t pset)
{
ipc_port_dealloc_kernel(pset->pset_self);
ipc_port_dealloc_kernel(pset->pset_name_self);
}
kern_return_t
processor_set_default(
const host_t host,
processor_set_t *pset)
{
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
*pset = &default_pset;
pset_reference(*pset);
return KERN_SUCCESS;
}
host_t
convert_port_to_host(
ipc_port_t port)
{
host_t host = HOST_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
((ip_kotype(port) == IKOT_HOST) ||
(ip_kotype(port) == IKOT_HOST_PRIV)))
host = (host_t) port->ip_kobject;
ip_unlock(port);
}
return host;
}
host_t
convert_port_to_host_priv(
ipc_port_t port)
{
host_t host = HOST_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_HOST_PRIV))
host = (host_t) port->ip_kobject;
ip_unlock(port);
}
return host;
}
processor_t
convert_port_to_processor(
ipc_port_t port)
{
processor_t processor = PROCESSOR_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_PROCESSOR))
processor = (processor_t) port->ip_kobject;
ip_unlock(port);
}
return processor;
}
processor_set_t
convert_port_to_pset(
ipc_port_t port)
{
processor_set_t pset = PROCESSOR_SET_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_PSET)) {
pset = (processor_set_t) port->ip_kobject;
pset_reference(pset);
}
ip_unlock(port);
}
return pset;
}
processor_set_t
convert_port_to_pset_name(
ipc_port_t port)
{
processor_set_t pset = PROCESSOR_SET_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
((ip_kotype(port) == IKOT_PSET) ||
(ip_kotype(port) == IKOT_PSET_NAME))) {
pset = (processor_set_t) port->ip_kobject;
pset_reference(pset);
}
ip_unlock(port);
}
return pset;
}
ipc_port_t
convert_host_to_port(
host_t host)
{
ipc_port_t port;
port = ipc_port_make_send(host->host_self);
return port;
}
ipc_port_t
convert_processor_to_port(processor_t processor)
{
ipc_port_t port;
port = ipc_port_make_send(processor->processor_self);
return port;
}
ipc_port_t
convert_pset_to_port(
processor_set_t pset)
{
ipc_port_t port;
pset_lock(pset);
if (pset->active)
port = ipc_port_make_send(pset->pset_self);
else
port = IP_NULL;
pset_unlock(pset);
pset_deallocate(pset);
return port;
}
ipc_port_t
convert_pset_name_to_port(
processor_set_t pset)
{
ipc_port_t port;
pset_lock(pset);
if (pset->active)
port = ipc_port_make_send(pset->pset_name_self);
else
port = IP_NULL;
pset_unlock(pset);
pset_deallocate(pset);
return port;
}