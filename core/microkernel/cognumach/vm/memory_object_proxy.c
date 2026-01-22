#include <mach/port.h>
#include <mach/kern_return.h>
#include <mach/notify.h>
#include <mach/vm_prot.h>
#include <kern/printf.h>
#include <kern/slab.h>
#include <kern/mach4.server.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <vm/memory_object_proxy.h>
static struct kmem_cache memory_object_proxy_cache;
struct memory_object_proxy
{
struct ipc_port *port;
ipc_port_t object;
ipc_port_t notify;
vm_prot_t max_protection;
vm_offset_t start;
vm_offset_t len;
};
typedef struct memory_object_proxy *memory_object_proxy_t;
void
memory_object_proxy_init (void)
{
kmem_cache_init (&memory_object_proxy_cache, "memory_object_proxy",
sizeof (struct memory_object_proxy), 0, NULL, 0);
}
static memory_object_proxy_t
memory_object_proxy_port_lookup (ipc_port_t port)
{
memory_object_proxy_t proxy;
if (!IP_VALID(port))
return 0;
ip_lock (port);
if (ip_active (port) && (ip_kotype (port) == IKOT_PAGER_PROXY))
proxy = (memory_object_proxy_t) port->ip_kobject;
else
proxy = 0;
ip_unlock (port);
return proxy;
}
boolean_t
memory_object_proxy_notify (mach_msg_header_t *msg)
{
if (msg->msgh_id == MACH_NOTIFY_NO_SENDERS)
{
memory_object_proxy_t proxy;
mach_no_senders_notification_t *ns;
ns = (mach_no_senders_notification_t *) msg;
proxy = (memory_object_proxy_t)
((ipc_port_t) ns->not_header.msgh_remote_port)->ip_kobject;
if (!proxy)
return FALSE;
if ((ipc_port_t) ns->not_header.msgh_remote_port != proxy->notify)
return FALSE;
ipc_port_release_send (proxy->object);
ipc_kobject_set (proxy->port, IKO_NULL, IKOT_NONE);
ipc_port_dealloc_kernel (proxy->port);
ipc_kobject_set (proxy->notify, IKO_NULL, IKOT_NONE);
ipc_port_dealloc_kernel (proxy->notify);
kmem_cache_free (&memory_object_proxy_cache, (vm_offset_t) proxy);
return TRUE;
}
printf ("memory_object_proxy_notify: strange notification %d\n",
msg->msgh_id);
return FALSE;
}
kern_return_t
memory_object_create_proxy (ipc_space_t space, vm_prot_t max_protection,
ipc_port_t *object, natural_t object_count,
rpc_vm_offset_t *offset, natural_t offset_count,
rpc_vm_offset_t *start, natural_t start_count,
rpc_vm_size_t *len, natural_t len_count,
ipc_port_t *port)
{
memory_object_proxy_t proxy;
ipc_port_t notify;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (offset_count != object_count || start_count != object_count
|| len_count != object_count)
return KERN_INVALID_ARGUMENT;
if (object_count != 1)
return KERN_INVALID_ARGUMENT;
if (!IP_VALID(object[0]))
return KERN_INVALID_NAME;
if (offset[0] != 0)
return KERN_INVALID_ARGUMENT;
if (start[0] + len[0] < start[0])
return KERN_INVALID_ARGUMENT;
proxy = (memory_object_proxy_t) kmem_cache_alloc (&memory_object_proxy_cache);
proxy->port = ipc_port_alloc_kernel ();
if (proxy->port == IP_NULL)
{
kmem_cache_free (&memory_object_proxy_cache, (vm_offset_t) proxy);
return KERN_RESOURCE_SHORTAGE;
}
ipc_kobject_set (proxy->port, (ipc_kobject_t) proxy, IKOT_PAGER_PROXY);
proxy->notify = ipc_port_alloc_kernel ();
ipc_kobject_set (proxy->notify, (ipc_kobject_t) proxy, IKOT_PAGER_PROXY);
notify = ipc_port_make_sonce (proxy->notify);
ip_lock (proxy->port);
ipc_port_nsrequest (proxy->port, 1, notify, &notify);
assert (notify == IP_NULL);
proxy->object = object[0];
proxy->max_protection = max_protection;
proxy->start = start[0];
proxy->len = len[0];
*port = ipc_port_make_send (proxy->port);
return KERN_SUCCESS;
}
kern_return_t
memory_object_proxy_lookup (ipc_port_t port, ipc_port_t *object,
vm_prot_t *max_protection, vm_offset_t *start,
vm_offset_t *len)
{
memory_object_proxy_t proxy;
proxy = memory_object_proxy_port_lookup (port);
if (!proxy)
return KERN_INVALID_ARGUMENT;
*max_protection = proxy->max_protection;
*start = 0;
*len = (vm_offset_t) ~0;
do
{
*object = proxy->object;
if (proxy->len <= *start)
*len = 0;
else
*len = MIN(*len, proxy->len - *start);
*start += proxy->start;
}
while ((proxy = memory_object_proxy_port_lookup (proxy->object)));
return KERN_SUCCESS;
}