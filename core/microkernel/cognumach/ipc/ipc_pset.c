#include <kern/printf.h>
#include <mach/port.h>
#include <mach/kern_return.h>
#include <mach/message.h>
#include <ipc/ipc_mqueue.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_pset.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_space.h>
#if	MACH_KDB
#include <ddb/db_output.h>
#include <ipc/ipc_print.h>
#endif
kern_return_t
ipc_pset_alloc(
ipc_space_t	space,
mach_port_name_t	*namep,
ipc_pset_t	*psetp)
{
ipc_pset_t pset;
mach_port_name_t name;
kern_return_t kr;
kr = ipc_object_alloc(space, IOT_PORT_SET,
MACH_PORT_TYPE_PORT_SET, 0,
&name, (ipc_object_t *) &pset);
if (kr != KERN_SUCCESS)
return kr;
ipc_target_init(&pset->ips_target, name);
*namep = name;
*psetp = pset;
return KERN_SUCCESS;
}
kern_return_t
ipc_pset_alloc_name(
ipc_space_t	space,
mach_port_name_t	name,
ipc_pset_t	*psetp)
{
ipc_pset_t pset;
kern_return_t kr;
kr = ipc_object_alloc_name(space, IOT_PORT_SET,
MACH_PORT_TYPE_PORT_SET, 0,
name, (ipc_object_t *) &pset);
if (kr != KERN_SUCCESS)
return kr;
ipc_target_init(&pset->ips_target, name);
*psetp = pset;
return KERN_SUCCESS;
}
void
ipc_pset_add(
ipc_pset_t	pset,
ipc_port_t	port)
{
assert(ips_active(pset));
assert(ip_active(port));
assert(port->ip_pset == IPS_NULL);
port->ip_pset = pset;
port->ip_cur_target = &pset->ips_target;
ips_reference(pset);
imq_lock(&port->ip_messages);
imq_lock(&pset->ips_messages);
ipc_mqueue_move(&pset->ips_messages, &port->ip_messages, port);
imq_unlock(&pset->ips_messages);
assert(ipc_kmsg_queue_empty(&port->ip_messages.imq_messages));
ipc_mqueue_changed(&port->ip_messages, MACH_RCV_PORT_CHANGED);
assert(ipc_thread_queue_empty(&port->ip_messages.imq_threads));
imq_unlock(&port->ip_messages);
}
void
ipc_pset_remove(
ipc_pset_t	pset,
ipc_port_t	port)
{
assert(ip_active(port));
assert(port->ip_pset == pset);
port->ip_pset = IPS_NULL;
port->ip_cur_target = &port->ip_target;
ips_release(pset);
imq_lock(&port->ip_messages);
imq_lock(&pset->ips_messages);
ipc_mqueue_move(&port->ip_messages, &pset->ips_messages, port);
imq_unlock(&pset->ips_messages);
imq_unlock(&port->ip_messages);
}
kern_return_t
ipc_pset_move(
ipc_space_t	space,
ipc_port_t	port,
ipc_pset_t	nset)
{
ipc_pset_t oset;
ip_lock(port);
assert(ip_active(port));
oset = port->ip_pset;
if (oset == nset) {
is_read_unlock(space);
} else if (oset == IPS_NULL) {
ips_lock(nset);
assert(ips_active(nset));
is_read_unlock(space);
ipc_pset_add(nset, port);
ips_unlock(nset);
} else if (nset == IPS_NULL) {
is_read_unlock(space);
ips_lock(oset);
ipc_pset_remove(oset, port);
if (ips_active(oset))
ips_unlock(oset);
else {
ips_check_unlock(oset);
oset = IPS_NULL;
}
} else {
if (oset < nset) {
ips_lock(oset);
ips_lock(nset);
} else {
ips_lock(nset);
ips_lock(oset);
}
is_read_unlock(space);
assert(ips_active(nset));
ipc_pset_remove(oset, port);
ipc_pset_add(nset, port);
ips_unlock(nset);
ips_check_unlock(oset);
}
ip_unlock(port);
return (((nset == IPS_NULL) && (oset == IPS_NULL)) ?
KERN_NOT_IN_SET : KERN_SUCCESS);
}
void
ipc_pset_destroy(
ipc_pset_t	pset)
{
assert(ips_active(pset));
pset->ips_object.io_bits &= ~IO_BITS_ACTIVE;
imq_lock(&pset->ips_messages);
ipc_mqueue_changed(&pset->ips_messages, MACH_RCV_PORT_DIED);
imq_unlock(&pset->ips_messages);
ipc_target_terminate(&pset->ips_target);
ips_release(pset);
ips_check_unlock(pset);
}
#if	MACH_KDB
#define	printf	kdbprintf
void
ipc_pset_print(
const ipc_pset_t pset)
{
printf("pset 0x%x\n", pset);
indent += 2;
ipc_object_print(&pset->ips_object);
iprintf("local_name = 0x%x\n", pset->ips_local_name);
iprintf("kmsgs = 0x%x", pset->ips_messages.imq_messages.ikmq_base);
printf(",rcvrs = 0x%x\n", pset->ips_messages.imq_threads.ithq_base);
indent -= 2;
}
#endif