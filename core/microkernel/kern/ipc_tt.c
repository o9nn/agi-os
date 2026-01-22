#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/mach_param.h>
#include <mach/mach_traps.h>
#include <mach/task_special_ports.h>
#include <mach/thread_special_ports.h>
#include <vm/vm_kern.h>
#include <kern/debug.h>
#include <kern/kalloc.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/ipc_kobject.h>
#include <kern/ipc_tt.h>
#include <kern/mach.server.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_table.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_object.h>
void
ipc_task_init(
task_t		task,
task_t		parent)
{
ipc_space_t space;
ipc_port_t kport;
kern_return_t kr;
int i;
kr = ipc_space_create(&space);
if (kr != KERN_SUCCESS)
panic("ipc_task_init");
kport = ipc_port_alloc_kernel();
if (kport == IP_NULL)
panic("ipc_task_init");
itk_lock_init(task);
task->itk_self = kport;
task->itk_sself = ipc_port_make_send(kport);
task->itk_space = space;
if (parent == TASK_NULL) {
task->itk_exception = IP_NULL;
task->itk_bootstrap = IP_NULL;
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++)
task->itk_registered[i] = IP_NULL;
} else {
itk_lock(parent);
assert(parent->itk_self != IP_NULL);
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++)
task->itk_registered[i] =
ipc_port_copy_send(parent->itk_registered[i]);
task->itk_exception =
ipc_port_copy_send(parent->itk_exception);
task->itk_bootstrap =
ipc_port_copy_send(parent->itk_bootstrap);
itk_unlock(parent);
}
}
void
ipc_task_enable(
task_t		task)
{
ipc_port_t kport;
itk_lock(task);
kport = task->itk_self;
if (kport != IP_NULL)
ipc_kobject_set(kport, (ipc_kobject_t) task, IKOT_TASK);
itk_unlock(task);
}
void
ipc_task_disable(
task_t		task)
{
ipc_port_t kport;
itk_lock(task);
kport = task->itk_self;
if (kport != IP_NULL)
ipc_kobject_set(kport, IKO_NULL, IKOT_NONE);
itk_unlock(task);
}
void
ipc_task_terminate(
task_t		task)
{
ipc_port_t kport;
int i;
itk_lock(task);
kport = task->itk_self;
if (kport == IP_NULL) {
itk_unlock(task);
return;
}
task->itk_self = IP_NULL;
itk_unlock(task);
if (IP_VALID(task->itk_sself))
ipc_port_release_send(task->itk_sself);
if (IP_VALID(task->itk_exception))
ipc_port_release_send(task->itk_exception);
if (IP_VALID(task->itk_bootstrap))
ipc_port_release_send(task->itk_bootstrap);
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++)
if (IP_VALID(task->itk_registered[i]))
ipc_port_release_send(task->itk_registered[i]);
ipc_space_destroy(task->itk_space);
ipc_port_dealloc_kernel(kport);
}
void
ipc_thread_init(thread_t thread)
{
ipc_port_t kport;
kport = ipc_port_alloc_kernel();
if (kport == IP_NULL)
panic("ipc_thread_init");
ipc_thread_links_init(thread);
ipc_kmsg_queue_init(&thread->ith_messages);
ith_lock_init(thread);
thread->ith_self = kport;
thread->ith_sself = ipc_port_make_send(kport);
thread->ith_exception = IP_NULL;
thread->ith_mig_reply = MACH_PORT_NULL;
thread->ith_rpc_reply = IP_NULL;
}
void
ipc_thread_enable(thread_t thread)
{
ipc_port_t kport;
ith_lock(thread);
kport = thread->ith_self;
if (kport != IP_NULL)
ipc_kobject_set(kport, (ipc_kobject_t) thread, IKOT_THREAD);
ith_unlock(thread);
}
void
ipc_thread_disable(thread_t thread)
{
ipc_port_t kport;
ith_lock(thread);
kport = thread->ith_self;
if (kport != IP_NULL)
ipc_kobject_set(kport, IKO_NULL, IKOT_NONE);
ith_unlock(thread);
}
void
ipc_thread_terminate(thread_t thread)
{
ipc_port_t kport;
ith_lock(thread);
kport = thread->ith_self;
if (kport == IP_NULL) {
ith_unlock(thread);
return;
}
thread->ith_self = IP_NULL;
ith_unlock(thread);
assert(ipc_kmsg_queue_empty(&thread->ith_messages));
if (IP_VALID(thread->ith_sself))
ipc_port_release_send(thread->ith_sself);
if (IP_VALID(thread->ith_exception))
ipc_port_release_send(thread->ith_exception);
ipc_port_dealloc_kernel(kport);
}
#if	0
ipc_port_t
retrieve_task_self(task)
task_t task;
{
ipc_port_t port;
assert(task != TASK_NULL);
itk_lock(task);
if (task->itk_self != IP_NULL)
port = ipc_port_copy_send(task->itk_sself);
else
port = IP_NULL;
itk_unlock(task);
return port;
}
ipc_port_t
retrieve_thread_self(thread)
thread_t thread;
{
ipc_port_t port;
assert(thread != ITH_NULL);
ith_lock(thread);
if (thread->ith_self != IP_NULL)
port = ipc_port_copy_send(thread->ith_sself);
else
port = IP_NULL;
ith_unlock(thread);
return port;
}
#endif
ipc_port_t
retrieve_task_self_fast(
task_t		task)
{
ipc_port_t port;
assert(task == current_task());
itk_lock(task);
assert(task->itk_self != IP_NULL);
if ((port = task->itk_sself) == task->itk_self) {
ip_lock(port);
assert(ip_active(port));
ip_reference(port);
port->ip_srights++;
ip_unlock(port);
} else
port = ipc_port_copy_send(port);
itk_unlock(task);
return port;
}
ipc_port_t
retrieve_thread_self_fast(thread_t thread)
{
ipc_port_t port;
assert(thread == current_thread());
ith_lock(thread);
assert(thread->ith_self != IP_NULL);
if ((port = thread->ith_sself) == thread->ith_self) {
ip_lock(port);
assert(ip_active(port));
ip_reference(port);
port->ip_srights++;
ip_unlock(port);
} else
port = ipc_port_copy_send(port);
ith_unlock(thread);
return port;
}
#if	0
ipc_port_t
retrieve_task_exception(task)
task_t task;
{
ipc_port_t port;
assert(task != TASK_NULL);
itk_lock(task);
if (task->itk_self != IP_NULL)
port = ipc_port_copy_send(task->itk_exception);
else
port = IP_NULL;
itk_unlock(task);
return port;
}
ipc_port_t
retrieve_thread_exception(thread)
thread_t thread;
{
ipc_port_t port;
assert(thread != ITH_NULL);
ith_lock(thread);
if (thread->ith_self != IP_NULL)
port = ipc_port_copy_send(thread->ith_exception);
else
port = IP_NULL;
ith_unlock(thread);
return port;
}
#endif
mach_port_name_t
mach_task_self(void)
{
task_t task = current_task();
ipc_port_t sright;
sright = retrieve_task_self_fast(task);
return ipc_port_copyout_send(sright, task->itk_space);
}
mach_port_name_t
mach_thread_self(void)
{
thread_t thread = current_thread();
task_t task = thread->task;
ipc_port_t sright;
sright = retrieve_thread_self_fast(thread);
return ipc_port_copyout_send(sright, task->itk_space);
}
mach_port_name_t
mach_reply_port(void)
{
ipc_port_t port;
mach_port_name_t name;
kern_return_t kr;
kr = ipc_port_alloc(current_task()->itk_space, &name, &port);
if (kr == KERN_SUCCESS)
ip_unlock(port);
else
name = MACH_PORT_NULL;
return name;
}
kern_return_t
task_get_special_port(
task_t		task,
int		which,
ipc_port_t	*portp)
{
ipc_port_t *whichp;
ipc_port_t port;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
switch (which) {
case TASK_KERNEL_PORT:
whichp = &task->itk_sself;
break;
case TASK_EXCEPTION_PORT:
whichp = &task->itk_exception;
break;
case TASK_BOOTSTRAP_PORT:
whichp = &task->itk_bootstrap;
break;
default:
return KERN_INVALID_ARGUMENT;
}
itk_lock(task);
if (task->itk_self == IP_NULL) {
itk_unlock(task);
return KERN_FAILURE;
}
port = ipc_port_copy_send(*whichp);
itk_unlock(task);
*portp = port;
return KERN_SUCCESS;
}
kern_return_t
task_set_special_port(
task_t			task,
int			which,
const ipc_port_t 	port)
{
ipc_port_t *whichp;
ipc_port_t old;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
switch (which) {
case TASK_KERNEL_PORT:
whichp = &task->itk_sself;
break;
case TASK_EXCEPTION_PORT:
whichp = &task->itk_exception;
break;
case TASK_BOOTSTRAP_PORT:
whichp = &task->itk_bootstrap;
break;
default:
return KERN_INVALID_ARGUMENT;
}
itk_lock(task);
if (task->itk_self == IP_NULL) {
itk_unlock(task);
return KERN_FAILURE;
}
old = *whichp;
*whichp = port;
itk_unlock(task);
if (IP_VALID(old))
ipc_port_release_send(old);
return KERN_SUCCESS;
}
kern_return_t
thread_get_special_port(
thread_t 	thread,
int 		which,
ipc_port_t 	*portp)
{
ipc_port_t *whichp;
ipc_port_t port;
if (thread == ITH_NULL)
return KERN_INVALID_ARGUMENT;
switch (which) {
case THREAD_KERNEL_PORT:
whichp = &thread->ith_sself;
break;
case THREAD_EXCEPTION_PORT:
whichp = &thread->ith_exception;
break;
default:
return KERN_INVALID_ARGUMENT;
}
ith_lock(thread);
if (thread->ith_self == IP_NULL) {
ith_unlock(thread);
return KERN_FAILURE;
}
port = ipc_port_copy_send(*whichp);
ith_unlock(thread);
*portp = port;
return KERN_SUCCESS;
}
kern_return_t
thread_set_special_port(
thread_t 	thread,
int 		which,
ipc_port_t 	port)
{
ipc_port_t *whichp;
ipc_port_t old;
if (thread == ITH_NULL)
return KERN_INVALID_ARGUMENT;
switch (which) {
case THREAD_KERNEL_PORT:
whichp = &thread->ith_sself;
break;
case THREAD_EXCEPTION_PORT:
whichp = &thread->ith_exception;
break;
default:
return KERN_INVALID_ARGUMENT;
}
ith_lock(thread);
if (thread->ith_self == IP_NULL) {
ith_unlock(thread);
return KERN_FAILURE;
}
old = *whichp;
*whichp = port;
ith_unlock(thread);
if (IP_VALID(old))
ipc_port_release_send(old);
return KERN_SUCCESS;
}
kern_return_t
mach_ports_register(
task_t			task,
mach_port_array_t	memory,
mach_msg_type_number_t	portsCnt)
{
ipc_port_t ports[TASK_PORT_REGISTER_MAX];
unsigned i;
if ((task == TASK_NULL) ||
(portsCnt > TASK_PORT_REGISTER_MAX))
return KERN_INVALID_ARGUMENT;
for (i = 0; i < portsCnt; i++)
ports[i] = (ipc_port_t)memory[i];
for (; i < TASK_PORT_REGISTER_MAX; i++)
ports[i] = IP_NULL;
itk_lock(task);
if (task->itk_self == IP_NULL) {
itk_unlock(task);
return KERN_INVALID_ARGUMENT;
}
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++) {
ipc_port_t old;
old = task->itk_registered[i];
task->itk_registered[i] = ports[i];
ports[i] = old;
}
itk_unlock(task);
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++)
if (IP_VALID(ports[i]))
ipc_port_release_send(ports[i]);
if (portsCnt != 0)
kfree((vm_offset_t) memory,
(vm_size_t) (portsCnt * sizeof(mach_port_t)));
return KERN_SUCCESS;
}
kern_return_t
mach_ports_lookup(
task_t 			task,
mach_port_t 		**portsp,
mach_msg_type_number_t 	*portsCnt)
{
vm_offset_t memory;
vm_size_t size;
ipc_port_t *ports;
int i;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
size = (vm_size_t) (TASK_PORT_REGISTER_MAX * sizeof(ipc_port_t));
memory = kalloc(size);
if (memory == 0)
return KERN_RESOURCE_SHORTAGE;
itk_lock(task);
if (task->itk_self == IP_NULL) {
itk_unlock(task);
kfree(memory, size);
return KERN_INVALID_ARGUMENT;
}
ports = (ipc_port_t *) memory;
for (i = 0; i < TASK_PORT_REGISTER_MAX; i++)
ports[i] = ipc_port_copy_send(task->itk_registered[i]);
itk_unlock(task);
*portsp = (mach_port_t *)ports;
*portsCnt = TASK_PORT_REGISTER_MAX;
return KERN_SUCCESS;
}
task_t
convert_port_to_task(
ipc_port_t	port)
{
task_t task = TASK_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_TASK)) {
task = (task_t) port->ip_kobject;
task_reference(task);
}
ip_unlock(port);
}
return task;
}
ipc_space_t
convert_port_to_space(
ipc_port_t	port)
{
ipc_space_t space = IS_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_TASK)) {
space = ((task_t) port->ip_kobject)->itk_space;
is_reference(space);
}
ip_unlock(port);
}
return space;
}
vm_map_t
convert_port_to_map(ipc_port_t port)
{
vm_map_t map = VM_MAP_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_TASK)) {
map = ((task_t) port->ip_kobject)->map;
vm_map_reference(map);
}
ip_unlock(port);
}
return map;
}
thread_t
convert_port_to_thread(ipc_port_t port)
{
thread_t thread = THREAD_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_THREAD)) {
thread = (thread_t) port->ip_kobject;
thread_reference(thread);
}
ip_unlock(port);
}
return thread;
}
ipc_port_t
convert_task_to_port(task_t task)
{
ipc_port_t port;
itk_lock(task);
if (task->itk_self != IP_NULL)
port = ipc_port_make_send(task->itk_self);
else
port = IP_NULL;
itk_unlock(task);
task_deallocate(task);
return port;
}
ipc_port_t
convert_thread_to_port(thread_t thread)
{
ipc_port_t port;
ith_lock(thread);
if (thread->ith_self != IP_NULL)
port = ipc_port_make_send(thread->ith_self);
else
port = IP_NULL;
ith_unlock(thread);
thread_deallocate(thread);
return port;
}
void
space_deallocate(ipc_space_t space)
{
if (space != IS_NULL)
is_release(space);
}