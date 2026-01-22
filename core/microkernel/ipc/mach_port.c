#include <kern/debug.h>
#include <kern/printf.h>
#include <mach/port.h>
#include <mach/kern_return.h>
#include <mach/notify.h>
#include <mach/mach_param.h>
#include <mach/vm_param.h>
#include <mach/vm_prot.h>
#ifdef MIGRATING_THREADS
#include <kern/task.h>
#include <kern/act.h>
#endif
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <vm/vm_user.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_notify.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_pset.h>
#include <ipc/ipc_right.h>
#include <ipc/mach_port.h>
#include <ipc/mach_port.server.h>
static void
mach_port_names_helper(
ipc_port_timestamp_t	timestamp,
ipc_entry_t		entry,
mach_port_name_t	name,
mach_port_name_t	*names,
mach_port_type_t	*types,
ipc_entry_num_t		*actualp)
{
ipc_entry_bits_t bits = entry->ie_bits;
ipc_port_request_index_t request = entry->ie_request;
mach_port_type_t type;
ipc_entry_num_t actual;
if (bits & MACH_PORT_TYPE_SEND_RIGHTS) {
ipc_port_t port;
boolean_t died;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
died = (!ip_active(port) &&
IP_TIMESTAMP_ORDER(port->ip_timestamp, timestamp));
ip_unlock(port);
if (died) {
bits &= ~(IE_BITS_TYPE_MASK|IE_BITS_MAREQUEST);
bits |= MACH_PORT_TYPE_DEAD_NAME;
if (request != 0)
bits++;
request = 0;
}
}
type = IE_BITS_TYPE(bits);
if (request != 0)
type |= MACH_PORT_TYPE_DNREQUEST;
if (bits & IE_BITS_MAREQUEST)
type |= MACH_PORT_TYPE_MAREQUEST;
actual = *actualp;
names[actual] = name;
types[actual] = type;
*actualp = actual+1;
}
kern_return_t
mach_port_names(
ipc_space_t		space,
mach_port_name_t	**namesp,
mach_msg_type_number_t	*namesCnt,
mach_port_type_t	**typesp,
mach_msg_type_number_t	*typesCnt)
{
ipc_entry_num_t actual;
ipc_port_timestamp_t timestamp;
mach_port_name_t *names;
mach_port_type_t *types;
kern_return_t kr;
vm_size_t size;
vm_offset_t addr1;
vm_offset_t addr2;
vm_map_copy_t memory1;
vm_map_copy_t memory2;
ipc_entry_num_t bound;
assert_static(sizeof(mach_port_name_t) == sizeof(mach_port_type_t));
if (space == IS_NULL)
return KERN_INVALID_TASK;
size = 0;
for (;;) {
vm_size_t size_needed;
is_read_lock(space);
if (!space->is_active) {
is_read_unlock(space);
if (size != 0) {
kmem_free(ipc_kernel_map, addr1, size);
kmem_free(ipc_kernel_map, addr2, size);
}
return KERN_INVALID_TASK;
}
bound = space->is_size;
size_needed = round_page(bound * sizeof(mach_port_name_t));
if (size_needed <= size)
break;
is_read_unlock(space);
if (size != 0) {
kmem_free(ipc_kernel_map, addr1, size);
kmem_free(ipc_kernel_map, addr2, size);
}
size = size_needed;
kr = vm_allocate(ipc_kernel_map, &addr1, size, TRUE);
if (kr != KERN_SUCCESS) {
printf_once("no more room in ipc_kernel_map\n");
return KERN_RESOURCE_SHORTAGE;
}
kr = vm_allocate(ipc_kernel_map, &addr2, size, TRUE);
if (kr != KERN_SUCCESS) {
printf_once("no more room in ipc_kernel_map\n");
kmem_free(ipc_kernel_map, addr1, size);
return KERN_RESOURCE_SHORTAGE;
}
kr = vm_map_pageable(ipc_kernel_map, addr1, addr1 + size,
VM_PROT_READ|VM_PROT_WRITE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
kr = vm_map_pageable(ipc_kernel_map, addr2, addr2 + size,
VM_PROT_READ|VM_PROT_WRITE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
}
names = (mach_port_name_t *) addr1;
types = (mach_port_type_t *) addr2;
actual = 0;
timestamp = ipc_port_timestamp();
ipc_entry_t entry;
struct rdxtree_iter iter;
rdxtree_for_each(&space->is_map, &iter, entry) {
ipc_entry_bits_t bits = entry->ie_bits;
if (IE_BITS_TYPE(bits) != MACH_PORT_TYPE_NONE) {
mach_port_names_helper(timestamp, entry, entry->ie_name,
names, types, &actual);
}
}
assert(actual < bound);
is_read_unlock(space);
if (actual == 0) {
memory1 = VM_MAP_COPY_NULL;
memory2 = VM_MAP_COPY_NULL;
if (size != 0) {
kmem_free(ipc_kernel_map, addr1, size);
kmem_free(ipc_kernel_map, addr2, size);
}
} else {
vm_size_t size_used;
size_used = round_page(actual * sizeof(mach_port_name_t));
kr = vm_map_pageable(ipc_kernel_map,
addr1, addr1 + size_used,
VM_PROT_NONE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
kr = vm_map_pageable(ipc_kernel_map,
addr2, addr2 + size_used,
VM_PROT_NONE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
kr = vm_map_copyin(ipc_kernel_map, addr1, size_used,
TRUE, &memory1);
assert(kr == KERN_SUCCESS);
kr = vm_map_copyin(ipc_kernel_map, addr2, size_used,
TRUE, &memory2);
assert(kr == KERN_SUCCESS);
if (size_used != size) {
kmem_free(ipc_kernel_map,
addr1 + size_used, size - size_used);
kmem_free(ipc_kernel_map,
addr2 + size_used, size - size_used);
}
}
*namesp = (mach_port_name_t *) memory1;
*namesCnt = actual;
*typesp = (mach_port_type_t *) memory2;
*typesCnt = actual;
return KERN_SUCCESS;
}
kern_return_t
mach_port_type(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_type_t	*typep)
{
mach_port_urefs_t urefs;
ipc_entry_t entry;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS)
return kr;
kr = ipc_right_info(space, name, entry, typep, &urefs);
if (kr == KERN_SUCCESS)
is_write_unlock(space);
return kr;
}
kern_return_t
mach_port_rename(
ipc_space_t		space,
mach_port_name_t	oname,
mach_port_name_t	nname)
{
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (!MACH_PORT_NAME_VALID(nname))
return KERN_INVALID_VALUE;
return ipc_object_rename(space, oname, nname);
}
kern_return_t
mach_port_allocate_name(
ipc_space_t 		space,
mach_port_right_t 	right,
mach_port_name_t 	name)
{
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (!MACH_PORT_NAME_VALID(name))
return KERN_INVALID_VALUE;
switch (right) {
case MACH_PORT_RIGHT_RECEIVE: {
ipc_port_t port;
kr = ipc_port_alloc_name(space, name, &port);
if (kr == KERN_SUCCESS)
ip_unlock(port);
break;
}
case MACH_PORT_RIGHT_PORT_SET: {
ipc_pset_t pset;
kr = ipc_pset_alloc_name(space, name, &pset);
if (kr == KERN_SUCCESS)
ips_unlock(pset);
break;
}
case MACH_PORT_RIGHT_DEAD_NAME:
kr = ipc_object_alloc_dead_name(space, name);
break;
default:
kr = KERN_INVALID_VALUE;
break;
}
return kr;
}
kern_return_t
mach_port_allocate(
ipc_space_t 		space,
mach_port_right_t 	right,
mach_port_name_t 	*namep)
{
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
switch (right) {
case MACH_PORT_RIGHT_RECEIVE: {
ipc_port_t port;
kr = ipc_port_alloc(space, namep, &port);
if (kr == KERN_SUCCESS)
ip_unlock(port);
break;
}
case MACH_PORT_RIGHT_PORT_SET: {
ipc_pset_t pset;
kr = ipc_pset_alloc(space, namep, &pset);
if (kr == KERN_SUCCESS)
ips_unlock(pset);
break;
}
case MACH_PORT_RIGHT_DEAD_NAME:
kr = ipc_object_alloc_dead(space, namep);
break;
default:
kr = KERN_INVALID_VALUE;
break;
}
return (kr);
}
volatile boolean_t mach_port_deallocate_debug = FALSE;
kern_return_t
mach_port_destroy(
ipc_space_t		space,
mach_port_name_t	name)
{
ipc_entry_t entry;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS) {
if (MACH_PORT_NAME_VALID (name) && space == current_space()) {
printf("task %.*s destroying a bogus port %lu, most probably a bug.\n", (int) sizeof current_task()->name, current_task()->name, (unsigned long) name);
if (mach_port_deallocate_debug)
SoftDebugger("mach_port_deallocate");
}
return kr;
}
kr = ipc_right_destroy(space, name, entry);
return kr;
}
kern_return_t
mach_port_deallocate(
ipc_space_t		space,
mach_port_name_t	name)
{
ipc_entry_t entry;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS) {
if (MACH_PORT_NAME_VALID (name) && space == current_space()) {
printf("task %.*s deallocating a bogus port %lu, most probably a bug.\n", (int) sizeof current_task()->name, current_task()->name, (unsigned long) name);
if (mach_port_deallocate_debug)
SoftDebugger("mach_port_deallocate");
}
return kr;
}
kr = ipc_right_dealloc(space, name, entry);
return kr;
}
kern_return_t
mach_port_get_refs(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_right_t	right,
mach_port_urefs_t	*urefsp)
{
mach_port_type_t type;
mach_port_urefs_t urefs;
ipc_entry_t entry;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (right >= MACH_PORT_RIGHT_NUMBER)
return KERN_INVALID_VALUE;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS)
return kr;
kr = ipc_right_info(space, name, entry, &type, &urefs);
if (kr != KERN_SUCCESS)
return kr;
is_write_unlock(space);
if (type & MACH_PORT_TYPE(right))
switch (right) {
case MACH_PORT_RIGHT_SEND_ONCE:
assert(urefs == 1);
case MACH_PORT_RIGHT_PORT_SET:
case MACH_PORT_RIGHT_RECEIVE:
*urefsp = 1;
break;
case MACH_PORT_RIGHT_DEAD_NAME:
case MACH_PORT_RIGHT_SEND:
assert(urefs > 0);
*urefsp = urefs;
break;
default:
panic("mach_port_get_refs: strange rights");
}
else
*urefsp = 0;
return kr;
}
kern_return_t
mach_port_mod_refs(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_right_t	right,
mach_port_delta_t	delta)
{
ipc_entry_t entry;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (right >= MACH_PORT_RIGHT_NUMBER)
return KERN_INVALID_VALUE;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS) {
if (MACH_PORT_NAME_VALID (name) && space == current_space()) {
printf("task %.*s %screasing a bogus port "
"%u by %d, most probably a bug.\n",
(int) (sizeof current_task()->name),
current_task()->name,
delta < 0 ? "de" : "in", name,
delta < 0 ? -delta : delta);
if (mach_port_deallocate_debug)
SoftDebugger("mach_port_mod_refs");
}
return kr;
}
kr = ipc_right_delta(space, name, entry, right, delta);
return kr;
}
kern_return_t
mach_port_set_qlimit(
ipc_space_t 		space,
mach_port_name_t 	name,
mach_port_msgcount_t 	qlimit)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (qlimit > MACH_PORT_QLIMIT_MAX)
return KERN_INVALID_VALUE;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_set_qlimit(port, qlimit);
ip_unlock(port);
return KERN_SUCCESS;
}
kern_return_t
mach_port_set_mscount(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_mscount_t	mscount)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_set_mscount(port, mscount);
ip_unlock(port);
return KERN_SUCCESS;
}
kern_return_t
mach_port_set_seqno(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_seqno_t	seqno)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_set_seqno(port, seqno);
ip_unlock(port);
return KERN_SUCCESS;
}
static void
mach_port_gst_helper(
ipc_pset_t		pset,
ipc_port_t		port,
ipc_entry_num_t		maxnames,
mach_port_name_t	*names,
ipc_entry_num_t		*actualp)
{
ipc_pset_t ip_pset;
mach_port_name_t name;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
name = port->ip_receiver_name;
assert(name != MACH_PORT_NULL);
ip_pset = port->ip_pset;
ip_unlock(port);
if (pset == ip_pset) {
ipc_entry_num_t actual = *actualp;
if (actual < maxnames)
names[actual] = name;
*actualp = actual+1;
}
}
kern_return_t
mach_port_get_set_status(
ipc_space_t			space,
mach_port_name_t		name,
mach_port_name_t		**members,
mach_msg_type_number_t		*membersCnt)
{
ipc_entry_num_t actual;
ipc_entry_num_t maxnames;
kern_return_t kr;
vm_size_t size;
vm_offset_t addr;
vm_map_copy_t memory;
if (space == IS_NULL)
return KERN_INVALID_TASK;
size = PAGE_SIZE;
for (;;) {
ipc_entry_t entry;
mach_port_name_t *names;
ipc_pset_t pset;
kr = vm_allocate(ipc_kernel_map, &addr, size, TRUE);
if (kr != KERN_SUCCESS) {
printf_once("no more room in ipc_kernel_map\n");
return KERN_RESOURCE_SHORTAGE;
}
kr = vm_map_pageable(ipc_kernel_map, addr, addr + size,
VM_PROT_READ|VM_PROT_WRITE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
kr = ipc_right_lookup_read(space, name, &entry);
if (kr != KERN_SUCCESS) {
kmem_free(ipc_kernel_map, addr, size);
return kr;
}
if (IE_BITS_TYPE(entry->ie_bits) != MACH_PORT_TYPE_PORT_SET) {
is_read_unlock(space);
kmem_free(ipc_kernel_map, addr, size);
return KERN_INVALID_RIGHT;
}
pset = (ipc_pset_t) entry->ie_object;
assert(pset != IPS_NULL);
names = (mach_port_name_t *) addr;
maxnames = size / sizeof(mach_port_name_t);
actual = 0;
ipc_entry_t ientry;
struct rdxtree_iter iter;
rdxtree_for_each(&space->is_map, &iter, ientry) {
ipc_entry_bits_t bits = ientry->ie_bits;
if (bits & MACH_PORT_TYPE_RECEIVE) {
ipc_port_t port =
(ipc_port_t) ientry->ie_object;
mach_port_gst_helper(pset, port, maxnames,
names, &actual);
}
}
is_read_unlock(space);
if (actual <= maxnames)
break;
kmem_free(ipc_kernel_map, addr, size);
size = round_page(actual * sizeof(mach_port_name_t)) + PAGE_SIZE;
}
if (actual == 0) {
memory = VM_MAP_COPY_NULL;
kmem_free(ipc_kernel_map, addr, size);
} else {
vm_size_t size_used;
size_used = round_page(actual * sizeof(mach_port_name_t));
kr = vm_map_pageable(ipc_kernel_map,
addr, addr + size_used,
VM_PROT_NONE, TRUE, TRUE);
assert(kr == KERN_SUCCESS);
kr = vm_map_copyin(ipc_kernel_map, addr, size_used,
TRUE, &memory);
assert(kr == KERN_SUCCESS);
if (size_used != size)
kmem_free(ipc_kernel_map,
addr + size_used, size - size_used);
}
*members = (mach_port_name_t *) memory;
*membersCnt = actual;
return KERN_SUCCESS;
}
kern_return_t
mach_port_move_member(
ipc_space_t	space,
mach_port_name_t	member,
mach_port_name_t	after)
{
ipc_entry_t entry;
ipc_port_t port;
ipc_pset_t nset;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_right_lookup_read(space, member, &entry);
if (kr != KERN_SUCCESS)
return kr;
if ((entry->ie_bits & MACH_PORT_TYPE_RECEIVE) == 0) {
is_read_unlock(space);
return KERN_INVALID_RIGHT;
}
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (after == MACH_PORT_NULL)
nset = IPS_NULL;
else {
entry = ipc_entry_lookup(space, after);
if (entry == IE_NULL) {
is_read_unlock(space);
return KERN_INVALID_NAME;
}
if ((entry->ie_bits & MACH_PORT_TYPE_PORT_SET) == 0) {
is_read_unlock(space);
return KERN_INVALID_RIGHT;
}
nset = (ipc_pset_t) entry->ie_object;
assert(nset != IPS_NULL);
}
kr = ipc_pset_move(space, port, nset);
return kr;
}
kern_return_t
mach_port_request_notification(
ipc_space_t		space,
mach_port_name_t		name,
mach_msg_id_t		id,
mach_port_mscount_t	sync,
ipc_port_t		notify,
ipc_port_t		*previousp)
{
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (notify == IP_DEAD)
return KERN_INVALID_CAPABILITY;
switch (id) {
case MACH_NOTIFY_PORT_DESTROYED: {
ipc_port_t port, previous;
if (sync != 0)
return KERN_INVALID_VALUE;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_pdrequest(port, notify, &previous);
*previousp = previous;
break;
}
case MACH_NOTIFY_NO_SENDERS: {
ipc_port_t port;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_nsrequest(port, sync, notify, previousp);
break;
}
case MACH_NOTIFY_DEAD_NAME:
kr = ipc_right_dnrequest(space, name, sync != 0,
notify, previousp);
if (kr != KERN_SUCCESS)
return kr;
break;
default:
return KERN_INVALID_VALUE;
}
return KERN_SUCCESS;
}
kern_return_t
mach_port_insert_right(
ipc_space_t		space,
mach_port_name_t	name,
ipc_port_t		poly,
mach_msg_type_name_t	polyPoly)
{
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (!MACH_PORT_NAME_VALID(name) ||
!MACH_MSG_TYPE_PORT_ANY_RIGHT(polyPoly))
return KERN_INVALID_VALUE;
if (!IO_VALID((ipc_object_t)poly))
return KERN_INVALID_CAPABILITY;
return ipc_object_copyout_name(space, (ipc_object_t)poly,
polyPoly, FALSE, name);
}
kern_return_t
mach_port_extract_right(
ipc_space_t		space,
mach_port_name_t	name,
mach_msg_type_name_t	msgt_name,
ipc_port_t		*poly,
mach_msg_type_name_t	*polyPoly)
{
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
if (!MACH_MSG_TYPE_PORT_ANY(msgt_name))
return KERN_INVALID_VALUE;
kr = ipc_object_copyin(space, name, msgt_name, (ipc_object_t *) poly);
if (kr == KERN_SUCCESS)
*polyPoly = ipc_object_copyin_type(msgt_name);
return kr;
}
kern_return_t
mach_port_get_receive_status(
ipc_space_t 		space,
mach_port_name_t 	name,
mach_port_status_t 	*statusp)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
if (port->ip_pset != IPS_NULL) {
ipc_pset_t pset = port->ip_pset;
ips_lock(pset);
if (!ips_active(pset)) {
ipc_pset_remove(pset, port);
ips_check_unlock(pset);
goto no_port_set;
} else {
statusp->mps_pset = pset->ips_local_name;
imq_lock(&pset->ips_messages);
statusp->mps_seqno = port->ip_seqno;
imq_unlock(&pset->ips_messages);
ips_unlock(pset);
assert(MACH_PORT_NAME_VALID(statusp->mps_pset));
}
} else {
no_port_set:
statusp->mps_pset = MACH_PORT_NULL;
imq_lock(&port->ip_messages);
statusp->mps_seqno = port->ip_seqno;
imq_unlock(&port->ip_messages);
}
statusp->mps_mscount = port->ip_mscount;
statusp->mps_qlimit = port->ip_qlimit;
statusp->mps_msgcount = port->ip_msgcount;
statusp->mps_sorights = port->ip_sorights;
statusp->mps_srights = port->ip_srights > 0;
statusp->mps_pdrequest = port->ip_pdrequest != IP_NULL;
statusp->mps_nsrequest = port->ip_nsrequest != IP_NULL;
ip_unlock(port);
return KERN_SUCCESS;
}
#ifdef MIGRATING_THREADS
kern_return_t
mach_port_set_rpcinfo(
ipc_space_t 	space,
mach_port_name_t 	name,
void 		*rpc_info,
unsigned int 	rpc_info_count)
{
ipc_target_t target;
ipc_object_t object;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_object_translate(space, name,
MACH_PORT_RIGHT_PORT_SET, &object);
if (kr == KERN_SUCCESS)
target = &((ipc_pset_t)object)->ips_target;
else {
kr = ipc_object_translate(space, name,
MACH_PORT_RIGHT_RECEIVE, &object);
if (kr != KERN_SUCCESS)
return kr;
target = &((ipc_port_t)object)->ip_target;
}
kr = port_machine_set_rpcinfo(target, rpc_info, rpc_info_count);
io_unlock(object);
return kr;
}
#if 1
int sacts, maxsacts;
#endif
void sact_count(void)
{
printf("%d server activations in use, %d max\n", sacts, maxsacts);
}
kern_return_t
mach_port_create_act(
task_t 		task,
mach_port_name_t 	name,
vm_offset_t 	user_stack,
vm_offset_t 	user_rbuf,
vm_size_t 	user_rbuf_size,
Act 		**out_act)
{
ipc_target_t target;
ipc_space_t space;
ipc_object_t object;
kern_return_t kr;
Act *act;
if (task == 0)
return KERN_INVALID_TASK;
kr = act_create(task, user_stack, user_rbuf, user_rbuf_size, &act);
if (kr != KERN_SUCCESS)
return kr;
space = task->itk_space;
kr = ipc_object_translate(space, name,
MACH_PORT_RIGHT_PORT_SET, &object);
if (kr == KERN_SUCCESS)
target = &((ipc_pset_t)object)->ips_target;
else {
kr = ipc_object_translate(space, name,
MACH_PORT_RIGHT_RECEIVE, &object);
if (kr != KERN_SUCCESS) {
act_terminate(act);
act_deallocate(act);
return kr;
}
target = &((ipc_port_t)object)->ip_target;
}
#if 0
printf("act port/pset %08x ipc_target %08x stack %08x act %08x\n",
object, target, user_stack, act);
#endif
kr = act_set_target(act, target);
if (kr != KERN_SUCCESS) {
io_unlock(object);
act_terminate(act);
act_deallocate(act);
return kr;
}
#if 0
printf(" actpool %08x act %08x\n", target->ip_actpool, act);
#endif
io_unlock(object);
*out_act = act;
#if 1
sacts++;
if (sacts > maxsacts)
maxsacts = sacts;
act->mact.pcb->ss.mpsfu_high = 0x69;
#endif
return KERN_SUCCESS;
}
#ifdef RPCKERNELSIG
kern_return_t
mach_port_set_syscall_right(
task_t 		task,
mach_port_name_t 	name)
{
ipc_entry_t entry;
kern_return_t kr;
if (task == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_right_lookup_write(task, name, &entry);
if (kr != KERN_SUCCESS) {
return kr;
}
if (!(entry->ie_bits & MACH_PORT_TYPE(MACH_PORT_RIGHT_SEND))) {
is_write_unlock(space);
return KERN_INVALID_RIGHT;
}
task->syscall_ipc_entry = *entry;
is_write_unlock(space);
return KERN_SUCCESS;
}
#endif
#endif
kern_return_t
mach_port_set_protected_payload(
ipc_space_t		space,
mach_port_name_t	name,
rpc_uintptr_t		payload)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_set_protected_payload(port, payload);
ip_unlock(port);
return KERN_SUCCESS;
}
kern_return_t
mach_port_clear_protected_payload(
ipc_space_t		space,
mach_port_name_t	name)
{
ipc_port_t port;
kern_return_t kr;
if (space == IS_NULL)
return KERN_INVALID_TASK;
kr = ipc_port_translate_receive(space, name, &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_clear_protected_payload(port);
ip_unlock(port);
return KERN_SUCCESS;
}
#if	MACH_KDB
void
db_debug_port_references (boolean_t enable)
{
mach_port_deallocate_debug = enable;
}
#endif