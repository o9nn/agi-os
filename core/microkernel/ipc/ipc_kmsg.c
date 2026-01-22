#include <kern/printf.h>
#include <string.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/message.h>
#include <mach/port.h>
#include <machine/locore.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/kalloc.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_kern.h>
#include <vm/vm_user.h>
#include <ipc/port.h>
#include <ipc/copy_user.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_kmsg.h>
#include <ipc/ipc_thread.h>
#include <ipc/ipc_marequest.h>
#include <ipc/ipc_notify.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_machdep.h>
#include <device/net_io.h>
#if MACH_KDB
#include <ddb/db_output.h>
#include <ipc/ipc_print.h>
#endif
ipc_kmsg_t ipc_kmsg_cache[NCPUS];
void
ipc_kmsg_enqueue(
ipc_kmsg_queue_t	queue,
ipc_kmsg_t		kmsg)
{
ipc_kmsg_enqueue_macro(queue, kmsg);
}
ipc_kmsg_t
ipc_kmsg_dequeue(
ipc_kmsg_queue_t	queue)
{
ipc_kmsg_t first;
first = ipc_kmsg_queue_first(queue);
if (first != IKM_NULL)
ipc_kmsg_rmqueue_first_macro(queue, first);
return first;
}
void
ipc_kmsg_rmqueue(
ipc_kmsg_queue_t	queue,
ipc_kmsg_t		kmsg)
{
ipc_kmsg_t next, prev;
assert(queue->ikmq_base != IKM_NULL);
next = kmsg->ikm_next;
prev = kmsg->ikm_prev;
if (next == kmsg) {
assert(prev == kmsg);
assert(queue->ikmq_base == kmsg);
queue->ikmq_base = IKM_NULL;
} else {
if (queue->ikmq_base == kmsg)
queue->ikmq_base = next;
next->ikm_prev = prev;
prev->ikm_next = next;
}
ikm_mark_bogus (kmsg);
}
ipc_kmsg_t
ipc_kmsg_queue_next(
ipc_kmsg_queue_t	queue,
ipc_kmsg_t		kmsg)
{
ipc_kmsg_t next;
assert(queue->ikmq_base != IKM_NULL);
next = kmsg->ikm_next;
if (queue->ikmq_base == next)
next = IKM_NULL;
return next;
}
void
ipc_kmsg_destroy(
ipc_kmsg_t	kmsg)
{
ipc_kmsg_queue_t queue;
boolean_t empty;
queue = &current_thread()->ith_messages;
empty = ipc_kmsg_queue_empty(queue);
ipc_kmsg_enqueue(queue, kmsg);
if (empty) {
while ((kmsg = ipc_kmsg_queue_first(queue)) != IKM_NULL) {
ipc_kmsg_clean(kmsg);
ipc_kmsg_rmqueue(queue, kmsg);
ikm_free(kmsg);
}
}
}
static void
ipc_kmsg_clean_body(
vm_offset_t saddr,
vm_offset_t eaddr)
{
while (saddr < eaddr) {
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t number;
boolean_t is_inline, is_port;
vm_size_t length;
type = (mach_msg_type_long_t *) saddr;
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
if (((mach_msg_type_t*)type)->msgt_longform) {
name = type->msgtl_name;
size = type->msgtl_size;
number = type->msgtl_number;
saddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
saddr = mach_msg_kernel_align(saddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
number = ((mach_msg_type_t*)type)->msgt_number;
saddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
saddr = mach_msg_kernel_align(saddr);
}
}
length = ((number * size) + 7) >> 3;
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if (is_port) {
ipc_object_t *objects;
mach_msg_type_number_t i;
if (is_inline) {
objects = (ipc_object_t *) saddr;
while (eaddr < (vm_offset_t)&objects[number]) {
number--;
}
} else {
objects = (ipc_object_t *)
* (vm_offset_t *) saddr;
}
for (i = 0; i < number; i++) {
ipc_object_t object = objects[i];
if (!IO_VALID(object))
continue;
ipc_object_destroy(object, name);
}
}
if (is_inline) {
saddr += length;
} else {
vm_offset_t data = * (vm_offset_t *) saddr;
if (length == 0)
assert(data == 0);
else if (is_port)
kfree(data, length);
else
vm_map_copy_discard((vm_map_copy_t) data);
saddr += sizeof(vm_offset_t);
}
saddr = mach_msg_kernel_align(saddr);
}
}
void
ipc_kmsg_clean(ipc_kmsg_t kmsg)
{
ipc_marequest_t marequest;
ipc_object_t object;
mach_msg_bits_t mbits = kmsg->ikm_header.msgh_bits;
marequest = kmsg->ikm_marequest;
if (marequest != IMAR_NULL)
ipc_marequest_destroy(marequest);
object = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
if (IO_VALID(object))
ipc_object_destroy(object, MACH_MSGH_BITS_REMOTE(mbits));
object = (ipc_object_t) kmsg->ikm_header.msgh_local_port;
if (IO_VALID(object))
ipc_object_destroy(object, MACH_MSGH_BITS_LOCAL(mbits));
if (mbits & MACH_MSGH_BITS_COMPLEX) {
vm_offset_t saddr, eaddr;
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
eaddr = (vm_offset_t) &kmsg->ikm_header +
kmsg->ikm_header.msgh_size;
ipc_kmsg_clean_body(saddr, eaddr);
}
}
static void
ipc_kmsg_clean_partial(
ipc_kmsg_t 		kmsg,
vm_offset_t 		eaddr,
boolean_t 		dolast,
mach_msg_type_number_t 	number)
{
ipc_object_t object;
mach_msg_bits_t mbits = kmsg->ikm_header.msgh_bits;
vm_offset_t saddr;
assert(kmsg->ikm_marequest == IMAR_NULL);
object = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
assert(IO_VALID(object));
ipc_object_destroy(object, MACH_MSGH_BITS_REMOTE(mbits));
object = (ipc_object_t) kmsg->ikm_header.msgh_local_port;
if (IO_VALID(object))
ipc_object_destroy(object, MACH_MSGH_BITS_LOCAL(mbits));
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
ipc_kmsg_clean_body(saddr, eaddr);
if (dolast) {
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t rnumber;
boolean_t is_inline, is_port;
vm_size_t length;
type = (mach_msg_type_long_t *) eaddr;
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
if (((mach_msg_type_t*)type)->msgt_longform) {
name = type->msgtl_name;
size = type->msgtl_size;
rnumber = type->msgtl_number;
eaddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
eaddr = mach_msg_kernel_align(eaddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
rnumber = ((mach_msg_type_t*)type)->msgt_number;
eaddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
eaddr = mach_msg_kernel_align(eaddr);
}
}
length = ((rnumber * size) + 7) >> 3;
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if (is_port) {
ipc_object_t *objects;
mach_msg_type_number_t i;
objects = (ipc_object_t *)
(is_inline ? eaddr : * (vm_offset_t *) eaddr);
for (i = 0; i < number; i++) {
ipc_object_t obj = objects[i];
if (!IO_VALID(obj))
continue;
ipc_object_destroy(obj, name);
}
}
if (!is_inline) {
vm_offset_t data = * (vm_offset_t *) eaddr;
if (length == 0)
assert(data == 0);
else if (is_port)
kfree(data, length);
else
vm_map_copy_discard((vm_map_copy_t) data);
}
}
}
void
ipc_kmsg_free(ipc_kmsg_t kmsg)
{
vm_size_t size = kmsg->ikm_size;
switch (size) {
case IKM_SIZE_NETWORK:
net_kmsg_put(kmsg);
break;
default:
kfree((vm_offset_t) kmsg, size);
break;
}
}
static inline boolean_t
ipc_should_use_virtual_copy(vm_size_t length)
{
return (length >= IPC_VIRTUAL_COPY_THRESHOLD);
}
static inline boolean_t
ipc_should_use_zero_copy(vm_size_t length)
{
return (length >= IPC_ZERO_COPY_THRESHOLD);
}
mach_msg_return_t
ipc_kmsg_get(
mach_msg_user_header_t 	*msg,
mach_msg_size_t 	size,
ipc_kmsg_t 		*kmsgp)
{
ipc_kmsg_t kmsg;
mach_msg_size_t 	ksize = size * IKM_EXPAND_FACTOR;
if ((size < sizeof(mach_msg_user_header_t)) || mach_msg_user_is_misaligned(size))
return MACH_SEND_MSG_TOO_SMALL;
if (ksize <= IKM_SAVED_MSG_SIZE) {
kmsg = ikm_cache_alloc();
if (kmsg == IKM_NULL)
return MACH_SEND_NO_BUFFER;
} else {
kmsg = ikm_alloc(ksize);
if (kmsg == IKM_NULL)
return MACH_SEND_NO_BUFFER;
ikm_init(kmsg, ksize);
}
if (copyinmsg(msg, &kmsg->ikm_header, size, kmsg->ikm_size)) {
ikm_free(kmsg);
return MACH_SEND_INVALID_DATA;
}
*kmsgp = kmsg;
return MACH_MSG_SUCCESS;
}
extern mach_msg_return_t
ipc_kmsg_get_from_kernel(
mach_msg_header_t 	*msg,
mach_msg_size_t 	size,
ipc_kmsg_t 		*kmsgp)
{
ipc_kmsg_t kmsg;
assert(size >= sizeof(mach_msg_header_t));
assert(!mach_msg_kernel_is_misaligned(size));
kmsg = ikm_alloc(size);
if (kmsg == IKM_NULL)
return MACH_SEND_NO_BUFFER;
ikm_init(kmsg, size);
memcpy(&kmsg->ikm_header, msg, size);
kmsg->ikm_header.msgh_size = size;
*kmsgp = kmsg;
return MACH_MSG_SUCCESS;
}
mach_msg_return_t
ipc_kmsg_put(
mach_msg_user_header_t 	*msg,
ipc_kmsg_t 		kmsg,
mach_msg_size_t 	size)
{
mach_msg_return_t mr;
ikm_check_initialized(kmsg, kmsg->ikm_size);
if (copyoutmsg(&kmsg->ikm_header, msg, size))
mr = MACH_RCV_INVALID_DATA;
else
mr = MACH_MSG_SUCCESS;
ikm_cache_free(kmsg);
return mr;
}
void
ipc_kmsg_put_to_kernel(
mach_msg_header_t	*msg,
ipc_kmsg_t		kmsg,
mach_msg_size_t		size)
{
#if	DIPC
assert(!KMSG_IN_DIPC(kmsg));
#endif
memcpy(msg, &kmsg->ikm_header, size);
ikm_free(kmsg);
}
mach_msg_return_t
ipc_kmsg_copyin_header(
mach_msg_header_t 	*msg,
ipc_space_t 		space,
mach_port_name_t 	notify)
{
mach_msg_bits_t mbits = msg->msgh_bits &~ MACH_MSGH_BITS_CIRCULAR;
mach_port_name_t dest_name = (mach_port_name_t)msg->msgh_remote_port;
mach_port_name_t reply_name = (mach_port_name_t)msg->msgh_local_port;
kern_return_t kr;
#ifndef MIGRATING_THREADS
if (notify == MACH_PORT_NULL) switch (MACH_MSGH_BITS_PORTS(mbits)) {
case MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0): {
ipc_entry_t entry;
ipc_entry_bits_t bits;
ipc_port_t dest_port;
if (reply_name != MACH_PORT_NULL)
break;
is_read_lock(space);
if (!space->is_active)
goto abort_async;
entry = ipc_entry_lookup (space, dest_name);
if (entry == IE_NULL)
{
ipc_entry_lookup_failed (msg, dest_name);
goto abort_async;
}
bits = entry->ie_bits;
if (IE_BITS_TYPE (bits) != MACH_PORT_TYPE_SEND)
goto abort_async;
assert(IE_BITS_UREFS(bits) > 0);
dest_port = (ipc_port_t) entry->ie_object;
assert(dest_port != IP_NULL);
ip_lock(dest_port);
is_read_unlock(space);
if (!ip_active(dest_port)) {
ip_unlock(dest_port);
break;
}
assert(dest_port->ip_srights > 0);
dest_port->ip_srights++;
ip_reference(dest_port);
ip_unlock(dest_port);
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND, 0));
msg->msgh_remote_port = (mach_port_t) dest_port;
return MACH_MSG_SUCCESS;
abort_async:
is_read_unlock(space);
break;
}
case MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE): {
ipc_entry_t entry;
ipc_entry_bits_t bits;
ipc_port_t dest_port, reply_port;
is_read_lock(space);
if (!space->is_active)
goto abort_request;
entry = ipc_entry_lookup (space, dest_name);
if (entry == IE_NULL)
{
ipc_entry_lookup_failed (msg, dest_name);
goto abort_request;
}
bits = entry->ie_bits;
if (IE_BITS_TYPE (bits) != MACH_PORT_TYPE_SEND)
goto abort_request;
assert(IE_BITS_UREFS(bits) > 0);
dest_port = (ipc_port_t) entry->ie_object;
assert(dest_port != IP_NULL);
entry = ipc_entry_lookup (space, reply_name);
if (entry == IE_NULL)
{
ipc_entry_lookup_failed (msg, reply_name);
goto abort_request;
}
bits = entry->ie_bits;
if (IE_BITS_TYPE (bits) != MACH_PORT_TYPE_RECEIVE)
goto abort_request;
reply_port = (ipc_port_t) entry->ie_object;
assert(reply_port != IP_NULL);
ip_lock(dest_port);
if (!ip_active(dest_port) || !ip_lock_try(reply_port)) {
ip_unlock(dest_port);
goto abort_request;
}
is_read_unlock(space);
assert(dest_port->ip_srights > 0);
dest_port->ip_srights++;
ip_reference(dest_port);
ip_unlock(dest_port);
assert(ip_active(reply_port));
assert(reply_port->ip_receiver_name == reply_name);
assert(reply_port->ip_receiver == space);
reply_port->ip_sorights++;
ip_reference(reply_port);
ip_unlock(reply_port);
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND,
MACH_MSG_TYPE_PORT_SEND_ONCE));
msg->msgh_remote_port = (mach_port_t) dest_port;
msg->msgh_local_port = (mach_port_t) reply_port;
return MACH_MSG_SUCCESS;
abort_request:
is_read_unlock(space);
break;
}
case MACH_MSGH_BITS(MACH_MSG_TYPE_MOVE_SEND_ONCE, 0): {
ipc_entry_t entry;
ipc_entry_bits_t bits;
ipc_port_t dest_port;
if (reply_name != MACH_PORT_NULL)
break;
is_write_lock(space);
if (!space->is_active)
goto abort_reply;
entry = ipc_entry_lookup (space, dest_name);
if (entry == IE_NULL)
{
ipc_entry_lookup_failed (msg, dest_name);
goto abort_reply;
}
bits = entry->ie_bits;
if (IE_BITS_TYPE (bits) != MACH_PORT_TYPE_SEND_ONCE)
goto abort_reply;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(IE_BITS_UREFS(bits) == 1);
assert((bits & IE_BITS_MAREQUEST) == 0);
if (entry->ie_request != 0)
goto abort_reply;
dest_port = (ipc_port_t) entry->ie_object;
assert(dest_port != IP_NULL);
ip_lock(dest_port);
if (!ip_active(dest_port)) {
ip_unlock(dest_port);
goto abort_reply;
}
assert(dest_port->ip_sorights > 0);
ip_unlock(dest_port);
entry->ie_object = IO_NULL;
ipc_entry_dealloc (space, dest_name, entry);
is_write_unlock(space);
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE,
0));
msg->msgh_remote_port = (mach_port_t) dest_port;
return MACH_MSG_SUCCESS;
abort_reply:
is_write_unlock(space);
break;
}
default:
break;
}
#endif
{
mach_msg_type_name_t dest_type = MACH_MSGH_BITS_REMOTE(mbits);
mach_msg_type_name_t reply_type = MACH_MSGH_BITS_LOCAL(mbits);
ipc_object_t dest_port, reply_port;
ipc_port_t dest_soright, reply_soright;
ipc_port_t notify_port = 0;
if (!MACH_MSG_TYPE_PORT_ANY_SEND(dest_type))
return MACH_SEND_INVALID_HEADER;
if ((reply_type == 0) ?
(reply_name != MACH_PORT_NULL) :
!MACH_MSG_TYPE_PORT_ANY_SEND(reply_type))
return MACH_SEND_INVALID_HEADER;
is_write_lock(space);
if (!space->is_active)
goto invalid_dest;
if (notify != MACH_PORT_NULL) {
ipc_entry_t entry;
if (((entry = ipc_entry_lookup(space, notify)) == IE_NULL) ||
((entry->ie_bits & MACH_PORT_TYPE_RECEIVE) == 0)) {
if (entry == IE_NULL)
ipc_entry_lookup_failed (msg, notify);
is_write_unlock(space);
return MACH_SEND_INVALID_NOTIFY;
}
notify_port = (ipc_port_t) entry->ie_object;
}
if (dest_name == reply_name) {
ipc_entry_t entry;
mach_port_name_t name = dest_name;
entry = ipc_entry_lookup(space, name);
if (entry == IE_NULL) {
ipc_entry_lookup_failed (msg, name);
goto invalid_dest;
}
assert(reply_type != 0);
if (!ipc_right_copyin_check(space, name, entry, reply_type))
goto invalid_reply;
if ((dest_type == MACH_MSG_TYPE_MOVE_SEND_ONCE) ||
(reply_type == MACH_MSG_TYPE_MOVE_SEND_ONCE)) {
goto invalid_dest;
} else if ((dest_type == MACH_MSG_TYPE_MAKE_SEND) ||
(dest_type == MACH_MSG_TYPE_MAKE_SEND_ONCE) ||
(reply_type == MACH_MSG_TYPE_MAKE_SEND) ||
(reply_type == MACH_MSG_TYPE_MAKE_SEND_ONCE)) {
kr = ipc_right_copyin(space, name, entry,
dest_type, FALSE,
&dest_port, &dest_soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
assert(IO_VALID(dest_port));
assert(entry->ie_bits & MACH_PORT_TYPE_RECEIVE);
assert(dest_soright == IP_NULL);
kr = ipc_right_copyin(space, name, entry,
reply_type, TRUE,
&reply_port, &reply_soright);
assert(kr == KERN_SUCCESS);
assert(reply_port == dest_port);
assert(entry->ie_bits & MACH_PORT_TYPE_RECEIVE);
assert(reply_soright == IP_NULL);
} else if ((dest_type == MACH_MSG_TYPE_COPY_SEND) &&
(reply_type == MACH_MSG_TYPE_COPY_SEND)) {
kr = ipc_right_copyin(space, name, entry,
dest_type, FALSE,
&dest_port, &dest_soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
assert(entry->ie_bits & MACH_PORT_TYPE_SEND);
assert(dest_soright == IP_NULL);
reply_port = (ipc_object_t)
ipc_port_copy_send((ipc_port_t) dest_port);
reply_soright = IP_NULL;
} else if ((dest_type == MACH_MSG_TYPE_MOVE_SEND) &&
(reply_type == MACH_MSG_TYPE_MOVE_SEND)) {
kr = ipc_right_copyin_two(space, name, entry,
&dest_port, &dest_soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
if (IE_BITS_TYPE(entry->ie_bits)
== MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, name, entry);
reply_port = dest_port;
reply_soright = IP_NULL;
} else {
ipc_port_t soright;
assert(((dest_type == MACH_MSG_TYPE_COPY_SEND) &&
(reply_type == MACH_MSG_TYPE_MOVE_SEND)) ||
((dest_type == MACH_MSG_TYPE_MOVE_SEND) &&
(reply_type == MACH_MSG_TYPE_COPY_SEND)));
kr = ipc_right_copyin(space, name, entry,
MACH_MSG_TYPE_MOVE_SEND, FALSE,
&dest_port, &soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
if (IE_BITS_TYPE(entry->ie_bits)
== MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, name, entry);
reply_port = (ipc_object_t)
ipc_port_copy_send((ipc_port_t) dest_port);
if (dest_type == MACH_MSG_TYPE_MOVE_SEND) {
dest_soright = soright;
reply_soright = IP_NULL;
} else {
dest_soright = IP_NULL;
reply_soright = soright;
}
}
} else if (!MACH_PORT_NAME_VALID(reply_name)) {
ipc_entry_t entry;
entry = ipc_entry_lookup(space, dest_name);
if (entry == IE_NULL) {
ipc_entry_lookup_failed (msg, dest_name);
goto invalid_dest;
}
kr = ipc_right_copyin(space, dest_name, entry,
dest_type, FALSE,
&dest_port, &dest_soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
if (IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, dest_name, entry);
reply_port = (ipc_object_t) invalid_name_to_port(reply_name);
reply_soright = IP_NULL;
} else {
ipc_entry_t dest_entry, reply_entry;
ipc_port_t saved_reply;
dest_entry = ipc_entry_lookup(space, dest_name);
if (dest_entry == IE_NULL) {
ipc_entry_lookup_failed (msg, dest_name);
goto invalid_dest;
}
reply_entry = ipc_entry_lookup(space, reply_name);
if (reply_entry == IE_NULL)
{
ipc_entry_lookup_failed (msg, reply_name);
goto invalid_reply;
}
assert(dest_entry != reply_entry);
assert(reply_type != 0);
if (!ipc_right_copyin_check(space, reply_name, reply_entry,
reply_type))
goto invalid_reply;
kr = ipc_right_copyin(space, dest_name, dest_entry,
dest_type, FALSE,
&dest_port, &dest_soright);
if (kr != KERN_SUCCESS)
goto invalid_dest;
assert(IO_VALID(dest_port));
saved_reply = (ipc_port_t) reply_entry->ie_object;
if (saved_reply != IP_NULL)
ipc_port_reference(saved_reply);
kr = ipc_right_copyin(space, reply_name, reply_entry,
reply_type, TRUE,
&reply_port, &reply_soright);
assert(kr == KERN_SUCCESS);
if ((saved_reply != IP_NULL) && (reply_port == IO_DEAD)) {
ipc_port_t dest = (ipc_port_t) dest_port;
ipc_port_timestamp_t timestamp;
boolean_t must_undo;
ip_lock(saved_reply);
assert(!ip_active(saved_reply));
timestamp = saved_reply->ip_timestamp;
ip_unlock(saved_reply);
ip_lock(dest);
must_undo = (!ip_active(dest) &&
IP_TIMESTAMP_ORDER(dest->ip_timestamp,
timestamp));
ip_unlock(dest);
if (must_undo) {
ipc_right_copyin_undo(
space, dest_name, dest_entry,
dest_type, dest_port,
dest_soright);
ipc_right_copyin_undo(
space, reply_name, reply_entry,
reply_type, reply_port,
reply_soright);
is_write_unlock(space);
if (dest_soright != IP_NULL)
ipc_notify_dead_name(dest_soright,
dest_name);
assert(reply_soright == IP_NULL);
ipc_port_release(saved_reply);
return MACH_SEND_INVALID_DEST;
}
}
if (IE_BITS_TYPE(reply_entry->ie_bits) == MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, reply_name, reply_entry);
if (IE_BITS_TYPE(dest_entry->ie_bits) == MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, dest_name, dest_entry);
if (saved_reply != IP_NULL)
ipc_port_release(saved_reply);
}
if ((notify != MACH_PORT_NULL) &&
(dest_soright == notify_port)) {
ipc_port_release_sonce(dest_soright);
dest_soright = IP_NULL;
}
is_write_unlock(space);
if (dest_soright != IP_NULL)
ipc_notify_port_deleted(dest_soright, dest_name);
if (reply_soright != IP_NULL)
ipc_notify_port_deleted(reply_soright, reply_name);
dest_type = ipc_object_copyin_type(dest_type);
reply_type = ipc_object_copyin_type(reply_type);
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(dest_type, reply_type));
msg->msgh_remote_port = (mach_port_t) dest_port;
msg->msgh_local_port = (mach_port_t) reply_port;
}
return MACH_MSG_SUCCESS;
invalid_dest:
is_write_unlock(space);
return MACH_SEND_INVALID_DEST;
invalid_reply:
is_write_unlock(space);
return MACH_SEND_INVALID_REPLY;
}
static mach_msg_return_t
ipc_kmsg_copyin_body(
ipc_kmsg_t 	kmsg,
ipc_space_t 	space,
vm_map_t 	map)
{
ipc_object_t dest;
vm_offset_t saddr, eaddr;
boolean_t complex;
boolean_t use_page_lists, steal_pages;
dest = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
complex = FALSE;
use_page_lists = ipc_kobject_vm_page_list(ip_kotype((ipc_port_t)dest));
steal_pages = ipc_kobject_vm_page_steal(ip_kotype((ipc_port_t)dest));
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
eaddr = (vm_offset_t) &kmsg->ikm_header + kmsg->ikm_header.msgh_size;
_Static_assert(!mach_msg_kernel_is_misaligned(sizeof(mach_msg_header_t)),
"mach_msg_header_t needs to be MACH_MSG_KERNEL_ALIGNMENT aligned.");
while (saddr < eaddr) {
vm_offset_t taddr = saddr;
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t number;
boolean_t is_inline, longform, dealloc, is_port;
vm_offset_t data;
vm_size_t length;
kern_return_t kr;
type = (mach_msg_type_long_t *) saddr;
if (((eaddr - saddr) < sizeof(mach_msg_type_t)) ||
((longform = ((mach_msg_type_t*)type)->msgt_longform) &&
((eaddr - saddr) < sizeof(mach_msg_type_long_t)))) {
ipc_kmsg_clean_partial(kmsg, taddr, FALSE, 0);
return MACH_SEND_MSG_TOO_SMALL;
}
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
dealloc = ((mach_msg_type_t*)type)->msgt_deallocate;
if (longform) {
name = type->msgtl_name;
size = type->msgtl_size;
number = type->msgtl_number;
saddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
saddr = mach_msg_kernel_align(saddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
number = ((mach_msg_type_t*)type)->msgt_number;
saddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
saddr = mach_msg_kernel_align(saddr);
}
}
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if ((is_port && !is_inline && (size != PORT_NAME_T_SIZE_IN_BITS)) ||
(is_port && is_inline && (size != PORT_T_SIZE_IN_BITS)) ||
#ifndef __LP64__
(longform && ((type->msgtl_header.msgt_name != 0) ||
(type->msgtl_header.msgt_size != 0) ||
(type->msgtl_header.msgt_number != 0))) ||
#endif
(((mach_msg_type_t*)type)->msgt_unused != 0) ||
(dealloc && is_inline)) {
ipc_kmsg_clean_partial(kmsg, taddr, FALSE, 0);
return MACH_SEND_INVALID_TYPE;
}
length = (((uint64_t) number * size) + 7) >> 3;
if (is_inline) {
vm_size_t amount = length;
if ((eaddr - saddr) < amount) {
ipc_kmsg_clean_partial(kmsg, taddr, FALSE, 0);
return MACH_SEND_MSG_TOO_SMALL;
}
data = saddr;
saddr += amount;
} else {
vm_offset_t addr;
if ((eaddr - saddr) < sizeof(vm_offset_t)) {
ipc_kmsg_clean_partial(kmsg, taddr, FALSE, 0);
return MACH_SEND_MSG_TOO_SMALL;
}
addr = * (vm_offset_t *) saddr;
if (length == 0)
data = 0;
else if (is_port) {
const vm_size_t user_length = length;
if (sizeof(mach_port_name_t) != sizeof(mach_port_t)) {
length = sizeof(mach_port_t) * number;
type->msgtl_size = sizeof(mach_port_t) * 8;
}
data = kalloc(length);
if (data == 0)
goto invalid_memory;
if (user_length != length)
{
mach_port_name_t *src = (mach_port_name_t*)addr;
mach_port_t *dst = (mach_port_t*)data;
for (int i=0; i<number; i++) {
if (copyin_port(src + i, dst + i)) {
kfree(data, length);
goto invalid_memory;
}
}
} else if (copyinmap(map, (char *) addr,
(char *) data, length)) {
kfree(data, length);
goto invalid_memory;
}
if (dealloc &&
(vm_deallocate(map, addr, user_length) != KERN_SUCCESS)) {
kfree(data, length);
goto invalid_memory;
}
} else {
vm_map_copy_t copy;
if (use_page_lists) {
kr = vm_map_copyin_page_list(map,
addr, length, dealloc,
steal_pages, &copy, FALSE);
} else if (ipc_should_use_zero_copy(length)) {
kr = vm_map_copyin(map, addr, length,
FALSE, &copy);
if (kr == KERN_SUCCESS && dealloc) {
(void) vm_deallocate(map, addr, length);
}
} else if (ipc_should_use_virtual_copy(length)) {
kr = vm_map_copyin(map, addr, length,
dealloc, &copy);
} else {
kr = vm_map_copyin(map, addr, length,
dealloc, &copy);
}
if (kr != KERN_SUCCESS) {
invalid_memory:
ipc_kmsg_clean_partial(kmsg, taddr,
FALSE, 0);
return MACH_SEND_INVALID_MEMORY;
}
data = (vm_offset_t) copy;
}
* (vm_offset_t *) saddr = data;
saddr += sizeof(vm_offset_t);
complex = TRUE;
}
if (is_port) {
mach_msg_type_name_t newname =
ipc_object_copyin_type(name);
ipc_object_t *objects = (ipc_object_t *) data;
mach_msg_type_number_t i;
if (longform)
type->msgtl_name = newname;
else
((mach_msg_type_t*)type)->msgt_name = newname;
for (i = 0; i < number; i++) {
mach_port_name_t port = ((mach_port_t*)data)[i];
ipc_object_t object;
if (!MACH_PORT_NAME_VALID(port)) {
objects[i] = (ipc_object_t)invalid_name_to_port(port);
continue;
}
kr = ipc_object_copyin(space, port,
name, &object);
if (kr != KERN_SUCCESS) {
ipc_kmsg_clean_partial(kmsg, taddr,
TRUE, i);
return MACH_SEND_INVALID_RIGHT;
}
if ((newname == MACH_MSG_TYPE_PORT_RECEIVE) &&
ipc_port_check_circularity(
(ipc_port_t) object,
(ipc_port_t) dest))
kmsg->ikm_header.msgh_bits |=
MACH_MSGH_BITS_CIRCULAR;
objects[i] = object;
}
complex = TRUE;
}
saddr = mach_msg_kernel_align(saddr);
}
if (!complex)
kmsg->ikm_header.msgh_bits &= ~MACH_MSGH_BITS_COMPLEX;
return MACH_MSG_SUCCESS;
}
mach_msg_return_t
ipc_kmsg_copyin(
ipc_kmsg_t 	kmsg,
ipc_space_t 	space,
vm_map_t 	map,
mach_port_name_t notify)
{
mach_msg_return_t mr;
mr = ipc_kmsg_copyin_header(&kmsg->ikm_header, space, notify);
if (mr != MACH_MSG_SUCCESS)
return mr;
if ((kmsg->ikm_header.msgh_bits & MACH_MSGH_BITS_COMPLEX) == 0)
return MACH_MSG_SUCCESS;
return ipc_kmsg_copyin_body(kmsg, space, map);
}
void
ipc_kmsg_copyin_from_kernel(ipc_kmsg_t kmsg)
{
mach_msg_bits_t bits = kmsg->ikm_header.msgh_bits;
mach_msg_type_name_t rname = MACH_MSGH_BITS_REMOTE(bits);
mach_msg_type_name_t lname = MACH_MSGH_BITS_LOCAL(bits);
ipc_object_t remote = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
ipc_object_t local = (ipc_object_t) kmsg->ikm_header.msgh_local_port;
vm_offset_t saddr, eaddr;
ipc_object_copyin_from_kernel(remote, rname);
if (IO_VALID(local))
ipc_object_copyin_from_kernel(local, lname);
if (bits == (MACH_MSGH_BITS_COMPLEX |
MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0))) {
bits = (MACH_MSGH_BITS_COMPLEX |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND, 0));
kmsg->ikm_header.msgh_bits = bits;
} else {
bits = (MACH_MSGH_BITS_OTHER(bits) |
MACH_MSGH_BITS(ipc_object_copyin_type(rname),
ipc_object_copyin_type(lname)));
kmsg->ikm_header.msgh_bits = bits;
if ((bits & MACH_MSGH_BITS_COMPLEX) == 0)
return;
}
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
eaddr = (vm_offset_t) &kmsg->ikm_header + kmsg->ikm_header.msgh_size;
while (saddr < eaddr) {
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t number;
boolean_t is_inline, longform, is_port;
vm_offset_t data;
vm_size_t length;
type = (mach_msg_type_long_t *) saddr;
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
longform = ((mach_msg_type_t*)type)->msgt_longform;
if (longform) {
name = type->msgtl_name;
size = type->msgtl_size;
number = type->msgtl_number;
saddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
saddr = mach_msg_kernel_align(saddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
number = ((mach_msg_type_t*)type)->msgt_number;
saddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
saddr = mach_msg_kernel_align(saddr);
}
}
length = ((number * size) + 7) >> 3;
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if (is_inline) {
data = saddr;
saddr += length;
} else {
data = * (vm_offset_t *) saddr;
saddr += sizeof(vm_offset_t);
}
if (is_port) {
mach_msg_type_name_t newname =
ipc_object_copyin_type(name);
ipc_object_t *objects = (ipc_object_t *) data;
mach_msg_type_number_t i;
if (longform)
type->msgtl_name = newname;
else
((mach_msg_type_t*)type)->msgt_name = newname;
for (i = 0; i < number; i++) {
ipc_object_t object = objects[i];
if (!IO_VALID(object))
continue;
ipc_object_copyin_from_kernel(object, name);
if ((newname == MACH_MSG_TYPE_PORT_RECEIVE) &&
ipc_port_check_circularity(
(ipc_port_t) object,
(ipc_port_t) remote))
kmsg->ikm_header.msgh_bits |=
MACH_MSGH_BITS_CIRCULAR;
}
}
saddr = mach_msg_kernel_align(saddr);
}
}
mach_msg_return_t
ipc_kmsg_copyout_header(
mach_msg_header_t 	*msg,
ipc_space_t 		space,
mach_port_name_t 		notify)
{
mach_msg_bits_t mbits = msg->msgh_bits;
ipc_port_t dest = (ipc_port_t) msg->msgh_remote_port;
assert(IP_VALID(dest));
#ifndef MIGRATING_THREADS
if (notify == MACH_PORT_NULL) switch (MACH_MSGH_BITS_PORTS(mbits)) {
case MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND, 0): {
mach_port_name_t dest_name;
ipc_port_t nsrequest;
rpc_uintptr_t payload;
ip_lock(dest);
if (!ip_active(dest)) {
ip_unlock(dest);
break;
}
assert(dest->ip_srights > 0);
ip_release(dest);
if (dest->ip_receiver == space)
dest_name = dest->ip_receiver_name;
else
dest_name = MACH_PORT_NULL;
payload = dest->ip_protected_payload;
if ((--dest->ip_srights == 0) &&
((nsrequest = dest->ip_nsrequest) != IP_NULL)) {
mach_port_mscount_t mscount;
dest->ip_nsrequest = IP_NULL;
mscount = dest->ip_mscount;
ip_unlock(dest);
ipc_notify_no_senders(nsrequest, mscount);
} else
ip_unlock(dest);
if (! ipc_port_flag_protected_payload(dest)) {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(0, MACH_MSG_TYPE_PORT_SEND));
msg->msgh_local_port = dest_name;
} else {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(
0, MACH_MSG_TYPE_PROTECTED_PAYLOAD));
msg->msgh_protected_payload = payload;
}
msg->msgh_remote_port = MACH_PORT_NULL;
return MACH_MSG_SUCCESS;
}
case MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND,
MACH_MSG_TYPE_PORT_SEND_ONCE): {
ipc_entry_t entry;
ipc_port_t reply = (ipc_port_t) msg->msgh_local_port;
mach_port_name_t dest_name, reply_name;
ipc_port_t nsrequest;
rpc_uintptr_t payload;
if (!IP_VALID(reply))
break;
is_write_lock(space);
if (!space->is_active || space->is_free_list == NULL) {
is_write_unlock(space);
break;
}
ip_lock(dest);
if (!ip_active(dest) || !ip_lock_try(reply)) {
ip_unlock(dest);
is_write_unlock(space);
break;
}
if (!ip_active(reply)) {
ip_unlock(reply);
ip_unlock(dest);
is_write_unlock(space);
break;
}
assert(reply->ip_sorights > 0);
ip_unlock(reply);
kern_return_t kr;
kr = ipc_entry_get (space, &reply_name, &entry);
if (kr) {
ip_unlock(reply);
ip_unlock(dest);
is_write_unlock(space);
break;
}
{
mach_port_gen_t gen;
assert((entry->ie_bits &~ IE_BITS_GEN_MASK) == 0);
gen = entry->ie_bits + IE_BITS_GEN_ONE;
entry->ie_bits = gen | (MACH_PORT_TYPE_SEND_ONCE | 1);
}
assert(MACH_PORT_NAME_VALID(reply_name));
entry->ie_object = (ipc_object_t) reply;
is_write_unlock(space);
assert(dest->ip_srights > 0);
ip_release(dest);
if (dest->ip_receiver == space)
dest_name = dest->ip_receiver_name;
else
dest_name = MACH_PORT_NULL;
payload = dest->ip_protected_payload;
if ((--dest->ip_srights == 0) &&
((nsrequest = dest->ip_nsrequest) != IP_NULL)) {
mach_port_mscount_t mscount;
dest->ip_nsrequest = IP_NULL;
mscount = dest->ip_mscount;
ip_unlock(dest);
ipc_notify_no_senders(nsrequest, mscount);
} else
ip_unlock(dest);
if (! ipc_port_flag_protected_payload(dest)) {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE,
MACH_MSG_TYPE_PORT_SEND));
msg->msgh_local_port = dest_name;
} else {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE,
MACH_MSG_TYPE_PROTECTED_PAYLOAD));
msg->msgh_protected_payload = payload;
}
msg->msgh_remote_port = reply_name;
return MACH_MSG_SUCCESS;
}
case MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE, 0): {
mach_port_name_t dest_name;
rpc_uintptr_t payload;
ip_lock(dest);
if (!ip_active(dest)) {
ip_unlock(dest);
break;
}
assert(dest->ip_sorights > 0);
payload = dest->ip_protected_payload;
if (dest->ip_receiver == space) {
ip_release(dest);
dest->ip_sorights--;
dest_name = dest->ip_receiver_name;
ip_unlock(dest);
} else {
ip_unlock(dest);
ipc_notify_send_once(dest);
dest_name = MACH_PORT_NULL;
}
if (! ipc_port_flag_protected_payload(dest)) {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(0,
MACH_MSG_TYPE_PORT_SEND_ONCE));
msg->msgh_local_port = dest_name;
} else {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(0,
MACH_MSG_TYPE_PROTECTED_PAYLOAD));
msg->msgh_protected_payload = payload;
}
msg->msgh_remote_port = MACH_PORT_NULL;
return MACH_MSG_SUCCESS;
}
default:
break;
}
#endif
{
mach_msg_type_name_t dest_type = MACH_MSGH_BITS_REMOTE(mbits);
mach_msg_type_name_t reply_type = MACH_MSGH_BITS_LOCAL(mbits);
ipc_port_t reply = (ipc_port_t) msg->msgh_local_port;
mach_port_name_t dest_name, reply_name;
rpc_uintptr_t payload;
if (IP_VALID(reply)) {
ipc_port_t notify_port;
ipc_entry_t entry;
kern_return_t kr;
is_write_lock(space);
for (;;) {
ipc_port_request_index_t request;
if (!space->is_active) {
is_write_unlock(space);
return (MACH_RCV_HEADER_ERROR|
MACH_MSG_IPC_SPACE);
}
if (notify != MACH_PORT_NULL) {
notify_port = ipc_port_lookup_notify(space,
notify);
if (notify_port == IP_NULL) {
is_write_unlock(space);
return MACH_RCV_INVALID_NOTIFY;
}
} else
notify_port = IP_NULL;
if ((reply_type != MACH_MSG_TYPE_PORT_SEND_ONCE) &&
ipc_right_reverse(space, (ipc_object_t) reply,
&reply_name, &entry)) {
assert(entry->ie_bits &
MACH_PORT_TYPE_SEND_RECEIVE);
break;
}
ip_lock(reply);
if (!ip_active(reply)) {
ip_release(reply);
ip_check_unlock(reply);
if (notify_port != IP_NULL)
ipc_port_release_sonce(notify_port);
ip_lock(dest);
is_write_unlock(space);
reply = IP_DEAD;
reply_name = MACH_PORT_NAME_DEAD;
goto copyout_dest;
}
kr = ipc_entry_alloc(space, &reply_name, &entry);
if (kr != KERN_SUCCESS) {
ip_unlock(reply);
if (notify_port != IP_NULL)
ipc_port_release_sonce(notify_port);
is_write_unlock(space);
if (kr == KERN_RESOURCE_SHORTAGE)
return (MACH_RCV_HEADER_ERROR|
MACH_MSG_IPC_KERNEL);
else
return (MACH_RCV_HEADER_ERROR|
MACH_MSG_IPC_SPACE);
}
assert(IE_BITS_TYPE(entry->ie_bits)
== MACH_PORT_TYPE_NONE);
assert(entry->ie_object == IO_NULL);
if (notify_port == IP_NULL) {
entry->ie_object = (ipc_object_t) reply;
break;
}
kr = ipc_port_dnrequest(reply, reply_name,
notify_port, &request);
if (kr != KERN_SUCCESS) {
ip_unlock(reply);
ipc_port_release_sonce(notify_port);
ipc_entry_dealloc(space, reply_name, entry);
is_write_unlock(space);
ip_lock(reply);
if (!ip_active(reply)) {
ip_unlock(reply);
is_write_lock(space);
continue;
}
kr = ipc_port_dngrow(reply);
if (kr != KERN_SUCCESS)
return (MACH_RCV_HEADER_ERROR|
MACH_MSG_IPC_KERNEL);
is_write_lock(space);
continue;
}
notify_port = IP_NULL;
entry->ie_object = (ipc_object_t) reply;
entry->ie_request = request;
break;
}
ip_reference(reply);
kr = ipc_right_copyout(space, reply_name, entry,
reply_type, TRUE, (ipc_object_t) reply);
assert(kr == KERN_SUCCESS);
if (notify_port != IP_NULL)
ipc_port_release_sonce(notify_port);
ip_lock(dest);
is_write_unlock(space);
} else {
is_read_lock(space);
if (!space->is_active) {
is_read_unlock(space);
return MACH_RCV_HEADER_ERROR|MACH_MSG_IPC_SPACE;
}
if (notify != MACH_PORT_NULL) {
ipc_entry_t entry;
if (((entry = ipc_entry_lookup(space, notify))
== IE_NULL) ||
((entry->ie_bits & MACH_PORT_TYPE_RECEIVE) == 0)) {
if (entry == IE_NULL)
ipc_entry_lookup_failed (msg, notify);
is_read_unlock(space);
return MACH_RCV_INVALID_NOTIFY;
}
}
ip_lock(dest);
is_read_unlock(space);
reply_name = invalid_port_to_name(msg->msgh_local_port);
}
copyout_dest:
payload = dest->ip_protected_payload;
if (ip_active(dest)) {
ipc_object_copyout_dest(space, (ipc_object_t) dest,
dest_type, &dest_name);
} else {
ipc_port_timestamp_t timestamp;
timestamp = dest->ip_timestamp;
ip_release(dest);
ip_check_unlock(dest);
if (IP_VALID(reply)) {
ip_lock(reply);
if (ip_active(reply) ||
IP_TIMESTAMP_ORDER(timestamp,
reply->ip_timestamp))
dest_name = MACH_PORT_NAME_DEAD;
else
dest_name = MACH_PORT_NAME_NULL;
ip_unlock(reply);
} else
dest_name = MACH_PORT_NAME_DEAD;
}
if (IP_VALID(reply))
ipc_port_release(reply);
if (! ipc_port_flag_protected_payload(dest)) {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(reply_type, dest_type));
msg->msgh_local_port = dest_name;
} else {
msg->msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(reply_type,
MACH_MSG_TYPE_PROTECTED_PAYLOAD));
msg->msgh_protected_payload = payload;
}
msg->msgh_remote_port = reply_name;
}
return MACH_MSG_SUCCESS;
}
mach_msg_return_t
ipc_kmsg_copyout_object(
ipc_space_t 		space,
ipc_object_t 		object,
mach_msg_type_name_t 	msgt_name,
mach_port_name_t 	*namep)
{
if (!IO_VALID(object)) {
*namep = invalid_port_to_name((mach_port_t)object);
return MACH_MSG_SUCCESS;
}
#ifndef MIGRATING_THREADS
if (msgt_name != MACH_MSG_TYPE_PORT_SEND)
goto slow_copyout;
{
ipc_port_t port = (ipc_port_t) object;
ipc_entry_t entry;
is_write_lock(space);
if (!space->is_active) {
is_write_unlock(space);
goto slow_copyout;
}
ip_lock(port);
if (!ip_active(port) ||
(entry = ipc_reverse_lookup(space,
(ipc_object_t) port)) == NULL) {
ip_unlock(port);
is_write_unlock(space);
goto slow_copyout;
}
*namep = entry->ie_name;
assert(port->ip_srights > 1);
port->ip_srights--;
ip_release(port);
ip_unlock(port);
assert(entry->ie_bits & MACH_PORT_TYPE_SEND);
assert(IE_BITS_UREFS(entry->ie_bits) > 0);
assert(IE_BITS_UREFS(entry->ie_bits) < MACH_PORT_UREFS_MAX);
{
ipc_entry_bits_t bits = entry->ie_bits + 1;
if (IE_BITS_UREFS(bits) < MACH_PORT_UREFS_MAX)
entry->ie_bits = bits;
}
is_write_unlock(space);
return MACH_MSG_SUCCESS;
}
slow_copyout:
#endif
{
kern_return_t kr;
kr = ipc_object_copyout(space, object, msgt_name, TRUE, namep);
if (kr != KERN_SUCCESS) {
ipc_object_destroy(object, msgt_name);
if (kr == KERN_INVALID_CAPABILITY)
*namep = MACH_PORT_NAME_DEAD;
else {
*namep = MACH_PORT_NAME_NULL;
if (kr == KERN_RESOURCE_SHORTAGE)
return MACH_MSG_IPC_KERNEL;
else
return MACH_MSG_IPC_SPACE;
}
}
return MACH_MSG_SUCCESS;
}
}
mach_msg_return_t
ipc_kmsg_copyout_body(
ipc_kmsg_t kmsg,
ipc_space_t 	space,
vm_map_t 	map)
{
mach_msg_return_t mr = MACH_MSG_SUCCESS;
kern_return_t kr;
vm_offset_t saddr, eaddr;
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
eaddr = (vm_offset_t) &kmsg->ikm_header +
kmsg->ikm_header.msgh_size;
while (saddr < eaddr) {
vm_offset_t taddr = saddr;
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t number;
boolean_t is_inline, longform, is_port;
vm_size_t length;
vm_offset_t addr;
type = (mach_msg_type_long_t *) saddr;
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
longform = ((mach_msg_type_t*)type)->msgt_longform;
if (longform) {
name = type->msgtl_name;
size = type->msgtl_size;
number = type->msgtl_number;
saddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
saddr = mach_msg_kernel_align(saddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
number = ((mach_msg_type_t*)type)->msgt_number;
saddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
saddr = mach_msg_kernel_align(saddr);
}
}
length = (((uint64_t) number * size) + 7) >> 3;
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if (is_port) {
ipc_object_t *objects;
mach_msg_type_number_t i;
if (!is_inline) {
if (length != 0) {
vm_size_t user_length = length;
if (sizeof(mach_port_name_t) != sizeof(mach_port_t)) {
user_length = sizeof(mach_port_name_t) * number;
}
kr = vm_allocate(map, &addr, user_length, TRUE);
if (kr != KERN_SUCCESS) {
ipc_kmsg_clean_body(taddr, saddr);
goto vm_copyout_failure;
}
}
if (sizeof(mach_port_name_t) != sizeof(mach_port_t)) {
type->msgtl_size = sizeof(mach_port_name_t) * 8;
}
}
objects = (ipc_object_t *)
(is_inline ? saddr : * (vm_offset_t *) saddr);
for (i = 0; i < number; i++) {
ipc_object_t object = objects[i];
mr |= ipc_kmsg_copyout_object_to_port(space, object,
name, (mach_port_t *)&objects[i]);
}
}
if (is_inline) {
((mach_msg_type_t*)type)->msgt_deallocate = FALSE;
saddr += length;
} else {
vm_offset_t data;
data = * (vm_offset_t *) saddr;
if (length == 0) {
assert(data == 0);
addr = 0;
} else if (is_port) {
if (sizeof(mach_port_name_t) != sizeof(mach_port_t)) {
mach_port_t *src = (mach_port_t*)data;
mach_port_name_t *dst = (mach_port_name_t*)addr;
for (int i=0; i<number; i++) {
if (copyout_port(src + i, dst + i)) {
kr = KERN_FAILURE;
goto vm_copyout_failure;
}
}
} else {
(void) copyoutmap(map, (char *) data,
(char *) addr, length);
}
kfree(data, length);
} else {
vm_map_copy_t copy = (vm_map_copy_t) data;
kr = vm_map_copyout(map, &addr, copy);
if (kr != KERN_SUCCESS) {
vm_map_copy_discard(copy);
vm_copyout_failure:
addr = 0;
if (longform)
type->msgtl_size = 0;
else
((mach_msg_type_t*)type)->msgt_size = 0;
if (kr == KERN_RESOURCE_SHORTAGE)
mr |= MACH_MSG_VM_KERNEL;
else
mr |= MACH_MSG_VM_SPACE;
}
}
((mach_msg_type_t*)type)->msgt_deallocate = TRUE;
* (vm_offset_t *) saddr = addr;
saddr += sizeof(vm_offset_t);
}
saddr = mach_msg_kernel_align(saddr);
}
return mr;
}
mach_msg_return_t
ipc_kmsg_copyout(
ipc_kmsg_t 	kmsg,
ipc_space_t 	space,
vm_map_t 	map,
mach_port_name_t 	notify)
{
mach_msg_bits_t mbits = kmsg->ikm_header.msgh_bits;
mach_msg_return_t mr;
mr = ipc_kmsg_copyout_header(&kmsg->ikm_header, space, notify);
if (mr != MACH_MSG_SUCCESS)
return mr;
if (mbits & MACH_MSGH_BITS_COMPLEX) {
mr = ipc_kmsg_copyout_body(kmsg, space, map);
if (mr != MACH_MSG_SUCCESS)
mr |= MACH_RCV_BODY_ERROR;
}
return mr;
}
mach_msg_return_t
ipc_kmsg_copyout_pseudo(
ipc_kmsg_t		kmsg,
ipc_space_t		space,
vm_map_t		map)
{
mach_msg_bits_t mbits = kmsg->ikm_header.msgh_bits;
ipc_object_t dest = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
ipc_object_t reply = (ipc_object_t) kmsg->ikm_header.msgh_local_port;
mach_msg_type_name_t dest_type = MACH_MSGH_BITS_REMOTE(mbits);
mach_msg_type_name_t reply_type = MACH_MSGH_BITS_LOCAL(mbits);
mach_port_name_t dest_name, reply_name;
mach_msg_return_t mr;
assert(IO_VALID(dest));
mr = (ipc_kmsg_copyout_object(space, dest, dest_type, &dest_name) |
ipc_kmsg_copyout_object(space, reply, reply_type, &reply_name));
kmsg->ikm_header.msgh_bits = mbits &~ MACH_MSGH_BITS_CIRCULAR;
kmsg->ikm_header.msgh_remote_port = dest_name;
kmsg->ikm_header.msgh_local_port = reply_name;
if (mbits & MACH_MSGH_BITS_COMPLEX) {
mr |= ipc_kmsg_copyout_body(kmsg, space, map);
}
return mr;
}
void
ipc_kmsg_copyout_dest(
ipc_kmsg_t 	kmsg,
ipc_space_t 	space)
{
mach_msg_bits_t mbits = kmsg->ikm_header.msgh_bits;
ipc_object_t dest = (ipc_object_t) kmsg->ikm_header.msgh_remote_port;
ipc_object_t reply = (ipc_object_t) kmsg->ikm_header.msgh_local_port;
mach_msg_type_name_t dest_type = MACH_MSGH_BITS_REMOTE(mbits);
mach_msg_type_name_t reply_type = MACH_MSGH_BITS_LOCAL(mbits);
mach_port_name_t dest_name, reply_name;
assert(IO_VALID(dest));
io_lock(dest);
if (io_active(dest)) {
ipc_object_copyout_dest(space, dest, dest_type, &dest_name);
} else {
io_release(dest);
io_check_unlock(dest);
dest_name = MACH_PORT_NAME_DEAD;
}
if (IO_VALID(reply)) {
ipc_object_destroy(reply, reply_type);
reply_name = MACH_PORT_NAME_NULL;
} else
reply_name = invalid_port_to_name((mach_port_t)reply);
kmsg->ikm_header.msgh_bits = (MACH_MSGH_BITS_OTHER(mbits) |
MACH_MSGH_BITS(reply_type, dest_type));
kmsg->ikm_header.msgh_local_port = dest_name;
kmsg->ikm_header.msgh_remote_port = reply_name;
if (mbits & MACH_MSGH_BITS_COMPLEX) {
vm_offset_t saddr, eaddr;
saddr = (vm_offset_t) (&kmsg->ikm_header + 1);
eaddr = (vm_offset_t) &kmsg->ikm_header +
kmsg->ikm_header.msgh_size;
ipc_kmsg_clean_body(saddr, eaddr);
}
}
#if	MACH_KDB
static char *
ipc_type_name(
int 		type_name,
boolean_t 	received)
{
switch (type_name) {
case MACH_MSG_TYPE_BOOLEAN:
return "boolean";
case MACH_MSG_TYPE_INTEGER_16:
return "short";
case MACH_MSG_TYPE_INTEGER_32:
return "int32";
case MACH_MSG_TYPE_INTEGER_64:
return "int64";
case MACH_MSG_TYPE_CHAR:
return "char";
case MACH_MSG_TYPE_BYTE:
return "byte";
case MACH_MSG_TYPE_REAL:
return "real";
case MACH_MSG_TYPE_STRING:
return "string";
case MACH_MSG_TYPE_PORT_NAME:
return "port_name";
case MACH_MSG_TYPE_MOVE_RECEIVE:
if (received) {
return "port_receive";
} else {
return "move_receive";
}
case MACH_MSG_TYPE_MOVE_SEND:
if (received) {
return "port_send";
} else {
return "move_send";
}
case MACH_MSG_TYPE_MOVE_SEND_ONCE:
if (received) {
return "port_send_once";
} else {
return "move_send_once";
}
case MACH_MSG_TYPE_COPY_SEND:
return "copy_send";
case MACH_MSG_TYPE_MAKE_SEND:
return "make_send";
case MACH_MSG_TYPE_MAKE_SEND_ONCE:
return "make_send_once";
default:
return (char *) 0;
}
}
static void
ipc_print_type_name(
int	type_name)
{
char *name = ipc_type_name(type_name, TRUE);
if (name) {
printf("%s", name);
} else {
printf("type%d", type_name);
}
}
void
ipc_kmsg_print(ipc_kmsg_t kmsg)
{
db_printf("kmsg=0x%x\n", kmsg);
db_printf("ikm_next=0x%x,prev=0x%x,size=%d,marequest=0x%x",
kmsg->ikm_next,
kmsg->ikm_prev,
kmsg->ikm_size,
kmsg->ikm_marequest);
db_printf("\n");
ipc_msg_print(&kmsg->ikm_header);
}
void
ipc_msg_print(mach_msg_header_t *msgh)
{
vm_offset_t saddr, eaddr;
db_printf("msgh_bits=0x%x: ", msgh->msgh_bits);
if (msgh->msgh_bits & MACH_MSGH_BITS_COMPLEX) {
db_printf("complex,");
}
if (msgh->msgh_bits & MACH_MSGH_BITS_CIRCULAR) {
db_printf("circular,");
}
if (msgh->msgh_bits & MACH_MSGH_BITS_COMPLEX_PORTS) {
db_printf("complex_ports,");
}
if (msgh->msgh_bits & MACH_MSGH_BITS_COMPLEX_DATA) {
db_printf("complex_data,");
}
if (msgh->msgh_bits & MACH_MSGH_BITS_MIGRATED) {
db_printf("migrated,");
}
if (msgh->msgh_bits & MACH_MSGH_BITS_UNUSED) {
db_printf("unused=0x%x,",
msgh->msgh_bits & MACH_MSGH_BITS_UNUSED);
}
db_printf("l=0x%x,r=0x%x\n",
MACH_MSGH_BITS_LOCAL(msgh->msgh_bits),
MACH_MSGH_BITS_REMOTE(msgh->msgh_bits));
db_printf("msgh_id=%d,size=%u,seqno=%d,",
msgh->msgh_id,
msgh->msgh_size,
msgh->msgh_seqno);
if (msgh->msgh_remote_port) {
db_printf("remote=0x%x(", msgh->msgh_remote_port);
ipc_print_type_name(MACH_MSGH_BITS_REMOTE(msgh->msgh_bits));
db_printf("),");
} else {
db_printf("remote=null,\n");
}
if (msgh->msgh_local_port) {
db_printf("local=0x%x(", msgh->msgh_local_port);
ipc_print_type_name(MACH_MSGH_BITS_LOCAL(msgh->msgh_bits));
db_printf(")\n");
} else {
db_printf("local=null\n");
}
saddr = (vm_offset_t) (msgh + 1);
eaddr = (vm_offset_t) msgh + msgh->msgh_size;
while (saddr < eaddr) {
mach_msg_type_long_t *type;
mach_msg_type_name_t name;
mach_msg_type_size_t size;
mach_msg_type_number_t number;
boolean_t is_inline, longform, dealloc, is_port;
vm_size_t length;
type = (mach_msg_type_long_t *) saddr;
if (((eaddr - saddr) < sizeof(mach_msg_type_t)) ||
((longform = ((mach_msg_type_t*)type)->msgt_longform) &&
((eaddr - saddr) < sizeof(mach_msg_type_long_t)))) {
db_printf("*** msg too small\n");
return;
}
is_inline = ((mach_msg_type_t*)type)->msgt_inline;
dealloc = ((mach_msg_type_t*)type)->msgt_deallocate;
if (longform) {
name = type->msgtl_name;
size = type->msgtl_size;
number = type->msgtl_number;
saddr += sizeof(mach_msg_type_long_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_long_t))) {
saddr = mach_msg_kernel_align(saddr);
}
} else {
name = ((mach_msg_type_t*)type)->msgt_name;
size = ((mach_msg_type_t*)type)->msgt_size;
number = ((mach_msg_type_t*)type)->msgt_number;
saddr += sizeof(mach_msg_type_t);
if (mach_msg_kernel_is_misaligned(sizeof(mach_msg_type_t))) {
saddr = mach_msg_kernel_align(saddr);
}
}
db_printf("-- type=");
ipc_print_type_name(name);
if (! is_inline) {
db_printf(",ool");
}
if (dealloc) {
db_printf(",dealloc");
}
if (longform) {
db_printf(",longform");
}
db_printf(",size=%d,number=%d,addr=0x%x\n",
size,
number,
saddr);
is_port = MACH_MSG_TYPE_PORT_ANY(name);
if ((is_port && (size != PORT_T_SIZE_IN_BITS)) ||
#ifndef __LP64__
(longform && ((type->msgtl_header.msgt_name != 0) ||
(type->msgtl_header.msgt_size != 0) ||
(type->msgtl_header.msgt_number != 0))) ||
#endif
(((mach_msg_type_t*)type)->msgt_unused != 0) ||
(dealloc && is_inline)) {
db_printf("*** invalid type\n");
return;
}
length = ((number * size) + 7) >> 3;
if (is_inline) {
vm_size_t amount;
unsigned i, numwords;
amount = (length + 3) &~ 3;
if ((eaddr - saddr) < amount) {
db_printf("*** too small\n");
return;
}
numwords = amount / sizeof(int);
if (numwords > 8) {
numwords = 8;
}
for (i = 0; i < numwords; i++) {
db_printf("0x%x\n", ((int *) saddr)[i]);
}
if (numwords < amount / sizeof(int)) {
db_printf("...\n");
}
saddr += amount;
} else {
if ((eaddr - saddr) < sizeof(vm_offset_t)) {
db_printf("*** too small\n");
return;
}
db_printf("0x%x\n", * (vm_offset_t *) saddr);
saddr += sizeof(vm_offset_t);
}
saddr = mach_msg_kernel_align(saddr);
}
}
#endif