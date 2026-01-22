#include <mach/port.h>
#include <mach/message.h>
#include <kern/assert.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/sched_prim.h>
#include <kern/ipc_sched.h>
#include <kern/ipc_kobject.h>
#include <ipc/copy_user.h>
#include <ipc/ipc_mqueue.h>
#include <ipc/ipc_thread.h>
#include <ipc/ipc_kmsg.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_pset.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_marequest.h>
void
ipc_mqueue_init(
ipc_mqueue_t mqueue)
{
imq_lock_init(mqueue);
ipc_kmsg_queue_init(&mqueue->imq_messages);
ipc_thread_queue_init(&mqueue->imq_threads);
}
void
ipc_mqueue_move(
ipc_mqueue_t dest,
ipc_mqueue_t source,
const ipc_port_t port)
{
ipc_kmsg_queue_t oldq, newq;
ipc_thread_queue_t blockedq;
ipc_kmsg_t kmsg, next;
ipc_thread_t th;
oldq = &source->imq_messages;
newq = &dest->imq_messages;
blockedq = &dest->imq_threads;
for (kmsg = ipc_kmsg_queue_first(oldq);
kmsg != IKM_NULL; kmsg = next) {
next = ipc_kmsg_queue_next(oldq, kmsg);
if (kmsg->ikm_header.msgh_remote_port != (mach_port_t) port)
continue;
ipc_kmsg_rmqueue(oldq, kmsg);
while ((th = ipc_thread_dequeue(blockedq)) != ITH_NULL) {
assert(ipc_kmsg_queue_empty(newq));
thread_go(th);
if (kmsg->ikm_header.msgh_size <= th->ith_msize) {
th->ith_state = MACH_MSG_SUCCESS;
th->ith_kmsg = kmsg;
th->ith_seqno = port->ip_seqno++;
goto next_kmsg;
}
th->ith_state = MACH_RCV_TOO_LARGE;
th->ith_msize = kmsg->ikm_header.msgh_size;
}
ipc_kmsg_enqueue(newq, kmsg);
next_kmsg:;
}
}
void
ipc_mqueue_changed(
ipc_mqueue_t mqueue,
mach_msg_return_t mr)
{
ipc_thread_t th;
while ((th = ipc_thread_dequeue(&mqueue->imq_threads)) != ITH_NULL) {
th->ith_state = mr;
thread_go(th);
}
}
mach_msg_return_t
ipc_mqueue_send(
ipc_kmsg_t kmsg,
mach_msg_option_t option,
mach_msg_timeout_t time_out)
{
ipc_port_t port;
port = (ipc_port_t) kmsg->ikm_header.msgh_remote_port;
assert(IP_VALID(port));
ip_lock(port);
if (port->ip_receiver == ipc_space_kernel) {
ipc_kmsg_t reply;
assert(ip_active(port));
ip_unlock(port);
reply = ipc_kobject_server(kmsg);
if (reply != IKM_NULL)
ipc_mqueue_send_always(reply);
return MACH_MSG_SUCCESS;
}
for (;;) {
ipc_thread_t self;
if (!ip_active(port)) {
ip_release(port);
ip_check_unlock(port);
kmsg->ikm_header.msgh_remote_port = MACH_PORT_NULL;
ipc_kmsg_destroy(kmsg);
return MACH_MSG_SUCCESS;
}
if ((port->ip_msgcount < port->ip_qlimit) ||
(option & MACH_SEND_ALWAYS) ||
(MACH_MSGH_BITS_REMOTE(kmsg->ikm_header.msgh_bits) ==
MACH_MSG_TYPE_PORT_SEND_ONCE))
break;
self = current_thread();
if (option & MACH_SEND_TIMEOUT) {
if (time_out == 0) {
ip_unlock(port);
return MACH_SEND_TIMED_OUT;
}
thread_will_wait_with_timeout(self, time_out);
} else
thread_will_wait(self);
ipc_thread_enqueue(&port->ip_blocked, self);
self->ith_state = MACH_SEND_IN_PROGRESS;
ip_unlock(port);
counter(c_ipc_mqueue_send_block++);
thread_block(thread_no_continuation);
ip_lock(port);
if (self->ith_state == MACH_MSG_SUCCESS)
continue;
assert(self->ith_state == MACH_SEND_IN_PROGRESS);
ipc_thread_rmqueue(&port->ip_blocked, self);
switch (self->ith_wait_result) {
case THREAD_INTERRUPTED:
ip_unlock(port);
return MACH_SEND_INTERRUPTED;
case THREAD_TIMED_OUT:
assert(option & MACH_SEND_TIMEOUT);
time_out = 0;
break;
case THREAD_RESTART:
default:
#if MACH_ASSERT
assert(!"ipc_mqueue_send");
#else
panic("ipc_mqueue_send");
#endif
}
}
if (kmsg->ikm_header.msgh_bits & MACH_MSGH_BITS_CIRCULAR) {
ip_unlock(port);
ipc_kmsg_destroy(kmsg);
return MACH_MSG_SUCCESS;
}
{
ipc_mqueue_t mqueue;
ipc_pset_t pset;
ipc_thread_t receiver;
ipc_thread_queue_t receivers;
port->ip_msgcount++;
assert(port->ip_msgcount > 0);
pset = port->ip_pset;
if (pset == IPS_NULL)
mqueue = &port->ip_messages;
else
mqueue = &pset->ips_messages;
imq_lock(mqueue);
receivers = &mqueue->imq_threads;
ip_unlock(port);
for (;;) {
receiver = ipc_thread_queue_first(receivers);
if (receiver == ITH_NULL) {
ipc_kmsg_enqueue_macro(&mqueue->imq_messages, kmsg);
imq_unlock(mqueue);
break;
}
ipc_thread_rmqueue_first_macro(receivers, receiver);
assert(ipc_kmsg_queue_empty(&mqueue->imq_messages));
if (kmsg->ikm_header.msgh_size <= receiver->ith_msize) {
receiver->ith_state = MACH_MSG_SUCCESS;
receiver->ith_kmsg = kmsg;
receiver->ith_seqno = port->ip_seqno++;
imq_unlock(mqueue);
thread_go(receiver);
break;
}
receiver->ith_state = MACH_RCV_TOO_LARGE;
receiver->ith_msize = kmsg->ikm_header.msgh_size;
thread_go(receiver);
}
}
current_task()->messages_sent++;
return MACH_MSG_SUCCESS;
}
mach_msg_return_t
ipc_mqueue_copyin(
ipc_space_t space,
mach_port_name_t name,
ipc_mqueue_t *mqueuep,
ipc_object_t *objectp)
{
ipc_entry_t entry;
ipc_entry_bits_t bits;
ipc_object_t object;
ipc_mqueue_t mqueue;
is_read_lock(space);
if (!space->is_active) {
is_read_unlock(space);
return MACH_RCV_INVALID_NAME;
}
entry = ipc_entry_lookup(space, name);
if (entry == IE_NULL) {
is_read_unlock(space);
return MACH_RCV_INVALID_NAME;
}
bits = entry->ie_bits;
object = entry->ie_object;
if (bits & MACH_PORT_TYPE_RECEIVE) {
ipc_port_t port;
ipc_pset_t pset;
port = (ipc_port_t) object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
is_read_unlock(space);
pset = port->ip_pset;
if (pset != IPS_NULL) {
ips_lock(pset);
if (ips_active(pset)) {
ips_unlock(pset);
ip_unlock(port);
return MACH_RCV_IN_SET;
}
ipc_pset_remove(pset, port);
ips_check_unlock(pset);
assert(port->ip_pset == IPS_NULL);
}
mqueue = &port->ip_messages;
} else if (bits & MACH_PORT_TYPE_PORT_SET) {
ipc_pset_t pset;
pset = (ipc_pset_t) object;
assert(pset != IPS_NULL);
ips_lock(pset);
assert(ips_active(pset));
assert(pset->ips_local_name == name);
is_read_unlock(space);
mqueue = &pset->ips_messages;
} else {
is_read_unlock(space);
return MACH_RCV_INVALID_NAME;
}
io_reference(object);
imq_lock(mqueue);
io_unlock(object);
*objectp = object;
*mqueuep = mqueue;
return MACH_MSG_SUCCESS;
}
mach_msg_return_t
ipc_mqueue_receive(
ipc_mqueue_t mqueue,
mach_msg_option_t option,
mach_msg_size_t max_size,
mach_msg_timeout_t time_out,
boolean_t resume,
continuation_t continuation,
ipc_kmsg_t *kmsgp,
mach_port_seqno_t *seqnop)
{
ipc_port_t port;
ipc_kmsg_t kmsg;
mach_port_seqno_t seqno;
{
ipc_kmsg_queue_t kmsgs = &mqueue->imq_messages;
ipc_thread_t self = current_thread();
if (resume)
goto after_thread_block;
for (;;) {
kmsg = ipc_kmsg_queue_first(kmsgs);
if (kmsg != IKM_NULL) {
if (msg_usize(&kmsg->ikm_header) > max_size) {
* (mach_msg_size_t *) kmsgp =
kmsg->ikm_header.msgh_size;
imq_unlock(mqueue);
return MACH_RCV_TOO_LARGE;
}
ipc_kmsg_rmqueue_first_macro(kmsgs, kmsg);
port = (ipc_port_t) kmsg->ikm_header.msgh_remote_port;
seqno = port->ip_seqno++;
break;
}
if (option & MACH_RCV_TIMEOUT) {
if (time_out == 0) {
imq_unlock(mqueue);
return MACH_RCV_TIMED_OUT;
}
thread_will_wait_with_timeout(self, time_out);
} else
thread_will_wait(self);
ipc_thread_enqueue_macro(&mqueue->imq_threads, self);
self->ith_state = MACH_RCV_IN_PROGRESS;
self->ith_msize = max_size;
imq_unlock(mqueue);
if (continuation != (void (*)(void)) 0) {
counter(c_ipc_mqueue_receive_block_user++);
} else {
counter(c_ipc_mqueue_receive_block_kernel++);
}
thread_block(continuation);
after_thread_block:
imq_lock(mqueue);
if (self->ith_state == MACH_MSG_SUCCESS) {
kmsg = self->ith_kmsg;
seqno = self->ith_seqno;
port = (ipc_port_t) kmsg->ikm_header.msgh_remote_port;
break;
}
switch (self->ith_state) {
case MACH_RCV_TOO_LARGE:
* (mach_msg_size_t *) kmsgp = self->ith_msize;
case MACH_RCV_PORT_DIED:
case MACH_RCV_PORT_CHANGED:
imq_unlock(mqueue);
return self->ith_state;
case MACH_RCV_IN_PROGRESS:
ipc_thread_rmqueue(&mqueue->imq_threads, self);
switch (self->ith_wait_result) {
case THREAD_INTERRUPTED:
imq_unlock(mqueue);
return MACH_RCV_INTERRUPTED;
case THREAD_TIMED_OUT:
assert(option & MACH_RCV_TIMEOUT);
time_out = 0;
break;
case THREAD_RESTART:
default:
#if MACH_ASSERT
assert(!"ipc_mqueue_receive");
#else
panic("ipc_mqueue_receive");
#endif
}
break;
default:
#if MACH_ASSERT
assert(!"ipc_mqueue_receive: strange ith_state");
#else
panic("ipc_mqueue_receive: strange ith_state");
#endif
}
}
imq_unlock(mqueue);
assert(msg_usize(&kmsg->ikm_header) <= max_size);
}
{
ipc_marequest_t marequest;
marequest = kmsg->ikm_marequest;
if (marequest != IMAR_NULL) {
ipc_marequest_destroy(marequest);
kmsg->ikm_marequest = IMAR_NULL;
}
assert((kmsg->ikm_header.msgh_bits & MACH_MSGH_BITS_CIRCULAR) == 0);
assert(port == (ipc_port_t) kmsg->ikm_header.msgh_remote_port);
ip_lock(port);
if (ip_active(port)) {
ipc_thread_queue_t senders;
ipc_thread_t sender;
assert(port->ip_msgcount > 0);
port->ip_msgcount--;
senders = &port->ip_blocked;
sender = ipc_thread_queue_first(senders);
if ((sender != ITH_NULL) &&
(port->ip_msgcount < port->ip_qlimit)) {
ipc_thread_rmqueue(senders, sender);
sender->ith_state = MACH_MSG_SUCCESS;
thread_go(sender);
}
}
ip_unlock(port);
}
current_task()->messages_received++;
*kmsgp = kmsg;
*seqnop = seqno;
return MACH_MSG_SUCCESS;
}