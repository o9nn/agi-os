#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/message.h>
#include <mach/port.h>
#include <mach/mig_errors.h>
#include <machine/locore.h>
#include <ipc/port.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_notify.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_pset.h>
#include <ipc/mach_msg.h>
#include <ipc/ipc_machdep.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/ipc_tt.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/processor.h>
#include <kern/printf.h>
#include <kern/sched.h>
#include <kern/sched_prim.h>
#include <kern/exception.h>
#include <kern/macros.h>
#include <kern/constants.h>
#include <mach/machine/vm_types.h>
#if MACH_KDB
#include <machine/trap.h>
#include <ddb/db_output.h>
boolean_t debug_user_with_kdb = FALSE;
#endif
#ifdef KEEP_STACKS
#endif
void
exception(
integer_t _exception,
integer_t code,
long_integer_t subcode)
{
ipc_thread_t self = current_thread();
ipc_port_t exc_port;
if (_exception == KERN_SUCCESS)
panic("exception");
ith_lock(self);
assert(self->ith_self != IP_NULL);
exc_port = self->ith_exception;
if (!IP_VALID(exc_port)) {
ith_unlock(self);
exception_try_task(_exception, code, subcode);
}
ip_lock(exc_port);
ith_unlock(self);
if (!ip_active(exc_port)) {
ip_unlock(exc_port);
exception_try_task(_exception, code, subcode);
}
ip_reference(exc_port);
exc_port->ip_srights++;
ip_unlock(exc_port);
self->ith_exc = _exception;
self->ith_exc_code = code;
self->ith_exc_subcode = subcode;
exception_raise(exc_port,
retrieve_thread_self_fast(self),
retrieve_task_self_fast(self->task),
_exception, code, subcode);
}
void
exception_try_task(
integer_t _exception,
integer_t code,
long_integer_t subcode)
{
ipc_thread_t self = current_thread();
task_t task = self->task;
ipc_port_t exc_port;
itk_lock(task);
assert(task->itk_self != IP_NULL);
exc_port = task->itk_exception;
if (!IP_VALID(exc_port)) {
itk_unlock(task);
exception_no_server();
}
ip_lock(exc_port);
itk_unlock(task);
if (!ip_active(exc_port)) {
ip_unlock(exc_port);
exception_no_server();
}
ip_reference(exc_port);
exc_port->ip_srights++;
ip_unlock(exc_port);
self->ith_exc = KERN_SUCCESS;
exception_raise(exc_port,
retrieve_thread_self_fast(self),
retrieve_task_self_fast(task),
_exception, code, subcode);
}
void
exception_no_server(void)
{
ipc_thread_t self = current_thread();
while (thread_should_halt(self))
thread_halt_self(thread_exception_return);
#if 0
if (thread_suspend (self) == KERN_SUCCESS)
thread_exception_return ();
#endif
#if MACH_KDB
if (debug_user_with_kdb) {
db_printf("No exception server, calling kdb...\n");
thread_kdb_return();
}
#endif
(void) task_terminate(self->task);
thread_halt_self(thread_exception_return);
panic("terminating the task didn't kill us");
}
#define MACH_EXCEPTION_ID MACH_EXCEPTION_BASE_ID
#define MACH_EXCEPTION_REPLY_ID (MACH_EXCEPTION_ID + MACH_EXCEPTION_REPLY_OFFSET)
struct mach_exception {
mach_msg_header_t Head;
mach_msg_type_t threadType;
mach_port_t thread;
mach_msg_type_t taskType;
mach_port_t task;
mach_msg_type_t exceptionType;
integer_t exception;
mach_msg_type_t codeType;
integer_t code;
mach_msg_type_t subcodeType;
rpc_long_integer_t subcode;
};
#define INTEGER_T_SIZE_IN_BITS (8 * sizeof(integer_t))
#define INTEGER_T_TYPE MACH_MSG_TYPE_INTEGER_T
#define RPC_LONG_INTEGER_T_SIZE_IN_BITS (8 * sizeof(rpc_long_integer_t))
#if defined(__LP64__) && !defined(USER32)
#define RPC_LONG_INTEGER_T_TYPE MACH_MSG_TYPE_INTEGER_64
#else
#define RPC_LONG_INTEGER_T_TYPE MACH_MSG_TYPE_INTEGER_32
#endif
mach_msg_type_t exc_port_proto = {
.msgt_name = MACH_MSG_TYPE_PORT_SEND,
.msgt_size = PORT_T_SIZE_IN_BITS,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
mach_msg_type_t exc_code_proto = {
.msgt_name = INTEGER_T_TYPE,
.msgt_size = INTEGER_T_SIZE_IN_BITS,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
mach_msg_type_t exc_subcode_proto = {
.msgt_name = RPC_LONG_INTEGER_T_TYPE,
.msgt_size = RPC_LONG_INTEGER_T_SIZE_IN_BITS,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
int exception_raise_misses = 0;
void
exception_raise(
ipc_port_t dest_port,
ipc_port_t thread_port,
ipc_port_t task_port,
integer_t _exception,
integer_t code,
long_integer_t subcode)
{
ipc_thread_t self = current_thread();
ipc_thread_t receiver;
ipc_port_t reply_port;
ipc_mqueue_t dest_mqueue;
ipc_mqueue_t reply_mqueue;
ipc_kmsg_t kmsg;
mach_msg_return_t mr;
assert(IP_VALID(dest_port));
kmsg = ikm_cache_alloc();
if (kmsg == IKM_NULL)
panic("exception_raise");
ith_lock(self);
assert(self->ith_self != IP_NULL);
reply_port = self->ith_rpc_reply;
if (reply_port == IP_NULL) {
ith_unlock(self);
reply_port = ipc_port_alloc_reply();
ith_lock(self);
if ((reply_port == IP_NULL) ||
(self->ith_rpc_reply != IP_NULL))
panic("exception_raise");
self->ith_rpc_reply = reply_port;
}
ip_lock(reply_port);
assert(ip_active(reply_port));
ith_unlock(self);
reply_port->ip_sorights++;
ip_reference(reply_port);
ip_reference(reply_port);
self->ith_port = reply_port;
reply_mqueue = &reply_port->ip_messages;
imq_lock(reply_mqueue);
assert(ipc_kmsg_queue_empty(&reply_mqueue->imq_messages));
ip_unlock(reply_port);
if (!ip_lock_try(dest_port)) {
imq_unlock(reply_mqueue);
goto slow_exception_raise;
}
if (!ip_active(dest_port) ||
(dest_port->ip_receiver == ipc_space_kernel)) {
imq_unlock(reply_mqueue);
ip_unlock(dest_port);
goto slow_exception_raise;
}
{
ipc_pset_t dest_pset;
dest_pset = dest_port->ip_pset;
if (dest_pset == IPS_NULL)
dest_mqueue = &dest_port->ip_messages;
else
dest_mqueue = &dest_pset->ips_messages;
}
if (!imq_lock_try(dest_mqueue)) {
imq_unlock(reply_mqueue);
ip_unlock(dest_port);
goto slow_exception_raise;
}
ip_unlock(dest_port);
receiver = ipc_thread_queue_first(&dest_mqueue->imq_threads);
if ((receiver == ITH_NULL) ||
!((receiver->swap_func == mach_msg_continue) ||
((receiver->swap_func == mach_msg_receive_continue) &&
(sizeof(struct mach_exception) <= receiver->ith_msize) &&
((receiver->ith_option & MACH_RCV_NOTIFY) == 0))) ||
!thread_handoff(self, exception_raise_continue, receiver)) {
imq_unlock(reply_mqueue);
imq_unlock(dest_mqueue);
goto slow_exception_raise;
}
counter(c_exception_raise_block++);
assert(current_thread() == receiver);
ipc_thread_enqueue_macro(&reply_mqueue->imq_threads, self);
self->ith_state = MACH_RCV_IN_PROGRESS;
self->ith_msize = MACH_MSG_SIZE_MAX;
imq_unlock(reply_mqueue);
ipc_thread_rmqueue_first_macro(
&dest_mqueue->imq_threads, receiver);
imq_unlock(dest_mqueue);
{
ipc_object_t object = receiver->ith_object;
io_lock(object);
io_release(object);
io_check_unlock(object);
}
{
struct mach_exception *exc =
(struct mach_exception *) &kmsg->ikm_header;
ipc_space_t space = receiver->task->itk_space;
exc->Head.msgh_bits = (MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE,
MACH_MSG_TYPE_PORT_SEND) |
MACH_MSGH_BITS_COMPLEX);
exc->Head.msgh_size = sizeof *exc;
exc->Head.msgh_seqno = 0;
exc->Head.msgh_id = MACH_EXCEPTION_ID;
exc->threadType = exc_port_proto;
exc->taskType = exc_port_proto;
exc->exceptionType = exc_code_proto;
exc->exception = _exception;
exc->codeType = exc_code_proto;
exc->code = code;
exc->subcodeType = exc_subcode_proto;
exc->subcode = subcode;
if (receiver->ith_rcv_size < sizeof(struct mach_exception)) {
exc->Head.msgh_bits =
(MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND,
MACH_MSG_TYPE_PORT_SEND_ONCE) |
MACH_MSGH_BITS_COMPLEX);
exc->Head.msgh_remote_port = (mach_port_t) dest_port;
exc->Head.msgh_local_port = (mach_port_t) reply_port;
exc->thread = (mach_port_t) thread_port;
exc->task = (mach_port_t) task_port;
ipc_kmsg_destroy(kmsg);
thread_syscall_return(MACH_RCV_TOO_LARGE);
}
is_write_lock(space);
assert(space->is_active);
ip_lock(dest_port);
if (!ip_active(dest_port) ||
!ip_lock_try(reply_port)) {
abort_copyout:
ip_unlock(dest_port);
is_write_unlock(space);
exc->Head.msgh_bits =
(MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND,
MACH_MSG_TYPE_PORT_SEND_ONCE) |
MACH_MSGH_BITS_COMPLEX);
exc->Head.msgh_remote_port = (mach_port_t) dest_port;
exc->Head.msgh_local_port = (mach_port_t) reply_port;
mr = ipc_kmsg_copyout_header(&exc->Head, space,
MACH_PORT_NULL);
if (mr == MACH_MSG_SUCCESS)
goto copyout_body;
exc->thread = (mach_port_t) thread_port;
exc->task = (mach_port_t) task_port;
ipc_kmsg_copyout_dest(kmsg, space);
(void) ipc_kmsg_put(receiver->ith_msg, kmsg,
sizeof(mach_msg_header_t));
thread_syscall_return(mr);
}
if (!ip_active(reply_port)) {
ip_unlock(reply_port);
goto abort_copyout;
}
assert(reply_port->ip_sorights > 0);
ip_unlock(reply_port);
{
kern_return_t kr;
ipc_entry_t entry;
mach_port_name_t port_name;
kr = ipc_entry_get (space, &port_name, &entry);
if (kr)
goto abort_copyout;
exc->Head.msgh_remote_port = (mach_port_t) port_name;
{
mach_port_gen_t gen;
assert((entry->ie_bits &~ IE_BITS_GEN_MASK) == 0);
gen = entry->ie_bits + IE_BITS_GEN_ONE;
entry->ie_bits = gen | (MACH_PORT_TYPE_SEND_ONCE | 1);
}
entry->ie_object = (ipc_object_t) reply_port;
is_write_unlock(space);
}
assert(dest_port->ip_srights > 0);
ip_release(dest_port);
exc->Head.msgh_local_port =
((dest_port->ip_receiver == space) ?
dest_port->ip_receiver_name : MACH_PORT_NULL);
if ((--dest_port->ip_srights == 0) &&
(dest_port->ip_nsrequest != IP_NULL)) {
ipc_port_t nsrequest;
mach_port_mscount_t mscount;
nsrequest = dest_port->ip_nsrequest;
mscount = dest_port->ip_mscount;
dest_port->ip_nsrequest = IP_NULL;
ip_unlock(dest_port);
ipc_notify_no_senders(nsrequest, mscount);
} else
ip_unlock(dest_port);
copyout_body:
mr = (ipc_kmsg_copyout_object_to_port(space, (ipc_object_t) thread_port,
MACH_MSG_TYPE_PORT_SEND, &exc->thread) |
ipc_kmsg_copyout_object_to_port(space, (ipc_object_t) task_port,
MACH_MSG_TYPE_PORT_SEND, &exc->task));
if (mr != MACH_MSG_SUCCESS) {
(void) ipc_kmsg_put(receiver->ith_msg, kmsg,
kmsg->ikm_header.msgh_size);
thread_syscall_return(mr | MACH_RCV_BODY_ERROR);
}
}
ikm_check_initialized(kmsg, kmsg->ikm_size);
assert(kmsg->ikm_size == IKM_SAVED_KMSG_SIZE);
if (copyoutmsg(&kmsg->ikm_header, receiver->ith_msg,
sizeof(struct mach_exception))) {
mr = ipc_kmsg_put(receiver->ith_msg, kmsg,
kmsg->ikm_header.msgh_size);
thread_syscall_return(mr);
}
if (!ikm_cache_free_try(kmsg)) {
mr = ipc_kmsg_put(receiver->ith_msg, kmsg,
kmsg->ikm_header.msgh_size);
thread_syscall_return(mr);
}
thread_syscall_return(MACH_MSG_SUCCESS);
#ifndef __GNUC__
return;
#endif
slow_exception_raise: {
struct mach_exception *exc =
(struct mach_exception *) &kmsg->ikm_header;
ipc_kmsg_t reply_kmsg;
mach_port_seqno_t reply_seqno;
exception_raise_misses++;
exc->Head.msgh_bits = (MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND,
MACH_MSG_TYPE_PORT_SEND_ONCE) |
MACH_MSGH_BITS_COMPLEX);
exc->Head.msgh_size = sizeof *exc;
exc->Head.msgh_remote_port = (mach_port_t) dest_port;
exc->Head.msgh_local_port = (mach_port_t) reply_port;
exc->Head.msgh_seqno = 0;
exc->Head.msgh_id = MACH_EXCEPTION_ID;
exc->threadType = exc_port_proto;
exc->thread = (mach_port_t) thread_port;
exc->taskType = exc_port_proto;
exc->task = (mach_port_t) task_port;
exc->exceptionType = exc_code_proto;
exc->exception = _exception;
exc->codeType = exc_code_proto;
exc->code = code;
exc->subcodeType = exc_subcode_proto;
exc->subcode = subcode;
ipc_mqueue_send_always(kmsg);
ip_lock(reply_port);
if (!ip_active(reply_port)) {
ip_unlock(reply_port);
exception_raise_continue_slow(MACH_RCV_PORT_DIED, IKM_NULL, 0);
}
imq_lock(reply_mqueue);
ip_unlock(reply_port);
mr = ipc_mqueue_receive(reply_mqueue, MACH_MSG_OPTION_NONE,
MACH_MSG_SIZE_MAX,
MACH_MSG_TIMEOUT_NONE,
FALSE, exception_raise_continue,
&reply_kmsg, &reply_seqno);
exception_raise_continue_slow(mr, reply_kmsg, reply_seqno);
}
}
#define BAD_TYPECHECK(type, check) unlikely (({\
union { mach_msg_type_t t; uint32_t w; } _t, _c;\
_t.t = *(type); _c.t = *(check);_t.w != _c.w; }))
mach_msg_type_t exc_RetCode_proto = {
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
kern_return_t
exception_parse_reply(ipc_kmsg_t kmsg)
{
mig_reply_header_t *msg =
(mig_reply_header_t *) &kmsg->ikm_header;
kern_return_t kr;
if ((msg->Head.msgh_bits !=
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND_ONCE, 0)) ||
(msg->Head.msgh_size != sizeof *msg) ||
(msg->Head.msgh_id != MACH_EXCEPTION_REPLY_ID) ||
(BAD_TYPECHECK(&msg->RetCodeType, &exc_RetCode_proto))) {
kmsg->ikm_header.msgh_remote_port = MACH_PORT_NULL;
ipc_kmsg_destroy(kmsg);
return MIG_REPLY_MISMATCH;
}
kr = msg->RetCode;
ikm_cache_free(kmsg);
return kr;
}
void
exception_raise_continue(void)
{
ipc_thread_t self = current_thread();
ipc_port_t reply_port = self->ith_port;
ipc_mqueue_t reply_mqueue = &reply_port->ip_messages;
ipc_kmsg_t kmsg;
mach_port_seqno_t seqno;
mach_msg_return_t mr;
mr = ipc_mqueue_receive(reply_mqueue, MACH_MSG_OPTION_NONE,
MACH_MSG_SIZE_MAX,
MACH_MSG_TIMEOUT_NONE,
TRUE, exception_raise_continue,
&kmsg, &seqno);
exception_raise_continue_slow(mr, kmsg, seqno);
}
static void
thread_release_and_exception_return(void)
{
ipc_thread_t self = current_thread();
ipc_port_release(self->ith_port);
thread_exception_return();
}
void
exception_raise_continue_slow(
mach_msg_return_t mr,
ipc_kmsg_t kmsg,
mach_port_seqno_t seqno)
{
ipc_thread_t self = current_thread();
ipc_port_t reply_port = self->ith_port;
ipc_mqueue_t reply_mqueue = &reply_port->ip_messages;
while (mr == MACH_RCV_INTERRUPTED) {
while (thread_should_halt(self)) {
if (self->ast & AST_TERMINATE)
ipc_port_release(reply_port);
thread_halt_self(thread_release_and_exception_return);
}
ip_lock(reply_port);
if (!ip_active(reply_port)) {
ip_unlock(reply_port);
mr = MACH_RCV_PORT_DIED;
break;
}
imq_lock(reply_mqueue);
ip_unlock(reply_port);
mr = ipc_mqueue_receive(reply_mqueue, MACH_MSG_OPTION_NONE,
MACH_MSG_SIZE_MAX,
MACH_MSG_TIMEOUT_NONE,
FALSE, exception_raise_continue,
&kmsg, &seqno);
}
ipc_port_release(reply_port);
assert((mr == MACH_MSG_SUCCESS) ||
(mr == MACH_RCV_PORT_DIED));
if (mr == MACH_MSG_SUCCESS) {
ipc_port_release_sonce(reply_port);
mr = exception_parse_reply(kmsg);
}
if ((mr == KERN_SUCCESS) ||
(mr == MACH_RCV_PORT_DIED)) {
thread_exception_return();
}
if (self->ith_exc != KERN_SUCCESS) {
exception_try_task(self->ith_exc,
self->ith_exc_code,
self->ith_exc_subcode);
}
exception_no_server();
}
void
exception_raise_continue_fast(
ipc_port_t reply_port,
ipc_kmsg_t kmsg)
{
ipc_thread_t self = current_thread();
kern_return_t kr;
assert(ip_active(reply_port));
assert(reply_port == self->ith_port);
assert(reply_port == (ipc_port_t) kmsg->ikm_header.msgh_remote_port);
assert(MACH_MSGH_BITS_REMOTE(kmsg->ikm_header.msgh_bits) ==
MACH_MSG_TYPE_PORT_SEND_ONCE);
reply_port->ip_sorights--;
ip_release(reply_port);
ip_release(reply_port);
ip_unlock(reply_port);
kr = exception_parse_reply(kmsg);
if (kr == KERN_SUCCESS) {
thread_exception_return();
}
if (self->ith_exc != KERN_SUCCESS) {
exception_try_task(self->ith_exc,
self->ith_exc_code,
self->ith_exc_subcode);
}
exception_no_server();
}