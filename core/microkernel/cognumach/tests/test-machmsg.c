#include <mach/message.h>
#include <mach/mach_types.h>
#include <mach/vm_param.h>
#include <syscalls.h>
#include <testlib.h>
#include <mach.user.h>
#include <mach_port.user.h>
#include <mach_host.user.h>
#define ECHO_MAX_BODY_LEN 256
static uint32_t align(uint32_t val, size_t aln)
{
aln--;
return (val + aln) & (~aln);
}
#define ALIGN_INLINE(val, n) { (val) = align((val), (n)); }
struct echo_params
{
mach_port_t tx_port;
mach_port_t rx_port;
mach_msg_size_t rx_size;
mach_msg_size_t rx_number;
};
void echo_thread (void *arg)
{
struct echo_params *params = arg;
int err;
struct message
{
mach_msg_header_t header;
char body[ECHO_MAX_BODY_LEN];
} message;
printf ("thread echo started\n");
for (mach_msg_size_t i=0; i<params->rx_number; i++)
{
message.header.msgh_local_port = params->rx_port;
message.header.msgh_size = sizeof (message);
err = mach_msg (&message.header,
MACH_RCV_MSG,
0, sizeof (message),
params->rx_port, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg echo rx");
printf("echo rx %d expected 5d\n",
message.header.msgh_size, params->rx_size);
ASSERT(message.header.msgh_size == params->rx_size,
"wrong size in echo rx");
message.header.msgh_local_port = MACH_PORT_NULL;
printf ("echo: msgh_id %d\n", message.header.msgh_id);
err = mach_msg (&message.header,
MACH_SEND_MSG,
message.header.msgh_size, 0,
MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg echo tx");
}
printf ("thread echo stopped\n");
thread_terminate (mach_thread_self ());
FAILURE("thread_terminate");
}
#define TEST_ITERATIONS 3
void
test_iterations (void)
{
mach_port_t port, receive;
int err;
struct message
{
mach_msg_header_t header;
mach_msg_type_t type;
char data[64];
} message;
err = mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &port);
ASSERT_RET(err, "mach_port_allocate");
err = mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &receive);
ASSERT_RET(err, "mach_port_allocate 2");
struct echo_params params;
params.rx_port = port;
params.tx_port = port;
params.rx_size = sizeof(message.header) + sizeof(message.type) + 5;
ALIGN_INLINE(params.rx_size, MACH_MSG_USER_ALIGNMENT);
params.rx_number = TEST_ITERATIONS;
test_thread_start (mach_task_self (), echo_thread, &params);
time_value_t start_time;
err = host_get_time (mach_host_self (), &start_time);
ASSERT_RET(err, "host_get_time");
for (int i = 0; i < TEST_ITERATIONS; i++)
{
struct message message;
memset (&message, 0, sizeof (message));
strcpy (message.data, "ciao");
size_t datalen = strlen (message.data) + 1;
message.header.msgh_bits
= MACH_MSGH_BITS (MACH_MSG_TYPE_MAKE_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE);
message.header.msgh_remote_port = port;
message.header.msgh_local_port = receive;
message.header.msgh_id = 123;
message.header.msgh_size = sizeof (message.header) + sizeof (message.type) + datalen;
ALIGN_INLINE(message.header.msgh_size, 4);
message.type.msgt_name = MACH_MSG_TYPE_STRING;
message.type.msgt_size = 8 * datalen;
message.type.msgt_number = 1;
message.type.msgt_inline = TRUE;
message.type.msgt_longform = FALSE;
message.type.msgt_deallocate = FALSE;
message.type.msgt_unused = 0;
err = mach_msg (&message.header,
MACH_SEND_MSG | MACH_RCV_MSG,
message.header.msgh_size,
sizeof (message),
receive,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg txrx");
}
time_value_t stop_time;
err = host_get_time (mach_host_self (), &stop_time);
ASSERT_RET(err, "host_get_time");
printf ("start: %d.%06d\n", start_time.seconds, start_time.microseconds);
printf ("stop: %d.%06d\n", stop_time.seconds, stop_time.microseconds);
}
void
run_test_simple(void *msg, mach_msg_size_t msglen, mach_msg_id_t msgid)
{
mach_msg_header_t *head = msg;
mach_port_t port, receive;
int err;
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &port);
ASSERT_RET(err, "syscall_mach_port_allocate");
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &receive);
ASSERT_RET(err, "syscall_mach_port_allocate 2");
struct echo_params params;
params.tx_port = MACH_PORT_NULL;
params.rx_port = port;
params.rx_size = msglen;
params.rx_number = 1;
test_thread_start (mach_task_self (), echo_thread, &params);
head->msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_MAKE_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE);
head->msgh_remote_port = port;
head->msgh_local_port = receive;
head->msgh_id = msgid;
head->msgh_size = msglen;
err = mach_msg (msg,
MACH_SEND_MSG | MACH_RCV_MSG,
msglen,
msglen,
receive,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg txrx");
printf("size in final rx: %d expected %d\n", head->msgh_size, msglen);
ASSERT(head->msgh_size == msglen, "wrong size in final rx");
}
void
run_test_simple_split(void *msg, mach_msg_size_t msglen, mach_msg_id_t msgid)
{
mach_msg_header_t *head = msg;
mach_port_t port, receive;
int err;
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &port);
ASSERT_RET(err, "syscall_mach_port_allocate");
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &receive);
ASSERT_RET(err, "syscall_mach_port_allocate 2");
struct echo_params params;
params.tx_port = receive;
params.rx_port = port;
params.rx_size = msglen;
params.rx_number = 1;
test_thread_start (mach_task_self (), echo_thread, &params);
head->msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_MAKE_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE);
head->msgh_remote_port = port;
head->msgh_local_port = receive;
head->msgh_id = msgid;
head->msgh_size = 0;
err = mach_msg (msg,
MACH_SEND_MSG,
msglen,
0,
MACH_PORT_NULL,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg tx");
memset(msg, 0, msglen);
err = mach_msg (msg,
MACH_RCV_MSG,
0,
msglen,
receive,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg rx");
printf("size in final rx: %d expected %d\n", head->msgh_size, msglen);
ASSERT(head->msgh_size == msglen, "wrong size in final rx");
}
void
run_test_simple_self(void *msg, mach_msg_size_t msglen, mach_msg_id_t msgid)
{
mach_msg_header_t *head = msg;
mach_port_t port, receive;
int err;
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &port);
ASSERT_RET(err, "syscall_mach_port_allocate");
head->msgh_bits
= MACH_MSGH_BITS (MACH_MSG_TYPE_MAKE_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE);
head->msgh_bits |= MACH_MSGH_BITS_COMPLEX;
head->msgh_remote_port = port;
head->msgh_local_port = port;
head->msgh_id = msgid;
head->msgh_size = msglen;
err = mach_msg (msg,
MACH_SEND_MSG | MACH_RCV_MSG,
msglen,
msglen,
port,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg txrx");
printf("size in final rx: %d expected %d\n", head->msgh_size, msglen);
ASSERT(head->msgh_size == msglen, "wrong size in final rx\n");
}
void
run_test_simple_self_split(void *msg, mach_msg_size_t msglen, mach_msg_id_t msgid)
{
mach_msg_header_t *head = msg;
mach_port_t port, receive;
int err;
err = syscall_mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE, &port);
ASSERT_RET(err, "syscall_mach_port_allocate");
head->msgh_bits
= MACH_MSGH_BITS (MACH_MSG_TYPE_MAKE_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE);
head->msgh_bits |= MACH_MSGH_BITS_COMPLEX;
head->msgh_remote_port = port;
head->msgh_local_port = port;
head->msgh_id = msgid;
head->msgh_size = msglen;
err = mach_msg (msg,
MACH_SEND_MSG,
msglen,
0,
port,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg tx");
memset(msg, 0, msglen);
err = mach_msg (msg,
MACH_RCV_MSG,
0,
msglen,
port,
MACH_MSG_TIMEOUT_NONE,
MACH_PORT_NULL);
ASSERT_RET(err, "mach_msg rx");
printf("size in final rx: %d expected %d\n", head->msgh_size, msglen);
ASSERT(head->msgh_size == msglen, "wrong size in final rx\n");
}
void test_msg_string(void)
{
struct message
{
mach_msg_header_t header;
mach_msg_type_t type;
char data[64];
} msg;
char *test_strings[] = {"123", "12345", "ciaociao"};
memset (&msg, 0, sizeof (struct message));
strcpy (msg.data, "ciao");
size_t datalen = strlen (msg.data) + 1;
int msgid = 123;
int msglen = sizeof (msg.header) + sizeof (msg.type) + datalen;
ALIGN_INLINE(msglen, MACH_MSG_USER_ALIGNMENT);
msg.type.msgt_name = MACH_MSG_TYPE_STRING;
msg.type.msgt_size = 8 * datalen;
msg.type.msgt_number = 1;
msg.type.msgt_inline = TRUE;
msg.type.msgt_longform = FALSE;
msg.type.msgt_deallocate = FALSE;
msg.type.msgt_unused = 0;
run_test_simple_self(&msg, msglen, msgid);
run_test_simple_self_split(&msg, msglen, msgid);
run_test_simple(&msg, msglen, msgid);
run_test_simple_split(&msg, msglen, msgid);
}
void test_msg_string2(void)
{
struct message
{
mach_msg_header_t header;
mach_msg_type_t type;
char data[10];
mach_msg_type_t type2;
char data2[5];
} msg;
const int len1 = 10;
const int len2 = 5;
memset (&msg, 0, sizeof (struct message));
int msgid = 123;
int msglen = sizeof (msg.header) + sizeof (msg.type) + len1;
ALIGN_INLINE(msglen, MACH_MSG_USER_ALIGNMENT);
msglen += sizeof (msg.type2) + len2;
ALIGN_INLINE(msglen, MACH_MSG_USER_ALIGNMENT);
msg.type.msgt_name = MACH_MSG_TYPE_STRING;
msg.type.msgt_size = 8 * len1;
msg.type.msgt_number = 1;
msg.type.msgt_inline = TRUE;
msg.type.msgt_longform = FALSE;
msg.type.msgt_deallocate = FALSE;
msg.type.msgt_unused = 0;
memset (msg.data, 'c', len1);
msg.type2.msgt_name = MACH_MSG_TYPE_CHAR;
msg.type2.msgt_size = 8;
msg.type2.msgt_number = len2;
msg.type2.msgt_inline = TRUE;
msg.type2.msgt_longform = FALSE;
msg.type2.msgt_deallocate = FALSE;
msg.type2.msgt_unused = 0;
memset (msg.data2, 'x', len2);
run_test_simple_self(&msg, msglen, msgid);
run_test_simple_self_split(&msg, msglen, msgid);
run_test_simple(&msg, msglen, msgid);
run_test_simple_split(&msg, msglen, msgid);
}
void test_msg_ports(void)
{
struct message
{
mach_msg_header_t head;
mach_msg_type_t type;
mach_port_t *portp;
} msg;
mach_port_t msgports[3];
memset (&msg, 0, sizeof (struct message));
int msgid = 123;
int msglen = sizeof (msg.head) + sizeof (msg.type) + sizeof(msg.portp);
msg.type.msgt_name = MACH_MSG_TYPE_MOVE_SEND;
msg.type.msgt_size = 8*sizeof(mach_port_t);
msg.type.msgt_number = 3;
msg.type.msgt_inline = FALSE;
msg.type.msgt_longform = FALSE;
msg.type.msgt_deallocate = FALSE;
msg.type.msgt_unused = 0;
msg.portp = msgports;
msgports[0] = mach_host_self();
msgports[1] = mach_task_self();
msgports[2] = mach_thread_self();
run_test_simple_self(&msg, msglen, msgid);
run_test_simple_self_split(&msg, msglen, msgid);
run_test_simple(&msg, msglen, msgid);
run_test_simple_split(&msg, msglen, msgid);
}
void test_msg_emptydesc(void)
{
struct message
{
mach_msg_header_t header;
mach_msg_type_t type_empty;
vm_offset_t addr_empty;
mach_msg_type_t type;
char data[64];
} msg;
memset (&msg, 0, sizeof (struct message));
strcpy (msg.data, "ciao");
size_t datalen = strlen (msg.data) + 1;
int msgid = 123;
int msglen = sizeof (msg.header);
msglen += sizeof (msg.type_empty)+ sizeof (msg.addr_empty);
msglen += sizeof (msg.type) + datalen;
ALIGN_INLINE(msglen, MACH_MSG_USER_ALIGNMENT);
msg.type_empty.msgt_name = MACH_MSG_TYPE_STRING;
msg.type_empty.msgt_size = 8;
msg.type_empty.msgt_number = 0;
msg.type_empty.msgt_inline = FALSE;
msg.type_empty.msgt_longform = FALSE;
msg.type_empty.msgt_deallocate = FALSE;
msg.type_empty.msgt_unused = 0;
msg.addr_empty = 0;
msg.type.msgt_name = MACH_MSG_TYPE_STRING;
msg.type.msgt_size = 8 * datalen;
msg.type.msgt_number = 1;
msg.type.msgt_inline = TRUE;
msg.type.msgt_longform = FALSE;
msg.type.msgt_deallocate = FALSE;
msg.type.msgt_unused = 0;
run_test_simple_self(&msg, msglen, msgid);
run_test_simple_self_split(&msg, msglen, msgid);
run_test_simple(&msg, msglen, msgid);
run_test_simple_split(&msg, msglen, msgid);
}
void recv_to_be_interrupted(void *arg)
{
mach_msg_header_t msg;
kern_return_t ret;
mach_port_t rcv_name;
long err = (long)arg;
ret = mach_port_allocate(mach_task_self (), MACH_PORT_RIGHT_RECEIVE, &rcv_name);
ASSERT_RET(ret, "creating rx port");
ret = mach_msg(&msg, MACH_RCV_MSG,
0, sizeof(msg), rcv_name,
MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
printf("mach_msg returned %x\n", ret);
ASSERT(ret == err, "recv not interrupted correctly");
thread_terminate(mach_thread_self());
FAILURE("thread_terminate");
}
void test_recv_interrupted(void)
{
kern_return_t ret;
thread_t th;
th = test_thread_start(mach_task_self(), recv_to_be_interrupted, (void*)MACH_RCV_INTERRUPTED);
msleep(100);
ret = thread_suspend(th);
ASSERT_RET(ret, "thread_suspend");
ret = thread_abort(th);
ASSERT_RET(ret, "thread_abort");
ret = thread_resume(th);
ASSERT_RET(ret, "thread_resume");
wait_thread_terminated(th);
}
void test_recv_interrupted_setreturn(void)
{
kern_return_t ret;
thread_t th;
th = test_thread_start(mach_task_self(), recv_to_be_interrupted, (void*)123L);
msleep(100);
ret = thread_suspend(th);
ASSERT_RET(ret, "thread_suspend");
ret = thread_abort(th);
ASSERT_RET(ret, "thread_abort");
struct i386_thread_state state;
unsigned int count;
count = i386_THREAD_STATE_COUNT;
ret = thread_get_state(th, i386_REGS_SEGS_STATE,
(thread_state_t) &state, &count);
ASSERT_RET(ret, "thread_get_state()");
#ifdef __i386__
state.eax = 123;
#elif defined(__x86_64__)
state.rax = 123;
#endif
ret = thread_set_state(th, i386_REGS_SEGS_STATE,
(thread_state_t) &state, i386_THREAD_STATE_COUNT);
ASSERT_RET(ret, "thread_set_state");
ret = thread_resume(th);
ASSERT_RET(ret, "thread_resume");
wait_thread_terminated(th);
}
int
main (int argc, char *argv[], int envc, char *envp[])
{
printf("test_msg_string()\n");
test_msg_string();
printf("test_msg_string2()\n");
test_msg_string2();
printf("test_msg_ports()\n");
test_msg_ports();
printf("test_msg_emptydesc()\n");
test_msg_emptydesc();
printf("test_iters()\n");
test_iterations();
printf("test_recv_interrupted()\n");
test_recv_interrupted();
test_recv_interrupted_setreturn();
return 0;
}