#include <pthread.h>
#include <stdlib.h>
#include <hurd/hurd_types.h>
#include <mach/message.h>
#include <string.h>
#include <assert-backtrace.h>
#include <stdio.h>
#include "proc.h"
#define RPCID_SIG_POST 23000
struct msg_sig_post_request
{
mach_msg_header_t head;
mach_msg_type_t signaltype;
int signal;
mach_msg_type_t sigcode_type;
natural_t sigcode;
mach_msg_type_t refporttype;
mach_port_name_inlined_t refport;
};
static void *
blocking_message_send (void *arg)
{
struct msg_sig_post_request *const req = arg;
error_t err;
pthread_setname_np (pthread_self (), "message_send");
err = mach_msg (&req->head, MACH_SEND_MSG, sizeof *req, 0,
MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
switch (err)
{
case MACH_SEND_TIMED_OUT:
case MACH_SEND_INTERRUPTED:
case MACH_SEND_INVALID_NOTIFY:
case MACH_SEND_NO_NOTIFY:
case MACH_SEND_NOTIFY_IN_PROGRESS:
assert_perror_backtrace (err);
break;
default:
break;
}
free (req);
return 0;
}
void
send_signal (mach_port_t msgport,
int signal,
int sigcode,
mach_port_t refport)
{
error_t err;
struct msg_sig_post_request message =
{
.head = {
.msgh_bits = (MACH_MSGH_BITS_COMPLEX
| MACH_MSGH_BITS (MACH_MSG_TYPE_COPY_SEND,
MACH_MSG_TYPE_MAKE_SEND_ONCE)),
.msgh_size = sizeof message,
.msgh_remote_port = msgport,
.msgh_local_port = MACH_PORT_NULL,
.msgh_seqno = 0,
.msgh_id = RPCID_SIG_POST,
},
.signaltype = {
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
},
.signal = signal,
.sigcode_type = {
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
},
.sigcode = sigcode,
.refporttype = {
.msgt_name = MACH_MSG_TYPE_COPY_SEND,
.msgt_size = 8 * sizeof(mach_port_name_inlined_t),
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
},
.refport = {
.name = refport
}
};
err = mach_msg ((mach_msg_header_t *)&message,
MACH_SEND_MSG|MACH_SEND_TIMEOUT, sizeof message, 0,
MACH_PORT_NULL, 0, MACH_PORT_NULL);
switch (err)
{
case MACH_SEND_TIMED_OUT:
{
struct msg_sig_post_request *copy = malloc (sizeof *copy);
if (copy)
{
pthread_t thread;
error_t err;
memcpy (copy, &message, sizeof message);
err = pthread_create (&thread, NULL, blocking_message_send, copy);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
}
break;
}
case MACH_SEND_INTERRUPTED:
case MACH_SEND_INVALID_NOTIFY:
case MACH_SEND_NO_NOTIFY:
case MACH_SEND_NOTIFY_IN_PROGRESS:
assert_perror_backtrace (err);
break;
default:
break;
}
}