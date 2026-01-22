#include <hurd.h>
#include <mach/notify.h>
#include <mach.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <assert-backtrace.h>
#include "fshelp.h"
struct fsys_startup_request
{
mach_msg_header_t head;
mach_msg_type_t flagsType;
int flags;
mach_msg_type_t control_portType;
mach_port_name_inlined_t control_port;
};
struct fsys_startup_reply
{
mach_msg_header_t head;
mach_msg_type_t RetCodeType;
kern_return_t RetCode;
mach_msg_type_t realnodeType;
mach_port_name_inlined_t realnode;
};
static error_t
service_fsys_startup (fshelp_open_fn_t underlying_open_fn, void *cookie,
mach_port_t port, long timeout, fsys_t *control,
task_t task)
{
const mach_msg_type_t flagsCheck =
{
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
const mach_msg_type_t control_portCheck =
{
.msgt_name = MACH_MSG_TYPE_PORT_SEND,
.msgt_size = 8 * sizeof(mach_port_name_inlined_t),
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
const mach_msg_type_t RetCodeType =
{
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
const mach_msg_type_t realnodeType =
{
.msgt_name = (unsigned char) MACH_MSG_TYPE_POLYMORPHIC,
.msgt_size = 8 * sizeof(mach_port_name_inlined_t),
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
inline int type_check (const mach_msg_type_t *type,
const mach_msg_type_t *check)
{
union
{
uintptr_t word;
mach_msg_type_t type;
} t, c;
t.type = *type;
c.type = *check;
return t.word != c.word;
}
error_t err;
union
{
mach_msg_header_t head;
struct fsys_startup_request startup;
}
request;
struct fsys_startup_reply reply;
err = mach_msg (&request.head, (MACH_RCV_MSG | MACH_RCV_INTERRUPT
| (timeout ? MACH_RCV_TIMEOUT : 0)),
0, sizeof(request), port, timeout, MACH_PORT_NULL);
if (err)
return err;
if (request.head.msgh_id == MACH_NOTIFY_NO_SENDERS)
return EDIED;
reply.head.msgh_size = sizeof(reply);
reply.head.msgh_bits =
MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(request.head.msgh_bits), 0);
reply.head.msgh_remote_port = request.head.msgh_remote_port;
reply.head.msgh_local_port = MACH_PORT_NULL;
reply.head.msgh_seqno = 0;
reply.head.msgh_id = request.head.msgh_id + 100;
reply.RetCodeType = RetCodeType;
if (request.head.msgh_id != 22000)
reply.RetCode = MIG_BAD_ID;
else if (type_check (&request.startup.control_portType, &control_portCheck)
|| type_check (&request.startup.flagsType, &flagsCheck))
reply.RetCode = MIG_BAD_ARGUMENTS;
else
{
mach_msg_type_name_t realnode_type;
*control = request.startup.control_port.name;
reply.RetCode =
(*underlying_open_fn) (request.startup.flags,
&reply.realnode.name, &realnode_type, task,
cookie);
reply.realnodeType = realnodeType;
reply.realnodeType.msgt_name = realnode_type;
if (!reply.RetCode && reply.realnode.name != MACH_PORT_NULL)
reply.head.msgh_bits |= MACH_MSGH_BITS_COMPLEX;
}
err = mach_msg (&reply.head, MACH_SEND_MSG | MACH_SEND_INTERRUPT,
sizeof(reply), 0,
request.head.msgh_remote_port,
MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
if (err == MACH_SEND_INTERRUPTED
&& reply.realnodeType.msgt_name == MACH_MSG_TYPE_MOVE_SEND)
mach_port_deallocate (mach_task_self (), reply.realnode.name);
if (reply.RetCode)
err = reply.RetCode;
return err;
}
error_t
fshelp_start_translator_long (fshelp_open_fn_t underlying_open_fn,
void *cookie, char *name, char *argz,
mach_msg_type_number_t argz_len,
mach_port_t *fds,
mach_msg_type_name_t fds_type,
mach_msg_type_number_t fds_len,
mach_port_t *ports,
mach_msg_type_name_t ports_type,
mach_msg_type_number_t ports_len,
int *ints,
mach_msg_type_number_t ints_len,
uid_t owner_uid,
int timeout, fsys_t *control)
{
error_t err;
file_t executable;
mach_port_t bootstrap = MACH_PORT_NULL;
mach_port_t task = MACH_PORT_NULL;
mach_port_t prev_notify, proc, saveport;
int deallocate_proc;
assert_backtrace (ports_len > INIT_PORT_BOOTSTRAP);
assert_backtrace (ports_type == MACH_MSG_TYPE_COPY_SEND);
assert_backtrace (fds_type == MACH_MSG_TYPE_COPY_SEND);
executable = file_name_lookup (name, O_EXEC, 0);
if (executable == MACH_PORT_NULL)
return errno;
err = mach_port_allocate (mach_task_self (),
MACH_PORT_RIGHT_RECEIVE,
&bootstrap);
if (err)
goto lose;
err = mach_port_insert_right (mach_task_self (),
bootstrap, bootstrap,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
goto lose;
err = task_create (mach_task_self (),
#ifdef KERN_INVALID_LEDGER
NULL, 0,
#endif
0, &task);
if (err)
goto lose;
err = task_priority (task, 25, FALSE);
if (err)
goto lose_task;
if (ports[INIT_PORT_PROC] == MACH_PORT_NULL)
{
proc = getproc ();
deallocate_proc = 1;
}
else
{
proc = ports[INIT_PORT_PROC];
deallocate_proc = 0;
}
proc_child (proc, task);
err = proc_task2proc (proc, task, &ports[INIT_PORT_PROC]);
if (!err)
{
err = proc_setowner (ports[INIT_PORT_PROC],
owner_uid,
owner_uid == (uid_t) -1);
if (err == EOPNOTSUPP)
err = 0;
}
if (deallocate_proc)
mach_port_deallocate (mach_task_self (), proc);
if (err)
goto lose_task;
if (MACH_PORT_VALID (ports[INIT_PORT_AUTH])
&& HURD_PORT_USE (&_hurd_ports[INIT_PORT_AUTH],
port != ports[INIT_PORT_AUTH]))
{
mach_port_t rend, newport = MACH_PORT_NULL;
rend = mach_reply_port ();
err = proc_reauthenticate (ports[INIT_PORT_PROC],
rend, MACH_MSG_TYPE_MAKE_SEND);
if (!err)
err = auth_user_authenticate (ports[INIT_PORT_AUTH],
rend, MACH_MSG_TYPE_MAKE_SEND,
&newport);
mach_port_mod_refs (mach_task_self (), rend,
MACH_PORT_RIGHT_RECEIVE, -1);
if (err)
goto lose_task;
err = proc_reauthenticate_complete (newport);
if (err)
{
mach_port_deallocate (mach_task_self (), newport);
goto lose_task;
}
mach_port_deallocate (mach_task_self (), ports[INIT_PORT_PROC]);
ports[INIT_PORT_PROC] = newport;
}
saveport = ports[INIT_PORT_BOOTSTRAP];
ports[INIT_PORT_BOOTSTRAP] = bootstrap;
#ifdef HAVE_FILE_EXEC_PATHS
err = file_exec_paths (executable, task, EXEC_DEFAULTS, name, name,
argz, argz_len, 0, 0,
fds, fds_type, fds_len,
ports, ports_type, ports_len,
ints, ints_len, 0, 0, 0, 0);
if (err == MIG_BAD_ID)
#endif
err = file_exec (executable, task, EXEC_DEFAULTS,
argz, argz_len, 0, 0,
fds, fds_type, fds_len,
ports, ports_type, ports_len,
ints, ints_len, 0, 0, 0, 0);
mach_port_deallocate (mach_task_self (), bootstrap);
ports[INIT_PORT_BOOTSTRAP] = saveport;
if (err)
goto lose_task;
err = mach_port_request_notification (mach_task_self (),
bootstrap,
MACH_NOTIFY_NO_SENDERS,
0,
bootstrap,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&prev_notify);
if (err)
goto lose_task;
err = service_fsys_startup (underlying_open_fn,
cookie, bootstrap,
timeout, control, task);
lose_task:
if (err)
task_terminate (task);
lose:
if (bootstrap != MACH_PORT_NULL)
mach_port_destroy (mach_task_self (), bootstrap);
if (executable != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), executable);
if (task != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), task);
if (ports[INIT_PORT_PROC] != MACH_PORT_NULL)
{
mach_port_deallocate (mach_task_self (), ports[INIT_PORT_PROC]);
ports[INIT_PORT_PROC] = MACH_PORT_NULL;
}
return err;
}