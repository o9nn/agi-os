#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/ihash.h>
#include <mach/message.h>
#include <assert-backtrace.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <argp.h>
#include <error.h>
#include <string.h>
#include <version.h>
#include <sys/wait.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <argz.h>
#include <envz.h>
#include "msgids.h"
#define MSG_ALIGNMENT __alignof__(uintptr_t)
const char *argp_program_version = STANDARD_HURD_VERSION (rpctrace);
static unsigned strsize = 80;
static const struct argp_option options[] =
{
{"output", 'o', "FILE", 0, "Send trace output to FILE instead of stderr."},
{0, 's', "SIZE", 0, "Specify the maximum string size to print (the default is 80)."},
{0, 'E', "var[=value]", 0,
"Set/change (var=value) or remove (var) an environment variable among the "
"ones inherited by the executed process."},
{0}
};
#define UNKNOWN_NAME MACH_PORT_NULL
static const char args_doc[] = "COMMAND [ARG...]";
static const char doc[] = "Trace Mach Remote Procedure Calls.";
struct task_info
{
task_t task;
boolean_t threads_wrapped;
};
static struct hurd_ihash task_ihash
= HURD_IHASH_INITIALIZER (HURD_IHASH_NO_LOCP);
task_t unknown_task;
void
add_task (task_t task)
{
error_t err;
struct task_info *info = malloc (sizeof *info);
if (info == NULL)
error (1, 0, "Fail to allocate memory.");
info->task = task;
info->threads_wrapped = FALSE;
err = hurd_ihash_add (&task_ihash, task, info);
if (err)
error (1, err, "hurd_ihash_add");
}
void
remove_task (task_t task)
{
hurd_ihash_remove (&task_ihash, task);
}
static const char *
msgid_name (mach_msg_id_t msgid)
{
const struct msgid_info *info = msgid_info (msgid);
return info ? info->name : 0;
}
static int
msgid_display (const struct msgid_info *info)
{
return 1;
}
static int
msgid_trace_replies (const struct msgid_info *info)
{
return 1;
}
struct traced_info
{
struct port_info pi;
mach_msg_type_name_t type;
char *name;
};
struct receiver_info
{
char *name;
hurd_ihash_locp_t locp;
mach_port_t portname;
task_t task;
mach_port_t forward;
struct receiver_info *receive_right;
struct sender_info *next;
};
struct sender_info
{
struct traced_info pi;
task_t task;
struct sender_info *next;
struct receiver_info *receive_right;
};
struct send_once_info
{
struct traced_info pi;
mach_port_t forward;
struct send_once_info *nextfree;
};
#define INFO_SEND_ONCE(info) ((info)->type == MACH_MSG_TYPE_MOVE_SEND_ONCE)
#define TRACED_INFO(info) ((struct traced_info *) info)
#define SEND_INFO(info) ((struct sender_info *) info)
#define SEND_ONCE_INFO(info) ((struct send_once_info *) info)
struct req_info
{
boolean_t is_req;
mach_msg_id_t req_id;
mach_port_t reply_port;
task_t from;
task_t to;
struct req_info *next;
};
static struct req_info *req_head = NULL;
static struct req_info *
add_request (mach_msg_id_t req_id, mach_port_t reply_port,
task_t from, task_t to)
{
struct req_info *req = malloc (sizeof (*req));
if (!req)
error (1, 0, "cannot allocate memory");
req->req_id = req_id;
req->from = from;
req->to = to;
req->reply_port = reply_port;
req->is_req = TRUE;
req->next = req_head;
req_head = req;
return req;
}
static struct req_info *
remove_request (mach_msg_id_t req_id, mach_port_t reply_port)
{
struct req_info **prev;
struct req_info *req;
prev = &req_head;
while (*prev)
{
if ((*prev)->req_id == req_id && (*prev)->reply_port == reply_port)
break;
prev = &(*prev)->next;
}
if (*prev == NULL)
return NULL;
req = *prev;
*prev = req->next;
return req;
}
struct port_info *notify_pi;
struct receiver_info *receive_right_list;
static struct traced_info dummy_wrapper;
static struct send_once_info *freelist;
struct hurd_ihash traced_names
= HURD_IHASH_INITIALIZER (offsetof (struct receiver_info, locp));
struct port_class *traced_class;
struct port_class *other_class;
struct port_bucket *traced_bucket;
FILE *ostream;
static void print_request_header (struct sender_info *info,
mach_msg_header_t *header);
static void print_reply_header (struct send_once_info *info,
mig_reply_header_t *header,
struct req_info *req);
static void print_data (mach_msg_type_name_t type,
const void *data,
mach_msg_type_number_t nelt,
mach_msg_type_number_t eltsize);
static struct receiver_info *
new_receiver_info (mach_port_t right, mach_port_t owner)
{
error_t err;
struct receiver_info *info;
mach_port_t foo;
info = malloc (sizeof (*info));
if (!info)
error (1, 0, "cannot allocate memory");
info->forward = right;
info->task = owner;
info->portname = UNKNOWN_NAME;
info->receive_right = NULL;
info->next = NULL;
if (owner != unknown_task)
{
info->receive_right = receive_right_list;
receive_right_list = info;
}
info->name = 0;
err = mach_port_request_notification (mach_task_self (), right,
MACH_NOTIFY_DEAD_NAME, 1,
notify_pi->port_right,
MACH_MSG_TYPE_MAKE_SEND_ONCE, &foo);
if (err)
error (2, err, "mach_port_request_notification");
if (MACH_PORT_VALID (foo))
mach_port_deallocate (mach_task_self (), foo);
err = hurd_ihash_add (&traced_names, info->forward, info);
if (err)
error (2, err, "hurd_ihash_add");
return info;
}
static void
destroy_receiver_info (struct receiver_info *info)
{
struct sender_info *send_wrapper;
struct receiver_info **prev;
mach_port_deallocate (mach_task_self (), info->forward);
prev = &receive_right_list;
while (*prev != info && *prev)
prev = &((*prev)->receive_right);
if (*prev)
*prev = info->receive_right;
send_wrapper = info->next;
while (send_wrapper)
{
struct sender_info *next = send_wrapper->next;
assert_backtrace (
refcounts_hard_references (&TRACED_INFO (send_wrapper)->pi.refcounts)
== 1);
send_wrapper->receive_right = NULL;
ports_destroy_right (send_wrapper);
send_wrapper = next;
}
hurd_ihash_locp_remove (&traced_names, info->locp);
free (info);
}
static struct sender_info *
new_send_wrapper (struct receiver_info *receive, task_t task,
mach_port_t *wrapper_right)
{
error_t err;
struct sender_info *info;
err = ports_create_port (traced_class, traced_bucket,
sizeof *info, &info);
assert_perror_backtrace (err);
TRACED_INFO (info)->name = 0;
asprintf (&TRACED_INFO (info)->name, "  %u<--%u(pid%d)",
receive->forward, TRACED_INFO (info)->pi.port_right, task2pid (task));
TRACED_INFO (info)->type = MACH_MSG_TYPE_MOVE_SEND;
info->task = task;
info->receive_right = receive;
info->next = receive->next;
receive->next = info;
*wrapper_right = ports_get_right (info);
ports_port_deref (info);
return info;
}
static struct send_once_info *
new_send_once_wrapper (mach_port_t right, mach_port_t *wrapper_right)
{
error_t err;
struct send_once_info *info;
if (freelist)
{
info = freelist;
freelist = info->nextfree;
}
else
{
err = ports_create_port (traced_class, traced_bucket,
sizeof *info, &info);
assert_perror_backtrace (err);
TRACED_INFO (info)->name = 0;
}
info->forward = right;
TRACED_INFO (info)->type = MACH_MSG_TYPE_MOVE_SEND_ONCE;
info->nextfree = NULL;
*wrapper_right = TRACED_INFO (info)->pi.port_right;
return info;
}
static void
unlink_sender_info (void *pi)
{
struct sender_info *info = pi;
struct sender_info **prev;
if (info->receive_right)
{
prev = &info->receive_right->next;
while (*prev != info && *prev)
prev = &((*prev)->next);
assert_backtrace (*prev);
*prev = info->next;
info->next = NULL;
}
}
static void
traced_clean (void *pi)
{
struct sender_info *info = pi;
assert_backtrace (TRACED_INFO (info)->type == MACH_MSG_TYPE_MOVE_SEND);
free (TRACED_INFO (info)->name);
if (info->receive_right)
{
unlink_sender_info (pi);
if (info->receive_right->next == NULL)
destroy_receiver_info (info->receive_right);
info->receive_right = NULL;
}
}
boolean_t
seen_receive_right (task_t task, mach_port_t name)
{
struct receiver_info *info = receive_right_list;
while (info)
{
if (info->task == task && info->portname == name)
return TRUE;
info = info->receive_right;
}
return FALSE;
}
struct receiver_info *
discover_receive_right (mach_port_t send, task_t task)
{
error_t err;
struct receiver_info *info = NULL;
info = hurd_ihash_find (&traced_names, send);
if (info
&& !(info->task != unknown_task
&& info->portname == UNKNOWN_NAME))
return info;
{
int j;
mach_port_t *portnames = NULL;
mach_msg_type_number_t nportnames = 0;
mach_port_type_t *porttypes = NULL;
mach_msg_type_number_t nporttypes = 0;
struct receiver_info *receiver_info = NULL;
err = mach_port_names (task, &portnames, &nportnames,
&porttypes, &nporttypes);
if (err == MACH_SEND_INVALID_DEST)
{
remove_task (task);
return 0;
}
if (err)
error (2, err, "mach_port_names");
for (j = 0; j < nportnames; j++)
{
mach_port_status_t port_status;
mach_port_t send_right;
mach_msg_type_name_t type;
if (!(porttypes[j] & MACH_PORT_TYPE_RECEIVE)
|| seen_receive_right (task, portnames[j]))
continue;
err = mach_port_get_receive_status (task, portnames[j],
&port_status);
if (err)
error (2, err, "mach_port_get_receive_status");
if (!port_status.mps_srights)
continue;
err = mach_port_extract_right (task, portnames[j],
MACH_MSG_TYPE_MAKE_SEND,
&send_right, &type);
if (err)
error (2, err, "mach_port_extract_right");
if (
hurd_ihash_find (&traced_names, send_right)
|| send_right != send )
{
mach_port_deallocate (mach_task_self (), send_right);
continue;
}
receiver_info = new_receiver_info (send_right, task);
receiver_info->portname = portnames[j];
break;
}
if (portnames)
vm_deallocate (mach_task_self (), (vm_address_t) portnames,
nportnames * sizeof (*portnames));
if (porttypes)
vm_deallocate (mach_task_self (), (vm_address_t) porttypes,
nporttypes * sizeof (*porttypes));
if (receiver_info)
return receiver_info;
}
return NULL;
}
struct sender_info *
get_send_wrapper (struct receiver_info *receiver_info,
mach_port_t task, mach_port_t *right)
{
struct sender_info *info = receiver_info->next;
while (info)
{
if (info->task == task)
{
*right = ports_get_right (info);
return info;
}
info = info->next;
}
return new_send_wrapper (receiver_info, task, right);
}
static char *
rewrite_right (mach_port_t *right, mach_msg_type_name_t *type,
struct req_info *req)
{
error_t err;
struct receiver_info *receiver_info;
struct sender_info *send_wrapper;
task_t dest = unknown_task;
task_t source = unknown_task;
if (!MACH_PORT_VALID (*right))
return 0;
if (req)
{
if (req->is_req)
{
source = req->from;
dest = req->to;
}
else
{
source = req->to;
dest = req->from;
}
}
switch (*type)
{
case MACH_MSG_TYPE_PORT_SEND:
send_wrapper = ports_lookup_port (traced_bucket, *right, 0);
if (send_wrapper)
{
mach_port_deallocate (mach_task_self (), *right);
assert_backtrace (send_wrapper->receive_right);
if (dest == send_wrapper->receive_right->task)
{
*right = send_wrapper->receive_right->forward;
err = mach_port_mod_refs (mach_task_self (), *right,
MACH_PORT_RIGHT_SEND, +1);
if (err)
error (2, err, "mach_port_mod_refs");
ports_port_deref (send_wrapper);
}
else
{
struct sender_info *send_wrapper2
= get_send_wrapper (send_wrapper->receive_right, dest, right);
ports_port_deref (send_wrapper);
*type = MACH_MSG_TYPE_MAKE_SEND;
send_wrapper = send_wrapper2;
}
return TRACED_INFO (send_wrapper)->name;
}
if (req && req->req_id == 3216)
receiver_info = discover_receive_right (*right, dest);
else
receiver_info = discover_receive_right (*right, source);
if (receiver_info == NULL)
{
if (source != unknown_task)
{
error (0, 0, "get an unknown send right from process %d",
task2pid (source));
return dummy_wrapper.name;
}
receiver_info = new_receiver_info (*right, unknown_task);
mach_port_mod_refs (mach_task_self (), *right,
MACH_PORT_RIGHT_SEND, 1);
}
if (dest == receiver_info->task)
return receiver_info->name;
else
{
assert_backtrace (*right == receiver_info->forward);
mach_port_deallocate (mach_task_self (), *right);
send_wrapper = get_send_wrapper (receiver_info, dest, right);
*type = MACH_MSG_TYPE_MAKE_SEND;
return TRACED_INFO (send_wrapper)->name;
}
case MACH_MSG_TYPE_PORT_SEND_ONCE:
*type = MACH_MSG_TYPE_MAKE_SEND_ONCE;
return TRACED_INFO (new_send_once_wrapper (*right, right))->name;
case MACH_MSG_TYPE_PORT_RECEIVE:
{
assert_backtrace (req);
receiver_info = hurd_ihash_find (&traced_names, *right);
if (receiver_info)
{
struct sender_info *send_wrapper2;
char *name;
mach_port_t rr;
mach_port_deallocate (mach_task_self (), receiver_info->forward);
err = ports_import_port (traced_class, traced_bucket,
*right, sizeof *send_wrapper,
&send_wrapper);
if (err)
error (2, err, "ports_import_port");
TRACED_INFO (send_wrapper)->type = MACH_MSG_TYPE_MOVE_SEND;
send_wrapper->task = source;
TRACED_INFO (send_wrapper)->name = receiver_info->name;
send_wrapper->receive_right = NULL;
send_wrapper->next = NULL;
ports_port_deref (send_wrapper);
hurd_ihash_locp_remove (&traced_names, receiver_info->locp);
send_wrapper2 = get_send_wrapper (receiver_info, dest, &rr);
assert_backtrace (
refcounts_hard_references (
&TRACED_INFO (send_wrapper2)->pi.refcounts)
== 1);
name = TRACED_INFO (send_wrapper2)->name;
TRACED_INFO (send_wrapper2)->name = NULL;
unlink_sender_info (send_wrapper2);
send_wrapper2->receive_right = NULL;
rr = ports_claim_right (send_wrapper2);
err = mach_port_insert_right (mach_task_self (), rr, rr,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
error (2, err, "mach_port_insert_right");
receiver_info->forward = rr;
receiver_info->task = dest;
if (dest != unknown_task)
{
receiver_info->receive_right = receive_right_list;
receive_right_list = receiver_info;
}
receiver_info->portname = UNKNOWN_NAME;
receiver_info->name = name;
send_wrapper->receive_right = receiver_info;
send_wrapper->next = receiver_info->next;
receiver_info->next = send_wrapper;
err = hurd_ihash_add (&traced_names, receiver_info->forward,
receiver_info);
if (err)
error (2, err, "hurd_ihash_add");
*right = rr;
}
else
{
err = mach_port_insert_right (mach_task_self (), *right, *right,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
error (2, err, "mach_port_insert_right");
receiver_info = new_receiver_info (*right, dest);
}
return receiver_info->name;
}
default:
assert_backtrace (!"??? bogus port type from kernel!");
}
return 0;
}
static mach_port_name_t *
get_port_ref (void *data, const boolean_t is_inline, const int i) {
if (is_inline)
{
mach_port_name_inlined_t *const inlined_port_names = data;
return &inlined_port_names[i].name;
}
else
{
mach_port_t *const portnames = data;
return &portnames[i];
}
}
static void
print_contents (mach_msg_header_t *inp,
void *msg_buf_ptr, struct req_info *req)
{
error_t err;
int first = 1;
while ((char *) msg_buf_ptr < (char *) inp + inp->msgh_size)
{
mach_msg_type_t *const type = msg_buf_ptr;
mach_msg_type_long_t *const lt = (mach_msg_type_long_t *) type;
void *data;
mach_msg_type_number_t nelt;
mach_msg_type_size_t eltsize;
mach_msg_type_name_t name;
boolean_t is_inline = type->msgt_inline;
if (!type->msgt_longform)
{
name = type->msgt_name;
nelt = type->msgt_number;
eltsize = type->msgt_size / 8;
data = msg_buf_ptr = type + 1;
}
else
{
name = lt->msgtl_name;
nelt = lt->msgtl_number;
eltsize = lt->msgtl_size / 8;
data = msg_buf_ptr = lt + 1;
}
if (!is_inline)
{
data = *(void **) data;
msg_buf_ptr += sizeof (void *);
}
else
msg_buf_ptr += ((nelt * eltsize + MSG_ALIGNMENT - 1) & ~(MSG_ALIGNMENT - 1));
if (first)
first = 0;
else
putc (' ', ostream);
if (MACH_MSG_TYPE_PORT_ANY_RIGHT (name))
{
mach_msg_type_number_t i;
mach_msg_type_name_t newtypes[nelt ? : 1];
int poly;
assert_backtrace (inp->msgh_bits & MACH_MSGH_BITS_COMPLEX);
if (is_inline)
assert_backtrace (eltsize == sizeof (mach_port_name_inlined_t));
else
assert_backtrace (eltsize == sizeof (mach_port_t));
poly = 0;
for (i = 0; i < nelt; ++i)
{
char *str;
mach_port_name_t *port_name = get_port_ref (data, is_inline, i);
newtypes[i] = name;
str = rewrite_right (port_name, &newtypes[i], req);
putc ((i == 0 && nelt > 1) ? '{' : ' ', ostream);
if (*port_name == MACH_PORT_NULL)
fprintf (ostream, "(null)");
else if (*port_name == MACH_PORT_DEAD)
fprintf (ostream, "(dead)");
else
{
if (str != 0)
fprintf (ostream, "%s", str);
else
fprintf (ostream, "%3u", (unsigned int) *port_name);
}
if (i > 0 && newtypes[i] != newtypes[0])
poly = 1;
}
if (nelt > 1)
putc ('}', ostream);
if (poly)
{
if (name == MACH_MSG_TYPE_MOVE_SEND_ONCE)
{
for (i = 0; i < nelt; ++i)
{
mach_port_name_t *port_name = get_port_ref (data, is_inline, i);
if (newtypes[i] == MACH_MSG_TYPE_MAKE_SEND_ONCE)
{
err = mach_port_insert_right (mach_task_self (),
*port_name,
*port_name,
newtypes[i]);
assert_perror_backtrace (err);
}
else
assert_backtrace (newtypes[i] == MACH_MSG_TYPE_MOVE_SEND_ONCE);
}
}
else
{
for (i = 0; i < nelt; ++i)
{
mach_port_name_t *port_name = get_port_ref (data, is_inline, i);
switch (newtypes[i])
{
case MACH_MSG_TYPE_COPY_SEND:
err = mach_port_mod_refs (mach_task_self (),
*port_name,
MACH_PORT_RIGHT_SEND, +1);
assert_perror_backtrace (err);
break;
case MACH_MSG_TYPE_MAKE_SEND:
err = mach_port_insert_right (mach_task_self (),
*port_name,
*port_name,
newtypes[i]);
assert_perror_backtrace (err);
break;
default:
assert_backtrace (newtypes[i] == MACH_MSG_TYPE_MOVE_SEND);
break;
}
}
name = MACH_MSG_TYPE_MOVE_SEND;
}
if (type->msgt_longform)
lt->msgtl_name = name;
else
type->msgt_name = name;
}
else if (nelt > 0 && newtypes[0] != name)
{
if (type->msgt_longform)
lt->msgtl_name = newtypes[0];
else
type->msgt_name = newtypes[0];
}
}
else
print_data (name, data, nelt, eltsize);
}
}
static void
wrap_all_threads (task_t task)
{
struct sender_info *thread_send_wrapper;
struct receiver_info *thread_receiver_info;
thread_t *threads;
mach_msg_type_number_t nthreads;
error_t err;
err = task_threads (task, &threads, &nthreads);
if (err)
error (2, err, "task_threads");
for (int i = 0; i < nthreads; ++i)
{
thread_receiver_info = hurd_ihash_find (&traced_names, threads[i]);
if (thread_receiver_info == NULL)
{
mach_port_t new_thread_port;
thread_receiver_info = new_receiver_info (threads[i], unknown_task);
thread_send_wrapper = new_send_wrapper (thread_receiver_info,
task, &new_thread_port);
free (TRACED_INFO (thread_send_wrapper)->name);
asprintf (&TRACED_INFO (thread_send_wrapper)->name,
"thread%u(pid%d)", threads[i], task2pid (task));
err = mach_port_insert_right (mach_task_self (),
new_thread_port, new_thread_port,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
error (2, err, "mach_port_insert_right");
err = thread_set_kernel_port (threads[i], new_thread_port);
if (err)
error (2, err, "thread_set_kernel_port");
mach_port_deallocate (mach_task_self (), new_thread_port);
}
}
vm_deallocate (mach_task_self (), (vm_address_t) threads,
nthreads * sizeof (thread_t));
}
static void
wrap_new_thread (mach_msg_header_t *inp, struct req_info *req)
{
error_t err;
mach_port_t thread_port;
struct
{
mach_msg_header_t head;
mach_msg_type_t retcode_type;
kern_return_t retcode;
mach_msg_type_t child_thread_type;
mach_port_t child_thread;
} *reply = (void *) inp;
struct sender_info *send_wrapper = ports_lookup_port (traced_bucket,
reply->child_thread, 0);
assert_backtrace (send_wrapper);
assert_backtrace (send_wrapper->receive_right);
thread_port = send_wrapper->receive_right->forward;
err = mach_port_insert_right (mach_task_self (), reply->child_thread,
reply->child_thread, MACH_MSG_TYPE_MAKE_SEND);
if (err)
error (2, err, "mach_port_insert_right");
err = thread_set_kernel_port (thread_port, reply->child_thread);
if (err)
error (2, err, "thread_set_kernel_port");
mach_port_deallocate (mach_task_self (), reply->child_thread);
free (TRACED_INFO (send_wrapper)->name);
asprintf (&TRACED_INFO (send_wrapper)->name, "thread%u(pid%d)",
thread_port, task2pid (req->from));
ports_port_deref (send_wrapper);
}
static void
wrap_new_task (mach_msg_header_t *inp, struct req_info *req)
{
error_t err;
pid_t pid;
task_t pseudo_task_port;
task_t task_port;
struct
{
mach_msg_header_t head;
mach_msg_type_t retcode_type;
kern_return_t retcode;
mach_msg_type_t child_task_type;
mach_port_t child_task;
} *reply = (void *) inp;
struct sender_info *task_wrapper1 = ports_lookup_port (traced_bucket,
reply->child_task, 0);
struct sender_info *task_wrapper2;
assert_backtrace (task_wrapper1);
assert_backtrace (task_wrapper1->receive_right);
task_port = task_wrapper1->receive_right->forward;
add_task (task_port);
task_wrapper2 = new_send_wrapper (task_wrapper1->receive_right,
task_port, &pseudo_task_port);
err = mach_port_insert_right (mach_task_self (),
pseudo_task_port, pseudo_task_port,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
error (2, err, "mach_port_insert_right");
err = task_set_kernel_port (task_port, pseudo_task_port);
if (err)
error (2, err, "task_set_kernel_port");
mach_port_deallocate (mach_task_self (), pseudo_task_port);
pid = task2pid (task_port);
free (TRACED_INFO (task_wrapper1)->name);
asprintf (&TRACED_INFO (task_wrapper1)->name, "task%u(pid%d)",
task_port, task2pid (req->from));
free (TRACED_INFO (task_wrapper2)->name);
asprintf (&TRACED_INFO (task_wrapper2)->name, "task%u(pid%d)",
task_port, pid);
ports_port_deref (task_wrapper1);
}
static inline int
is_notification (const mach_msg_header_t *InHeadP)
{
int msgh_id = InHeadP->msgh_id - 64;
if ((msgh_id > 8) || (msgh_id < 0))
return 0;
return 1;
}
int
trace_and_forward (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mach_port_t reply_port;
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
error_t err;
const struct msgid_info *msgid;
struct traced_info *info;
mach_msg_bits_t complex;
if (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) == MACH_MSG_TYPE_PROTECTED_PAYLOAD)
{
info = ports_lookup_payload (traced_bucket, inp->msgh_protected_payload,
NULL);
if (info)
{
inp->msgh_bits = MACH_MSGH_BITS (
MACH_MSGH_BITS_REMOTE (inp->msgh_bits),
is_notification (inp)? MACH_MSG_TYPE_MOVE_SEND_ONCE: info->type)
| MACH_MSGH_BITS_OTHER (inp->msgh_bits);
inp->msgh_local_port = ports_payload_get_name ((uintptr_t) info);
}
}
else
info = ports_lookup_port (traced_bucket, inp->msgh_local_port, NULL);
assert_backtrace (info);
if (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) == MACH_MSG_TYPE_MOVE_SEND_ONCE)
{
if (inp->msgh_id == MACH_NOTIFY_DEAD_NAME && info == (void *) notify_pi)
{
struct receiver_info *receiver_info;
const mach_dead_name_notification_t *const n = (void *) inp;
mach_port_deallocate (mach_task_self (), n->not_port);
receiver_info = hurd_ihash_find (&traced_names, n->not_port);
if (receiver_info)
{
assert_backtrace (n->not_port == receiver_info->forward);
destroy_receiver_info (receiver_info);
}
((mig_reply_header_t *) outp)->RetCode = MIG_NO_REPLY;
ports_port_deref (info);
remove_task (n->not_port);
return 1;
}
else if (inp->msgh_id == MACH_NOTIFY_NO_SENDERS
&& !INFO_SEND_ONCE (info))
{
mach_no_senders_notification_t *n = (void *) inp;
ports_no_senders (info, n->not_count);
ports_port_deref (info);
((mig_reply_header_t *) outp)->RetCode = MIG_NO_REPLY;
return 1;
}
else if (info == (void *) notify_pi)
{
ports_port_deref (info);
((mig_reply_header_t *) outp)->RetCode = MIG_NO_REPLY;
return 1;
}
}
assert_backtrace (info != (void *) notify_pi);
assert_backtrace (MACH_MSGH_BITS_LOCAL (inp->msgh_bits) == info->type);
complex = inp->msgh_bits & MACH_MSGH_BITS_COMPLEX;
msgid = msgid_info (inp->msgh_id);
{
mach_msg_type_name_t this_type = MACH_MSGH_BITS_LOCAL (inp->msgh_bits);
mach_msg_type_name_t reply_type = MACH_MSGH_BITS_REMOTE (inp->msgh_bits);
reply_port = inp->msgh_remote_port;
inp->msgh_local_port = inp->msgh_remote_port;
if (reply_type && msgid_trace_replies (msgid)
&& MACH_PORT_VALID (inp->msgh_local_port))
{
switch (reply_type)
{
case MACH_MSG_TYPE_PORT_SEND:
rewrite_right (&inp->msgh_local_port, &reply_type, NULL);
break;
case MACH_MSG_TYPE_PORT_SEND_ONCE:;
struct send_once_info *info;
info = new_send_once_wrapper (inp->msgh_local_port,
&inp->msgh_local_port);
reply_type = MACH_MSG_TYPE_MAKE_SEND_ONCE;
assert_backtrace (inp->msgh_local_port);
if (TRACED_INFO (info)->name == 0)
{
if (msgid == 0)
asprintf (&TRACED_INFO (info)->name, "reply(%u:%u)",
(unsigned int) TRACED_INFO (info)->pi.port_right,
(unsigned int) inp->msgh_id);
else
asprintf (&TRACED_INFO (info)->name, "reply(%u:%s)",
(unsigned int) TRACED_INFO (info)->pi.port_right,
msgid->name);
}
break;
default:
error (1, 0, "Reply type %i not handled", reply_type);
}
}
if (info->type == MACH_MSG_TYPE_MOVE_SEND_ONCE)
inp->msgh_remote_port = SEND_ONCE_INFO (info)->forward;
else
{
assert_backtrace (SEND_INFO (info)->receive_right);
inp->msgh_remote_port = SEND_INFO (info)->receive_right->forward;
}
if (this_type == MACH_MSG_TYPE_MOVE_SEND_ONCE)
{
free (info->name);
info->name = 0;
SEND_ONCE_INFO (info)->forward = 0;
SEND_ONCE_INFO (info)->nextfree = freelist;
freelist = SEND_ONCE_INFO (info);
}
else
this_type = MACH_MSG_TYPE_COPY_SEND;
inp->msgh_bits = complex | MACH_MSGH_BITS (this_type, reply_type);
}
if (msgid_display (msgid))
{
if (inp->msgh_local_port == MACH_PORT_NULL
&& info->type == MACH_MSG_TYPE_MOVE_SEND_ONCE
&& inp->msgh_size >= sizeof (mig_reply_header_t)
&& (inp->msgh_id > 72 || inp->msgh_id < 64)
&& !memcmp(&((mig_reply_header_t *) inp)->RetCodeType,
&RetCodeType, sizeof (RetCodeType)))
{
struct req_info *req = remove_request (inp->msgh_id - 100,
inp->msgh_remote_port);
assert_backtrace (req);
req->is_req = FALSE;
mig_reply_header_t *rh = (void *) inp;
print_reply_header ((struct send_once_info *) info, rh, req);
putc (' ', ostream);
fflush (ostream);
print_contents (&rh->Head, rh + 1, req);
putc ('\n', ostream);
if (inp->msgh_id == 2161)
wrap_new_thread (inp, req);
else if (inp->msgh_id == 2107)
wrap_new_task (inp, req);
free (req);
}
else
{
struct task_info *task_info;
task_t to = 0;
struct req_info *req = NULL;
print_request_header ((struct sender_info *) info, inp);
if (inp->msgh_id <= 72 && inp->msgh_id >= 64)
{
assert_backtrace (info->type == MACH_MSG_TYPE_MOVE_SEND_ONCE);
assert_backtrace (inp->msgh_id != 69);
}
else if (inp->msgh_id >= 3200 && inp->msgh_id <= 3218)
to = SEND_INFO (info)->receive_right->forward;
else
to = SEND_INFO (info)->receive_right->task;
if (info->type == MACH_MSG_TYPE_MOVE_SEND)
req = add_request (inp->msgh_id, reply_port,
SEND_INFO (info)->task, to);
print_contents (inp, inp + 1, req);
if (inp->msgh_local_port == MACH_PORT_NULL)
{
req = remove_request (inp->msgh_id, reply_port);
free (req);
fprintf (ostream, ");\n");
}
else
fprintf (ostream, ")");
fflush (ostream);
task_info = hurd_ihash_find (&task_ihash, SEND_INFO (info)->task);
if (task_info && !task_info->threads_wrapped)
{
wrap_all_threads (SEND_INFO (info)->task);
task_info->threads_wrapped = TRUE;
}
}
}
err = mach_msg (inp, MACH_SEND_MSG, inp->msgh_size, 0,
MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
if (err == MACH_SEND_INVALID_DEST)
{
mach_msg_destroy (inp);
}
else
assert_perror_backtrace (err);
ports_port_deref (info);
((mig_reply_header_t *) outp)->RetCode = MIG_NO_REPLY;
return 1;
}
static void *
trace_thread_function (void *arg)
{
struct port_bucket *const bucket = arg;
ports_manage_port_operations_one_thread (bucket, trace_and_forward, 0);
return 0;
}
#if 0
struct msg_type
{
const char *name;
const char *letter;
};
static const char *const msg_types[] =
{
[MACH_MSG_TYPE_BIT] = {"bool", "b"},
[MACH_MSG_TYPE_INTEGER_16] = {"int16", "h"},
[MACH_MSG_TYPE_INTEGER_32] = {"int32", "i"},
[MACH_MSG_TYPE_CHAR] = {"char", "c"},
[MACH_MSG_TYPE_INTEGER_8] = {"int8", "B"},
[MACH_MSG_TYPE_REAL] = {"float", "f"},
[MACH_MSG_TYPE_INTEGER_64] = {"int64", "q"},
[MACH_MSG_TYPE_STRING] = {"string", "s"},
[MACH_MSG_TYPE_MOVE_RECEIVE] = {"move-receive", "R"},
[MACH_MSG_TYPE_MOVE_SEND] = {"move-send", "S"},
[MACH_MSG_TYPE_MOVE_SEND_ONCE]= {"move-send-once", "O"},
[MACH_MSG_TYPE_COPY_SEND] = {"copy-send", "s"},
[MACH_MSG_TYPE_MAKE_SEND] = {"make-send", ""},
[MACH_MSG_TYPE_MAKE_SEND_ONCE]= {"make-send-once", ""},
[MACH_MSG_TYPE_PORT_NAME] = {"port-name", "n"},
};
#endif
static mach_port_t last_reply_port;
static void
print_ellipsis (void)
{
if (MACH_PORT_VALID (last_reply_port))
fprintf (ostream, " ...%u\n", (unsigned int) last_reply_port);
}
static void
print_request_header (struct sender_info *receiver, mach_msg_header_t *msg)
{
const char *msgname = msgid_name (msg->msgh_id);
print_ellipsis ();
last_reply_port = msg->msgh_local_port;
if (TRACED_INFO (receiver)->name != 0)
fprintf (ostream, "%4s->", TRACED_INFO (receiver)->name);
else
fprintf (ostream, "%4u->",
(unsigned int) TRACED_INFO (receiver)->pi.port_right);
if (msgname != 0)
fprintf (ostream, "%5s (", msgname);
else
fprintf (ostream, "%5u (", (unsigned int) msg->msgh_id);
}
static void
print_reply_header (struct send_once_info *info, mig_reply_header_t *reply,
struct req_info *req)
{
if (last_reply_port != info->pi.pi.port_right)
{
print_ellipsis ();
fprintf (ostream, "%u...", (unsigned int) info->pi.pi.port_right);
}
last_reply_port = MACH_PORT_NULL;
if (reply->Head.msgh_id == req->req_id + 100)
fprintf (ostream, " = ");
else
fprintf (ostream, " =(%u != %u) ",
reply->Head.msgh_id, req->req_id + 100);
if (reply->RetCode == 0)
fprintf (ostream, "0");
else
{
const char *str = strerror (reply->RetCode);
if (str == 0)
fprintf (ostream, "%#x", reply->RetCode);
else
fprintf (ostream, "%#x (%s)", reply->RetCode, str);
}
}
static char escape_sequences[0x100] =
{
['\0'] = '0',
['\a'] = 'a',
['\b'] = 'b',
['\f'] = 'f',
['\n'] = 'n',
['\r'] = 'r',
['\t'] = 't',
['\v'] = 'v',
['\\'] = '\\',
['\''] = '\'',
['"'] = '"',
};
static void
print_data (mach_msg_type_name_t type,
const void *data,
mach_msg_type_number_t nelt,
mach_msg_type_number_t eltsize)
{
switch (type)
{
case MACH_MSG_TYPE_PORT_NAME:
assert_backtrace (eltsize == sizeof (mach_port_t));
{
mach_msg_type_number_t i;
fprintf (ostream, "pn{");
for (i = 0; i < nelt; ++i)
{
fprintf (ostream, "%*u", (i > 0) ? 4 : 3,
(unsigned int) ((mach_port_t *) data)[i]);
}
fprintf (ostream, "}");
return;
}
case MACH_MSG_TYPE_STRING:
case MACH_MSG_TYPE_CHAR:
if (nelt > strsize)
nelt = strsize;
fprintf (ostream, "\"");
const char *p, *q;
p = q = (const char *) data;
while (q && q - (const char *) data < (int) (nelt * eltsize)
&& (*q || type == MACH_MSG_TYPE_CHAR))
{
if (isgraph (*q) || *q == ' ')
{
q += 1;
continue;
}
if (p < q)
fprintf (ostream, "%.*s", (int) (q - p), p);
char c = escape_sequences[*((const unsigned char *) q)];
if (c)
fprintf (ostream, "\\%c", c);
else
fprintf (ostream, "\\x%02x", *((const unsigned char *) q));
q += 1;
p = q;
}
if (p < q)
fprintf (ostream, "%.*s", (int) (q - p), p);
fprintf (ostream, "\"");
return;
#if 0
case MACH_MSG_TYPE_CHAR:
if (eltsize == 1)
FMT ("'%c'", unsigned char);
break;
#endif
#define FMT(fmt, ctype) do { \
mach_msg_type_number_t i; \
for (i = 0; i < nelt; ++i) \
{ \
fprintf (ostream, "%s" fmt, \
(i == 0 && nelt > 1) ? "{" : i > 0 ? " " : "", \
*(const ctype *) data); \
data += eltsize; \
} \
if (nelt > 1) \
putc ('}', ostream); \
return; \
} while (0)
case MACH_MSG_TYPE_BIT:
case MACH_MSG_TYPE_INTEGER_8:
case MACH_MSG_TYPE_INTEGER_16:
case MACH_MSG_TYPE_INTEGER_32:
case MACH_MSG_TYPE_INTEGER_64:
switch (eltsize)
{
case 1: FMT ("%"PRId8, int8_t);
case 2: FMT ("%"PRId16, int16_t);
case 4: FMT ("%"PRId32, int32_t);
case 8: FMT ("%"PRId64, int64_t);
}
break;
case MACH_MSG_TYPE_REAL:
if (eltsize == sizeof (float))
FMT ("%g", float);
else if (eltsize == sizeof (double))
FMT ("%g", double);
else if (eltsize == sizeof (long double))
FMT ("%Lg", long double);
else
abort ();
break;
}
fprintf (ostream, "\t%#x (type %d, %d*%d)\n", *(const int *)data, type,
nelt, eltsize);
}
pid_t
traced_spawn (char **argv, char **envp)
{
error_t err;
pid_t pid;
mach_port_t task_wrapper;
task_t traced_task;
struct sender_info *ti;
struct receiver_info *receive_ti;
char *prefixed_name;
file_t file = file_name_path_lookup (argv[0], getenv ("PATH"),
O_EXEC, 0, &prefixed_name);
if (file == MACH_PORT_NULL)
error (1, errno, "command not found: %s", argv[0]);
err = task_create (mach_task_self (),
#ifdef KERN_INVALID_LEDGER
NULL, 0,
#endif
0, &traced_task);
assert_perror_backtrace (err);
add_task (traced_task);
err = proc_child (getproc (), traced_task);
if (err)
error (2, err, "proc_child");
pid = task2pid (traced_task);
if (pid < 0)
error (2, errno, "task2pid");
receive_ti = new_receiver_info (traced_task, unknown_task);
ti = new_send_wrapper (receive_ti, traced_task, &task_wrapper);
ti->task = traced_task;
free (TRACED_INFO (ti)->name);
asprintf (&TRACED_INFO (ti)->name, "task%u(pid%d)", traced_task, pid);
err = mach_port_insert_right (mach_task_self (), task_wrapper,
task_wrapper, MACH_MSG_TYPE_MAKE_SEND);
assert_perror_backtrace (err);
err = task_set_special_port (traced_task, TASK_KERNEL_PORT, task_wrapper);
assert_perror_backtrace (err);
#ifdef HAVE__HURD_EXEC_PATHS
err = _hurd_exec_paths (traced_task, file, prefixed_name ?: *argv,
prefixed_name ?: *argv, argv, envp);
#else
err = _hurd_exec (traced_task, file, argv, envp);
#endif
if (err)
error (2, err, "cannot exec `%s'", argv[0]);
mach_port_deallocate (mach_task_self (), task_wrapper);
free (prefixed_name);
return pid;
}
int
main (int argc, char **argv, char **envp)
{
const char *outfile = 0;
char **cmd_argv = 0;
pthread_t thread;
error_t err;
char **cmd_envp = NULL;
char *envz = NULL;
size_t envz_len = 0;
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'o':
outfile = arg;
break;
case 's':
strsize = atoi (arg);
break;
case 'E':
if (envz == NULL)
{
if (argz_create (envp, &envz, &envz_len))
error (1, errno, "argz_create");
}
if (envz != NULL)
{
char *equal = strchr (arg, '=');
char *name;
char *newval;
if (equal != NULL)
{
name = strndupa (arg, equal - arg);
if (name == NULL)
error (1, errno, "strndupa");
newval = equal + 1;
}
else
{
name = arg;
newval = NULL;
}
if (envz_add (&envz, &envz_len, name, newval))
error (1, errno, "envz_add");
}
break;
case ARGP_KEY_NO_ARGS:
argp_usage (state);
return EINVAL;
case ARGP_KEY_ARG:
cmd_argv = &state->argv[state->next - 1];
state->next = state->argc;
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
const struct argp_child children[] =
{
{ .argp=&msgid_argp, },
{ 0 }
};
const struct argp argp = { options, parse_opt, args_doc, doc, children };
argp_parse (&argp, argc, argv, ARGP_IN_ORDER, 0, 0);
err = mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_DEAD_NAME,
&unknown_task);
assert_perror_backtrace (err);
if (outfile)
{
ostream = fopen (outfile, "w");
if (!ostream)
error (1, errno, "%s", outfile);
}
else
ostream = stderr;
setlinebuf (ostream);
traced_bucket = ports_create_bucket ();
traced_class = ports_create_class (&traced_clean, NULL);
other_class = ports_create_class (0, 0);
err = ports_create_port (other_class, traced_bucket,
sizeof (*notify_pi), &notify_pi);
assert_perror_backtrace (err);
err = pthread_create (&thread, NULL, trace_thread_function, traced_bucket);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
if (envz != NULL)
{
envz_strip (&envz, &envz_len);
cmd_envp = alloca ((argz_count (envz, envz_len) + 1) * sizeof (char *));
if (cmd_envp == NULL)
error (1, errno, "alloca");
else
argz_extract (envz, envz_len, cmd_envp);
}
if (cmd_envp == NULL)
cmd_envp = envp;
{
pid_t child, pid;
int status;
child = traced_spawn (cmd_argv, cmd_envp);
pid = waitpid (child, &status, 0);
sleep (1);
if (pid != child)
error (1, errno, "waitpid");
if (WIFEXITED (status))
fprintf (ostream, "Child %d exited with %d\n",
pid, WEXITSTATUS (status));
else
fprintf (ostream, "Child %d %s\n", pid, strsignal (WTERMSIG (status)));
}
ports_destroy_right (notify_pi);
free (envz);
return 0;
}