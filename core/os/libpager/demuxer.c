#include <assert-backtrace.h>
#include <error.h>
#include <mach/mig_errors.h>
#include <pthread.h>
#include <string.h>
#include <sys/resource.h>
#include <errno.h>
#include <stdio.h>
#include "priv.h"
#include "memory_object_S.h"
#include "libports/notify_S.h"
#include "queue.h"
#define WORKER_COUNT 10
struct request
{
struct item item;
mig_routine_t routine;
};
static inline mach_msg_header_t *
request_inp (const struct request *r)
{
return (mach_msg_header_t *) ((char *) r + sizeof *r);
}
struct worker
{
struct pager_requests *requests;
struct queue queue;
unsigned long tag;
};
struct pager_requests
{
struct port_bucket *bucket;
struct queue *queue_in;
struct queue *queue_out;
int asleep;
pthread_cond_t wakeup;
pthread_cond_t inhibit_wakeup;
pthread_mutex_t lock;
struct worker workers[WORKER_COUNT];
};
static int
pager_demuxer (struct pager_requests *requests,
mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
error_t err = MIG_NO_REPLY;
mig_routine_t routine;
if (! ((routine = _pager_memory_object_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp))))
return FALSE;
#define MASK	(8u - 1u)
mach_msg_size_t padded_size = (inp->msgh_size + MASK) & ~MASK;
#undef MASK
struct request *r = malloc (sizeof *r + padded_size);
if (r == NULL)
{
err = ENOMEM;
goto out;
}
r->routine = routine;
memcpy (request_inp (r), inp, inp->msgh_size);
pthread_mutex_lock (&requests->lock);
queue_enqueue (requests->queue_in, &r->item);
if (requests->asleep > 0 && requests->queue_in == requests->queue_out)
pthread_cond_signal (&requests->wakeup);
pthread_mutex_unlock (&requests->lock);
err = MIG_NO_REPLY;
out:
((mig_reply_header_t *) outp)->RetCode = err;
return TRUE;
}
static void
mig_reply_setup (
const mach_msg_header_t	*in,
mach_msg_header_t	*out)
{
static const mach_msg_type_t RetCodeType = {
.msgt_name = MACH_MSG_TYPE_INTEGER_32,
.msgt_size = 32,
.msgt_number = 1,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
#define	InP	(in)
#define	OutP	((mig_reply_header_t *) out)
OutP->Head.msgh_bits =
MACH_MSGH_BITS(MACH_MSGH_BITS_REMOTE(InP->msgh_bits), 0);
OutP->Head.msgh_size = sizeof *OutP;
OutP->Head.msgh_remote_port = InP->msgh_remote_port;
OutP->Head.msgh_local_port = MACH_PORT_NULL;
OutP->Head.msgh_seqno = 0;
OutP->Head.msgh_id = InP->msgh_id + 100;
OutP->RetCodeType = RetCodeType;
OutP->RetCode = MIG_BAD_ID;
#undef InP
#undef OutP
}
static void *
worker_func (void *arg)
{
struct worker *self = (struct worker *) arg;
struct pager_requests *requests = self->requests;
struct request *r = NULL;
mig_reply_header_t reply_msg;
while (1)
{
int i;
mach_msg_return_t mr;
free (r);
pthread_mutex_lock (&requests->lock);
r = queue_dequeue (&self->queue);
if (r != NULL)
goto got_one;
self->tag = 0;
get_request_locked:
while ((r = queue_dequeue (requests->queue_out)) == NULL)
{
requests->asleep += 1;
if (requests->asleep == WORKER_COUNT)
pthread_cond_broadcast (&requests->inhibit_wakeup);
pthread_cond_wait (&requests->wakeup, &requests->lock);
requests->asleep -= 1;
}
for (i = 0; i < WORKER_COUNT; i++)
if (requests->workers[i].tag
== (unsigned long) request_inp (r)->msgh_local_port)
{
queue_enqueue (&requests->workers[i].queue, &r->item);
goto get_request_locked;
}
self->tag = (unsigned long) request_inp (r)->msgh_local_port;
got_one:
pthread_mutex_unlock (&requests->lock);
mig_reply_setup (request_inp (r), (mach_msg_header_t *) &reply_msg);
(*r->routine) (request_inp (r), (mach_msg_header_t *) &reply_msg);
mig_reply_header_t *request = (mig_reply_header_t *) request_inp (r);
mig_reply_header_t *reply = &reply_msg;
switch (reply->RetCode)
{
case KERN_SUCCESS:
break;
case MIG_NO_REPLY:
continue;
default:
request->Head.msgh_remote_port = MACH_PORT_NULL;
mach_msg_destroy (&request->Head);
break;
}
if (reply->Head.msgh_remote_port == MACH_PORT_NULL)
{
if (reply->Head.msgh_bits & MACH_MSGH_BITS_COMPLEX)
mach_msg_destroy (&reply->Head);
continue;
}
mr = mach_msg (&reply->Head,
MACH_SEND_MSG,
reply->Head.msgh_size,
0,
MACH_PORT_NULL,
0,
MACH_PORT_NULL);
switch (mr)
{
case MACH_SEND_INVALID_DEST:
mach_msg_destroy (&reply->Head);
break;
default:
error (0, mr, "mach_msg");
}
}
return NULL;
}
static void *
service_paging_requests (void *arg)
{
struct pager_requests *requests = arg;
pthread_setname_np (pthread_self (), "paging_requests");
int demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
return pager_demuxer (requests, inp, outp);
}
ports_manage_port_operations_one_thread (requests->bucket,
demuxer,
0);
return NULL;
}
error_t
pager_start_workers (struct port_bucket *pager_bucket,
struct pager_requests **out_requests)
{
error_t err;
int i;
pthread_t t;
struct pager_requests *requests;
struct rlimit limits = { RLIM_INFINITY, RLIM_INFINITY };
assert_backtrace (out_requests != NULL);
if (setrlimit (RLIMIT_AS, &limits) == -1 && errno != EPERM)
perror ("error lifting address space limits");
requests = malloc (sizeof *requests);
if (requests == NULL)
{
err = ENOMEM;
goto done;
}
requests->bucket = pager_bucket;
requests->asleep = 0;
requests->queue_in = malloc (sizeof *requests->queue_in);
if (requests->queue_in == NULL)
{
err = ENOMEM;
goto done;
}
queue_init (requests->queue_in);
requests->queue_out = requests->queue_in;
pthread_cond_init (&requests->wakeup, NULL);
pthread_cond_init (&requests->inhibit_wakeup, NULL);
pthread_mutex_init (&requests->lock, NULL);
err = pthread_create (&t, NULL, service_paging_requests, requests);
if (err)
goto done;
pthread_detach (t);
for (i = 0; i < WORKER_COUNT; i++)
{
requests->workers[i].requests = requests;
requests->workers[i].tag = 0;
queue_init (&requests->workers[i].queue);
err = pthread_create (&t, NULL, &worker_func, &requests->workers[i]);
if (err)
goto done;
pthread_detach (t);
}
done:
if (err)
{
free (requests);
*out_requests = NULL;
}
else
*out_requests = requests;
return err;
}
error_t
pager_inhibit_workers (struct pager_requests *requests)
{
error_t err = 0;
pthread_mutex_lock (&requests->lock);
assert_backtrace (requests->queue_out == requests->queue_in);
struct queue *new_queue = malloc (sizeof *new_queue);
if (new_queue == NULL)
{
err = ENOMEM;
goto done_locked;
}
queue_init (new_queue);
requests->queue_in = new_queue;
while (requests->asleep < WORKER_COUNT || !queue_empty(requests->queue_out))
pthread_cond_wait (&requests->inhibit_wakeup, &requests->lock);
done_locked:
pthread_mutex_unlock (&requests->lock);
return err;
}
void
pager_resume_workers (struct pager_requests *requests)
{
pthread_mutex_lock (&requests->lock);
assert_backtrace (requests->queue_out != requests->queue_in);
assert_backtrace (requests->asleep == WORKER_COUNT);
assert_backtrace (queue_empty(requests->queue_out));
free (requests->queue_out);
requests->queue_out = requests->queue_in;
pthread_cond_broadcast (&requests->wakeup);
pthread_mutex_unlock (&requests->lock);
}