#include <pthread.h>
#include <assert-backtrace.h>
#include <stdlib.h>
#include "connq.h"
struct connq
{
struct connq_request *head;
struct connq_request **tail;
unsigned count;
unsigned max;
pthread_cond_t listeners;
unsigned num_listeners;
pthread_cond_t connectors;
unsigned num_connectors;
pthread_mutex_t lock;
};
struct connq_request
{
struct connq_request *next;
struct sock *sock;
};
static inline void
connq_request_init (struct connq_request *req, struct sock *sock)
{
req->sock = sock;
}
static void
connq_request_enqueue (struct connq *cq, struct connq_request *req)
{
assert_backtrace (pthread_mutex_trylock (&cq->lock));
req->next = NULL;
*cq->tail = req;
cq->tail = &req->next;
cq->count ++;
}
static struct connq_request *
connq_request_dequeue (struct connq *cq)
{
struct connq_request *req;
assert_backtrace (pthread_mutex_trylock (&cq->lock));
assert_backtrace (cq->head);
req = cq->head;
cq->head = req->next;
if (! cq->head)
cq->tail = &cq->head;
cq->count --;
return req;
}
error_t
connq_create (struct connq **cq)
{
struct connq *new = malloc (sizeof (struct connq));
if (!new)
return ENOBUFS;
new->head = NULL;
new->tail = &new->head;
new->count = 0;
new->max = 0;
new->num_listeners = 0;
new->num_connectors = 0;
pthread_mutex_init (&new->lock, NULL);
pthread_cond_init (&new->listeners, NULL);
pthread_cond_init (&new->connectors, NULL);
*cq = new;
return 0;
}
void
connq_destroy (struct connq *cq)
{
assert_backtrace (! cq->head);
assert_backtrace (cq->count == 0);
free (cq);
}
error_t
connq_listen (struct connq *cq, struct timespec *tsp, struct sock **sock)
{
error_t err = 0;
pthread_mutex_lock (&cq->lock);
if (tsp && tsp->tv_sec == 0 && tsp->tv_nsec == 0 && cq->count == 0
&& cq->num_connectors == 0)
{
pthread_mutex_unlock (&cq->lock);
return EWOULDBLOCK;
}
if (! sock && (cq->count > 0 || cq->num_connectors > 0))
{
pthread_mutex_unlock (&cq->lock);
return 0;
}
cq->num_listeners++;
if (cq->count == 0)
{
assert_backtrace (! cq->head);
if (cq->num_connectors > 0)
pthread_cond_signal (&cq->connectors);
do
{
err = pthread_hurd_cond_timedwait_np (&cq->listeners, &cq->lock, tsp);
if (err)
{
cq->num_listeners--;
goto out;
}
}
while (cq->count == 0);
}
assert_backtrace (cq->head);
if (sock)
{
struct connq_request *req = connq_request_dequeue (cq);
*sock = req->sock;
free (req);
}
else if (cq->num_listeners > 0)
pthread_cond_signal (&cq->listeners);
else
{  }
out:
pthread_mutex_unlock (&cq->lock);
return err;
}
error_t
connq_connect (struct connq *cq, int noblock)
{
pthread_mutex_lock (&cq->lock);
if (noblock
&& cq->count + cq->num_connectors >= cq->max + cq->num_listeners)
{
pthread_mutex_unlock (&cq->lock);
return EWOULDBLOCK;
}
cq->num_connectors ++;
while (cq->count + cq->num_connectors > cq->max + cq->num_listeners)
if (pthread_hurd_cond_wait_np (&cq->connectors, &cq->lock))
{
cq->num_connectors --;
pthread_mutex_unlock (&cq->lock);
return EINTR;
}
pthread_mutex_unlock (&cq->lock);
return 0;
}
void
connq_connect_complete (struct connq *cq, struct sock *sock)
{
struct connq_request *req;
req = malloc (sizeof (struct connq_request));
if (! req)
abort ();
connq_request_init (req, sock);
pthread_mutex_lock (&cq->lock);
assert_backtrace (cq->num_connectors > 0);
cq->num_connectors --;
connq_request_enqueue (cq, req);
if (cq->num_listeners > 0)
{
cq->num_listeners --;
pthread_cond_signal (&cq->listeners);
}
pthread_mutex_unlock (&cq->lock);
}
void
connq_connect_cancel (struct connq *cq)
{
pthread_mutex_lock (&cq->lock);
assert_backtrace (cq->num_connectors > 0);
cq->num_connectors --;
if (cq->count + cq->num_connectors >= cq->max + cq->num_listeners)
pthread_cond_signal (&cq->connectors);
pthread_mutex_unlock (&cq->lock);
}
error_t
connq_set_length (struct connq *cq, int max)
{
int omax;
pthread_mutex_lock (&cq->lock);
omax = cq->max;
cq->max = max;
if (max > omax && cq->count >= omax && cq->count < max
&& cq->num_connectors >= cq->num_listeners)
pthread_cond_broadcast (&cq->listeners);
pthread_mutex_unlock (&cq->lock);
return 0;
}