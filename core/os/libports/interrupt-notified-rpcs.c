#include "ports.h"
struct ports_notify *_ports_notifications;
struct ports_notify *_ports_free_ports_notifies;
struct rpc_notify *_ports_free_rpc_notifies;
void
ports_interrupt_notified_rpcs (void *object,
mach_port_t port, mach_msg_id_t what)
{
if (_ports_notifications)
{
struct ports_notify *np;
pthread_mutex_lock (&_ports_lock);
for (np = _ports_notifications; np; np = np->next)
if (np->port == port && np->what == what)
{
struct rpc_notify *req;
for (req = np->reqs; req; req = req->next_req)
if (req->pending)
{
req->pending--;
hurd_thread_cancel (req->rpc->thread);
}
break;
}
pthread_mutex_unlock (&_ports_lock);
}
}
static void
remove_req (struct rpc_notify *req)
{
struct ports_notify *np = req->notify;
if (req->next_req)
req->next_req->prev_req_p = req->prev_req_p;
*req->prev_req_p = req->next_req;
if (np->reqs == 0)
{
if (np->next)
np->next->prevp = np->prevp;
*np->prevp = np->next;
np->next = _ports_free_ports_notifies;
_ports_free_ports_notifies = np;
if (np->pending)
{
mach_port_t old;
error_t err =
mach_port_request_notification (mach_task_self (), np->port,
np->what, 0, MACH_PORT_NULL,
MACH_MSG_TYPE_MAKE_SEND_ONCE,
&old);
if (! err && old != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), old);
}
}
}
void
_ports_remove_notified_rpc (struct rpc_info *rpc)
{
struct rpc_notify *req = rpc->notifies;
if (req)
{
struct rpc_notify *last = req;
while (last->next)
{
remove_req (last);
last = last->next;
}
remove_req (last);
last->next = _ports_free_rpc_notifies;
_ports_free_rpc_notifies = req;
}
}