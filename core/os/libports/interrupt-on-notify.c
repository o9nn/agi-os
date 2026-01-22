#include "ports.h"
#include <assert-backtrace.h>
error_t
ports_interrupt_rpc_on_notification (void *object,
struct rpc_info *rpc,
mach_port_t port, mach_msg_id_t what)
{
int req_notify;
struct ports_notify *pn;
struct rpc_notify *new_req, *req;
pthread_mutex_lock (&_ports_lock);
if (! MACH_PORT_VALID (port))
{
hurd_thread_cancel (rpc->thread);
pthread_mutex_unlock (&_ports_lock);
return 0;
}
new_req = _ports_free_rpc_notifies;
if (new_req)
_ports_free_rpc_notifies = new_req->next;
else
{
pthread_mutex_unlock (&_ports_lock);
new_req = malloc (sizeof (struct rpc_notify));
if (! new_req)
return ENOMEM;
pthread_mutex_lock (&_ports_lock);
}
for (pn = _ports_notifications; pn; pn = pn->next)
if (pn->port == port && pn->what == what)
break;
if (! pn)
{
pn = _ports_free_ports_notifies;
if (pn)
_ports_free_ports_notifies = pn->next;
else
{
pn = malloc (sizeof (struct ports_notify));
if (! pn)
{
new_req->next = _ports_free_rpc_notifies;
_ports_free_rpc_notifies = new_req;
pthread_mutex_unlock (&_ports_lock);
return ENOMEM;
}
}
pn->reqs = 0;
pn->port = port;
pn->what = what;
pn->pending = 0;
pthread_mutex_init (&pn->lock, NULL);
pn->next = _ports_notifications;
pn->prevp = &_ports_notifications;
if (_ports_notifications)
_ports_notifications->prevp = &pn->next;
_ports_notifications = pn;
}
for (req = rpc->notifies; req; req = req->next)
if (req->notify == pn)
break;
if (req)
{
new_req->next = _ports_free_rpc_notifies;
_ports_free_rpc_notifies = new_req;
}
else
{
req = new_req;
req->rpc = rpc;
req->notify = pn;
req->pending = 0;
req->next_req = pn->reqs;
req->prev_req_p = &pn->reqs;
if (pn->reqs)
pn->reqs->prev_req_p = &req->next_req;
pn->reqs = req;
req->next = rpc->notifies;
rpc->notifies = req;
}
req->pending++;
req_notify = !pn->pending;
if (req_notify)
pthread_mutex_lock (&pn->lock);
pthread_mutex_unlock (&_ports_lock);
if (req_notify)
{
mach_port_t old;
error_t err =
mach_port_request_notification (mach_task_self (), port,
what, 1,
ports_port_notify_right (object),
MACH_MSG_TYPE_MAKE_SEND_ONCE, &old);
if (! err && old != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), old);
pn->pending = 1;
pthread_mutex_unlock (&pn->lock);
return err;
}
else
return 0;
}
error_t
ports_interrupt_self_on_notification (void *object,
mach_port_t port, mach_msg_id_t what)
{
struct rpc_info *rpc;
struct port_info *pi = object;
thread_t thread = hurd_thread_self ();
pthread_mutex_lock (&_ports_lock);
for (rpc = pi->current_rpcs; rpc; rpc = rpc->next)
if (rpc->thread == thread)
break;
pthread_mutex_unlock (&_ports_lock);
assert_backtrace (rpc);
return ports_interrupt_rpc_on_notification (object, rpc, port, what);
}