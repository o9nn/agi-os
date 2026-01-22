#include "priv.h"
#include <assert-backtrace.h>
void
pager_change_attributes (struct pager *p,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy,
int wait)
{
struct attribute_request *ar = 0;
pthread_mutex_lock (&p->interlock);
if (p->may_cache == may_cache && p->copy_strategy == copy_strategy
&& ! (p->attribute_requests && wait))
{
pthread_mutex_unlock (&p->interlock);
return;
}
p->may_cache = may_cache;
p->copy_strategy = copy_strategy;
if (p->pager_state == NOTINIT)
{
pthread_mutex_unlock (&p->interlock);
return;
}
if (wait)
{
for (ar = p->attribute_requests; ar; ar = ar->next)
if (ar->may_cache == may_cache
&& ar->copy_strategy == copy_strategy)
{
ar->attrs_pending++;
ar->threads_waiting++;
break;
}
if (!ar)
{
ar = malloc (sizeof (struct attribute_request));
ar->may_cache = may_cache;
ar->copy_strategy = copy_strategy;
ar->attrs_pending = 1;
ar->threads_waiting = 1;
ar->next = p->attribute_requests;
if (ar->next)
ar->next->prevp = &ar->next;
ar->prevp = &p->attribute_requests;
p->attribute_requests = ar;
}
}
pthread_mutex_unlock (&p->interlock);
memory_object_change_attributes (p->memobjcntl, may_cache, copy_strategy,
wait ? p->port.port_right : MACH_PORT_NULL);
if (wait)
{
pthread_mutex_lock (&p->interlock);
while (ar->attrs_pending)
pthread_cond_wait (&p->wakeup, &p->interlock);
if (! --ar->threads_waiting)
{
*ar->prevp = ar->next;
if (ar->next)
ar->next->prevp = ar->prevp;
free (ar);
}
pthread_mutex_unlock (&p->interlock);
}
}