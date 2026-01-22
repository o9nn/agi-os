#include "priv.h"
#include <stdio.h>
kern_return_t
_pager_S_memory_object_change_completed (struct pager *p,
boolean_t maycache,
memory_object_copy_strategy_t strat)
{
struct attribute_request *ar;
if (!p
|| p->port.class != _pager_class)
{
printf ("Bad change completed\n");
return EOPNOTSUPP;
}
pthread_mutex_lock (&p->interlock);
for (ar = p->attribute_requests; ar; ar = ar->next)
if (ar->may_cache == maycache && ar->copy_strategy == strat)
{
if (ar->attrs_pending && !--ar->attrs_pending)
pthread_cond_broadcast (&p->wakeup);
break;
}
pthread_mutex_unlock (&p->interlock);
return 0;
}