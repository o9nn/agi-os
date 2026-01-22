#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
kern_return_t
_pager_S_memory_object_init (struct pager *p,
mach_port_t control,
mach_port_t name,
vm_size_t pagesize)
{
if (!p
|| p->port.class != _pager_class)
return EOPNOTSUPP;
pthread_mutex_lock (&p->interlock);
if (pagesize != __vm_page_size)
{
printf ("incg init: bad page size");
goto out;
}
if (p->pager_state != NOTINIT)
{
#ifdef KERNEL_INIT_RACE
struct pending_init *i = malloc (sizeof (struct pending_init));
printf ("pager out-of-sequence init\n");
i->control = control;
i->name = name;
i->next = 0;
if (p->init_tail)
p->init_tail->next = i;
else
p->init_head = i;
p->init_tail = i;
#else
printf ("pager dup init\n");
#endif
goto out;
}
p->memobjcntl = control;
p->memobjname = name;
memory_object_ready (control, p->may_cache, p->copy_strategy);
p->pager_state = NORMAL;
out:
pthread_mutex_unlock (&p->interlock);
return 0;
}