#include "priv.h"
void
_pager_clean (void *arg)
{
struct pager *p = arg;
#ifdef KERNEL_INIT_RACE
struct pending_init *i, *tmp;
#endif
if (p->pager_state != NOTINIT)
{
pthread_mutex_lock (&p->interlock);
_pager_free_structure (p);
#ifdef KERNEL_INIT_RACE
for (i = p->init_head; i; i = tmp)
{
mach_port_deallocate (mach_task_self (), i->control);
mach_port_deallocate (mach_task_self (), i->name);
tmp = i->next;
free (i);
}
#endif
pthread_mutex_unlock (&p->interlock);
}
pager_clear_user_data (p->upi);
}