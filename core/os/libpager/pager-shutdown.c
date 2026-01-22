#include "priv.h"
#include <mach/notify.h>
void
pager_shutdown (struct pager *p)
{
pager_sync (p, 1);
pager_flush (p, 1);
pthread_mutex_lock (&p->interlock);
p->pager_state = SHUTDOWN;
ports_destroy_right (p);
pthread_mutex_unlock (&p->interlock);
}