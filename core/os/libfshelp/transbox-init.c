#include "fshelp.h"
#include <pthread.h>
void
fshelp_transbox_init (struct transbox *transbox,
pthread_mutex_t *lock,
void *cookie)
{
transbox->active = MACH_PORT_NULL;
transbox->flags = 0;
transbox->lock = lock;
pthread_cond_init (&transbox->wakeup, NULL);
transbox->cookie = cookie;
}