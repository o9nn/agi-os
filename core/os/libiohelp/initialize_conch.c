#include "iohelp.h"
void
iohelp_initialize_conch (struct conch *c, pthread_mutex_t *m)
{
c->lock = m;
pthread_cond_init (&c->wait, NULL);
c->holder = 0;
c->holder_shared_page = 0;
}