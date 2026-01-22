#include "priv.h"
void
_pager_block_termination (struct pager *p)
{
p->noterm++;
}
void
_pager_allow_termination (struct pager *p)
{
if (!--p->noterm && p->termwaiting)
pthread_cond_broadcast (&p->wakeup);
}